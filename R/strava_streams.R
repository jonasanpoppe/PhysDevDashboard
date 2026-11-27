library(jsonlite)
library(dplyr)
library(tidyr)
library(duckdb)

# Strava streams → time in heart-rate zones. The stream files under
# data/landing/strava/streams/ stay files; only this derived summary reaches
# the database. The other Strava tables (activities, laps, segment efforts,
# segments, best efforts) are loaded in SQL: sql/staging/strava_activities.sql.
#
# Built here, per ride and only for new rides (one targets branch per stream
# file), which a full SQL rescan of every stream on each build wouldn't be.

# Recording gaps longer than this (pauses, signal loss) don't count as time
# at the last heart rate.
MAX_STREAM_GAP_S <- 30

# Seconds at each whole bpm for one activity's stream file. Independent of the
# zone boundaries, so changing zones doesn't reprocess the streams.
strava_hr_histogram <- function(stream_file) {
  empty <- tibble(activity_id = character(), hr = integer(), seconds = numeric())
  s <- read_json(stream_file, simplifyVector = TRUE)
  if (is.null(s$heartrate) || is.null(s$time)) return(empty)

  t  <- s$time$data
  hr <- s$heartrate$data
  dt <- c(diff(t), 0)
  ok <- dt > 0 & dt <= MAX_STREAM_GAP_S & !is.na(hr)
  if (!any(ok)) return(empty)

  tibble(
    activity_id = sub("\\.json$", "", basename(stream_file)),
    hr          = as.integer(round(hr[ok])),
    seconds     = dt[ok]
  ) |>
    summarise(.by = c(activity_id, hr), seconds = sum(seconds))
}

RIDE_TYPES <- c("Ride", "GravelRide", "MountainBikeRide", "VirtualRide",
                "EBikeRide", "EMountainBikeRide", "Velomobile", "Handcycle")
RUN_TYPES  <- c("Run", "TrailRun", "VirtualRun")

# Seconds per HR zone per activity (all 7 zones, 0 when none). `zones` is the
# hr_zones block of config/athlete.yml.
load_strava_hr_zones <- function(histogram, activities, zones, db_path = "db/health.db") {
  family <- function(sport) case_when(
    sport %in% RIDE_TYPES ~ "ride",
    sport %in% RUN_TYPES  ~ "run",
    .default = "other"
  )
  zone_of <- function(hr, fam) {
    out <- integer(length(hr))
    for (f in unique(fam)) {
      i <- fam == f
      # Upper bounds are inclusive: hr ≤ bound 1 is zone 1, above bound 6 is 7
      out[i] <- findInterval(hr[i], unlist(zones$upper_bpm[[f]]), left.open = TRUE) + 1L
    }
    out
  }

  hr_zones <- histogram |>
    inner_join(select(activities, activity_id, sport_type), by = "activity_id") |>
    mutate(zone = zone_of(hr, family(sport_type))) |>
    summarise(.by = c(activity_id, zone), seconds = sum(seconds)) |>
    complete(activity_id, zone = 1:7, fill = list(seconds = 0)) |>
    mutate(zone_name = unlist(zones$names)[zone], .after = zone) |>
    arrange(activity_id, zone)

  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "strava_hr_zones", hr_zones, overwrite = TRUE)
  message("strava_hr_zones written (", nrow(hr_zones), " rows)")

  invisible(hr_zones)
}
