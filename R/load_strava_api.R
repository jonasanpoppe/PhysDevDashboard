library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(duckdb)

# Strava API → tables. The raw files under data/strava_api/ (written by
# python/fetch_strava_api.py) are the source of truth; these tables are
# rebuilt from them. Streams stay files: only derived summaries (time in HR
# zone) reach the database.
#
# IDs are character: Strava's 64-bit IDs survive JSON as doubles but print in
# scientific notation and must never be summed.
#
# Times: *_utc are POSIXct UTC. *_local are Strava's local wall time, as text
# without offset (Strava appends a misleading "Z"). `date` is the local
# calendar date, the convention for every source.

# ── Helpers ──────────────────────────────────────────────────────────────────

# One field from each item, NA when absent or null. `...` is a pluck() path.
col_of <- function(items, ..., type = c("dbl", "chr", "lgl")) {
  type <- match.arg(type)
  vals <- map(items, \(x) pluck(x, ..., .default = NA) %||% NA)
  switch(type,
    dbl = map_dbl(vals, as.numeric),
    chr = map_chr(vals, as.character),
    lgl = map_lgl(vals, as.logical)
  )
}

id_chr <- function(x) if_else(is.na(x), NA_character_, sprintf("%.0f", x))

parse_utc <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

strip_z <- function(x) sub("Z$", "", x)

# Parsed JSON items in a directory, minus {"_status": …} "no data" markers
read_strava_items <- function(dir) {
  list.files(dir, pattern = "\\.json$", full.names = TRUE) |>
    map(read_json) |>
    discard(\(x) !is.null(x[["_status"]]))
}

# Activity summaries from every list fetch; a later file wins for an activity
read_strava_summaries <- function(files) {
  acts <- sort(files) |> map(read_json) |> list_flatten()
  ids <- map_dbl(acts, "id")
  acts[!duplicated(ids, fromLast = TRUE)]
}

# ── Tables ───────────────────────────────────────────────────────────────────

strava_activities_table <- function(summaries, details) {
  base <- tibble(
    activity_id        = id_chr(col_of(summaries, "id")),
    date               = as.Date(substr(col_of(summaries, "start_date_local", type = "chr"), 1, 10)),
    start_utc          = parse_utc(col_of(summaries, "start_date", type = "chr")),
    start_local        = strip_z(col_of(summaries, "start_date_local", type = "chr")),
    timezone           = col_of(summaries, "timezone", type = "chr"),
    name               = col_of(summaries, "name", type = "chr"),
    sport_type         = col_of(summaries, "sport_type", type = "chr"),
    trainer            = col_of(summaries, "trainer", type = "lgl"),
    commute            = col_of(summaries, "commute", type = "lgl"),
    manual             = col_of(summaries, "manual", type = "lgl"),
    distance_m         = col_of(summaries, "distance"),
    moving_s           = col_of(summaries, "moving_time"),
    elapsed_s          = col_of(summaries, "elapsed_time"),
    elev_gain_m        = col_of(summaries, "total_elevation_gain"),
    avg_speed_ms       = col_of(summaries, "average_speed"),
    max_speed_ms       = col_of(summaries, "max_speed"),
    has_heartrate      = col_of(summaries, "has_heartrate", type = "lgl"),
    avg_hr             = col_of(summaries, "average_heartrate"),
    max_hr             = col_of(summaries, "max_heartrate"),
    # Without a power meter (device_watts FALSE) Strava estimates power:
    # directional only
    device_watts       = col_of(summaries, "device_watts", type = "lgl"),
    avg_watts          = col_of(summaries, "average_watts"),
    weighted_avg_watts = col_of(summaries, "weighted_average_watts"),
    kilojoules         = col_of(summaries, "kilojoules"),
    avg_cadence        = col_of(summaries, "average_cadence"),
    suffer_score       = col_of(summaries, "suffer_score"),
    gear_id            = col_of(summaries, "gear_id", type = "chr"),
    device_name        = col_of(summaries, "device_name", type = "chr")
  )
  extra <- tibble(
    activity_id        = id_chr(col_of(details, "id")),
    kcal               = col_of(details, "calories"),
    perceived_exertion = col_of(details, "perceived_exertion"),
    description        = col_of(details, "description", type = "chr")
  )
  left_join(base, extra, by = "activity_id") |> arrange(start_utc)
}

strava_laps_table <- function(details) {
  laps <- details |> map("laps") |> list_flatten()
  tibble(
    lap_id       = id_chr(col_of(laps, "id")),
    activity_id  = id_chr(col_of(laps, "activity", "id")),
    lap_index    = col_of(laps, "lap_index"),
    start_utc    = parse_utc(col_of(laps, "start_date", type = "chr")),
    start_local  = strip_z(col_of(laps, "start_date_local", type = "chr")),
    distance_m   = col_of(laps, "distance"),
    moving_s     = col_of(laps, "moving_time"),
    elapsed_s    = col_of(laps, "elapsed_time"),
    elev_gain_m  = col_of(laps, "total_elevation_gain"),
    avg_speed_ms = col_of(laps, "average_speed"),
    max_speed_ms = col_of(laps, "max_speed"),
    avg_hr       = col_of(laps, "average_heartrate"),
    max_hr       = col_of(laps, "max_heartrate"),
    device_watts = col_of(laps, "device_watts", type = "lgl"),
    avg_watts    = col_of(laps, "average_watts"),
    avg_cadence  = col_of(laps, "average_cadence"),
    start_index  = col_of(laps, "start_index"),
    end_index    = col_of(laps, "end_index")
  ) |> arrange(start_utc, lap_index)
}

strava_segment_efforts_table <- function(details) {
  efforts <- details |> map("segment_efforts") |> list_flatten()
  tibble(
    effort_id    = id_chr(col_of(efforts, "id")),
    activity_id  = id_chr(col_of(efforts, "activity", "id")),
    segment_id   = id_chr(col_of(efforts, "segment", "id")),
    segment_name = col_of(efforts, "segment", "name", type = "chr"),
    date         = as.Date(substr(col_of(efforts, "start_date_local", type = "chr"), 1, 10)),
    start_utc    = parse_utc(col_of(efforts, "start_date", type = "chr")),
    start_local  = strip_z(col_of(efforts, "start_date_local", type = "chr")),
    distance_m   = col_of(efforts, "distance"),
    elapsed_s    = col_of(efforts, "elapsed_time"),
    moving_s     = col_of(efforts, "moving_time"),
    avg_hr       = col_of(efforts, "average_heartrate"),
    max_hr       = col_of(efforts, "max_heartrate"),
    device_watts = col_of(efforts, "device_watts", type = "lgl"),
    avg_watts    = col_of(efforts, "average_watts"),
    avg_cadence  = col_of(efforts, "average_cadence"),
    # 1-3 when among your three best at the time; NA otherwise
    pr_rank      = col_of(efforts, "pr_rank"),
    kom_rank     = col_of(efforts, "kom_rank"),
    hidden       = col_of(efforts, "hidden", type = "lgl"),
    # Positions in the activity's streams, to slice HR or power per effort
    start_index  = col_of(efforts, "start_index"),
    end_index    = col_of(efforts, "end_index")
  ) |> arrange(start_utc)
}

# Max credible climbing rate (m/h) for a best effort; above it, the segment's
# elevation data is broken (common for auto-created "… Climb" segments).
MAX_CREDIBLE_VAM <- 1800

strava_segments_table <- function(details, full_segments, efforts) {
  # Every segment appears inside its efforts; the full segment files (fetched
  # later) add totals and your PR stats.
  embedded <- details |> map("segment_efforts") |> list_flatten() |> map("segment")
  base <- tibble(
    segment_id     = id_chr(col_of(embedded, "id")),
    name           = col_of(embedded, "name", type = "chr"),
    activity_type  = col_of(embedded, "activity_type", type = "chr"),
    distance_m     = col_of(embedded, "distance"),
    avg_grade_pct  = col_of(embedded, "average_grade"),
    max_grade_pct  = col_of(embedded, "maximum_grade"),
    elev_low_m     = col_of(embedded, "elevation_low"),
    elev_high_m    = col_of(embedded, "elevation_high"),
    climb_category = col_of(embedded, "climb_category"),
    city           = col_of(embedded, "city", type = "chr"),
    country        = col_of(embedded, "country", type = "chr"),
    start_lat      = col_of(embedded, "start_latlng", 1),
    start_lng      = col_of(embedded, "start_latlng", 2),
    end_lat        = col_of(embedded, "end_latlng", 1),
    end_lng        = col_of(embedded, "end_latlng", 2),
    hazardous      = col_of(embedded, "hazardous", type = "lgl"),
    private        = col_of(embedded, "private", type = "lgl")
  ) |> distinct(segment_id, .keep_all = TRUE)

  full <- tibble(
    segment_id         = id_chr(col_of(full_segments, "id")),
    elev_gain_m        = col_of(full_segments, "total_elevation_gain"),
    starred            = col_of(full_segments, "starred", type = "lgl"),
    athlete_count      = col_of(full_segments, "athlete_count"),
    effort_count_all   = col_of(full_segments, "effort_count"),
    pr_elapsed_s       = col_of(full_segments, "athlete_segment_stats", "pr_elapsed_time"),
    pr_date            = as.Date(col_of(full_segments, "athlete_segment_stats", "pr_date", type = "chr")),
    my_effort_count    = col_of(full_segments, "athlete_segment_stats", "effort_count"),
    polyline           = col_of(full_segments, "map", "polyline", type = "chr")
  )

  mine <- efforts |>
    summarise(
      .by = segment_id,
      my_efforts_loaded = n(),
      best_elapsed_s    = min(elapsed_s, na.rm = TRUE),
      first_date        = min(date),
      last_date         = max(date)
    )

  base |>
    left_join(full, by = "segment_id") |>
    left_join(mine, by = "segment_id") |>
    mutate(
      best_vam_mh  = (elev_high_m - elev_low_m) / best_elapsed_s * 3600,
      elev_suspect = best_vam_mh >= MAX_CREDIBLE_VAM
    )
}

strava_best_efforts_table <- function(details) {
  best <- details |> map("best_efforts") |> list_flatten()
  tibble(
    best_effort_id = id_chr(col_of(best, "id")),
    activity_id    = id_chr(col_of(best, "activity", "id")),
    name           = col_of(best, "name", type = "chr"),
    date           = as.Date(substr(col_of(best, "start_date_local", type = "chr"), 1, 10)),
    start_utc      = parse_utc(col_of(best, "start_date", type = "chr")),
    distance_m     = col_of(best, "distance"),
    elapsed_s      = col_of(best, "elapsed_time"),
    moving_s       = col_of(best, "moving_time"),
    pr_rank        = col_of(best, "pr_rank"),
    start_index    = col_of(best, "start_index"),
    end_index      = col_of(best, "end_index")
  ) |> arrange(start_utc, distance_m)
}

# ── Loader ───────────────────────────────────────────────────────────────────

load_strava_api <- function(
    activity_files,
    details_dir  = "data/strava_api/details",
    segments_dir = "data/strava_api/segments",
    db_path      = "db/health.db"
) {
  summaries <- read_strava_summaries(activity_files)
  details   <- read_strava_items(details_dir)
  segments  <- read_strava_items(segments_dir)

  efforts <- strava_segment_efforts_table(details)
  tables <- list(
    activities      = strava_activities_table(summaries, details),
    laps            = strava_laps_table(details),
    segment_efforts = efforts,
    segments        = strava_segments_table(details, segments, efforts),
    best_efforts    = strava_best_efforts_table(details)
  )

  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  iwalk(tables, \(t, name) {
    DBI::dbWriteTable(con, paste0("strava_", name), t, overwrite = TRUE)
    message("strava_", name, " written (", nrow(t), " rows)")
  })

  invisible(tables)
}

# ── Heart-rate zones from streams ────────────────────────────────────────────

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
