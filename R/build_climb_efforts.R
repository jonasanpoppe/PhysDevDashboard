library(dplyr)
library(readr)
library(duckdb)

# strava_climb_efforts: one row per ascent of a climb in config/climbs.csv,
# on rides only, with the bike and the effort status.
#
# - Effort metrics come from climb_efforts() per stream (R/climb_matching.R).
# - status_auto is what the stream says; status is after
#   config/effort_overrides.csv (status_source says which).
# - The bike comes from assign_bikes() (R/assign_bikes.R); every ride must get
#   one, so the power model never runs on a guessed mass.
# - in_record_scope: the ride climbed at least scope_min_gain_m in total
#   (the climb record counts those rides).
# - Estimated power per effort (R/power_model.R): a model output with its
#   range, using the rider's trend weight and the bike's mass on the day.

build_strava_climb_efforts <- function(
    efforts, activities, bikes, bike_overrides, effort_overrides,
    climbs, weights, power = list(),
    scope_min_gain_m = 120, db_path = "db/health.db"
) {
  rides <- activities %>%
    mutate(activity_id = as.character(activity_id)) %>%
    filter(sport_type %in% RIDE_TYPES)

  bike_of <- assign_bikes(rides %>% select(activity_id, date, gear_id), bikes, bike_overrides)

  ov <- effort_overrides %>%
    transmute(activity_id = as.character(activity_id), climb, status_override = status)
  unknown_status <- setdiff(ov$status_override, c("clean", "top", "foot", "early", "walk"))
  if (length(unknown_status) > 0) {
    stop("effort_overrides.csv: unknown status ", paste(unknown_status, collapse = ", "))
  }

  out <- efforts %>%
    mutate(activity_id = as.character(activity_id)) %>%
    inner_join(rides %>% select(activity_id, date, start_local, ride_gain_m = elev_gain_m),
               by = "activity_id") %>%
    left_join(bike_of, by = "activity_id") %>%
    left_join(ov, by = c("activity_id", "climb")) %>%
    mutate(
      status_auto = status,
      status = coalesce(status_override, status_auto),
      status_source = if_else(is.na(status_override), "stream", "override"),
      in_record_scope = !is.na(ride_gain_m) & ride_gain_m >= scope_min_gain_m
    ) %>%
    select(activity_id, date, start_local, climb, i0, i1,
           elapsed_s, stop_s, moving_s, climb_m, crawl10_kmh, walk_s,
           hr_avg, hr_end, hrs_into_ride,
           status, status_auto, status_source,
           bike_id, bike_source, bike_mass_kg, ride_gain_m, in_record_scope) %>%
    estimate_power(climbs, weights, power) %>%
    arrange(date, start_local, climb)

  no_weight <- sum(is.na(out$rider_kg))
  if (no_weight > 0) warning(no_weight, " effort(s) before the first trend weight: no power estimate")

  stale <- anti_join(ov, out, by = c("activity_id", "climb"))
  if (nrow(stale) > 0) {
    warning("effort_overrides.csv: no effort for ",
            paste(stale$activity_id, stale$climb, collapse = "; "), " (kept, unused)")
  }

  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "strava_climb_efforts", out, overwrite = TRUE)
  message("✓ strava_climb_efforts written (", nrow(out), " efforts, ",
          n_distinct(out$climb), " climbs)")
  invisible(out)
}

# config/climbs.csv -> the columns matching needs, checked.
read_climbs <- function(path) {
  climbs <- read_csv(path, col_types = cols(segment_id = col_character(), .default = col_guess()),
                     show_col_types = FALSE)
  need <- c("name", "start_lat", "start_lon", "end_lat", "end_lon", "length_m")
  missing <- setdiff(need, names(climbs))
  if (length(missing) > 0) stop("climbs.csv: missing column(s) ", paste(missing, collapse = ", "))
  if (anyDuplicated(climbs$name)) {
    stop("climbs.csv: duplicate name(s) ", paste(unique(climbs$name[duplicated(climbs$name)]), collapse = ", "))
  }
  if (anyNA(climbs[need])) stop("climbs.csv: empty coordinates or length")
  climbs
}

read_config_csv <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(across(any_of(c("mass_kg", "lowest_gear_m")), as.numeric))
}
