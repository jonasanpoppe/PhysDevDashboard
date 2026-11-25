library(dplyr)

# Riding: rides from Strava (the system of record for rides), distance
# against the year's target, and time in the training plan's heart-rate
# bands per ride. Targets: riding in config/goals.yml; bands: plan_bands in
# config/athlete.yml.

build_rides <- function(activities, ride_types = RIDE_TYPES) {
  activities %>%
    filter(sport_type %in% ride_types) %>%
    transmute(activity_id = as.character(activity_id), date = as.Date(date),
              year = as.integer(format(date, "%Y")), month = as.integer(format(date, "%m")),
              weekday = as.integer(format(date, "%u")), name, sport_type,
              km = distance_m / 1000, moving_h = moving_s / 3600, elev_m = elev_gain_m,
              avg_kmh = if_else(moving_s > 0, distance_m / moving_s * 3.6, NA_real_)) %>%
    arrange(date)
}

# Cumulative km per year by day of year, and the target's straight pace.
rides_cumulative <- function(rides) {
  rides %>%
    mutate(doy = as.integer(format(date, "%j"))) %>%
    arrange(date) %>%
    mutate(km_cum = cumsum(km), .by = year) %>%
    select(year, date, doy, km_cum)
}

# Seconds per ride in each plan band, from the HR histogram (seconds at each
# bpm). Bands are inclusive ranges; above_cap = above the sweet-spot cap.
# Descriptive zones (hr_zones) are a separate set; never mixed.
build_plan_bands <- function(histogram, rides, bands) {
  inside <- function(hr, r) hr >= r[[1]] & hr <= r[[2]]
  histogram %>%
    mutate(activity_id = as.character(activity_id)) %>%
    inner_join(rides %>% select(activity_id, date), by = "activity_id") %>%
    summarise(
      .by = c(activity_id, date),
      z2_s = sum(seconds[inside(hr, bands$z2)]),
      sweet_spot_s = sum(seconds[inside(hr, bands$sweet_spot)]),
      above_cap_s = sum(seconds[hr > bands$sweet_spot_cap]),
      hr_s = sum(seconds)
    ) %>%
    arrange(date)
}

write_riding_tables <- function(rides, plan_bands, goal, bands = NULL, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "strava_rides", rides, overwrite = TRUE)
  DBI::dbWriteTable(con, "strava_plan_bands", plan_bands, overwrite = TRUE)
  targets <- tibble(year = as.integer(names(goal$annual_km %||% list())),
                    target_km = unlist(goal$annual_km %||% list()))
  DBI::dbWriteTable(con, "riding_targets", targets, overwrite = TRUE)
  DBI::dbWriteTable(con, "riding_benchmark",
                    tibble(benchmark_climb = goal$benchmark_climb %||% NA_character_), overwrite = TRUE)
  if (!is.null(bands)) {
    DBI::dbWriteTable(con, "riding_plan_bands", tibble(
      z2_low = bands$z2[[1]], z2_high = bands$z2[[2]], ss_low = bands$sweet_spot[[1]],
      ss_high = bands$sweet_spot[[2]], cap = bands$sweet_spot_cap), overwrite = TRUE)
  }
  message("✓ riding tables written (", nrow(rides), " rides)")
  invisible(TRUE)
}
