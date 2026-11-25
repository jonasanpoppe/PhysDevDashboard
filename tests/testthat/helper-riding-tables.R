# Synthetic rides and plan bands (invented), shaped like the pipeline's tables.
fake_riding_tables <- function() {
  dates <- as.Date("2030-01-05") + seq(0, 500, by = 7)
  rides <- tibble::tibble(
    activity_id = as.character(seq_along(dates)), date = dates,
    year = as.integer(format(dates, "%Y")), month = as.integer(format(dates, "%m")),
    weekday = as.integer(format(dates, "%u")), name = paste("Ride", seq_along(dates)),
    sport_type = "Ride", km = 40 + (seq_along(dates) %% 5) * 10, moving_h = 2,
    elev_m = 300 + (seq_along(dates) %% 4) * 100, avg_kmh = 25
  )
  bands <- tibble::tibble(activity_id = rides$activity_id, date = rides$date,
                          z2_s = 3600, sweet_spot_s = 900, above_cap_s = 120, hr_s = 7200)
  list(rides = rides, bands = bands, targets = tibble::tibble(year = 2031L, target_km = 3000))
}
