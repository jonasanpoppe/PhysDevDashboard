# Synthetic analytics_daily (invented): HRV follows sleep the same day, resting
# HR follows weight three days later, steps are noise. HRV and resting HR come
# from two device eras.
fake_analytics_daily <- function(n = 300, seed = 7) {
  withr::with_seed(seed, {
    days <- as.Date("2030-01-01") + seq_len(n) - 1
    era <- ifelse(days < as.Date("2030-06-01"), "fitbit", "garmin")
    sleep <- 7 + sin(seq_len(n) / 9) + rnorm(n, 0, 0.3)
    weight <- 80 + cumsum(rnorm(n, 0, 0.1))
    rhr <- 50 + 2 * (dplyr::lag(weight, 3) - 80) + rnorm(n, 0, 0.2)
    ride <- ifelse(seq_len(n) %% 7 == 0, 2, 0)
    dplyr::bind_rows(
      tibble::tibble(date = days, variable = "sleep_h", era = era, value = sleep),
      tibble::tibble(date = days, variable = "hrv", era = era, value = 40 + 8 * sleep + rnorm(n, 0, 1)),
      tibble::tibble(date = days, variable = "scale_kg", era = "all", value = weight),
      tibble::tibble(date = days, variable = "rhr", era = era, value = rhr),
      tibble::tibble(date = days, variable = "steps", era = "all", value = rnorm(n, 9000, 2000)),
      tibble::tibble(date = days, variable = "ride_h", era = "all", value = ride)
    ) %>%
      dplyr::filter(!is.na(value))
  })
}
