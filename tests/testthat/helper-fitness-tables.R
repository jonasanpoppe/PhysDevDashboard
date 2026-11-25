# Synthetic fitness tables, shaped like the pipeline's (invented values).
fake_fitness_tables <- function() {
  days <- as.Date("2030-01-01") + 0:500
  list(
    seasons = tibble::tibble(season = c(2030L, 2031L), watts_ref = c(360, 340),
                             ci_low = c(330, 325), ci_high = c(390, 356), n = c(12L, 30L),
                             first_date = as.Date(c("2030-03-01", "2031-01-05")),
                             last_date = as.Date(c("2030-12-20", "2031-05-10")),
                             ref_hr_bpm = 150, ref_duration_s = 180, ref_hours_into_ride = 1.5),
    coefs = tibble::tibble(term = c("(Intercept)", "hr_end", "season2031"),
                           estimate = c(100, 2.6, -20), ci_low = c(-200, 1, -45), ci_high = c(400, 4.5, 4)),
    check = tibble::tibble(w_per_weekly_hour = 5, ci_low = -8, ci_high = 18, n = 42L),
    load = tibble::tibble(date = days, ride_h = 0.3, run_h = 0.1, ride_6w_hpw = 2.1,
                          run_6w_hpw = 0.7, fitness_h = 0.4, fatigue_h = 0.4),
    weight = tibble::tibble(date = days, trend_kg = 80 - seq_along(days) / 200,
                            scale_kg = ifelse(seq_along(days) %% 3 == 0, 80, NA))
  )
}
