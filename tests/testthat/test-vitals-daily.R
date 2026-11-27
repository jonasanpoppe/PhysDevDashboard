source(testthat::test_path("..", "..", "R", "run_sql.R"))

# sql/marts/vitals_daily.sql on synthetic staging tables with invented values.
# Fitbit runs up to the switch date, Garmin starts the day after.
switch_date <- as.Date("2024-06-30")

fitbit_days <- function(n) switch_date - rev(seq_len(n)) + 1
garmin_days <- function(n) switch_date + seq_len(n)

vitals_inputs <- function(n_fitbit = 70, n_garmin = 10) {
  fd <- fitbit_days(n_fitbit)
  gd <- garmin_days(n_garmin)
  list(
    fitbit_rhr_daily = tibble(date = fd, resting_heart_rate = 55),
    fitbit_hrv_daily = tibble(date = fd, hrv_rmssd = exp(log(40) + rep(c(-0.1, 0.1), length.out = n_fitbit))),
    fitbit_rr_daily = tibble(date = fd, respiratory_rate = 14),
    fitbit_spo2_daily = tibble(date = fd, spo2_avg = 96),
    fitbit_temperature_nightly = tibble(date = fd, temperature_celsius = 33.2, baseline_temperature_celsius = 33.0),
    fitbit_sleep_daily = tibble(date = fd, sleep_end_local = paste0(fd, "T07:00:00"),
                          sleep_s = 25200, sleep_deep_s = 3600, sleep_light_s = 14400,
                          sleep_rem_s = 5400, sleep_awake_s = 1800),
    garmin_api_daily = tibble(
      date = gd, ga_rhr = 50, ga_hrv_last_night_avg = 60,
      ga_hrv_baseline_low = 52, ga_hrv_baseline_high = 68,
      ga_sleep_resp_avg = 15, ga_sleep_spo2_avg = 95, ga_sleep_skin_temp_dev_c = -0.3,
      ga_sleep_end_local = paste0(gd, c("T13:15:00", rep("T07:30:00", n_garmin - 1))),
      ga_sleep_s = 27000, ga_sleep_deep_s = 4000, ga_sleep_light_s = 15000,
      ga_sleep_rem_s = 6000, ga_sleep_awake_s = 2000
    )
  )
}

# Writes the inputs as staging tables, runs the mart, returns vitals_daily
build <- function(inputs, db = withr::local_tempfile(fileext = ".duckdb", .local_envir = parent.frame())) {
  con <- DBI::dbConnect(duckdb::duckdb(), db)
  for (t in names(inputs)) DBI::dbWriteTable(con, t, inputs[[t]], overwrite = TRUE)
  DBI::dbDisconnect(con, shutdown = TRUE)
  suppressMessages(run_sql(testthat::test_path("..", "..", "sql", "marts", "vitals_daily.sql"),
                           read_back = "vitals_daily", db_path = db))
}

vitals_metric_names <- c("rhr", "hrv", "resp", "spo2", "skin_temp_dev",
                         "sleep_s", "sleep_deep_s", "sleep_light_s", "sleep_rem_s", "sleep_awake_s")

test_that("every metric appears once per day and era, eras labelled by source", {
  out <- build(vitals_inputs())
  expect_equal(nrow(dplyr::count(out, date, metric, era) %>% dplyr::filter(n > 1)), 0)
  expect_setequal(unique(out$metric), vitals_metric_names)
  expect_true(all(out$date[out$era == "fitbit"] <= switch_date))
  expect_true(all(out$date[out$era == "garmin"] > switch_date))
})

test_that("7-day mean stays within an era and needs 4 values", {
  out <- build(vitals_inputs())
  rhr_g <- out %>% dplyr::filter(metric == "rhr", era == "garmin") %>% dplyr::arrange(date)
  # Garmin days 1-3 have fewer than 4 Garmin values in their window: the
  # Fitbit days just before the switch must not fill them up.
  expect_true(all(is.na(rhr_g$mean_7d[1:3])))
  expect_equal(rhr_g$mean_7d[4:10], rep(50, 7))
})

test_that("7-day mean uses calendar days, not rows", {
  inputs <- vitals_inputs()
  # Keep Garmin RHR only on days 1-4 and 10: day 10's window (days 4-10)
  # holds 2 values, so no mean, although the previous 4 rows exist.
  inputs$garmin_api_daily$ga_rhr[5:9] <- NA
  out <- build(inputs)
  rhr_g <- out %>% dplyr::filter(metric == "rhr", era == "garmin") %>% dplyr::arrange(date)
  expect_equal(rhr_g$date[5], garmin_days(10)[10])
  expect_true(is.na(rhr_g$mean_7d[5]))
})

test_that("HRV band: Garmin's own for Garmin, derived for Fitbit after 30 nights", {
  out <- build(vitals_inputs())
  hrv_g <- dplyr::filter(out, metric == "hrv", era == "garmin")
  expect_equal(unique(hrv_g$band_source), "garmin")
  expect_equal(unique(hrv_g$band_low), 52)

  hrv_f <- out %>% dplyr::filter(metric == "hrv", era == "fitbit") %>% dplyr::arrange(date)
  expect_true(all(is.na(hrv_f$band_low[1:30])))           # fewer than 30 previous nights
  expect_equal(unique(hrv_f$band_source[31:70]), "derived")
  # ln values alternate log(40) +/- 0.1: mean log(40), SD ~0.1
  last <- hrv_f[70, ]
  expect_equal(last$band_low, 40 * exp(-sd(rep(c(-0.1, 0.1), 30))), tolerance = 1e-3)
  expect_equal(last$band_high, 40 * exp(sd(rep(c(-0.1, 0.1), 30))), tolerance = 1e-3)
  # Only HRV gets a band
  expect_true(all(is.na(out$band_low[out$metric != "hrv"])))
})

test_that("Fitbit skin temperature becomes a derived deviation", {
  out <- build(vitals_inputs())
  st <- dplyr::filter(out, metric == "skin_temp_dev")
  expect_equal(unique(st$value[st$era == "fitbit"]), 0.2, tolerance = 1e-9)
  expect_true(all(st$derived[st$era == "fitbit"]))
  expect_false(any(st$derived[st$era == "garmin"]))
})

test_that("late wake-ups are listed, not moved", {
  inputs <- vitals_inputs()
  db <- withr::local_tempfile(fileext = ".duckdb")
  out <- build(inputs, db)
  con <- DBI::dbConnect(duckdb::duckdb(), db, read_only = TRUE)
  late <- DBI::dbReadTable(con, "vitals_late_wakes")
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_equal(late$date, garmin_days(1))
  expect_true(garmin_days(1) %in% out$date[out$metric == "sleep_s"])
})

test_that("overlapping eras and duplicate days stop the build", {
  overlap <- vitals_inputs()
  overlap$garmin_api_daily$date[1] <- switch_date            # same day as the last Fitbit day
  expect_error(build(overlap), "eras overlap")

  dup <- vitals_inputs()
  dup$fitbit_rhr_daily <- dplyr::bind_rows(dup$fitbit_rhr_daily, dup$fitbit_rhr_daily[1, ])
  expect_error(build(dup), "duplicate")
})

test_that("missing values and non-finite values are left out", {
  inputs <- vitals_inputs()
  inputs$fitbit_spo2_daily$spo2_avg[1:2] <- c(NA, Inf)
  out <- build(inputs)
  expect_equal(sum(out$metric == "spo2" & out$era == "fitbit"), 68)
})
