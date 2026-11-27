source(testthat::test_path("..", "..", "R", "homogenise.R"))

# Synthetic vitals (invented): Fitbit up to the switch, Garmin from the day
# after, a slow trend through both, and a known device step.
fake_eras <- function(step = -6, scale = "additive", trend_per_year = -2, n = 400, seed = 3,
                      gap = 1L) {
  withr::with_seed(seed, {
    sw <- as.Date("2030-01-01")
    f <- sw - rev(seq_len(n))
    g <- sw + gap - 1L + seq_len(n) - 1L
    t <- as.numeric(c(f, g) - sw) / 365.25
    era <- rep(c("fitbit", "garmin"), each = n)
    base <- 55 + trend_per_year * t + rnorm(2 * n, 0, 1.5)
    value <- if (scale == "log") {
      exp(log(45) + 0.03 * t + rnorm(2 * n, 0, 0.12) - (era == "fitbit") * step)
    } else {
      base - (era == "fitbit") * step
    }
    tibble::tibble(date = c(f, g), era = era, value = value)
  })
}

test_that("the step is recovered, with the trend through the switch not counted as device", {
  d <- fake_eras(step = -6, trend_per_year = -2)
  s <- estimate_era_step(d, "additive", window_days = 180)
  expect_equal(s$step, -6, tolerance = 0.1)
  expect_lt(s$step_low, -6); expect_gt(s$step_high, -6)
  expect_equal(s$slope_per_year, -2, tolerance = 0.5)
  expect_equal(s$gap_days, 1L)
  expect_equal(s$sd_fitbit, 1.5, tolerance = 0.15)
  expect_equal(s$noise_garmin, 1.5, tolerance = 0.15)          # white noise: day-to-day = level SD
})

test_that("log scale: the step is a ratio", {
  d <- fake_eras(step = 0.15, scale = "log")
  s <- estimate_era_step(d, "log", window_days = 180)
  expect_equal(s$step, 0.15, tolerance = 0.2)
  expect_equal(s$sd_fitbit, 0.12, tolerance = 0.15)
})

test_that("one era missing or too short: no step", {
  d <- fake_eras()
  expect_true(is.na(estimate_era_step(dplyr::filter(d, era == "fitbit"))$step))
  short <- dplyr::filter(d, era == "fitbit" | date < as.Date("2030-01-10"))
  expect_true(is.na(estimate_era_step(short)$step))
})

test_that("steps for every metric in the spec; applied only where asked and the eras meet", {
  v <- dplyr::bind_rows(
    dplyr::mutate(fake_eras(), metric = "rhr"),
    dplyr::mutate(fake_eras(scale = "log", step = 0.1), metric = "hrv"),
    dplyr::mutate(fake_eras(gap = 60L), metric = "resp"),
    dplyr::mutate(fake_eras(), metric = "sleep_s")
  )
  s <- estimate_era_steps(v, reps = 100)
  expect_setequal(s$metric, homogenise_spec$metric)
  expect_true(all(s$applied[s$metric %in% c("rhr", "hrv")]))
  expect_false(s$applied[s$metric == "resp"])
  expect_match(s$why_not[s$metric == "resp"], "60 days apart")
  expect_false(s$applied[s$metric == "sleep_s"])
  expect_match(s$why_not[s$metric == "spo2"], "too few days")       # no data at all
  expect_match(s$why_not[s$metric == "sleep_s"], "evaluated only")
})

test_that("homogenised: Fitbit moved by the step, Garmin unchanged, originals untouched", {
  v <- dplyr::bind_rows(
    dplyr::mutate(fake_eras(), metric = "rhr"),
    dplyr::mutate(fake_eras(scale = "log", step = 0.1), metric = "hrv")
  )
  s <- estimate_era_steps(v, reps = 100)
  h <- homogenise_vitals(v, s)
  expect_equal(unique(h$era), "homogenised")
  expect_equal(nrow(h), nrow(v))
  key <- c("date", "metric", "era")
  j <- dplyr::inner_join(h, dplyr::rename(v, source_era = era, original = value),
                         by = c("date", "metric", "source_era"))
  st <- setNames(s$step, s$metric)
  g <- dplyr::filter(j, source_era == "garmin")
  expect_equal(g$value, g$original)
  expect_equal(g$value_low, g$original)
  f_rhr <- dplyr::filter(j, source_era == "fitbit", metric == "rhr")
  expect_equal(f_rhr$value, f_rhr$original + st[["rhr"]])
  f_hrv <- dplyr::filter(j, source_era == "fitbit", metric == "hrv")
  expect_equal(f_hrv$value, f_hrv$original * exp(st[["hrv"]]))
  expect_true(all(f_rhr$value_low <= f_rhr$value & f_rhr$value <= f_rhr$value_high))
})

test_that("the note names the move and its interval", {
  v <- dplyr::mutate(fake_eras(), metric = "rhr")
  s <- estimate_era_steps(v, reps = 100)
  row <- tibble::tibble(variable = "rhr", era = "homogenised", series_label = "Resting HR · homogenised",
                        unit = "bpm")
  expect_match(homogenised_note(row, s), "Fitbit values −[0-9.]+ bpm \\(95% interval")
  expect_null(homogenised_note(dplyr::mutate(row, era = "garmin"), s))
  expect_null(homogenised_note(row, NULL))
})

test_that("steps walked: from device_steps, Fitbit scaled onto Garmin by a ratio", {
  d <- fake_eras(step = -0.25, scale = "log")         # Fitbit counts e^0.25 ≈ 1.28× Garmin
  dev <- dplyr::transmute(d, date, era, steps = value * 150)
  v <- era_series(tibble::tibble(date = as.Date(character()), metric = character(),
                                 era = character(), value = numeric()), dev)
  expect_equal(unique(v$metric), "steps")
  s <- estimate_era_steps(v, reps = 100)
  st <- s[s$metric == "steps", ]
  expect_true(st$applied)
  expect_equal(st$step, -0.25, tolerance = 0.2)       # Garmin minus Fitbit, on the log scale
  h <- homogenise_vitals(v, s)
  f <- dplyr::filter(h, source_era == "fitbit")
  expect_equal(f$value, dev$steps[dev$era == "fitbit"] * exp(st$step))
  expect_equal(h$value[h$source_era == "garmin"], dev$steps[dev$era == "garmin"])   # Garmin as measured
})
