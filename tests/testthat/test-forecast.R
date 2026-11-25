source(testthat::test_path("..", "..", "R", "forecast.R"))

test_that("plateau curve: starts at y0, approaches c; months_to inverts it", {
  expect_equal(plateau_curve(0, 150, 100, 6), 100)
  expect_equal(plateau_curve(1e4, 150, 100, 6), 150)
  m <- months_to(130, 150, 100, 6)
  expect_equal(plateau_curve(m, 150, 100, 6), 130)
  expect_equal(months_to(c(150, 160, 130), 150, 100, 6), c(Inf, Inf, m))
})

test_that("fit_plateau recovers y0 and tau from noisy simulated sessions", {
  set.seed(1)
  t <- seq(0, 8, length.out = 12)
  y <- plateau_curve(t, 150, 100, 6) + stats::rnorm(12, sd = 2)
  p <- fit_plateau(t, y, 150)
  expect_equal(unname(p["y0"]), 100, tolerance = 0.05)
  expect_equal(unname(p["tau"]), 6, tolerance = 0.35)
})

test_that("training_months caps gaps, counts from the start, and is plain months without gaps", {
  start <- as.Date("2030-01-01")
  expect_equal(training_months(start + c(10, 20, 30), start, gap_cap_days = 14), c(10, 20, 30) / MONTH_DAYS)
  expect_equal(training_months(start + c(10, 110), start, gap_cap_days = 14), c(10, 24) / MONTH_DAYS)
  expect_equal(training_months(start + 40, start, gap_cap_days = 14), 14 / MONTH_DAYS)   # a single session
  expect_equal(training_months(start + c(10, 110), start, gap_cap_days = Inf), c(10, 110) / MONTH_DAYS)
})

test_that("elite line and the attempt level", {
  el <- elite_line(80, 180)
  expect_equal(unname(el["bench"]), 427.14 * 80 / 180 - 14.75)
  expect_equal(attempt_level(1.2, 100, 0.96), 125)
})

test_that("time_to_target: from now on the curve's clock, never for targets above the ceiling", {
  params <- tibble::tibble(aptitude = c(0.7, 0.8), c = c(120, 150), y0 = 100, tau = 6, fitted = TRUE)
  tt <- time_to_target(params, 130, t_now = 2, from = as.Date("2030-01-01"))
  expect_equal(tt$months, c(Inf, months_to(130, 150, 100, 6) - 2))
  expect_true(is.na(tt$date[1]))
  expect_equal(tt$date[2], as.Date("2030-01-01") + round(tt$months[2] * MONTH_DAYS))
  # A target already passed: zero months
  expect_equal(time_to_target(params[2, ], 105, t_now = 5, from = as.Date("2030-01-01"))$months, 0)
})

test_that("first_round_curve: both shapes from the cached parameters", {
  par <- list(exp = list(c = 150, y0 = 60, tau = 9), log = list(a = 50, b = 40, s = 2))
  fr <- first_round_curve(par, as.Date("2030-01-01") + c(0, 3652), origin = "2030-01-01")
  expect_equal(fr$exponential[1], 60)
  expect_equal(fr$logarithmic[1], 50)
  expect_equal(fr$exponential[2], plateau_curve(3652 / MONTH_DAYS, 150, 60, 9))
})

fake_lift_sets <- function() {
  tibble::tibble(
    date = as.Date(c("2030-01-01", "2030-01-01", "2030-01-01", "2030-01-08", "2030-02-01")),
    exercise = "Barbell Bench Press",
    set_type = c("Warm-Up Set", "Standard Set", "Standard Set", "Standard Set", "Standard Set"),
    is_warmup = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    weight_kg = c(140, 100, 80, 90, 85), exercise_base_weight_kg = c(NA, NA, NA, NA, 20),
    reps = c(1, 1, 5, 3, 0), rir = c(NA, 1, 1, 2, 0),
    e1rm_kg = c(NA, 104, 95, 100, NA)
  )
}

test_that("on_the_bar: heaviest completed working set, base weight included, warm-ups ignored", {
  b <- on_the_bar(fake_lift_sets(), "Barbell Bench Press")
  expect_equal(c(b$load_kg, b$reps), c(100, 1))
  expect_equal(b$date, as.Date("2030-01-01"))
  expect_equal(nrow(on_the_bar(fake_lift_sets(), "Barbell Bench Press", since = "2030-03-01")), 0)
})

test_that("single ratio: a single's e1RM against the best 2-6-rep e1RM within the window", {
  pr <- single_ratio_pairs(fake_lift_sets(), "Barbell Bench Press", window_days = 14)
  expect_equal(nrow(pr), 1)
  expect_equal(pr$ratio, 104 / 100)    # same day (95) and a week later (100): the best counts
})
