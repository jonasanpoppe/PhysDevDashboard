source(testthat::test_path("..", "..", "R", "run_sql.R"))
source(testthat::test_path("..", "..", "R", "e1rm.R"))
source(testthat::test_path("..", "..", "R", "load_macrofactor_workouts.R"))
source(testthat::test_path("..", "..", "R", "bodyweight.R"))
source(testthat::test_path("..", "..", "R", "strength.R"))

test_that("fitbit weight: grams to kg, local date in the home timezone, body fat joined by timestamp", {
  dir <- withr::local_tempdir()
  writeLines(c("timestamp,weight grams,data source",
               "2030-01-31T22:59:59Z,90500,Fitbit App",    # 23:59:59 local in Berlin
               "2030-07-01T08:00:00Z,88000,Fitbit App"),
             file.path(dir, "weight.csv"))
  writeLines(c("timestamp,body fat percentage,data source", "2030-01-31T22:59:59Z,20.0,Fitbit App"),
             file.path(dir, "body_fat_2030-01-01.csv"))
  db <- file.path(dir, "t.duckdb")
  out <- suppressMessages(run_sql(
    testthat::test_path("..", "..", "sql", "staging", "fitbit_weight.sql"),
    params = list(fitbit_activity_dir = dir, home_timezone = "Europe/Berlin"),
    read_back = "fitbit_weight", db_path = db
  ))
  expect_equal(as.Date(out$date), as.Date(c("2030-01-31", "2030-07-01")))
  expect_equal(out$weight_kg, c(90.5, 88))
  expect_equal(out$fat_percent, c(20, NA))
})

test_that("bodyweight for ratios: MacroFactor from its first day, Fitbit only before, labelled", {
  mf <- tibble::tibble(date = as.Date(c("2030-03-01", "2030-03-02")), trend_weight_kg = c(85, 84.9))
  fb <- tibble::tibble(date = as.Date(c("2030-01-10", "2030-01-10", "2030-03-02")), weight_kg = c(86, 87, 99))
  bw <- build_bodyweight_for_ratios(mf, fb)
  expect_equal(bw$date, as.Date(c("2030-01-10", "2030-03-01", "2030-03-02")))
  expect_equal(bw$bodyweight_kg, c(86.5, 85, 84.9))     # same-day weigh-ins averaged; MacroFactor wins
  expect_equal(bw$basis, c("fitbit scale", "macrofactor trend", "macrofactor trend"))
})

test_that("a weight older than the age limit isn't used", {
  sets <- tibble::tibble(date = as.Date(c("2030-01-05", "2030-03-01")))
  bw <- tibble::tibble(date = as.Date("2030-01-01"), bodyweight_kg = 80)
  expect_equal(join_bodyweight(sets, bw, max_age_days = 30)$bodyweight_kg, c(80, NA))
  expect_equal(join_bodyweight(sets, bw)$bodyweight_kg, c(80, 80))
})

test_that("strength sessions take bodyweight and its basis, within the age limit", {
  sets <- tibble::tibble(
    date = as.Date(c("2030-01-05", "2030-06-01")), workout = "A", exercise = "Barbell Bench Press",
    exercise_base_weight_kg = NA_real_, set_type = "Standard Set", weight_kg = 100, reps = 5, rir = 1,
    set_number = 1L, is_warmup = FALSE
  )
  lifts <- tibble::tibble(exercise = "Barbell Bench Press", group = "Bench", main = TRUE)
  bw <- tibble::tibble(date = as.Date("2030-01-01"), bodyweight_kg = 80, basis = "fitbit scale")
  s <- build_strength_sessions(sets, lifts, bw, max_bw_age_days = 45)
  expect_equal(s$bodyweight_kg, c(80, NA))
  expect_equal(s$bodyweight_basis, c("fitbit scale", NA))
  expect_equal(s$x_bodyweight[1], s$e1rm[1] / 80)
})
