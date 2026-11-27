source(testthat::test_path("..", "..", "R", "run_sql.R"))

# sql/marts/device_steps.sql on synthetic staging tables with invented values.

build_steps <- function(fitbit, garmin, db = withr::local_tempfile(fileext = ".duckdb", .local_envir = parent.frame())) {
  con <- DBI::dbConnect(duckdb::duckdb(), db)
  DBI::dbWriteTable(con, "fitbit_steps_daily", fitbit, overwrite = TRUE)
  DBI::dbWriteTable(con, "garmin_api_daily", garmin, overwrite = TRUE)
  DBI::dbDisconnect(con, shutdown = TRUE)
  suppressMessages(run_sql(testthat::test_path("..", "..", "sql", "marts", "device_steps.sql"),
                           read_back = "device_steps", db_path = db))
}

test_that("one row a day, Garmin from its first day with steps, Fitbit before it", {
  d0 <- as.Date("2030-03-01")
  fitbit <- tibble::tibble(date = d0 + 0:5, steps = c(4000, 5000, 6000, 7000, 8000, 9000))
  # Garmin rows exist before its first step count (other fields fetched): they don't move the switch
  garmin <- tibble::tibble(date = d0 + 2:8, ga_steps = c(NA, NA, 7100, 8200, 9300, NA, 6000))
  out <- build_steps(fitbit, garmin)
  expect_equal(nrow(dplyr::count(out, date) %>% dplyr::filter(n > 1)), 0)
  expect_equal(out$era[out$date < d0 + 4], rep("fitbit", 4))
  expect_equal(out$steps[out$date == d0 + 4], 7100)          # switch day: Garmin's count
  expect_equal(out$era[out$date >= d0 + 4], rep("garmin", 4))
  expect_false(any(out$date == d0 + 7))                      # no count: left out, not 0
})

test_that("Fitbit alone, before any Garmin steps, is kept whole", {
  d0 <- as.Date("2030-03-01")
  out <- build_steps(tibble::tibble(date = d0 + 0:2, steps = 5000),
                     tibble::tibble(date = d0, ga_steps = NA_real_))
  expect_equal(out$era, rep("fitbit", 3))
})
