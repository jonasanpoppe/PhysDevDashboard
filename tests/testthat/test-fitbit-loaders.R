source(testthat::test_path("..", "..", "R", "read_fitbit_daily.R"))
source(testthat::test_path("..", "..", "R", "load_hrv.R"))
source(testthat::test_path("..", "..", "R", "load_rr.R"))
source(testthat::test_path("..", "..", "R", "load_fitbit_sleep.R"))

# Synthetic files with the real Takeout headers and invented values.

write_lines_to <- function(dir, name, lines) {
  writeLines(lines, file.path(dir, name))
}

test_that("daily HRV keeps Fitbit's date label in any session timezone", {
  dir <- withr::local_tempdir()
  db <- file.path(dir, "test.duckdb")
  write_lines_to(dir, "daily_heart_rate_variability.csv", c(
    "timestamp,average heart rate variability milliseconds,non rem heart rate beats per minute,entropy,deep sleep root mean square of successive differences milliseconds,data source",
    "2024-03-30T00:00:00Z,40.1,55.0,3.1,45.0,Fitbit",
    "2024-03-31T00:00:00Z,42.5,54.0,3.0,47.0,Fitbit"   # DST change night in Europe
  ))
  # A per-sample file in the same folder must be ignored: its UTC timestamps
  # would split a night across two dates.
  write_lines_to(dir, "heart_rate_variability_2024-03-01.csv", c(
    "timestamp,root mean square of successive differences milliseconds,standard deviation milliseconds,data source",
    "2024-03-29T23:55:00Z,99,1,Fitbit"
  ))

  for (tz in c("UTC", "Pacific/Auckland", "America/Los_Angeles")) {
    withr::local_timezone(tz)
    out <- load_hrv(dir = dir, db_path = db)
    expect_equal(out$date, as.Date(c("2024-03-30", "2024-03-31")), info = tz)
  }
  expect_equal(out$hrv_rmssd, c(40.1, 42.5))
  expect_named(out, c("date", "hrv_rmssd"))
})

test_that("daily HRV of 0 is read as missing", {
  dir <- withr::local_tempdir()
  write_lines_to(dir, "daily_heart_rate_variability.csv", c(
    "timestamp,average heart rate variability milliseconds,non rem heart rate beats per minute,entropy,deep sleep root mean square of successive differences milliseconds,data source",
    "2024-04-01T00:00:00Z,0,56.0,0,0,Fitbit",
    "2024-04-02T00:00:00Z,41.0,55.0,3.0,45.0,Fitbit"
  ))
  out <- load_hrv(dir = dir, db_path = file.path(dir, "test.duckdb"))
  expect_equal(out$hrv_rmssd, c(NA, 41.0))
})

test_that("daily breathing rate comes from the daily file, not per-sleep summaries", {
  dir <- withr::local_tempdir()
  db <- file.path(dir, "test.duckdb")
  write_lines_to(dir, "daily_respiratory_rate.csv", c(
    "timestamp,breaths per minute,data source",
    "2024-05-02T00:00:00Z,14.5,Fitbit"
  ))
  # A nap summary on the same day must not be averaged in
  write_lines_to(dir, "respiratory_rate_sleep_summary_2024-05-01.csv", c(
    paste0("timestamp,full sleep stats - milli breaths per minute,data source"),
    "2024-05-02T13:30:00Z,20.0,Fitbit"
  ))
  out <- load_rr(dir = dir, db_path = db)
  expect_equal(out$date, as.Date("2024-05-02"))
  expect_equal(out$respiratory_rate, 14.5)
})

sleep_log <- function(id, date_of_sleep, start, end, main = TRUE, type = "stages") {
  summary <- if (type == "stages") {
    list(deep = list(count = 3, minutes = 60, thirtyDayAvgMinutes = 0),
         light = list(count = 20, minutes = 240, thirtyDayAvgMinutes = 0),
         rem = list(count = 5, minutes = 90, thirtyDayAvgMinutes = 0),
         wake = list(count = 15, minutes = 30, thirtyDayAvgMinutes = 0))
  } else {
    list(asleep = list(count = 0, minutes = 380),
         awake = list(count = 2, minutes = 5),
         restless = list(count = 8, minutes = 15))
  }
  list(logId = id, dateOfSleep = date_of_sleep, startTime = start, endTime = end,
       duration = 0, minutesToFallAsleep = 0, minutesAsleep = 390,
       minutesAwake = 30, minutesAfterWakeup = 0, timeInBed = 420,
       efficiency = 93, type = type, infoCode = 0, logType = "auto_detected",
       levels = list(summary = summary, data = list(), shortData = list()),
       mainSleep = main)
}

test_that("Fitbit sleep: wake-up day, naps out, overlapping files deduplicated", {
  dir <- withr::local_tempdir()
  db <- file.path(dir, "test.duckdb")
  night <- sleep_log(1, "2024-06-02", "2024-06-01T23:10:00.000", "2024-06-02T06:50:00.000")
  nap <- sleep_log(2, "2024-06-02", "2024-06-02T14:00:00.000", "2024-06-02T14:40:00.000", main = FALSE)
  classic <- sleep_log(3, "2024-06-03", "2024-06-02T23:30:00.000", "2024-06-03T07:00:00.000", type = "classic")
  # The night of 1-2 June appears in two monthly files, as in real exports
  jsonlite::write_json(list(night, nap), file.path(dir, "sleep-2024-05-15.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(night, classic), file.path(dir, "sleep-2024-06-01.json"), auto_unbox = TRUE)

  out <- load_fitbit_sleep(dir = dir, db_path = db)
  expect_equal(out$date, as.Date(c("2024-06-02", "2024-06-03")))  # started before midnight
  expect_equal(out$sleep_s, c(390, 390) * 60)
  expect_equal(out$sleep_deep_s, c(3600, NA))                      # classic: no stages
  expect_equal(out$log_type, c("stages", "classic"))
  expect_equal(out$sleep_end_local[1], "2024-06-02T06:50:00")
})

test_that("two main sleeps on one wake-up day stop the load", {
  dir <- withr::local_tempdir()
  a <- sleep_log(1, "2024-06-02", "2024-06-01T23:00:00.000", "2024-06-02T07:00:00.000")
  b <- sleep_log(2, "2024-06-02", "2024-06-02T08:00:00.000", "2024-06-02T09:00:00.000")
  jsonlite::write_json(list(a, b), file.path(dir, "sleep-2024-06-01.json"), auto_unbox = TRUE)
  expect_error(load_fitbit_sleep(dir = dir, db_path = file.path(dir, "t.duckdb")),
               "more than one main sleep")
})
