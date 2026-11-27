source(testthat::test_path("..", "..", "R", "run_sql.R"))

# The Fitbit staging SQL (sql/staging/fitbit_*.sql) on synthetic files with
# the real Takeout headers and invented values.

staging <- function(name, params, db = file.path(withr::local_tempdir(.local_envir = parent.frame()), "t.duckdb")) {
  run_sql(testthat::test_path("..", "..", "sql", "staging", paste0(name, ".sql")),
          params = params, read_back = name, db_path = db)
}

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
    out <- staging("fitbit_hrv_daily", list(fitbit_activity_dir = dir), db)
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
  out <- staging("fitbit_hrv_daily", list(fitbit_activity_dir = dir))
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
  out <- staging("fitbit_rr_daily", list(fitbit_activity_dir = dir), db)
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

  out <- staging("fitbit_sleep_daily", list(fitbit_sleep_dir = dir), db)
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
  expect_error(staging("fitbit_sleep_daily", list(fitbit_sleep_dir = dir)),
               "more than one main sleep on 2024-06-02")
})

test_that("Fitbit sleep: a nap on its own date isn't a main sleep", {
  dir <- withr::local_tempdir()
  night <- sleep_log(1, "2024-06-02", "2024-06-01T23:10:00.000", "2024-06-02T06:50:00.000")
  nap <- sleep_log(2, "2024-06-05", "2024-06-05T14:00:00.000", "2024-06-05T14:40:00.000", main = FALSE)
  jsonlite::write_json(list(night, nap), file.path(dir, "sleep-2024-06-01.json"), auto_unbox = TRUE)
  out <- staging("fitbit_sleep_daily", list(fitbit_sleep_dir = dir))
  expect_equal(out$date, as.Date("2024-06-02"))
  expect_equal(out$log_id, "1")
})

test_that("per-minute steps and distance are summed per date as written", {
  dir <- withr::local_tempdir()
  writeLines(c("timestamp,steps,data source",
               "2030-01-01T00:01:00Z,10,Charge",
               "2030-01-01T23:59:00Z,5,Charge",        # stays on its own date
               "2030-01-02T08:00:00Z,7,Charge"),
             file.path(dir, "steps_2030-01-01.csv"))
  writeLines("not a csv", file.path(dir, "steps_readme.txt"))
  writeLines(c("timestamp,distance,data source", "2030-01-01T10:00:00Z,12.5,Charge"),
             file.path(dir, "distance_2030-01-01.csv"))
  out <- staging("fitbit_steps_daily", list(fitbit_activity_dir = dir))
  expect_equal(out$date, as.Date(c("2030-01-01", "2030-01-02")))
  expect_equal(out$steps, c(15, 7))
  expect_equal(staging("fitbit_distance_daily", list(fitbit_activity_dir = dir))$distance, 12.5)
})

test_that("AZM: one column per zone, 0 when none, and an unknown zone stops the load", {
  dir <- withr::local_tempdir()
  writeLines(c("date_time,heart_zone_id,total_minutes",
               "2030-01-01T07:00,FAT_BURN,3",
               "2030-01-01T07:01,CARDIO,2",
               "2030-01-01T07:02,CARDIO,2",
               "2030-01-02T18:00,PEAK,1"),
             file.path(dir, "Active Zone Minutes - 2030-01-01.csv"))
  out <- staging("fitbit_azm_daily", list(fitbit_azm_dir = dir))
  expect_named(out, c("date", "azm_fat_burn", "azm_cardio", "azm_peak", "azm_total"))
  expect_equal(out$azm_cardio, c(4, 0))
  expect_equal(out$azm_total, c(7, 1))

  writeLines(c("date_time,heart_zone_id,total_minutes", "2030-02-01T07:00,ULTRA,1"),
             file.path(dir, "Active Zone Minutes - 2030-02-01.csv"))
  expect_error(staging("fitbit_azm_daily", list(fitbit_azm_dir = dir)), "unknown zone ULTRA")
})

test_that("temperature: Fitbit's NaN is read as missing", {
  dir <- withr::local_tempdir()
  writeLines(c("timestamp,nightly temperature celsius,baseline temperature celsius,relative nightly stddev 30d celsius,data source",
               "2030-01-01T00:00:00Z,33.1,NaN,NaN,Fitbit App",
               "2030-01-02T00:00:00Z,33.3,33.2,0.1,Fitbit App"),
             file.path(dir, "daily_sleep_temperature_derivations.csv"))
  out <- staging("fitbit_temperature_nightly", list(fitbit_activity_dir = dir))
  expect_equal(out$baseline_temperature_celsius, c(NA, 33.2))
  expect_false(any(is.nan(out$baseline_temperature_celsius)))
})
