source(testthat::test_path("..", "..", "R", "run_sql.R"))

# The Garmin and Strava staging SQL on synthetic files (invented values, the
# fields the fetchers write).

sql_path <- function(name) testthat::test_path("..", "..", "sql", "staging", paste0(name, ".sql"))
write_json_to <- function(x, ...) jsonlite::write_json(x, file.path(...), auto_unbox = TRUE, null = "null", digits = NA)

test_that("Garmin: overlapping fetches give the first non-missing value per field", {
  dir <- withr::local_tempdir()
  write_json_to(list(
    list(date = "2030-01-01", rhr = 50, steps = NULL, im_moderate = 10, im_vigorous = 5),
    list(date = "2030-01-02", rhr = 51, steps = 8000, hrv_status = "BALANCED")
  ), dir, "2030-01-01_2030-01-31_garmin_api.json")
  # A later fetch: fills the missing steps, doesn't overwrite rhr, adds a field
  # the SQL doesn't know (left out)
  write_json_to(list(
    list(date = "2030-01-01", rhr = 99, steps = 9000, new_field = 1)
  ), dir, "2030-01-01_2030-02-28_garmin_api.json")

  out <- run_sql(sql_path("garmin_api_daily"), list(garmin_dir = dir), "garmin_api_daily",
                 db_path = file.path(dir, "t.duckdb"))
  expect_equal(out$date, as.Date(c("2030-01-01", "2030-01-02")))
  expect_equal(out$ga_rhr, c(50, 51))
  expect_equal(out$ga_steps, c(9000, 8000))
  expect_equal(out$ga_im_total, c(20, NA))          # vigorous minutes count double
  expect_equal(out$ga_hrv_status, c(NA, "BALANCED"))
  expect_false("ga_new_field" %in% names(out))
  expect_equal(names(out)[1:3], c("date", "ga_rhr", "ga_hrv"))
})

# A minimal Strava activity: summary and detail
strava_summary <- function(id, start, name = "Ride", sport = "Ride") {
  list(id = id, start_date = paste0(start, "Z"), start_date_local = paste0(start, "Z"),
       timezone = "(GMT+00:00) UTC", name = name, sport_type = sport,
       trainer = FALSE, commute = FALSE, manual = FALSE, distance = 30000, moving_time = 3600,
       elapsed_time = 3700, total_elevation_gain = 300, average_speed = 8.3, max_speed = 15,
       has_heartrate = TRUE, average_heartrate = 140, max_heartrate = 170, device_watts = FALSE)
}
strava_segment <- function(id, low = 10, high = 60) {
  list(id = id, name = paste("Segment", id), activity_type = "Ride", distance = 1000,
       average_grade = 5, maximum_grade = 9, elevation_low = low, elevation_high = high,
       climb_category = 0, city = "Town", country = "Country",
       start_latlng = list(50.1, 3.1), end_latlng = list(50.2, 3.2), hazardous = FALSE, private = FALSE)
}

test_that("Strava: exact 64-bit IDs, later list fetch wins, markers and empty lists left out", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "details")); dir.create(file.path(dir, "segments"))
  big <- "9007199254740993"   # 2^53 + 1: a double rounds it to 2^53
  write_json_to(list(strava_summary(101, "2030-03-01T09:00:00", name = "old name")),
                dir, "2030-03-01T120000Z_strava_activities.json")
  write_json_to(list(strava_summary(101, "2030-03-01T09:00:00", name = "new name"),
                     strava_summary(102, "2030-03-02T09:00:00", sport = "Run")),
                dir, "2030-03-03T120000Z_strava_activities.json")
  # The effort ID is written as a JSON number, as Strava does
  detail <- jsonlite::toJSON(list(
    id = 101, calories = 800,
    laps = list(list(id = 5, activity = list(id = 101), lap_index = 1,
                     start_date = "2030-03-01T08:00:00Z", start_date_local = "2030-03-01T09:00:00Z",
                     distance = 30000)),
    segment_efforts = list(
      list(id = "BIG", activity = list(id = 101), segment = strava_segment(7, low = 10, high = 30),
           start_date = "2030-03-01T08:10:00Z", start_date_local = "2030-03-01T09:10:00Z",
           elapsed_time = 100, distance = 1000),
      list(id = 8, activity = list(id = 101), segment = strava_segment(7, low = 10, high = 30),
           start_date = "2030-03-01T08:40:00Z", start_date_local = "2030-03-01T09:40:00Z",
           elapsed_time = 90, distance = 1000)),
    best_efforts = list()
  ), auto_unbox = TRUE)
  writeLines(sub('"BIG"', big, detail), file.path(dir, "details", "101.json"))
  write_json_to(list(id = 102, calories = 300,
                     best_efforts = list(list(id = 9, activity = list(id = 102), name = "1k",
                                              start_date = "2030-03-02T08:00:00Z",
                                              start_date_local = "2030-03-02T09:00:00Z",
                                              distance = 1000, elapsed_time = 240))),
                dir, "details", "102.json")
  write_json_to(list(`_status` = 404), dir, "details", "103.json")
  write_json_to(list(id = 7, total_elevation_gain = 55, starred = TRUE,
                     athlete_segment_stats = list(pr_elapsed_time = 90, pr_date = "2030-03-01", effort_count = 2),
                     map = list(polyline = "abc")),
                dir, "segments", "7.json")

  tables <- c("activities", "laps", "segment_efforts", "segments", "best_efforts")
  out <- run_sql(sql_path("strava_activities"), list(strava_dir = dir), paste0("strava_", tables),
                 db_path = file.path(dir, "t.duckdb"))

  a <- out$strava_activities
  expect_equal(a$activity_id, c("101", "102"))
  expect_equal(a$name, c("new name", "Ride"))
  expect_equal(a$kcal, c(800, 300))
  expect_equal(a$start_local[1], "2030-03-01T09:00:00")
  expect_equal(format(a$start_utc[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2030-03-01 09:00:00")
  expect_equal(a$date, as.Date(c("2030-03-01", "2030-03-02")))

  e <- out$strava_segment_efforts
  expect_equal(e$effort_id, c(big, "8"))
  expect_equal(e$segment_name, c("Segment 7", "Segment 7"))

  s <- out$strava_segments
  expect_equal(nrow(s), 1)
  expect_equal(s$my_efforts_loaded, 2)
  expect_equal(s$best_elapsed_s, 90)
  expect_equal(s$best_vam_mh, 20 / 90 * 3600)                    # 800 m/h
  expect_false(s$elev_suspect)
  expect_equal(s$pr_date, as.Date("2030-03-01"))
  expect_equal(c(s$start_lat, s$end_lng), c(50.1, 3.2))

  # Only real best efforts: none for the ride, one for the run
  expect_equal(out$strava_best_efforts$best_effort_id, "9")
  expect_equal(out$strava_laps$lap_id, "5")
})

test_that("Strava: a segment climbed faster than 1800 m/h is flagged as suspect", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "details")); dir.create(file.path(dir, "segments"))
  write_json_to(list(strava_summary(1, "2030-04-01T09:00:00")), dir, "2030-04-01T120000Z_strava_activities.json")
  write_json_to(list(id = 1, segment_efforts = list(
    list(id = 2, activity = list(id = 1), segment = strava_segment(3, low = 0, high = 100),
         start_date = "2030-04-01T08:00:00Z", start_date_local = "2030-04-01T09:00:00Z", elapsed_time = 100))),
    dir, "details", "1.json")
  write_json_to(list(id = 3, total_elevation_gain = 100), dir, "segments", "3.json")
  out <- run_sql(sql_path("strava_activities"), list(strava_dir = dir), "strava_segments",
                 db_path = file.path(dir, "t.duckdb"))
  expect_true(out$elev_suspect)   # 100 m in 100 s = 3600 m/h
})
