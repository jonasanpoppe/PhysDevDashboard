source(testthat::test_path("..", "..", "R", "run_sql.R"))
sql_file <- function(...) testthat::test_path("..", "..", "sql", ...)

test_that("render_sql fills placeholders as quoted literals and refuses typos", {
  expect_equal(render_sql("SELECT {{dir}} || '/x.csv'", list(dir = "a b/it's")),
               "SELECT 'a b/it''s' || '/x.csv'")
  expect_error(render_sql("SELECT {{dir}}", list()), "no value for \\{\\{dir\\}\\}")
  expect_error(render_sql("SELECT 1", list(dir = "x")), "not used in the SQL: dir")
})

test_that("fitbit calories: per-minute rows summed per day, zone files left out", {
  dir <- file.path(withr::local_tempdir(), "Physical Activity's data")
  dir.create(dir)
  writeLines(c("timestamp,calories,data source",
               "2030-01-01T00:01:00Z,1.5,Fitbit App",
               "2030-01-01T23:59:00Z,2.5,Fitbit App",       # stays on its own date
               "2030-01-02T08:00:00Z,3,Fitbit App"),
             file.path(dir, "calories_2030-01-01.csv"))
  writeLines(c("timestamp,calories,data source", "2030-02-01T10:00:00Z,4,Fitbit App"),
             file.path(dir, "calories_2030-02-01.csv"))
  writeLines(c("timestamp,heart rate zone type,kcal,data source",
               "2030-01-01T00:01:00Z,LIGHT,100,Fitbit App"),
             file.path(dir, "calories_in_heart_rate_zone_2030-01-01.csv"))
  db <- file.path(withr::local_tempdir(), "t.duckdb")

  out <- run_sql(sql_file("staging", "fitbit_calories_daily.sql"),
                 params = list(fitbit_activity_dir = dir), read_back = "fitbit_calories_daily", db_path = db)
  expect_equal(as.Date(out$date), as.Date(c("2030-01-01", "2030-01-02", "2030-02-01")))
  expect_equal(out$calories, c(4, 3, 4))
  expect_equal(names(out), c("date", "calories"))
})
