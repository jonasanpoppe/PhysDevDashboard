source(testthat::test_path("..", "..", "R", "run_sql.R"))

# sql/checks/ on a synthetic database: every source table the contract names,
# built from the contract itself, with a few rows of invented values.

checks_dir <- testthat::test_path("..", "..", "sql", "checks")

# Empty tables with exactly the expected columns and types
contract_db <- function(env = parent.frame()) {
  db <- withr::local_tempfile(fileext = ".duckdb", .local_envir = env)
  run_sql(file.path(checks_dir, "expected_columns.sql"), db_path = db)
  con <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  ddl <- DBI::dbGetQuery(con, "
    SELECT 'CREATE TABLE ' || table_name || ' (' ||
           string_agg(column_name || ' ' || data_type, ', ') || ')' AS sql
    FROM expected_columns GROUP BY table_name")$sql
  for (q in ddl) DBI::dbExecute(con, q)
  db
}

exec <- function(db, sql) {
  con <- DBI::dbConnect(duckdb::duckdb(), db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, sql)
}

test_that("a database that keeps the contract passes with no rows", {
  db <- contract_db()
  exec(db, "INSERT INTO fitbit_rhr_daily VALUES ('2030-01-01', 55), ('2030-01-02', 56)")
  expect_no_warning(out <- run_data_checks(checks_dir, db_path = db))
  expect_equal(nrow(out), 0)
})

test_that("a missing column or a changed type stops the build, naming it", {
  db <- contract_db()
  exec(db, "ALTER TABLE macrofactor_daily DROP COLUMN trend_weight_kg")
  expect_error(run_data_checks(checks_dir, db_path = db), "macrofactor_daily.trend_weight_kg is missing")

  db <- contract_db()
  exec(db, "ALTER TABLE fitbit_steps_daily ALTER steps TYPE VARCHAR")
  expect_error(run_data_checks(checks_dir, db_path = db), "fitbit_steps_daily.steps is VARCHAR, expected DOUBLE")

  db <- contract_db()
  exec(db, "DROP TABLE strava_laps")
  expect_error(run_data_checks(checks_dir, db_path = db), "strava_laps.lap_id is missing")
})

test_that("a duplicate date or ID stops the build", {
  db <- contract_db()
  exec(db, "INSERT INTO garmin_api_daily (date, ga_rhr) VALUES ('2030-01-01', 50), ('2030-01-01', 51)")
  expect_error(run_data_checks(checks_dir, db_path = db),
               "garmin_api_daily: date 2030-01-01 appears 2 times")

  db <- contract_db()
  exec(db, "INSERT INTO strava_activities (activity_id, date) VALUES ('7', '2030-01-01'), ('7', '2030-01-02')")
  expect_error(run_data_checks(checks_dir, db_path = db), "activity_id 7 appears 2 times")
})

test_that("implausible values are kept, listed and raised as a warning", {
  db <- contract_db()
  exec(db, "INSERT INTO fitbit_rhr_daily VALUES ('2030-01-01', 55), ('2030-01-02', 0)")
  exec(db, "INSERT INTO garmin_api_daily (date, ga_sleep_s) VALUES ('2030-01-03', 90000)")
  expect_warning(out <- run_data_checks(checks_dir, db_path = db), "2 implausible values kept")
  expect_equal(out$severity, c("warn", "warn"))
  expect_setequal(out$table_name, c("fitbit_rhr_daily", "garmin_api_daily"))
  expect_match(out$detail[out$table_name == "fitbit_rhr_daily"],
               "resting_heart_rate = 0.0 on 2030-01-02 \\(plausible 30 to 120\\)")
  # The value itself stays in its table
  con <- DBI::dbConnect(duckdb::duckdb(), db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM fitbit_rhr_daily")$n, 2)
})
