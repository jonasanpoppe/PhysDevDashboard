# Golden test on real data (local only): the strength sessions match the
# reference dashboard's e1rm.py. Expected values live in
# private/golden_strength.json (gitignored), the sets in db/health.db and the
# lift groups in config/lifts.csv; without any of them the test skips.

root <- testthat::test_path("..", "..")
golden_strength <- file.path(root, "private", "golden_strength.json")
db_path <- file.path(root, "db", "health.db")
lifts_path <- file.path(root, "config", "lifts.csv")

test_that("golden: every reference session best and top set is reproduced", {
  testthat::skip_if_not(file.exists(golden_strength), "no private/golden_strength.json")
  testthat::skip_if_not(file.exists(db_path), "no db/health.db (run just build)")
  testthat::skip_if_not(file.exists(lifts_path), "no config/lifts.csv")
  source(file.path(root, "R", "e1rm.R"))
  source(file.path(root, "R", "strength.R"))

  ref <- jsonlite::read_json(golden_strength, simplifyVector = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  sets <- DBI::dbReadTable(con, "macrofactor_workout_sets")
  weights <- DBI::dbReadTable(con, "rider_weight")
  DBI::dbDisconnect(con, shutdown = TRUE)
  lifts <- readr::read_csv(lifts_path, show_col_types = FALSE)

  ours <- build_strength_sessions(
    sets[as.Date(sets$date) <= as.Date(ref$until), ], lifts, weights,
    settings = list(e1rm_formula = ref$formula, max_rir = ref$max_rir)
  )
  ours <- ours[ours$lift %in% ref$rows$lift, ]
  j <- merge(ref$rows, transform(ours, date = as.character(date)),
             by = c("date", "lift"), all = TRUE, suffixes = c("_ref", "_ours"))
  expect_equal(sum(is.na(j$e1rm_ours)), 0, info = "sessions missing from ours")
  expect_equal(sum(is.na(j$e1rm_ref)), 0, info = "sessions missing from the reference")
  expect_equal(j$e1rm_ours, j$e1rm_ref, tolerance = 0.051, scale = 1)
  expect_equal(j$top_set_ours, j$top_set_ref)
  expect_equal(j$group_ours, j$group_ref)
})
