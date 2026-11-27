# End to end on the synthetic MacroFactor export (fixtures/): the real
# loaders read it, and the weight tables build from it with the invented
# goals in config/goals.example.yml. This is what CI runs instead of real data.

root <- testthat::test_path("..", "..")
source(file.path(root, "fixtures", "macrofactor_schema.R"))
source(file.path(root, "fixtures", "make_macrofactor_fixture.R"))
for (f in c("find_macrofactor_exports", "load_macrofactor", "load_macrofactor_food_log",
            "e1rm", "load_macrofactor_workouts", "weight_plan")) {
  source(file.path(root, "R", paste0(f, ".R")))
}

load_fixture <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  make_macrofactor_fixture(dir)
  db <- file.path(dir, "fixture.duckdb")
  ex <- find_macrofactor_exports(dir)
  mf <- suppressMessages(load_macrofactor(ex$summary, db))
  list(dir = dir, db = db, exports = ex, mf = mf,
       food = suppressMessages(load_macrofactor_food_log(ex$food_log, db)),
       sets = suppressMessages(load_macrofactor_workouts(
         ex$workout_log, dplyr::select(mf$daily, date, bodyweight_kg = trend_weight_kg), db)))
}

test_that("the fixture has the real export's structure", {
  fx <- load_fixture()
  expect_named(fx$exports, c("summary", "food_log", "workout_log"))   # told apart by header
  expect_equal(ncol(fx$mf$daily), 1 + 4 + length(macrofactor_schema$micronutrients) + 2 +
                 length(macrofactor_schema$body_metrics) + 1 + 1 + 1)
  expect_true(all(c("trend_weight_kg", "weight_kg", "calories_kcal", "protein_g",
                    "expenditure", "steps") %in% names(fx$mf$daily)))
  expect_equal(ncol(fx$food), length(macrofactor_schema$food_log))
})

test_that("MacroFactor's double-encoded workout export is repaired", {
  fx <- load_fixture()
  expect_true("45° Leg Press" %in% fx$sets$exercise)             # degree sign repaired
  expect_true(all(!grepl("∈", fx$sets$exercise)))                 # superset mark split off
  expect_true(all(fx$sets$superset[fx$sets$exercise == "45° Leg Press"] == "SS1"))
  expect_true(any(!is.na(fx$sets$e1rm_kg)))
})

test_that("weight tables build from the fixture with the example goals", {
  fx <- load_fixture()
  goals <- yaml::read_yaml(file.path(root, "config", "goals.example.yml"))
  daily <- build_weight_daily(fx$mf$daily, goals$weight_goal)
  sessions <- build_sessions(tibble::tibble(date = as.Date(character()), sport_type = character()),
                             fx$sets, goals$sessions)
  weekly <- build_weight_weekly(daily, sessions, goals$weight_goal)
  status <- compute_weight_status(daily, sessions, goals$weight_goal, goals$nutrition, goals$sessions)
  expect_gt(sum(weekly$in_plan), 8)
  expect_true(all(sessions$source == "macrofactor"))
  expect_equal(status$lifting_per_week, 2)            # the fixture lifts Mon and Thu
  # The invented person loses ~0.4 kg a week, the example goal asks 0.4
  expect_equal(status$rate_recent, -0.4, tolerance = 0.15)
})
