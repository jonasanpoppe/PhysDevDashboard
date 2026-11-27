# `just build-fixture`: a synthetic MacroFactor export into fixtures/landing/,
# loaded with the real loaders into db/fixture.db (never db/health.db), plus
# the weight tables with the invented goals of config/goals.example.yml.
# Needs no personal data or config, so it runs in a fresh clone and in CI.

suppressMessages({
  source("fixtures/macrofactor_schema.R")
  source("fixtures/make_macrofactor_fixture.R")
  for (f in c("find_macrofactor_exports", "load_macrofactor", "load_macrofactor_food_log",
              "e1rm", "load_macrofactor_workouts", "weight_plan", "common")) {
    source(file.path("R", paste0(f, ".R")))
  }
})

landing <- "fixtures/landing/macrofactor"
db <- "db/fixture.db"
unlink(landing, recursive = TRUE)
dir.create("db", showWarnings = FALSE)
if (file.exists(db)) file.remove(db)

make_macrofactor_fixture(landing)
ex <- find_macrofactor_exports(landing)
mf <- load_macrofactor(ex$summary, db)
load_macrofactor_food_log(ex$food_log, db)
sets <- load_macrofactor_workouts(ex$workout_log,
                                  dplyr::select(mf$daily, date, bodyweight_kg = trend_weight_kg), db)

goals <- yaml::read_yaml("config/goals.example.yml")
daily <- build_weight_daily(mf$daily, goals$weight_goal)
sessions <- build_sessions(tibble::tibble(date = as.Date(character()), sport_type = character()),
                           sets, goals$sessions)
weekly <- build_weight_weekly(daily, sessions, goals$weight_goal)
status <- compute_weight_status(daily, sessions, goals$weight_goal, goals$nutrition, goals$sessions)
write_weight_tables(daily, weekly, sessions, status, goal = goals$weight_goal, db_path = db)
message("✓ fixture database: ", db)
