# A synthetic database for the app tests (test-app.R, test-app-snapshots.R):
# every table the tabs read, from the fixtures in the other helpers and the
# example configs. Returns the path, the example oath config and the level.
write_fake_app_db <- function(dir) {
  db <- file.path(dir, "app.duckdb")
  days <- as.Date("2024-01-01") + 0:199
  vitals <- tibble::tibble(
    date = days, metric = "rhr", era = ifelse(days < as.Date("2024-04-01"), "fitbit", "garmin"),
    value = 55, derived = FALSE, mean_7d = 55,
    band_low = NA_real_, band_high = NA_real_, band_source = NA_character_
  )
  ct <- fake_climb_tables()
  con <- DBI::dbConnect(duckdb::duckdb(), db)
  DBI::dbWriteTable(con, "vitals_daily", vitals)
  DBI::dbWriteTable(con, "strava_climb_efforts", ct$efforts)
  DBI::dbWriteTable(con, "climbs", ct$climbs)
  DBI::dbWriteTable(con, "bikes", ct$bikes)
  ft <- fake_fitness_tables()
  DBI::dbWriteTable(con, "fitness_seasons", ft$seasons)
  DBI::dbWriteTable(con, "fitness_coefficients", ft$coefs)
  DBI::dbWriteTable(con, "fitness_load_check", ft$check)
  DBI::dbWriteTable(con, "strava_daily_load", ft$load)
  DBI::dbWriteTable(con, "rider_weight", ft$weight)
  # Every table from the same example configs (goals.example.yml, the
  # resolved oath.example.yml), so the tabs agree as on real data: the
  # consistency check (R/consistency.R) runs on this database too
  cfgs <- example_configs()
  oath <- cfgs$oath
  source(testthat::test_path("..", "..", "R", "weight_plan.R"), local = TRUE)
  wd <- build_weight_daily(fake_macrofactor_daily(), fake_goal())
  ws <- tibble::tibble(date = max(wd$date) - 1, kind = "bjj", source = "strava")
  DBI::dbWriteTable(con, "weight_daily", wd)
  DBI::dbWriteTable(con, "weight_weekly", build_weight_weekly(
    wd, ws, fake_goal(), device_steps = tibble::tibble(date = wd$date, era = "garmin", steps = 9000)))
  DBI::dbWriteTable(con, "weight_status", compute_weight_status(
    wd, ws, fake_goal(), list(protein_g_per_day = 140), cfgs$goals$sessions,
    as_of = max(wd$date), adherence = oath$adherence_met, recent_weeks = 8))
  DBI::dbWriteTable(con, "weight_goal", tibble::as_tibble(fake_goal()))
  source(testthat::test_path("..", "..", "R", "e1rm.R"), local = TRUE)
  source(testthat::test_path("..", "..", "R", "plan_config.R"), local = TRUE)
  source(testthat::test_path("..", "..", "R", "strength.R"), local = TRUE)
  sg <- strength_goal_tables(cfgs$lifts, cfgs$goals$strength, goals = cfgs$goals)
  sg$ceilings <- strength_goal_tables(fake_lifts(), fake_strength_goal())$ceilings   # fixed bands, for the chart
  DBI::dbWriteTable(con, "strength_sessions", build_strength_sessions(
    fake_sets(), fake_lifts(), tibble::tibble(date = as.Date("2030-01-01"), trend_kg = 80)))
  source(testthat::test_path("..", "..", "R", "forecast.R"), local = TRUE)
  fcc <- fake_forecast_config()
  fcc$strength_forecast$targets_kg <- cfgs$goals$strength_forecast$targets_kg["bench"]   # from the target on the bar
  fct <- build_strength_forecast(
    build_strength_sessions(fake_sets(), fake_lifts(), tibble::tibble(date = as.Date("2030-01-01"), trend_kg = 80)),
    dplyr::mutate(fake_sets(), is_warmup = set_type == "Warm-Up Set"),
    fcc$strength_forecast, fcc$strength_ceilings
  )
  for (n in names(fct)) DBI::dbWriteTable(con, paste0("strength_forecast_", n), fct[[n]])
  DBI::dbWriteTable(con, "strength_targets", sg$targets)
  DBI::dbWriteTable(con, "strength_ceilings", sg$ceilings)
  rt <- fake_riding_tables()
  DBI::dbWriteTable(con, "strava_rides", rt$rides)
  DBI::dbWriteTable(con, "strava_plan_bands", rt$bands)
  # The example's year from goals.example.yml, plus the fake rides' own year
  DBI::dbWriteTable(con, "riding_targets", dplyr::bind_rows(
    tibble::tibble(year = as.integer(names(cfgs$goals$riding$annual_km)), target_km = unlist(cfgs$goals$riding$annual_km)),
    rt$targets))
  DBI::dbWriteTable(con, "riding_benchmark", tibble::tibble(benchmark_climb = "Example Berg"))
  DBI::dbWriteTable(con, "riding_plan_bands", tibble::tibble(z2_low = 125, z2_high = 135, ss_low = 145,
                                                             ss_high = 152, cap = 155))
  source(testthat::test_path("..", "..", "R", "analytics.R"), local = TRUE)
  ad <- fake_analytics_daily()
  DBI::dbWriteTable(con, "analytics_daily", ad)
  DBI::dbWriteTable(con, "analytics_variables", analytics_variables(ad))
  source(testthat::test_path("..", "..", "R", "quests.R"), local = TRUE)
  DBI::dbWriteTable(con, "quest_board", quest_board(oath, as_of = max(wd$date), data = list(
    weight = wd, strength = DBI::dbReadTable(con, "strength_sessions"), rides = rt$rides,
    sessions = ws, climbs = ct$efforts)))
  DBI::dbWriteTable(con, "oath_config", tibble::tibble(json = as.character(
    jsonlite::toJSON(oath, auto_unbox = TRUE, null = "null"))))
  # The Reliquary: relic III logged by hand, so one card is unlocked (no art)
  source(testthat::test_path("..", "..", "R", "relics.R"), local = TRUE)
  relics <- relic_status(oath, as_of = max(wd$date), data = list(
    weight = wd, weekly = DBI::dbReadTable(con, "weight_weekly"),
    sets = tibble::tibble(date = as.Date(character()), exercise = character(), reps = numeric(),
                          rir = numeric(), is_warmup = logical()),
    rides = rt$rides, climbs = ct$efforts,
    events = tibble::tibble(date = min(wd$date), relic = "III", note = "test")))
  DBI::dbWriteTable(con, "relic_status", relics)
  level <- relic_level(relics, oath)
  source(testthat::test_path("..", "..", "R", "roadmap.R"), local = TRUE)
  DBI::dbWriteTable(con, "roadmap_phases", roadmap_phases(oath, as_of = max(wd$date)))
  DBI::dbWriteTable(con, "roadmap_gates", roadmap_gates(oath, relics))
  # Scenarios with the example's default assumptions (the Home chips)
  source(testthat::test_path("..", "..", "R", "scenarios.R"), local = TRUE)
  events <- tibble::tibble(date = min(wd$date), relic = "III", note = "test")
  DBI::dbWriteTable(con, "relic_events", events)
  DBI::dbWriteTable(con, "sessions_daily", ws)
  sc <- quest_scenarios(oath, as_of = max(wd$date), data = list(
    weight = wd, strength = DBI::dbReadTable(con, "strength_sessions"), rides = rt$rides,
    sessions = ws, climbs = ct$efforts, events = events,
    weekly = DBI::dbReadTable(con, "weight_weekly"), goal = fake_goal(),
    forecast = list(params = fct$params, curve = fct$curve)))
  DBI::dbWriteTable(con, "quest_scenarios", sc)
  DBI::dbDisconnect(con, shutdown = TRUE)
  list(db = db, oath = oath, level = level, scenarios = sc)
}
