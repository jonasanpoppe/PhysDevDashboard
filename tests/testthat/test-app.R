# Loads the app the way runApp() does (shinyAppDir), against a synthetic
# database, and checks that the Vitals plot renders in both ranges. This is
# what caught functions sourced into the wrong environment.

test_that("the app starts and renders the Vitals plot", {
  dir <- withr::local_tempdir()
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
  source(testthat::test_path("..", "..", "R", "weight_plan.R"), local = TRUE)
  wd <- build_weight_daily(fake_macrofactor_daily(), fake_goal())
  ws <- tibble::tibble(date = max(wd$date) - 1, kind = "bjj", source = "strava")
  DBI::dbWriteTable(con, "weight_daily", wd)
  DBI::dbWriteTable(con, "weight_weekly", build_weight_weekly(wd, ws, fake_goal()))
  DBI::dbWriteTable(con, "weight_status", compute_weight_status(
    wd, ws, fake_goal(), list(protein_g_per_day = 140),
    list(bjj = list(per_week = 2), lifting = list(per_week = 3))))
  DBI::dbWriteTable(con, "weight_goal", tibble::as_tibble(fake_goal()))
  source(testthat::test_path("..", "..", "R", "e1rm.R"), local = TRUE)
  source(testthat::test_path("..", "..", "R", "strength.R"), local = TRUE)
  sg <- strength_goal_tables(fake_lifts(), fake_strength_goal())
  DBI::dbWriteTable(con, "strength_sessions", build_strength_sessions(
    fake_sets(), fake_lifts(), tibble::tibble(date = as.Date("2030-01-01"), trend_kg = 80)))
  source(testthat::test_path("..", "..", "R", "forecast.R"), local = TRUE)
  fcc <- fake_forecast_config()
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
  DBI::dbWriteTable(con, "riding_targets", rt$targets)
  DBI::dbWriteTable(con, "riding_benchmark", tibble::tibble(benchmark_climb = "Example Berg"))
  DBI::dbWriteTable(con, "riding_plan_bands", tibble::tibble(z2_low = 125, z2_high = 135, ss_low = 145,
                                                             ss_high = 152, cap = 155))
  source(testthat::test_path("..", "..", "R", "analytics.R"), local = TRUE)
  ad <- fake_analytics_daily()
  DBI::dbWriteTable(con, "analytics_daily", ad)
  DBI::dbWriteTable(con, "analytics_variables", analytics_variables(ad))
  source(testthat::test_path("..", "..", "R", "quests.R"), local = TRUE)
  oath <- yaml::read_yaml(testthat::test_path("..", "..", "config", "oath.example.yml"))
  DBI::dbWriteTable(con, "quest_board", quest_board(oath, as_of = max(wd$date), data = list(
    weight = wd, strength = DBI::dbReadTable(con, "strength_sessions"), rides = rt$rides,
    sessions = ws, climbs = ct$efforts)))
  DBI::dbWriteTable(con, "oath_config", tibble::tibble(json = as.character(
    jsonlite::toJSON(oath, auto_unbox = TRUE, null = "null"))))
  DBI::dbDisconnect(con, shutdown = TRUE)
  withr::local_envvar(PDD_DB = db)

  app <- shiny::shinyAppDir(testthat::test_path("..", "..", "app"))
  shiny::testServer(app, {
    # Home: the oath's copy, the main quest and a card per side quest
    home <- as.character(output$`oath-page`$html)
    expect_match(home, "The Wandering Squire")
    expect_match(home, "Judo now:")
    expect_match(home, "level 1")
    expect_equal(lengths(regmatches(home, gregexpr("class=\"oath-card( oath-link)?\"", home))), 6)
    expect_equal(lengths(regmatches(home, gregexpr("oath-card oath-link", home))), 5)  # the Ledger has no tab
    expect_match(home, "of the way from")
    # A click on a card opens its tab with the view set
    session$setInputs(`oath-goto` = list(tab = "engine", inputs = list(view = "rides")))

    # Corporate mode: plain words on Home, neutral chart colours, and back
    session$setInputs(corporate = TRUE)
    home <- as.character(output$`oath-page`$html)
    expect_match(home, "2030 goals")
    expect_match(home, "oath-corporate")
    expect_no_match(home, "The Wandering Squire")
    expect_no_match(home, "level 1")
    expect_match(home, "Example Hill without stopping")
    session$setInputs(`vitals-metrics` = "rhr", `vitals-points` = TRUE, `vitals-range` = "all")
    expect_gt(nchar(output$`vitals-plot`$src), 1000)
    session$setInputs(corporate = FALSE)
    expect_match(as.character(output$`oath-page`$html), "The Wandering Squire")

    session$setInputs(`vitals-metrics` = "rhr", `vitals-points` = TRUE)
    for (range in c("90", "365", "all")) {
      session$setInputs(`vitals-range` = range)
      expect_gt(nchar(output$`vitals-plot`$src), 1000)
    }
    session$setInputs(`vitals-points` = FALSE)
    expect_gt(nchar(output$`vitals-plot`$src), 1000)
    # No metric picked: a message instead of an error
    session$setInputs(`vitals-metrics` = character(0))
    expect_error(output$`vitals-plot`, "at least one metric")

    # Engine: one view at a time. Climb record: the table and its filters
    session$setInputs(`engine-view` = "climbs",
                      `engine-climbs-surface` = c("cobbles", "part_cobbles", "asphalt", "unknown"),
                      `engine-climbs-bikes` = c("hybrid", "roadie"), `engine-climbs-scope` = FALSE)
    html <- as.character(output$`engine-climbs-table`$html)
    expect_match(html, "Pinned Muur")
    expect_match(html, "none yet")
    expect_equal(output$`engine-climbs-summary`, "3 climbs \u00B7 5 ascents \u00B7 3 clean")
    session$setInputs(`engine-climbs-scope` = TRUE)
    expect_equal(output$`engine-climbs-summary`, "2 climbs \u00B7 3 ascents \u00B7 2 clean")
    session$setInputs(`engine-climbs-surface` = "asphalt")
    expect_match(as.character(output$`engine-climbs-table`$html), "No ascents with these filters")

    # Power by duration: clean ascents only, filters by year and short efforts
    session$setInputs(`engine-view` = "power", `engine-power-years` = "2024",
                      `engine-power-range` = TRUE, `engine-power-short` = TRUE)
    expect_gt(nchar(output$`engine-power-plot`$src), 1000)
    expect_equal(output$`engine-power-summary`, "3 ascents \u00B7 2 climbs")
    session$setInputs(`engine-power-range` = FALSE)
    expect_gt(nchar(output$`engine-power-plot`$src), 1000)

    # Benchmark: the attempts on the benchmark climb
    expect_gt(nchar(output$`engine-benchmark-plot`$src), 1000)
    expect_match(output$`engine-benchmark-title`, "Example Berg \u00B7 3 attempts")

    # Watts at the same heart rate: headline and notes follow the numbers
    expect_gt(nchar(output$`engine-fitness-plot`$src), 1000)
    expect_match(as.character(output$`engine-fitness-headline`$html), "340 W in 2031 against 360 W in 2030")
    expect_match(as.character(output$`engine-fitness-headline`$html), "includes zero")
    expect_match(as.character(output$`engine-fitness-footer`$html), "bootstrap intervals")
    # Training load, with the load check
    expect_gt(nchar(output$`engine-load-plot`$src), 1000)
    expect_match(as.character(output$`engine-load-footer`$html), "No clear effect")

    # Rides: cumulative, monthly, best rides
    session$setInputs(`engine-rides-years` = c("2030", "2031"))
    for (v in c("cumulative", "monthly")) {
      session$setInputs(`engine-rides-view` = v)
      expect_gt(nchar(output$`engine-rides-plot`$src), 1000)
    }
    session$setInputs(`engine-rides-view` = "best")
    expect_match(as.character(output$`engine-rides-best`$html), "Longest")
    # Plan: weekly hours in the bands, the band limits in the footer
    session$setInputs(`engine-plan-period` = "all")
    expect_gt(nchar(output$`engine-plan-plot`$src), 1000)
    expect_match(output$`engine-plan-footer`, "125\u2013135 bpm")

    # Weight tab: tiles judged against the goals, then each figure in turn
    session$setInputs(`weight-figure` = "trend", `weight-period` = "plan", `weight-metric` = "protein_g")
    tiles <- as.character(output$`weight-tiles`$html)
    expect_match(tiles, "Trend weight")
    expect_match(tiles, "Goal: 140 g")
    for (f in c("trend", "weeks", "intake")) {
      session$setInputs(`weight-figure` = f)
      expect_gt(nchar(output$`weight-plot`$src), 1000)
      expect_match(output$`weight-footer`, ".")
    }

    # Strength tab: each figure, summary with the formula
    for (f in c("lifts", "targets", "standards")) {
      session$setInputs(`strength-figure` = f, `strength-group` = "Bench")
      expect_gt(nchar(output$`strength-plot`$src), 1000)
    }
    expect_match(output$`strength-summary`, "3 session bests \u00B7 Wathen formula")
    expect_match(as.character(output$`strength-footer`$html), "back squat")
    # Forecast: chart, both tables, the start value named in the footer
    session$setInputs(`strength-figure` = "forecast", `strength-fc_lift` = "bench", `strength-fc_aptitude` = 0.8,
                      `strength-fc_period` = "all", `strength-fc_history` = "monthly", `strength-fc_gap` = "14")
    expect_gt(nchar(output$`strength-plot`$src), 1000)
    below <- as.character(output$`strength-below`$html)
    expect_match(below, "Target 110 kg")
    expect_match(below, "96 kg \\(1.2 × 80 kg\\)")
    expect_match(as.character(output$`strength-footer`$html), "starts at 100 kg")

    # Analytics: scatter with its statistic, over time, the overview matrix
    session$setInputs(`analytics-view` = "scatter", `analytics-x` = "sleep_h@garmin",
                      `analytics-y` = "hrv@garmin", `analytics-lag` = 0,
                      `analytics-smooth` = "daily", `analytics-period` = "all")
    expect_gt(nchar(output$`analytics-plot`$src), 1000)
    expect_match(as.character(output$`analytics-stat`$html), "Spearman's rho 0\\.[89]")
    expect_match(as.character(output$`analytics-stat`$html), "Straight line: [0-9.]+ ms per h")
    for (f in c("quadratic", "log", "loess")) {
      session$setInputs(`analytics-fit` = f)
      expect_gt(nchar(output$`analytics-plot`$src), 1000)
    }
    expect_match(as.character(output$`analytics-stat`$html), "for the eye only")
    session$setInputs(`analytics-fit` = "log")
    expect_match(as.character(output$`analytics-stat`$html), "per doubling of X")
    session$setInputs(`analytics-fit` = "linear")
    session$setInputs(`analytics-y` = "hrv@fitbit")       # devices never overlap
    expect_error(output$`analytics-plot`, "Two devices never overlap")
    session$setInputs(`analytics-x` = "sleep_h@garmin", `analytics-y` = "sleep_h@garmin")
    expect_error(output$`analytics-plot`, "same quantity")
    session$setInputs(`analytics-view` = "time", `analytics-smooth` = "7d")
    expect_gt(nchar(output$`analytics-plot`$src), 1000)
    session$setInputs(`analytics-view` = "matrix", `analytics-lag` = 3)
    expect_gt(nchar(output$`analytics-plot`$src), 1000)
    expect_match(as.character(output$`analytics-top`$html), "Strongest pairs")
    expect_match(output$`analytics-footer`, "by chance")
  })
})
