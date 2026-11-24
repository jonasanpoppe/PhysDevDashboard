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
  DBI::dbWriteTable(con, "strength_targets", sg$targets)
  DBI::dbWriteTable(con, "strength_ceilings", sg$ceilings)
  rt <- fake_riding_tables()
  DBI::dbWriteTable(con, "strava_rides", rt$rides)
  DBI::dbWriteTable(con, "strava_plan_bands", rt$bands)
  DBI::dbWriteTable(con, "riding_targets", rt$targets)
  DBI::dbWriteTable(con, "riding_benchmark", tibble::tibble(benchmark_climb = "Example Berg"))
  DBI::dbWriteTable(con, "riding_plan_bands", tibble::tibble(z2_low = 125, z2_high = 135, ss_low = 145,
                                                             ss_high = 152, cap = 155))
  DBI::dbDisconnect(con, shutdown = TRUE)
  withr::local_envvar(PDD_DB = db)

  app <- shiny::shinyAppDir(testthat::test_path("..", "..", "app"))
  shiny::testServer(app, {
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
  })
})
