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

    # Climbs tab: the record table and its filters
    session$setInputs(`climbs-surface` = c("cobbles", "part_cobbles", "asphalt", "unknown"),
                      `climbs-bikes` = c("hybrid", "roadie"), `climbs-scope` = FALSE)
    html <- as.character(output$`climbs-table`$html)
    expect_match(html, "Pinned Muur")
    expect_match(html, "none yet")
    expect_equal(output$`climbs-summary`, "3 climbs \u00B7 5 ascents \u00B7 3 clean")
    session$setInputs(`climbs-scope` = TRUE)
    expect_equal(output$`climbs-summary`, "2 climbs \u00B7 3 ascents \u00B7 2 clean")
    session$setInputs(`climbs-surface` = "asphalt")
    expect_match(as.character(output$`climbs-table`$html), "No ascents with these filters")

    # Engine tab, one figure at a time. Power by duration: clean ascents
    # only, filters by year and short efforts
    session$setInputs(`engine-figure` = "power", `engine-years` = "2024",
                      `engine-range` = TRUE, `engine-short` = TRUE)
    expect_gt(nchar(output$`engine-plot`$src), 1000)
    expect_equal(output$`engine-summary`, "3 ascents \u00B7 2 climbs")
    expect_match(as.character(output$`engine-footer`$html), "FTP categories")
    session$setInputs(`engine-range` = FALSE)
    expect_gt(nchar(output$`engine-plot`$src), 1000)
    # Watts at the same heart rate: headline and notes follow the numbers
    session$setInputs(`engine-figure` = "fitness")
    expect_gt(nchar(output$`engine-plot`$src), 1000)
    expect_match(as.character(output$`engine-headline`$html), "340 W in 2031 against 360 W in 2030")
    expect_match(as.character(output$`engine-headline`$html), "includes zero")
    expect_match(as.character(output$`engine-footer`$html), "bootstrap intervals")
    # Training load, with the load check
    session$setInputs(`engine-figure` = "load")
    expect_gt(nchar(output$`engine-plot`$src), 1000)
    expect_match(as.character(output$`engine-footer`$html), "No clear effect")

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
