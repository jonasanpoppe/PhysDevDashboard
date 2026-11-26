# Loads the app the way runApp() does (shinyAppDir), against a synthetic
# database, and checks that the Vitals plot renders in both ranges. This is
# what caught functions sourced into the wrong environment.

test_that("the app starts and renders the Vitals plot", {
  dir <- withr::local_tempdir()
  fake <- write_fake_app_db(dir)
  oath <- fake$oath
  level <- fake$level
  db <- fake$db
  withr::local_envvar(PDD_DB = db, PDD_RELIC_ART = file.path(dir, "relics"))

  app <- shiny::shinyAppDir(testthat::test_path("..", "..", "app"))
  shiny::testServer(app, {
    # Home: the oath's copy, the main quest and a card per side quest
    home <- as.character(output$`oath-page`$html)
    expect_match(home, "The Wandering Squire")
    expect_match(home, "Judo now:")
    expect_match(home, sprintf("level %d<", level))
    expect_match(home, "New relic unlocked: [^<]*III")
    # Reliquary: a card per relic, sealed and unlocked, and the level path
    rel <- as.character(output$`reliquary-page`$html)
    expect_equal(lengths(regmatches(rel, gregexpr("class=\"relic-card ", rel))), length(oath$relics))
    expect_match(rel, "relic-card relic-unlocked")
    expect_match(rel, "relic-card relic-sealed")
    expect_match(rel, sprintf("Level %d", level))
    expect_match(rel, "the Chalice")
    # Roadmap: a block per phase, the today line, gates
    road <- as.character(output$`roadmap-page`$html)
    expect_equal(lengths(regmatches(road, gregexpr("class=\"road-phase ", road))), length(oath$roadmap$phases))
    expect_match(road, "road-today")
    expect_match(road, "road-done")
    expect_match(road, "<b>78 kg</b>")
    expect_match(road, "Gates:")
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
    expect_no_match(home, sprintf("level %d<", level))
    rel <- as.character(output$`reliquary-page`$html)
    expect_match(rel, "Milestones")
    expect_no_match(rel, "the Chalice")
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
