# Loads the app the way runApp() does (shinyAppDir), against a synthetic
# database, and checks that the Vitals plot renders in both ranges. This is
# what caught functions sourced into the wrong environment.

test_that("the app starts and renders the Vitals plot", {
  dir <- withr::local_tempdir()
  fake <- write_fake_app_db(dir)
  oath <- fake$oath
  level <- fake$level
  db <- fake$db
  dir.create(file.path(dir, "relics"))   # an art folder without cards: the note asks for one
  withr::local_envvar(PDD_DB = db, PDD_RELIC_ART = file.path(dir, "relics"))
  # Attached after shiny, as Posit Connect's launcher does: its validate()
  # must not replace Shiny's (app.R binds Shiny's)
  withr::local_package("shiny"); withr::local_package("jsonlite")

  pages_file <- normalizePath(testthat::test_path("..", "..", "app", "pages.R"))   # testServer runs in app/
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
    # Home: a verdict chip per quest with a scenario
    expect_match(home, "class=\"verdict verdict-")
    # Scenarios: a verdict per quest, a figure per quest kind, live assumptions
    sc_html <- as.character(output$`scenarios-verdicts`$html)
    expect_equal(lengths(regmatches(sc_html, gregexpr("<tr>", sc_html))), length(oath$quests) + 1)
    expect_match(sc_html, "Projections are model outputs")
    for (q in c("bodyweight", "bench", "riding")) {
      session$setInputs(`scenarios-quest` = q)
      expect_gt(nchar(output$`scenarios-plot`$src), 1000)
      expect_gt(nchar(output$`scenarios-footer`), 50)
    }
    session$setInputs(`scenarios-quest` = "squat")             # no forecast for this lift in the fixture
    expect_error(output$`scenarios-plot`, "No strength forecast")
    session$setInputs(`scenarios-quest` = "riding")
    expect_equal(output$`scenarios-kind`, "ride_km_ytd")
    session$setInputs(`scenarios-quest` = "bodyweight", `scenarios-rate` = 0.2, `scenarios-adherence` = 0.5)
    slow <- as.character(output$`scenarios-verdicts`$html)
    session$setInputs(`scenarios-rate` = 1, `scenarios-adherence` = 1)
    fast <- as.character(output$`scenarios-verdicts`$html)
    expect_false(identical(slow, fast))                     # a slider moves the projection
    # Roadmap: a block per phase, the today line, gates
    road <- as.character(output$`roadmap-page`$html)
    expect_equal(lengths(regmatches(road, gregexpr("class=\"road-phase ", road))), length(oath$roadmap$phases))
    expect_match(road, "road-today")
    expect_match(road, "road-current")
    expect_match(road, "<b>78 kg</b>")
    expect_match(road, "Gates:")
    expect_equal(lengths(regmatches(home, gregexpr("class=\"oath-card( oath-link)?\"", home))), 6)
    expect_equal(lengths(regmatches(home, gregexpr("oath-card oath-link", home))), 5)  # the Ledger has no tab
    expect_match(home, "of the way from")
    # A click on a card opens its tab with the view set
    session$setInputs(`oath-goto` = list(tab = "engine", inputs = list(view = "rides")))
    # Every quest links to a page the app has
    con <- DBI::dbConnect(duckdb::duckdb(), db, read_only = TRUE)
    links <- DBI::dbGetQuery(con, "SELECT link_tab FROM quest_board WHERE link_tab IS NOT NULL")$link_tab
    DBI::dbDisconnect(con, shutdown = TRUE)
    expect_true(length(links) > 0)
    source(pages_file, local = TRUE)
    expect_true(all(links %in% names(app_pages)))
    # Home as the hub: Today (level, phase, next gate), the relic shelf, the
    # links to the full pages; the tenets folded
    expect_match(home, "oath-today")
    expect_match(home, sprintf("Level %d<", level))
    expect_match(home, "Next gate: ")
    expect_equal(lengths(regmatches(home, gregexpr("class=\"shelf-relic ", home))), length(oath$relics))
    for (page in c("roadmap", "scenarios", "reliquary")) expect_match(home, sprintf("tab: (&#39;|')%s", page))
    expect_match(home, "<details class=\"oath-tenets-fold\">")
    # The Reliquary's footer no longer carries the card prompt
    expect_no_match(rel, "Card prompt")

    # Corporate mode: plain words on Home, neutral chart colours, and back
    session$setInputs(corporate = TRUE)
    home <- as.character(output$`oath-page`$html)
    expect_match(home, "2030 goals")
    expect_match(home, "oath-corporate")
    expect_match(home, "no catch-up after a missed week")   # the rules, not their plain title twice
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
    expect_match(below, "Target 100 kg on the bar \\(e1RM 104.2\\)")   # goals.example: 100 on the bar / 0.96
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
