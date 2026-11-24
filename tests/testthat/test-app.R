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
  })
})
