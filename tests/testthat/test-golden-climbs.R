# Golden tests on real data: local only.
#
# The expected values are personal results, so they live in
# private/golden_climbs.yml (gitignored), and the data in db/health.db (built
# by `just build`). Without either, every test here skips: in CI and in a
# fresh clone nothing runs and nothing personal is needed.
#
# A clean ascent, as in the climb record: status clean or top (a stop only
# at the very top still counts, see top_frac in config/athlete.yml).

# The ride types the pipeline counts as rides (R/load_strava_api.R), repeated
# here so this file runs without sourcing the loaders.
RIDE_TYPES_GOLDEN <- c("Ride", "GravelRide", "MountainBikeRide", "VirtualRide",
                       "EBikeRide", "EMountainBikeRide", "Velomobile", "Handcycle")

golden_path <- testthat::test_path("..", "..", "private", "golden_climbs.yml")
db_path <- testthat::test_path("..", "..", "db", "health.db")

skip_without_golden <- function() {
  testthat::skip_if_not(file.exists(golden_path), "no private/golden_climbs.yml")
  testthat::skip_if_not(file.exists(db_path), "no db/health.db (run just build)")
}

golden_query <- function(sql) {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbGetQuery(con, sql)
}

in_scope <- function(g) {
  e <- golden_query(sprintf(
    "SELECT * FROM strava_climb_efforts WHERE date >= DATE '%s' AND ride_gain_m >= %s",
    g$scope$since, g$scope$min_ride_gain_m
  ))
  e[!e$climb %in% unlist(g$scope$exclude_climbs), ]
}

test_that("golden: the scope has the expected rides, ascents and climbs", {
  skip_without_golden()
  g <- yaml::read_yaml(golden_path)
  rides <- golden_query(sprintf(
    "SELECT count(*) AS n FROM strava_activities
     WHERE date >= DATE '%s' AND elev_gain_m >= %s AND sport_type IN (%s)",
    g$scope$since, g$scope$min_ride_gain_m,
    paste0("'", RIDE_TYPES_GOLDEN, "'", collapse = ", ")
  ))$n
  e <- in_scope(g)
  expect_equal(rides, g$scope$rides)
  expect_equal(nrow(e), g$scope$ascents)
  expect_equal(length(unique(e$activity_id)), g$scope$rides_with_efforts)
  expect_equal(length(unique(e$climb)), g$scope$climbs)
})

test_that("golden: per climb, ascents, clean ascents and the best time", {
  skip_without_golden()
  g <- yaml::read_yaml(golden_path)
  e <- in_scope(g)
  for (name in names(g$climbs)) {
    want <- g$climbs[[name]]
    got <- e[e$climb == name, ]
    clean <- got$status %in% c("clean", "top")
    expect_equal(nrow(got), want$ascents, info = name)
    expect_equal(sum(clean), want$clean, info = name)
    if (!is.null(want$best_s)) {
      expect_equal(min(got$moving_s[clean]), want$best_s,
                   tolerance = g$time_tolerance_s, info = name, scale = 1)
    }
    if (!is.null(want$best_wkg)) {
      best <- got[clean, ][which.min(got$moving_s[clean]), ]
      expect_equal(best$wkg, want$best_wkg, tolerance = g$wkg_tolerance, scale = 1, info = name)
    }
  }
})

test_that("golden: single efforts", {
  skip_without_golden()
  g <- yaml::read_yaml(golden_path)
  e <- golden_query("SELECT * FROM strava_climb_efforts")
  for (want in g$efforts) {
    got <- e[as.character(e$date) == want$date & e$climb == want$climb, ]
    label <- paste(want$date, want$climb)
    expect_equal(nrow(got), 1, info = label)
    if (!is.null(want$stop_s)) expect_equal(got$stop_s, want$stop_s, tolerance = 1, scale = 1, info = label)
    if (!is.null(want$status)) expect_equal(got$status, want$status, info = label)
    if (!is.null(want$power_w)) {
      expect_equal(got$power_w, want$power_w, tolerance = g$power_tolerance, info = label)
    }
  }
})

test_that("golden: watts at the same heart rate per season", {
  skip_without_golden()
  g <- yaml::read_yaml(golden_path)
  skip_if(is.null(g$fitness), "no fitness block in the golden file")
  s <- golden_query("SELECT * FROM fitness_seasons")
  for (want in g$fitness$seasons) {
    w <- s$watts_ref[s$season == want$season]
    expect_true(length(w) == 1 && w >= want$watts_min && w <= want$watts_max,
                info = paste(want$season, round(w)))
  }
  ch <- g$fitness$season_change
  d <- s$watts_ref[s$season == ch$to] - s$watts_ref[s$season == ch$from]
  expect_true(d >= ch$min && d <= ch$max, info = round(d))
})

test_that("golden: riding volume per year", {
  skip_without_golden()
  g <- yaml::read_yaml(golden_path)
  skip_if(is.null(g$riding), "no riding block in the golden file")
  r <- golden_query("SELECT * FROM strava_rides")
  r <- r[r$sport_type %in% unlist(g$riding$sport_types), ]
  for (want in g$riding$years) {
    y <- r[r$year == want$year, ]
    expect_equal(sum(y$km), want$km, tolerance = g$riding$tolerance, info = paste(want$year, "km"))
    expect_equal(sum(y$moving_h), want$hours, tolerance = g$riding$tolerance, info = paste(want$year, "hours"))
  }
})
