source(testthat::test_path("..", "..", "R", "climb_matching.R"))

# A synthetic climb: 1000 m due east from an invented start point.
lat0 <- 50.8
lon0 <- 3.8
m_to_lon <- function(x) lon0 + x / (111320 * cos(lat0 * pi / 180))
climb <- tibble(name = "Test climb", start_lat = lat0, start_lon = lon0,
                end_lat = lat0, end_lon = m_to_lon(1000), length_m = 1000)

# A ride along the x axis, sampled every second, from per-second speeds
# (negative = riding back). Distance is cumulative path, as in Strava.
# `gaps`: named list (sample index = seconds) of stops: a stationary stretch
# after that sample, as auto-pause records it. `hr`: a function of time.
ride <- function(speeds, x0 = -200, gaps = list(), hr = function(t) 150) {
  x <- x0 + c(0, cumsum(speeds))
  d <- c(0, cumsum(abs(speeds)))
  t <- seq_along(x) - 1
  for (k in sort(as.integer(names(gaps)), decreasing = TRUE)) {
    i <- k + 1                                   # sample k is row k + 1
    secs <- gaps[[as.character(k)]]
    x <- append(x, x[i], after = i)
    d <- append(d, d[i], after = i)
    t <- append(t, t[i] + secs, after = i)
    t[(i + 2):length(t)] <- t[(i + 2):length(t)] + secs
  }
  tibble(t = t, lat = lat0, lon = m_to_lon(x), dist = d, alt = NA_real_, hr = hr(t))
}

# Run-up 200 m, climb 1000 m at 5 m/s, run-out 200 m
steady <- function() c(rep(10, 20), rep(5, 200), rep(10, 20))

test_that("one ascent: matched from start to end", {
  s <- ride(steady())
  m <- match_climb(s, climb)
  expect_equal(nrow(m), 1)
  expect_equal(s$dist[m$i0] - 200, 0, tolerance = 5)       # at the start point
  expect_equal(s$dist[m$i1] - 200, 1000, tolerance = 5)    # at the end point
  e <- effort_metrics(s, m$i0, m$i1)
  expect_equal(e$elapsed_s, 200)
  expect_equal(e$stop_s, 0)
  expect_equal(e$status, "clean")
  expect_equal(e$crawl10_kmh, 18)
})

test_that("passing the start without reaching the end is no ascent", {
  # Turns back after 600 m
  s <- ride(c(rep(10, 20), rep(5, 120), rep(-5, 120)))
  expect_equal(nrow(match_climb(s, climb)), 0)
})

test_that("a detour longer than 1.35 x L + 40 m is no ascent", {
  # Rides 600 m up, 400 m back down, then up again to the end: 1800 m of path
  s <- ride(c(rep(10, 20), rep(5, 120), rep(-5, 80), rep(5, 200), rep(10, 20)))
  expect_equal(nrow(match_climb(s, climb)), 0)
})

test_that("two ascents in one ride are two efforts", {
  up <- c(rep(10, 20), rep(5, 200), rep(10, 20))
  back <- rep(-10, 140)                     # ride back to the run-up
  s <- ride(c(up, back, up[-(1:20)], rep(10, 0)))
  m <- match_climb(s, climb)
  expect_equal(nrow(m), 2)
  expect_true(m$i0[2] > m$i1[1])
})

test_that("stops decide the status", {
  base <- steady()
  # sample 20 is the climb start; the climb takes samples 20-220
  at <- function(frac) as.character(20 + round(200 * frac))
  status_with_gap <- function(frac, secs) {
    s <- ride(base, gaps = setNames(list(secs), at(frac)))
    m <- match_climb(s, climb)
    effort_metrics(s, m$i0, m$i1)
  }
  expect_equal(status_with_gap(0.5, 1)$status, "clean")   # <= 2 s
  e <- status_with_gap(0.5, 60)
  expect_equal(e$status, "early")
  expect_equal(e$stop_s, 60)
  expect_equal(e$elapsed_s, 260)
  expect_equal(e$moving_s, e$elapsed_s - e$stop_s)
  expect_equal(status_with_gap(0.97, 60)$status, "top")
  expect_equal(status_with_gap(0.5, 6)$status, "foot")
})

test_that("a stretch at walking speed is a walk", {
  speeds <- c(rep(10, 20), rep(5, 80), rep(1, 100), rep(5, 80), rep(10, 20))
  s <- ride(speeds)
  m <- match_climb(s, climb)
  e <- effort_metrics(s, m$i0, m$i1)
  expect_equal(e$status, "walk")
  expect_equal(e$crawl10_kmh, 3.6)
})

test_that("heart rate: time-weighted average and the last 30 s", {
  # HR rises 1 bpm per second from 100 during the climb
  s <- ride(steady(), hr = function(t) 100 + pmax(0, t - 20))
  m <- match_climb(s, climb)
  e <- effort_metrics(s, m$i0, m$i1)
  # Intervals start at HR 100..299 over the climb: mean 199.5
  expect_equal(e$hr_avg, 199.5)
  # Last 30 intervals start at 270..299: mean 284.5
  expect_equal(e$hr_end, 284.5)
  expect_equal(e$hrs_into_ride, 20 / 3600)
})

test_that("climb_efforts collects all climbs of a ride", {
  s <- ride(steady())
  other <- mutate(climb, name = "Elsewhere", start_lat = lat0 + 0.01, end_lat = lat0 + 0.01)
  out <- climb_efforts(s, bind_rows(climb, other), activity_id = 42)
  expect_equal(out$climb, "Test climb")
  expect_equal(out$activity_id, "42")
  expect_equal(nrow(climb_efforts(NULL, climb, 1)), 0)
})
