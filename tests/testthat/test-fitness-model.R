source(testthat::test_path("..", "..", "R", "fitness_model.R"))
types <- list(ride_types = c("Ride", "GravelRide"), run_types = c("Run", "TrailRun"))

# Synthetic efforts with known effects: +2.5 W per bpm, -40 W per log(s),
# no effect of time into the ride, and season 2 is 30 W weaker.
simulate_efforts <- function(n_per_season = 40, seed = 3) {
  set.seed(seed)
  n <- 2 * n_per_season
  season <- rep(c(2030L, 2031L), each = n_per_season)
  hr <- runif(n, 140, 170); dur <- runif(n, 100, 400); hrs <- runif(n, 0.5, 3)
  tibble::tibble(
    activity_id = as.character(seq_len(n)),
    season = season,                          # as fitness_efforts() adds it
    date = as.Date(paste0(season, "-06-01")) + sample(0:150, n, replace = TRUE),
    status = "clean", moving_s = dur, bike_id = "roadie",
    hr_end = hr, hrs_into_ride = hrs,
    power_w = 200 + 2.5 * hr - 40 * log(dur) - 30 * (season == 2031L) + rnorm(n, 0, 8)
  )
}
settings <- list(bike_ids = list("roadie"), bootstrap = 300, seed = 1,
                 ref_hr_bpm = 155, ref_duration_s = 180, ref_hours_into_ride = 1.5)

test_that("the model recovers known effects, with intervals around them", {
  m <- fit_efficiency(simulate_efforts(), settings)
  co <- setNames(m$coefs$estimate, m$coefs$term)
  expect_equal(unname(co["hr_end"]), 2.5, tolerance = 0.15)
  expect_equal(unname(co["season2031"]), -30, tolerance = 0.15)
  s31 <- m$coefs[m$coefs$term == "season2031", ]
  expect_true(s31$ci_low < -30 && s31$ci_high > -30)
  truth_2030 <- 200 + 2.5 * 155 - 40 * log(180)
  expect_equal(m$seasons$watts_ref[m$seasons$season == 2030], truth_2030, tolerance = 0.03)
  expect_equal(m$seasons$n, c(40, 40))
})

test_that("seasons with too few efforts are left out", {
  e <- simulate_efforts()
  e <- rbind(e[e$season == 2030, ], head(e[e$season == 2031, ], 3))   # 2031: only 3
  m <- fit_efficiency(e, modifyList(settings, list(min_efforts_per_season = 5)))
  expect_equal(m$seasons$season, 2030L)
})

test_that("only clean ascents on the chosen bike and heart-rate era", {
  e <- simulate_efforts(5)
  e$status[1] <- "early"; e$bike_id[2] <- "hybrid"; e$moving_s[3] <- 60
  acts <- tibble::tibble(activity_id = e$activity_id,
                         device_name = c("Example Watch", rep("Example Bike Computer", nrow(e) - 1)))
  eras <- list("Example Bike Computer" = "chest_strap", "Example Watch" = "wrist")
  fe <- fitness_efforts(e, acts, eras, settings)
  expect_equal(sort(as.integer(fe$activity_id)), 4:10)        # 1: early (and wrist), 2: bike, 3: short
  expect_equal(hr_era_of(c("Example Watch", "Other"), eras), c("wrist", "unknown"))
})

test_that("daily load: calendar days, 6-week mean per week, EWMAs", {
  acts <- tibble::tibble(
    date = as.Date(c("2030-01-01", "2030-01-01", "2030-01-15")),
    moving_s = c(3600, 1800, 7200), sport_type = c("Ride", "Run", "Ride")
  )
  l <- daily_load(acts, ride_types = types$ride_types, run_types = types$run_types)
  expect_equal(nrow(l), 15)                                   # every day, gaps filled
  expect_equal(l$ride_h[1], 1); expect_equal(l$run_h[1], 0.5)
  expect_equal(l$ride_6w_hpw[15], (1 + 2) / 6)                # 42-day window, per week
  expect_equal(l$fatigue_h[1], 1.5 / 7)                       # first step of the 7-day EWMA
  expect_true(all(diff(l$fitness_h[2:14]) < 0))               # decays on rest days
})

test_that("load check returns a slope with its interval", {
  m <- fit_efficiency(simulate_efforts(), settings)
  acts <- tibble::tibble(date = as.Date("2030-01-01") + 0:800, moving_s = 3600, sport_type = "Ride")
  chk <- load_check(m, daily_load(acts, ride_types = types$ride_types, run_types = types$run_types))
  expect_named(chk, c("w_per_weekly_hour", "ci_low", "ci_high", "n"))
  expect_equal(chk$n, 80)
})
