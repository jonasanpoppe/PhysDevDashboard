source(testthat::test_path("..", "..", "R", "weight_plan.R"))

test_that("pace: down at the rate, flat for the pause, floor at the target", {
  g <- fake_goal()                                       # 80 kg start, 0.7 kg/wk = 0.1 kg/day
  d <- as.Date("2030-01-07") + c(-1, 0, 20, 25, 27, 30, 60)
  kg <- pace_curve(d, "2030-01-07", 80, g)
  expect_true(is.na(kg[1]))                              # before the start
  expect_equal(kg[2:3], c(80, 78))                       # 20 days: reaches the pause at 78
  expect_equal(kg[4:5], c(78, 78))                       # flat for 7 days
  expect_equal(kg[6], 78 - 0.1 * 3, tolerance = 1e-9)    # then on again
  expect_equal(kg[7], 75)                                # never below the target
})

test_that("ETA includes the pause; no pause once below it", {
  g <- fake_goal()
  expect_equal(pace_eta("2030-01-07", 80, g), as.Date("2030-01-07") + 50 + 7)
  expect_equal(pace_eta("2030-01-07", 77, g), as.Date("2030-01-07") + 20)
  expect_equal(pace_eta("2030-01-07", 74, g), as.Date("2030-01-07"))
})

test_that("daily table: pace from the start's trend, the last (unfinished) day's intake left out", {
  d <- build_weight_daily(fake_macrofactor_daily(), fake_goal())
  expect_equal(d$pace_kg[d$date == as.Date("2030-01-07")], 80)
  expect_true(all(is.na(d$pace_kg[d$date < as.Date("2030-01-07")])))
  expect_true(is.na(d$kcal[d$date == max(d$date)]))
  expect_equal(d$kcal[1], 2200)
  expect_false("steps" %in% names(d))                    # MacroFactor's steps are not used
  bad <- fake_goal(); bad$start_date <- "2031-01-01"
  expect_error(build_weight_daily(fake_macrofactor_daily(), bad), "no trend weight on start_date")
})

test_that("sessions: one per day per kind, logged sets first for lifting, any set source", {
  acts <- tibble::tibble(date = as.Date(c("2030-01-08", "2030-01-08", "2030-01-09", "2030-01-10")),
                         sport_type = c("Workout", "WeightTraining", "WeightTraining", "Workout"))
  sets <- tibble::tibble(date = as.Date(c("2030-01-08", "2030-01-08", "2030-01-03", "2030-01-05")),
                         source = c("macrofactor", "macrofactor", "strong", "strong"),
                         is_warmup = c(FALSE, FALSE, FALSE, TRUE))   # 5 Jan: warm-up only
  cfg <- list(bjj = list(strava_types = list("Workout")),
              lifting = list(strava_types = list("WeightTraining"), logged_sets = TRUE))
  s <- build_sessions(acts, sets, cfg)
  lift <- s[s$kind == "lifting", ]
  expect_equal(lift$date, as.Date(c("2030-01-03", "2030-01-08", "2030-01-09")))
  expect_equal(lift$source, c("strong", "macrofactor", "strava"))   # Strong counts; Strava fills 9 Jan
  expect_equal(sum(s$kind == "bjj"), 2)
  # Without logged_sets, Strava only
  cfg$lifting$logged_sets <- NULL
  expect_true(all(build_sessions(acts, sets, cfg)$source == "strava"))
})

test_that("weeks: context before the plan, planned change from the start, met or missed", {
  d <- build_weight_daily(fake_macrofactor_daily(), fake_goal())
  w <- build_weight_weekly(d, tibble::tibble(date = as.Date(character()), kind = character()),
                           fake_goal(), context_weeks = 2)
  expect_equal(w$week[1], as.Date("2029-12-24"))     # two Mondays before the start week
  expect_equal(w$in_plan, c(FALSE, FALSE, rep(TRUE, nrow(w) - 2)))
  plan <- w[w$in_plan & w$complete, ]
  expect_equal(plan$goal_change[1], -0.6, tolerance = 1e-9)  # start Monday 7 Jan: 6 days at 0.1
  # The trend follows the pace exactly until the pause, so those weeks are met
  expect_true(all(plan$met[plan$week < as.Date("2030-01-21")]))
})

test_that("status: rate, ETA from today, checkpoint, sessions per week", {
  d <- build_weight_daily(fake_macrofactor_daily(), fake_goal())
  s <- tibble::tibble(date = max(d$date) - c(1, 3, 8, 15), kind = c("bjj", "bjj", "lifting", "bjj"))
  st <- compute_weight_status(d, s, fake_goal(), list(protein_g_per_day = 140),
                              list(bjj = list(per_week = 2), lifting = list(per_week = 3)))
  expect_equal(st$rate_recent, -0.7, tolerance = 1e-9)
  expect_equal(st$bjj_per_week, 0.75)
  expect_equal(st$protein_7d, 150)
  expect_equal(st$eta, pace_eta(st$date, st$trend_kg, fake_goal()))
})

test_that("weeks: steps from the device, none for a week spanning the switch", {
  d <- build_weight_daily(fake_macrofactor_daily(), fake_goal())
  none <- tibble::tibble(date = as.Date(character()), kind = character())
  days <- seq(min(d$date), max(d$date), by = "day")
  switch_day <- days[days > days[7] & format(days, "%u") == "3"][1]   # a Wednesday: mid-week
  dev <- tibble::tibble(date = days, era = ifelse(days < switch_day, "fitbit", "garmin"),
                        steps = ifelse(days < switch_day, 5000, 9000))
  w <- build_weight_weekly(d, none, fake_goal(), device_steps = dev)
  wk <- function(day) w$steps[w$week == day - (as.integer(format(day, "%u")) - 1L)]
  expect_true(is.na(wk(switch_day)))                     # both eras in that week
  expect_equal(wk(max(days)), 9000)
  expect_equal(wk(min(days)), 5000)
  # Without device steps the column is there, empty
  expect_true(all(is.na(build_weight_weekly(d, none, fake_goal())$steps)))
})
