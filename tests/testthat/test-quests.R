# The Quest Board (R/quests.R) on synthetic data and the example config.

source(testthat::test_path("..", "..", "R", "quests.R"))

fake_quest_data <- function() {
  days <- as.Date("2029-11-01") + 0:120
  list(
    weight = tibble::tibble(date = days, trend_kg = seq(82, 76, length.out = length(days))),
    strength = tibble::tibble(
      date = as.Date(c("2029-12-01", "2030-01-10", "2030-01-20")),
      lift = "Barbell Back Squat", e1rm = c(100, 110, 120)
    ),
    rides = tibble::tibble(
      date = as.Date(c("2029-12-20", "2030-01-05", "2030-02-01")),
      km = c(50, 99.9, 100), elev_m = c(300, 900, 799)
    ),
    sessions = tibble::tibble(date = as.Date("2030-01-31") - c(0, 3, 7, 10, 14, 20, 27, 28), kind = "bjj"),
    climbs = tibble::tibble(
      date = as.Date(c("2030-01-15", "2030-01-25")), climb = "Example Hill",
      status = c("clean", "clean"), bike_id = c("other", "roadie")
    )
  )
}

test_that("decreasing: progress from the baseline, then from the value on progress_start", {
  q <- list(id = "w", kind = "decreasing", metric = "weight_trend_kg",
            baseline = list(value = 82, date = "2029-11-01"), target = 75)
  d <- fake_quest_data()
  cfg <- list(progress_start = "2030-01-01")
  # Before progress_start: counts from the baseline
  now <- quest_now(q, d, "2029-12-15", cfg)
  expect_equal(now$source, "live")
  start <- quest_start(q, d, "2029-12-15", cfg)
  expect_equal(start, 82)
  p <- quest_progress(q, now, start)
  expect_equal(p$share, (82 - now$value) / 7)
  # After: counts from the trend on 1 January
  start2 <- quest_start(q, d, "2030-02-15", cfg)
  expect_equal(start2, d$weight$trend_kg[d$weight$date == as.Date("2030-01-01")])
  # Never below 0 or above 1
  expect_equal(quest_progress(q, list(value = 90, source = "live"), 82)$share, 0)
  expect_equal(quest_progress(q, list(value = 70, source = "live"), 82)$share, 1)
})

test_that("e1RM now: mean of the last n sessions, baseline without any", {
  q <- list(id = "s", kind = "increasing", metric = "e1rm_bar_kg", exercise = "Barbell Back Squat",
            baseline = list(value = 95, date = "2029-11-01"), target = 140)
  d <- fake_quest_data()
  expect_equal(quest_now(q, d, "2030-02-01")$value, 115)
  expect_equal(quest_now(q, d, "2030-02-01", list(now_sessions = 1))$value, 120)
  expect_equal(quest_now(q, d, "2030-01-15")$value, 105)
  b <- quest_now(q, d, "2029-11-15")
  expect_equal(b$source, "baseline")
  expect_equal(b$value, 95)
})

test_that("hold: held within the band of the target", {
  q <- list(id = "b", kind = "hold", metric = "e1rm_bar_kg", target = 100)
  expect_match(quest_progress(q, list(value = 97.5, source = "live"))$label, "^Held")
  expect_match(quest_progress(q, list(value = 97.4, source = "live"))$label, "^Below")
  expect_match(quest_progress(q, list(value = 98, source = "live"), cfg = list(hold_band_kg = 1))$label, "^Below")
})

test_that("cumulative: the year so far stands in before the quest's year", {
  q <- list(id = "r", kind = "cumulative", metric = "ride_km_ytd", target = 2000)
  d <- fake_quest_data()
  cfg <- list(year = 2030)
  before <- quest_now(q, d, "2029-12-31", cfg)
  expect_equal(before$value, 50)
  expect_match(quest_progress(q, before, as_of = "2029-12-31", cfg = cfg)$label, "50 km in 2029 so far")
  during <- quest_now(q, d, "2030-02-15", cfg)
  expect_equal(during$value, 199.9)
  p <- quest_progress(q, during, as_of = as.Date("2030-02-15"), cfg = cfg)
  expect_equal(p$share, 199.9 / 2000)
  expect_match(p$label, sprintf("even pace would be at %d km", round(2000 * 46 / 365)))
})

test_that("goal ride: both distance and climbing, in the quest's year", {
  q <- list(id = "r", kind = "cumulative", metric = "ride_km_ytd", target = 2000,
            extra = list(goal_ride = list(km = 100, gain_m = 800)))
  d <- fake_quest_data()
  # 99.9 km with 900 m, then 100 km with 799 m: neither counts
  expect_match(quest_extras(q, d, list(value = 0), as.Date("2030-03-01"), list(year = 2030)), "not yet")
  d$rides$elev_m[3] <- 800
  expect_match(quest_extras(q, d, list(value = 0), as.Date("2030-03-01"), list(year = 2030)), "ridden on 1 Feb 2030")
})

test_that("rate: sessions over the last 28 days, or not logged", {
  q <- list(id = "j", kind = "rate", metric = "bjj_sessions_per_week", target = 2, met_at = 1.6)
  d <- fake_quest_data()
  now <- quest_now(q, d, "2030-01-31")
  expect_equal(now$value, 7 / 4)                    # 28 days ago falls outside
  expect_match(quest_progress(q, now)$label, "^Met")
  d$sessions <- d$sessions[0, ]
  none <- quest_now(q, d, "2030-01-31")
  expect_equal(none$text, "not logged")
  expect_true(is.na(quest_progress(q, none)$share))
})

test_that("event: a clean ascent on the named bike, or a stage ticked by hand", {
  d <- fake_quest_data()
  q <- list(id = "h", kind = "event", metric = "climb_clean", target = list(text = "Clean"), stages = list(
    list(label = "On the roadie", climb = "Example Hill", bike = "roadie"),
    list(label = "Any bike, from February", climb = "Example Hill", since = "2030-02-01"),
    list(label = "By hand", done = "2030-03-01")
  ))
  now <- quest_now(q, d, "2030-02-15")
  expect_equal(now$value, 1)
  expect_equal(now$stages[[1]], as.Date("2030-01-25"))
  expect_true(is.na(now$stages[[2]]))
  expect_true(is.na(now$stages[[3]]))               # ticked for a later day
  p <- quest_progress(q, now)
  expect_equal(p$share, 1 / 3)
  expect_match(p$label, "^1 of 3")
  expect_equal(quest_now(q, d, "2030-03-01")$value, 2)
})

test_that("the example config builds a board, one row per quest", {
  cfg <- yaml::read_yaml(testthat::test_path("..", "..", "config", "oath.example.yml"))
  b <- quest_board(cfg, fake_quest_data(), as_of = "2030-02-15")
  expect_equal(nrow(b), length(cfg$quests))
  expect_equal(b$id, vapply(cfg$quests, `[[`, "", "id"))
  expect_equal(sum(b$role == "main"), 1)
  expect_true(all(b$accent %in% 1:4))
  expect_equal(b$headline[b$id == "bodyweight"], sprintf("%.1f → 75 kg", quest_now(cfg$quests[[2]], fake_quest_data(), "2030-02-15")$value))
  expect_match(b$progress[b$id == "hill"], "^1 of 1")     # no bike named: any bike counts
})
