# The Reliquary's rules (R/relics.R) on synthetic data and invented relics.

source(testthat::test_path("..", "..", "R", "relics.R"))

# 10 weeks from Monday 7 Jan 2030: trend from 82 kg, losing `rate` a week
fake_relic_data <- function(rate = 0.4, weeks_met = rep(TRUE, 10)) {
  start <- as.Date("2030-01-07")
  days <- start + 0:69
  trend <- 82 - rate * as.numeric(days - start) / 7
  wk <- start + 7 * 0:9
  list(
    weight = tibble::tibble(date = days, trend_kg = trend),
    weekly = tibble::tibble(week = wk, trend_change = -rate, goal_change = -0.4,
                            in_plan = TRUE, met = weeks_met, complete = TRUE),
    sets = tibble::tibble(
      date = as.Date(c("2029-12-20", "2030-01-15", "2030-02-10", "2030-02-12")),
      exercise = "Front Squat", reps = c(3, 3, 5, 3), rir = c(1, 3, 1, 2), is_warmup = FALSE
    ),
    rides = tibble::tibble(
      date = as.Date(c("2030-03-01", "2030-04-01", "2030-05-01", "2031-01-02")),
      km = c(99.9, 105, 100, 200), elev_m = c(900, 799, 800, 2000)
    ),
    climbs = tibble::tibble(
      date = as.Date(c("2030-03-10", "2030-04-10")), climb = "Example Hill",
      status = "clean", bike_id = c("other", "roadie")
    ),
    events = tibble::tibble(date = as.Date(c("2030-02-01", "2030-12-01")), relic = c("M", "N"),
                            note = c("logged", "logged ahead of time"))
  )
}

relic <- function(id, rule, levels = 0, ...) {
  c(list(id = id, name_pt = paste("Relíquia", id), name_en = paste("Relic", id),
         unlock = "a rule", levels = levels, rule = rule), list(...))
}

status_of <- function(rule, as_of = "2030-06-30", data = fake_relic_data(), cfg = list(), levels = 0) {
  cfg$relics <- list(relic("R", rule, levels))
  relic_status(cfg, data, as_of)
}

test_that("weight_reached: trend at or below kg, with the weekly loss on plan", {
  rule <- list(type = "weight_reached", kg = 80, adherence = 0.8, rate_kg_week = 0.5, since = "2030-01-07")
  # 0.4 kg a week against 0.5: 80% exactly, so it counts; 80 kg on day 35
  s <- status_of(rule)
  expect_true(s$unlocked)
  expect_equal(s$date, as.Date("2030-01-07") + 35)
  # Just above: 80.01 kg is never reached in 10 weeks at this slower pace
  s2 <- status_of(modifyList(rule, list(kg = 78)))
  expect_false(s2$unlocked)
  expect_match(s2$hint, "Trend [0-9.]+ → 78 kg")
  # 0.395 kg a week: 79%, below the bar, so 80 kg alone doesn't unlock it
  s3 <- status_of(rule, data = fake_relic_data(rate = 0.395))
  expect_false(s3$unlocked)
  expect_match(s3$hint, "79% of the planned loss \\(needs 80%\\)")
  # Not yet on an earlier day
  expect_false(status_of(rule, as_of = "2030-02-10")$unlocked)
})

test_that("weight_reached with kept_share counts the weeks kept", {
  rule <- list(type = "weight_reached", kg = 80, kept_share = 0.8, since = "2030-01-07")
  # 4 of the 5 weeks done by day 35 kept: 80%
  s <- status_of(rule, data = fake_relic_data(weeks_met = c(TRUE, FALSE, TRUE, TRUE, TRUE, rep(TRUE, 5))))
  expect_true(s$unlocked)
  # 3 of 5: 60%; it reaches 80% only once 8 of 10 weeks are kept (day 69)
  s2 <- status_of(rule, data = fake_relic_data(weeks_met = c(TRUE, FALSE, FALSE, TRUE, TRUE, rep(TRUE, 5))))
  expect_equal(s2$date, as.Date("2030-01-07") + 69)
})

test_that("week_kept_at: a week is kept at that share of its planned loss", {
  rule <- list(type = "weight_reached", kg = 80, kept_share = 0.8, since = "2030-01-07")
  # Losing 0.32 a week against 0.4 planned: met is false every week, but 80%
  d <- fake_relic_data(rate = 0.32, weeks_met = rep(FALSE, 10))
  expect_false(status_of(rule, data = d)$unlocked)
  s <- status_of(modifyList(rule, list(week_kept_at = 0.8)), data = d)
  expect_true(s$unlocked)
  expect_match(s$hint, "Unlocked")
  # 0.31: 77.5%, not kept
  d2 <- fake_relic_data(rate = 0.31, weeks_met = rep(FALSE, 10))
  expect_false(status_of(modifyList(rule, list(week_kept_at = 0.8)), data = d2)$unlocked)
})

test_that("once unlocked, a relic stays unlocked when the trend goes back up", {
  d <- fake_relic_data()
  d$weight$trend_kg[d$weight$date > as.Date("2030-02-20")] <- 85
  s <- status_of(list(type = "weight_reached", kg = 80, adherence = 0.8, rate_kg_week = 0.5,
                      since = "2030-01-07"), data = d)
  expect_true(s$unlocked)
  expect_equal(s$date, as.Date("2030-01-07") + 35)
})

test_that("lift_gate: the blocks ticked, then a heavy triple at the right RIR from since", {
  rule <- list(type = "lift_gate", exercise = "Front Squat", reps = 3, rir = list(1, 2), since = "2030-01-01",
               blocks = list(list(label = "B1", done = "2030-01-20"), list(label = "B2")))
  # Block 2 not done: sealed, though a triple qualifies
  s <- status_of(rule)
  expect_false(s$unlocked)
  expect_match(s$hint, "Blocks 1 of 2 · heavy triple: 12 Feb 2030")
  # Both done: the triple of 20 Dec 2029 is before since, 15 Jan is RIR 3 and
  # 10 Feb is 5 reps, so 12 Feb counts
  rule$blocks[[2]]$done <- "2030-02-01"
  expect_equal(status_of(rule)$date, as.Date("2030-02-12"))
})

test_that("ride: 99.9 km or 799 m don't count, a ride in another year doesn't either", {
  s <- status_of(list(type = "ride", km = 100, gain_m = 800, year = 2030))
  expect_equal(s$date, as.Date("2030-05-01"))
  s2 <- status_of(list(type = "ride", km = 100, gain_m = 800, year = 2030), as_of = "2030-04-30")
  expect_false(s2$unlocked)
  expect_match(s2$hint, "Longest so far: 105 km / 799 m")
})

test_that("km_year: the day the year's sum reaches the target", {
  s <- status_of(list(type = "km_year", km = 200, year = 2030))
  expect_equal(s$date, as.Date("2030-04-01"))   # 99.9 + 105
  expect_match(status_of(list(type = "km_year", km = 500, year = 2030))$hint, "305 of 500 km in 2030")
})

test_that("climb_clean: on the given bike only", {
  expect_equal(status_of(list(type = "climb_clean", climb = "Example Hill"))$date, as.Date("2030-03-10"))
  expect_equal(status_of(list(type = "climb_clean", climb = "Example Hill", bike = "roadie"))$date,
               as.Date("2030-04-10"))
  expect_false(status_of(list(type = "climb_clean", climb = "Example Hill", bike = "roadie"),
                         as_of = "2030-04-01")$unlocked)
})

test_that("manual: from the log; an event dated after as_of doesn't count yet", {
  cfg <- list(relics = list(relic("M", list(type = "manual")), relic("N", list(type = "manual"))))
  s <- relic_status(cfg, fake_relic_data(), "2030-06-30")
  expect_equal(s$unlocked, c(TRUE, FALSE))
  expect_equal(s$date[1], as.Date("2030-02-01"))
  expect_true(relic_status(cfg, fake_relic_data(), "2030-12-01")$unlocked[2])
})

test_that("quest_stages earns a level per stage done, and unlocks when all are", {
  cfg <- list(level_base = 11, quests = list(list(id = "ledger", stages = list(
    list(label = "A", done = "2030-01-01"), list(label = "B", done = "2030-03-01"), list(label = "C")))))
  rule <- list(type = "quest_stages", quest = "ledger", levels_per_stage = TRUE)
  s <- status_of(rule, cfg = cfg, levels = 3)
  expect_false(s$unlocked)
  expect_equal(s$levels, 2L)
  expect_equal(relic_level(s, cfg), 13L)
  expect_equal(status_of(rule, as_of = "2030-02-01", cfg = cfg, levels = 3)$levels, 1L)
  # Without levels_per_stage: the levels come only with the unlock
  expect_equal(status_of(modifyList(rule, list(levels_per_stage = FALSE)), cfg = cfg, levels = 3)$levels, 0L)
  expect_error(status_of(list(type = "quest_stages", quest = "nope")), "no quest 'nope'")
})

test_that("the example config builds a status row per relic, colours cycling", {
  cfg <- example_oath()
  d <- fake_relic_data()
  d$events$relic[1] <- "III"
  s <- relic_status(cfg, d, "2030-06-30")
  expect_equal(s$id, c("I", "II", "III", "IV", "V"))
  expect_equal(s$colour, unlist(cfg$relic_colours))
  expect_true(s$unlocked[s$id == "III"])
  # 1 (base) + the ride of 1 March + III + the Ledger's stage done in Feb
  expect_equal(s$unlocked, c(FALSE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(relic_level(s, cfg), 1L + 1L + 1L + 1L)
  expect_error(relic_status(list(relics = list(relic("Z", list(type = "nope")))), d), "Unknown relic rule")
})

test_that("read_relic_events: an empty log when the file is missing", {
  e <- read_relic_events(file.path(withr::local_tempdir(), "none.csv"))
  expect_equal(nrow(e), 0)
  expect_s3_class(e$date, "Date")
  e2 <- read_relic_events(testthat::test_path("..", "..", "config", "relic_events.example.csv"))
  expect_equal(e2$relic, "III")
})
