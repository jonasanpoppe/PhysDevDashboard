# Scenarios (R/scenarios.R) on synthetic data: each projection and the edges
# of the verdict (on the deadline, a day late, the range straddling it).

for (f in c("weight_plan", "forecast", "quests", "scenarios")) {
  source(testthat::test_path("..", "..", "R", paste0(f, ".R")))
}

a0 <- scenario_defaults(list())

# Trend from 82 kg on Mon 7 Jan 2030, losing `rate` a week; weekly changes
# from `changes` (constant: an SD of 0, so no range)
fake_weight_data <- function(rate = 0.4, changes = rep(-rate, 8), in_plan = TRUE) {
  start <- as.Date("2030-01-07")
  days <- start + 0:55
  list(
    weight = tibble::tibble(date = days, trend_kg = 82 - rate * as.numeric(days - start) / 7),
    weekly = tibble::tibble(week = start + 7 * 0:7, trend_change = changes, in_plan = in_plan, complete = TRUE),
    goal = list(rate_kg_per_week = 0.5, target_kg = 75, pause_weeks = 0)
  )
}

test_that("date_verdict: on the deadline is on track, a day late at risk or behind", {
  d <- as.Date("2030-06-01")
  expect_equal(date_verdict(d, d - 10, d), "on_track")
  expect_equal(date_verdict(d + 1, d - 10, d), "at_risk")
  expect_equal(date_verdict(d + 1, d + 1, d), "behind")
  expect_equal(date_verdict(NA, NA, d), "behind")
  # A window: on track by its start, at risk by the deadline
  expect_equal(date_verdict(d - 30, d - 40, d, window_start = d - 30), "on_track")
  expect_equal(date_verdict(d - 29, d - 40, d, window_start = d - 30), "at_risk")
  expect_equal(date_verdict(d + 5, d - 1, d, window_start = d - 30), "at_risk")
  expect_equal(date_verdict(d + 5, d + 1, d, window_start = d - 30), "behind")
})

test_that("weight: the plan at rate x adherence, the deadline edge", {
  d <- fake_weight_data()
  as_of <- as.Date("2030-03-03")                              # day 55: 82 - 0.4 x 55/7
  now <- 82 - 0.4 * 55 / 7
  q <- list(id = "w", metric = "weight_trend_kg", target = 76)
  eta <- as_of + ceiling(round((now - 76) / (0.4 / 7), 6))    # 0.5 x 80% = 0.4 kg a week
  s <- scenario_weight(c(q, deadline = as.character(eta)), d, as_of, a0)
  plan <- s$models[s$models$model == "plan", ]
  expect_equal(plan$mid, eta)
  expect_equal(plan$fast, eta)                                # SD 0: no range
  expect_equal(plan$verdict, "on_track")
  # The deadline a day earlier: no range to save it, so behind
  expect_equal(scenario_weight(c(q, deadline = as.character(eta - 1)), d, as_of, a0)$models$verdict[1], "behind")
  # The recent pace equals the plan here (0.4 a week)
  expect_equal(s$models$rate[2], 0.4, tolerance = 1e-6)
  # The chip follows the recent pace once min_weeks of the cut are behind
  expect_equal(s$model, "recent")
  expect_equal(scenario_weight(c(q, deadline = "2031-01-01"), fake_weight_data(in_plan = FALSE), as_of, a0)$model, "plan")
})

test_that("weight: the range from the weekly SD straddles the deadline", {
  d <- fake_weight_data(changes = c(-0.1, -0.7, -0.2, -0.6, -0.3, -0.5, -0.4, -0.4))
  as_of <- as.Date("2030-03-03")
  q <- list(id = "w", metric = "weight_trend_kg", target = 76)
  s <- scenario_weight(c(q, deadline = "2030-06-01"), d, as_of, a0)
  plan <- s$models[s$models$model == "plan", ]
  expect_lt(plan$fast, plan$mid)
  expect_gt(plan$slow, plan$mid)
  # Deadline between fast and mid: at risk
  s2 <- scenario_weight(c(q, deadline = as.character(plan$mid - 1)), d, as_of, a0)
  expect_equal(s2$models$verdict[1], "at_risk")
  expect_match(s2$summary, "^76 kg at the recent pace")
  # Gaining weight: never
  g <- fake_weight_data(rate = -0.2)
  expect_true(is.na(scenario_weight(c(q, deadline = "2030-06-01"), g, as_of, a0)$models$mid[2]))
  # The projection for the figure: three bands per model
  p <- weight_projection(s, "2030-06-01")
  expect_setequal(unique(p$band), c("mid", "fast", "slow"))
  expect_setequal(unique(p$model), c("plan", "recent"))
})

test_that("weight SD: the cut's weeks, or the weeks before it with too few", {
  w <- tibble::tibble(week = as.Date("2030-01-07") + 7 * 0:9, trend_change = c(rep(c(0, -1), 4), -0.4, -0.6),
                      in_plan = c(rep(FALSE, 8), TRUE, TRUE), complete = TRUE)
  s <- weekly_loss_sd(w, a0)
  expect_equal(s$basis, "the weeks before the cut")
  expect_equal(s$weeks, 10)
  w$in_plan[5:10] <- TRUE
  expect_equal(weekly_loss_sd(w, a0)$weeks, 6)
})

fake_forecast <- function() {
  dates <- as.Date("2030-01-01") + 7 * 0:52
  t <- 0:52
  curve <- dplyr::bind_rows(
    tibble::tibble(lift = "squat", band = "p10", date = dates, e1rm = 100 + 0.2 * t),
    tibble::tibble(lift = "squat", band = "p50", date = dates, e1rm = 100 + 0.5 * t),
    tibble::tibble(lift = "squat", band = "p90", date = dates, e1rm = 100 + 1.0 * t)
  )
  params <- tibble::tibble(lift = "squat", band = c("p10", "p50", "p90"), c = c(130, 150, 180),
                           y0 = 100, tau = 6)
  list(params = params, curve = curve)
}

test_that("lift: the heavy single per band against the deadline", {
  d <- list(forecast = fake_forecast(), strength = tibble::tibble())
  q <- list(id = "s", metric = "e1rm_bar_kg", exercise = "Barbell Back Squat", single_ratio = 1,
            target = 120, forecast = list(lift = "squat"), deadline = "2030-10-08")
  s <- scenario_lift(q, d, "2030-01-01")
  expect_equal(s$mid, as.Date("2030-01-01") + 7 * 40)          # 100 + 0.5 x 40
  expect_equal(s$fast, as.Date("2030-01-01") + 7 * 20)
  expect_true(is.na(s$slow))                                   # 100 + 0.2 x 52 < 120
  expect_equal(s$verdict, "on_track")                          # mid exactly on the deadline
  expect_equal(scenario_lift(modifyList(q, list(deadline = "2030-08-01")), d, "2030-01-01")$verdict, "at_risk")
  expect_equal(scenario_lift(modifyList(q, list(deadline = "2030-03-01")), d, "2030-01-01")$verdict, "behind")
  # On the bar: x single_ratio, so the target needs a higher e1RM
  s2 <- scenario_lift(modifyList(q, list(single_ratio = 0.96)), d, "2030-01-01")
  expect_gt(s2$mid, s$mid)
  # A range target counts by its low end
  expect_equal(scenario_lift(modifyList(q, list(target = list(low = 120, high = 125))), d, "2030-01-01")$mid, s$mid)
  # No forecast for the lift
  expect_equal(scenario_lift(modifyList(q, list(forecast = list(lift = "bench"))), d, "2030-01-01")$verdict, "none")
})

test_that("lift: a variant borrows the parent's curve, scaled, from its own e1RM", {
  d <- list(forecast = fake_forecast(),
            strength = tibble::tibble(date = as.Date("2030-01-01"), lift = "Front Squat", e1rm = 80))
  q <- list(id = "f", metric = "e1rm_bar_kg", exercise = "Front Squat", single_ratio = 1, target = 100,
            forecast = list(lift = "squat", variant_ratio = 0.8), deadline = "2030-12-31")
  p <- lift_projection(q, d, "2030-02-01")
  expect_equal(p$value[p$band == "p50"][1], 80)                 # starts at its own e1RM
  expect_lt(max(p$value[p$band == "p50"]), 150 * 0.8)           # under the scaled ceiling
  s <- scenario_lift(q, d, "2030-02-01")
  # p50 ceiling 120: 100 reached at 6 x ln(40 / 20) months
  expect_equal(as.numeric(s$mid - as.Date("2030-01-01")), 6 * log(2) * MONTH_DAYS, tolerance = 7)
  expect_match(s$summary, "the squat curve × 0.8")
})

fake_rides <- function(km = 50) {
  mondays <- as.Date("2029-07-02") + 7 * 0:51
  tibble::tibble(date = mondays, km = km, elev_m = 0)
}

test_that("km: so far plus weeks left x km a week, at the recent pace and set", {
  cfg <- list(year = 2030)
  q <- list(id = "c", metric = "ride_km_ytd", target = 2000, deadline = "2030-12-31")
  d <- list(rides = fake_rides())
  s <- scenario_km(q, d, "2030-06-30", a0, cfg)
  expect_equal(s$so_far, 50 * 25)                              # 25 Mondays in 2030, 7 Jan to 24 Jun
  expect_equal(s$models$mid[2], 1250 + 50 * as.numeric(as.Date("2030-12-31") - as.Date("2030-06-30")) / 7)
  expect_equal(s$verdict, "on_track")
  # The default km a week is the even pace: it reaches the target exactly
  expect_equal(s$models$mid[1], 2000)
  expect_equal(s$need, 750 / (184 / 7))
  # Too far for the recent pace, SD 0: behind; with a set km a week it is only a what-if
  s2 <- scenario_km(modifyList(q, list(target = 3000)), d, "2030-06-30", modifyList(a0, list(km_week = 100)), cfg)
  expect_equal(s2$verdict, "behind")
  expect_equal(s2$models$verdict[1], "on_track")
  # Before the year: nothing so far, the whole year ahead
  s3 <- scenario_km(q, d, "2029-12-20", a0, cfg)
  expect_equal(s3$so_far, 0)
  expect_equal(s3$weeks_left, 365 / 7)
})

test_that("km: the range from weekly km straddles the target", {
  r <- fake_rides()
  r$km <- rep(c(20, 80), 26)
  s <- scenario_km(list(id = "c", metric = "ride_km_ytd", target = 2700, deadline = "2030-12-31"),
                   list(rides = r), "2030-06-30", a0, list(year = 2030))
  expect_lt(s$models$mid[2], 2700)
  expect_gte(s$models$fast[2], 2700)
  expect_equal(s$verdict, "at_risk")
})

test_that("rate: the last 28 days and the year so far", {
  cfg <- list(year = 2030)
  q <- list(id = "b", metric = "bjj_sessions_per_week", target = 3, met_at = 2.4)
  three <- tibble::tibble(date = as.Date("2030-01-01") + sort(c(seq(0, 180, 7), seq(2, 180, 7), seq(4, 180, 7))), kind = "bjj")
  expect_equal(scenario_rate(q, list(sessions = three), "2030-06-30", cfg)$verdict, "on_track")
  # Every third session dropped: 2 a week, under 2.4 in both views
  two <- three[seq_len(nrow(three)) %% 3 != 0, ]
  s <- scenario_rate(q, list(sessions = two), "2030-06-30", cfg)
  expect_lt(s$rate, 2.4)
  expect_equal(s$verdict, "behind")
  # Before the year: only the rate counts
  before <- dplyr::mutate(three, date = date - 365)
  expect_match(scenario_rate(q, list(sessions = before), "2029-12-31", cfg)$summary, "2030 starts on 1 January")
  expect_equal(scenario_rate(q, list(sessions = three[0, ]), "2030-06-30", cfg)$verdict, "none")
})

test_that("events: done, due before the stage's deadline, overdue after", {
  q <- list(id = "e", metric = "manual", deadline = "2030-12-31", stages = list(
    list(label = "A", done = "2030-02-01"), list(label = "B", deadline = "2030-05-31")))
  expect_equal(scenario_event(q, list(), "2030-05-31")$verdict, "due")
  expect_equal(scenario_event(q, list(), "2030-06-01")$verdict, "overdue")
  q$stages[[2]]$done <- "2030-04-01"
  expect_equal(scenario_event(q, list(), "2030-06-01")$verdict, "done")
})

test_that("the example config: a verdict per quest", {
  cfg <- example_oath()
  d <- c(fake_weight_data(), list(forecast = fake_forecast(), rides = fake_rides(),
       strength = tibble::tibble(date = as.Date("2030-01-01"), lift = "Barbell Bench Press", e1rm = 90),
       sessions = tibble::tibble(date = as.Date("2030-02-20"), kind = "bjj"),
       climbs = tibble::tibble(date = as.Date(character()), climb = character(), status = character(), bike_id = character())))
  s <- quest_scenarios(cfg, d, "2030-03-03")
  expect_equal(s$id, vapply(cfg$quests, `[[`, "", "id"))
  expect_true(all(s$verdict %in% names(scenario_verdict_labels)))
  expect_equal(s$verdict[s$id == "hill"], "due")
  expect_equal(s$verdict_label[s$id == "hill"], "Due")
  expect_equal(scenario_defaults(cfg)$rate_kg_week, 0.4)
  expect_null(scenario_defaults(cfg)$km_week)                  # null in YAML: the even pace
})
