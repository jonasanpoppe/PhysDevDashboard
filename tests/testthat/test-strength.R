source(testthat::test_path("..", "..", "R", "e1rm.R"))
source(testthat::test_path("..", "..", "R", "strength.R"))
library(dplyr)

weights <- tibble::tibble(date = as.Date(c("2030-01-01", "2030-02-01")), trend_kg = c(80, 78))

test_that("session best per lift, grouped; warm-ups, high RIR and unmapped lifts left out", {
  s <- build_strength_sessions(fake_sets(), fake_lifts(), weights)
  expect_equal(nrow(s), 3)
  expect_false("Cable Curl" %in% s$lift)
  jan <- s[s$lift == "Barbell Bench Press" & s$date == as.Date("2030-01-06"), ]
  # Wathen: 90 x 5 @ 2 (r = 7) beats 85 x 8 @ 1 (r = 9)? Compare directly
  expect_equal(jan$e1rm, round(max(e1rm_wathen(90, 5, 2), e1rm_wathen(85, 8, 1)), 1))
  feb <- s[s$lift == "Barbell Bench Press" & s$date == as.Date("2030-02-10"), ]
  expect_equal(feb$top_set, "95 x 5 @ 2 RIR")              # the RIR-4 set doesn't count
  hack <- s[s$lift == "Hack Squat", ]
  expect_equal(hack$e1rm, round(e1rm_wathen(120, 8, 2), 1))   # base weight added
  expect_equal(hack$group, "Squat")
})

test_that("formula switch and bodyweight on the day", {
  s <- build_strength_sessions(fake_sets(), fake_lifts(), weights, settings = list(e1rm_formula = "epley"))
  feb <- s[s$lift == "Barbell Bench Press" & s$date == as.Date("2030-02-10"), ]
  expect_equal(feb$e1rm, round(95 * (1 + 7 / 30), 1))
  expect_equal(feb$bodyweight_kg, 78)                         # last trend on or before
  expect_equal(feb$x_bodyweight, feb$e1rm / 78)
  expect_equal(unique(s$formula), "epley")
  expect_error(build_strength_sessions(fake_sets(), fake_lifts(), weights,
                                       settings = list(e1rm_formula = "brzycki")), "Unknown e1RM formula")
})

test_that("MacroFactor's own 1-RM joins on its cleaned exercise names", {
  mf <- tibble::tibble(date = as.Date("2030-02-10"), exercise = "barbell_bench_press_kg", x1_rm = 112)
  s <- build_strength_sessions(fake_sets(), fake_lifts(), weights, mf_exercise = mf)
  expect_equal(s$macrofactor_e1rm[s$date == as.Date("2030-02-10")], 112)
})

test_that("goal tables: targets with their main lift, ceilings per profile", {
  g <- strength_goal_tables(fake_lifts(), fake_strength_goal())
  expect_equal(g$targets$main_lift, c("Barbell Bench Press", "Front Squat"))
  expect_equal(g$ceilings$low, c(110, 140))
})
