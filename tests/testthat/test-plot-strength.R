source(testthat::test_path("..", "..", "R", "theme_physdev.R"))
source(testthat::test_path("..", "..", "R", "e1rm.R"))
source(testthat::test_path("..", "..", "R", "strength.R"))
source(testthat::test_path("..", "..", "R", "plot_strength.R"))

sessions <- function() {
  build_strength_sessions(fake_sets(), fake_lifts(),
                          tibble::tibble(date = as.Date("2030-01-01"), trend_kg = 80))
}

test_that("now: the main lift's best of the recent window", {
  n <- strength_now(sessions())
  expect_equal(n$lift, "Barbell Bench Press")                 # only main lifts
  expect_equal(n$date, as.Date("2030-02-10"))
  expect_true(n$recent)
})

test_that("strength charts build; lines break at long gaps", {
  s <- sessions()
  b <- ggplot2::ggplot_build(plot_strength_lifts(s, "Bench", 110))
  expect_equal(length(unique(b$data[[1]]$group)), 1)          # Jan and Feb: 35 days apart, one run
  s2 <- rbind(s, transform(s[1, ], date = as.Date("2030-06-01")))
  b2 <- ggplot2::ggplot_build(plot_strength_lifts(s2, "Bench", 110))
  expect_equal(length(unique(b2$data[[1]]$group)), 2)         # a 16-week gap breaks the line
  g <- strength_goal_tables(fake_lifts(), fake_strength_goal())
  expect_s3_class(ggplot2::ggplot_build(plot_strength_targets(strength_now(s), g$targets, g$ceilings)), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(plot_strength_standards(strength_now(s), strength_standards)), "ggplot_built")
})
