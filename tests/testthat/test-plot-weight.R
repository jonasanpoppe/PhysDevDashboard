source(testthat::test_path("..", "..", "R", "theme_physdev.R"))
source(testthat::test_path("..", "..", "R", "weight_plan.R"))
source(testthat::test_path("..", "..", "R", "plot_weight.R"))

test_that("weight charts build: trend with pace, weeks by status, intake with goal", {
  d <- build_weight_daily(fake_macrofactor_daily(), fake_goal())
  w <- build_weight_weekly(d, tibble::tibble(date = as.Date(character()), kind = character()), fake_goal())
  b <- ggplot2::ggplot_build(plot_weight_trend(d, fake_goal()))
  expect_equal(min(b$data[[3]]$y), 75)                       # the pace reaches the target
  wk <- ggplot2::ggplot_build(plot_weight_weeks(w))
  expect_gt(nrow(wk$data[[2]]), 0)
  intake <- plot_weight_intake(w, "protein_g", goal_value = 140)
  expect_true("GeomHline" %in% vapply(intake$layers, \(l) class(l$geom)[1], ""))
  expect_false("GeomHline" %in% vapply(plot_weight_intake(w, "steps")$layers, \(l) class(l$geom)[1], ""))
})
