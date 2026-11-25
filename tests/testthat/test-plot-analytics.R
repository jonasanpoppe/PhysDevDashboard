source(testthat::test_path("..", "..", "R", "theme_physdev.R"))
source(testthat::test_path("..", "..", "R", "plot_engine.R"))
source(testthat::test_path("..", "..", "R", "analytics.R"))
source(testthat::test_path("..", "..", "R", "plot_analytics.R"))

test_that("analytics charts build", {
  d <- fake_analytics_daily()
  s <- analytics_series(d, analytics_variables(d))
  p <- analytics_pair(d, s, "scale_kg", "steps", smooth = "7d")
  sc <- plot_analytics_scatter(p, "Scale weight, kg", "Steps, steps")
  expect_s3_class(ggplot_build(sc), "ggplot_built")
  expect_equal(sc$labels$x, "Scale weight, kg")
  for (f in analytics_fits) {
    expect_s3_class(ggplot_build(plot_analytics_scatter(p, "X", "Y", fit = f)), "ggplot_built")
  }

  td <- bind_rows(smooth_series(filter(d, variable == "scale_kg"), "level", "7d") %>% mutate(panel = "A"),
                  smooth_series(filter(d, variable == "ride_h"), "volume", "7d") %>% mutate(panel = "B"))
  tp <- ggplot_build(plot_analytics_time(td))
  expect_equal(length(unique(tp$layout$layout$PANEL)), 2)      # two panels, never a dual axis

  m <- analytics_matrix(d, s, smooth = "daily")
  mb <- ggplot_build(plot_analytics_matrix(m, setNames(s$series_label, s$series)))
  expect_equal(nrow(mb$data[[1]]), nrow(s)^2)
})

test_that("top pairs: each pair once, never a variable against its other era", {
  d <- fake_analytics_daily()
  s <- analytics_series(d, analytics_variables(d))
  top <- analytics_top_pairs(analytics_matrix(d, s, smooth = "daily"), s, k = 50)
  expect_false(any(top$x == top$y))
  expect_equal(nrow(dplyr::distinct(top, a = pmin(x, y), b = pmax(x, y))), nrow(top))
  expect_equal(top$rho, top$rho[order(-abs(top$rho))])
  top3 <- analytics_top_pairs(analytics_matrix(d, s, lag = 3, smooth = "daily"), s, lag = 3, k = 3)
  expect_equal(top3$x[1], "scale_kg")                           # the lagged relationship comes first
})
