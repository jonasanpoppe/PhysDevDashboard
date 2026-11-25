source(testthat::test_path("..", "..", "R", "theme_physdev.R"))
source(testthat::test_path("..", "..", "R", "plot_fitness.R"))

test_that("the charts share one date range", {
  f <- fake_fitness_tables()
  x <- fitness_xlim(f$seasons, f$load)
  expect_equal(x, c(as.Date("2030-03-01") - 60, max(f$load$date)))
  for (p in list(plot_fitness_seasons(f$seasons, x, 150), plot_fitness_load(f$load, x))) {
    b <- ggplot2::ggplot_build(p)
    expect_equal(as.Date(b$layout$panel_params[[1]]$x$limits, origin = "1970-01-01"), x,
                 tolerance = 1)
  }
})

test_that("seasons: a band per season with its label", {
  f <- fake_fitness_tables()
  x <- fitness_xlim(f$seasons, f$load)
  b <- ggplot2::ggplot_build(plot_fitness_seasons(f$seasons, x, 150))
  expect_equal(nrow(b$data[[1]]), 2)                             # one band each
  expect_match(b$data[[3]]$label[1], "^2030: 360 W \\(330")
})
