source(testthat::test_path("..", "..", "R", "theme_physdev.R"))
source(testthat::test_path("..", "..", "R", "plot_engine.R"))
source(testthat::test_path("..", "..", "R", "riding.R"))
source(testthat::test_path("..", "..", "R", "plot_riding.R"))
source(testthat::test_path("..", "..", "R", "climb_record.R"))      # fmt_mmss

test_that("rides: ride types only, km and speed", {
  acts <- tibble::tibble(activity_id = 1:3, date = as.Date("2030-05-01"), name = c("a", "b", "c"),
                         sport_type = c("Ride", "Run", "GravelRide"), distance_m = c(50000, 10000, 30000),
                         moving_s = c(7200, 3000, 3600), elev_gain_m = c(500, 50, 200))
  r <- build_rides(acts, ride_types = c("Ride", "GravelRide"))
  expect_equal(r$activity_id, c("1", "3"))
  expect_equal(r$km, c(50, 30))
  expect_equal(r$avg_kmh, c(25, 30))
})

test_that("cumulative km restart each year", {
  r <- fake_riding_tables()$rides
  cum <- rides_cumulative(r)
  first_2031 <- cum[cum$year == 2031, ][1, ]
  expect_equal(first_2031$km_cum, r$km[r$year == 2031][1])
  expect_equal(max(cum$km_cum[cum$year == 2030]), sum(r$km[r$year == 2030]))
})

test_that("plan bands: seconds per band from the HR histogram, bands inclusive", {
  hist <- tibble::tibble(activity_id = "1", hr = c(130, 133, 142, 143, 150, 156, 158, 159),
                         seconds = 10)
  rides <- tibble::tibble(activity_id = "1", date = as.Date("2030-05-01"))
  b <- build_plan_bands(hist, rides, list(z2 = c(133, 142), sweet_spot = c(150, 156), sweet_spot_cap = 158))
  expect_equal(c(b$z2_s, b$sweet_spot_s, b$above_cap_s, b$hr_s), c(20, 20, 10, 80))
  expect_equal(b$hr_era, "unknown")                    # no devices given
})

test_that("plan bands: each ride labelled with its heart-rate sensor era", {
  hist <- tibble::tibble(activity_id = c("1", "2", "3"), hr = 140, seconds = 10)
  rides <- tibble::tibble(activity_id = c("1", "2", "3"), date = as.Date("2030-05-01") + 0:2)
  acts <- tibble::tibble(activity_id = c("1", "2", "3"),
                         device_name = c("Example Watch", "Example Bike Computer", "Other"))
  b <- build_plan_bands(hist, rides, list(z2 = c(133, 142), sweet_spot = c(150, 156), sweet_spot_cap = 158),
                        acts, list("Example Bike Computer" = "chest_strap", "Example Watch" = "wrist"))
  expect_equal(b$hr_era, c("wrist", "chest_strap", "unknown"))
})

test_that("riding charts build, with the target pace for target years shown", {
  t <- fake_riding_tables()
  p <- plot_rides_cumulative(rides_cumulative(t$rides), t$targets, c(2030, 2031))
  expect_true(any(vapply(p$layers, \(l) class(l$geom)[1], "") == "GeomLine"))     # 2031 target shown
  p2 <- plot_rides_cumulative(rides_cumulative(t$rides), t$targets, 2030)
  expect_false(any(vapply(p2$layers, \(l) class(l$geom)[1], "") == "GeomLine"))   # no target for 2030
  expect_s3_class(ggplot2::ggplot_build(plot_rides_monthly(t$rides, 2030)), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(plot_plan_bands(t$bands)), "ggplot_built")
})

test_that("benchmark: clean attempts filled, stopped ones labelled with their stop", {
  a <- fake_climb_tables()$efforts
  a <- a[a$climb == "Example Berg", ]
  b <- ggplot2::ggplot_build(plot_benchmark(a))
  labels <- b$data[[4]]$label
  expect_true(any(grepl("70s stop", labels)))
  expect_equal(nrow(b$data[[2]]), 2)                 # clean + top
})
