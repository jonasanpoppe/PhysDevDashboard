source(testthat::test_path("..", "..", "R", "theme_physdev.R"))
source(testthat::test_path("..", "..", "R", "plot_vitals.R"))

# Synthetic vitals_daily rows: two metrics, Fitbit then Garmin.
fake_vitals <- function() {
  days_f <- as.Date("2024-01-01") + 0:59
  days_g <- as.Date("2024-03-01") + 0:59
  rows <- function(days, era, metric, value) {
    is_hrv <- metric == "hrv"   # decided before tibble(), where `metric` is the column
    tibble::tibble(date = days, metric = metric, era = era, value = value,
                   derived = FALSE, mean_7d = value,
                   band_low = if (is_hrv) value - 5 else NA_real_,
                   band_high = if (is_hrv) value + 5 else NA_real_,
                   band_source = if (is_hrv) "garmin" else NA_character_)
  }
  dplyr::bind_rows(
    rows(days_f, "fitbit", "rhr", 60), rows(days_g, "garmin", "rhr", 55),
    rows(days_f, "fitbit", "hrv", 40), rows(days_g, "garmin", "hrv", 50),
    rows(days_g, "garmin", "sleep_s", 7.5 * 3600)
  )
}

test_that("switch date is the first Garmin day", {
  expect_equal(vitals_switch_date(fake_vitals()), as.Date("2024-03-01"))
})

test_that("plot builds with a panel per metric, sleep in hours", {
  b <- ggplot2::ggplot_build(plot_vitals(fake_vitals()))
  expect_equal(nrow(b$layout$layout), 3)        # rhr, hrv, sleep
  points <- b$data[[2]]                          # geom_point layer
  expect_true(7.5 %in% points$y)                 # sleep scaled to hours
})

test_that("each era keeps its own colour whatever the range", {
  v <- fake_vitals()
  all_colours <- unique(ggplot2::ggplot_build(plot_vitals(v))$data[[2]]$colour)
  expect_setequal(all_colours, unname(vitals_era_colours()))
  garmin_only <- unique(ggplot2::ggplot_build(
    plot_vitals(v, from = as.Date("2024-03-10"))
  )$data[[2]]$colour)
  expect_equal(garmin_only, unname(vitals_era_colours()["garmin"]))
})

test_that("switch label appears once, and only when the switch is in range", {
  v <- fake_vitals()
  layers <- function(p) vapply(p$layers, \(l) class(l$geom)[1], "")
  expect_equal(sum(layers(plot_vitals(v)) == "GeomText"), 1)
  text_rows <- ggplot2::ggplot_build(plot_vitals(v))$data[[5]]
  expect_equal(nrow(text_rows), 1)
  expect_false("GeomVline" %in% layers(plot_vitals(v, from = as.Date("2024-03-10"))))
})

test_that("metric selection and hidden points", {
  v <- fake_vitals()
  b <- ggplot2::ggplot_build(plot_vitals(v, metrics = c("hrv", "rhr")))
  expect_equal(nrow(b$layout$layout), 2)
  # Panels keep the fixed order (rhr before hrv), whatever the selection order
  expect_equal(levels(b$plot$data$panel), c("Resting heart rate (bpm)", "HRV, overnight RMSSD (ms)"))
  hidden <- ggplot2::ggplot_build(plot_vitals(v, show_points = FALSE))
  expect_equal(nrow(hidden$data[[2]]), 0)
})
