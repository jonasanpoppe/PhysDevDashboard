source(testthat::test_path("..", "..", "R", "power_model.R"))

# An invented climb: 1000 m, rising 60 m (cumulative gain 70), ridden in 200 s.
climbs <- tibble::tibble(name = c("Example Berg", "Unknown Kouter"),
                         length_m = 1000, gain_m = c(70, 70), rise_m = c(60, NA),
                         surface = c("cobbles", NA))
weights <- tibble::tibble(date = as.Date(c("2024-05-01", "2024-05-10")), trend_kg = c(80, 79))
effort <- function(climb = "Example Berg", date = "2024-05-05", moving_s = 200) {
  tibble::tibble(climb = climb, date = date, moving_s = moving_s, bike_mass_kg = 9)
}
settings <- list(kit_mass_kg = 2)   # everything else: the spec's defaults

test_that("power matches a hand calculation", {
  out <- estimate_power(effort(), climbs, weights, settings)
  m <- 80 + 9 + 2                     # weight of 1 May: the last one on or before 5 May
  v <- 1000 / 200
  e <- m * 9.81 * 60 + m * 9.81 * 0.018 * 1000 + 0.5 * 1.2 * 0.45 * v^2 * 1000
  expect_equal(out$rider_kg, 80)
  expect_equal(out$system_kg, 91)
  expect_equal(out$power_w, e / 200 / 0.97)
  expect_equal(out$wkg, out$power_w / 80)
  expect_equal(c(out$power_low, out$power_high), out$power_w * c(0.85, 1.15))
  expect_equal(out$crr_source, "surface")
  expect_false(out$short)
})

test_that("rise is the height; gain only when there is no rise", {
  no_rise <- estimate_power(effort("Unknown Kouter"), climbs, weights, settings)
  # No rise: the 70 m gain is the height; no surface: asphalt is assumed
  m <- 91
  e <- m * 9.81 * 70 + m * 9.81 * 0.006 * 1000 + 0.5 * 1.2 * 0.45 * 5^2 * 1000
  expect_equal(no_rise$power_w, e / 200 / 0.97)
  expect_equal(no_rise$crr, 0.006)
  expect_equal(no_rise$crr_source, "assumed (asphalt)")
})

test_that("short efforts are flagged; no weight yet means no estimate", {
  expect_true(estimate_power(effort(moving_s = 60), climbs, weights, settings)$short)
  early <- estimate_power(effort(date = "2024-04-01"), climbs, weights, settings)
  expect_true(is.na(early$rider_kg))
  expect_true(is.na(early$power_w))
})

test_that("settings: defaults fill in, a bad surface name stops", {
  expect_equal(power_settings(list(cda_m2 = 0.4))$cda_m2, 0.4)
  expect_equal(power_settings()$drivetrain_eff, 0.97)
  expect_error(power_settings(list(crr_unknown_surface = "gravel")), "crr_unknown_surface")
})

test_that("climb_rise takes high minus low from the segment", {
  segs <- tibble::tibble(segment_id = 1, elev_high_m = 120.4, elev_low_m = 60.1)
  cl <- tibble::tibble(name = "X", segment_id = "1")
  expect_equal(climb_rise(cl, segs)$rise_m, 60)
})
