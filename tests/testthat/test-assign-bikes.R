source(testthat::test_path("..", "..", "R", "assign_bikes.R"))

# Invented bikes, shaped like config/bikes.example.csv
bikes <- tibble::tribble(
  ~bike_id, ~strava_gear_id, ~assign,  ~valid_from,  ~valid_to,    ~mass_kg, ~lowest_gear_m,
  "roadie", "b1",            "gear",   NA,           "2030-02-28", 8.8,      2.20,
  "roadie", "b1",            "gear",   "2030-03-01", NA,           8.8,      2.05,
  "hybrid", "b2",            "gear",   NA,           NA,           11.0,     2.10,
  "hybrid", NA,              "no_gear", NA,          "2029-12-31", 11.0,     2.10,
  "rental", NA,              "dates",  "2030-06-01", "2030-06-07", 7.5,      NA
)

rides <- tibble::tribble(
  ~activity_id, ~date,        ~gear_id,
  1,            "2029-05-01", NA,      # no gear in Strava -> hybrid
  2,            "2030-01-10", "b1",    # roadie, first configuration
  3,            "2030-04-10", "b1",    # roadie, after the cassette change
  4,            "2030-06-03", "b1",    # rental week: dates beats gear
  5,            "2029-07-01", NA       # no gear, but overridden
)

test_that("rules apply in order: override > dates > gear > no_gear", {
  ov <- tibble::tibble(activity_id = 5, bike_id = "roadie", reason = "fast ride")
  out <- assign_bikes(rides, bikes, ov)
  expect_equal(out$bike_id, c("hybrid", "roadie", "roadie", "rental", "roadie"))
  expect_equal(out$bike_source, c("no_gear", "gear", "gear", "dates", "override"))
  expect_equal(out$lowest_gear_m, c(2.10, 2.20, 2.05, NA, 2.20))
  expect_equal(out$bike_mass_kg[4], 7.5)
})

test_that("a ride no rule claims stops the build", {
  stray <- tibble::tibble(activity_id = 9, date = "2030-05-01", gear_id = NA)  # after no_gear ends
  expect_error(assign_bikes(stray, bikes), "No bike for ride")
})

test_that("two rows of the same rule claiming a ride stop the build", {
  dup <- dplyr::bind_rows(bikes, tibble::tibble(bike_id = "other", strava_gear_id = "b2",
                                                assign = "gear", mass_kg = 9))
  ride_b2 <- tibble::tibble(activity_id = 7, date = "2030-01-01", gear_id = "b2")
  expect_error(assign_bikes(ride_b2, dup), "more than one row")
})

test_that("an override to an unknown bike stops the build", {
  ov <- tibble::tibble(activity_id = 1, bike_id = "ghost")
  expect_error(assign_bikes(rides, bikes, ov), "unknown bike_id")
})
