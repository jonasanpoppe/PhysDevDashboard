# Run with: testthat::test_dir("tests/testthat")
# The functions live in R/, which tar_source() loads for the pipeline.
source(testthat::test_path("..", "..", "R", "e1rm.R"))

test_that("Wathen matches hand-checked values", {
  expect_equal(e1rm_wathen(95, 9, 2), 131.26, tolerance = 1e-3)  # bench, 12 Nov
  expect_equal(e1rm_wathen(55, 9, 3), 77.82, tolerance = 1e-3)   # front squat, 12 Nov
  expect_equal(e1rm_wathen(100, 1, 0), 101.30, tolerance = 1e-3) # known quirk at r = 1
})

test_that("Wathen is vectorised and propagates NA", {
  expect_equal(e1rm_wathen(c(100, NA), c(5, 5), c(1, 1))[2], NA_real_)
  expect_length(e1rm_wathen(c(80, 90, 100), 5, 2), 3)
})

# Cleaned set log, as load_macrofactor_workouts() builds it before add_set_e1rm()
hack_squat_sets <- function() {
  data.frame(
    date = as.Date("2026-11-12"),
    exercise = "Hack Squat",
    set_type = c("Warm-Up Set", "Standard Set", "Standard Set", "Standard Set", "Myo Set"),
    weight_kg = c(150, 80, 90, 120, 130),
    exercise_base_weight_kg = 12.9,
    reps = c(10, 10, 8, 5, 6),
    rir = c(NA, 2, 2, 4, 1)
  )
}

test_that("set-level e1RM only on trusted working sets, with base weight", {
  out <- add_set_e1rm(hack_squat_sets())
  expect_equal(nrow(out), 5)                          # no rows dropped
  expect_equal(out$load_kg[2], 92.9)
  expect_equal(is.na(out$e1rm_kg), c(TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(out$e1rm_kg[3], e1rm_wathen(102.9, 8, 2))
})

test_that("assisted lifts use bodyweight minus assistance", {
  sets <- data.frame(
    exercise = "Machine-Assisted Triceps Dip",
    set_type = "Standard Set",
    weight_kg = c(-40, -40),
    exercise_base_weight_kg = NA_real_,
    reps = 10, rir = 1,
    bodyweight_kg = c(85, NA)
  )
  out <- add_set_e1rm(sets)
  expect_equal(out$load_kg, c(45, NA))
  expect_equal(out$e1rm_kg[1], e1rm_wathen(45, 10, 1))
  expect_true(is.na(out$e1rm_kg[2]))              # no bodyweight, no e1RM
})

test_that("session best excludes warm-ups, myo sets and high RIR", {
  out <- session_best_wathen(add_set_e1rm(hack_squat_sets()))
  expect_equal(nrow(out), 1)
  expect_equal(out$top_set, "102.9 x 8 @ 2 RIR")
  expect_equal(out$e1rm, round(e1rm_wathen(102.9, 8, 2), 1))
})
