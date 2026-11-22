source(testthat::test_path("..", "..", "R", "load_macrofactor_workouts.R"))

test_that("double-encoded UTF-8 is repaired, clean text untouched", {
  mangled <- iconv(c("45° Leg Press", "Dip ∈ SS1"), "latin1", "UTF-8")
  # iconv above double-encodes the UTF-8 bytes, as MacroFactor does
  expect_equal(fix_double_utf8(mangled), c("45° Leg Press", "Dip ∈ SS1"))
  expect_equal(fix_double_utf8(c("Bench Press", NA)), c("Bench Press", NA))
  expect_equal(fix_double_utf8("45° Leg Press"), "45° Leg Press")  # already fine
})

test_that("bodyweight joins the closest earlier day", {
  sets <- data.frame(date = as.Date(c("2026-11-10", "2026-11-12", "2026-01-01")))
  bw <- data.frame(
    date = as.Date(c("2026-11-09", "2026-11-12", "2026-11-13")),
    bodyweight_kg = c(84, 85, 86)
  )
  expect_equal(join_bodyweight(sets, bw)$bodyweight_kg, c(84, 85, NA))
  expect_true(all(is.na(join_bodyweight(sets, NULL)$bodyweight_kg)))
})
