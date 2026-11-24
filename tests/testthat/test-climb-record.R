source(testthat::test_path("..", "..", "R", "climb_record.R"))

test_that("per climb: ascents, clean (top counts), best clean, last attempt", {
  t <- fake_climb_tables()
  r <- climb_record(t$efforts, t$climbs, t$bikes)
  eb <- r[r$climb == "Example Berg", ]
  expect_equal(eb$ascents, 3)
  expect_equal(eb$clean, 2)                       # clean + top
  expect_equal(eb$best_s, 240)                    # the top ascent is fastest
  expect_equal(eb$best_bike, "Roadie")            # nickname when there is one
  expect_equal(eb$best_wkg, 380 / 80)             # W/kg of that same ascent
  expect_equal(eb$last_status, "top")
  expect_equal(eb$history[[1]]$status, c("early", "clean", "top"))  # oldest first
  muur <- r[r$climb == "Pinned Muur", ]
  expect_true(is.na(muur$best_s))                 # never clean
})

test_that("pinned climbs come first, then most ascents", {
  t <- fake_climb_tables()
  expect_equal(climb_record(t$efforts, t$climbs, t$bikes)$climb,
               c("Pinned Muur", "Example Berg", "Flat Kouter"))
})

test_that("filters: surface (unknown for empty), bike, record scope", {
  t <- fake_climb_tables()
  expect_equal(climb_record(t$efforts, t$climbs, t$bikes, surfaces = "unknown")$climb, "Flat Kouter")
  hybrid <- climb_record(t$efforts, t$climbs, t$bikes, bike_ids = "hybrid")
  expect_equal(hybrid$ascents, 2)
  scope <- climb_record(t$efforts, t$climbs, t$bikes, scope_only = TRUE)
  expect_setequal(scope$climb, c("Example Berg", "Flat Kouter"))
  expect_equal(scope$ascents[scope$climb == "Example Berg"], 2)
})

test_that("times print as m:ss, rounding first", {
  expect_equal(fmt_mmss(c(215, 119.6, 59.4, NA)), c("3:35", "2:00", "0:59", NA))
})
