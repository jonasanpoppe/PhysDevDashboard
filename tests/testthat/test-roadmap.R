# The Roadmap (R/roadmap.R) on the example config.

source(testthat::test_path("..", "..", "R", "roadmap.R"))

cfg <- example_oath()

test_that("phases: grid column, span and state on a day", {
  p <- roadmap_phases(cfg, as_of = "2030-04-10")
  expect_equal(nrow(p), length(cfg$roadmap$phases))
  cut <- p[p$text == "Cut, 0.4 kg/wk to **78 kg**", ]
  expect_equal(c(cut$col, cut$span), c(2L, 5L))                      # January to May
  expect_equal(cut$to, as.Date("2030-05-31"))                        # a month runs to its last day
  expect_equal(p$state[p$text == "Base"], "done")
  expect_equal(p$state[p$text == "100 km / 800 m"], "current")
  expect_equal(p$state[p$text == "Tournament"], "ahead")
  expect_equal(p$lane_ord[p$text == "Hold"], 2L)
})

test_that("phases outside the year are clipped to it; an unknown lane is an error", {
  c2 <- cfg
  c2$roadmap$phases <- list(list(lane = "judo", from = "2029-11", to = "2030-02-15", text = "x"))
  p <- roadmap_phases(c2, "2030-01-01")
  expect_equal(c(p$col, p$span), c(2L, 2L))
  c2$roadmap$phases[[1]]$lane <- "nope"
  expect_error(roadmap_phases(c2), "no lane 'nope'")
})

test_that("today: the share of the year, NA outside it", {
  expect_equal(roadmap_today(cfg, "2030-01-01"), 0)
  expect_equal(roadmap_today(cfg, "2030-07-02"), 182 / 365)
  expect_true(is.na(roadmap_today(cfg, "2031-01-01")))
})

test_that("gates tick when their relic is unlocked", {
  relics <- tibble::tibble(id = c("I", "II", "V"), unlocked = c(FALSE, TRUE, FALSE),
                           date = as.Date(c(NA, "2030-04-12", NA)))
  g <- roadmap_gates(cfg, relics)
  expect_equal(g$ticked, c(FALSE, TRUE, FALSE))
  expect_equal(g$ticked_on[2], as.Date("2030-04-12"))
  expect_equal(g$date[1], as.Date("2030-05-01"))
  expect_false(any(roadmap_gates(cfg)$ticked))                       # no relic table yet
  expect_equal(nrow(roadmap_gates(list())), 0)
})
