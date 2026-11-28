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
  expect_equal(g$ticked, c(FALSE, TRUE, FALSE, FALSE))     # the checkpoint gate: no checkpoints given
  expect_equal(g$ticked_on[2], as.Date("2030-04-12"))
  expect_equal(g$date[1], as.Date("2030-05-01"))
  expect_false(any(roadmap_gates(cfg)$ticked))                       # no relic table yet
  expect_equal(nrow(roadmap_gates(list())), 0)
})

test_that("a gate naming a quest ticks when that quest's checkpoint is met", {
  cps <- tibble::tibble(id = "squat", value = 125, due = as.Date("2030-06-30"), met = TRUE,
                        met_on = as.Date("2030-06-02"))
  g <- roadmap_gates(cfg, NULL, cps)
  expect_equal(g$ticked[g$label == "Squat checkpoint"], TRUE)
  expect_equal(g$ticked_on[g$label == "Squat checkpoint"], as.Date("2030-06-02"))
  cps$met <- FALSE; cps$met_on <- as.Date(NA)
  expect_false(roadmap_gates(cfg, NULL, cps)$ticked[4])
})

test_that("now (the Home hub): current phases by lane, the next open gate", {
  p <- roadmap_phases(cfg, as_of = "2030-04-10")
  relics <- tibble::tibble(id = c("I", "II", "V"), unlocked = c(TRUE, FALSE, FALSE),
                           date = as.Date(c("2030-03-01", NA, NA)))
  n <- roadmap_now(p, roadmap_gates(cfg, relics))
  expect_equal(n$current, c("Two sessions a week", "Cut, 0.4 kg/wk to 78 kg", "100 km / 800 m"))  # lane order, no **
  expect_false(n$gate == "78 kg")                                    # gate I is ticked: skipped
  expect_false(is.na(n$gate))
  # Before the year: no phase is current; with every gate ticked, no next gate
  n0 <- roadmap_now(roadmap_phases(cfg, as_of = "2029-12-01"), NULL)
  expect_equal(n0$current, character(0))
  expect_true(is.na(n0$gate))
})
