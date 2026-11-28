# The Quest Board's small charts (R/plot_quests.R): inline SVG

library(htmltools)
source(testthat::test_path("..", "..", "R", "plot_quests.R"))

h <- tibble::tibble(date = as.Date("2030-01-01") + c(0, 7, 14), value = c(80, 79, 78))
svg <- function(...) as.character(quest_spark(...))

test_that("a line up to today, the target dashed, the time to go shaded", {
  s <- svg(h, "decreasing", 75, as.Date("2029-10-01"), as.Date("2030-12-31"), as.Date("2030-01-15"))
  expect_match(s, "<svg")
  expect_match(s, "class=\"spark-line\"")
  expect_match(s, "class=\"spark-now\"")
  expect_match(s, "class=\"spark-future\"")
  # The target: a horizontal line across the chart
  expect_match(s, "<line x1=\"0\" y1=\"([0-9.]+)\" x2=\"300\" y2=\"\\1\" class=\"spark-target\"")
})

test_that("a year's total: the even pace from 0 on 1 January to the target on 31 December", {
  s <- svg(h, "cumulative", 2000, as.Date("2029-10-01"), as.Date("2030-12-31"), as.Date("2030-01-15"), year = 2030)
  pace <- regmatches(s, regexpr("<line[^>]*spark-target[^>]*>", s))
  y <- as.numeric(regmatches(pace, gregexpr("(?<=y[12]=\")[0-9.]+", pace, perl = TRUE))[[1]])
  expect_gt(y[1], y[2])   # rises (SVG y runs down)
  x2 <- as.numeric(sub(".*x2=\"([0-9.]+)\".*", "\\1", pace))
  expect_equal(x2, 300)   # ends on the chart's last day
})

test_that("a rate met below its target: met_at dotted", {
  expect_match(svg(h, "rate", 3, as.Date("2029-10-01"), as.Date("2030-12-31"), as.Date("2030-01-15"), met_at = 2.4),
               "spark-met")
  expect_no_match(svg(h, "rate", 3, as.Date("2029-10-01"), as.Date("2030-12-31"), as.Date("2030-01-15"), met_at = 3),
                  "spark-met")
})

test_that("nothing to draw: no chart; stages: one pip each", {
  expect_null(quest_spark(h[0, ], "increasing", NA, as.Date("2030-01-01"), as.Date("2030-12-31"), as.Date("2030-06-01")))
  p <- as.character(quest_pips(1, 3))
  expect_equal(lengths(regmatches(p, gregexpr("class=\"oath-pip[ \"]", p))), 3)
  expect_equal(lengths(regmatches(p, gregexpr("oath-pip-done", p))), 1)
  expect_null(quest_pips(0, NA))
})
