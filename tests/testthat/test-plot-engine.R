source(testthat::test_path("..", "..", "R", "theme_physdev.R"))
source(testthat::test_path("..", "..", "R", "plot_engine.R"))

clean_efforts <- function() {
  e <- fake_climb_tables()$efforts          # helper-climb-tables.R
  e[e$status %in% c("clean", "top"), ]
}

test_that("one dot per clean ascent, all FTP bands in the visible range", {
  b <- ggplot2::ggplot_build(plot_engine(clean_efforts()))
  layers <- vapply(b$plot$layers, \(l) class(l$geom)[1], "")
  dots <- b$data[layers == "GeomPoint"]
  expect_equal(sum(vapply(dots, nrow, 1L)), 3)
  band_labels <- b$data[[which(layers == "GeomText")[1]]]$label
  expect_true(all(c("Fair", "Good", "Excellent") %in% band_labels))
})

test_that("short efforts are hollow, ranges can be hidden, labels are opt-in", {
  e <- clean_efforts()
  e$short[1] <- TRUE
  b <- ggplot2::ggplot_build(plot_engine(e))
  layers <- vapply(b$plot$layers, \(l) class(l$geom)[1], "")
  expect_equal(nrow(b$data[layers == "GeomPoint"][[2]]), 1)      # the hollow layer
  no_range <- plot_engine(e, show_range = FALSE)
  expect_false("GeomLinerange" %in% vapply(no_range$layers, \(l) class(l$geom)[1], ""))
  labelled <- plot_engine(e, label_climbs = "Example Berg")
  expect_equal(sum(vapply(labelled$layers, \(l) class(l$geom)[1], "") == "GeomText"), 2)
})

test_that("a year has the same colour in every view, whatever is shown", {
  cols <- engine_year_colours(c(2023, 2025, 2026))
  expect_equal(names(cols), c("2023", "2025", "2026"))
  expect_equal(unname(cols["2026"]), physdev$cat[[4]])
  expect_equal(unname(engine_year_colours(2022:2026)["2026"]), unname(cols["2026"]))
})
