# The app in a headless Chrome (shinytest2), on the synthetic database of
# helper-app-db.R. Two kinds of check:
#   - snapshots of each tab's rendered HTML (tests/testthat/_snaps/app-snapshots/):
#     a change to the page shows up as a diff to review, and is accepted with
#     testthat::snapshot_accept("app-snapshots")
#   - what only a browser runs: a quest card's click switching tabs, the
#     corporate switch and its memory in the browser (localStorage)
# Values, not screenshots: screenshots differ between machines (fonts load
# from Google, rendering differs by platform). Skips without Chrome.

skip_if_no_chrome <- function() {
  testthat::skip_if_not_installed("shinytest2")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  testthat::skip_if(is.null(chrome) || !nzchar(chrome), "no Chrome found")
}

# name: the snapshot files' prefix, one per test (shinytest2 numbers from 001)
local_app <- function(name = "app", env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  fake <- write_fake_app_db(dir)
  # NOT_CRAN: AppDriver skips itself "on CRAN" otherwise (this is no package)
  withr::local_envvar(PDD_DB = fake$db, PDD_RELIC_ART = file.path(dir, "relics"), NOT_CRAN = "true",
                      .local_envir = env)
  app <- shinytest2::AppDriver$new(
    testthat::test_path("..", "..", "app"), name = name, variant = NULL,
    load_timeout = 60 * 1000, timeout = 20 * 1000, height = 1000, width = 1400
  )
  withr::defer(app$stop(), envir = env)
  app
}

# An Oath page's output, after switching to its pill (hidden pages don't
# render). corporate: wait until the page is drawn in that look, not a stale
# copy.
tab_html <- function(app, tab, output, corporate = FALSE) {
  app$set_inputs(main = "oath", wait_ = FALSE)
  app$set_inputs(oath_page = tab, wait_ = FALSE)
  app$wait_for_js(sprintf("$('#%s > .oath%s').length > 0", output,
                          if (corporate) ".oath-corporate" else ":not(.oath-corporate)"),
                  timeout = 20 * 1000)
  app$wait_for_idle()
  as.character(app$get_value(output = output)$html)
}

test_that("snapshots: Home, Reliquary and Roadmap, in both looks", {
  skip_if_no_chrome()
  app <- local_app()
  for (tab in c("oath", "reliquary", "roadmap")) {
    tab_html(app, tab, paste0(tab, "-page"))
    app$expect_values(output = paste0(tab, "-page"), screenshot_args = FALSE)
  }
  app$set_inputs(corporate = TRUE)
  for (tab in c("oath", "reliquary", "roadmap")) {
    expect_match(tab_html(app, tab, paste0(tab, "-page"), corporate = TRUE), "oath-corporate")
    app$expect_values(output = paste0(tab, "-page"), screenshot_args = FALSE)
  }
})

test_that("a quest card's click opens its tab and pill; the hub links open their pages", {
  skip_if_no_chrome()
  app <- local_app()
  tab_html(app, "oath", "oath-page")
  # The first linked card on the board (bodyweight -> Body, the Weight pill)
  app$click(selector = ".oath-board > .oath-link:first-child")
  app$wait_for_js("$('#main').find('.nav-link.active').data('value') !== 'oath'", timeout = 10 * 1000)
  expect_equal(app$get_value(input = "main"), "body")
  expect_equal(app$get_value(input = "body_page"), "weight")
  # Each hub link on Home opens its Oath page
  for (page in c("roadmap", "scenarios", "reliquary")) {
    tab_html(app, "oath", "oath-page")
    app$click(selector = sprintf(".oath-more[onclick*=\"tab: '%s'\"]", page))
    app$wait_for_value(input = "oath_page", ignore = list("oath", NULL))
    expect_equal(app$get_value(input = "oath_page"), page)
  }
})

test_that("the corporate switch restyles the page and is remembered in the browser", {
  skip_if_no_chrome()
  app <- local_app()
  tab_html(app, "oath", "oath-page")
  expect_false(isTRUE(app$get_value(input = "corporate")))
  # Flip the switch as a click would (the change handler stores it)
  app$run_js("$('#corporate').prop('checked', true).trigger('change');")
  app$wait_for_value(input = "corporate", ignore = list(FALSE, NULL))
  app$wait_for_idle()
  expect_match(as.character(app$get_value(output = "oath-page")$html), "oath-corporate")
  expect_equal(app$get_js("localStorage.getItem('pdd-corporate')"), "1")
  # (A reload would end shinytest2's session, so the read-back on connect
  # isn't driven here; it is the same key.)
  app$run_js("$('#corporate').prop('checked', false).trigger('change');")
  app$wait_for_value(input = "corporate", ignore = list(TRUE, NULL))
  expect_equal(app$get_js("localStorage.getItem('pdd-corporate')"), "0")
})

test_that("Scenarios: the verdict table, and a slider that moves it", {
  skip_if_no_chrome()
  app <- local_app("scenarios")
  app$set_inputs(main = "oath", wait_ = FALSE)
  app$set_inputs(oath_page = "scenarios", wait_ = FALSE)
  app$wait_for_js("$('#scenarios-verdicts table').length > 0", timeout = 20 * 1000)
  app$set_inputs(`scenarios-quest` = "bodyweight")
  app$wait_for_idle()
  app$expect_values(output = "scenarios-verdicts", screenshot_args = FALSE)
  # The weight sliders show for the weight figure only
  expect_true(app$get_js("$('#scenarios-rate').is(':visible')"))
  before <- as.character(app$get_value(output = "scenarios-verdicts")$html)
  app$set_inputs(`scenarios-rate` = 1, `scenarios-adherence` = 1)
  app$wait_for_idle()
  expect_false(identical(before, as.character(app$get_value(output = "scenarios-verdicts")$html)))
  app$set_inputs(`scenarios-quest` = "riding")
  app$wait_for_idle()
  expect_false(app$get_js("$('#scenarios-rate').is(':visible')"))
  expect_true(app$get_js("$('#scenarios-km_week').is(':visible')"))
})
