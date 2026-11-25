# Golden test on real data (local only): the lifting history and the
# strength forecast reproduce the numbers in the private specs. Expected
# values: private/golden_strength_forecast.yml (gitignored); data:
# db/health.db. Without either, the test skips.

root <- testthat::test_path("..", "..")
golden_path <- file.path(root, "private", "golden_strength_forecast.yml")
db_path <- file.path(root, "db", "health.db")

golden_data <- function() {
  testthat::skip_if_not(file.exists(golden_path), "no private/golden_strength_forecast.yml")
  testthat::skip_if_not(file.exists(db_path), "no db/health.db (run just build)")
  source(file.path(root, "R", "forecast.R"))
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  list(g = yaml::read_yaml(golden_path),
       sets = DBI::dbReadTable(con, "lifting_sets"),
       sessions = DBI::dbReadTable(con, "strength_sessions"))
}

test_that("golden: lifting history counts and the switch between sources", {
  d <- golden_data()
  n <- d$sets %>% dplyr::filter(!is_assisted, !is.na(e1rm_kg)) %>% dplyr::count(source)
  expect_equal(n$n[match(names(d$g$e1rm_sets), n$source)], unname(unlist(d$g$e1rm_sets)))
  sw <- d$sessions %>% dplyr::filter(lift == "Barbell Bench Press", as.character(date) %in% d$g$switch_bench$dates)
  expect_equal(round(sw$e1rm[order(sw$date)]), d$g$switch_bench$e1rm)
})

test_that("golden: elite line, attempt level, first-round curve", {
  g <- golden_data()$g
  a <- g$attempt_level
  expect_equal(attempt_level(a$threshold_bw, a$bodyweight_kg, a$single_ratio), a$expected, tolerance = 0.1, scale = 1)
  e <- g$elite_line_target_body
  expect_equal(unname(round(elite_line(e$ffm_kg, e$height_cm))), c(e$squat, e$bench, e$deadlift))
  fr <- first_round_curve(g$first_round_squat$par, as.Date(g$first_round_squat$date))
  expect_equal(fr$exponential, g$first_round_squat$exponential, tolerance = 0.1, scale = 1)
  expect_equal(fr$logarithmic, g$first_round_squat$logarithmic, tolerance = 0.1, scale = 1)
})

test_that("golden: singles against the 2-6-rep e1RM", {
  d <- golden_data()
  for (ex in names(d$g$single_ratio)) {
    pr <- single_ratio_pairs(d$sets, ex)
    expect_equal(nrow(pr), d$g$single_ratio[[ex]]$pairs, info = ex)
    expect_equal(stats::median(pr$ratio), d$g$single_ratio[[ex]]$median, tolerance = 0.002, scale = 1, info = ex)
  }
})

test_that("golden: bench time rules (training months, straight line, months to the target)", {
  d <- golden_data()
  b <- d$g$bench_time_rule
  sessions <- d$sessions %>% dplyr::filter(as.Date(date) <= as.Date(b$until))
  elite <- elite_line(d$g$elite_line_target_body$ffm_kg, d$g$elite_line_target_body$height_cm)[["bench"]]
  for (rule in b$rules) {
    round <- lift_round(sessions, "Barbell Bench Press", b$round_start, rule$gap_cap_days)
    t_now <- utils::tail(round$t, 1)
    expect_equal(t_now, rule$t_now, tolerance = 0.05, scale = 1)
    expect_equal(straight_line_months(round, b$target_kg)$months, rule$straight, tolerance = 0.1, scale = 1)
    apt <- as.numeric(names(rule$months))
    p <- plateau_params(round, apt, elite)
    got <- time_to_target(p, b$target_kg, t_now, max(round$date))$months
    expect_equal(got, unname(unlist(rule$months)), tolerance = 0.2, scale = 1, info = paste("gap cap", rule$gap_cap_days))
  }
})

test_that("golden: on the bar", {
  d <- golden_data()
  for (ex in names(d$g$on_the_bar)) {
    expect_equal(on_the_bar(d$sets, ex, "2026-01-01")$load_kg, d$g$on_the_bar[[ex]]$since_2026, info = ex)
    expect_equal(on_the_bar(d$sets, ex)$load_kg, d$g$on_the_bar[[ex]]$ever, info = ex)
  }
})
