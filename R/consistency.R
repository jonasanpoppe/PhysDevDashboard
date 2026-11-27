library(dplyr)

# The consistency check across tabs: numbers that two views show must agree,
# because they come from one source (goals.yml via R/plan_config.R) and one
# function each (trend_rate(), the 28-day counts, the 80% rule). This reads
# the built database and lists every pair that differs, so a drift shows up
# in `just build` instead of on the screen.
#
# One row per check: what, the two places, their values, ok. The checks
# whose tables are missing are left out (a partial database, the fixture).

check_views <- function(con) {
  has <- function(t) DBI::dbExistsTable(con, t)
  get <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
  out <- list()
  same <- function(what, a_name, x, b_name, y, tol = 1e-6) {
    x <- if (length(x)) x[1] else NA; y <- if (length(y)) y[1] else NA
    ok <- if (inherits(x, "Date") || inherits(y, "Date")) identical(as.Date(x), as.Date(y)) else
      (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && abs(as.numeric(x) - as.numeric(y)) <= tol)
    # (values first: tibble() evaluates in order, so `a = ...` would shadow them)
    xv <- format(x); yv <- format(y)
    out[[length(out) + 1]] <<- tibble::tibble(check = what, a = a_name, a_value = xv, b = b_name,
                                              b_value = yv, ok = ok)
  }
  if (!has("quest_board")) return(tibble::tibble(check = character(), a = character(), a_value = character(),
                                                 b = character(), b_value = character(), ok = logical()))
  qb <- get("SELECT * FROM quest_board")
  q <- function(metric) qb[qb$metric == metric, ]

  if (has("weight_status") && nrow(q("weight_trend_kg"))) {
    ws <- get("SELECT * FROM weight_status")
    w <- q("weight_trend_kg")
    same("trend weight now", "Weight tile", ws$trend_kg, "Home card", w$now_value)
    same("trend weight date", "Weight tile", ws$date, "Home card", w$now_date)
    if (has("weight_goal")) same("weight target", "goals.yml (Weight tab)", get("SELECT target_kg FROM weight_goal")$target_kg,
                                 "Home card", w$target_value)
  }
  if (has("weight_status") && nrow(q("bjj_sessions_per_week"))) {
    ws <- get("SELECT * FROM weight_status")
    b <- q("bjj_sessions_per_week")
    same("BJJ a week, last 28 days", "Weight tile", ws$bjj_per_week, "Home", b$now_value)
    same("BJJ goal", "Weight tile", ws$bjj_goal, "Home", b$target_value)
    same("BJJ met at", "Weight tile", ws$bjj_met_at, "Home", b$met_at)
    same("counted up to", "Weight tile", ws$as_of, "Home", b$as_of)
  }
  if (has("strength_targets")) {
    st <- get("SELECT * FROM strength_targets")
    for (i in which(qb$metric == "e1rm_bar_kg")) {
      t <- st[st$main_lift %in% qb$exercise[i], ]
      same(sprintf("%s target on the bar", qb$exercise[i]), "Strength tab", t$target_kg, "Home card", qb$target_value[i])
    }
    if (has("strength_forecast_settings")) {
      fs <- get("SELECT exercise, target_kg, single_ratio FROM strength_forecast_settings")
      for (i in seq_len(nrow(fs))) {
        t <- st[st$main_lift %in% fs$exercise[i], ]
        if (nrow(t) && !is.na(fs$target_kg[i])) {
          same(sprintf("%s forecast target", fs$exercise[i]), "Strength tab (target / ratio)", t$target_e1rm_kg,
               "Forecast", fs$target_kg[i], tol = 0.05)
        }
      }
    }
  }
  if (has("riding_targets") && nrow(q("ride_km_ytd"))) {
    rt <- get("SELECT * FROM riding_targets")
    c1 <- q("ride_km_ytd")
    yr <- if (has("oath_config")) jsonlite::fromJSON(get("SELECT json FROM oath_config")$json)$year else NA
    same("km a year", "Engine tab", rt$target_km[rt$year == yr], "Home card", c1$target_value)
  }
  for (t in c("quest_scenarios", "relic_status", "roadmap_phases")) {
    if (has(t)) same("as of", "Home", qb$as_of, t, get(sprintf("SELECT max(as_of) AS d FROM %s", t))$d)
  }
  dplyr::bind_rows(out)
}

# For the pipeline: stop the build when two views disagree
assert_views <- function(db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  v <- check_views(con)
  bad <- v %>% filter(!ok)
  if (nrow(bad)) {
    stop("The tabs disagree (R/consistency.R):\n",
         paste0("  - ", bad$check, ": ", bad$a, " ", bad$a_value, ", ", bad$b, " ", bad$b_value, collapse = "\n"),
         call. = FALSE)
  }
  message("✓ views consistent (", nrow(v), " checks)")
  invisible(v)
}
