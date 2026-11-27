library(dplyr)

# Analytics: any two daily variables side by side, built from the marts that
# already exist. It describes associations; it cannot show cause.
#
# analytics_daily(date, variable, era, value)
#   - one row per day, variable and era; days without a value are absent,
#     except for volumes (hours, sessions, time in band), which are 0 on days
#     without activity, within the span of their source.
#   - era: the device for vitals ("fitbit", "garmin"), "all" otherwise. A
#     variable from two devices is two series; they are never pooled.
#     "homogenised" is a third, derived series beside them: Fitbit moved onto
#     Garmin's scale at the switch, then Garmin (R/homogenise.R).
# analytics_variables: the registry below (label, group, unit, kind, agg).
#
# family: variables that are the same quantity or parts of one another
# (scale and trend weight; sleep and its stages; hours and the loads built
# from them). Pairs within a family are never correlated.
#
# agg decides smoothing:
#   level   mean over the window, at least 4 values in 7 days
#   volume  sum over the window (7-day total, weekly total)
#   sparse  mean of whatever falls in the window (e1RM, W/kg)

analytics_registry <- tibble::tribble(
  ~variable,          ~label,                         ~group,             ~unit,       ~kind,          ~agg,  ~family,
  "scale_kg",         "Scale weight",                 "Weight & intake",  "kg",        "measurement",  "level",   "weight",
  "trend_kg",         "Trend weight",                 "Weight & intake",  "kg",        "model output", "level",   "weight",
  "kcal",             "Calories eaten",               "Weight & intake",  "kcal",      "measurement",  "level",   NA,
  "protein_g",        "Protein",                      "Weight & intake",  "g",         "measurement",  "level",   NA,
  "expenditure_kcal", "Expenditure",                  "Weight & intake",  "kcal",      "model output", "level",   NA,
  "rhr",              "Resting HR",                   "Vitals",           "bpm",       "measurement",  "level",   NA,
  "hrv",              "HRV",                          "Vitals",           "ms",        "measurement",  "level",   NA,
  "sleep_h",          "Sleep",                        "Vitals",           "h",         "measurement",  "level",   "sleep",
  "sleep_deep_h",     "Deep sleep",                   "Vitals",           "h",         "measurement",  "level",   "sleep",
  "sleep_rem_h",      "REM sleep",                    "Vitals",           "h",         "measurement",  "level",   "sleep",
  "resp",             "Breathing rate",               "Vitals",           "br/min",    "measurement",  "level",   NA,
  "spo2",             "SpO2",                         "Vitals",           "%",         "measurement",  "level",   NA,
  "skin_temp_dev",    "Skin temperature (deviation)", "Vitals",           "°C",        "measurement",  "level",   NA,
  "steps",            "Steps",                        "Training",         "steps",     "measurement",  "level",   NA,
  "ride_h",           "Riding",                       "Training",         "h",         "measurement",  "volume",  "hours",
  "run_h",            "Running",                      "Training",         "h",         "measurement",  "volume",  "hours",
  "load_6w_hpw",      "6-week load",                  "Training",         "h/week",    "derived",      "level",   "hours",
  "fitness_h",        "Fitness (42-day EWMA)",        "Training",         "h/day",     "derived",      "level",   "hours",
  "fatigue_h",        "Fatigue (7-day EWMA)",         "Training",         "h/day",     "derived",      "level",   "hours",
  "bjj",              "BJJ sessions",                 "Training",         "sessions",  "measurement",  "volume",  NA,
  "lifting",          "Lifting sessions",             "Training",         "sessions",  "measurement",  "volume",  NA,
  "z2_h",             "Time in Zone 2",               "Training",         "h",         "measurement",  "volume",  "hours",
  "sweet_spot_h",     "Time in sweet spot",           "Training",         "h",         "measurement",  "volume",  "hours",
  "wkg",              "Climb W/kg (best of the day)", "Strength & bike",  "W/kg",      "model output", "sparse", NA
)

# Pairs offered as presets in the app.
analytics_presets <- tibble::tribble(
  ~preset,                        ~x,         ~y,     ~lag, ~smooth,
  "Sleep and HRV (same night)",   "sleep_h",  "hrv",  0L,   "daily",
  "Scale weight and resting HR",  "scale_kg", "rhr",  0L,   "7d"
)

# device_steps: steps from the wrist device by era (sql/marts/device_steps.sql),
# never MacroFactor's
build_analytics_daily <- function(weight_daily, vitals_daily, load, sessions, plan_bands,
                                  strength_sessions, climb_efforts, vitals_homogenised = NULL,
                                  device_steps = NULL) {
  long <- function(df, cols, era = "all") {
    purrr::imap(cols, \(col, v) tibble(date = as.Date(df$date), variable = v, era = era,
                                       value = as.numeric(df[[col]]))) %>%
      purrr::list_rbind()
  }
  # Volumes: 0 on days without activity, over the span of the source
  fill_days <- function(d, from, to) {
    tidyr::complete(d, date = seq(from, to, by = "day"), variable, era, fill = list(value = 0))
  }

  weight <- long(weight_daily, c(scale_kg = "scale_kg", trend_kg = "trend_kg", kcal = "kcal",
                                 protein_g = "protein_g", expenditure_kcal = "expenditure_kcal"))
  steps <- if (is.null(device_steps)) NULL else
    transmute(device_steps, date = as.Date(date), variable = "steps", era, value = as.numeric(steps))

  sleep_vars <- c(sleep_s = "sleep_h", sleep_deep_s = "sleep_deep_h", sleep_rem_s = "sleep_rem_h")
  vitals <- vitals_daily %>%
    select(date, metric, era, value) %>%
    bind_rows(if (!is.null(vitals_homogenised)) select(vitals_homogenised, date, metric, era, value)) %>%
    # steps: only the homogenised series comes this way (the eras come from device_steps)
    filter(metric %in% c("rhr", "hrv", "resp", "spo2", "skin_temp_dev", "steps", names(sleep_vars))) %>%
    transmute(date = as.Date(date),
              variable = if_else(metric %in% names(sleep_vars), unname(sleep_vars[metric]), metric),
              era, value = if_else(metric %in% names(sleep_vars), value / 3600, value))

  load_d <- load %>% mutate(load_6w_hpw = ride_6w_hpw + run_6w_hpw)
  training <- bind_rows(
    long(load_d, c(ride_h = "ride_h", run_h = "run_h")),
    long(load_d, c(load_6w_hpw = "load_6w_hpw", fitness_h = "fitness_h", fatigue_h = "fatigue_h"))
  )
  span <- range(as.Date(load$date))
  if (nrow(sessions) > 0) span <- range(c(span, as.Date(sessions$date)))
  # Each kind from its first recorded session on: before it the kind wasn't
  # recorded, which is not 0 (BJJ is on Strava only from the Garmin era)
  sess <- sessions %>%
    transmute(date = as.Date(date), variable = kind, era = "all", value = 1) %>%
    distinct() %>%
    { purrr::map(split(., .$variable), \(k) fill_days(k, min(k$date), span[[2]])) } %>%
    purrr::list_rbind()
  # Time in the plan's bands per heart-rate sensor era (hr_era, from the
  # recording device): wrist and chest strap read differently, so the eras
  # stay apart; each era is 0-filled over its own span
  bands <- plan_bands %>%
    mutate(era = if ("hr_era" %in% names(plan_bands)) hr_era else "all") %>%
    summarise(.by = c(date, era), z2_h = sum(z2_s) / 3600, sweet_spot_h = sum(sweet_spot_s) / 3600)
  bands <- purrr::map(split(bands, bands$era), \(b) {
    long(b, c(z2_h = "z2_h", sweet_spot_h = "sweet_spot_h"), era = b$era[[1]]) %>%
      fill_days(min(.$date), max(.$date))
  }) %>% purrr::list_rbind()

  # Best e1RM per main lift group on lift days (an estimate from a submaximal set)
  e1rm <- strength_sessions %>%
    filter(as.logical(main)) %>%
    summarise(.by = c(date, group), value = max(e1rm)) %>%
    transmute(date = as.Date(date), variable = e1rm_variable(group), era = "all", value)

  wkg <- climb_efforts %>%
    filter(status %in% c("clean", "top"), !as.logical(short), !is.na(wkg)) %>%
    summarise(.by = date, value = max(wkg)) %>%
    transmute(date = as.Date(date), variable = "wkg", era = "all", value)

  bind_rows(weight, steps, vitals, training, sess, bands, e1rm, wkg) %>%
    filter(!is.na(value), is.finite(value)) %>%
    arrange(variable, era, date)
}

e1rm_variable <- function(group) paste0("e1rm_", janitor::make_clean_names(group, allow_dupes = TRUE))

# Registry plus one row per main-lift group, keeping only variables present.
analytics_variables <- function(daily, e1rm_groups = character()) {
  extra <- tibble(variable = e1rm_variable(e1rm_groups), label = paste0("e1RM: ", e1rm_groups),
                  group = "Strength & bike", unit = "kg", kind = "model output", agg = "sparse",
                  family = NA_character_)
  bind_rows(analytics_registry, extra) %>% semi_join(distinct(daily, variable), by = "variable")
}

# Series keys: "variable" for single-era variables, "variable@era" otherwise,
# with a label that names the device when there is more than one.
analytics_series <- function(daily, variables) {
  daily %>%
    summarise(.by = c(variable, era), last_date = max(as.Date(date))) %>%
    mutate(n_eras = n(), .by = variable) %>%
    inner_join(variables, by = "variable") %>%
    # The Fitbit part of a homogenised series is adjusted by an estimate
    mutate(kind = if_else(era == "homogenised", "model output", kind)) %>%
    mutate(series = if_else(n_eras > 1, paste0(variable, "@", era), variable),
           series_label = if_else(n_eras > 1, paste0(label, " · ", era), label)) %>%
    select(series, series_label, variable, era, label, group, unit, kind, agg, family, last_date) %>%
    arrange(match(group, unique(analytics_registry$group)), match(variable, analytics_registry$variable),
            variable, era)
}

# The series for a variable: its most recent measured era (presets name
# variables; a homogenised series is never picked for them).
resolve_series <- function(series, variable) {
  s <- series[series$variable == variable & series$era != "homogenised", ]
  if (nrow(s) == 0) return(NA_character_)
  s$series[which.max(s$last_date)]
}

# TRUE where two series are the same variable or the same family.
analytics_same_family <- function(series, x, y) {
  f <- setNames(series$family, series$series); v <- setNames(series$variable, series$series)
  v[x] == v[y] | (!is.na(f[x]) & !is.na(f[y]) & f[x] == f[y])
}

# One series, smoothed. smooth: "daily", "7d" (trailing 7 calendar days) or
# "weekly" (Monday-Sunday, dated by the Monday).
smooth_series <- function(d, agg, smooth = c("7d", "daily", "weekly")) {
  smooth <- match.arg(smooth)
  d <- d %>% select(date, value) %>% arrange(date)
  if (smooth == "daily" || nrow(d) == 0) return(d)
  min_n <- switch(agg, level = 4L, volume = 7L, sparse = 1L)
  stat <- if (agg == "volume") sum else mean
  if (smooth == "weekly") {
    return(d %>%
      mutate(date = date - (as.integer(format(date, "%u")) - 1L)) %>%
      summarise(.by = date, n = n(), value = stat(value)) %>%
      filter(n >= min_n) %>%
      select(date, value))
  }
  # Trailing 7 calendar days, gaps respected
  days <- tibble(date = seq(min(d$date), max(d$date), by = "day")) %>% left_join(d, by = "date")
  v <- days$value
  ok <- !is.na(v)
  cs <- cumsum(ifelse(ok, v, 0)); cn <- cumsum(ok)
  lag7 <- function(x) c(rep(0, 7), head(x, -7))[seq_along(x)]
  s <- cs - lag7(cs); n <- cn - lag7(cn)
  days %>%
    mutate(value = if (agg == "volume") s else s / n, n = n) %>%
    filter(n >= min_n) %>%
    select(date, value)
}

# Detrending, after smoothing, one series (one era) at a time. What remains is
# the departure from the series' own slow course; the trend removed is a model
# output and is returned beside it, so it can be shown.
#   rolling  minus a centred mean of `window` days (whole weeks when weekly);
#            at the ends and beside gaps the window shrinks to what exists;
#            days where either half of the window is under half filled are
#            flagged `edge` (drawn lighter). A day needs at least half its
#            window filled, else no value.
#   loess    minus a LOESS curve (span about 90 days), fitted separately on
#            stretches without a gap over 28 days; 45 days at their ends are
#            `edge`.
#   diff     change from the step (day or week) before; no trend to show.
# Variables with a weekly rhythm (analytics_weekday_vars) lose their weekday
# effect first, when values are daily and a detrend is on. Sparse series
# (e1RM, W/kg) are never detrended: their trend is the signal.
analytics_detrends <- c("None" = "none", "Minus rolling mean" = "rolling",
                        "Minus LOESS trend" = "loess", "Day-to-day change" = "diff")
analytics_detrend_windows <- c(28L, 56L, 91L)
analytics_weekday_vars <- c("steps", "ride_h")

# d: date, value (one value per step, sorted). Returns date, value (what
# remains), raw (value before detrending), trend, edge.
detrend_series <- function(d, method = c("none", "rolling", "loess", "diff"), window = 56L,
                           weekday = FALSE, step = 1L) {
  method <- match.arg(method)
  d <- d %>% select(date, value) %>% arrange(date) %>% mutate(raw = value)
  out <- function(v_out, t_out, e_out) mutate(d, value = v_out, trend = t_out, edge = e_out) %>%
    filter(!is.na(value))
  if (method == "none" || nrow(d) == 0) return(out(d$value, NA_real_, FALSE))
  v <- d$value
  if (weekday && step == 1L) {
    wd <- format(d$date, "%u")
    v <- v - stats::ave(v, wd) + mean(v)
  }
  if (method == "diff") {
    prev <- v[match(d$date - step, d$date)]
    return(out(v - prev, NA_real_, FALSE))
  }
  if (method == "rolling") {
    grid <- seq(min(d$date), max(d$date), by = step)
    g <- v[match(grid, d$date)]
    N <- length(grid)
    half <- max(1L, as.integer(round(window / step))) %/% 2L
    i <- seq_len(N)
    lo <- pmax(1L, i - half); hi <- pmin(N, i + half)
    ok <- !is.na(g)
    cs <- c(0, cumsum(ifelse(ok, g, 0))); cn <- c(0, cumsum(ok))
    n <- cn[hi + 1] - cn[lo]
    trend <- ifelse(n >= (hi - lo + 1) / 2, (cs[hi + 1] - cs[lo]) / n, NA_real_)
    # One-sided: either half of the window under half filled (the ends of
    # the data, and the days beside a gap)
    left <- cn[i] - cn[lo]; right <- cn[hi + 1] - cn[i + 1]   # the day itself in neither
    edge <- pmin(left, right) < half / 2
    k <- match(d$date, grid)
    return(out(v - trend[k], trend[k], edge[k]))
  }
  # loess
  x <- as.numeric(d$date)
  stretch <- cumsum(c(TRUE, diff(x) > 28))
  trend <- rep(NA_real_, length(v)); edge <- rep(FALSE, length(v))
  for (st in unique(stretch)) {
    k <- which(stretch == st)
    if (length(k) < 10) next
    span <- min(1, max(90 / (diff(range(x[k])) + 1), 6 / length(k)))
    fit <- stats::loess(v[k] ~ x[k], span = span, degree = 1)
    trend[k] <- stats::fitted(fit)
    edge[k] <- x[k] - min(x[k]) < 45 | max(x[k]) - x[k] < 45
  }
  out(v - trend, trend, edge)
}

# The name of what a detrend removes, for titles and notes.
analytics_detrend_name <- function(detrend, window = 56L, smooth = "7d") {
  switch(detrend, none = "", rolling = sprintf("%d-day mean", window), loess = "LOESS trend",
         diff = if (smooth == "weekly") "change from the week before" else "change from the day before")
}

# Suffix for a series' label: what was removed from it ("" when nothing).
analytics_detrend_label <- function(s, detrend, window = 56L, smooth = "7d") {
  if (detrend == "none" || s$agg == "sparse") return("")
  wk <- if (smooth == "daily" && s$variable %in% analytics_weekday_vars) " \u2212 weekday effect" else ""
  if (detrend == "diff") paste0(wk, ", ", analytics_detrend_name(detrend, window, smooth))
  else paste0(wk, " \u2212 ", analytics_detrend_name(detrend, window, smooth))
}

# One series (a row of analytics_series), smoothed, then detrended. Returns
# date, value, raw, trend, edge. Every view gets its values from here.
analytics_values <- function(daily, s, smooth = "7d", detrend = "none", window = 56L) {
  d <- daily %>% filter(variable == s$variable, era == s$era) %>% smooth_series(s$agg, smooth)
  if (s$agg == "sparse") detrend <- "none"
  detrend_series(d, detrend, window, weekday = s$variable %in% analytics_weekday_vars,
                 step = if (smooth == "weekly") 7L else 1L)
}

# Standardised: SDs from the mean (for the overlay of several series).
analytics_z <- function(v) (v - mean(v, na.rm = TRUE)) / stats::sd(v, na.rm = TRUE)

# X and Y paired: Y `lag` days after X (whole weeks when weekly). Returns
# date (of X), x, y, year. Detrending comes before the lag shift.
analytics_pair <- function(daily, series, x, y, lag = 0L, smooth = "7d", from = NULL,
                           detrend = "none", window = 56L) {
  get <- function(key) {
    s <- series[series$series == key, ]
    if (nrow(s) != 1) stop("analytics: unknown series ", key)
    analytics_values(daily, s, smooth, detrend, window) %>% select(date, value)
  }
  shift <- if (smooth == "weekly") 7L * as.integer(round(lag / 7)) else as.integer(lag)
  out <- get(x) %>% rename(x = value) %>%
    inner_join(get(y) %>% transmute(date = date - shift, y = value), by = "date") %>%
    mutate(year = as.integer(format(date, "%Y")))
  if (!is.null(from)) out <- filter(out, date >= from)
  out
}

# Fits offered in the scatter. LOESS is drawn for the eye only: it has no
# single number to report.
analytics_fits <- c("Straight line" = "linear", "Quadratic" = "quadratic",
                    "Log of X" = "log", "LOESS (for the eye)" = "loess")

analytics_fit_formula <- function(fit) {
  switch(fit, linear = y ~ x, quadratic = y ~ x + I(x^2), log = y ~ log(x), loess = y ~ x)
}

# The numbers for a pair, with moving-block bootstrap intervals:
#   rho     Spearman's rank correlation (any steady rise or fall), every fit
#   effect  linear: change in Y per unit of X (the slope)
#           log: change in Y per doubling of X (b * ln 2, for y = a + b ln x)
#           quadratic, loess: NA
#   r2      share of Y's variance the fitted curve explains (not for loess)
#   turn    quadratic only: X where the curve turns, if inside the data
# Rows are taken in date order; blocks keep neighbouring (autocorrelated)
# days together, so the intervals are wider than naive ones, as they should
# be. rho and effect come from the same resamples.
analytics_fit_stats <- function(x, y, fit = "linear", block = 14L, reps = 1000L, level = 0.95,
                                seed = 1L) {
  fit <- match.arg(fit, unname(analytics_fits))
  n <- length(x)
  empty <- tibble(fit = fit, rho = NA_real_, rho_low = NA_real_, rho_high = NA_real_,
                  effect = NA_real_, effect_low = NA_real_, effect_high = NA_real_,
                  r2 = NA_real_, turn = NA_real_, n = n, block = as.integer(block), note = NA_character_)
  if (fit == "log" && any(x <= 0)) {
    return(mutate(empty, note = "The log fit needs X above 0 on every day."))
  }
  effect_of <- function(x, y) {
    if (stats::var(x) == 0 || stats::var(y) == 0) return(c(NA_real_, NA_real_))
    e <- switch(fit,
      linear = stats::cor(x, y) * stats::sd(y) / stats::sd(x),
      log    = { lx <- log(x); stats::cov(lx, y) / stats::var(lx) * log(2) },
      NA_real_)
    c(stats::cor(x, y, method = "spearman"), e)
  }
  if (n < 3) return(empty)
  est <- effect_of(x, y)
  out <- mutate(empty, rho = est[[1]], effect = est[[2]])
  if (fit != "loess" && n > 3) {
    m <- stats::lm(analytics_fit_formula(fit), data = tibble(x = x, y = y))
    out$r2 <- 1 - sum(stats::residuals(m)^2) / sum((y - mean(y))^2)
    if (fit == "quadratic") {
      b <- stats::coef(m)
      t <- -b[[2]] / (2 * b[[3]])
      if (is.finite(t) && t > min(x) && t < max(x)) out$turn <- t
    }
  }
  if (n < 10 || is.na(est[[1]])) return(out)
  block <- min(as.integer(block), max(1L, n %/% 3L))
  starts <- seq_len(n - block + 1L)
  k <- ceiling(n / block)
  boot <- withr::with_seed(seed, vapply(seq_len(reps), \(i) {
    idx <- as.vector(outer(0:(block - 1L), sample(starts, k, replace = TRUE), `+`))[seq_len(n)]
    effect_of(x[idx], y[idx])
  }, numeric(2)))
  probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
  q <- function(v) if (all(is.na(v))) c(NA_real_, NA_real_) else
    stats::quantile(v, probs, na.rm = TRUE, names = FALSE)
  qr <- q(boot[1, ]); qe <- q(boot[2, ])
  mutate(out, rho_low = qr[[1]], rho_high = qr[[2]], effect_low = qe[[1]], effect_high = qe[[2]],
         block = block)
}

analytics_block_len <- function(smooth) if (smooth == "weekly") 3L else 14L

# All series against all series at one lag: Spearman's rho and the number of
# paired days. Cells under min_n pairs are NA.
analytics_matrix <- function(daily, series, lag = 0L, smooth = "7d", from = NULL, min_n = NULL,
                             detrend = "none", window = 56L) {
  min_n <- min_n %||% if (smooth == "weekly") 10L else 30L
  step <- if (smooth == "weekly") "week" else "day"
  rows <- if (smooth == "weekly") as.integer(round(lag / 7)) else as.integer(lag)
  wide <- lapply(seq_len(nrow(series)), \(i) {
    analytics_values(daily, series[i, ], smooth, detrend, window) %>%
      transmute(date, value, series = series$series[i])
  }) %>%
    purrr::list_rbind() %>%
    tidyr::pivot_wider(names_from = series, values_from = value)
  keys <- intersect(series$series, names(wide))
  wide <- tibble(date = seq(min(wide$date), max(wide$date), by = step)) %>% left_join(wide, by = "date")
  X <- as.matrix(wide[keys])
  # Y one step per lag later: row i of Y holds day i + rows
  rows <- min(rows, nrow(X))
  Y <- if (rows == 0) X else rbind(X[-seq_len(rows), , drop = FALSE], matrix(NA_real_, rows, ncol(X)))
  keep <- if (is.null(from)) rep(TRUE, nrow(X)) else wide$date >= from
  X <- X[keep, , drop = FALSE]; Y <- Y[keep, , drop = FALSE]
  n <- crossprod(!is.na(X), !is.na(Y))
  rho <- suppressWarnings(stats::cor(X, Y, use = "pairwise.complete.obs", method = "spearman"))
  rho[n < min_n] <- NA
  tidyr::expand_grid(x = keys, y = keys) %>%
    mutate(rho = rho[cbind(x, y)], n = n[cbind(x, y)],
           same = unname(analytics_same_family(series, x, y)),
           rho = if_else(same, NA_real_, rho)) %>%
    select(-same)
}

write_analytics_tables <- function(daily, e1rm_groups = character(), db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "analytics_daily", daily, overwrite = TRUE)
  DBI::dbWriteTable(con, "analytics_variables", analytics_variables(daily, e1rm_groups), overwrite = TRUE)
  message("✓ analytics_daily written (", nrow(daily), " rows, ",
          n_distinct(daily$variable), " variables)")
  invisible(TRUE)
}
