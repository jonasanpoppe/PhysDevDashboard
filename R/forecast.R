library(dplyr)

# Strength forecast: plain functions, no Shiny. Where a lift's current round
# is heading, when it reaches a target, how that compares with the first
# round, and whether a threshold has been lifted ("on the bar").
#
# Model: a plateau curve per lift,
#   e1RM(t) = c - (c - y0) * exp(-t / tau)
# with the ceiling c = aptitude x elite line (fat-free mass per cm of height)
# fixed, and y0 and tau fitted to the current round. t is months spent
# training the lift (gaps between sessions capped), not calendar time.
# Every number here is a model output: show it with its band (aptitude
# p10-p90), never as a measurement.

MONTH_DAYS <- 365.25 / 12

# ── The curve ──────────────────────────────────────────────────────────────────

plateau_curve <- function(t, c, y0, tau) c - (c - y0) * exp(-t / tau)

# Months of training (on the curve's own clock) at which it reaches `target`;
# Inf when the target is at or above the ceiling.
months_to <- function(target, c, y0, tau) {
  n <- max(length(target), length(c), length(y0), length(tau))
  target <- rep_len(target, n); c <- rep_len(c, n); y0 <- rep_len(y0, n); tau <- rep_len(tau, n)
  out <- rep(Inf, n)
  ok <- target < c
  out[ok] <- tau[ok] * log((c[ok] - y0[ok]) / (c[ok] - target[ok]))
  out
}

# Fits y0 and tau with the ceiling fixed. Returns c(y0, tau), or NULL when
# the fit doesn't converge.
fit_plateau <- function(t, y, c) {
  fit <- tryCatch(
    stats::nls(y ~ c - (c - y0) * exp(-t / tau), data = list(t = t, y = y, c = c),
               start = list(y0 = min(y), tau = 10), algorithm = "port",
               lower = c(0, 0.1), upper = c(c - 0.01, 5000)),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  stats::coef(fit)[c("y0", "tau")]
}

# ── Ceilings ───────────────────────────────────────────────────────────────────

# The elite line (Nuckols, Stronger By Science; squat and bench slopes reduced
# by 10% for supportive gear), in kg, from fat-free mass and height.
elite_line <- function(ffm_kg, height_cm) {
  x <- ffm_kg / height_cm
  c(squat = 611.19 * x - 10.43, bench = 427.14 * x - 14.75, deadlift = 410.2 * x + 102.5)
}

# The ceiling for each aptitude (fraction of the elite line).
ceiling_kg <- function(aptitude, elite_kg) aptitude * elite_kg

# ── Time ───────────────────────────────────────────────────────────────────────

# Cumulative months of training at each session: the time since the previous
# session (or since `start` for the first), each gap counted at most
# gap_cap_days. gap_cap_days = Inf gives calendar time.
training_months <- function(dates, start, gap_cap_days = 14) {
  dates <- as.Date(dates)
  stopifnot(!is.unsorted(dates))
  gaps <- as.numeric(diff(c(as.Date(start), dates)))
  cumsum(pmin(gaps, gap_cap_days)) / MONTH_DAYS
}

# ── Sessions ───────────────────────────────────────────────────────────────────

# The current round of one lift: session bests (from strength_sessions) of the
# exact exercise from round_start, with their training months.
lift_round <- function(sessions, exercise, round_start, gap_cap_days = 14) {
  d <- sessions %>%
    filter(lift == exercise, as.Date(date) >= as.Date(round_start)) %>%
    transmute(date = as.Date(date), e1rm) %>%
    arrange(date)
  mutate(d, t = training_months(date, round_start, gap_cap_days))
}

# ── Forecast ───────────────────────────────────────────────────────────────────

# Plateau parameters for each aptitude: fitted when the round has at least
# min_sessions sessions, otherwise the fallback (a start value and a fixed
# tau, with t counted from the start date).
#
# round: output of lift_round(). Returns one row per aptitude: aptitude, c,
# y0, tau, t0 (the round's t on the start of the curve's clock), fitted.
plateau_params <- function(round, aptitude, elite_kg, min_sessions = 8, tau_start = NULL,
                           start = NULL) {
  purrr::map(aptitude, \(a) {
    c <- ceiling_kg(a, elite_kg)
    p <- if (nrow(round) >= min_sessions) fit_plateau(round$t, round$e1rm, c)
    if (!is.null(p)) {
      tibble(aptitude = a, c = c, y0 = unname(p["y0"]), tau = unname(p["tau"]), fitted = TRUE)
    } else {
      y0 <- start$e1rm_kg %||% utils::tail(round$e1rm, 1)
      if (is.null(tau_start) || length(y0) == 0) stop("plateau_params: too few sessions and no start value")
      tibble(aptitude = a, c = c, y0 = y0, tau = tau_start, fitted = FALSE)
    }
  }) %>% purrr::list_rbind()
}

# Projection from `from` (a date, where the clock reads t_now) to the
# horizon, for each aptitude. Calendar date = from + elapsed months, which
# assumes training goes on without further gaps.
project_lift <- function(params, t_now, from, horizon, step_days = 7) {
  days <- seq(0, as.numeric(as.Date(horizon) - as.Date(from)), by = step_days)
  purrr::pmap(params, \(aptitude, c, y0, tau, fitted) {
    tibble(aptitude = aptitude, date = as.Date(from) + days,
           e1rm = plateau_curve(t_now + days / MONTH_DAYS, c, y0, tau))
  }) %>% purrr::list_rbind()
}

# Months from now (t_now) to `target`, and the date, per aptitude. NA date
# when the target is out of reach (at or above the ceiling).
time_to_target <- function(params, target, t_now, from) {
  params %>%
    mutate(target = target,
           months = pmax(months_to(target, c, y0, tau) - t_now, 0),
           date = if_else(is.finite(months), as.Date(from) + round(months * MONTH_DAYS), as.Date(NA)))
}

# Straight-line reference (no ceiling): months from the last session to the
# target at the round's linear rate. For the table only.
straight_line_months <- function(round, target) {
  fit <- stats::lm(e1rm ~ t, data = round)
  b <- stats::coef(fit)
  t_last <- utils::tail(round$t, 1)
  list(slope_kg_month = unname(b["t"]),
       months = unname((target - (b[1] + b["t"] * t_last)) / b["t"]))
}

# ── The first round (2020-22), from cached parameters ─────────────────────────

# Exponential and logarithmic curves on calendar months since `origin`.
#   exponential: c - (c - y0) * exp(-t / tau)
#   logarithmic: a + b * ln(1 + t / s)
first_round_curve <- function(par, dates, origin = "2020-03-01") {
  t <- as.numeric(as.Date(dates) - as.Date(origin)) / MONTH_DAYS
  tibble(date = as.Date(dates),
         exponential = plateau_curve(t, par$exp$c, par$exp$y0, par$exp$tau),
         logarithmic = par$log$a + par$log$b * log(1 + t / par$log$s))
}

# ── On the bar ─────────────────────────────────────────────────────────────────

# The heaviest load completed for at least one rep (a working set, base
# weight included) since `since`, with its date and reps. Zero rows if none.
on_the_bar <- function(sets, exercise, since = as.Date("1900-01-01")) {
  sets %>%
    filter(.data$exercise == .env$exercise, as.Date(date) >= as.Date(since),
           set_type %in% c("Standard Set", "Failure Set"), !coalesce(as.logical(is_warmup), FALSE),
           !is.na(reps), reps >= 1) %>%
    mutate(load_kg = weight_kg + coalesce(exercise_base_weight_kg, 0)) %>%
    slice_max(load_kg, n = 1, with_ties = FALSE) %>%
    transmute(date = as.Date(date), load_kg, reps)
}

# The e1RM at which an attempt on the threshold makes sense: singles come out
# below the 2-6-rep e1RM by single_ratio.
attempt_level <- function(threshold_bw, bodyweight_kg, single_ratio) {
  threshold_bw * bodyweight_kg / single_ratio
}

# Each session's best single (one rep, working set with an e1RM) against the
# best e1RM from 2-6-rep sets of the same exercise within window_days either
# side, the same day included. ratio = the single's e1RM / that e1RM: how far
# a single comes out below what the 2-6-rep sets estimate.
single_ratio_pairs <- function(sets, exercise, window_days = 14) {
  ex <- sets %>%
    filter(.data$exercise == .env$exercise, set_type %in% c("Standard Set", "Failure Set"),
           !is.na(e1rm_kg)) %>%
    mutate(date = as.Date(date))
  singles <- ex %>% filter(reps == 1) %>% summarise(single_e1rm_kg = max(e1rm_kg), .by = date)
  multi <- ex %>% filter(reps >= 2, reps <= 6) %>%
    summarise(e1rm_kg = max(e1rm_kg), .by = date) %>% rename(ref_date = date)
  singles %>%
    mutate(lo = date - window_days, hi = date + window_days) %>%
    inner_join(multi, by = join_by(between(y$ref_date, x$lo, x$hi))) %>%
    summarise(ref_e1rm_kg = max(e1rm_kg), .by = c(date, single_e1rm_kg)) %>%
    mutate(ratio = single_e1rm_kg / ref_e1rm_kg) %>%
    arrange(date)
}

# ── One lift, from its settings (shared by the pipeline and the app) ─────────

# One row of strength_forecast_settings and the session bests give the
# round, the plateau parameters for each aptitude, where the curve's clock
# stands now (`from`, `t_now`) and the projection to the horizon. Fewer than
# min_sessions_fit sessions: the start override with tau_start, the clock at
# 0 on the start date. bands: names for the aptitudes (e.g. p10, p50, p90).
forecast_lift <- function(sessions, st, aptitude = c(st$p10, st$p50, st$p90),
                          bands = c("p10", "p50", "p90"), gap_cap_days = st$gap_cap_days) {
  round <- lift_round(sessions, st$exercise, st$round_start, gap_cap_days)
  use_start <- nrow(round) < st$min_sessions_fit
  start <- if (use_start && !is.na(st$start_e1rm_kg)) list(e1rm_kg = st$start_e1rm_kg)
  params <- plateau_params(round, aptitude, st$elite_kg, min_sessions = st$min_sessions_fit,
                           tau_start = st$tau_start, start = start) %>%
    mutate(lift = st$lift, exercise = st$exercise, band = bands, elite_kg = st$elite_kg, .before = 1)
  if (use_start) {
    from <- if (!is.na(st$start_date)) st$start_date else max(round$date)
    t_now <- 0
  } else {
    from <- max(round$date)
    t_now <- utils::tail(round$t, 1)
  }
  curve <- project_lift(params %>% select(aptitude, c, y0, tau, fitted), t_now, from, st$horizon) %>%
    left_join(params %>% select(aptitude, band), by = "aptitude") %>%
    mutate(lift = st$lift, .before = 1)
  list(round = round, params = params, from = from, t_now = t_now, fitted = !use_start, curve = curve)
}

# Months and dates to `target` per band, plus the straight line (fits only).
forecast_targets <- function(fl, target) {
  tt <- time_to_target(fl$params, target, fl$t_now, fl$from) %>% select(lift, band, target, months, date)
  line <- if (fl$fitted && nrow(fl$round) >= 3) straight_line_months(fl$round, target)
  bind_rows(tt, tibble(lift = fl$params$lift[1], band = "straight line", target = target,
                       months = line$months %||% NA_real_,
                       date = if (is.null(line)) as.Date(NA) else fl$from + round(line$months * MONTH_DAYS)))
}

# ── Everything for the app, from config ────────────────────────────────────────

# The forecast per lift in strength_forecast (goals.yml), at the aptitude
# band in strength_ceilings. Returns tables:
#   settings    one row per lift: what the app needs to recompute live
#   rounds      current-round sessions with training months t
#   params      plateau parameters per lift and aptitude (p10, p50, p90)
#   curve       projections from the last session (or start) to the horizon
#   targets     months and dates to each target, plus the straight line
#   thresholds  on the bar (round and ever) against threshold x bodyweight,
#               the attempt level and the date the p50 curve reaches it
#   first_round the first round's exponential and logarithmic curves
# `sets`: lifting_sets.
build_strength_forecast <- function(sessions, sets, fc, ceil) {
  tb <- ceil$target_body
  elite <- elite_line(tb$bodyweight_kg * (1 - tb$body_fat), tb$height_cm)

  per_lift <- purrr::imap(fc$exercises, \(exercise, key) {
    start <- fc$start_override[[key]]
    apt <- unlist(ceil$aptitude[[key]])
    st <- tibble(
      lift = key, exercise = exercise, round_start = as.Date(fc$round_start),
      gap_cap_days = fc$gap_cap_days %||% 14, min_sessions_fit = fc$min_sessions_fit %||% 8,
      horizon = as.Date(fc$horizon), tau_start = fc$first_round[[key]]$exp$tau %||% NA_real_,
      start_date = as.Date(start$date %||% NA), start_e1rm_kg = start$e1rm_kg %||% NA_real_,
      start_note = start$note %||% NA_character_,
      elite_kg = elite[[key]], p10 = apt[1], p50 = apt[2], p90 = apt[3],
      target_kg = unlist(fc$targets_kg[[key]])[1] %||% NA_real_,
      threshold_bw = fc$thresholds_bw[[key]], threshold_bodyweight_kg = fc$threshold_bodyweight_kg,
      single_ratio = fc$single_ratio[[key]],
      first_round_origin = as.Date(fc$first_round$origin %||% NA),
      first_round_fitted_until = as.Date(fc$first_round$fitted_until %||% NA)
    )
    fl <- forecast_lift(sessions, st)

    targets <- purrr::map(unlist(fc$targets_kg[[key]]), \(target) forecast_targets(fl, target)) %>%
      purrr::list_rbind()

    level <- attempt_level(st$threshold_bw, st$threshold_bodyweight_kg, st$single_ratio)
    bar_round <- on_the_bar(sets, exercise, st$round_start)
    bar_ever <- on_the_bar(sets, exercise)
    thresholds <- tibble(
      lift = key, exercise = exercise, threshold_bw = st$threshold_bw,
      bodyweight_kg = st$threshold_bodyweight_kg, threshold_kg = st$threshold_bw * st$threshold_bodyweight_kg,
      round_best_kg = bar_round$load_kg[1] %||% NA_real_, round_best_reps = bar_round$reps[1] %||% NA_real_,
      round_best_date = bar_round$date[1] %||% as.Date(NA),
      ever_best_kg = bar_ever$load_kg[1] %||% NA_real_, ever_best_reps = bar_ever$reps[1] %||% NA_real_,
      ever_best_date = bar_ever$date[1] %||% as.Date(NA),
      single_ratio = st$single_ratio, attempt_e1rm_kg = level,
      attempt_from = time_to_target(filter(fl$params, band == "p50"), level, fl$t_now, fl$from)$date
    ) %>%
      mutate(on_the_bar = coalesce(round_best_kg >= threshold_kg, FALSE))

    fr <- fc$first_round[[key]]
    first <- if (!is.null(fr)) {
      dates <- seq(st$first_round_origin, st$horizon, by = "week")
      first_round_curve(fr, dates, st$first_round_origin) %>%
        mutate(lift = key, fitted_part = date <= st$first_round_fitted_until, .before = 1)
    }

    list(settings = st, rounds = mutate(fl$round, lift = key, exercise = exercise, .before = 1),
         params = fl$params, curve = fl$curve, targets = targets, thresholds = thresholds, first_round = first)
  })
  purrr::map(purrr::set_names(names(per_lift[[1]])), \(n) purrr::list_rbind(purrr::map(per_lift, n)))
}

write_strength_forecast <- function(fc_tables, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  for (n in names(fc_tables)) {
    DBI::dbWriteTable(con, paste0("strength_forecast_", n), fc_tables[[n]], overwrite = TRUE)
  }
  message("✓ strength_forecast_* written (", nrow(fc_tables$params), " parameter sets)")
  invisible(fc_tables)
}
