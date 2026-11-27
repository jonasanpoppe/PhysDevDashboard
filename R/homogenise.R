library(dplyr)

# Homogenised vitals: the Fitbit era drawn onto the Garmin era's scale, as a
# derived series kept beside the two originals (ROADMAP: no silent splicing).
#
# The two devices never overlap in time, so there is no night measured by both
# to calibrate on. The step is estimated at the switch instead, the way climate
# records are homogenised at a station move: within `window_days` either side
# of the switch, fit
#
#   y = a + step * garmin + slope * t        (t = years from the switch)
#
# and take `step` as the device offset. The common slope keeps a trend that
# runs through the switch (getting fitter) from being counted as device; a
# symmetric window around the switch roughly cancels the season. What remains
# is device plus whatever changed physiologically at that moment, which the
# data cannot separate: the step is an estimate, with a moving-block bootstrap
# interval (blocks resampled within each era).
#
# scale "additive": Fitbit + step (bpm, br/min). scale "log": the step is
# fitted on ln(value) and Fitbit is multiplied by exp(step) (HRV: devices
# differ by a ratio, and its spread grows with its level; steps: devices
# count a different share of the same steps).
#
# Steps come from device_steps (sql/marts/device_steps.sql), not
# vitals_daily; era_series() stacks the two for the estimate. Note the two
# senses of "step": the step at the switch (this file) and steps walked.
#
# Only the location is moved. The spread (SD) is reported, not matched:
# where the eras differ in day-to-day noise it comes from how each device
# smooths or measures, and scaling Fitbit's spread would scale its real
# variation along with it.
#
# Evaluated: every metric in homogenise_spec. Applied (a series is made) only
# where `apply` is TRUE and the eras meet within `max_gap_days`.

homogenise_spec <- tibble::tribble(
  ~metric,          ~scale,      ~apply, ~why_not,
  "rhr",            "additive",  TRUE,   NA,
  "steps",          "log",       TRUE,   NA,
  "hrv",            "log",       TRUE,   NA,
  "resp",           "additive",  TRUE,   NA,
  "spo2",           "additive",  TRUE,   NA,
  "skin_temp_dev",  "additive",  FALSE,  "both are deviations from each device's own baseline: no level to move",
  "sleep_s",        "additive",  FALSE,  "sleep detection and staging differ by device: evaluated only",
  "sleep_deep_s",   "additive",  FALSE,  "sleep detection and staging differ by device: evaluated only",
  "sleep_rem_s",    "additive",  FALSE,  "sleep detection and staging differ by device: evaluated only"
)

# The step for one metric. d: date, era ("fitbit"/"garmin"), value.
# Returns one row: the switch, the step with its interval, and the spread of
# each era (SD of the residuals around the fit; day-to-day noise as the SD of
# consecutive-day differences / sqrt 2).
estimate_era_step <- function(d, scale = c("additive", "log"), window_days = 180L,
                              block = 14L, reps = 500L, level = 0.95, seed = 1L) {
  scale <- match.arg(scale)
  d <- d %>% filter(era %in% c("fitbit", "garmin"), is.finite(value)) %>%
    mutate(date = as.Date(date))
  if (scale == "log") d <- filter(d, value > 0)
  empty <- tibble(last_fitbit = as.Date(NA), first_garmin = as.Date(NA), gap_days = NA_integer_,
                  window_days = as.integer(window_days), n_fitbit = 0L, n_garmin = 0L,
                  step = NA_real_, step_low = NA_real_, step_high = NA_real_, step_flat = NA_real_,
                  slope_per_year = NA_real_, sd_fitbit = NA_real_, sd_garmin = NA_real_,
                  noise_fitbit = NA_real_, noise_garmin = NA_real_)
  if (!all(c("fitbit", "garmin") %in% d$era)) return(empty)

  last_f <- max(d$date[d$era == "fitbit"]); first_g <- min(d$date[d$era == "garmin"])
  w <- d %>%
    filter((era == "fitbit" & date > last_f - window_days) |
           (era == "garmin" & date < first_g + window_days)) %>%
    arrange(date) %>%
    mutate(g = as.numeric(era == "garmin"),
           t = as.numeric(date - first_g) / 365.25,
           y = if (scale == "log") log(value) else value)
  out <- mutate(empty, last_fitbit = last_f, first_garmin = first_g,
                gap_days = as.integer(first_g - last_f), n_fitbit = sum(w$g == 0), n_garmin = sum(w$g == 1))
  if (min(out$n_fitbit, out$n_garmin) < 20) return(out)

  fit <- function(w) stats::lm.fit(cbind(1, w$g, w$t), w$y)
  m <- fit(w)
  res <- m$residuals
  # Day-to-day noise: consecutive calendar days only
  noise <- function(e) {
    x <- w[w$era == e, ]
    dd <- diff(x$y)[diff(x$date) == 1]
    if (length(dd) < 10) NA_real_ else stats::sd(dd) / sqrt(2)
  }
  # Moving blocks within each era, so a resample keeps both sides
  idx_f <- which(w$g == 0); idx_g <- which(w$g == 1)
  blocks <- function(ii) {
    b <- min(as.integer(block), length(ii))
    st <- sample(seq_len(length(ii) - b + 1L), ceiling(length(ii) / b), replace = TRUE)
    ii[as.vector(outer(0:(b - 1L), st, `+`))][seq_along(ii)]
  }
  boot <- withr::with_seed(seed, vapply(seq_len(reps), \(i) {
    fit(w[c(blocks(idx_f), blocks(idx_g)), ])$coefficients[[2]]
  }, numeric(1)))
  q <- stats::quantile(boot, c((1 - level) / 2, 1 - (1 - level) / 2), names = FALSE, na.rm = TRUE)

  mutate(out,
         step = m$coefficients[[2]], step_low = q[[1]], step_high = q[[2]],
         step_flat = mean(w$y[w$g == 1]) - mean(w$y[w$g == 0]),
         slope_per_year = m$coefficients[[3]],
         sd_fitbit = stats::sd(res[w$g == 0]), sd_garmin = stats::sd(res[w$g == 1]),
         noise_fitbit = noise("fitbit"), noise_garmin = noise("garmin"))
}

# The device series by era, long (date, metric, era, value): the vitals plus
# steps walked as metric "steps".
era_series <- function(vitals_daily, device_steps = NULL) {
  v <- select(vitals_daily, date, metric, era, value)
  if (is.null(device_steps)) return(v)
  bind_rows(v, transmute(device_steps, date = as.Date(date), metric = "steps", era,
                         value = as.numeric(steps)))
}

# Steps for every metric in the spec, with whether a series is made.
# vitals_daily: date, metric, era, value.
estimate_era_steps <- function(vitals_daily, spec = homogenise_spec, window_days = 180L,
                               max_gap_days = 14L, ...) {
  purrr::pmap(spec, \(metric, scale, apply, why_not) {
    estimate_era_step(vitals_daily[vitals_daily$metric == metric, ], scale = scale,
                      window_days = window_days, ...) %>%
      mutate(metric = metric, scale = scale, .before = 1) %>%
      mutate(applied = apply & !is.na(step) & gap_days <= max_gap_days,
             why_not = case_when(
               applied ~ NA_character_,
               !apply ~ why_not,
               is.na(step) ~ "too few days on one side of the switch",
               TRUE ~ sprintf("the eras are %d days apart", gap_days)))
  }) %>%
    purrr::list_rbind()
}

# The homogenised series: Fitbit moved by the step, Garmin as measured, under
# era "homogenised". Also returns, per row, the value at both ends of the
# step's interval, so a caller can show how much the choice of step matters.
homogenise_vitals <- function(vitals_daily, steps) {
  move <- function(value, era, scale, k) {
    case_when(era == "garmin" ~ value, scale == "log" ~ value * exp(k), TRUE ~ value + k)
  }
  s <- filter(steps, applied)
  vitals_daily %>%
    select(date, metric, era, value) %>%
    inner_join(select(s, metric, scale, step, step_low, step_high), by = "metric") %>%
    filter(era %in% c("fitbit", "garmin")) %>%
    mutate(
      value_low  = move(value, era, scale, step_low),
      value_high = move(value, era, scale, step_high),
      value      = move(value, era, scale, step),
      source_era = era,
      era = "homogenised"
    ) %>%
    select(date, metric, era, source_era, value, value_low, value_high) %>%
    arrange(metric, date)
}

write_homogenisation_tables <- function(steps, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "vitals_era_steps", steps, overwrite = TRUE)
  message("✓ vitals_era_steps written (", sum(steps$applied), " of ", nrow(steps),
          " metrics homogenised)")
  invisible(TRUE)
}

# One line for the app: how the Fitbit part of a homogenised series was moved.
# s: one analytics series row (variable, era, series_label, unit); steps:
# vitals_era_steps. NULL when the series is not homogenised.
homogenised_note <- function(s, steps) {
  if (s$era != "homogenised") return(NULL)
  if (startsWith(s$variable, "e1rm_")) {
    return(sprintf("%s: every variant of the lift converted onto it by its ratio (strength_forecast variants in config/goals.yml); the lift itself where done that day. A model output on a model output.",
                   s$series_label))
  }
  if (is.null(steps)) return(NULL)
  metric <- if (s$variable %in% steps$metric) s$variable else NA
  st <- steps[steps$metric %in% metric, ]
  if (nrow(st) != 1) return(NULL)
  num <- function(v) format(signif(v, 2), trim = TRUE)
  move <- if (st$scale == "log") {
    sprintf("Fitbit values × %s (95%% interval %s to %s)",
            num(exp(st$step)), num(exp(st$step_low)), num(exp(st$step_high)))
  } else {
    sprintf("Fitbit values %s%s %s (95%% interval %s to %s)", if (st$step >= 0) "+" else "−",
            num(abs(st$step)), s$unit, num(st$step_low), num(st$step_high))
  }
  sprintf("%s: %s, estimated from the %d days either side of the switch on %s. A model output, not a measurement.",
          s$series_label, move, st$window_days, format(st$first_garmin))
}

# ── e1RM onto the reference lifts ─────────────────────────────────────────────
#
# One scale per lift family: the reference lift's (strength_forecast$exercises
# in config/goals.yml: back squat, flat bench, conventional deadlift). A
# variant's e1RM is divided by its ratio to the reference
# (strength_forecast$variants: {exercise: {lift, ratio}}; ratio = variant
# e1RM / reference e1RM), so 100 kg of a variant at a ratio of 0.8 reads as
# 125 kg of the reference lift. A variant marked
# placeholder: true is left out until its ratio is settled; variants without
# a ratio are never converted.
#
# Returns date, lift (the key), reference (its exercise), era ("measured":
# the reference itself; "homogenised": the reference where done that day,
# else the best converted variant), value, source (the exercise behind it).
homogenise_e1rm <- function(strength_sessions, fc) {
  refs <- unlist(fc$exercises %||% list())
  empty <- tibble(date = as.Date(character()), lift = character(), reference = character(),
                  era = character(), value = numeric(), source = character())
  if (!length(refs)) return(empty)
  v <- fc$variants %||% list()
  v <- v[vapply(v, \(x) !isTRUE(x$placeholder) && !is.null(x$ratio) && x$lift %in% names(refs), logical(1))]
  map <- bind_rows(
    tibble(lift = names(refs), exercise = unname(refs), ratio = 1),
    tibble(lift = unname(vapply(v, \(x) x$lift, "")), exercise = names(v),
           ratio = unname(vapply(v, \(x) as.numeric(x$ratio), 0)))
  )
  s <- strength_sessions %>%
    transmute(date = as.Date(date), exercise = lift, e1rm = as.numeric(e1rm)) %>%
    filter(is.finite(e1rm)) %>%
    inner_join(map, by = "exercise") %>%
    mutate(reference = unname(refs[lift]), is_ref = exercise == reference, value = e1rm / ratio)
  if (!nrow(s)) return(empty)
  measured <- s %>% filter(is_ref) %>%
    summarise(.by = c(date, lift, reference), value = max(value)) %>%
    mutate(era = "measured", source = reference)
  homogenised <- s %>%
    arrange(date, lift, desc(is_ref), desc(value)) %>%
    distinct(date, lift, .keep_all = TRUE) %>%
    transmute(date, lift, reference, era = "homogenised", value, source = exercise)
  bind_rows(measured, homogenised) %>%
    select(date, lift, reference, era, value, source) %>%
    arrange(lift, era, date)
}
