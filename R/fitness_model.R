library(dplyr)

# Fitness: are you getting fitter? Watts at the same heart rate, per season,
# and the training load beside it. Spec, sections 4.6–4.7.
#
#   power_w ~ hr_end + log(moving_s) + hours_into_ride + season
#
# - Pooled OLS on clean ascents (clean or top) of at least min_duration_s,
#   one bike, one heart-rate era (chest strap): a wrist sensor reads
#   differently, so eras are never mixed (filter on era, not on year).
# - No climb fixed effects: they are collinear with duration.
# - Per season (calendar year): the predicted watts at a reference heart
#   rate, duration and time into the ride, with a bootstrap interval.
# - The power is itself an estimate (±15%): the interval says how well the
#   model is pinned down, not how accurate the watts are.

# hr_era_of(): R/common.R (shared with the plan bands)

fitness_efforts <- function(efforts, activities, eras, settings) {
  s <- fitness_settings(settings)
  efforts %>%
    left_join(activities %>% transmute(activity_id = as.character(activity_id), device_name),
              by = "activity_id") %>%
    mutate(hr_era = hr_era_of(device_name, eras), season = as.integer(format(as.Date(date), "%Y"))) %>%
    filter(status %in% c("clean", "top"), moving_s >= s$min_duration_s,
           hr_era == s$hr_era, bike_id %in% s$bike_ids,
           !is.na(power_w), !is.na(hr_end), !is.na(hrs_into_ride))
}

fit_efficiency <- function(fe, settings) {
  s <- fitness_settings(settings)
  counts <- count(fe, season)
  keep <- counts$season[counts$n >= s$min_efforts_per_season]
  fe <- filter(fe, season %in% keep) %>% mutate(season = factor(season))
  if (nlevels(fe$season) < 1) stop("fitness: no season with enough efforts")

  formula <- if (nlevels(fe$season) > 1) {
    power_w ~ hr_end + log(moving_s) + hrs_into_ride + season
  } else {
    power_w ~ hr_end + log(moving_s) + hrs_into_ride
  }
  ref <- tibble(season = factor(levels(fe$season), levels = levels(fe$season)),
                hr_end = s$ref_hr_bpm, moving_s = s$ref_duration_s,
                hrs_into_ride = s$ref_hours_into_ride)
  fit <- lm(formula, data = fe)

  # Bootstrap: refit on resampled efforts, predict at the reference
  set.seed(s$seed)
  boot <- replicate(s$bootstrap, {
    b <- fe[sample.int(nrow(fe), replace = TRUE), ]
    if (any(table(b$season) == 0)) return(rep(NA_real_, nrow(ref)))
    predict(lm(formula, data = b), newdata = ref)
  })
  boot <- matrix(boot, nrow = nrow(ref))
  coef_boot <- replicate(min(s$bootstrap, 1000), {
    b <- fe[sample.int(nrow(fe), replace = TRUE), ]
    if (any(table(b$season) == 0)) return(rep(NA_real_, length(coef(fit))))
    coef(lm(formula, data = b))
  })

  seasons <- ref %>%
    transmute(
      season = as.integer(as.character(season)),
      watts_ref = unname(predict(fit, newdata = ref)),
      ci_low = apply(boot, 1, stats::quantile, 0.025, na.rm = TRUE),
      ci_high = apply(boot, 1, stats::quantile, 0.975, na.rm = TRUE)
    ) %>%
    left_join(fe %>% mutate(season = as.integer(as.character(season))) %>%
                summarise(.by = season, n = n(), first_date = min(as.Date(date)), last_date = max(as.Date(date))),
              by = "season")

  coefs <- tibble(
    term = names(coef(fit)), estimate = unname(coef(fit)),
    ci_low = apply(coef_boot, 1, stats::quantile, 0.025, na.rm = TRUE),
    ci_high = apply(coef_boot, 1, stats::quantile, 0.975, na.rm = TRUE)
  )

  list(fit = fit, seasons = seasons, coefs = coefs,
       efforts = fe %>% mutate(residual_w = unname(residuals(fit)),
                               season = as.integer(as.character(season))),
       reference = s[c("ref_hr_bpm", "ref_duration_s", "ref_hours_into_ride")])
}

# Daily riding and running hours (moving time), every calendar day from the
# first to the last activity (or `until`), with the spec's rolling views
# (ride and run types default to the lists in R/strava_streams.R):
#   *_6w_hpw   6-week rolling mean, in hours per week
#   fitness_h  42-day EWMA of daily hours (long-term load)
#   fatigue_h  7-day EWMA of daily hours (short-term load)
daily_load <- function(activities, until = NULL,
                       ride_types = RIDE_TYPES, run_types = RUN_TYPES) {
  typed <- activities %>%
    transmute(date = as.Date(date), h = moving_s / 3600,
              kind = case_when(sport_type %in% ride_types ~ "ride",
                               sport_type %in% run_types ~ "run",
                               TRUE ~ "other")) %>%
    filter(kind %in% c("ride", "run"))
  last <- if (is.null(until)) max(typed$date) else max(c(typed$date, as.Date(until)))
  days <- tibble(date = seq(min(typed$date), last, by = "day"))
  per_day <- typed %>%
    summarise(.by = c(date, kind), h = sum(h)) %>%
    tidyr::pivot_wider(names_from = kind, values_from = h, names_glue = "{kind}_h", values_fill = 0)
  for (col in c("ride_h", "run_h")) if (!col %in% names(per_day)) per_day[[col]] <- 0
  days %>%
    left_join(per_day, by = "date") %>%
    mutate(across(c(ride_h, run_h), \(x) coalesce(x, 0))) %>%
    arrange(date) %>%
    mutate(
      ride_6w_hpw = roll_sum(ride_h, 42) / 6,
      run_6w_hpw  = roll_sum(run_h, 42) / 6,
      total_h     = ride_h + run_h,
      fitness_h   = ewma(total_h, 42),
      fatigue_h   = ewma(total_h, 7)
    ) %>%
    select(date, ride_h, run_h, ride_6w_hpw, run_6w_hpw, fitness_h, fatigue_h)
}

# Residual watts (after heart rate, duration, time in ride, season) against
# the 6-week riding + running hours before the effort. The doc found about
# +15 W per extra weekly hour; this is a check, not a model input.
load_check <- function(model, load) {
  d <- model$efforts %>%
    mutate(date = as.Date(date)) %>%
    left_join(load %>% transmute(date, load_6w_hpw = ride_6w_hpw + run_6w_hpw), by = "date")
  fit <- lm(residual_w ~ load_6w_hpw, data = d)
  tibble(w_per_weekly_hour = unname(coef(fit)[2]),
         ci_low = unname(confint(fit)[2, 1]), ci_high = unname(confint(fit)[2, 2]),
         n = nrow(d))
}

roll_sum <- function(x, k) {
  cs <- cumsum(x)
  cs - c(rep(0, k), utils::head(cs, -k))[seq_along(x)]
}

ewma <- function(x, days) {
  a <- 1 / days
  out <- numeric(length(x))
  acc <- 0
  for (i in seq_along(x)) { acc <- acc + a * (x[i] - acc); out[i] <- acc }
  out
}

fitness_settings <- function(cfg = list()) {
  defaults <- list(hr_era = "chest_strap", bike_ids = character(), min_duration_s = 90,
                   ref_hr_bpm = 155, ref_duration_s = 180, ref_hours_into_ride = 1.5,
                   min_efforts_per_season = 5, bootstrap = 1000, seed = 1)
  s <- utils::modifyList(defaults, cfg %||% list())
  s$bike_ids <- unlist(s$bike_ids)
  s
}

# Writes the fitness tables for the app.
write_fitness_tables <- function(model, load, check, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "strava_daily_load", load, overwrite = TRUE)
  DBI::dbWriteTable(con, "fitness_seasons", model$seasons %>%
                      mutate(ref_hr_bpm = model$reference$ref_hr_bpm,
                             ref_duration_s = model$reference$ref_duration_s,
                             ref_hours_into_ride = model$reference$ref_hours_into_ride),
                    overwrite = TRUE)
  DBI::dbWriteTable(con, "fitness_coefficients", model$coefs, overwrite = TRUE)
  DBI::dbWriteTable(con, "fitness_load_check", check, overwrite = TRUE)
  message("✓ fitness tables written (", nrow(model$seasons), " seasons)")
  invisible(TRUE)
}
