library(dplyr)
library(purrr)
library(duckdb)

# vitals_daily: one row per date x metric x device era, for the Vitals view.
#
#   date, metric, era, value, derived, mean_7d, band_low, band_high, band_source
#
# - era is set by source (fitbit, garmin), never by date. Eras stay separate
#   series; nothing is spliced.
# - derived = TRUE when the value is computed by us rather than read as is
#   (Fitbit skin temperature as a deviation from Fitbit's own baseline).
# - mean_7d: mean of the last 7 calendar days within the same metric and era
#   (so it never rolls across the device switch); NA with fewer than 4 values.
# - HRV band: Garmin's own baseline in the Garmin era (band_source "garmin").
#   Fitbit has none, so the Fitbit era gets a derived band (band_source
#   "derived"): mean +/- 1 SD of ln(RMSSD) over the previous 60 days, excluding
#   the day itself, back-transformed; NA with fewer than 30 values.
#
# Also writes vitals_late_wakes: nights whose sleep ended after 12:00 local.
# They stay on their wake-up day (day convention); the list is for review.

vitals_metrics <- tribble(
  ~metric,          ~unit,  ~low,  ~high,
  "rhr",            "bpm",    30,    120,
  "hrv",            "ms",      5,    250,
  "resp",           "br/min",  6,     30,
  "spo2",           "%",      80,    100,
  "skin_temp_dev",  "°C",     -5,      5,
  "sleep_s",        "s",       0,  57600,
  "sleep_deep_s",   "s",       0,  57600,
  "sleep_light_s",  "s",       0,  57600,
  "sleep_rem_s",    "s",       0,  57600,
  "sleep_awake_s",  "s",       0,  57600
)

build_vitals_daily <- function(
    fitbit_rhr, fitbit_hrv, fitbit_rr, fitbit_spo2, fitbit_temp, fitbit_sleep,
    garmin,
    db_path = "db/health.db"
) {
  base <- bind_rows(
    vitals_long(fitbit_rhr,   "fitbit", c(rhr = "resting_heart_rate")),
    vitals_long(fitbit_hrv,   "fitbit", c(hrv = "hrv_rmssd")),
    vitals_long(fitbit_rr,    "fitbit", c(resp = "respiratory_rate")),
    vitals_long(fitbit_spo2,  "fitbit", c(spo2 = "spo2_avg")),
    vitals_long(
      mutate(fitbit_temp, dev = temperature_celsius - baseline_temperature_celsius),
      "fitbit", c(skin_temp_dev = "dev"), derived = TRUE
    ),
    vitals_long(fitbit_sleep, "fitbit", sleep_cols("")),
    vitals_long(garmin, "garmin", c(
      rhr           = "ga_rhr",
      resp          = "ga_sleep_resp_avg",
      spo2          = "ga_sleep_spo2_avg",
      skin_temp_dev = "ga_sleep_skin_temp_dev_c",
      sleep_cols("ga_")
    )),
    vitals_long(garmin, "garmin", c(hrv = "ga_hrv_last_night_avg"),
                band = c("ga_hrv_baseline_low", "ga_hrv_baseline_high"))
  )

  check_vitals_daily(base)

  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbWriteTable(con, "vitals_base", base, temporary = TRUE, overwrite = TRUE)
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE vitals_daily AS
    WITH w AS (
      SELECT *,
        avg(value)   OVER d7 AS r7,
        count(value) OVER d7 AS n7,
        avg(ln(CASE WHEN metric = 'hrv' AND value > 0 THEN value END))         OVER d60 AS lm,
        stddev_samp(ln(CASE WHEN metric = 'hrv' AND value > 0 THEN value END)) OVER d60 AS ls,
        count(CASE WHEN metric = 'hrv' AND value > 0 THEN value END)           OVER d60 AS n60
      FROM vitals_base
      WINDOW
        d7  AS (PARTITION BY metric, era ORDER BY date
                RANGE BETWEEN INTERVAL 6 DAYS PRECEDING AND CURRENT ROW),
        d60 AS (PARTITION BY metric, era ORDER BY date
                RANGE BETWEEN INTERVAL 60 DAYS PRECEDING AND INTERVAL 1 DAY PRECEDING)
    ),
    b AS (
      SELECT *,
        metric = 'hrv' AND era = 'fitbit' AND n60 >= 30 AS own_band
      FROM w
    )
    SELECT
      date, metric, era, value, derived,
      CASE WHEN n7 >= 4 THEN r7 END AS mean_7d,
      CASE WHEN band_low IS NOT NULL THEN band_low
           WHEN own_band THEN exp(lm - ls) END AS band_low,
      CASE WHEN band_high IS NOT NULL THEN band_high
           WHEN own_band THEN exp(lm + ls) END AS band_high,
      CASE WHEN band_low IS NOT NULL THEN 'garmin'
           WHEN own_band THEN 'derived' END AS band_source
    FROM b
    ORDER BY metric, era, date
  ")

  late <- bind_rows(
    tibble(date = fitbit_sleep$date, era = "fitbit",
           sleep_end_local = fitbit_sleep$sleep_end_local),
    tibble(date = garmin$date, era = "garmin",
           sleep_end_local = garmin$ga_sleep_end_local)
  ) %>%
    filter(as.integer(substr(sleep_end_local, 12, 13)) >= 12) %>%
    arrange(date)
  DBI::dbWriteTable(con, "vitals_late_wakes", late, overwrite = TRUE)

  out <- DBI::dbReadTable(con, "vitals_daily") %>% tibble::as_tibble()
  message("✓ vitals_daily written (", nrow(out), " rows); ",
          nrow(late), " late wake-ups in vitals_late_wakes")
  invisible(out)
}

# Sleep columns share names across sources, apart from Garmin's ga_ prefix.
sleep_cols <- function(prefix) {
  m <- c("sleep_s", "sleep_deep_s", "sleep_light_s", "sleep_rem_s", "sleep_awake_s")
  set_names(paste0(prefix, m), m)
}

# Wide source table -> long rows for the named columns (c(metric = "column")).
# Rows without a value are dropped. `band` names two columns for band_low and
# band_high.
vitals_long <- function(df, era, cols, derived = FALSE, band = NULL) {
  imap(cols, \(col, metric) {
    tibble(
      date      = as.Date(df$date),
      metric    = metric,
      era       = era,
      value     = as.numeric(df[[col]]),
      derived   = derived,
      band_low  = if (is.null(band)) NA_real_ else as.numeric(df[[band[1]]]),
      band_high = if (is.null(band)) NA_real_ else as.numeric(df[[band[2]]])
    )
  }) %>%
    list_rbind() %>%
    filter(!is.na(value), is.finite(value))
}

# Stops on structural problems, warns on implausible values (listed, never
# dropped silently).
check_vitals_daily <- function(base) {
  dups <- base %>% count(date, metric, era) %>% filter(n > 1)
  if (nrow(dups) > 0) {
    stop("vitals_daily: duplicate date x metric x era, e.g. ",
         paste(head(paste(dups$date, dups$metric, dups$era), 3), collapse = "; "))
  }

  overlap <- base %>%
    summarise(.by = c(metric, era), first = min(date), last = max(date)) %>%
    tidyr::pivot_wider(names_from = era, values_from = c(first, last)) %>%
    filter(!is.na(last_fitbit), !is.na(first_garmin), last_fitbit >= first_garmin)
  if (nrow(overlap) > 0) {
    stop("vitals_daily: Fitbit and Garmin eras overlap for ",
         paste(overlap$metric, collapse = ", "),
         ". Decide which device is the system of record for those days.")
  }

  unknown <- setdiff(unique(base$metric), vitals_metrics$metric)
  if (length(unknown) > 0) stop("vitals_daily: no range defined for ", paste(unknown, collapse = ", "))

  odd <- base %>%
    inner_join(vitals_metrics, by = "metric") %>%
    filter(value < low | value > high)
  if (nrow(odd) > 0) {
    warning("vitals_daily: ", nrow(odd), " implausible values kept: ",
            paste(head(paste(odd$date, odd$era, odd$metric, odd$value), 5), collapse = "; "))
  }

  invisible(TRUE)
}
