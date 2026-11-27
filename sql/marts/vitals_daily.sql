-- vitals_daily: one row per date × metric × device era, for the Vitals view.
--
--   date, metric, era, value, derived, mean_7d, band_low, band_high, band_source
--
-- - era is set by source (fitbit, garmin), never by date. Eras stay separate
--   series; nothing is spliced.
-- - derived = true when the value is computed by us rather than read as is
--   (Fitbit skin temperature as a deviation from Fitbit's own baseline).
-- - mean_7d: mean of the last 7 calendar days within the same metric and era
--   (so it never rolls across the device switch); NULL with fewer than 4
--   values.
-- - HRV band: Garmin's own baseline in the Garmin era (band_source "garmin").
--   Fitbit has none, so the Fitbit era gets a derived band (band_source
--   "derived"): mean ± 1 SD of ln(RMSSD) over the previous 60 days, excluding
--   the day itself, back-transformed; NULL with fewer than 30 values.
-- - Stops when a date × metric × era appears twice, or when the two eras
--   overlap for a metric. Plausible ranges: sql/checks/data_checks.sql.
--
-- Also writes vitals_late_wakes: nights whose sleep ended after 12:00 local.
-- They stay on their wake-up day (day convention); the list is for review.
-- Reads the staging tables (sql/staging/fitbit_*.sql, garmin_api_daily.sql).

CREATE OR REPLACE TEMP TABLE vitals_base AS
WITH long AS (
  SELECT date, 'rhr' AS metric, 'fitbit' AS era, resting_heart_rate AS value,
         false AS derived, CAST(NULL AS DOUBLE) AS band_low, CAST(NULL AS DOUBLE) AS band_high
  FROM fitbit_rhr_daily
  UNION ALL SELECT date, 'hrv',  'fitbit', hrv_rmssd,        false, NULL, NULL FROM fitbit_hrv_daily
  UNION ALL SELECT date, 'resp', 'fitbit', respiratory_rate, false, NULL, NULL FROM fitbit_rr_daily
  UNION ALL SELECT date, 'spo2', 'fitbit', spo2_avg,         false, NULL, NULL FROM fitbit_spo2_daily
  UNION ALL SELECT date, 'skin_temp_dev', 'fitbit', temperature_celsius - baseline_temperature_celsius,
                   true, NULL, NULL FROM fitbit_temperature_nightly
  UNION ALL SELECT date, metric, 'fitbit', value, false, NULL, NULL
            FROM (UNPIVOT (SELECT date, sleep_s, sleep_deep_s, sleep_light_s, sleep_rem_s, sleep_awake_s
                           FROM fitbit_sleep_daily)
                  ON COLUMNS(* EXCLUDE (date)) INTO NAME metric VALUE value)
  UNION ALL SELECT date, 'rhr',  'garmin', ga_rhr,                   false, NULL, NULL FROM garmin_api_daily
  UNION ALL SELECT date, 'resp', 'garmin', ga_sleep_resp_avg,        false, NULL, NULL FROM garmin_api_daily
  UNION ALL SELECT date, 'spo2', 'garmin', ga_sleep_spo2_avg,        false, NULL, NULL FROM garmin_api_daily
  UNION ALL SELECT date, 'skin_temp_dev', 'garmin', ga_sleep_skin_temp_dev_c, false, NULL, NULL FROM garmin_api_daily
  UNION ALL SELECT date, replace(metric, 'ga_', ''), 'garmin', value, false, NULL, NULL
            FROM (UNPIVOT (SELECT date, ga_sleep_s, ga_sleep_deep_s, ga_sleep_light_s, ga_sleep_rem_s, ga_sleep_awake_s
                           FROM garmin_api_daily)
                  ON COLUMNS(* EXCLUDE (date)) INTO NAME metric VALUE value)
  UNION ALL SELECT date, 'hrv', 'garmin', ga_hrv_last_night_avg, false,
                   ga_hrv_baseline_low, ga_hrv_baseline_high FROM garmin_api_daily
)
SELECT * FROM long
WHERE value IS NOT NULL AND isfinite(value);

SELECT error('vitals_daily: duplicate date x metric x era, e.g. ' || string_agg(d, '; '))
FROM (
  SELECT date || ' ' || metric || ' ' || era AS d
  FROM vitals_base GROUP BY date, metric, era HAVING count(*) > 1
  ORDER BY d LIMIT 3
)
HAVING count(*) > 0;

SELECT error('vitals_daily: Fitbit and Garmin eras overlap for ' || string_agg(metric, ', ' ORDER BY metric)
             || '. Decide which device is the system of record for those days.')
FROM (
  SELECT metric
  FROM vitals_base
  GROUP BY metric
  HAVING max(date) FILTER (era = 'fitbit') >= min(date) FILTER (era = 'garmin')
)
HAVING count(*) > 0;

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
  SELECT *, metric = 'hrv' AND era = 'fitbit' AND n60 >= 30 AS own_band
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
ORDER BY metric, era, date;

CREATE OR REPLACE TABLE vitals_late_wakes AS
SELECT date, era, sleep_end_local
FROM (
  SELECT date, 'fitbit' AS era, sleep_end_local, 1 AS o FROM fitbit_sleep_daily
  UNION ALL
  SELECT date, 'garmin', ga_sleep_end_local, 2 FROM garmin_api_daily
)
WHERE TRY_CAST(substr(sleep_end_local, 12, 2) AS INTEGER) >= 12
ORDER BY date, o;
