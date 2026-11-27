-- Fitbit nightly skin temperature and its baseline, one row per night, from
-- daily_sleep_temperature_derivations*.csv (Physical Activity_GoogleData).
-- Fitbit's baseline is the median nightly value over the past 30 days; the
-- deviation from it is what Garmin reports directly.
--
-- Day: the date as written. Fitbit writes NaN where it has no value (the
-- baseline in the first weeks); NaN becomes NULL here.
-- Replaces R/load_temperature.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_temperature_nightly AS
SELECT
  CAST(substr("timestamp", 1, 10) AS DATE)                            AS date,
  avg(CASE WHEN NOT isnan(t) THEN t END)                              AS temperature_celsius,
  avg(CASE WHEN NOT isnan(b) THEN b END)                              AS baseline_temperature_celsius
FROM (
  SELECT "timestamp", "nightly temperature celsius" AS t, "baseline temperature celsius" AS b
  FROM read_csv(
    {{fitbit_activity_dir}} || '/daily_sleep_temperature_derivations*.csv',
    header = true,
    types  = {'timestamp': 'VARCHAR', 'nightly temperature celsius': 'DOUBLE',
              'baseline temperature celsius': 'DOUBLE'}
  )
)
GROUP BY ALL
ORDER BY date;
