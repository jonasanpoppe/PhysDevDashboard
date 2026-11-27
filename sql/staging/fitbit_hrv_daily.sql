-- Fitbit nightly HRV (RMSSD during sleep), one row per night, dated by the
-- wake-up day (see docs/sources.md): the date of Fitbit's daily file as
-- written (its "Z" is not UTC).
--
-- Reads Fitbit's daily file, not the 5-minute samples: averaging samples by
-- their UTC date mixed the end of one night with the start of the next.
-- Fitbit writes 0 when a night was recorded but HRV could not be computed
-- (the same rows have a normal non-REM heart rate), so 0 means missing.
-- Replaces R/load_hrv.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_hrv_daily AS
SELECT
  CAST(substr("timestamp", 1, 10) AS DATE)                 AS date,
  nullif("average heart rate variability milliseconds", 0) AS hrv_rmssd
FROM read_csv(
  {{fitbit_activity_dir}} || '/daily_heart_rate_variability.csv',
  header = true,
  types  = {'timestamp': 'VARCHAR', 'average heart rate variability milliseconds': 'DOUBLE'}
)
ORDER BY date;
