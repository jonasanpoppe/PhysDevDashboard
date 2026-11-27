-- Fitbit resting heart rate, one row per day, from the daily file in the
-- Takeout export (Physical Activity_GoogleData). Day: the date as written.
-- Replaces R/load_rhr.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_rhr_daily AS
SELECT
  CAST(substr("timestamp", 1, 10) AS DATE) AS date,
  "beats per minute"                       AS resting_heart_rate
FROM read_csv(
  {{fitbit_activity_dir}} || '/daily_resting_heart_rate.csv',
  header  = true,
  columns = {'timestamp': 'VARCHAR', 'beats per minute': 'DOUBLE', 'data source': 'VARCHAR'}
)
ORDER BY date;
