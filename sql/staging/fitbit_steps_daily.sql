-- Fitbit steps per day, from the per-minute steps_<date>.csv files in the
-- Takeout export (Physical Activity_GoogleData).
--
-- Day: the date part of the timestamp as written; like Fitbit's daily files,
-- the trailing "Z" is not treated as UTC, so nothing shifts across midnight.
-- The glob takes only steps_<date>.csv (not the readme).
-- Replaces R/load_steps.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_steps_daily AS
SELECT
  CAST(substr("timestamp", 1, 10) AS DATE) AS date,
  coalesce(sum(steps), 0)                 AS steps
FROM read_csv(
  {{fitbit_activity_dir}} || '/steps_[0-9]*.csv',
  header  = true,
  columns = {'timestamp': 'VARCHAR', 'steps': 'DOUBLE', 'data source': 'VARCHAR'}
)
GROUP BY ALL
ORDER BY date;
