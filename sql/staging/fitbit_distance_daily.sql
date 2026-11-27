-- Fitbit distance per day, from the per-minute distance_<date>.csv files in the
-- Takeout export (Physical Activity_GoogleData).
--
-- Day: the date part of the timestamp as written; like Fitbit's daily files,
-- the trailing "Z" is not treated as UTC, so nothing shifts across midnight.
-- The glob takes only distance_<date>.csv (not the readme).
-- Replaces R/load_distance.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_distance_daily AS
SELECT
  CAST(substr("timestamp", 1, 10) AS DATE) AS date,
  coalesce(sum(distance), 0)                 AS distance
FROM read_csv(
  {{fitbit_activity_dir}} || '/distance_[0-9]*.csv',
  header  = true,
  columns = {'timestamp': 'VARCHAR', 'distance': 'DOUBLE', 'data source': 'VARCHAR'}
)
GROUP BY ALL
ORDER BY date;
