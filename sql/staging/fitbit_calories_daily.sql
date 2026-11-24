-- Fitbit calories burned per day, from the per-minute calories_<date>.csv files in
-- the Takeout export (Physical Activity_GoogleData).
--
-- Day: the date part of the timestamp as written. Like Fitbit's daily files,
-- the trailing "Z" is not treated as UTC, so nothing shifts across midnight.
-- The glob leaves out calories_in_heart_rate_zone_*.csv (another table, with
-- a kcal column per zone).
-- Replaces R/load_calories.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_calories_daily AS
SELECT
  CAST(substr("timestamp", 1, 10) AS DATE) AS date,
  sum(calories)                            AS calories
FROM read_csv(
  {{fitbit_activity_dir}} || '/calories_[0-9]*.csv',
  header  = true,
  columns = {'timestamp': 'VARCHAR', 'calories': 'DOUBLE', 'data source': 'VARCHAR'}
)
GROUP BY ALL
ORDER BY date;
