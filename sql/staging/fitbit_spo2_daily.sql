-- Fitbit nightly SpO2 (average percentage), one row per day, from the
-- daily_oxygen_saturation_<date>.csv files (Physical Activity_GoogleData).
-- Day: the date as written; several rows on a day are averaged.
-- Replaces R/load_spo2.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_spo2_daily AS
SELECT
  CAST(substr("timestamp", 1, 10) AS DATE) AS date,
  avg("average percentage")                AS spo2_avg
FROM read_csv(
  {{fitbit_activity_dir}} || '/daily_oxygen_saturation_*.csv',
  header = true,
  types  = {'timestamp': 'VARCHAR', 'average percentage': 'DOUBLE'}
)
GROUP BY ALL
ORDER BY date;
