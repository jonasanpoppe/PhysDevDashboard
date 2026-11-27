-- Fitbit breathing rate during sleep, one row per night, dated by the wake-up
-- day (see docs/sources.md): the date of Fitbit's daily file as written.
--
-- Reads Fitbit's daily file. The per-sleep summaries include naps and are
-- timestamped in UTC, so averaging them by UTC date mixed naps and nights.
-- Replaces R/load_rr.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_rr_daily AS
SELECT
  CAST(substr("timestamp", 1, 10) AS DATE) AS date,
  "breaths per minute"                     AS respiratory_rate
FROM read_csv(
  {{fitbit_activity_dir}} || '/daily_respiratory_rate.csv',
  header  = true,
  columns = {'timestamp': 'VARCHAR', 'breaths per minute': 'DOUBLE', 'data source': 'VARCHAR'}
)
ORDER BY date;
