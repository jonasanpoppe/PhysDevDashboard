-- Fitbit Daily Readiness score, one row per day, from the monthly files
-- "Daily Readiness/Daily Readiness Score - <date>.csv". Day: their date.
-- Replaces R/load_readiness.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_readiness_daily AS
SELECT
  CAST("date" AS DATE)       AS date,
  avg(readiness_score_value) AS readiness_score
FROM read_csv(
  {{fitbit_readiness_dir}} || '/Daily Readiness Score*.csv',
  header = true,
  types  = {'date': 'VARCHAR', 'readiness_score_value': 'DOUBLE'}
)
GROUP BY ALL
ORDER BY date;
