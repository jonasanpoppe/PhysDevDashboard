-- Fitbit Stress Management score, one row per day, from
-- "Stress Score/Stress Score.csv". Day: the date part of DATE as written.
-- Replaces R/load_stress_score.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_stress_score AS
SELECT
  CAST(substr("DATE", 1, 10) AS DATE) AS date,
  avg(STRESS_SCORE)                   AS stress_score
FROM read_csv(
  {{fitbit_stress_dir}} || '/Stress Score.csv',
  header = true,
  types  = {'DATE': 'VARCHAR', 'STRESS_SCORE': 'DOUBLE'}
)
GROUP BY ALL
ORDER BY date;
