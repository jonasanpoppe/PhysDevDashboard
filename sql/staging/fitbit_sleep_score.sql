-- Fitbit sleep scores, one row per day, from UserSleepScores_*.csv
-- (Health Fitness Data_GoogleData). Day: the date part of score_time as
-- written (UTC); several scores on a day are averaged. Fitbit writes -1 for a
-- sub-score it did not compute (kept as written).
-- Replaces R/load_sleep_score.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE fitbit_sleep_score AS
SELECT
  CAST(substr(score_time, 1, 10) AS DATE) AS date,
  avg(overall_score)                      AS sleep_score,
  avg(duration_score)                     AS sleep_duration_score,
  avg(composition_score)                  AS sleep_composition_score,
  avg(revitalization_score)               AS sleep_revitalization_score
FROM read_csv(
  {{fitbit_sleep_score_dir}} || '/UserSleepScores_*.csv',
  header = true,
  types  = {'score_time': 'VARCHAR', 'overall_score': 'DOUBLE', 'duration_score': 'DOUBLE',
            'composition_score': 'DOUBLE', 'revitalization_score': 'DOUBLE'}
)
GROUP BY ALL
ORDER BY date;
