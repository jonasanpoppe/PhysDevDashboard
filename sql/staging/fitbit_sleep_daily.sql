-- Fitbit sleep logs (Takeout "Global Export Data/sleep-<date>.json"): the
-- main sleep of each night, dated by Fitbit's dateOfSleep, which is the
-- wake-up day. Durations are in seconds, with the same names as the Garmin
-- columns.
--
-- - The monthly files overlap, so logs are deduplicated by logId (the copy
--   in the first file by name is kept).
-- - Naps (mainSleep = false) are left out; two main sleeps on one date stop
--   the build.
-- - "classic" logs (older or short sleeps) have no stages: asleep and awake
--   time only, stage columns NULL.
-- - Start and end are local wall time without offset, as Fitbit writes them.
-- - Only the summary is read; the per-30-second stage data stays in the file.
-- Replaces R/load_fitbit_sleep.R (same table, same values; tested for parity).

CREATE OR REPLACE TEMP TABLE fitbit_sleep_logs AS
SELECT *
FROM (
  SELECT
    *,
    row_number() OVER (PARTITION BY logId ORDER BY filename) AS copy_n
  FROM read_json(
    {{fitbit_sleep_dir}} || '/sleep-*.json',
    format   = 'array',
    filename = true,
    columns  = {
      logId: 'BIGINT', dateOfSleep: 'DATE', startTime: 'VARCHAR', endTime: 'VARCHAR',
      minutesAsleep: 'DOUBLE', minutesAwake: 'DOUBLE', efficiency: 'DOUBLE',
      "type": 'VARCHAR', mainSleep: 'BOOLEAN',
      levels: 'STRUCT(summary STRUCT(deep STRUCT(minutes DOUBLE), light STRUCT(minutes DOUBLE), rem STRUCT(minutes DOUBLE)))'
    }
  )
)
WHERE copy_n = 1 AND mainSleep;

SELECT error('fitbit_sleep_daily: more than one main sleep on ' || string_agg(DISTINCT CAST(dateOfSleep AS VARCHAR), ', '))
FROM (SELECT dateOfSleep FROM fitbit_sleep_logs GROUP BY dateOfSleep HAVING count(*) > 1)
HAVING count(*) > 0;

CREATE OR REPLACE TABLE fitbit_sleep_daily AS
SELECT
  dateOfSleep                                   AS date,
  CAST(logId AS VARCHAR)                        AS log_id,
  "type"                                        AS log_type,
  regexp_replace(startTime, '\.000$', '')       AS sleep_start_local,
  regexp_replace(endTime, '\.000$', '')         AS sleep_end_local,
  minutesAsleep * 60                            AS sleep_s,
  minutesAwake * 60                             AS sleep_awake_s,
  CASE WHEN "type" = 'stages' THEN levels.summary.deep.minutes * 60 END  AS sleep_deep_s,
  CASE WHEN "type" = 'stages' THEN levels.summary.light.minutes * 60 END AS sleep_light_s,
  CASE WHEN "type" = 'stages' THEN levels.summary.rem.minutes * 60 END   AS sleep_rem_s,
  efficiency                                    AS sleep_efficiency
FROM fitbit_sleep_logs
ORDER BY date;
