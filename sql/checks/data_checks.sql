-- Data checks on the source tables, after staging and before the marts.
-- Run by run_data_checks() (R/run_sql.R), after expected_columns.sql.
--
--   1. columns:       every column of expected_columns.sql present, with its
--                     type                                        → stops
--   2. duplicate key: one row per date (daily tables) or per ID   → stops
--   3. range:         values outside a plausible range            → warns
--
-- A stop names the table and the problem. Implausible values are kept and
-- listed (never dropped silently): the build warns, and they stay in the
-- table data_checks for review. Decide per value: an override in config or
-- a flag, never an edit of the raw file.

-- 1. Columns ─────────────────────────────────────────────────────────────────

CREATE OR REPLACE TABLE data_checks AS
SELECT
  'stop'          AS severity,
  'columns'       AS check_name,
  e.table_name,
  e.column_name || CASE WHEN c.data_type IS NULL THEN ' is missing'
                        ELSE ' is ' || c.data_type || ', expected ' || e.data_type END AS detail
FROM expected_columns e
LEFT JOIN information_schema.columns c
  ON c.table_schema = 'main' AND c.table_name = e.table_name AND c.column_name = e.column_name
WHERE c.data_type IS DISTINCT FROM e.data_type;

SELECT error('data checks failed: ' || string_agg(table_name || '.' || detail, '; '))
FROM data_checks
HAVING count(*) > 0;

-- 2. Duplicate keys ──────────────────────────────────────────────────────────

INSERT INTO data_checks
SELECT 'stop', 'duplicate key', t, key_name || ' ' || k || ' appears ' || n || ' times'
FROM (
  SELECT 'fitbit_azm_daily' t, 'date' key_name, CAST(date AS VARCHAR) k, count(*) n FROM fitbit_azm_daily GROUP BY ALL
  UNION ALL SELECT 'fitbit_calories_daily', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_calories_daily GROUP BY ALL
  UNION ALL SELECT 'fitbit_distance_daily', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_distance_daily GROUP BY ALL
  UNION ALL SELECT 'fitbit_hrv_daily', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_hrv_daily GROUP BY ALL
  UNION ALL SELECT 'fitbit_readiness_daily', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_readiness_daily GROUP BY ALL
  UNION ALL SELECT 'fitbit_rhr_daily', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_rhr_daily GROUP BY ALL
  UNION ALL SELECT 'fitbit_rr_daily', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_rr_daily GROUP BY ALL
  UNION ALL SELECT 'fitbit_sleep_daily', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_sleep_daily GROUP BY ALL
  UNION ALL SELECT 'fitbit_sleep_score', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_sleep_score GROUP BY ALL
  UNION ALL SELECT 'fitbit_spo2_daily', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_spo2_daily GROUP BY ALL
  UNION ALL SELECT 'fitbit_steps_daily', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_steps_daily GROUP BY ALL
  UNION ALL SELECT 'fitbit_stress_score', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_stress_score GROUP BY ALL
  UNION ALL SELECT 'fitbit_temperature_nightly', 'date', CAST(date AS VARCHAR), count(*) FROM fitbit_temperature_nightly GROUP BY ALL
  UNION ALL SELECT 'garmin_api_daily', 'date', CAST(date AS VARCHAR), count(*) FROM garmin_api_daily GROUP BY ALL
  UNION ALL SELECT 'macrofactor_daily', 'date', CAST(date AS VARCHAR), count(*) FROM macrofactor_daily GROUP BY ALL
  UNION ALL SELECT 'strava_activities', 'activity_id', activity_id, count(*) FROM strava_activities GROUP BY ALL
  UNION ALL SELECT 'strava_laps', 'lap_id', lap_id, count(*) FROM strava_laps GROUP BY ALL
  UNION ALL SELECT 'strava_segment_efforts', 'effort_id', effort_id, count(*) FROM strava_segment_efforts GROUP BY ALL
  UNION ALL SELECT 'strava_segments', 'segment_id', segment_id, count(*) FROM strava_segments GROUP BY ALL
  UNION ALL SELECT 'strava_best_efforts', 'best_effort_id', best_effort_id, count(*) FROM strava_best_efforts GROUP BY ALL
)
WHERE n > 1 OR k IS NULL;

SELECT error('data checks failed: ' || string_agg(table_name || ': ' || detail, '; ' ORDER BY table_name, detail))
FROM (SELECT * FROM data_checks WHERE severity = 'stop' LIMIT 10)
HAVING count(*) > 0;

-- 3. Plausible ranges ────────────────────────────────────────────────────────
-- Wide enough for anyone; a value outside is a sensor or logging error until
-- shown otherwise. Missing values are not checked here.

INSERT INTO data_checks
SELECT 'warn', 'range', t,
       c || ' = ' || round(v, 2) || ' on ' || date || ' (plausible ' || lo || ' to ' || hi || ')'
FROM (
  --        table                        column                  date  value                   low     high
  SELECT 'fitbit_rhr_daily' t,       'resting_heart_rate' c,     date, resting_heart_rate v,    30 lo,  120 hi FROM fitbit_rhr_daily
  UNION ALL SELECT 'garmin_api_daily',  'ga_rhr',                  date, ga_rhr,                  30,     120 FROM garmin_api_daily
  UNION ALL SELECT 'fitbit_hrv_daily',  'hrv_rmssd',               date, hrv_rmssd,                5,     250 FROM fitbit_hrv_daily
  UNION ALL SELECT 'garmin_api_daily',  'ga_hrv_last_night_avg',   date, ga_hrv_last_night_avg,    5,     250 FROM garmin_api_daily
  UNION ALL SELECT 'fitbit_rr_daily',   'respiratory_rate',        date, respiratory_rate,         6,      30 FROM fitbit_rr_daily
  UNION ALL SELECT 'garmin_api_daily',  'ga_sleep_resp_avg',       date, ga_sleep_resp_avg,        6,      30 FROM garmin_api_daily
  UNION ALL SELECT 'fitbit_spo2_daily', 'spo2_avg',                date, spo2_avg,                80,     100 FROM fitbit_spo2_daily
  UNION ALL SELECT 'garmin_api_daily',  'ga_sleep_spo2_avg',       date, ga_sleep_spo2_avg,       80,     100 FROM garmin_api_daily
  UNION ALL SELECT 'fitbit_temperature_nightly', 'temperature_celsius - baseline', date,
                   temperature_celsius - baseline_temperature_celsius,                          -5,       5 FROM fitbit_temperature_nightly
  UNION ALL SELECT 'garmin_api_daily',  'ga_sleep_skin_temp_dev_c', date, ga_sleep_skin_temp_dev_c, -5,     5 FROM garmin_api_daily
  -- Sleep: up to 16 hours
  UNION ALL SELECT 'fitbit_sleep_daily', 'sleep_s',                date, sleep_s,                  0,   57600 FROM fitbit_sleep_daily
  UNION ALL SELECT 'fitbit_sleep_daily', 'sleep_deep_s',           date, sleep_deep_s,             0,   57600 FROM fitbit_sleep_daily
  UNION ALL SELECT 'fitbit_sleep_daily', 'sleep_light_s',          date, sleep_light_s,            0,   57600 FROM fitbit_sleep_daily
  UNION ALL SELECT 'fitbit_sleep_daily', 'sleep_rem_s',            date, sleep_rem_s,              0,   57600 FROM fitbit_sleep_daily
  UNION ALL SELECT 'fitbit_sleep_daily', 'sleep_awake_s',          date, sleep_awake_s,            0,   57600 FROM fitbit_sleep_daily
  UNION ALL SELECT 'garmin_api_daily',  'ga_sleep_s',              date, ga_sleep_s,               0,   57600 FROM garmin_api_daily
  UNION ALL SELECT 'garmin_api_daily',  'ga_sleep_deep_s',         date, ga_sleep_deep_s,          0,   57600 FROM garmin_api_daily
  UNION ALL SELECT 'garmin_api_daily',  'ga_sleep_light_s',        date, ga_sleep_light_s,         0,   57600 FROM garmin_api_daily
  UNION ALL SELECT 'garmin_api_daily',  'ga_sleep_rem_s',          date, ga_sleep_rem_s,           0,   57600 FROM garmin_api_daily
  UNION ALL SELECT 'garmin_api_daily',  'ga_sleep_awake_s',        date, ga_sleep_awake_s,         0,   57600 FROM garmin_api_daily
  -- Activity and body
  UNION ALL SELECT 'fitbit_steps_daily', 'steps',                  date, steps,                    0,  100000 FROM fitbit_steps_daily
  UNION ALL SELECT 'garmin_api_daily',  'ga_steps',                date, ga_steps,                 0,  100000 FROM garmin_api_daily
  UNION ALL SELECT 'macrofactor_daily', 'weight_kg',               date, weight_kg,               30,     250 FROM macrofactor_daily
  UNION ALL SELECT 'macrofactor_daily', 'trend_weight_kg',         date, trend_weight_kg,         30,     250 FROM macrofactor_daily
  UNION ALL SELECT 'fitbit_weight',     'weight_kg',               date, weight_kg,               30,     250 FROM fitbit_weight
  UNION ALL SELECT 'macrofactor_daily', 'calories_kcal',           date, calories_kcal,            0,   10000 FROM macrofactor_daily
  UNION ALL SELECT 'strava_activities', 'avg_hr',                  date, avg_hr,                  30,     230 FROM strava_activities
  UNION ALL SELECT 'strava_activities', 'max_hr',                  date, max_hr,                  30,     230 FROM strava_activities
  UNION ALL SELECT 'strava_activities', 'distance_m',              date, distance_m,               0, 1000000 FROM strava_activities
)
WHERE v < lo OR v > hi;
