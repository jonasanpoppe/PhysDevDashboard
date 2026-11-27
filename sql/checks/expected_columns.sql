-- The source tables' contract: the columns (and types) the marts and views
-- read from each staging or loader table. data_checks.sql stops the build
-- when one is missing or has another type: an export that changed its
-- layout (MacroFactor's xlsx, a new Takeout) fails here, with the column
-- named, rather than somewhere downstream.
--
-- A table may have more columns; only these are promised. Add a column here
-- when new code starts reading it.

CREATE OR REPLACE TABLE expected_columns AS
SELECT
  table_name,
  split_part(col, ' ', 1) AS column_name,
  split_part(col, ' ', 2) AS data_type
FROM (
  SELECT table_name, trim(unnest(string_split(regexp_replace(cols, '\s+', ' ', 'g'), ','))) AS col
  FROM (VALUES
    ('fitbit_azm_daily',
      'date DATE, azm_fat_burn DOUBLE, azm_cardio DOUBLE, azm_peak DOUBLE,
       azm_total DOUBLE'),
    ('fitbit_calories_daily',
      'date DATE, calories DOUBLE'),
    ('fitbit_distance_daily',
      'date DATE, distance DOUBLE'),
    ('fitbit_hrv_daily',
      'date DATE, hrv_rmssd DOUBLE'),
    ('fitbit_readiness_daily',
      'date DATE, readiness_score DOUBLE'),
    ('fitbit_rhr_daily',
      'date DATE, resting_heart_rate DOUBLE'),
    ('fitbit_rr_daily',
      'date DATE, respiratory_rate DOUBLE'),
    ('fitbit_sleep_daily',
      'date DATE, log_id VARCHAR, sleep_end_local VARCHAR, sleep_s DOUBLE,
       sleep_awake_s DOUBLE, sleep_deep_s DOUBLE, sleep_light_s DOUBLE,
       sleep_rem_s DOUBLE'),
    ('fitbit_sleep_score',
      'date DATE, sleep_score DOUBLE'),
    ('fitbit_spo2_daily',
      'date DATE, spo2_avg DOUBLE'),
    ('fitbit_steps_daily',
      'date DATE, steps DOUBLE'),
    ('fitbit_stress_score',
      'date DATE, stress_score DOUBLE'),
    ('fitbit_temperature_nightly',
      'date DATE, temperature_celsius DOUBLE,
       baseline_temperature_celsius DOUBLE'),
    ('fitbit_weight',
      'date DATE, weight_kg DOUBLE, fat_percent DOUBLE'),
    ('garmin_api_daily',
      'date DATE, ga_rhr DOUBLE, ga_hrv_last_night_avg DOUBLE,
       ga_hrv_baseline_low DOUBLE, ga_hrv_baseline_high DOUBLE,
       ga_steps DOUBLE, ga_sleep_end_local VARCHAR, ga_sleep_s DOUBLE,
       ga_sleep_deep_s DOUBLE, ga_sleep_light_s DOUBLE, ga_sleep_rem_s DOUBLE,
       ga_sleep_awake_s DOUBLE, ga_sleep_spo2_avg DOUBLE,
       ga_sleep_resp_avg DOUBLE, ga_sleep_skin_temp_dev_c DOUBLE'),
    ('macrofactor_daily',
      'date DATE, calories_kcal DOUBLE, fat_g DOUBLE, carbs_g DOUBLE,
       protein_g DOUBLE, weight_kg DOUBLE, trend_weight_kg DOUBLE,
       expenditure DOUBLE'),
    ('macrofactor_food_log',
      'date DATE, food_name VARCHAR, calories_kcal DOUBLE, protein_g DOUBLE'),
    ('macrofactor_workout_sets',
      'date DATE, exercise VARCHAR, set_type VARCHAR, weight_kg DOUBLE,
       reps DOUBLE, rir DOUBLE, is_warmup BOOLEAN, load_kg DOUBLE,
       e1rm_kg DOUBLE'),
    ('strava_activities',
      'activity_id VARCHAR, date DATE, start_utc TIMESTAMP,
       sport_type VARCHAR, distance_m DOUBLE, moving_s DOUBLE,
       elapsed_s DOUBLE, elev_gain_m DOUBLE, avg_hr DOUBLE, max_hr DOUBLE,
       device_watts BOOLEAN, avg_watts DOUBLE, gear_id VARCHAR'),
    ('strava_laps',
      'lap_id VARCHAR, activity_id VARCHAR, start_utc TIMESTAMP,
       distance_m DOUBLE, moving_s DOUBLE, elapsed_s DOUBLE'),
    ('strava_segment_efforts',
      'effort_id VARCHAR, activity_id VARCHAR, segment_id VARCHAR, date DATE,
       start_utc TIMESTAMP, elapsed_s DOUBLE'),
    ('strava_segments',
      'segment_id VARCHAR, distance_m DOUBLE, elev_gain_m DOUBLE'),
    ('strava_best_efforts',
      'best_effort_id VARCHAR, activity_id VARCHAR, date DATE')
) t(table_name, cols)
);
