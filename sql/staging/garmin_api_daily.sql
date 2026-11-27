-- Garmin Connect daily values (data/landing/garmin/*_garmin_api.json, written
-- by python/fetch_garmin_api.py): one row per day, every column prefixed ga_.
--
-- - Only the fields listed here are read, so a new fetcher field can't
--   silently change the table before it's added; files from ranges without
--   some data (or from older fetcher versions) lack fields, which are NULL.
-- - Fetches overlap: per day and field, the first non-NULL value in file name
--   order is kept.
-- - Timestamps stay text: *_utc are ISO 8601 UTC, *_local are local wall time
--   without offset. Sleep is dated to the wake-up day by the fetcher.
-- Replaces R/load_garmin_api.R (same table, same values; tested for parity).

CREATE OR REPLACE TABLE garmin_api_daily AS
SELECT
  CAST("date" AS DATE) AS date,
  -- Heart
  first(rhr ORDER BY filename) FILTER (rhr IS NOT NULL) AS ga_rhr,
  first(hrv ORDER BY filename) FILTER (hrv IS NOT NULL) AS ga_hrv,
  first(hrv_last_night_avg ORDER BY filename) FILTER (hrv_last_night_avg IS NOT NULL) AS ga_hrv_last_night_avg,
  first(hrv_last_night_5_min_high ORDER BY filename) FILTER (hrv_last_night_5_min_high IS NOT NULL) AS ga_hrv_last_night_5_min_high,
  first(hrv_baseline_low ORDER BY filename) FILTER (hrv_baseline_low IS NOT NULL) AS ga_hrv_baseline_low,
  first(hrv_baseline_high ORDER BY filename) FILTER (hrv_baseline_high IS NOT NULL) AS ga_hrv_baseline_high,
  first(hrv_status ORDER BY filename) FILTER (hrv_status IS NOT NULL) AS ga_hrv_status,
  -- Intensity minutes
  first(im_moderate ORDER BY filename) FILTER (im_moderate IS NOT NULL) AS ga_im_moderate,
  first(im_vigorous ORDER BY filename) FILTER (im_vigorous IS NOT NULL) AS ga_im_vigorous,
  -- Garmin's own total: vigorous minutes count double
  first(im_moderate ORDER BY filename) FILTER (im_moderate IS NOT NULL)
    + 2 * first(im_vigorous ORDER BY filename) FILTER (im_vigorous IS NOT NULL) AS ga_im_total,
  first(im_week_goal ORDER BY filename) FILTER (im_week_goal IS NOT NULL) AS ga_im_week_goal,
  -- Daily totals
  first(steps ORDER BY filename) FILTER (steps IS NOT NULL) AS ga_steps,
  first(step_goal ORDER BY filename) FILTER (step_goal IS NOT NULL) AS ga_step_goal,
  first(distance_m ORDER BY filename) FILTER (distance_m IS NOT NULL) AS ga_distance_m,
  first(floors_ascended ORDER BY filename) FILTER (floors_ascended IS NOT NULL) AS ga_floors_ascended,
  first(kcal_total ORDER BY filename) FILTER (kcal_total IS NOT NULL) AS ga_kcal_total,
  first(kcal_active ORDER BY filename) FILTER (kcal_active IS NOT NULL) AS ga_kcal_active,
  first(kcal_bmr ORDER BY filename) FILTER (kcal_bmr IS NOT NULL) AS ga_kcal_bmr,
  -- Whole-day SpO2 and breathing rate
  first(spo2_avg ORDER BY filename) FILTER (spo2_avg IS NOT NULL) AS ga_spo2_avg,
  first(spo2_lowest ORDER BY filename) FILTER (spo2_lowest IS NOT NULL) AS ga_spo2_lowest,
  first(resp_waking_avg ORDER BY filename) FILTER (resp_waking_avg IS NOT NULL) AS ga_resp_waking_avg,
  first(resp_lowest ORDER BY filename) FILTER (resp_lowest IS NOT NULL) AS ga_resp_lowest,
  first(resp_highest ORDER BY filename) FILTER (resp_highest IS NOT NULL) AS ga_resp_highest,
  -- Sleep (dated to the wake-up day)
  first(sleep_start_utc ORDER BY filename) FILTER (sleep_start_utc IS NOT NULL) AS ga_sleep_start_utc,
  first(sleep_end_utc ORDER BY filename) FILTER (sleep_end_utc IS NOT NULL) AS ga_sleep_end_utc,
  first(sleep_start_local ORDER BY filename) FILTER (sleep_start_local IS NOT NULL) AS ga_sleep_start_local,
  first(sleep_end_local ORDER BY filename) FILTER (sleep_end_local IS NOT NULL) AS ga_sleep_end_local,
  first(sleep_s ORDER BY filename) FILTER (sleep_s IS NOT NULL) AS ga_sleep_s,
  first(sleep_deep_s ORDER BY filename) FILTER (sleep_deep_s IS NOT NULL) AS ga_sleep_deep_s,
  first(sleep_light_s ORDER BY filename) FILTER (sleep_light_s IS NOT NULL) AS ga_sleep_light_s,
  first(sleep_rem_s ORDER BY filename) FILTER (sleep_rem_s IS NOT NULL) AS ga_sleep_rem_s,
  first(sleep_awake_s ORDER BY filename) FILTER (sleep_awake_s IS NOT NULL) AS ga_sleep_awake_s,
  first(sleep_unmeasurable_s ORDER BY filename) FILTER (sleep_unmeasurable_s IS NOT NULL) AS ga_sleep_unmeasurable_s,
  first(nap_s ORDER BY filename) FILTER (nap_s IS NOT NULL) AS ga_nap_s,
  first(sleep_awake_count ORDER BY filename) FILTER (sleep_awake_count IS NOT NULL) AS ga_sleep_awake_count,
  first(sleep_score ORDER BY filename) FILTER (sleep_score IS NOT NULL) AS ga_sleep_score,
  first(sleep_avg_hr ORDER BY filename) FILTER (sleep_avg_hr IS NOT NULL) AS ga_sleep_avg_hr,
  first(sleep_avg_stress ORDER BY filename) FILTER (sleep_avg_stress IS NOT NULL) AS ga_sleep_avg_stress,
  first(sleep_spo2_avg ORDER BY filename) FILTER (sleep_spo2_avg IS NOT NULL) AS ga_sleep_spo2_avg,
  first(sleep_spo2_lowest ORDER BY filename) FILTER (sleep_spo2_lowest IS NOT NULL) AS ga_sleep_spo2_lowest,
  first(sleep_resp_avg ORDER BY filename) FILTER (sleep_resp_avg IS NOT NULL) AS ga_sleep_resp_avg,
  first(sleep_resp_lowest ORDER BY filename) FILTER (sleep_resp_lowest IS NOT NULL) AS ga_sleep_resp_lowest,
  first(sleep_resp_highest ORDER BY filename) FILTER (sleep_resp_highest IS NOT NULL) AS ga_sleep_resp_highest,
  first(sleep_skin_temp_dev_c ORDER BY filename) FILTER (sleep_skin_temp_dev_c IS NOT NULL) AS ga_sleep_skin_temp_dev_c,
  first(sleep_restless_moments ORDER BY filename) FILTER (sleep_restless_moments IS NOT NULL) AS ga_sleep_restless_moments,
  -- VO2max (Garmin model output, sparse)
  first(vo2max ORDER BY filename) FILTER (vo2max IS NOT NULL) AS ga_vo2max
FROM read_json(
  {{garmin_dir}} || '/*_garmin_api.json',
  format   = 'array',
  filename = true,
  columns  = {
      "date": 'VARCHAR',
      rhr: 'DOUBLE',
      hrv: 'DOUBLE',
      hrv_last_night_avg: 'DOUBLE',
      hrv_last_night_5_min_high: 'DOUBLE',
      hrv_baseline_low: 'DOUBLE',
      hrv_baseline_high: 'DOUBLE',
      hrv_status: 'VARCHAR',
      im_moderate: 'DOUBLE',
      im_vigorous: 'DOUBLE',
      im_week_goal: 'DOUBLE',
      steps: 'DOUBLE',
      step_goal: 'DOUBLE',
      distance_m: 'DOUBLE',
      floors_ascended: 'DOUBLE',
      kcal_total: 'DOUBLE',
      kcal_active: 'DOUBLE',
      kcal_bmr: 'DOUBLE',
      spo2_avg: 'DOUBLE',
      spo2_lowest: 'DOUBLE',
      resp_waking_avg: 'DOUBLE',
      resp_lowest: 'DOUBLE',
      resp_highest: 'DOUBLE',
      sleep_start_utc: 'VARCHAR',
      sleep_end_utc: 'VARCHAR',
      sleep_start_local: 'VARCHAR',
      sleep_end_local: 'VARCHAR',
      sleep_s: 'DOUBLE',
      sleep_deep_s: 'DOUBLE',
      sleep_light_s: 'DOUBLE',
      sleep_rem_s: 'DOUBLE',
      sleep_awake_s: 'DOUBLE',
      sleep_unmeasurable_s: 'DOUBLE',
      nap_s: 'DOUBLE',
      sleep_awake_count: 'DOUBLE',
      sleep_score: 'DOUBLE',
      sleep_avg_hr: 'DOUBLE',
      sleep_avg_stress: 'DOUBLE',
      sleep_spo2_avg: 'DOUBLE',
      sleep_spo2_lowest: 'DOUBLE',
      sleep_resp_avg: 'DOUBLE',
      sleep_resp_lowest: 'DOUBLE',
      sleep_resp_highest: 'DOUBLE',
      sleep_skin_temp_dev_c: 'DOUBLE',
      sleep_restless_moments: 'DOUBLE',
      vo2max: 'DOUBLE'
  }
)
GROUP BY ALL
ORDER BY date;
