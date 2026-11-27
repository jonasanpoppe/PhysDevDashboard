-- Fitbit Active Zone Minutes per day and zone, from the Takeout export
-- ("Active Zone Minutes (AZM)/Active Zone Minutes - <date>.csv", one row per
-- minute in a zone).
--
-- Day: the date part of date_time as written (local wall time, no offset).
-- Zones: Fitbit's three, one column each, 0 on a day without minutes in it;
-- azm_total is their sum. An unknown zone stops the build rather than being
-- dropped.
-- Replaces R/load_azm.R (same table, same values; tested for parity).

CREATE OR REPLACE TEMP TABLE azm_raw AS
SELECT
  CAST(substr(date_time, 1, 10) AS DATE) AS date,
  heart_zone_id,
  total_minutes
FROM read_csv(
  {{fitbit_azm_dir}} || '/Active Zone Minutes*.csv',
  header  = true,
  columns = {'date_time': 'VARCHAR', 'heart_zone_id': 'VARCHAR', 'total_minutes': 'DOUBLE'}
);

SELECT error('fitbit_azm_daily: unknown zone ' || heart_zone_id)
FROM azm_raw
WHERE heart_zone_id NOT IN ('FAT_BURN', 'CARDIO', 'PEAK');

CREATE OR REPLACE TABLE fitbit_azm_daily AS
SELECT
  date,
  coalesce(sum(total_minutes) FILTER (heart_zone_id = 'FAT_BURN'), 0) AS azm_fat_burn,
  coalesce(sum(total_minutes) FILTER (heart_zone_id = 'CARDIO'), 0)   AS azm_cardio,
  coalesce(sum(total_minutes) FILTER (heart_zone_id = 'PEAK'), 0)     AS azm_peak,
  sum(total_minutes)                                                  AS azm_total
FROM azm_raw
GROUP BY date
ORDER BY date;
