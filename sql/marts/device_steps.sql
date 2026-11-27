-- device_steps: steps a day from the wrist device, one row per date, with
-- its device era. MacroFactor's steps are not used: they are no
-- measurement (0 on days without a sync, and a changing mix of sources).
--
--   date, era, steps
--
-- - era by source (fitbit, garmin); the two are never pooled into one
--   series. A homogenised series (Fitbit moved by the step at the switch,
--   as for the vitals) is derived beside them in R/homogenise.R.
-- - Garmin is the system of record from its first day with steps on; the
--   Fitbit era ends the day before (on the switch day both devices counted
--   part of the day).
-- - Days without a count are left out, not 0.
-- - Stops on a duplicate date. Plausible ranges: sql/checks/data_checks.sql.
-- Reads the staging tables fitbit_steps_daily and garmin_api_daily.

CREATE OR REPLACE TEMP TABLE device_steps_base AS
WITH garmin AS (
  SELECT date, 'garmin' AS era, ga_steps AS steps
  FROM garmin_api_daily
  WHERE ga_steps IS NOT NULL
),
fitbit AS (
  SELECT date, 'fitbit' AS era, steps
  FROM fitbit_steps_daily
  WHERE steps IS NOT NULL
    AND date < coalesce((SELECT min(date) FROM garmin), DATE '9999-12-31')
)
SELECT * FROM fitbit
UNION ALL
SELECT * FROM garmin;

SELECT error('device_steps: duplicate date, e.g. ' || string_agg(CAST(date AS VARCHAR), '; '))
FROM (SELECT date FROM device_steps_base GROUP BY date HAVING count(*) > 1 ORDER BY date LIMIT 3)
HAVING count(*) > 0;

CREATE OR REPLACE TABLE device_steps AS
SELECT date, era, steps FROM device_steps_base ORDER BY date;
