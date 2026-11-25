-- Fitbit weigh-ins from the Takeout export (Physical Activity_GoogleData):
-- weight.csv (grams) and body_fat_<date>.csv (percent), one row per logged
-- entry. Measurements as logged in the Fitbit app; the body-fat values were
-- typed in, not measured by a device, so treat them as estimates.
--
-- Day: the timestamps are real UTC instants (entries without a time are
-- logged at 23:59:59 local), so the local calendar date in the home timezone
-- is the day.
-- MacroFactor is the system of record for body weight; this table is only a
-- backfill for the time before its first weigh-in (see R/bodyweight.R).

CREATE OR REPLACE TABLE fitbit_weight AS
WITH w AS (
  SELECT CAST("timestamp" AS TIMESTAMPTZ) AS ts, "weight grams" / 1000 AS weight_kg, "data source" AS data_source
  FROM read_csv({{fitbit_activity_dir}} || '/weight.csv', header = true,
                columns = {'timestamp': 'VARCHAR', 'weight grams': 'DOUBLE', 'data source': 'VARCHAR'})
),
f AS (
  SELECT CAST("timestamp" AS TIMESTAMPTZ) AS ts, "body fat percentage" AS fat_percent
  FROM read_csv({{fitbit_activity_dir}} || '/body_fat_[0-9]*.csv', header = true,
                columns = {'timestamp': 'VARCHAR', 'body fat percentage': 'DOUBLE', 'data source': 'VARCHAR'})
)
SELECT
  CAST(timezone({{home_timezone}}, w.ts) AS DATE) AS date,
  w.weight_kg,
  f.fat_percent,
  w.data_source
FROM w LEFT JOIN f USING (ts)
ORDER BY w.ts;
