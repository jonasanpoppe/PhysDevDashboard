-- Strava API → tables. The raw files under data/landing/strava/ (written by
-- python/fetch_strava_api.py) are the source of truth; these tables are
-- rebuilt from them:
--
--   strava_activities       one row per activity (list fetches + details)
--   strava_laps             laps of each activity
--   strava_segment_efforts  segment efforts of each activity
--   strava_segments         every segment ridden, with your PR stats
--   strava_best_efforts     best efforts (running distances) per activity
--
-- Streams stay files: only derived summaries (time in HR zone, built in R)
-- reach the database.
--
-- - IDs are text, read as exact 64-bit integers (never summed).
-- - Times: *_utc are UTC timestamps. *_local are Strava's local wall time, as
--   text without offset (Strava appends a misleading "Z"). `date` is the
--   local calendar date, the convention for every source.
-- - Activity summaries come from every list fetch; a later file wins.
-- - Detail and segment files may be {"_status": ...} "no data" markers; they
--   have no id and are left out.
-- - Only the fields listed here are read; a new API field changes nothing.
-- Replaces the table part of R/load_strava_api.R (same tables, same values;
-- tested for parity).

CREATE OR REPLACE TEMP MACRO strava_utc(x) AS CAST(strptime(x, '%Y-%m-%dT%H:%M:%SZ') AS TIMESTAMP);
CREATE OR REPLACE TEMP MACRO strava_local(x) AS regexp_replace(x, 'Z$', '');
CREATE OR REPLACE TEMP MACRO strava_date(x) AS CAST(substr(x, 1, 10) AS DATE);

-- ── Raw files ────────────────────────────────────────────────────────────────

CREATE OR REPLACE TEMP TABLE strava_summaries AS
SELECT * EXCLUDE (n)
FROM (
  SELECT *, row_number() OVER (PARTITION BY id ORDER BY filename DESC) AS n
  FROM read_json(
    {{strava_dir}} || '/*_strava_activities.json',
    format = 'array', filename = true,
    columns = {
      id: 'BIGINT', start_date: 'VARCHAR', start_date_local: 'VARCHAR', timezone: 'VARCHAR',
      name: 'VARCHAR', sport_type: 'VARCHAR', trainer: 'BOOLEAN', commute: 'BOOLEAN',
      manual: 'BOOLEAN', distance: 'DOUBLE', moving_time: 'DOUBLE', elapsed_time: 'DOUBLE',
      total_elevation_gain: 'DOUBLE', average_speed: 'DOUBLE', max_speed: 'DOUBLE',
      has_heartrate: 'BOOLEAN', average_heartrate: 'DOUBLE', max_heartrate: 'DOUBLE',
      device_watts: 'BOOLEAN', average_watts: 'DOUBLE', weighted_average_watts: 'DOUBLE',
      kilojoules: 'DOUBLE', average_cadence: 'DOUBLE', suffer_score: 'DOUBLE',
      gear_id: 'VARCHAR', device_name: 'VARCHAR'
    }
  )
)
WHERE n = 1;

CREATE OR REPLACE TEMP TABLE strava_details AS
SELECT *
FROM read_json(
  {{strava_dir}} || '/details/*.json',
  format = 'auto', filename = true,
  columns = {
    id: 'BIGINT', calories: 'DOUBLE', perceived_exertion: 'DOUBLE', description: 'VARCHAR',
    laps: 'STRUCT(
      id BIGINT, activity STRUCT(id BIGINT), lap_index DOUBLE, start_date VARCHAR,
      start_date_local VARCHAR, distance DOUBLE, moving_time DOUBLE, elapsed_time DOUBLE,
      total_elevation_gain DOUBLE, average_speed DOUBLE, max_speed DOUBLE,
      average_heartrate DOUBLE, max_heartrate DOUBLE, device_watts BOOLEAN,
      average_watts DOUBLE, average_cadence DOUBLE, start_index DOUBLE, end_index DOUBLE)[]',
    segment_efforts: 'STRUCT(
      id BIGINT, activity STRUCT(id BIGINT),
      segment STRUCT(
        id BIGINT, name VARCHAR, activity_type VARCHAR, distance DOUBLE,
        average_grade DOUBLE, maximum_grade DOUBLE, elevation_low DOUBLE,
        elevation_high DOUBLE, climb_category DOUBLE, city VARCHAR, country VARCHAR,
        start_latlng DOUBLE[], end_latlng DOUBLE[], hazardous BOOLEAN, private BOOLEAN),
      start_date VARCHAR, start_date_local VARCHAR, distance DOUBLE, elapsed_time DOUBLE,
      moving_time DOUBLE, average_heartrate DOUBLE, max_heartrate DOUBLE,
      device_watts BOOLEAN, average_watts DOUBLE, average_cadence DOUBLE,
      pr_rank DOUBLE, kom_rank DOUBLE, hidden BOOLEAN, start_index DOUBLE, end_index DOUBLE)[]',
    best_efforts: 'STRUCT(
      id BIGINT, activity STRUCT(id BIGINT), name VARCHAR, start_date VARCHAR,
      start_date_local VARCHAR, distance DOUBLE, elapsed_time DOUBLE, moving_time DOUBLE,
      pr_rank DOUBLE, start_index DOUBLE, end_index DOUBLE)[]'
  }
)
WHERE id IS NOT NULL;

-- One row per item of a detail's list, with its file and position (the order
-- the items appear in, for ties in the sort below)
CREATE OR REPLACE TEMP TABLE strava_effort_items AS
SELECT filename, generate_subscripts(segment_efforts, 1) AS i, unnest(segment_efforts) AS e
FROM strava_details;

-- ── Tables ───────────────────────────────────────────────────────────────────

CREATE OR REPLACE TABLE strava_activities AS
SELECT
  CAST(s.id AS VARCHAR)            AS activity_id,
  strava_date(s.start_date_local)  AS date,
  strava_utc(s.start_date)         AS start_utc,
  strava_local(s.start_date_local) AS start_local,
  s.timezone,
  s.name,
  s.sport_type,
  s.trainer,
  s.commute,
  s.manual,
  s.distance                       AS distance_m,
  s.moving_time                    AS moving_s,
  s.elapsed_time                   AS elapsed_s,
  s.total_elevation_gain           AS elev_gain_m,
  s.average_speed                  AS avg_speed_ms,
  s.max_speed                      AS max_speed_ms,
  s.has_heartrate,
  s.average_heartrate              AS avg_hr,
  s.max_heartrate                  AS max_hr,
  -- Without a power meter (device_watts false) Strava estimates power:
  -- directional only
  s.device_watts,
  s.average_watts                  AS avg_watts,
  s.weighted_average_watts         AS weighted_avg_watts,
  s.kilojoules,
  s.average_cadence                AS avg_cadence,
  s.suffer_score,
  s.gear_id,
  s.device_name,
  d.calories                       AS kcal,
  d.perceived_exertion,
  d.description
FROM strava_summaries s
LEFT JOIN strava_details d USING (id)
ORDER BY start_utc;

CREATE OR REPLACE TABLE strava_laps AS
WITH items AS (
  SELECT filename, generate_subscripts(laps, 1) AS i, unnest(laps) AS l FROM strava_details
)
SELECT
  CAST(l.id AS VARCHAR)            AS lap_id,
  CAST(l.activity.id AS VARCHAR)   AS activity_id,
  l.lap_index,
  strava_utc(l.start_date)         AS start_utc,
  strava_local(l.start_date_local) AS start_local,
  l.distance                       AS distance_m,
  l.moving_time                    AS moving_s,
  l.elapsed_time                   AS elapsed_s,
  l.total_elevation_gain           AS elev_gain_m,
  l.average_speed                  AS avg_speed_ms,
  l.max_speed                      AS max_speed_ms,
  l.average_heartrate              AS avg_hr,
  l.max_heartrate                  AS max_hr,
  l.device_watts,
  l.average_watts                  AS avg_watts,
  l.average_cadence                AS avg_cadence,
  l.start_index,
  l.end_index
FROM items
ORDER BY start_utc, l.lap_index, filename, i;

CREATE OR REPLACE TABLE strava_segment_efforts AS
SELECT
  CAST(e.id AS VARCHAR)            AS effort_id,
  CAST(e.activity.id AS VARCHAR)   AS activity_id,
  CAST(e.segment.id AS VARCHAR)    AS segment_id,
  e.segment.name                   AS segment_name,
  strava_date(e.start_date_local)  AS date,
  strava_utc(e.start_date)         AS start_utc,
  strava_local(e.start_date_local) AS start_local,
  e.distance                       AS distance_m,
  e.elapsed_time                   AS elapsed_s,
  e.moving_time                    AS moving_s,
  e.average_heartrate              AS avg_hr,
  e.max_heartrate                  AS max_hr,
  e.device_watts,
  e.average_watts                  AS avg_watts,
  e.average_cadence                AS avg_cadence,
  -- 1-3 when among your three best at the time; NULL otherwise
  e.pr_rank,
  e.kom_rank,
  e.hidden,
  -- Positions in the activity's streams, to slice HR or power per effort
  e.start_index,
  e.end_index
FROM strava_effort_items
ORDER BY start_utc, filename, i;

-- Every segment appears inside its efforts (the first appearance is kept);
-- the full segment files (fetched later) add totals and your PR stats.
-- best_vam_mh: climbing rate of your best effort; at or above 1800 m/h the
-- segment's elevation data is broken (common for auto-created "… Climb"
-- segments), flagged by elev_suspect.
CREATE OR REPLACE TABLE strava_segments AS
WITH embedded AS (
  SELECT e.segment AS g, row_number() OVER (PARTITION BY e.segment.id ORDER BY filename, i) AS n,
         filename, i
  FROM strava_effort_items
),
full_segments AS (
  SELECT *
  FROM read_json(
    {{strava_dir}} || '/segments/*.json',
    format = 'auto',
    columns = {
      id: 'BIGINT', total_elevation_gain: 'DOUBLE', starred: 'BOOLEAN',
      athlete_count: 'DOUBLE', effort_count: 'DOUBLE',
      athlete_segment_stats: 'STRUCT(pr_elapsed_time DOUBLE, pr_date VARCHAR, effort_count DOUBLE)',
      map: 'STRUCT(polyline VARCHAR)'
    }
  )
  WHERE id IS NOT NULL
),
mine AS (
  SELECT
    segment_id,
    CAST(count(*) AS INTEGER) AS my_efforts_loaded,
    min(elapsed_s)            AS best_elapsed_s,
    min(date)                 AS first_date,
    max(date)                 AS last_date
  FROM strava_segment_efforts
  GROUP BY segment_id
),
joined AS (
  SELECT
    CAST(g.id AS VARCHAR)                     AS segment_id,
    g.name,
    g.activity_type,
    g.distance                                AS distance_m,
    g.average_grade                           AS avg_grade_pct,
    g.maximum_grade                           AS max_grade_pct,
    g.elevation_low                           AS elev_low_m,
    g.elevation_high                          AS elev_high_m,
    g.climb_category,
    g.city,
    g.country,
    g.start_latlng[1]                         AS start_lat,
    g.start_latlng[2]                         AS start_lng,
    g.end_latlng[1]                           AS end_lat,
    g.end_latlng[2]                           AS end_lng,
    g.hazardous,
    g.private,
    f.total_elevation_gain                    AS elev_gain_m,
    f.starred,
    f.athlete_count,
    f.effort_count                            AS effort_count_all,
    f.athlete_segment_stats.pr_elapsed_time   AS pr_elapsed_s,
    CAST(f.athlete_segment_stats.pr_date AS DATE) AS pr_date,
    f.athlete_segment_stats.effort_count      AS my_effort_count,
    f.map.polyline                            AS polyline,
    m.my_efforts_loaded,
    m.best_elapsed_s,
    m.first_date,
    m.last_date,
    (g.elevation_high - g.elevation_low) / m.best_elapsed_s * 3600 AS best_vam_mh,
    filename, i
  FROM embedded
  LEFT JOIN full_segments f ON f.id = g.id
  LEFT JOIN mine m ON m.segment_id = CAST(g.id AS VARCHAR)
  WHERE n = 1
)
SELECT * EXCLUDE (filename, i), best_vam_mh >= 1800 AS elev_suspect
FROM joined
ORDER BY filename, i;

CREATE OR REPLACE TABLE strava_best_efforts AS
WITH items AS (
  SELECT filename, generate_subscripts(best_efforts, 1) AS i, unnest(best_efforts) AS b FROM strava_details
)
SELECT
  CAST(b.id AS VARCHAR)           AS best_effort_id,
  CAST(b.activity.id AS VARCHAR)  AS activity_id,
  b.name,
  strava_date(b.start_date_local) AS date,
  strava_utc(b.start_date)        AS start_utc,
  b.distance                      AS distance_m,
  b.elapsed_time                  AS elapsed_s,
  b.moving_time                   AS moving_s,
  b.pr_rank,
  b.start_index,
  b.end_index
FROM items
ORDER BY start_utc, distance_m, filename, i;
