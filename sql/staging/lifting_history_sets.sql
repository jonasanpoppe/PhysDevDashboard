-- Lifting history from before MacroFactor: two one-off backfills, already in
-- MacroFactor's workout-log layout (12 columns, same names and units) plus
-- provenance columns (where each set came from, how sure its date and RIR are).
--
--   drive_templates_workout_sets   training templates (spreadsheets), oldest
--   strong_workout_sets            Strong app, up to the first MacroFactor day
--
-- Columns follow macrofactor_workout_sets, so R can stack all three sources
-- and give them one e1RM rule (R/e1rm.R). Set numbers count sets within
-- (date, workout, exercise) in file order, as the MacroFactor loader does.
-- Types are fixed here rather than sniffed: an empty column in a later
-- rebuild must not change type.

CREATE OR REPLACE MACRO lifting_history_csv(path) AS TABLE
SELECT *, row_number() OVER () AS file_row
FROM read_csv(
  path,
  header = true,
  columns = {
    'Date': 'DATE', 'Workout Duration': 'DOUBLE', 'Workout': 'VARCHAR', 'Exercise': 'VARCHAR',
    'Exercise Base Weight (kg)': 'DOUBLE', 'Set Type': 'VARCHAR', 'Weight (kg)': 'DOUBLE',
    'Reps': 'DOUBLE', 'RIR': 'DOUBLE', 'Duration': 'VARCHAR', 'Distance short (m)': 'DOUBLE',
    'Distance long (km)': 'DOUBLE',
    'Source': 'VARCHAR', 'Source Workout': 'VARCHAR', 'Source Exercise': 'VARCHAR',
    'Exercise Match': 'VARCHAR', 'RIR Basis': 'VARCHAR', 'Date Confidence': 'VARCHAR',
    'Date Earliest': 'DATE', 'Date Latest': 'DATE', 'Program': 'VARCHAR',
    'Program Week': 'VARCHAR', 'Source Ref': 'VARCHAR'
  }
);

CREATE OR REPLACE MACRO lifting_history_sets(path) AS TABLE
SELECT
  "Date"                      AS date,
  "Workout Duration"          AS workout_duration_s,
  "Workout"                   AS workout,
  "Exercise"                  AS exercise,
  "Exercise Base Weight (kg)" AS exercise_base_weight_kg,
  "Set Type"                  AS set_type,
  "Weight (kg)"               AS weight_kg,
  "Reps"                      AS reps,
  "RIR"                       AS rir,
  "Duration"                  AS duration,
  "Distance short (m)"        AS distance_short_m,
  "Distance long (km)"        AS distance_long_km,
  CAST(NULL AS VARCHAR)       AS superset,
  CAST(row_number() OVER (PARTITION BY "Date", "Workout", "Exercise" ORDER BY file_row) AS INTEGER)
                              AS set_number,
  "Set Type" = 'Warm-Up Set'  AS is_warmup,
  "Source"                    AS source,
  "Source Workout"            AS source_workout,
  "Source Exercise"           AS source_exercise,
  "Exercise Match"            AS exercise_match,
  "RIR Basis"                 AS rir_basis,
  "Date Confidence"           AS date_confidence,
  "Date Earliest"             AS date_earliest,
  "Date Latest"               AS date_latest,
  "Program"                   AS program,
  "Program Week"              AS program_week,
  "Source Ref"                AS source_ref
FROM lifting_history_csv(path)
ORDER BY file_row;

CREATE OR REPLACE TABLE drive_templates_workout_sets AS
SELECT * FROM lifting_history_sets({{drive_templates_csv}});

CREATE OR REPLACE TABLE strong_workout_sets AS
SELECT * FROM lifting_history_sets({{strong_csv}});

DROP MACRO TABLE lifting_history_sets;
DROP MACRO TABLE lifting_history_csv;
