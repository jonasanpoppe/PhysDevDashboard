library(targets)
library(tarchetypes)

# Load all functions from R/
tar_source("R/")

# Pipeline options
tar_option_set(
  packages = c("readr", "readxl", "jsonlite", "duckdb", "dplyr", "tidyr", "purrr", "janitor", "stringr")
)

list(

  # ── Source file tracking ─────────────────────────────────────────────────────
  # Targets will re-run downstream steps if these files change
  # MacroFactor: newest export of each kind (summary xlsx, food log csv,
  # workout csv). Re-checks the folder every run so a new export is picked up.
  tar_target(
    macrofactor_file,
    find_macrofactor_exports("data/landing/macrofactor")$summary,
    format = "file",
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    macrofactor_food_log_file,
    find_macrofactor_exports("data/landing/macrofactor")$food_log,
    format = "file",
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    macrofactor_workouts_file,
    find_macrofactor_exports("data/landing/macrofactor")$workout_log,
    format = "file",
    cue = tar_cue(mode = "always")
  ),
  tar_target(azm_dir, "data/landing/fitbit/Takeout/Fitbit/Active Zone Minutes (AZM)", format = "file"),
  tar_target(physical_activity_dir, "data/landing/fitbit/Takeout/Fitbit/Physical Activity_GoogleData", format = "file"),
  tar_target(readiness_dir, "data/landing/fitbit/Takeout/Fitbit/Daily Readiness", format = "file"),
  tar_target(sleep_score_dir, "data/landing/fitbit/Takeout/Fitbit/Health Fitness Data_GoogleData", format = "file"),
  tar_target(stress_score_dir, "data/landing/fitbit/Takeout/Fitbit/Stress Score", format = "file"),
  tar_target(fitbit_sleep_dir, "data/landing/fitbit/Takeout/Fitbit/Global Export Data", format = "file"),
  tar_target(garmin_api_dir, "data/landing/garmin", format = "file"),
  # Strava: summaries are one file per fetch, so re-list them every run
  tar_target(
    strava_activity_files,
    list.files("data/landing/strava", "_strava_activities\\.json$", full.names = TRUE),
    format = "file",
    cue = tar_cue(mode = "always")
  ),
  tar_target(strava_details_dir, "data/landing/strava/details", format = "file"),
  tar_target(strava_segments_dir, "data/landing/strava/segments", format = "file"),
  # One branch per stream file: only new activities are processed on a rebuild
  tar_target(
    strava_stream_paths,
    list.files("data/landing/strava/streams", "\\.json$", full.names = TRUE),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    strava_stream_files,
    strava_stream_paths,
    pattern = map(strava_stream_paths),
    format = "file"
  ),
  # Personal settings (HR zones); gitignored, see config/athlete.example.yml
  tar_target(athlete_config_file, "config/athlete.yml", format = "file"),
  tar_target(athlete_config, yaml::read_yaml(athlete_config_file)),
  # Climb library and bikes (gitignored; see the *.example.csv twins)
  tar_target(climbs_file, "config/climbs.csv", format = "file"),
  tar_target(climbs_listed, read_climbs(climbs_file)),
  tar_target(bikes_file, "config/bikes.csv", format = "file"),
  tar_target(bike_overrides_file, "config/bike_overrides.csv", format = "file"),
  tar_target(effort_overrides_file, "config/effort_overrides.csv", format = "file"),
  tar_target(goals_config_file, "config/goals.yml", format = "file"),
  tar_target(goals_config, yaml::read_yaml(goals_config_file)),

  # ── Load raw data ────────────────────────────────────────────────────────────
  tar_target(
    macrofactor_raw,
    load_macrofactor(
      path    = macrofactor_file,
      db_path = "db/health.db"
    )
  ),
  tar_target(
    macrofactor_food_log,
    load_macrofactor_food_log(
      path    = macrofactor_food_log_file,
      db_path = "db/health.db"
    )
  ),
  tar_target(
    macrofactor_workout_sets,
    load_macrofactor_workouts(
      path       = macrofactor_workouts_file,
      bodyweight = dplyr::select(macrofactor_daily, date, bodyweight_kg = trend_weight_kg),
      db_path    = "db/health.db"
    )
  ),
  tar_target(
    macrofactor_e1rm_sessions,
    load_macrofactor_e1rm_sessions(
      sets    = macrofactor_workout_sets,
      db_path = "db/health.db"
    )
  ),
  tar_target(
    azm_daily,
    load_azm(azm_dir = azm_dir, db_path = "db/health.db")
  ),
  tar_target(
    steps_daily,
    load_steps(dir = physical_activity_dir, db_path = "db/health.db")
  ),
  tar_target(
    distance_daily,
    load_distance(dir = physical_activity_dir, db_path = "db/health.db")
  ),
  tar_target(
    calories_daily,
    load_calories(dir = physical_activity_dir, db_path = "db/health.db")
  ),
  tar_target(
    hrv_daily,
    load_hrv(dir = physical_activity_dir, db_path = "db/health.db")
  ),
  tar_target(
    rr_daily,
    load_rr(dir = physical_activity_dir, db_path = "db/health.db")
  ),
  tar_target(
    spo2_daily,
    load_spo2(dir = physical_activity_dir, db_path = "db/health.db")
  ),
  tar_target(
    temperature_daily,
    load_temperature(dir = physical_activity_dir, db_path = "db/health.db")
  ),
  tar_target(
    readiness_daily,
    load_readiness(dir = readiness_dir, db_path = "db/health.db")
  ),
  tar_target(
    sleep_score_daily,
    load_sleep_score(dir = sleep_score_dir, db_path = "db/health.db")
  ),
  tar_target(
    fitbit_sleep_daily,
    load_fitbit_sleep(dir = fitbit_sleep_dir, db_path = "db/health.db")
  ),
  tar_target(
    stress_score_daily,
    load_stress_score(dir = stress_score_dir, db_path = "db/health.db")
  ),
  tar_target(
    rhr_daily,
    load_rhr(dir = physical_activity_dir, db_path = "db/health.db")
  ),
  tar_target(
    garmin_api_daily,
    load_garmin_api(dir = garmin_api_dir, db_path = "db/health.db")
  ),
  tar_target(
    strava_raw,
    load_strava_api(
      activity_files = strava_activity_files,
      details_dir    = strava_details_dir,
      segments_dir   = strava_segments_dir,
      db_path        = "db/health.db"
    )
  ),
  tar_target(
    strava_hr_histogram_branches,
    strava_hr_histogram(strava_stream_files),
    pattern = map(strava_stream_files)
  ),
  tar_target(
    strava_hr_zones,
    load_strava_hr_zones(
      histogram  = strava_hr_histogram_branches,
      activities = strava_activities,
      zones      = athlete_config$hr_zones,
      db_path    = "db/health.db"
    )
  ),

  # ── Analysis: climbs ─────────────────────────────────────────────────────────
  # One branch per stream file; all rerun when the climb list or the matching
  # settings change (about half a minute for all rides).
  tar_target(
    strava_climb_effort_branches,
    climb_efforts(
      read_strava_stream(strava_stream_files),
      climbs,
      activity_id = sub("\\.json$", "", basename(strava_stream_files)),
      settings    = athlete_config$climbs
    ),
    pattern = map(strava_stream_files)
  ),
  tar_target(
    strava_climb_efforts,
    build_strava_climb_efforts(
      efforts          = strava_climb_effort_branches,
      activities       = strava_activities,
      bikes            = read_config_csv(bikes_file),
      bike_overrides   = read_config_csv(bike_overrides_file),
      effort_overrides = read_config_csv(effort_overrides_file),
      climbs           = climbs,
      weights          = rider_weight,
      power            = athlete_config$power,
      scope_min_gain_m = athlete_config$climbs$scope_min_gain_m,
      db_path          = "db/health.db"
    )
  ),

  # The climb list plus each segment's rise (for the power model)
  tar_target(climbs, climb_rise(climbs_listed, strava_segments)),

  # Rider weight for the power model: MacroFactor trend weight per day
  tar_target(rider_weight, rider_weight_table(macrofactor_daily, db_path = "db/health.db")),

  # Climb list and bike names for the app (it reads only the database)
  tar_target(
    climb_config_tables,
    write_climb_config_tables(
      climbs  = climbs,
      bikes   = read_config_csv(bikes_file),
      pinned   = unlist(goals_config$pinned_climbs),
      labelled = unlist(goals_config$engine_labels),
      db_path = "db/health.db"
    )
  ),

  # ── Marts ────────────────────────────────────────────────────────────────────
  tar_target(
    vitals_daily,
    build_vitals_daily(
      fitbit_rhr   = rhr_daily,
      fitbit_hrv   = hrv_daily,
      fitbit_rr    = rr_daily,
      fitbit_spo2  = spo2_daily,
      fitbit_temp  = temperature_daily,
      fitbit_sleep = fitbit_sleep_daily,
      garmin       = garmin_api_daily,
      db_path      = "db/health.db"
    )
  ),

  # ── Clean ───────────────────────────────────────────────────────────────────
  tar_target(
    macrofactor_daily,
    macrofactor_raw$daily
  ),
  tar_target(
    macrofactor_exercise,
    macrofactor_raw$exercise
  ),
  tar_target(strava_activities, strava_raw$activities),
  tar_target(strava_laps, strava_raw$laps),
  tar_target(strava_segment_efforts, strava_raw$segment_efforts),
  tar_target(strava_segments, strava_raw$segments),
  tar_target(strava_best_efforts, strava_raw$best_efforts),

  # ── Join ─────────────────────────────────────────────────────────────────────
  # Placeholder: extend as new daily-grain sources are added
  tar_target(
    daily,
    macrofactor_daily %>%
      full_join(azm_daily, by = "date") %>%
      full_join(steps_daily, by = "date") %>%
      full_join(distance_daily, by = "date") %>%
      full_join(calories_daily, by = "date") %>%
      full_join(hrv_daily, by = "date") %>%
      full_join(rr_daily, by = "date") %>%
      full_join(spo2_daily, by = "date") %>%
      full_join(temperature_daily, by = "date") %>%
      full_join(readiness_daily, by = "date") %>%
      full_join(sleep_score_daily, by = "date") %>%
      full_join(stress_score_daily, by = "date") %>%
      full_join(rhr_daily, by = "date") %>%
      full_join(garmin_api_daily, by = "date")
  )

)



