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
  # goals.yml holds the plan's numbers (the single source, R/plan_config.R);
  # resolve_goals() adds what they imply (the forecast's e1RM targets)
  tar_target(goals_config, resolve_goals(yaml::read_yaml(goals_config_file), lifts)),
  tar_target(lifts_file, "config/lifts.csv", format = "file"),
  tar_target(lifts, readr::read_csv(lifts_file, show_col_types = FALSE)),

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
  tar_target(sql_fitbit_azm_daily, "sql/staging/fitbit_azm_daily.sql", format = "file"),
  tar_target(
    azm_daily,
    run_sql(sql_fitbit_azm_daily, params = list(fitbit_azm_dir = azm_dir),
            read_back = "fitbit_azm_daily", db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_steps_daily, "sql/staging/fitbit_steps_daily.sql", format = "file"),
  tar_target(
    steps_daily,
    run_sql(sql_fitbit_steps_daily, params = list(fitbit_activity_dir = physical_activity_dir),
            read_back = "fitbit_steps_daily", db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_distance_daily, "sql/staging/fitbit_distance_daily.sql", format = "file"),
  tar_target(
    distance_daily,
    run_sql(sql_fitbit_distance_daily, params = list(fitbit_activity_dir = physical_activity_dir),
            read_back = "fitbit_distance_daily", db_path = "db/health.db")
  ),
  # SQL layer (sql/, run by R/run_sql.R). The landing files and the .sql file
  # are tracked, so a new export or an edited query reruns the step.
  tar_target(
    fitbit_calories_files,
    list.files(physical_activity_dir, "^calories_[0-9].*\\.csv$", full.names = TRUE),
    format = "file"
  ),
  tar_target(sql_fitbit_calories, "sql/staging/fitbit_calories_daily.sql", format = "file"),
  tar_target(
    calories_daily,
    {
      fitbit_calories_files
      run_sql(sql_fitbit_calories, params = list(fitbit_activity_dir = physical_activity_dir),
              read_back = "fitbit_calories_daily", db_path = "db/health.db") %>%
        mutate(date = as.Date(date))
    }
  ),
  # Lifting history before MacroFactor (one-off backfills in MacroFactor's
  # layout), stacked with MacroFactor's sets into lifting_sets.
  tar_target(drive_templates_workouts_file, "data/landing/backfill/drive_templates_workout_log.csv", format = "file"),
  tar_target(strong_workouts_file, "data/landing/backfill/strong_workout_log.csv", format = "file"),
  tar_target(sql_lifting_history, "sql/staging/lifting_history_sets.sql", format = "file"),
  tar_target(lifting_overrides_file, "config/lifting_overrides.csv", format = "file"),
  tar_target(
    lifting_history_sets,
    run_sql(sql_lifting_history,
            params = list(drive_templates_csv = drive_templates_workouts_file, strong_csv = strong_workouts_file),
            read_back = c("drive_templates_workout_sets", "strong_workout_sets"), db_path = "db/health.db")
  ),
  # Fitbit weigh-ins, and one labelled bodyweight series for ratios:
  # MacroFactor trend from its first day, Fitbit weigh-ins before (R/bodyweight.R)
  tar_target(fitbit_weight_file, file.path(physical_activity_dir, "weight.csv"), format = "file"),
  tar_target(sql_fitbit_weight, "sql/staging/fitbit_weight.sql", format = "file"),
  tar_target(
    fitbit_weight,
    {
      fitbit_weight_file
      run_sql(sql_fitbit_weight,
              params = list(fitbit_activity_dir = physical_activity_dir, home_timezone = athlete_config$home_timezone),
              read_back = "fitbit_weight", db_path = "db/health.db") %>%
        mutate(date = as.Date(date))
    }
  ),
  tar_target(
    bodyweight_for_ratios,
    build_bodyweight_for_ratios(macrofactor_daily, fitbit_weight) %>%
      write_bodyweight_for_ratios(db_path = "db/health.db")
  ),
  tar_target(
    lifting_sets,
    build_lifting_sets(
      macrofactor_workout_sets, lifting_history_sets,
      bodyweight = bodyweight_for_ratios,
      overrides  = read_lifting_overrides(lifting_overrides_file)
    ) %>%
      write_lifting_sets(db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_hrv_daily, "sql/staging/fitbit_hrv_daily.sql", format = "file"),
  tar_target(
    hrv_daily,
    run_sql(sql_fitbit_hrv_daily, params = list(fitbit_activity_dir = physical_activity_dir),
            read_back = "fitbit_hrv_daily", db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_rr_daily, "sql/staging/fitbit_rr_daily.sql", format = "file"),
  tar_target(
    rr_daily,
    run_sql(sql_fitbit_rr_daily, params = list(fitbit_activity_dir = physical_activity_dir),
            read_back = "fitbit_rr_daily", db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_spo2_daily, "sql/staging/fitbit_spo2_daily.sql", format = "file"),
  tar_target(
    spo2_daily,
    run_sql(sql_fitbit_spo2_daily, params = list(fitbit_activity_dir = physical_activity_dir),
            read_back = "fitbit_spo2_daily", db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_temperature_nightly, "sql/staging/fitbit_temperature_nightly.sql", format = "file"),
  tar_target(
    temperature_daily,
    run_sql(sql_fitbit_temperature_nightly, params = list(fitbit_activity_dir = physical_activity_dir),
            read_back = "fitbit_temperature_nightly", db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_readiness_daily, "sql/staging/fitbit_readiness_daily.sql", format = "file"),
  tar_target(
    readiness_daily,
    run_sql(sql_fitbit_readiness_daily, params = list(fitbit_readiness_dir = readiness_dir),
            read_back = "fitbit_readiness_daily", db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_sleep_score, "sql/staging/fitbit_sleep_score.sql", format = "file"),
  tar_target(
    sleep_score_daily,
    run_sql(sql_fitbit_sleep_score, params = list(fitbit_sleep_score_dir = sleep_score_dir),
            read_back = "fitbit_sleep_score", db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_sleep_daily, "sql/staging/fitbit_sleep_daily.sql", format = "file"),
  tar_target(
    fitbit_sleep_daily,
    run_sql(sql_fitbit_sleep_daily, params = list(fitbit_sleep_dir = fitbit_sleep_dir),
            read_back = "fitbit_sleep_daily", db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_stress_score, "sql/staging/fitbit_stress_score.sql", format = "file"),
  tar_target(
    stress_score_daily,
    run_sql(sql_fitbit_stress_score, params = list(fitbit_stress_dir = stress_score_dir),
            read_back = "fitbit_stress_score", db_path = "db/health.db")
  ),
  tar_target(sql_fitbit_rhr_daily, "sql/staging/fitbit_rhr_daily.sql", format = "file"),
  tar_target(
    rhr_daily,
    run_sql(sql_fitbit_rhr_daily, params = list(fitbit_activity_dir = physical_activity_dir),
            read_back = "fitbit_rhr_daily", db_path = "db/health.db")
  ),
  tar_target(sql_garmin_api_daily, "sql/staging/garmin_api_daily.sql", format = "file"),
  tar_target(
    garmin_api_daily,
    run_sql(sql_garmin_api_daily, params = list(garmin_dir = garmin_api_dir),
            read_back = "garmin_api_daily", db_path = "db/health.db")
  ),
  # Strava: five tables from the list fetches, details and segments
  tar_target(sql_strava_activities, "sql/staging/strava_activities.sql", format = "file"),
  tar_target(
    strava_raw,
    {
      # Tracked, so a new fetch reruns it (the SQL reads the folder)
      strava_activity_files
      strava_segments_dir
      tables <- c("activities", "laps", "segment_efforts", "segments", "best_efforts")
      run_sql(sql_strava_activities, params = list(strava_dir = dirname(strava_details_dir)),
              read_back = paste0("strava_", tables), db_path = "db/health.db") %>%
        stats::setNames(tables)
    }
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

  # ── Analysis: fitness ────────────────────────────────────────────────────────
  tar_target(strava_daily_load, daily_load(strava_activities)),
  tar_target(
    fitness_input,
    fitness_efforts(strava_climb_efforts, strava_activities,
                    eras = athlete_config$hr_device_eras, settings = athlete_config$fitness)
  ),
  tar_target(fitness_model, fit_efficiency(fitness_input, athlete_config$fitness)),
  tar_target(fitness_check, load_check(fitness_model, strava_daily_load)),
  tar_target(
    fitness_tables,
    write_fitness_tables(fitness_model, strava_daily_load, fitness_check, db_path = "db/health.db")
  ),

  # ── Weight against the plan (goals in config/goals.yml) ───────────────────────
  tar_target(weight_daily, build_weight_daily(macrofactor_daily, goals_config$weight_goal)),
  tar_target(sessions_daily, build_sessions(strava_activities, lifting_sets, goals_config$sessions)),
  tar_target(weight_weekly, build_weight_weekly(weight_daily, sessions_daily, goals_config$weight_goal, device_steps)),
  tar_target(
    weight_status,
    # Sessions counted to today and judged at adherence_met, the rate over
    # the Scenarios' recent weeks: the same numbers as Home and Scenarios
    compute_weight_status(weight_daily, sessions_daily, goals_config$weight_goal,
                          goals_config$nutrition, goals_config$sessions, as_of = Sys.Date(),
                          adherence = oath_config$adherence_met %||% 0.8,
                          recent_weeks = scenario_defaults(oath_config)$recent_weeks),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    weight_tables,
    write_weight_tables(weight_daily, weight_weekly, sessions_daily, weight_status,
                        goal = goals_config$weight_goal, db_path = "db/health.db")
  ),

  # ── Strength (lift groups in config/lifts.csv, targets in goals.yml) ─────────
  tar_target(
    strength_sessions,
    build_strength_sessions(lifting_sets, lifts, bodyweight_for_ratios,
                            mf_exercise = macrofactor_exercise, settings = athlete_config$strength,
                            max_bw_age_days = BODYWEIGHT_MAX_AGE_DAYS)
  ),
  tar_target(strength_goals, strength_goal_tables(lifts, goals_config$strength, strength_forecast$params, goals = goals_config)),
  # Forecast: plateau curves per main lift (R/forecast.R; config in goals.yml)
  tar_target(
    strength_forecast,
    build_strength_forecast(strength_sessions, lifting_sets, goals_config$strength_forecast,
                            goals_config$strength_ceilings) %>%
      write_strength_forecast(db_path = "db/health.db")
  ),
  tar_target(strength_tables, write_strength_tables(strength_sessions, strength_goals, db_path = "db/health.db")),

  # ── Riding (targets in goals.yml, plan bands in athlete.yml) ──────────────────
  tar_target(strava_rides, build_rides(strava_activities)),
  tar_target(strava_plan_bands,
             build_plan_bands(strava_hr_histogram_branches, strava_rides, athlete_config$plan_bands,
                              strava_activities, athlete_config$hr_device_eras)),
  tar_target(riding_tables,
             write_riding_tables(strava_rides, strava_plan_bands, goals_config$riding,
                                 bands = athlete_config$plan_bands, db_path = "db/health.db")),

  # ── Quests: the Quest Board on the Home tab (config/oath.yml, R/quests.R) ────
  # Always rerun: the board counts days up to today (last 28 days, the
  # year's pace), so it goes stale even when no data changes.
  tar_target(oath_config_file, "config/oath.yml", format = "file"),
  # oath.yml with its numbers filled from goals.yml; stops on a conflict
  tar_target(oath_config, oath_from_config(yaml::read_yaml(oath_config_file), goals_config, lifts)),
  tar_target(
    quest_tables,
    write_quest_tables(
      quest_board(oath_config, as_of = Sys.Date(), data = list(
        weight = weight_daily, strength = strength_sessions, rides = strava_rides,
        sessions = sessions_daily, climbs = strava_climb_efforts, events = relic_events
      )),
      oath_config, db_path = "db/health.db"
    ),
    cue = tar_cue(mode = "always")
  ),

  # ── Reliquary: relics unlocked by rules in oath.yml (R/relics.R) ────────────
  # The manual log is optional (no file: nothing logged yet), so it is read
  # on every run rather than tracked as a file target.
  tar_target(relic_events, read_relic_events("config/relic_events.csv"), cue = tar_cue(mode = "always")),
  tar_target(
    relics,
    relic_status(oath_config, as_of = Sys.Date(), data = list(
      weight = weight_daily, weekly = weight_weekly, sets = lifting_sets,
      rides = strava_rides, climbs = strava_climb_efforts, events = relic_events
    )),
    cue = tar_cue(mode = "always")
  ),
  tar_target(relic_tables, write_relic_tables(relics, relic_events, db_path = "db/health.db")),
  # Scenarios: a verdict per quest with the default assumptions, for the
  # Home chips (R/scenarios.R); the Scenarios tab reruns them live
  tar_target(
    scenario_tables,
    write_scenario_tables(
      quest_scenarios(oath_config, as_of = Sys.Date(), data = list(
        weight = weight_daily, strength = strength_sessions, rides = strava_rides,
        sessions = sessions_daily, climbs = strava_climb_efforts, events = relic_events,
        weekly = weight_weekly, goal = goals_config$weight_goal,
        forecast = list(params = strength_forecast$params, curve = strength_forecast$curve)
      )),
      db_path = "db/health.db"
    ),
    cue = tar_cue(mode = "always")
  ),
  # Roadmap: phases against today, gates ticked by the relics (R/roadmap.R)
  tar_target(
    roadmap_tables,
    write_roadmap_tables(roadmap_phases(oath_config, as_of = Sys.Date()),
                         roadmap_gates(oath_config, relics), db_path = "db/health.db"),
    cue = tar_cue(mode = "always")
  ),

  # ── Consistency: numbers two tabs show must agree (R/consistency.R) ─────────
  # After every table it reads is written; stops the build on a mismatch.
  tar_target(
    view_checks,
    {
      list(quest_tables, scenario_tables, relic_tables, roadmap_tables, weight_tables, strength_tables,
           strength_forecast, riding_tables)
      assert_views("db/health.db")
    },
    cue = tar_cue(mode = "always")
  ),

  # ── Analytics: any two daily variables (R/analytics.R) ──────────────────────
  tar_target(
    analytics_daily,
    build_analytics_daily(weight_daily, vitals_daily, strava_daily_load, sessions_daily,
                          strava_plan_bands, strength_sessions, strava_climb_efforts,
                          vitals_homogenised = vitals_homogenised, device_steps = device_steps,
                          strength_fc = goals_config$strength_forecast)
  ),
  # Fitbit era drawn onto Garmin's scale: a derived series beside the two
  # originals, with the step and its interval in vitals_era_steps (R/homogenise.R)
  # Steps walked too (device_steps, metric "steps")
  tar_target(era_series_long, era_series(vitals_daily, device_steps)),
  tar_target(vitals_era_steps, estimate_era_steps(era_series_long)),
  tar_target(vitals_homogenised, homogenise_vitals(era_series_long, vitals_era_steps)),
  tar_target(homogenisation_tables, write_homogenisation_tables(vitals_era_steps, db_path = "db/health.db")),
  tar_target(
    analytics_tables,
    write_analytics_tables(
      analytics_daily,
      e1rm_groups = e1rm_names(strength_sessions, goals_config$strength_forecast),
      db_path = "db/health.db"
    )
  ),

  # ── Marts ────────────────────────────────────────────────────────────────────
  # Data checks on the source tables (sql/checks/): a missing column or a
  # duplicate key stops the build, implausible values warn
  tar_target(sql_checks, c("sql/checks/expected_columns.sql", "sql/checks/data_checks.sql"), format = "file"),
  tar_target(
    data_checks,
    {
      sql_checks
      # Runs after every table it checks
      list(rhr_daily, hrv_daily, rr_daily, spo2_daily, temperature_daily, fitbit_sleep_daily,
           steps_daily, distance_daily, azm_daily, calories_daily, readiness_daily,
           sleep_score_daily, stress_score_daily, fitbit_weight, garmin_api_daily,
           strava_raw, macrofactor_raw, macrofactor_food_log, macrofactor_workout_sets)
      run_data_checks(db_path = "db/health.db")
    }
  ),
  # Steps from the wrist device, Fitbit and Garmin eras (not MacroFactor's)
  tar_target(sql_device_steps, "sql/marts/device_steps.sql", format = "file"),
  tar_target(
    device_steps,
    {
      data_checks
      run_sql(sql_device_steps, read_back = "device_steps", db_path = "db/health.db")
    }
  ),
  tar_target(sql_vitals_daily, "sql/marts/vitals_daily.sql", format = "file"),
  tar_target(
    vitals_daily,
    {
      data_checks
      run_sql(sql_vitals_daily, read_back = "vitals_daily", db_path = "db/health.db")
    }
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



