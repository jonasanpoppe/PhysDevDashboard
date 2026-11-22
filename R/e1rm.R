#' Wathen (1994) estimated one-rep max
#'
#' e1RM = 100 * load / (48.8 + 53.8 * exp(-0.075 * r)), with r = reps + RIR
#' (reps to failure). Vectorised over all arguments.
#'
#' Note: Wathen does not return the load itself at r = 1 (it gives ~1.3% more),
#' so a logged single at RIR 0 is reported slightly above the bar weight.
#'
#' @param load Bar weight in kg (machine base weight already added).
#' @param reps Reps performed.
#' @param rir  Reps in reserve as logged.
#' @return Estimated 1RM in kg; NA where any input is NA.
e1rm_wathen <- function(load, reps, rir = 0) {
  stopifnot(is.numeric(load), is.numeric(reps), is.numeric(rir))
  r <- reps + rir
  100 * load / (48.8 + 53.8 * exp(-0.075 * r))
}

# Set types that count as working sets for e1RM. Warm-Up, Myo and Mini-sets
# are left out.
E1RM_SET_TYPES <- c("Standard Set", "Failure Set")

#' Add set-level load and Wathen e1RM to the cleaned MacroFactor set log
#'
#' Same set selection as the dashboard's e1rm.py: working sets only
#' (Standard or Failure), RIR logged and <= `max_rir`, reps >= 1. Machine
#' base weight is added to the load. Sets that fail the selection keep their
#' row but get `e1rm_kg = NA`.
#'
#' Assisted lifts (MacroFactor logs the assistance as a negative weight) are
#' loaded as bodyweight minus assistance, so they need `bodyweight_kg`;
#' without it their e1RM is NA.
#'
#' @param sets Set log after `clean_names()` (columns: exercise, set_type,
#'   weight_kg, exercise_base_weight_kg, reps, rir, and optionally
#'   bodyweight_kg).
#' @param max_rir Highest RIR still trusted.
#' @return `sets` with `is_assisted`, `load_kg`, `reps_to_failure` and
#'   `e1rm_kg` added.
add_set_e1rm <- function(sets, max_rir = 3) {
  if (!"bodyweight_kg" %in% names(sets)) {
    sets$bodyweight_kg <- NA_real_
  }
  sets |>
    dplyr::mutate(
      is_assisted = grepl("Assisted", exercise, fixed = TRUE),
      load_kg = dplyr::if_else(
        is_assisted,
        bodyweight_kg - abs(weight_kg),
        weight_kg + dplyr::coalesce(exercise_base_weight_kg, 0)
      ),
      reps_to_failure = reps + rir,
      e1rm_kg = dplyr::if_else(
        set_type %in% E1RM_SET_TYPES &
          !is.na(rir) & rir <= max_rir &
          !is.na(reps) & reps >= 1 &
          !is.na(load_kg) & load_kg > 0,
        e1rm_wathen(load_kg, reps, rir),
        NA_real_
      )
    )
}

#' Best Wathen e1RM per lift and session
#'
#' @param sets Output of `add_set_e1rm()` (the `macrofactor_workout_sets`
#'   table).
#' @return One row per lift and date with the best set and its e1RM.
session_best_wathen <- function(sets) {
  sets |>
    dplyr::filter(!is.na(e1rm_kg)) |>
    dplyr::slice_max(e1rm_kg, n = 1, with_ties = FALSE, by = c(exercise, date)) |>
    dplyr::transmute(
      date,
      lift = exercise,
      top_set = sprintf("%g x %d @ %g RIR", load_kg, as.integer(reps), rir),
      reps_to_failure,
      e1rm = round(e1rm_kg, 1)
    ) |>
    dplyr::arrange(lift, date)
}

#' Compute session-best e1RM and write it to DuckDB
#'
#' @param sets The `macrofactor_workout_sets` target.
#' @return The session-best table, invisibly.
load_macrofactor_e1rm_sessions <- function(sets, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  best <- session_best_wathen(sets)
  DBI::dbWriteTable(con, "macrofactor_e1rm_sessions", best, overwrite = TRUE)
  message("✓ macrofactor_e1rm_sessions written (", nrow(best), " rows)")

  invisible(best)
}
