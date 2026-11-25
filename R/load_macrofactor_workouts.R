library(readr)
library(dplyr)
library(janitor)
library(duckdb)
library(stringr)

# MacroFactor workout export (CSV): one row per logged set.
load_macrofactor_workouts <- function(
    path,
    bodyweight = NULL,  # data frame: date, bodyweight_kg (for assisted lifts)
    db_path = "db/health.db"
) {

  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # MacroFactor writes a UTF-8 byte order mark that has been encoded twice,
  # so the first header arrives as `ï»¿"Date"` instead of `Date`. Take the
  # header, repair the first name, and read the data with the repaired names.
  col_names <- names(read_csv(path, n_max = 0, show_col_types = FALSE))
  col_names[1] <- "Date"

  workout_df <- read_csv(
    path,
    skip      = 1,
    col_names = col_names,
    col_types = cols(
      Date       = col_date(),
      Workout    = col_character(),
      Exercise   = col_character(),
      `Set Type` = col_character(),
      # Empty in all exports so far, so its format (seconds or "m:ss") is
      # unknown. Kept as text until a timed set shows what it looks like.
      Duration   = col_character(),
      .default   = col_double()
    )
  ) |>
    clean_names() |>
    rename(workout_duration_s = workout_duration) |>
    mutate(
      across(c(workout, exercise), fix_double_utf8),
      # Superset members get " ∈ SS1" (U+2208) appended. Split it off so a
      # lift keeps one name whether or not it was supersetted. Any single
      # symbol is accepted as the separator in case MacroFactor changes it.
      superset = str_match(exercise, "\\s+\\S\\s+(SS\\d+)$")[, 2],
      exercise = str_remove(exercise, "\\s+\\S\\s+SS\\d+$")
    ) |>
    mutate(
      set_number = row_number(),
      .by = c(date, workout, exercise)
    ) |>
    mutate(is_warmup = set_type == "Warm-Up Set") |>
    join_bodyweight(bodyweight) |>
    add_set_e1rm()  # load_kg, reps_to_failure, e1rm_kg (R/e1rm.R)

  DBI::dbWriteTable(con, "macrofactor_workout_sets", workout_df, overwrite = TRUE)
  message("✓ macrofactor_workout_sets written (", nrow(workout_df), " rows)")

  invisible(workout_df)
}

# MacroFactor's workout CSV is UTF-8 that was encoded to UTF-8 a second time,
# so "°" arrives as "Â°" and "—" as "â\u0080\u0094". Undo one layer where
# that gives valid UTF-8; strings that were fine (plain ASCII) stay as they are.
fix_double_utf8 <- function(x) {
  fixed <- iconv(x, from = "UTF-8", to = "latin1")
  Encoding(fixed) <- "UTF-8"
  ok <- !is.na(fixed) & validUTF8(fixed)
  x[ok] <- fixed[ok]
  x
}

# Adds bodyweight_kg: the most recent weight on or before each set's date, if
# it is at most max_age_days old. `bodyweight` has columns date and
# bodyweight_kg; NULL gives an NA column.
join_bodyweight <- function(sets, bodyweight, max_age_days = Inf) {
  if (is.null(bodyweight)) {
    return(mutate(sets, bodyweight_kg = NA_real_))
  }
  bw <- bodyweight |>
    filter(!is.na(bodyweight_kg)) |>
    select(bw_date = date, bodyweight_kg)

  sets |>
    left_join(bw, by = join_by(closest(date >= bw_date))) |>
    mutate(bodyweight_kg = if_else(as.numeric(date - bw_date) <= max_age_days, bodyweight_kg, NA_real_)) |>
    select(-bw_date)
}
