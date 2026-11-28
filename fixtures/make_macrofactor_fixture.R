# Synthetic MacroFactor export: the real sheet and column structure
# (fixtures/macrofactor_schema.R) with invented values, for CI and
# `just build-fixture`. Never real data.
#
# Writes three files into `dir`, named like real exports:
#   MacroFactor-<stamp>.xlsx   summary: daily sheets and exercise sheets
#   MacroFactor-<stamp>.csv    food log: one row per food item
#   MacroFactor-<stamp>.csv    workout log: one row per set, with MacroFactor's
#                              double-encoded UTF-8 (BOM, "°", superset mark)
#
# Dates run from `start` for `days` days and match the invented goals in
# config/goals.example.yml (start 6 Jan 2030).
#
# `trend` (kg, one per day) and `sets` (Date, Workout, Exercise, Set Type,
# Weight (kg), Reps, RIR) replace the invented defaults; the demo database
# (demo/make_demo_landing.R) passes its own.
#
# Needs fixtures/macrofactor_schema.R sourced first.

make_macrofactor_fixture <- function(dir, start = as.Date("2029-11-01"), days = 150, seed = 1,
                                     trend = NULL, sets = NULL) {
  set.seed(seed)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dates <- start + seq_len(days) - 1
  n <- length(dates)
  end <- max(dates)
  stamp <- function(min) format(as.POSIXct(paste(end, "12:00:00"), tz = "UTC") + 60 * min, "%Y%m%d%H%M%S")

  # Invented person: 82 kg, flat until the plan starts, then ~0.4 kg a week
  goal_start <- as.Date("2030-01-06")
  if (is.null(trend)) trend <- 82 - pmax(0, as.numeric(dates - goal_start)) * 0.4 / 7 + sin(seq_len(n) / 9) * 0.15
  weighed <- runif(n) < 0.6
  scale <- ifelse(weighed, round(trend + rnorm(n, 0, 0.5), 1), NA)
  kcal <- round(rnorm(n, 2300, 250))
  protein <- round(rnorm(n, 140, 20))
  fat <- round(kcal * 0.3 / 9); carbs <- round((kcal - protein * 4 - fat * 9) / 4)
  na_sheet <- function(cols) stats::setNames(as.data.frame(matrix(NA, n, length(cols))), cols)

  # ── Summary workbook ──────────────────────────────────────────────────────
  micro <- na_sheet(macrofactor_schema$micronutrients)
  micro[] <- lapply(micro, \(x) round(runif(n, 0, 50), 1))
  lift_days <- dates[as.integer(format(dates, "%u")) %in% c(1, 4)]
  exercises <- c("Barbell Back Squat", "Barbell Bench Press", "Barbell Romanian Deadlift", "45° Leg Press")
  exercise_sheet <- function(unit, value) {
    out <- data.frame(Date = lift_days)
    for (e in exercises) out[[paste0(e, " (", unit, ")")]] <- round(value(length(lift_days)))
    out
  }
  muscle_sheet <- function(unit) {
    out <- data.frame(Date = lift_days)
    for (m in macrofactor_schema$muscle_groups) out[[paste0(m, " (", unit, ")")]] <- round(runif(length(lift_days), 0, 10))
    out
  }
  sheets <- list(
    "Calories & Macros" = data.frame(Date = dates, "Calories (kcal)" = kcal, "Fat (g)" = fat,
                                     "Carbs (g)" = carbs, "Protein (g)" = protein, check.names = FALSE),
    "Micronutrients" = cbind(data.frame(Date = dates), micro),
    "Scale Weight" = data.frame(Date = dates, "Weight (kg)" = scale, "Fat Percent" = NA_real_, check.names = FALSE),
    "Body Metrics" = cbind(data.frame(Date = dates), na_sheet(macrofactor_schema$body_metrics)),
    "Weight Trend" = data.frame(Date = dates, "Trend Weight (kg)" = round(trend, 2), check.names = FALSE),
    "Expenditure" = data.frame(Date = dates, Expenditure = round(rnorm(n, 2750, 60))),
    "Steps" = data.frame(Date = dates, Steps = round(rnorm(n, 9000, 2000))),
    "Muscle Groups - Sets" = muscle_sheet("sets"),
    "Muscle Groups - Volume" = muscle_sheet("kg"),
    "Exercises - 1-RM" = exercise_sheet("kg", \(k) runif(k, 60, 140)),
    "Exercises - 3-RM" = exercise_sheet("kg", \(k) runif(k, 55, 130)),
    "Exercises - 10-RM" = exercise_sheet("kg", \(k) runif(k, 40, 100)),
    "Exercises - Total Volume" = exercise_sheet("kg", \(k) runif(k, 1000, 4000)),
    "Exercises - Best Set Volume" = exercise_sheet("kg", \(k) runif(k, 300, 900)),
    "Exercises - Heaviest Weight" = exercise_sheet("kg", \(k) runif(k, 50, 120)),
    "Exercises - Total Reps" = exercise_sheet("reps", \(k) runif(k, 15, 40)),
    "Exercises - Best Set Reps" = exercise_sheet("reps", \(k) runif(k, 5, 12)),
    "Exercises - Total Sets" = exercise_sheet("sets", \(k) runif(k, 3, 5))
  )
  xlsx <- file.path(dir, paste0("MacroFactor-", stamp(0), ".xlsx"))
  writexl::write_xlsx(sheets, xlsx)

  # ── Food log ──────────────────────────────────────────────────────────────
  food_cols <- macrofactor_schema$food_log
  items <- do.call(rbind, lapply(seq_len(n), \(i) data.frame(Date = dates[i], k = 1:3)))
  food <- stats::setNames(as.data.frame(matrix(0, nrow(items), length(food_cols))), food_cols)
  food$Date <- format(items$Date)
  food$Time <- c("8:00 AM", "1:00 PM", "7:30 PM")[items$k]
  food$`Food Name` <- c("Oats", "Rice and chicken", "Yoghurt")[items$k]
  food$`Serving Size` <- c("100 g", "1 bowl", "250 g")[items$k]
  food$`Calories (kcal)` <- round(rep(kcal, each = 3) / 3)
  food$`Protein (g)` <- round(rep(protein, each = 3) / 3)
  food_csv <- file.path(dir, paste0("MacroFactor-", stamp(1), ".csv"))
  utils::write.csv(food, food_csv, row.names = FALSE, na = "")

  # ── Workout log: MacroFactor's double-encoded UTF-8 ───────────────────────
  double_encode <- function(x) iconv(x, from = "latin1", to = "UTF-8")   # the UTF-8 bytes read as latin1
  if (is.null(sets)) sets <- do.call(rbind, lapply(lift_days, \(d) data.frame(
    Date = format(d), `Workout Duration` = 3600, Workout = "Full Body",
    Exercise = rep(c(exercises[1:3], paste(exercises[4], "∈ SS1")), each = 3),
    `Exercise Base Weight (kg)` = "", `Set Type` = rep(c("Warm-Up Set", "Standard Set", "Standard Set"), 4),
    `Weight (kg)` = round(runif(12, 40, 120)), Reps = sample(5:10, 12, TRUE), RIR = sample(1:3, 12, TRUE),
    Duration = "", `Distance short (m)` = "", `Distance long (km)` = "", check.names = FALSE
  )))
  sets <- data.frame(Date = format(as.Date(sets$Date)), `Workout Duration` = 3600, Workout = sets$Workout,
                     Exercise = sets$Exercise, `Exercise Base Weight (kg)` = "", `Set Type` = sets$`Set Type`,
                     `Weight (kg)` = sets$`Weight (kg)`, Reps = sets$Reps, RIR = sets$RIR, Duration = "",
                     `Distance short (m)` = "", `Distance long (km)` = "", check.names = FALSE)
  sets$Exercise <- double_encode(sets$Exercise)
  header <- c(paste0("\"", double_encode("﻿"), "\"\"Date\"\"\""), macrofactor_schema$workout_log[-1])
  # paste() per column: apply() would pad numbers with spaces
  lines <- c(paste(header, collapse = ","), do.call(paste, c(sets, sep = ",")))
  workout_csv <- file.path(dir, paste0("MacroFactor-", stamp(2), ".csv"))
  writeLines(enc2utf8(lines), workout_csv, useBytes = TRUE)

  invisible(c(summary = xlsx, food_log = food_csv, workout_log = workout_csv))
}
