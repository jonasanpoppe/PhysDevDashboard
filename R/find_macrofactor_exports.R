library(stringr)
library(purrr)

# MacroFactor gives every export the same name pattern,
# MacroFactor-<YYYYMMDDhhmmss>.<ext>, whatever it contains. There are three
# kinds:
#   summary     .xlsx  daily totals, weight, expenditure, exercise summaries
#   food_log    .csv   one row per logged food item
#   workout_log .csv   one row per logged set
# The two CSVs can only be told apart by their header line.

macrofactor_export_type <- function(path) {
  if (tolower(tools::file_ext(path)) == "xlsx") {
    return("summary")
  }

  # Only the header is needed. Matching on column names rather than on the
  # first column also sidesteps the mangled BOM at the start of the workout
  # export (see load_macrofactor_workouts()).
  header <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")

  has <- function(col) str_detect(header, fixed(col))

  if (has("Food Name") && has("Serving Size")) {
    "food_log"
  } else if (has("Exercise") && has("Set Type") && has("Reps")) {
    "workout_log"
  } else {
    "unknown"
  }
}

# Returns a named list with the newest export of each type in `dir`:
# list(summary = ..., food_log = ..., workout_log = ...).
# "Newest" is taken from the timestamp in the file name, not the file's
# modification time, so copying or restoring files does not change the result.
find_macrofactor_exports <- function(
    dir = "data/landing/macrofactor",
    types = c("summary", "food_log", "workout_log")
) {
  files <- list.files(
    dir,
    pattern    = "MacroFactor-\\d{14}\\.(csv|xlsx)$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop("No MacroFactor exports found in: ", dir)
  }

  exports <- tibble::tibble(
    path      = files,
    stamp     = str_extract(basename(files), "\\d{14}"),
    type      = map_chr(files, macrofactor_export_type)
  )

  unknown <- exports$path[exports$type == "unknown"]
  if (length(unknown) > 0) {
    warning(
      "Ignoring MacroFactor files of unrecognised type: ",
      paste(basename(unknown), collapse = ", ")
    )
  }

  newest <- types |>
    set_names() |>
    map(function(t) {
      of_type <- exports[exports$type == t, ]
      if (nrow(of_type) == 0) {
        stop("No MacroFactor ", t, " export found in: ", dir)
      }
      of_type$path[which.max(as.numeric(of_type$stamp))]
    })

  newest
}
