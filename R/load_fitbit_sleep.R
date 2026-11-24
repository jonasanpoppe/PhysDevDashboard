library(jsonlite)
library(dplyr)
library(purrr)
library(duckdb)

# Fitbit sleep logs (Takeout "Global Export Data/sleep-*.json"): the main sleep
# of each night, dated by Fitbit's dateOfSleep, which is the wake-up day.
# Durations are in seconds, with the same names as the Garmin columns.
#
# - The monthly files overlap, so logs are deduplicated by logId.
# - Naps (mainSleep = false) are left out.
# - "classic" logs (older or short sleeps) have no stages: asleep and awake
#   time only, stage columns NA.
# - Start and end are local wall time without offset, as Fitbit writes them.
load_fitbit_sleep <- function(
    dir = "data/landing/fitbit/Takeout/Fitbit/Global Export Data",
    db_path = "db/health.db"
) {
  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  json_files <- list.files(dir, pattern = "^sleep-.*\\.json$", full.names = TRUE)
  if (length(json_files) == 0) return(invisible(NULL))

  logs <- json_files |>
    map(\(f) fromJSON(f, simplifyVector = FALSE)) |>
    list_flatten()

  sleep_data <- logs |>
    map(fitbit_sleep_row) |>
    list_rbind() |>
    distinct(log_id, .keep_all = TRUE) |>
    filter(main_sleep) |>
    select(-main_sleep) |>
    arrange(date)

  if (anyDuplicated(sleep_data$date)) {
    stop("Fitbit sleep: more than one main sleep on ",
         paste(unique(sleep_data$date[duplicated(sleep_data$date)]), collapse = ", "))
  }

  DBI::dbWriteTable(con, "fitbit_sleep_daily", sleep_data, overwrite = TRUE)
  message("✓ fitbit_sleep_daily written (", nrow(sleep_data), " rows)")

  invisible(sleep_data)
}

# One sleep log (a list from the JSON) -> one row.
fitbit_sleep_row <- function(log) {
  stage_s <- function(stage) {
    m <- log$levels$summary[[stage]]$minutes
    if (log$type == "stages" && !is.null(m)) m * 60 else NA_real_
  }
  tibble(
    date              = as.Date(log$dateOfSleep),
    log_id            = as.character(log$logId),
    main_sleep        = isTRUE(log$mainSleep),
    log_type          = log$type,
    sleep_start_local = sub("\\.000$", "", log$startTime),
    sleep_end_local   = sub("\\.000$", "", log$endTime),
    sleep_s           = log$minutesAsleep * 60,
    sleep_awake_s     = log$minutesAwake * 60,
    sleep_deep_s      = stage_s("deep"),
    sleep_light_s     = stage_s("light"),
    sleep_rem_s       = stage_s("rem"),
    sleep_efficiency  = as.numeric(log$efficiency)
  )
}
