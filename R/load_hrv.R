library(dplyr)
library(duckdb)

# Fitbit nightly HRV (RMSSD during sleep), one row per night, dated by the
# wake-up day (see docs/sources.md).
#
# Reads Fitbit's daily file, not the 5-minute samples: averaging samples by
# their UTC date mixed the end of one night with the start of the next.
load_hrv <- function(
    dir = "data/landing/fitbit/Takeout/Fitbit/Physical Activity_GoogleData",
    db_path = "db/health.db"
) {
  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  path <- file.path(dir, "daily_heart_rate_variability.csv")
  if (!file.exists(path)) return(invisible(NULL))

  daily_data <- read_fitbit_daily(path) %>%
    select(date, hrv_rmssd = average_heart_rate_variability_milliseconds) %>%
    # Fitbit writes 0 when a night was recorded but HRV could not be computed
    # (the same rows have a normal non-REM heart rate). 0 means missing.
    mutate(hrv_rmssd = if_else(hrv_rmssd == 0, NA_real_, hrv_rmssd)) %>%
    arrange(date)

  DBI::dbWriteTable(con, "fitbit_hrv_daily", daily_data, overwrite = TRUE)
  message("✓ fitbit_hrv_daily written (", nrow(daily_data), " rows)")

  invisible(daily_data)
}
