library(dplyr)
library(duckdb)

# Fitbit breathing rate during sleep, one row per night, dated by the wake-up
# day (see docs/sources.md).
#
# Reads Fitbit's daily file. The per-sleep summaries include naps and are
# timestamped in UTC, so averaging them by UTC date mixed naps and nights.
load_rr <- function(
    dir = "data/landing/fitbit/Takeout/Fitbit/Physical Activity_GoogleData",
    db_path = "db/health.db"
) {
  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  path <- file.path(dir, "daily_respiratory_rate.csv")
  if (!file.exists(path)) return(invisible(NULL))

  daily_data <- read_fitbit_daily(path) %>%
    select(date, respiratory_rate = breaths_per_minute) %>%
    arrange(date)

  DBI::dbWriteTable(con, "fitbit_rr_daily", daily_data, overwrite = TRUE)
  message("✓ fitbit_rr_daily written (", nrow(daily_data), " rows)")

  invisible(daily_data)
}
