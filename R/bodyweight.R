library(dplyr)

# bodyweight_for_ratios: one bodyweight series for strength ratios and
# assisted lifts, spanning more years than MacroFactor alone. Derived and
# labelled, never a silent splice: `basis` says where each value comes from.
#
#   macrofactor trend   from MacroFactor's first weigh-in on (the system of
#                       record; its trend weight is a model output)
#   fitbit scale        before that: Fitbit weigh-ins as logged (measurements,
#                       sparse)
#
# Rows are the days with a value; users join the latest row on or before a
# date, up to max_age_days old (BODYWEIGHT_MAX_AGE_DAYS), so a weigh-in isn't
# stretched across a year without one.
BODYWEIGHT_MAX_AGE_DAYS <- 45

build_bodyweight_for_ratios <- function(macrofactor_daily, fitbit_weight) {
  mf <- macrofactor_daily %>%
    transmute(date = as.Date(date), bodyweight_kg = trend_weight_kg) %>%
    filter(!is.na(bodyweight_kg))
  mf_start <- if (nrow(mf) > 0) min(mf$date) else as.Date(Inf)
  fb <- fitbit_weight %>%
    transmute(date = as.Date(date), bodyweight_kg = weight_kg) %>%
    filter(!is.na(bodyweight_kg), date < mf_start) %>%
    # Two weigh-ins on one day: their mean
    summarise(bodyweight_kg = mean(bodyweight_kg), .by = date)
  bind_rows(
    fb %>% mutate(basis = "fitbit scale"),
    mf %>% mutate(basis = "macrofactor trend")
  ) %>%
    arrange(date)
}

write_bodyweight_for_ratios <- function(bw, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "bodyweight_for_ratios", bw, overwrite = TRUE)
  message("✓ bodyweight_for_ratios written (", nrow(bw), " days, ",
          sum(bw$basis == "fitbit scale"), " from Fitbit)")
  invisible(bw)
}
