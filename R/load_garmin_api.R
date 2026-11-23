library(jsonlite)
library(dplyr)
library(purrr)
library(duckdb)

load_garmin_api <- function(
    dir     = "data/garmin_api",
    db_path = "db/health.db"
) {
  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  json_files <- list.files(
    dir,
    pattern    = "_garmin_api\\.json$",
    full.names = TRUE
  )

  if (length(json_files) == 0) return(invisible(NULL))

  # Every field the fetcher can write; files from ranges without some data (or
  # from older fetcher versions) lack some of them, so add those as NA.
  # Timestamps stay character here: *_utc are ISO 8601 UTC, *_local are local
  # wall time without offset.
  template <- tibble(
    date                      = character(),
    # Heart
    rhr                       = numeric(),
    hrv                       = numeric(),
    hrv_last_night            = numeric(),
    hrv_last_night_avg        = numeric(),
    hrv_last_night_5_min_high = numeric(),
    hrv_baseline_low          = numeric(),
    hrv_baseline_high         = numeric(),
    hrv_status                = character(),
    # Intensity minutes
    im_moderate               = numeric(),
    im_vigorous               = numeric(),
    im_week_goal              = numeric(),
    # Daily totals
    steps                     = numeric(),
    step_goal                 = numeric(),
    distance_m                = numeric(),
    floors_ascended           = numeric(),
    kcal_total                = numeric(),
    kcal_active               = numeric(),
    kcal_bmr                  = numeric(),
    # Whole-day SpO2 and breathing rate
    spo2_avg                  = numeric(),
    spo2_lowest               = numeric(),
    resp_waking_avg           = numeric(),
    resp_lowest               = numeric(),
    resp_highest              = numeric(),
    # Sleep (dated to the wake-up day)
    sleep_start_utc           = character(),
    sleep_end_utc             = character(),
    sleep_start_local         = character(),
    sleep_end_local           = character(),
    sleep_s                   = numeric(),
    sleep_deep_s              = numeric(),
    sleep_light_s             = numeric(),
    sleep_rem_s               = numeric(),
    sleep_awake_s             = numeric(),
    sleep_unmeasurable_s      = numeric(),
    nap_s                     = numeric(),
    sleep_awake_count         = numeric(),
    sleep_score               = numeric(),
    sleep_avg_hr              = numeric(),
    sleep_avg_stress          = numeric(),
    sleep_spo2_avg            = numeric(),
    sleep_spo2_lowest         = numeric(),
    sleep_resp_avg            = numeric(),
    sleep_resp_lowest         = numeric(),
    sleep_resp_highest        = numeric(),
    sleep_skin_temp_dev_c     = numeric(),
    sleep_restless_moments    = numeric(),
    # VO2max (Garmin model output, sparse)
    vo2max                    = numeric(),
    vo2max_cycling            = numeric()
  )

  daily_data <- json_files %>%
    # Keep only known columns so a new fetcher field can't silently change the
    # table before it's added to the template.
    map(\(f) bind_rows(template, as_tibble(fromJSON(f)))[names(template)]) %>%
    list_rbind() %>%
    mutate(date = as.Date(date)) %>%
    # Collapse duplicates in case of overlapping fetches — keep non-NA values
    summarise(.by = date, across(everything(), \(x) first(na.omit(x)))) %>%
    # Garmin's own total: vigorous minutes count double
    mutate(im_total = im_moderate + 2 * im_vigorous, .after = im_vigorous) %>%
    rename_with(\(x) paste0("ga_", x), -date) %>%
    arrange(date)

  DBI::dbWriteTable(con, "garmin_api_daily", daily_data, overwrite = TRUE)
  message("garmin_api_daily written (", nrow(daily_data), " rows)")

  invisible(daily_data)
}
