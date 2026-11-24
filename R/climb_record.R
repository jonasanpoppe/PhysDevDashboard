library(dplyr)
library(duckdb)

# The Climb record: per climb, how often, how clean, the best effort and the
# history of attempts. Built from strava_climb_efforts plus the `climbs` and
# `bikes` tables (write_climb_config_tables() puts those in the database).
#
# A clean ascent is status clean or top: a stop only at the very top still
# counts (top_frac in config/athlete.yml). The best effort is the shortest
# moving time among clean ascents.

clean_statuses <- c("clean", "top")

# Config tables the app needs: the climb list (with surface and pin flag) and
# bike names. Written by the pipeline; the app only reads the database.
write_climb_config_tables <- function(climbs, bikes, pinned = character(),
                                      db_path = "db/health.db") {
  climbs_tbl <- climbs %>%
    transmute(
      name, segment_id, length_m, gain_m,
      avg_grade_pct = round(100 * gain_m / length_m, 1),
      surface = if ("surface" %in% names(.)) na_if(surface, "") else NA_character_,
      reviewed = if ("reviewed" %in% names(.)) as.logical(reviewed) else NA,
      pinned = name %in% pinned
    )
  unknown <- setdiff(pinned, climbs_tbl$name)
  if (length(unknown) > 0) {
    warning("pinned_climbs: not in climbs.csv: ", paste(unknown, collapse = ", "))
  }
  bikes_tbl <- bikes %>%
    distinct(bike_id, .keep_all = TRUE) %>%
    transmute(bike_id, bike, nickname = na_if(nickname, ""))

  con <- DBI::dbConnect(duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "climbs", climbs_tbl, overwrite = TRUE)
  DBI::dbWriteTable(con, "bikes", bikes_tbl, overwrite = TRUE)
  invisible(list(climbs = climbs_tbl, bikes = bikes_tbl))
}

# One row per climb with at least one effort after the filters, pinned first,
# then most ascents. `surfaces`: values of climbs$surface, "unknown" for NA.
climb_record <- function(efforts, climbs, bikes,
                         surfaces = NULL, bike_ids = NULL, scope_only = FALSE) {
  e <- efforts %>%
    mutate(date = as.Date(date)) %>%
    inner_join(climbs %>% select(name, surface, length_m, gain_m, avg_grade_pct, pinned),
               by = c(climb = "name")) %>%
    mutate(surface = coalesce(surface, "unknown"))
  if (!is.null(surfaces)) e <- filter(e, surface %in% surfaces)
  if (!is.null(bike_ids)) e <- filter(e, bike_id %in% bike_ids)
  if (scope_only) e <- filter(e, in_record_scope)

  bike_label <- bikes %>% transmute(bike_id, bike_label = coalesce(nickname, bike))

  e %>%
    left_join(bike_label, by = "bike_id") %>%
    arrange(date, start_local) %>%
    summarise(
      .by = c(climb, surface, length_m, gain_m, avg_grade_pct, pinned),
      ascents      = n(),
      clean        = sum(status %in% clean_statuses),
      best_s       = if (any(status %in% clean_statuses)) min(moving_s[status %in% clean_statuses]) else NA_real_,
      best_date    = if (any(status %in% clean_statuses)) date[status %in% clean_statuses][which.min(moving_s[status %in% clean_statuses])] else as.Date(NA),
      best_bike    = if (any(status %in% clean_statuses)) bike_label[status %in% clean_statuses][which.min(moving_s[status %in% clean_statuses])] else NA_character_,
      last_date    = last(date),
      last_status  = last(status),
      history      = list(tibble(date = date, status = status, moving_s = moving_s, stop_s = stop_s))
    ) %>%
    arrange(desc(pinned), desc(ascents), climb)
}

# 215 -> "3:35"
fmt_mmss <- function(s) {
  r <- as.integer(round(s))
  ifelse(is.na(r), NA_character_, sprintf("%d:%02d", r %/% 60L, r %% 60L))
}
