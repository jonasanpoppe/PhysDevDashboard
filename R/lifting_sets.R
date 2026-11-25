library(dplyr)

# lifting_sets: every logged set from all lifting sources, stacked.
#
#   macrofactor      the system of record from its first day on
#   strong           one-off backfill, the years before MacroFactor
#   drive_templates  one-off backfill, the years before Strong
#
# The backfills come from sql/staging/lifting_history_sets.sql, already in
# MacroFactor's layout and with MacroFactor's exercise names. Here they get the
# same bodyweight join and the same e1RM rule as MacroFactor's sets (R/e1rm.R),
# so a lift is one series across sources. The sources never overlap in time;
# a set that appears twice under the same key stops the build.
build_lifting_sets <- function(macrofactor, backfills, bodyweight = NULL) {
  backfill <- bind_rows(backfills) %>%
    mutate(date = as.Date(date), date_earliest = as.Date(date_earliest),
           date_latest = as.Date(date_latest), set_number = as.integer(set_number),
           is_warmup = as.logical(is_warmup)) %>%
    join_bodyweight(bodyweight) %>%
    add_set_e1rm()

  out <- bind_rows(
    macrofactor %>% mutate(source = "macrofactor", date = as.Date(date)),
    backfill
  ) %>%
    arrange(date, source, workout, exercise, set_number)

  dupes <- count(out, date, workout, exercise, set_number) %>% filter(n > 1)
  if (nrow(dupes) > 0) {
    stop("lifting_sets: ", nrow(dupes), " sets appear twice under (date, workout, exercise, set_number), ",
         "first on ", format(min(dupes$date)))
  }
  out
}

write_lifting_sets <- function(sets, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "lifting_sets", sets, overwrite = TRUE)
  message("✓ lifting_sets written (", nrow(sets), " sets: ",
          paste(sprintf("%s %d", names(table(sets$source)), table(sets$source)), collapse = ", "), ")")
  invisible(sets)
}
