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
#
# Overrides (config/lifting_overrides.csv) correct a logged value without
# touching the raw files: one row per set and field, with the value it must
# have now (`from`), so an override that no longer matches stops the build
# instead of silently changing another set.
build_lifting_sets <- function(macrofactor, backfills, bodyweight = NULL, overrides = NULL) {
  backfill <- bind_rows(backfills) %>%
    mutate(date = as.Date(date), date_earliest = as.Date(date_earliest),
           date_latest = as.Date(date_latest), set_number = as.integer(set_number),
           is_warmup = as.logical(is_warmup)) %>%
    join_bodyweight(bodyweight, max_age_days = BODYWEIGHT_MAX_AGE_DAYS)

  out <- bind_rows(
    macrofactor %>% mutate(source = "macrofactor", date = as.Date(date)),
    backfill
  ) %>%
    select(-any_of(c("e1rm_kg", "load_kg", "reps_to_failure", "is_assisted"))) %>%
    apply_lifting_overrides(overrides) %>%
    add_set_e1rm() %>%
    arrange(date, source, workout, exercise, set_number)

  dupes <- count(out, date, workout, exercise, set_number) %>% filter(n > 1)
  if (nrow(dupes) > 0) {
    stop("lifting_sets: ", nrow(dupes), " sets appear twice under (date, workout, exercise, set_number), ",
         "first on ", format(min(dupes$date)))
  }
  out
}

LIFTING_OVERRIDE_FIELDS <- c("rir", "reps", "weight_kg")

apply_lifting_overrides <- function(sets, overrides = NULL) {
  sets$override_note <- NA_character_
  if (is.null(overrides) || nrow(overrides) == 0) return(sets)
  bad <- setdiff(overrides$field, LIFTING_OVERRIDE_FIELDS)
  if (length(bad) > 0) stop("lifting overrides: field not allowed: ", paste(bad, collapse = ", "))
  for (i in seq_len(nrow(overrides))) {
    o <- overrides[i, ]
    hit <- which(sets$source == o$source & sets$date == as.Date(o$date) &
                   sets$exercise == o$exercise & sets$set_number == as.integer(o$set_number))
    label <- sprintf("%s %s %s set %s", o$source, o$date, o$exercise, o$set_number)
    if (length(hit) != 1) stop("lifting overrides: ", label, " matches ", length(hit), " sets, not 1")
    now <- sets[[o$field]][hit]
    if (!isTRUE(all.equal(now, as.numeric(o$from)))) {
      stop("lifting overrides: ", label, ": ", o$field, " is ", now, ", not ", o$from, " as the override expects")
    }
    sets[[o$field]][hit] <- as.numeric(o$to)
    sets$override_note[hit] <- o$note
  }
  sets
}

read_lifting_overrides <- function(path) {
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, col_types = readr::cols(date = "D", set_number = "i", from = "d", to = "d", .default = "c"))
}

write_lifting_sets <- function(sets, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "lifting_sets", sets, overwrite = TRUE)
  message("✓ lifting_sets written (", nrow(sets), " sets: ",
          paste(sprintf("%s %d", names(table(sets$source)), table(sets$source)), collapse = ", "), ")")
  invisible(sets)
}
