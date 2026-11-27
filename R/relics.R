library(dplyr)

# The Reliquary: one relic per gate of the year, sealed or unlocked by a rule
# in config/oath.yml (`relics:`), plus a level counter.
#
# Pure functions, no Shiny: the config, tables already in the database and
# the manual log (config/relic_events.csv) go in, one row per relic comes out
# (`relic_status()`), written to the database by `write_relic_tables()`.
#
# `data` is a list of the tables the rules read:
#   weight  (date, trend_kg)                       weight_daily: trend weight, a model output
#   weekly  (week, trend_change, goal_change, in_plan, met, complete)   weight_weekly
#   sets    (date, exercise, reps, rir, is_warmup) lifting_sets
#   rides   (date, km, elev_m)                     strava_rides: all ride types
#   climbs  (date, climb, status, bike_id)         strava_climb_efforts: our own matching
#   events  (date, relic, note)                    config/relic_events.csv: the manual log
#
# A relic's unlock date is the first day its rule held in the data. Once
# unlocked it stays unlocked (trend weight going back up doesn't seal it):
# the status is recomputed from the data on every build, and the first day
# the rule held doesn't move.
#
# Rule types (each relic's `rule:`):
#   weight_reached  trend <= kg, with either the mean weekly loss >= adherence
#                   x rate_kg_week (weeks with a planned loss, from `since`)
#                   or the share of weeks kept >= kept_share (a week is kept at
#                   week_kept_at x its planned loss, else weight_weekly$met)
#   lift_gate       blocks ticked by hand ({label, done}) plus a set of `reps`
#                   at RIR in `rir` on `exercise`, from `since`
#   ride            one ride of >= km and >= gain_m (in `year`, if given)
#   km_year         ride km in `year` >= km
#   climb_clean     a clean ascent of `climb` (on `bike`, from `since`)
#   quest_stages    the stages of a quest on the board (`quest:`, e.g. the
#                   Ledger): levels per stage done, unlocked when all are
#   manual          a row in relic_events.csv


# fmt_num(), fmt_day(): R/common.R

# The manual log; an empty one when the file doesn't exist yet
read_relic_events <- function(path = "config/relic_events.csv") {
  empty <- tibble::tibble(date = as.Date(character()), relic = character(), note = character())
  if (!file.exists(path)) return(empty)
  e <- readr::read_csv(path, col_types = readr::cols(date = "D", relic = "c", note = "c"))
  if (nrow(e) == 0) empty else e
}

# One rule's outcome on as_of: the unlock date (or NA), stages done (for the
# ones that earn levels per stage) and a hint for a sealed card
relic_rule <- function(rule, data, as_of, cfg, id) {
  as_of <- as.Date(as_of)
  out <- function(date, hint = "", stages_done = NA_integer_, stages = NA_integer_) {
    list(date = as.Date(date), hint = hint, stages_done = stages_done, stages = stages)
  }
  since <- as.Date(rule$since %||% "1900-01-01")
  in_year <- function(d) is.null(rule$year) | as.integer(format(as.Date(d), "%Y")) == as.integer(rule$year %||% 0)
  switch(rule$type,
    weight_reached = {
      w <- data$weight %>%
        filter(as.Date(date) >= since, as.Date(date) <= as_of, !is.na(trend_kg)) %>%
        arrange(date)
      now <- if (nrow(w)) dplyr::last(w$trend_kg) else NA_real_
      # Weeks that count: complete, in the plan, ended by the day checked
      weeks <- data$weekly %>%
        filter(in_plan, complete, as.Date(week) >= since, !is.na(trend_change)) %>%
        mutate(week_end = as.Date(week) + 6)
      if (!is.null(rule$adherence)) weeks <- weeks %>% filter(goal_change < 0)  # pause weeks left out
      kept <- function(d) {
        wk <- weeks %>% filter(week_end <= d)
        if (nrow(wk) == 0) return(NA_real_)
        if (!is.null(rule$adherence)) return(mean(-wk$trend_change) / rule$rate_kg_week)
        # A week is kept when its loss reaches week_kept_at x the planned one
        # (0.8: 80% counts as met); without it, weight_weekly$met (the full plan)
        if (is.null(rule$week_kept_at)) mean(wk$met) else
          mean(-wk$trend_change >= rule$week_kept_at * -wk$goal_change - 1e-9)
      }
      bar <- rule$adherence %||% rule$kept_share %||% 0
      what <- if (!is.null(rule$adherence)) "of the planned loss" else "of weeks kept"
      below <- w %>% filter(trend_kg <= rule$kg + 1e-9)
      hit <- NA
      for (d in as.list(below$date)) {
        k <- kept(d)
        if (!is.na(k) && k >= bar - 1e-9) { hit <- d; break }
      }
      k_now <- kept(as_of)
      out(hit, sprintf("Trend %s → %s kg%s", if (is.na(now)) "–" else fmt_num(now, 1),
                       fmt_num(rule$kg, if (rule$kg == round(rule$kg)) 0 else 1),
                       if (is.na(k_now)) "" else sprintf(" · %s%% %s (needs %s%%)",
                                                         round(100 * k_now), what, round(100 * bar))))
    },
    lift_gate = {
      blocks <- rule$blocks %||% list()
      done <- vapply(blocks, function(b) {
        d <- as.Date(b$done %||% NA)
        if (!is.na(d) && d <= as_of) as.numeric(d) else NA_real_
      }, numeric(1))
      rir <- unlist(rule$rir %||% list(0, 10))
      s <- data$sets %>%
        filter(exercise == rule$exercise, reps == rule$reps, !is.na(rir),
               rir >= min(.env$rir), rir <= max(.env$rir),
               as.Date(date) >= since, as.Date(date) <= as_of)
      if ("is_warmup" %in% names(s)) s <- s %>% filter(!is_warmup %in% TRUE)
      triple <- if (nrow(s)) min(as.Date(s$date)) else as.Date(NA)
      all_done <- length(blocks) == 0 || all(!is.na(done))
      date <- if (all_done && !is.na(triple)) max(c(triple, as.Date(done[!is.na(done)]))) else NA
      out(date, sprintf("Blocks %d of %d · heavy %s: %s", sum(!is.na(done)), length(blocks),
                        if (rule$reps == 3) "triple" else paste(rule$reps, "reps"),
                        if (is.na(triple)) "not yet" else fmt_day(triple)))
    },
    ride = {
      r <- data$rides %>%
        filter(as.Date(date) >= since, as.Date(date) <= as_of, in_year(date)) %>%
        arrange(date)
      hit <- r %>% filter(km >= rule$km, elev_m >= rule$gain_m)
      best <- if (nrow(r)) r %>% arrange(desc(km)) %>% slice(1) else NULL
      out(if (nrow(hit)) hit$date[1] else NA,
          if (is.null(best)) "No ride yet" else
            sprintf("Longest so far: %s km / %s m", fmt_num(best$km), fmt_num(best$elev_m)))
    },
    km_year = {
      r <- data$rides %>%
        filter(as.integer(format(as.Date(date), "%Y")) == as.integer(rule$year), as.Date(date) <= as_of) %>%
        arrange(date) %>%
        mutate(cum = cumsum(coalesce(km, 0)))
      hit <- r %>% filter(cum >= rule$km)
      out(if (nrow(hit)) hit$date[1] else NA,
          sprintf("%s of %s km in %s", fmt_num(if (nrow(r)) max(r$cum) else 0), fmt_num(rule$km), rule$year))
    },
    climb_clean = {
      e <- data$climbs %>%
        filter(climb == rule$climb, status == "clean", as.Date(date) >= since, as.Date(date) <= as_of)
      if (!is.null(rule$bike)) e <- e %>% filter(bike_id == rule$bike)
      out(if (nrow(e)) min(as.Date(e$date)) else NA, "Not yet ridden clean")
    },
    quest_stages = {
      q <- Filter(function(q) identical(q$id, rule$quest), cfg$quests %||% list())
      if (!length(q)) stop("Relic ", id, ": no quest '", rule$quest, "' in oath.yml")
      st <- q[[1]]$stages %||% list()
      done <- vapply(st, function(s) {
        d <- as.Date(s$done %||% NA)
        if (!is.na(d) && d <= as_of) as.numeric(d) else NA_real_
      }, numeric(1))
      n_done <- sum(!is.na(done))
      out(if (length(st) && n_done == length(st)) as.Date(max(done)) else NA,
          sprintf("%d of %d seals", n_done, length(st)), stages_done = n_done, stages = length(st))
    },
    manual = {
      e <- data$events %>% filter(relic == id, as.Date(date) <= as_of)
      out(if (nrow(e)) min(as.Date(e$date)) else NA, "Logged by hand when it happens")
    },
    stop("Unknown relic rule type '", rule$type, "' for relic ", id)
  )
}

# One row per relic, in config order. levels: the levels earned on as_of
# (all of the relic's levels once unlocked, or one per stage done for a
# quest_stages rule with levels_per_stage).
relic_status <- function(cfg, data, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  relics <- cfg$relics %||% list()
  colours <- cfg$relic_colours %||% character(0)
  rows <- lapply(seq_along(relics), function(i) {
    r <- relics[[i]]
    res <- relic_rule(r$rule %||% list(type = "manual"), data, as_of, cfg, r$id)
    unlocked <- !is.na(res$date)
    levels_max <- as.integer(r$levels %||% 0L)
    per_stage <- isTRUE(r$rule$levels_per_stage)
    levels <- if (per_stage && !is.na(res$stages_done)) min(levels_max, res$stages_done) else
      if (unlocked) levels_max else 0L
    tibble::tibble(
      ord = i, id = r$id, name_pt = r$name_pt %||% "", name_en = r$name_en %||% "",
      plain = r$plain %||% r$name_en %||% r$id,
      unlock = r$unlock %||% "", palette = r$palette %||% "", level_text = r$level_text %||% "",
      colour = r$colour %||% (if (length(colours)) colours[[(i - 1) %% length(colours) + 1]] else "#4B2A6B"),   # oath.yml relic_colours normally sets them
      rule_type = r$rule$type %||% "manual",
      unlocked = unlocked, date = as.Date(res$date),
      levels = as.integer(levels), levels_max = levels_max,
      hint = if (unlocked) sprintf("Unlocked %s", fmt_day(res$date)) else res$hint,
      as_of = as_of
    )
  })
  dplyr::bind_rows(rows)
}

# The level on as_of: the base plus every level earned
relic_level <- function(status, cfg) as.integer(cfg$level_base %||% 1L) + sum(status$levels %||% 0L)

# The tables relic_status() reads, from the database (plus the manual log)
relic_data <- function(con, events = read_relic_events()) {
  get <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
  list(
    weight = get("SELECT date, trend_kg FROM weight_daily"),
    weekly = get("SELECT week, trend_change, goal_change, in_plan, met, complete FROM weight_weekly"),
    sets   = get("SELECT date, exercise, reps, rir, is_warmup FROM lifting_sets"),
    rides  = get("SELECT date, km, elev_m FROM strava_rides"),
    climbs = get("SELECT date, climb, status, bike_id FROM strava_climb_efforts"),
    events = events
  )
}

# The status, plus the manual log itself (the Scenarios tab reruns the
# quests live and needs it; the app reads only the database)
write_relic_tables <- function(status, events = NULL, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "relic_status", status, overwrite = TRUE)
  if (!is.null(events)) DBI::dbWriteTable(con, "relic_events", events, overwrite = TRUE)
  message("✓ relic table written (", sum(status$unlocked), " of ", nrow(status), " unlocked)")
  invisible(TRUE)
}
