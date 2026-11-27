library(dplyr)

# Weight against the plan: the goal pace line, daily and weekly tables, and
# the sessions (BJJ, lifting) per day. Goals: weight_goal, nutrition and
# sessions in config/goals.yml.
#
# Scale weight and intake are measurements; trend weight and expenditure are
# MacroFactor model outputs (recalculated after edits), shown as such.

# The pace from (from_date, from_kg): down rate_kg_per_week, flat for
# pause_weeks once at pause_at_kg (if from_kg is still above it), flat at
# target_kg once there. Returns the planned kg on each of `dates` (NA before
# from_date).
pace_curve <- function(dates, from_date, from_kg, goal) {
  per_day <- goal$rate_kg_per_week / 7
  pause_days <- if (!is.null(goal$pause_at_kg) && from_kg > goal$pause_at_kg) goal$pause_weeks * 7 else 0
  pause_at <- if (pause_days > 0) goal$pause_at_kg else NA_real_
  t <- as.numeric(as.Date(dates) - as.Date(from_date))
  to_pause <- if (pause_days > 0) (from_kg - pause_at) / per_day else 0
  kg <- ifelse(
    t < 0, NA_real_,
    ifelse(pause_days > 0 & t <= to_pause, from_kg - per_day * t,
      ifelse(pause_days > 0 & t <= to_pause + pause_days, pause_at,
        (if (pause_days > 0) pause_at else from_kg) - per_day * (t - to_pause - pause_days)))
  )
  pmax(kg, goal$target_kg)
}

# The date the pace reaches target_kg.
pace_eta <- function(from_date, from_kg, goal) {
  per_day <- goal$rate_kg_per_week / 7
  if (from_kg <= goal$target_kg) return(as.Date(from_date))
  pause_days <- if (!is.null(goal$pause_at_kg) && from_kg > goal$pause_at_kg) goal$pause_weeks * 7 else 0
  # round() first: 0.7 / 7 is not exactly 0.1 in floating point, and ceiling()
  # would turn 50.0000000001 days into 51
  as.Date(from_date) + ceiling(round((from_kg - goal$target_kg) / per_day + pause_days, 6))
}

# Daily table: measurements, model outputs and the pace line. The last day
# of an export is usually unfinished, so its intake and steps are left out.
build_weight_daily <- function(macrofactor_daily, goal) {
  d <- macrofactor_daily %>%
    transmute(date = as.Date(date), scale_kg = weight_kg, trend_kg = trend_weight_kg,
              kcal = calories_kcal, protein_g, steps, expenditure_kcal = expenditure) %>%
    filter(!is.na(trend_kg)) %>%
    arrange(date)
  last <- max(d$date)
  d <- d %>% mutate(across(c(kcal, protein_g, steps), \(x) if_else(date == last, NA_real_, as.numeric(x))))
  start <- as.Date(goal$start_date)
  start_kg <- d$trend_kg[d$date == start]
  if (length(start_kg) == 0) stop("weight_goal: no trend weight on start_date ", start)
  d %>% mutate(pace_kg = pace_curve(date, start, start_kg, goal))
}

# Sessions per day and kind, never counted twice: at most one per day per
# kind. Lifting: MacroFactor workouts first (system of record for lifting
# sets), Strava for days MacroFactor has none. BJJ: Strava types only.
build_sessions <- function(activities, workout_sets, sessions_cfg) {
  strava <- function(kind) {
    types <- unlist(sessions_cfg[[kind]]$strava_types)
    activities %>% filter(sport_type %in% types) %>%
      transmute(date = as.Date(date), kind = kind, source = "strava")
  }
  mf <- if (isTRUE(sessions_cfg$lifting$macrofactor_workouts)) {
    workout_sets %>% transmute(date = as.Date(date), kind = "lifting", source = "macrofactor")
  } else NULL
  bind_rows(mf, strava("lifting"), strava("bjj")) %>%
    mutate(rank = if_else(source == "macrofactor", 1L, 2L)) %>%
    arrange(date, kind, rank) %>%
    distinct(date, kind, .keep_all = TRUE) %>%
    select(-rank)
}

# Weekly table (Monday to Sunday): change in trend weight against the planned
# change over the same days, intake and steps averaged over logged days,
# sessions counted. Weeks before the plan (`context_weeks` of them) are kept
# as context, without a goal. `complete` = the week has ended in the data.
build_weight_weekly <- function(daily, sessions, goal, context_weeks = 8) {
  start <- as.Date(goal$start_date)
  week_of <- function(d) d - (as.integer(format(d, "%u")) - 1L)
  last <- max(daily$date)
  ends <- daily %>%
    mutate(week = week_of(date)) %>%
    summarise(.by = week, trend_end = last(trend_kg), pace_end = last(pace_kg),
              last_day = max(date),
              kcal = mean(kcal, na.rm = TRUE), protein_g = mean(protein_g, na.rm = TRUE),
              steps = mean(steps, na.rm = TRUE)) %>%
    arrange(week) %>%
    mutate(trend_change = trend_end - lag(trend_end))
  # Planned change over the week: the pace line from the later of the
  # previous Sunday and the plan's start to this week's last day.
  start_kg <- daily$trend_kg[daily$date == start]
  pace_at <- function(d) pace_curve(d, start, start_kg, goal)
  ends <- ends %>%
    mutate(goal_change = if_else(last_day >= start,
                                 pace_at(last_day) - pace_at(pmax(week - 1, start)),
                                 NA_real_))
  per_week <- sessions %>%
    mutate(week = week_of(date)) %>%
    count(week, kind) %>%
    tidyr::pivot_wider(names_from = kind, values_from = n, values_fill = 0)
  for (k in c("bjj", "lifting")) if (!k %in% names(per_week)) per_week[[k]] <- 0L
  ends %>%
    filter(week >= week_of(start) - 7 * context_weeks) %>%
    left_join(per_week %>% select(week, bjj, lifting), by = "week") %>%
    mutate(across(c(bjj, lifting), \(x) coalesce(as.integer(x), 0L)),
           complete = last_day == week + 6 | week + 6 < last,
           in_plan = !is.na(goal_change),
           met = if_else(in_plan, trend_change <= goal_change + 1e-9, NA)) %>%
    mutate(across(c(kcal, protein_g, steps), \(x) if_else(is.nan(x), NA_real_, x))) %>%
    select(week, trend_end, trend_change, goal_change, in_plan, met, complete,
           kcal, protein_g, steps, bjj, lifting)
}

# The trend's weekly change over the last `weeks` up to its last day (a
# linear fit; negative = losing). Shared by the Weight tiles and the
# Scenarios' "recent pace", so the two never disagree.
trend_rate <- function(daily, weeks = 8) {
  d <- daily %>% filter(!is.na(trend_kg)) %>% mutate(date = as.Date(date))
  if (nrow(d) == 0) return(NA_real_)
  recent <- d %>% filter(date > max(date) - 7 * weeks)
  if (nrow(recent) < 14) return(NA_real_)
  unname(stats::coef(stats::lm(trend_kg ~ as.numeric(date), recent))[2]) * 7
}

# The numbers for the tiles. Weight values are as of the last day with a
# trend weight (`date`); sessions are counted up to `as_of` (the build day),
# as on Home. adherence: the share of a goal that counts as met (oath.yml
# adherence_met, 80%), as on Home and in the Scenarios; recent_weeks: the
# window of the rate (the Scenarios' recent pace).
compute_weight_status <- function(daily, sessions, goal, nutrition, sessions_cfg,
                                  as_of = max(daily$date), adherence = 1, recent_weeks = 4) {
  today <- max(daily$date)
  as_of <- max(as.Date(as_of), today)
  now <- daily %>% filter(date == today)
  last7 <- daily %>% filter(date > today - 8, date < today)
  # BJJ comes from Strava (fetched to today): counted to as_of, as on Home.
  # Lifting comes mostly from the MacroFactor export: counted to the export's
  # last day (`today`), or the days after it would look like missed sessions.

  tibble(
    date = today,
    as_of = as_of,
    adherence = adherence,
    recent_weeks = recent_weeks,
    rate_recent = trend_rate(daily, recent_weeks),
    goal_rate = -goal$rate_kg_per_week,
    goal_rate_met = -goal$rate_kg_per_week * adherence,
    trend_kg = now$trend_kg,
    left_kg = max(0, now$trend_kg - goal$target_kg),
    eta = pace_eta(today, now$trend_kg, goal),
    eta_met = pace_eta(today, now$trend_kg, utils::modifyList(goal, list(rate_kg_per_week = goal$rate_kg_per_week * adherence))),
    checkpoint = as.Date(goal$checkpoint),
    on_checkpoint = pace_curve(as.Date(goal$checkpoint), today, now$trend_kg, goal),
    pace_today = now$pace_kg,
    kcal_7d = mean(last7$kcal, na.rm = TRUE),
    protein_7d = mean(last7$protein_g, na.rm = TRUE),
    protein_goal = nutrition$protein_g_per_day,
    expenditure = now$expenditure_kcal,
    bjj_per_week = sessions_per_week(sessions, "bjj", as_of),
    bjj_goal = sessions_cfg$bjj$per_week,
    bjj_met_at = sessions_cfg$bjj$per_week * adherence,
    lifting_per_week = sessions_per_week(sessions, "lifting", today),
    lifting_as_of = today,
    lifting_goal = sessions_cfg$lifting$per_week,
    lifting_met_at = sessions_cfg$lifting$per_week * adherence
  )
}

write_weight_tables <- function(daily, weekly, sessions, status, goal,
                                db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "weight_daily", daily, overwrite = TRUE)
  DBI::dbWriteTable(con, "weight_weekly", weekly, overwrite = TRUE)
  DBI::dbWriteTable(con, "sessions_daily", sessions, overwrite = TRUE)
  DBI::dbWriteTable(con, "weight_status", status, overwrite = TRUE)
  # The goal itself, so the app can draw the pace line (it reads only the db)
  DBI::dbWriteTable(con, "weight_goal", tibble::as_tibble(lapply(goal, \(x) x[[1]])), overwrite = TRUE)
  message("✓ weight tables written (", nrow(weekly), " weeks)")
  invisible(TRUE)
}
