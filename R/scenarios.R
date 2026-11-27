library(dplyr)

# Scenarios: "am I on track?" per quest, under assumptions you can change.
# Decisions: private/scenarios_plan.md (26 Nov 2026).
#
# One projection per quest with a range, and a verdict from it:
#   on_track  the central projection reaches the target by the deadline
#   at_risk   only the optimistic end of the range does
#   behind    not even the optimistic end does
#   events:   done / due (deadline ahead) / overdue
# Bodyweight has a window: the central projection by window_start is on
# track, by the deadline at risk.
#
# Projections are model outputs, not measurements: every summary says what
# it assumes.
#
# Pure functions, no Shiny: the pipeline stores the verdicts with the default
# assumptions (quest_scenarios, for the Home chips); the Scenarios tab reruns
# them live from the same tables when a slider moves.
#
# `data` is quest_board()'s list plus:
#   weekly    (week, trend_change, in_plan, complete)    weight_weekly
#   goal      the weight goal (rate, pause_at_kg, pause_weeks, target_kg)
#   forecast  list(params, curve): strength_forecast_params / _curve
# `a` (assumptions): scenario_defaults(cfg) with any overrides.


scenario_verdict_labels <- c(on_track = "On track", at_risk = "At risk", behind = "Behind",
                             done = "Done", due = "Due", overdue = "Overdue", none = "No projection")

# The assumptions: oath.yml `scenarios:` over the built-in defaults
scenario_defaults <- function(cfg) {
  d <- list(rate_kg_week = 0.5, adherence = 0.8, recent_weeks = 8, sd_weeks = 8,
            sd_fallback_weeks = 52, min_weeks = 4, z = 1.2816,   # p10 / p90 of a normal
            km_week = NA_real_, km_recent_weeks = 8)
  utils::modifyList(d, cfg$scenarios %||% list())
}

# fmt_num(), fmt_goal(), fmt_day(), sessions_per_week(): R/common.R
scenario_fmt_day <- function(d) fmt_day(d, na = "never")

# Central / fast / slow dates against a deadline (and an optional window start)
date_verdict <- function(mid, fast, deadline, window_start = NULL) {
  ok <- function(d, by) !is.na(d) && as.Date(d) <= as.Date(by)
  if (!is.null(window_start)) {
    if (ok(mid, window_start)) return("on_track")
    if (ok(mid, deadline) || ok(fast, deadline)) return("at_risk")
    return("behind")
  }
  if (ok(mid, deadline)) "on_track" else if (ok(fast, deadline)) "at_risk" else "behind"
}

# ── Bodyweight ───────────────────────────────────────────────────────────────

# The SD of the weekly trend change: the last sd_weeks complete weeks in the
# plan, or, with fewer than min_weeks of them, the sd_fallback_weeks before
weekly_loss_sd <- function(weekly, a) {
  w <- weekly %>% filter(complete, !is.na(trend_change)) %>% arrange(week)
  plan <- w %>% filter(in_plan) %>% utils::tail(a$sd_weeks)
  use <- if (nrow(plan) >= a$min_weeks) plan else utils::tail(w, a$sd_fallback_weeks)
  list(sd = if (nrow(use) >= 2) stats::sd(use$trend_change) else NA_real_, weeks = nrow(use),
       basis = if (nrow(plan) >= a$min_weeks) "the cut" else "the weeks before the cut")
}

# The date the trend reaches target_kg at a weekly loss, with the goal's
# pause; NA when the loss is not a loss
weight_eta <- function(as_of, now_kg, rate, goal) {
  if (is.na(rate) || rate <= 0.01) return(as.Date(NA))
  pace_eta(as_of, now_kg, utils::modifyList(goal, list(rate_kg_per_week = rate)))
}

scenario_weight <- function(quest, data, as_of, a) {
  as_of <- as.Date(as_of)
  w <- data$weight %>% filter(as.Date(date) <= as_of, !is.na(trend_kg)) %>% arrange(date)
  if (nrow(w) == 0) return(list(verdict = "none", summary = "No trend weight yet", models = NULL))
  now_kg <- dplyr::last(w$trend_kg)
  from <- as.Date(dplyr::last(w$date))
  goal <- utils::modifyList(as.list(data$goal %||% list()), list(target_kg = quest$target))
  goal$pause_weeks <- goal$pause_weeks %||% 0
  # The recent pace: the trend's slope over the last recent_weeks
  slope <- trend_rate(w, a$recent_weeks)   # the same as the Weight tab's rate tile
  s <- weekly_loss_sd(data$weekly, a)
  model <- function(name, label, rate) {
    mid <- weight_eta(from, now_kg, rate, goal)
    k <- if (is.na(mid)) 52 else max(1, as.numeric(mid - from) / 7 - goal$pause_weeks)
    se <- if (is.na(s$sd)) 0 else a$z * s$sd / sqrt(k)
    fast <- weight_eta(from, now_kg, rate + se, goal)
    slow <- weight_eta(from, now_kg, rate - se, goal)
    tibble::tibble(model = name, label = label, rate = rate, rate_fast = rate + se, rate_slow = max(rate - se, 0),
                   mid = mid, fast = fast, slow = slow,
                   verdict = date_verdict(mid, fast, quest$deadline, quest$window_start))
  }
  models <- dplyr::bind_rows(
    model("plan", sprintf("On the plan: %.2f kg a week (%.1f × %d%%)", a$rate_kg_week * a$adherence,
                          a$rate_kg_week, round(100 * a$adherence)), a$rate_kg_week * a$adherence),
    model("recent", sprintf("At the recent pace: %.2f kg a week (last %d weeks)", -slope, a$recent_weeks), -slope)
  )
  # The chip follows what is happening (the recent pace) once the cut has
  # min_weeks behind it; before that, the plan
  in_cut <- sum(data$weekly$in_plan & data$weekly$complete, na.rm = TRUE)
  lead <- if (in_cut >= a$min_weeks && !is.na(slope)) "recent" else "plan"
  m <- models[models$model == lead, ]
  other <- models[models$model != lead, ]
  name <- function(x) if (x == "recent") "at the recent pace" else "on the plan"
  list(verdict = m$verdict, model = lead, now = now_kg, from = from, goal = goal, models = models, sd = s,
       summary = sprintf("%s kg %s %s (range %s to %s); %s %s (%s); %s",
                         fmt_num(quest$target), name(lead),
                         scenario_fmt_day(m$mid), scenario_fmt_day(m$fast), scenario_fmt_day(m$slow),
                         name(other$model), scenario_fmt_day(other$mid), tolower(scenario_verdict_labels[[other$verdict]]),
                         if (is.null(quest$window_start)) sprintf("deadline %s", scenario_fmt_day(quest$deadline))
                         else sprintf("window %s to %s", scenario_fmt_day(quest$window_start), scenario_fmt_day(quest$deadline))))
}

# Projected trend per model and band, for the figure
weight_projection <- function(sw, deadline, step_days = 7) {
  if (is.null(sw$models)) return(NULL)
  to <- max(as.Date(deadline) + 60, sw$from + 7)
  dates <- seq(sw$from, to, by = step_days)
  purrr::pmap(sw$models, function(model, label, rate, rate_fast, rate_slow, ...) {
    purrr::imap(c(mid = rate, fast = rate_fast, slow = rate_slow), function(r, band) {
      kg <- if (r <= 0.01) rep(sw$now, length(dates)) else
        pace_curve(dates, sw$from, sw$now, utils::modifyList(sw$goal, list(rate_kg_per_week = r)))
      tibble::tibble(model = model, label = label, band = band, date = dates, value = kg)
    }) %>% purrr::list_rbind()
  }) %>% purrr::list_rbind()
}

# ── Lifts ────────────────────────────────────────────────────────────────────

# The heavy single on the bar per band (p10 slow, p50 mid, p90 fast), from
# the strength forecast. A variant without its own forecast (front squat)
# borrows a parent's curve: the ceiling scaled by variant_ratio, starting
# from the variant's own e1RM now, the parent's time constant.
lift_projection <- function(quest, data, as_of, horizon = NULL) {
  fc <- quest$forecast %||% list()
  ratio <- as.numeric(quest$single_ratio %||% 1)
  key <- fc$lift
  if (is.null(key) || is.null(data$forecast)) return(NULL)
  if (!is.null(fc$variant_ratio)) {
    s <- data$strength %>% filter(lift == quest$exercise, as.Date(date) <= as.Date(as_of), !is.na(e1rm)) %>% arrange(date)
    p <- data$forecast$params %>% filter(lift == key)
    if (nrow(s) == 0 || nrow(p) == 0) return(NULL)
    from <- as.Date(dplyr::last(s$date))
    y0 <- dplyr::last(s$e1rm)
    to <- as.Date(horizon %||% max(data$forecast$curve$date))
    days <- seq(0, as.numeric(to - from), by = 7)
    return(purrr::pmap(p, function(band, c, tau, ...) {
      tibble::tibble(band = band, date = from + days,
                     value = plateau_curve(days / MONTH_DAYS, c * fc$variant_ratio, y0, tau) * ratio)
    }) %>% purrr::list_rbind())
  }
  data$forecast$curve %>% filter(lift == key) %>% transmute(band, date = as.Date(date), value = e1rm * ratio)
}

scenario_lift <- function(quest, data, as_of) {
  target <- quest_value_num(quest$target)
  proj <- lift_projection(quest, data, as_of)
  if (is.null(proj) || nrow(proj) == 0) {
    return(list(verdict = "none", summary = "No strength forecast for this lift", projection = NULL))
  }
  reach <- function(b) {
    x <- proj %>% filter(band == b, value >= target - 1e-9) %>% arrange(date)
    if (nrow(x)) x$date[1] else as.Date(NA)
  }
  mid <- reach("p50"); fast <- reach("p90"); slow <- reach("p10")
  borrowed <- !is.null(quest$forecast$variant_ratio)
  list(verdict = date_verdict(mid, fast, quest$deadline), mid = mid, fast = fast, slow = slow, target = target,
       projection = proj %>% mutate(band = recode(band, p50 = "mid", p90 = "fast", p10 = "slow")),
       summary = sprintf("%s kg on the bar %s (p90 %s, p10 %s); deadline %s%s",
                         fmt_goal(target), scenario_fmt_day(mid), scenario_fmt_day(fast), scenario_fmt_day(slow),
                         scenario_fmt_day(quest$deadline),
                         if (borrowed) sprintf("; the %s curve × %s", quest$forecast$lift, quest$forecast$variant_ratio) else ""))
}

# ── Cycling km ───────────────────────────────────────────────────────────────

# Km in the year by the deadline: so far plus weeks left x km a week, two
# ways: the km a week you set (default: what an even pace needs from today)
# and the recent pace. Range: +- z x the SD of weekly km x sqrt(weeks left).
scenario_km <- function(quest, data, as_of, a, cfg) {
  as_of <- as.Date(as_of)
  year <- as.integer(cfg$year %||% format(as_of, "%Y"))
  first <- as.Date(sprintf("%d-01-01", year))
  deadline <- as.Date(quest$deadline %||% sprintf("%d-12-31", year))
  target <- quest_value_num(quest$target)
  r <- data$rides %>% mutate(date = as.Date(date)) %>% filter(date <= as_of)
  so_far <- sum(r$km[r$date >= first], na.rm = TRUE)
  start <- max(as_of, first - 1)
  weeks_left <- max(0, as.numeric(deadline - start) / 7)
  need <- if (weeks_left > 0) max(0, (target - so_far) / weeks_left) else NA_real_
  weekly <- r %>% filter(date > as_of - 364) %>%
    mutate(week = date - (as.integer(format(date, "%u")) - 1L)) %>%
    count(week, wt = km, name = "km")
  all_weeks <- tibble::tibble(week = seq(max(weekly$week %||% as_of) - 7 * 51, max(weekly$week %||% as_of), by = 7))
  weekly <- all_weeks %>% left_join(weekly, by = "week") %>% mutate(km = coalesce(km, 0))
  sd_week <- if (nrow(weekly) >= 4) stats::sd(weekly$km) else NA_real_
  recent <- mean(utils::tail(weekly$km, a$km_recent_weeks))
  set <- if (is.na(a$km_week %||% NA)) need else a$km_week
  model <- function(name, label, km_week) {
    mid <- so_far + km_week * weeks_left
    half <- if (is.na(sd_week)) 0 else a$z * sd_week * sqrt(weeks_left)
    v <- if (mid >= target) "on_track" else if (mid + half >= target) "at_risk" else "behind"
    tibble::tibble(model = name, label = label, km_week = km_week, mid = mid, fast = mid + half,
                   slow = max(so_far, mid - half), verdict = v)
  }
  models <- dplyr::bind_rows(
    model("set", sprintf("At %s km a week%s", fmt_num(set), if (is.na(a$km_week %||% NA)) " (an even pace from today)" else ""), set),
    model("recent", sprintf("At the recent pace: %s km a week (last %d weeks)", fmt_num(recent), a$km_recent_weeks), recent)
  )
  # The chip follows the recent pace: the km a week you set is only a what-if
  m <- models[models$model == "recent", ]
  list(verdict = m$verdict, model = "recent", models = models, so_far = so_far, weeks_left = weeks_left,
       need = need, target = target, deadline = deadline, start = start,
       summary = sprintf("%s km by %s at the recent pace (range %s to %s); at %s km a week %s (%s); needs %s km a week",
                         fmt_num(m$mid), scenario_fmt_day(deadline), fmt_num(m$slow), fmt_num(m$fast),
                         fmt_num(set), fmt_num(models$mid[1]), tolower(scenario_verdict_labels[[models$verdict[1]]]),
                         fmt_num(need)))
}

# ── BJJ ──────────────────────────────────────────────────────────────────────

# The rate over the last 28 days against met_at, plus the year view:
# sessions so far against met_at x weeks gone
scenario_rate <- function(quest, data, as_of, cfg) {
  as_of <- as.Date(as_of)
  s <- data$sessions %>% filter(kind == "bjj", as.Date(date) <= as_of)
  met_at <- quest$met_at %||% quest$target
  if (nrow(s) == 0) return(list(verdict = "none", summary = "No sessions logged"))
  rate <- sessions_per_week(s, "bjj", as_of)
  year <- as.integer(cfg$year %||% format(as_of, "%Y"))
  first <- as.Date(sprintf("%d-01-01", year))
  weeks_gone <- max(0, as.numeric(as_of - first + 1) / 7)
  so_far <- sum(as.Date(s$date) >= first)
  need <- met_at * weeks_gone
  rate_ok <- rate >= met_at - 1e-9
  year_ok <- weeks_gone == 0 || so_far >= need - 1e-9
  list(verdict = if (rate_ok && year_ok) "on_track" else if (rate_ok || year_ok) "at_risk" else "behind",
       rate = rate, so_far = so_far, need = need,
       summary = paste0(sprintf("%s a week over the last 28 days (met at %s)", fmt_num(rate, 1), fmt_num(met_at, 1)),
                        if (weeks_gone > 0) sprintf("; %d sessions in %d so far against %s", so_far, year, fmt_num(need))
                        else sprintf("; %d starts on 1 January", year)))
}

# ── Events ───────────────────────────────────────────────────────────────────

# Done when every stage is; else due or overdue against the stage's own
# deadline (or the quest's)
scenario_event <- function(quest, data, as_of) {
  as_of <- as.Date(as_of)
  st <- quest$stages %||% list()
  done <- lapply(st, quest_stage_done, data = data, as_of = as_of)
  open <- which(vapply(done, is.na, logical(1)))
  if (length(st) && !length(open)) return(list(verdict = "done", summary = "All stages done"))
  dl <- vapply(open, function(i) as.numeric(as.Date(st[[i]]$deadline %||% quest$deadline %||% NA)), numeric(1))
  next_dl <- if (all(is.na(dl))) as.Date(NA) else as.Date(min(dl, na.rm = TRUE), origin = "1970-01-01")
  verdict <- if (is.na(next_dl)) "due" else if (as_of > next_dl) "overdue" else "due"
  list(verdict = verdict,
       summary = sprintf("%d of %d done%s", length(st) - length(open), length(st),
                         if (is.na(next_dl)) "" else sprintf("; next by %s", scenario_fmt_day(next_dl))))
}

# ── All quests ───────────────────────────────────────────────────────────────

# One scenario per quest (a list, with the detail the figures need)
quest_scenario <- function(quest, data, as_of, a, cfg) {
  switch(quest$metric,
    weight_trend_kg = scenario_weight(quest, data, as_of, a),
    e1rm_bar_kg = scenario_lift(quest, data, as_of),
    ride_km_ytd = scenario_km(quest, data, as_of, a, cfg),
    bjj_sessions_per_week = scenario_rate(quest, data, as_of, cfg),
    climb_clean = , manual = , milestones_done = , stages = scenario_event(quest, data, as_of),
    list(verdict = "none", summary = "")
  )
}

# One row per quest: the verdict and its summary, with the assumptions used
quest_scenarios <- function(cfg, data, as_of = Sys.Date(), a = scenario_defaults(cfg)) {
  dplyr::bind_rows(lapply(seq_along(cfg$quests), function(i) {
    q <- cfg$quests[[i]]
    sc <- quest_scenario(q, data, as_of, a, cfg)
    tibble::tibble(ord = i, id = q$id, label = q$label, plain = q$plain %||% q$label,
                   verdict = sc$verdict, verdict_label = unname(scenario_verdict_labels[sc$verdict]),
                   model = sc$model %||% NA_character_, summary = sc$summary,
                   deadline = as.Date(q$deadline %||% NA), as_of = as.Date(as_of))
  }))
}

# The tables quest_scenarios() reads beyond quest_data(), from the database
scenario_data <- function(con, events = NULL) {
  get <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
  c(quest_data(con, events), list(
    weekly = get("SELECT week, trend_change, in_plan, complete FROM weight_weekly"),
    goal = as.list(get("SELECT * FROM weight_goal")),
    forecast = list(params = get("SELECT * FROM strength_forecast_params"),
                    curve = get("SELECT lift, band, date, e1rm FROM strength_forecast_curve"))
  ))
}

write_scenario_tables <- function(scenarios, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "quest_scenarios", scenarios, overwrite = TRUE)
  message("✓ scenario table written (", paste(table(scenarios$verdict), names(table(scenarios$verdict)),
                                              collapse = ", "), ")")
  invisible(TRUE)
}
