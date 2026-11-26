library(dplyr)

# Quests: the Quest Board on the Home tab, driven by config/oath.yml.
#
# Pure functions, no Shiny: the config plus tables already in the database
# go in, one row per quest comes out (`quest_board()`), written to the
# database by `write_quest_tables()` so the app reads only the database.
#
# `data` is a list of the tables the metrics read:
#   weight   (date, trend_kg)            weight_daily: MacroFactor's trend, a model output
#   strength (date, lift, e1rm)          strength_sessions: session-best e1RM, bar weight
#   rides    (date, km, elev_m)          strava_rides: all ride types
#   sessions (date, kind)                sessions_daily: attendance only, never outcomes
#   climbs   (date, climb, status, bike_id)  strava_climb_efforts: our own matching on streams
#
# No "on track" verdict yet: that needs the scenarios of milestone 7. Each
# card shows now against target, and how far along the way it is.

`%||%` <- function(a, b) if (is.null(a)) b else a

quest_metric_units <- c(weight_trend_kg = "kg", e1rm_bar_kg = "kg", ride_km_ytd = "km",
                        bjj_sessions_per_week = "a week")
quest_metric_digits <- c(weight_trend_kg = 1, e1rm_bar_kg = 0, ride_km_ytd = 0,
                         bjj_sessions_per_week = 1)

fmt_num <- function(x, digits = 0) {
  formatC(round(x, digits), format = "f", digits = digits, big.mark = ",")
}
# Targets and milestones: whole numbers without decimals (98, not 98.0)
fmt_goal <- function(x, digits = 0) fmt_num(x, if (isTRUE(all.equal(x, round(x)))) 0 else digits)
fmt_day <- function(d) sub("^0", "", format(as.Date(d), "%d %b %Y"))

# A target or baseline that is a number, a {low, high} range or a {text}
quest_value_text <- function(v, digits = 0) {
  if (is.null(v)) return(NA_character_)
  if (is.list(v)) {
    if (!is.null(v$text)) return(v$text)
    if (!is.null(v$low) && !is.null(v$high)) {
      return(paste0(fmt_goal(v$low, digits), "\u2013", fmt_goal(v$high, digits)))
    }
    if (!is.null(v$value)) return(fmt_goal(v$value, digits))
    return(NA_character_)
  }
  fmt_goal(v, digits)
}
# The number progress runs to or from: a range counts by its low end
quest_value_num <- function(v) {
  if (is.null(v)) return(NA_real_)
  if (is.list(v)) return(as.numeric(v$value %||% v$low %||% NA_real_))
  as.numeric(v)
}

# A lift quest with single_ratio counts the heavy single on the bar: e1RM x
# single_ratio (singles come out below the 2-6-rep e1RM). Its baseline is an
# e1RM too and is scaled the same way, so now, start and target share a scale.
quest_scale <- function(v, ratio) {
  if (is.null(v) || ratio == 1) return(v)
  if (is.list(v)) {
    for (k in intersect(names(v), c("value", "low", "high"))) v[[k]] <- v[[k]] * ratio
    return(v)
  }
  v * ratio
}
quest_single_ratio <- function(quest) {
  if (identical(quest$metric, "e1rm_bar_kg")) as.numeric(quest$single_ratio %||% 1) else 1
}

# ── Now ──────────────────────────────────────────────────────────────────────

# The quest's current value on as_of: value, date and source ("live" from
# the database, "baseline" from the config when the data has nothing yet).
quest_now <- function(quest, data, as_of, cfg = list()) {
  as_of <- as.Date(as_of)
  ratio <- quest_single_ratio(quest)
  baseline <- function() {
    b <- quest_scale(quest$baseline, ratio)
    list(value = quest_value_num(b), date = as.Date(if (is.list(b)) b$date %||% NA else NA),
         source = "baseline", text = quest_value_text(b, quest_metric_digits[quest$metric] %||% 0))
  }
  live <- function(value, date, ...) {
    list(value = value, date = as.Date(date), source = "live",
         text = fmt_num(value, quest_metric_digits[quest$metric] %||% 0), ...)
  }
  switch(quest$metric,
    weight_trend_kg = {
      w <- data$weight %>% filter(as.Date(date) <= as_of, !is.na(trend_kg)) %>% arrange(date)
      if (nrow(w) == 0) baseline() else live(dplyr::last(w$trend_kg), dplyr::last(w$date))
    },
    e1rm_bar_kg = {
      n <- cfg$now_sessions %||% 2
      s <- data$strength %>%
        filter(lift == quest$exercise, as.Date(date) <= as_of, !is.na(e1rm)) %>%
        arrange(date) %>% utils::tail(n)
      if (nrow(s) == 0) baseline() else
        live(mean(s$e1rm) * ratio, max(s$date), sessions = nrow(s), e1rm = mean(s$e1rm), ratio = ratio)
    },
    ride_km_ytd = {
      # Before the quest's year starts, the year so far stands in (labelled)
      yr <- min(as.integer(format(as_of, "%Y")), as.integer(cfg$year %||% format(as_of, "%Y")))
      r <- data$rides %>% filter(as.integer(format(as.Date(date), "%Y")) == yr, as.Date(date) <= as_of)
      live(sum(r$km, na.rm = TRUE), as_of, year = yr)
    },
    bjj_sessions_per_week = {
      s <- data$sessions %>% filter(kind == "bjj")
      if (nrow(s) == 0) {
        list(value = NA_real_, date = as.Date(NA), source = "none", text = "not logged")
      } else {
        in28 <- s %>% filter(as.Date(date) > as_of - 28, as.Date(date) <= as_of)
        live(nrow(in28) / 4, as_of)
      }
    },
    climb_clean = , manual = , milestones_done = , stages = {
      done <- lapply(quest$stages %||% list(), quest_stage_done, data = data, as_of = as_of)
      list(value = sum(vapply(done, \(d) !is.na(d), logical(1))), date = as_of,
           source = "live", text = NA_character_, stages = done)
    },
    stop("Unknown quest metric '", quest$metric, "' in quest '", quest$id, "'")
  )
}

# When a stage was done (a Date), or NA. A stage is either checked from the
# climb record ({climb, bike, since}: a clean ascent) or ticked by hand
# ({done: date}).
quest_stage_done <- function(stage, data, as_of) {
  if (!is.null(stage$done)) {
    d <- as.Date(stage$done)
    return(if (d <= as_of) d else as.Date(NA))
  }
  if (!is.null(stage$climb)) {
    e <- data$climbs %>%
      filter(climb == stage$climb, status == "clean", as.Date(date) <= as_of,
             as.Date(date) >= as.Date(stage$since %||% "1900-01-01"))
    if (!is.null(stage$bike)) e <- e %>% filter(bike_id == stage$bike)
    return(if (nrow(e) == 0) as.Date(NA) else min(as.Date(e$date)))
  }
  as.Date(NA)
}

# ── Progress ─────────────────────────────────────────────────────────────────

# The value progress counts from: the value on progress_start once that day
# has passed, else the baseline in the config.
quest_start <- function(quest, data, as_of, cfg) {
  start_day <- as.Date(cfg$progress_start %||% NA)
  if (!is.na(start_day) && as.Date(as_of) >= start_day) {
    s <- quest_now(quest, data, start_day, cfg)
    if (identical(s$source, "live")) return(s$value)
  }
  quest_value_num(quest_scale(quest$baseline, quest_single_ratio(quest)))
}

# Share of the way (0-1, or NA) and a label, by kind (spec table A3)
quest_progress <- function(quest, now, start = NA_real_, as_of = Sys.Date(), cfg = list()) {
  target <- quest_value_num(quest$target)
  unit <- quest_metric_units[quest$metric] %||% ""
  digits <- quest_metric_digits[quest$metric] %||% 0
  if (identical(now$source, "none")) return(list(share = NA_real_, label = now$text))
  switch(quest$kind,
    increasing = , decreasing = {
      if (is.na(now$value) || is.na(start) || target == start) {
        return(list(share = NA_real_, label = ""))
      }
      share <- max(0, min(1, (now$value - start) / (target - start)))
      list(share = share, label = sprintf("%s%% of the way from %s %s", round(100 * share),
                                          fmt_goal(start, digits), unit))
    },
    hold = {
      # Held or below, plus the same bar as the lifts that increase: the
      # share of the way from the start value to the target
      band <- quest$hold_band_kg %||% cfg$hold_band_kg %||% 2.5
      held <- !is.na(now$value) && now$value >= target - band
      share <- if (is.na(now$value) || is.na(start) || target <= start) NA_real_ else
        max(0, min(1, (now$value - start) / (target - start)))
      list(share = share,
           label = paste0(if (held) "Held" else "Below",
                          if (!is.na(share)) sprintf(": %s%% of the way from %s %s", round(100 * share), fmt_goal(start, digits), unit) else "",
                          sprintf(" (within %s %s of the target counts as held)", fmt_num(band, 1), unit)))
    },
    cumulative = {
      yr <- as.integer(cfg$year %||% format(as.Date(as_of), "%Y"))
      if (!identical(now$year, yr)) {
        return(list(share = 0, label = sprintf("%s km in %s so far; %s starts on 1 January",
                                               fmt_num(now$value), now$year, yr)))
      }
      first <- as.Date(sprintf("%d-01-01", yr))
      days <- as.numeric(as.Date(sprintf("%d-12-31", yr)) - first) + 1
      pace <- target * (as.numeric(as.Date(as_of) - first) + 1) / days
      list(share = min(1, now$value / target),
           label = sprintf("%s%% of the target; an even pace would be at %s km today",
                           round(100 * min(1, now$value / target)), fmt_num(pace)))
    },
    rate = {
      met_at <- quest$met_at %||% target
      list(share = NA_real_,
           label = sprintf("%s: met at %s a week (last 28 days)",
                           if (now$value >= met_at) "Met" else "Not met", fmt_num(met_at, 1)))
    },
    event = , steps = {
      n <- length(now$stages)
      names_done <- vapply(seq_len(n), function(i) {
        st <- quest$stages[[i]]
        d <- now$stages[[i]]
        paste0(if (is.na(d)) "\u2610 " else "\u2611 ", st$label, if (!is.na(d)) paste0(" (", fmt_day(d), ")") else "")
      }, character(1))
      list(share = if (n > 0) now$value / n else NA_real_,
           label = paste0(now$value, " of ", n, if (n > 0) paste0(": ", paste(names_done, collapse = " \u00B7 ")) else ""))
    },
    stop("Unknown quest kind '", quest$kind, "' in quest '", quest$id, "'")
  )
}

# Notes under the progress line: milestones, a checkpoint, a goal ride
quest_extras <- function(quest, data, now, as_of, cfg) {
  digits <- quest_metric_digits[quest$metric] %||% 0
  unit <- quest_metric_units[quest$metric] %||% ""
  out <- character(0)
  for (m in quest$milestones %||% list()) {
    reached <- quest$kind == "decreasing" && !is.na(now$value) && now$value <= m$value ||
      quest$kind == "increasing" && !is.na(now$value) && now$value >= m$value
    out <- c(out, sprintf("%s %s %s: %s", if (reached) "\u2611" else "\u2610", fmt_goal(m$value, digits), unit,
                          paste0(if (reached) "reached" else "not yet", if (!is.null(m$note)) paste0(", then ", m$note) else "")))
  }
  if (!is.null(quest$checkpoint)) {
    cp <- quest$checkpoint
    out <- c(out, sprintf("Checkpoint %s: %s", fmt_day(cp$date),
                          if (is.null(cp$value)) "value set at the gate" else paste(fmt_num(cp$value, digits), unit)))
  }
  if (!is.null(quest$fallback_target)) {
    out <- c(out, sprintf("Fallback %s %s if the checkpoint is missed", fmt_num(quest$fallback_target, digits), unit))
  }
  gr <- quest$extra$goal_ride
  if (!is.null(gr)) {
    yr <- as.integer(cfg$year %||% format(as.Date(as_of), "%Y"))
    hit <- data$rides %>%
      filter(as.integer(format(as.Date(date), "%Y")) == yr, as.Date(date) <= as_of,
             km >= gr$km, elev_m >= gr$gain_m) %>% arrange(date)
    out <- c(out, sprintf("%s Goal ride %s km / %s m: %s", if (nrow(hit) > 0) "\u2611" else "\u2610",
                          fmt_num(gr$km), fmt_num(gr$gain_m),
                          if (nrow(hit) > 0) sprintf("ridden on %s", fmt_day(hit$date[1])) else "not yet"))
  }
  paste(out, collapse = " \u00B7 ")
}

# Where a card leads when clicked: a tab (its module id) and inputs to set
# there. From the quest's `link: {tab, inputs}`, else by metric; `link:
# none` for no link.
quest_link_defaults <- list(
  weight_trend_kg       = list(tab = "weight", inputs = list(figure = "trend")),
  bjj_sessions_per_week = list(tab = "weight", inputs = list(figure = "weeks")),
  e1rm_bar_kg           = list(tab = "strength", inputs = list(figure = "lifts")),
  ride_km_ytd           = list(tab = "engine", inputs = list(view = "rides")),
  climb_clean           = list(tab = "engine", inputs = list(view = "climbs"))
)
quest_link <- function(quest) {
  if (identical(quest$link, "none")) return(list(tab = NA_character_, inputs = "{}"))
  l <- quest$link %||% quest_link_defaults[[quest$metric]]
  if (is.null(l$tab)) return(list(tab = NA_character_, inputs = "{}"))
  list(tab = l$tab, inputs = as.character(jsonlite::toJSON(l$inputs %||% list(), auto_unbox = TRUE)))
}

# ── The board ────────────────────────────────────────────────────────────────

# One row per quest, in config order. headline is the card's big line
# ("now -> target unit", or the config's own headline for events).
quest_board <- function(cfg, data, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  rows <- lapply(seq_along(cfg$quests), function(i) {
    q <- cfg$quests[[i]]
    now <- quest_now(q, data, as_of, cfg)
    start <- quest_start(q, data, as_of, cfg)
    pr <- quest_progress(q, now, start, as_of, cfg)
    unit <- quest_metric_units[q$metric] %||% ""
    digits <- quest_metric_digits[q$metric] %||% 0
    headline <- q$headline %||%
      if (q$kind %in% c("event", "steps")) quest_value_text(q$target) else
        sprintf("%s \u2192 %s %s", now$text, quest_value_text(q$target, digits), unit)
    source_note <- switch(now$source,
      live = if (q$kind %in% c("event", "steps", "cumulative", "rate")) "" else
        if (q$metric == "e1rm_bar_kg" && now$ratio != 1)
          sprintf("Heavy single on the bar (estimate): e1RM %s \u00D7 %s, %s %s",
                  fmt_num(now$e1rm, 1), formatC(now$ratio, format = "f", digits = 3),
                  if (now$sessions == 1) "last session," else sprintf("mean of the last %d sessions, to", now$sessions),
                  fmt_day(now$date))
        else if (q$metric == "e1rm_bar_kg")
          sprintf("e1RM, %s %s", if (now$sessions == 1) "last session," else sprintf("mean of the last %d sessions, to", now$sessions), fmt_day(now$date))
        else sprintf("Trend weight (a model output) on %s", fmt_day(now$date)),
      baseline = sprintf("Baseline from the plan%s, no data yet", if (!is.na(now$date)) paste0(", ", fmt_day(now$date)) else ""),
      none = "")
    tibble::tibble(
      ord = i, id = q$id, label = q$label, role = q$role %||% "side", kind = q$kind,
      accent = as.integer(q$accent %||% min(4L, 1L + (i - 1L) %/% 2L)),
      headline = headline, by = q$by %||% "",
      progress = pr$label, share = pr$share, source = now$source, source_note = source_note,
      link_tab = quest_link(q)$tab, link_inputs = quest_link(q)$inputs,
      extras = quest_extras(q, data, now, as_of, cfg),
      now_value = as.numeric(now$value), now_e1rm = as.numeric(now$e1rm %||% NA_real_), now_date = as.Date(now$date), as_of = as_of
    )
  })
  dplyr::bind_rows(rows)
}

# The tables quest_board() reads, from the database
quest_data <- function(con) {
  get <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sql))
  list(
    weight   = get("SELECT date, trend_kg FROM weight_daily"),
    strength = get("SELECT date, lift, e1rm FROM strength_sessions"),
    rides    = get("SELECT date, km, elev_m FROM strava_rides"),
    sessions = get("SELECT date, kind FROM sessions_daily"),
    climbs   = get("SELECT date, climb, status, bike_id FROM strava_climb_efforts")
  )
}

# The board plus the config itself (as JSON: the fixed copy, tenets), so the
# app reads only the database
write_quest_tables <- function(board, cfg, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "quest_board", board, overwrite = TRUE)
  DBI::dbWriteTable(con, "oath_config",
                    tibble::tibble(json = as.character(jsonlite::toJSON(cfg, auto_unbox = TRUE, null = "null"))),
                    overwrite = TRUE)
  message("\u2713 quest tables written (", nrow(board), " quests)")
  invisible(TRUE)
}
