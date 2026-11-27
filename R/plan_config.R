library(dplyr)

# One source of truth for the plan's numbers.
#
#   config/goals.yml  what you aim for: weight goal (target, pause, rate,
#                     start), sessions a week, lift targets (the heavy single
#                     on the bar), single ratios, km a year, the goal ride
#   config/oath.yml   how it is shown and judged: copy, quests, relics,
#                     roadmap, deadlines, adherence (80% is met), the
#                     scenario assumptions
#
# resolve_goals() derives what goals.yml implies (the forecast's targets as
# e1RM from the main lifts' targets on the bar); resolve_oath() fills every number a
# quest, relic or scenario shares with goals.yml from goals.yml. A number set
# in both places is a conflict when the two differ: check_plan_config() lists
# them (with other mistakes) and the build stops on any.


# A lift target in goals.yml: a number, or [low, high]
target_low <- function(x) as.numeric(unlist(x))[1]
target_value <- function(x) {
  x <- as.numeric(unlist(x))
  if (length(x) >= 2) list(low = x[1], high = x[2]) else x
}

# The single ratio (heavy single / 2-6-rep e1RM) for an exercise: the
# forecast's own lifts by key, other exercises from strength$single_ratio
# (e.g. a variant borrowing its parent's ratio); 1 when none is known
single_ratio_for <- function(exercise, goals) {
  fc <- goals$strength_forecast %||% list()
  key <- names(fc$exercises)[unlist(fc$exercises) == exercise]
  r <- if (length(key)) fc$single_ratio[[key[1]]] else goals$strength$single_ratio[[exercise]]
  as.numeric(r %||% 1)
}

# The forecast a quest's lift uses: its own (strength_forecast$exercises) or
# a parent's, scaled (strength_forecast$variants: {exercise: {lift, ratio}})
forecast_for <- function(exercise, goals) {
  fc <- goals$strength_forecast %||% list()
  key <- names(fc$exercises)[unlist(fc$exercises) == exercise]
  if (length(key)) return(list(lift = key[1]))
  v <- fc$variants[[exercise]]
  if (!is.null(v)) return(list(lift = v$lift, variant_ratio = v$ratio))
  NULL
}

# goals.yml plus what it implies: the forecast's targets are the strength
# targets on the bar, as e1RM (target / single ratio), for the lifts the
# forecast covers
resolve_goals <- function(goals, lifts) {
  fc <- goals$strength_forecast
  if (!is.null(fc) && !is.null(goals$strength$targets_kg)) {
    fc$targets_kg <- purrr::imap(fc$exercises, function(exercise, key) {
      # Only a group's main lift carries its target (the Squat target is the
      # front squat's when that is the main lift, not the back squat's)
      main <- lifts$exercise == exercise & as.logical(lifts$main)
      group <- lifts$group[main]
      t <- if (length(group)) goals$strength$targets_kg[[group[1]]] else NULL
      if (is.null(t)) NULL else round(target_low(t) / single_ratio_for(exercise, goals), 1)
    }) %>% purrr::compact()
    goals$strength_forecast <- fc
  }
  goals
}

# ── oath.yml filled from goals.yml ───────────────────────────────────────────

# Set field `name` of `x` to `value`, recording a conflict when x already
# holds a different value (the config wrote the number twice)
plan_fill <- function(x, name, value, where, conflicts) {
  if (is.null(value)) return(x)
  old <- x[[name]]
  if (!is.null(old) && !isTRUE(all.equal(unlist(old), unlist(value), check.attributes = FALSE))) {
    conflicts$rows[[length(conflicts$rows) + 1]] <- tibble::tibble(
      where = where, field = name, oath = paste(unlist(old), collapse = "–"),
      goals = paste(unlist(value), collapse = "–"))
  }
  x[[name]] <- value
  x
}

resolve_oath <- function(oath, goals, lifts) {
  conflicts <- new.env()
  conflicts$rows <- list()
  wg <- goals$weight_goal %||% list()
  year <- as.integer(oath$year %||% NA)
  adherence <- oath$adherence_met %||% 0.8
  year_end <- if (!is.na(year)) sprintf("%d-12-31", year) else NULL

  oath$quests <- lapply(oath$quests %||% list(), function(q) {
    at <- paste0("quest ", q$id)
    switch(q$metric,
      weight_trend_kg = {
        q <- plan_fill(q, "target", wg$target_kg, at, conflicts)
        if (!is.null(wg$pause_at_kg)) {
          m <- q$milestones %||% list(list())
          m[[1]] <- plan_fill(m[[1]], "value", wg$pause_at_kg, paste(at, "milestone"), conflicts)
          q$milestones <- m
        }
      },
      bjj_sessions_per_week = {
        per_week <- goals$sessions$bjj$per_week
        q <- plan_fill(q, "target", per_week, at, conflicts)
        if (!is.null(per_week)) q <- plan_fill(q, "met_at", per_week * adherence, at, conflicts)
      },
      e1rm_bar_kg = {
        group <- lifts$group[lifts$exercise == q$exercise]
        t <- if (length(group)) goals$strength$targets_kg[[group[1]]] else NULL
        q <- plan_fill(q, "target", if (!is.null(t)) target_value(t), at, conflicts)
        q <- plan_fill(q, "single_ratio", single_ratio_for(q$exercise, goals), at, conflicts)
        q <- plan_fill(q, "deadline", goals$strength$target_date, at, conflicts)
        q <- plan_fill(q, "forecast", forecast_for(q$exercise, goals), at, conflicts)
      },
      ride_km_ytd = {
        q <- plan_fill(q, "target", if (!is.na(year)) goals$riding$annual_km[[as.character(year)]], at, conflicts)
        q <- plan_fill(q, "deadline", year_end, at, conflicts)
        gr <- goals$riding$goal_ride
        if (!is.null(gr)) {
          ex <- q$extra %||% list()
          ex <- plan_fill(ex, "goal_ride", list(km = gr$km, gain_m = gr$gain_m,
                                                month = if (!is.null(gr$by)) as.integer(format(as.Date(gr$by), "%m"))),
                          paste(at, "extra"), conflicts)
          q$extra <- ex
        }
      },
      NULL
    )
    q
  })

  # Relics: a rule with `goal:` takes its numbers from goals.yml
  oath$relics <- lapply(oath$relics %||% list(), function(r) {
    g <- r$rule$goal
    if (is.null(g)) return(r)
    at <- paste0("relic ", r$id)
    rule <- r$rule
    rule <- switch(g,
      pause = , target = {
        rule <- plan_fill(rule, "kg", if (g == "pause") wg$pause_at_kg else wg$target_kg, at, conflicts)
        if (!is.null(rule$adherence)) rule <- plan_fill(rule, "rate_kg_week", wg$rate_kg_per_week, at, conflicts)
        plan_fill(rule, "since", wg$start_date, at, conflicts)
      },
      goal_ride = {
        gr <- goals$riding$goal_ride
        rule <- plan_fill(rule, "km", gr$km, at, conflicts)
        rule <- plan_fill(rule, "gain_m", gr$gain_m, at, conflicts)
        plan_fill(rule, "year", year, at, conflicts)
      },
      annual_km = {
        rule <- plan_fill(rule, "km", goals$riding$annual_km[[as.character(year)]], at, conflicts)
        plan_fill(rule, "year", year, at, conflicts)
      },
      stop("Relic ", r$id, ": unknown goal '", g, "' (pause, target, goal_ride, annual_km)"))
    r$rule <- rule
    r
  })

  # Scenario defaults: the planned rate and the adherence
  sc <- oath$scenarios %||% list()
  sc <- plan_fill(sc, "rate_kg_week", wg$rate_kg_per_week, "scenarios", conflicts)
  sc <- plan_fill(sc, "adherence", adherence, "scenarios", conflicts)
  oath$scenarios <- sc

  attr(oath, "conflicts") <- dplyr::bind_rows(conflicts$rows)
  oath
}

# ── Numbers in the copy ──────────────────────────────────────────────────────

# Text in oath.yml can name a number instead of typing it: {weight_goal.target_kg},
# {riding.annual_km.2027}, {strength.targets_kg.Bench}: any path into
# goals.yml, plus a few derived ones under plan. (adherence_pct, bjj_met_at,
# year) and the quests' dates under quests.<id> (deadline, window_start,
# checkpoint, stages.<n>.deadline). Ranges print as 170–175, thousands with a comma.
copy_values <- function(goals, oath) {
  adherence <- oath$adherence_met %||% 0.8
  # The quests' dates by id, so a Roadmap gate can name a deadline:
  # {quests.bodyweight.deadline}, {quests.hill.stages.1.deadline}
  qs <- oath$quests %||% list()
  quests <- stats::setNames(lapply(qs, function(q) list(
    deadline = q$deadline, window_start = q$window_start, checkpoint = q$checkpoint$date,
    stages = lapply(q$stages %||% list(), function(st) list(deadline = st$deadline)))),
    vapply(qs, \(q) q$id %||% "", ""))
  c(goals, list(quests = quests, plan = list(
    adherence_pct = paste0(round(100 * adherence), "%"),
    bjj_met_at = (goals$sessions$bjj$per_week %||% NA) * adherence,
    year = oath$year)))
}
copy_lookup <- function(path, values) {
  v <- values
  for (k in strsplit(path, ".", fixed = TRUE)[[1]]) {
    if (!is.list(v)) return(NULL)
    # A number picks an item of an unnamed list (stages.1)
    v <- if (grepl("^[0-9]+$", k) && is.null(names(v))) { if (as.integer(k) <= length(v)) v[[as.integer(k)]] else NULL } else v[[k]]
    if (is.null(v)) return(NULL)
  }
  v <- unlist(v)
  if (is.character(v)) return(paste(v, collapse = "\u2013"))
  paste(vapply(as.numeric(v), fmt_plain, ""), collapse = "\u2013")
}
# Fill every {path} in every string of x; unknown paths are collected in
# `missing`. {level} is left alone: the app fills it (the Reliquary's level).
copy_runtime <- c("{level}")
fill_copy <- function(x, values, missing = new.env()) {
  if (is.null(missing$paths)) missing$paths <- character(0)
  if (is.list(x)) {
    a <- attributes(x)
    x <- lapply(x, fill_copy, values = values, missing = missing)
    attributes(x) <- a
    return(x)
  }
  if (!is.character(x)) return(x)
  vapply(x, function(s) {
    m <- regmatches(s, gregexpr("\\{[A-Za-z_][A-Za-z0-9_. ]*\\}", s))[[1]]   # group names have spaces
    for (ph in setdiff(unique(m), copy_runtime)) {
      v <- copy_lookup(substr(ph, 2, nchar(ph) - 1), values)
      if (is.null(v)) missing$paths <- c(missing$paths, ph) else s <- gsub(ph, v, s, fixed = TRUE)
    }
    s
  }, character(1), USE.NAMES = FALSE)
}

# Plan numbers typed into the copy next to their unit (75 kg, 2,000 km,
# 800 m): each should be a {path}, so it follows goals.yml. Returns
# one row per hit (where, text).
copy_literals <- function(oath, goals) {
  wg <- goals$weight_goal %||% list()
  gr <- goals$riding$goal_ride %||% list()
  watch <- list(
    kg = c(wg$target_kg, wg$pause_at_kg),
    km = c(unlist(goals$riding$annual_km), gr$km),
    m = gr$gain_m,
    "kg a week|kg/wk" = wg$rate_kg_per_week)
  hits <- list()
  walk <- function(x, where) {
    if (is.list(x)) { for (n in names(x) %||% seq_along(x)) walk(x[[n]], paste(where, n, sep = "/")); return() }
    if (!is.character(x)) return()
    for (s in x) for (unit in names(watch)) for (v in watch[[unit]]) {
      num <- gsub(".", "\\.", fmt_plain(v), fixed = TRUE)
      if (grepl(sprintf("(^|[^0-9.,])%s\\s*(%s)\\b", num, unit), s, perl = TRUE)) {
        hits[[length(hits) + 1]] <<- tibble::tibble(where = sub("^/", "", where), text = s)
      }
    }
  }
  walk(oath[setdiff(names(oath), c("quests", "relics", "roadmap"))], "")
  walk(oath$quests, "quests"); walk(oath$relics, "relics"); walk(oath$roadmap, "roadmap")
  dplyr::distinct(dplyr::bind_rows(hits))
}

# ── The check ────────────────────────────────────────────────────────────────

# Every problem in the plan's config, one row each (where, problem): numbers
# written in both files that differ, and references that lead nowhere. Empty
# when all is well.
check_plan_config <- function(oath, goals, lifts) {
  out <- list()
  add <- function(where, problem) out[[length(out) + 1]] <<- tibble::tibble(where = where, problem = problem)
  resolved <- resolve_oath(oath, goals, lifts)
  cf <- attr(resolved, "conflicts")
  if (!is.null(cf) && nrow(cf)) {
    for (i in seq_len(nrow(cf))) {
      add(cf$where[i], sprintf("%s is %s in oath.yml but %s in goals.yml: set it in goals.yml only",
                               cf$field[i], cf$oath[i], cf$goals[i]))
    }
  }
  year <- as.integer(oath$year %||% NA)
  for (q in resolved$quests) {
    at <- paste0("quest ", q$id)
    if (identical(q$metric, "e1rm_bar_kg")) {
      group <- lifts$group[lifts$exercise == q$exercise]
      if (!length(group)) add(at, sprintf("exercise '%s' is not in lifts.csv", q$exercise))
      else if (is.null(goals$strength$targets_kg[[group[1]]])) add(at, sprintf("no target for group '%s' in goals.yml strength", group[1]))
      else if (!isTRUE(as.logical(lifts$main[lifts$exercise == q$exercise][1])))
        add(at, sprintf("'%s' is not the main lift of '%s' in lifts.csv: the Strength tab draws the target on the main lift", q$exercise, group[1]))
      if (is.null(q$forecast)) add(at, sprintf("no forecast for '%s' (strength_forecast exercises or variants)", q$exercise))
      if (identical(q$kind, "hold")) add(at, "kind hold: lift targets are on the bar and projected like the others; use increasing")
    }
    if (identical(q$metric, "ride_km_ytd") && is.null(q$target)) add(at, sprintf("no annual_km for %s in goals.yml riding", year))
    if (!is.null(q$deadline) && !is.na(year) && as.integer(format(as.Date(q$deadline), "%Y")) > year)
      add(at, sprintf("deadline %s is after the plan's year %d", q$deadline, year))
  }
  for (r in resolved$relics) {
    if (identical(r$rule$type, "quest_stages") && !any(vapply(resolved$quests, \(q) identical(q$id, r$rule$quest), logical(1))))
      add(paste0("relic ", r$id), sprintf("no quest '%s'", r$rule$quest))
  }
  # Copy: unknown {paths}, and plan numbers typed in instead of named
  missing <- new.env()
  fill_copy(oath, copy_values(goals, oath), missing)
  for (ph in unique(missing$paths)) add("copy", sprintf("%s is not a path in goals.yml", ph))
  lit <- copy_literals(oath, goals)
  for (i in seq_len(nrow(lit))) {
    add(lit$where[i], sprintf("a plan number typed into the text (\"%s\"): write it as a {path} into goals.yml",
                              substr(lit$text[i], 1, 80)))
  }
  ids <- vapply(resolved$relics, \(r) r$id, character(1))
  for (g in oath$roadmap$gates %||% list()) {
    if (!is.null(g$relic) && !g$relic %in% ids) add(paste0("gate ", g$label), sprintf("no relic '%s'", g$relic))
  }
  if (!length(out)) return(tibble::tibble(where = character(), problem = character()))
  dplyr::bind_rows(out)
}

# The resolved oath config, or an error listing every problem
oath_from_config <- function(oath, goals, lifts) {
  problems <- check_plan_config(oath, goals, lifts)
  if (nrow(problems)) {
    stop("config/oath.yml and config/goals.yml disagree or point nowhere:\n",
         paste0("  - ", problems$where, ": ", problems$problem, collapse = "\n"), call. = FALSE)
  }
  r <- resolve_oath(oath, goals, lifts)
  attr(r, "conflicts") <- NULL
  fill_copy(r, copy_values(goals, oath))
}
