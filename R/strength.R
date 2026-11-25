library(dplyr)

# Strength: the best estimated one-rep max (bar weight) per session and lift,
# grouped by config/lifts.csv, against the targets and ceilings in
# config/goals.yml. Formula and RIR limit: strength in config/athlete.yml.
#
# Set selection (R/e1rm.R): working sets only (Standard or Failure), RIR
# logged and <= max_rir, reps >= 1, machine base weight added. Verified
# against the reference dashboard's e1rm.py: with the same formula, every
# session best and top set matches (golden test).
#
# An e1RM is an estimate from a submaximal set, not a tested max.

build_strength_sessions <- function(sets, lifts, weights, mf_exercise = NULL, settings = list(),
                                    max_bw_age_days = Inf) {
  s <- utils::modifyList(list(e1rm_formula = "wathen", max_rir = 3), settings %||% list())
  lifts <- lifts %>% mutate(main = as.logical(main))

  best <- sets %>%
    select(-any_of(c("e1rm_kg", "load_kg", "reps_to_failure", "is_assisted"))) %>%
    add_set_e1rm(max_rir = s$max_rir, formula = s$e1rm_formula) %>%
    filter(!is.na(e1rm_kg)) %>%
    slice_max(e1rm_kg, n = 1, with_ties = FALSE, by = c(exercise, date)) %>%
    # Where the top set's RIR came from: logged, or (backfills) the program's
    # target RPE or an assumption. MacroFactor's sets are always logged.
    mutate(rir_basis = if ("rir_basis" %in% names(.)) coalesce(rir_basis, "RPE logged") else "RPE logged") %>%
    transmute(date = as.Date(date), lift = exercise,
              top_set = sprintf("%g x %d @ %g RIR", load_kg, as.integer(reps), rir),
              reps_to_failure, e1rm = round(e1rm_kg, 1), rir_basis)

  # Bodyweight: `bodyweight_kg` (with its `basis`, from bodyweight_for_ratios)
  # or MacroFactor's `trend_kg`; the latest value up to max_bw_age_days old.
  bw <- weights %>%
    transmute(bw_date = as.Date(date),
              bodyweight_kg = if ("bodyweight_kg" %in% names(.)) bodyweight_kg else trend_kg,
              bodyweight_basis = if ("basis" %in% names(.)) basis else "macrofactor trend")
  out <- best %>%
    inner_join(lifts, by = c(lift = "exercise")) %>%
    left_join(bw, by = join_by(closest(date >= bw_date))) %>%
    mutate(stale = is.na(bw_date) | as.numeric(date - bw_date) > max_bw_age_days,
           bodyweight_kg = if_else(stale, NA_real_, bodyweight_kg),
           bodyweight_basis = if_else(stale, NA_character_, bodyweight_basis)) %>%
    select(-bw_date, -stale) %>%
    mutate(x_bodyweight = e1rm / bodyweight_kg, formula = s$e1rm_formula)

  # MacroFactor's own 1-RM figure for the same day, for comparison only
  if (!is.null(mf_exercise)) {
    mf <- mf_exercise %>%
      filter(!is.na(x1_rm)) %>%
      transmute(date = as.Date(date), mf_key = exercise, macrofactor_e1rm = x1_rm)
    out <- out %>%
      mutate(mf_key = janitor::make_clean_names(paste(lift, "(kg)"), allow_dupes = TRUE)) %>%
      left_join(mf, by = c("date", "mf_key")) %>%
      select(-mf_key)
  }
  arrange(out, group, lift, date)
}

# Targets and ceilings from goals.yml, as tables.
strength_goal_tables <- function(lifts, goal) {
  mains <- lifts %>% filter(as.logical(main)) %>% select(group, main_lift = exercise)
  targets <- tibble(group = names(goal$targets_kg), target_kg = unlist(goal$targets_kg),
                    target_date = as.Date(goal$target_date)) %>%
    left_join(mains, by = "group")
  ceilings <- purrr::imap(goal$ceilings_kg %||% list(), \(groups, profile) {
    tibble(profile = profile, group = names(groups),
           low = unname(vapply(groups, `[[`, 1, 1)), high = unname(vapply(groups, `[[`, 1, 2)))
  }) %>% purrr::list_rbind()
  list(targets = targets, ceilings = ceilings)
}

# Public reference standards, as multiples of bodyweight (cycling spec,
# section 6). They are for the back squat, bench press and deadlift.
strength_standards <- tibble::tribble(
  ~group,               ~standard,                              ~low,  ~high, ~source,
  "Bench",              "First-year recreational",              0.70,  1.10,  "Barbell Medicine, strength standards",
  "Squat",              "First-year recreational",              1.00,  1.50,  "Barbell Medicine, strength standards",
  "Deadlift and hinge", "First-year recreational",              1.25,  1.75,  "Barbell Medicine, strength standards",
  "Bench",              "Average raw competitor (USAPL, men)",  133.5 / 92.1, NA, "Stronger by Science, What is strong?",
  "Squat",              "Average raw competitor (USAPL, men)",  195.2 / 92.1, NA, "Stronger by Science, What is strong?",
  "Deadlift and hinge", "Average raw competitor (USAPL, men)",  227.0 / 92.1, NA, "Stronger by Science, What is strong?",
  "Bench",              "Top 10% of competitors, 18-35",        1.95,  NA,    "Normative powerlifting data, PubMed 39060209",
  "Squat",              "Top 10% of competitors, 18-35",        2.83,  NA,    "Normative powerlifting data, PubMed 39060209",
  "Deadlift and hinge", "Top 10% of competitors, 18-35",        3.25,  NA,    "Normative powerlifting data, PubMed 39060209"
)

write_strength_tables <- function(sessions, goal_tables, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "strength_sessions", sessions, overwrite = TRUE)
  DBI::dbWriteTable(con, "strength_targets", goal_tables$targets, overwrite = TRUE)
  DBI::dbWriteTable(con, "strength_ceilings", goal_tables$ceilings, overwrite = TRUE)
  message("✓ strength tables written (", nrow(sessions), " session bests)")
  invisible(TRUE)
}
