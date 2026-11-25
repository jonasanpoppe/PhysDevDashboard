# Synthetic set log (as load_macrofactor_workouts() leaves it) and lift groups.
fake_sets <- function() {
  tibble::tribble(
    ~date,        ~exercise,             ~set_type,      ~weight_kg, ~exercise_base_weight_kg, ~reps, ~rir, ~bodyweight_kg,
    "2030-01-06", "Barbell Bench Press", "Warm-Up Set",  60,         NA,                       10,    NA,   80,
    "2030-01-06", "Barbell Bench Press", "Standard Set", 90,         NA,                       5,     2,    80,
    "2030-01-06", "Barbell Bench Press", "Standard Set", 85,         NA,                       8,     1,    80,
    "2030-01-06", "Hack Squat",          "Standard Set", 100,        20,                       8,     2,    80,
    "2030-02-10", "Barbell Bench Press", "Standard Set", 95,         NA,                       5,     2,    79,
    "2030-02-10", "Barbell Bench Press", "Standard Set", 100,        NA,                       3,     4,    79,   # RIR too high
    "2030-02-10", "Cable Curl",          "Standard Set", 30,         NA,                       12,    1,    79    # not in a group
  ) %>% dplyr::mutate(date = as.Date(date))
}
fake_lifts <- function() {
  tibble::tibble(exercise = c("Barbell Bench Press", "Hack Squat", "Front Squat"),
                 group = c("Bench", "Squat", "Squat"), main = c(TRUE, FALSE, TRUE))
}
fake_strength_goal <- function() {
  list(target_date = "2030-12-31", targets_kg = list(Bench = 110, Squat = 130),
       ceilings_kg = list(steady = list(Bench = c(110, 120), Squat = c(140, 150))))
}

# Synthetic forecast config (goals.yml shape): bench only, too few sessions
# to fit, so it starts from the override.
fake_forecast_config <- function() {
  list(
    strength_forecast = list(
      exercises = list(bench = "Barbell Bench Press"), targets_kg = list(bench = 110),
      thresholds_bw = list(bench = 1.2), single_ratio = list(bench = 0.96), threshold_bodyweight_kg = 80,
      round_start = "2030-01-01", gap_cap_days = 14, min_sessions_fit = 8, horizon = "2032-12-31",
      start_override = list(bench = list(date = "2030-03-01", e1rm_kg = 100, note = "invented")),
      first_round = list(origin = "2025-01-01", fitted_until = "2026-12-31",
                         bench = list(exp = list(c = 100, y0 = 70, tau = 5), log = list(a = 68, b = 8, s = 0.5)))
    ),
    strength_ceilings = list(target_body = list(bodyweight_kg = 80, body_fat = 0.15, height_cm = 180),
                             aptitude = list(bench = c(0.65, 0.75, 0.9)))
  )
}
