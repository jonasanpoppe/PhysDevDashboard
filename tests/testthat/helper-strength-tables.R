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
