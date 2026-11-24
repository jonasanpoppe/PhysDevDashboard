# Loaded by testthat before the tests (helper-*.R).

# Synthetic tables shaped like strava_climb_efforts, climbs and bikes.
fake_climb_tables <- function() {
  efforts <- tibble::tribble(
    ~activity_id, ~date,        ~start_local,          ~climb,        ~moving_s, ~stop_s, ~status, ~bike_id, ~in_record_scope,
    "1",          "2024-04-01", "2024-04-01T10:00:00", "Example Berg", 300,      70,      "early", "hybrid", TRUE,
    "2",          "2024-05-01", "2024-05-01T10:00:00", "Example Berg", 250,      0,       "clean", "hybrid", TRUE,
    "3",          "2024-06-01", "2024-06-01T10:00:00", "Example Berg", 240,      5,       "top",   "roadie", FALSE,
    "3",          "2024-06-01", "2024-06-01T10:00:00", "Pinned Muur",  200,      90,      "early", "roadie", FALSE,
    "4",          "2024-07-01", "2024-07-01T10:00:00", "Flat Kouter",  100,      0,       "clean", "roadie", TRUE
  )
  climbs <- tibble::tribble(
    ~name,          ~surface,  ~length_m, ~gain_m, ~avg_grade_pct, ~pinned,
    "Example Berg", "cobbles", 900,       60,      6.7,            FALSE,
    "Pinned Muur",  "cobbles", 1100,      90,      8.2,            TRUE,
    "Flat Kouter",  NA,        700,       25,      3.6,            FALSE
  )
  bikes <- tibble::tribble(
    ~bike_id, ~bike,             ~nickname,
    "hybrid", "Example Hybrid 2", NA,
    "roadie", "Example Road 1",   "Roadie"
  )
  list(efforts = efforts, climbs = climbs, bikes = bikes)
}
