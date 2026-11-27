# The example configs as the pipeline reads them: goals.example.yml holds the
# numbers, oath.example.yml is filled from it (R/plan_config.R).
example_configs <- function() {
  root <- testthat::test_path("..", "..")
  source(file.path(root, "R", "plan_config.R"), local = TRUE)
  lifts <- readr::read_csv(file.path(root, "config", "lifts.example.csv"), show_col_types = FALSE)
  goals <- resolve_goals(yaml::read_yaml(file.path(root, "config", "goals.example.yml")), lifts)
  oath <- oath_from_config(yaml::read_yaml(file.path(root, "config", "oath.example.yml")), goals, lifts)
  list(goals = goals, oath = oath, lifts = lifts)
}
example_oath <- function() example_configs()$oath
