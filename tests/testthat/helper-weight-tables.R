# Synthetic MacroFactor-like days and an invented goal (as in config/goals.example.yml).
fake_goal <- function() {
  list(start_date = "2030-01-07", rate_kg_per_week = 0.7, target_kg = 75, pause_at_kg = 78,
       pause_weeks = 1, checkpoint = "2030-03-01", catch_up = FALSE)
}

fake_macrofactor_daily <- function() {
  dates <- as.Date("2029-12-01") + 0:70            # 2029-12-01 .. 2030-02-09
  trend <- 80 - pmax(0, as.numeric(dates - as.Date("2030-01-07"))) * 0.1  # -0.7 kg a week from the start
  tibble::tibble(
    date = dates, weight_kg = ifelse(seq_along(dates) %% 2 == 0, trend + 0.3, NA),
    trend_weight_kg = trend, calories_kcal = 2200, protein_g = 150, steps = 9000,
    expenditure = 2700
  )
}
