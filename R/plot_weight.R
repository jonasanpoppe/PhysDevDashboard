library(dplyr)
library(ggplot2)

# Weight against the plan. Needs R/theme_physdev.R.
#   plot_weight_trend   weigh-ins (dots, measurements), MacroFactor trend
#                       (line, a model output) and the goal pace (dashed),
#                       the pace running on to the target
#   plot_weight_weeks   change in trend weight per week; plan weeks against
#                       their planned change (tick), met or missed; weeks
#                       before the plan in grey as context
#   plot_weight_intake  weekly averages of one metric, with its goal if any

weight_week_colours <- c(Met = "#5E8C6A", Missed = "#B5533C", "Before the plan" = "#B9B4AB")

plot_weight_trend <- function(daily, goal, from = NULL) {
  d <- daily %>% mutate(date = as.Date(date))
  if (!is.null(from)) d <- filter(d, date >= from)
  start <- as.Date(goal$start_date)
  start_kg <- daily$trend_kg[as.Date(daily$date) == start]
  eta <- pace_eta(start, start_kg, goal)
  pace <- tibble(date = seq(start, max(eta, as.Date(goal$checkpoint)), by = "day")) %>%
    mutate(kg = pace_curve(date, start, start_kg, goal))
  ggplot(d, aes(date)) +
    geom_point(aes(y = scale_kg), colour = physdev$fg_subtle, size = 1, na.rm = TRUE) +
    geom_line(aes(y = trend_kg), colour = physdev$fg, linewidth = 0.8) +
    geom_line(data = pace, aes(y = kg), colour = physdev$coral, linewidth = 0.7, linetype = "22") +
    geom_hline(yintercept = goal$target_kg, colour = physdev$fg_subtle, linewidth = 0.3) +
    annotate("text", x = min(d$date), y = goal$target_kg, label = paste(goal$target_kg, "kg goal"),
             hjust = 0, vjust = -0.6, size = 3, colour = physdev$fg_muted) +
    annotate("text", x = start, y = start_kg, label = "goal pace", hjust = -0.15, vjust = -0.9,
             size = 3, colour = physdev$coral) +
    scale_x_date(date_labels = "%b %Y", expand = expansion(mult = c(0.01, 0.04))) +
    labs(x = NULL, y = "Weight (kg)") +
    theme_physdev(grid = "y")
}

plot_weight_weeks <- function(weekly) {
  d <- weekly %>%
    mutate(week = as.Date(week),
           status = case_when(!in_plan ~ "Before the plan", met ~ "Met", TRUE ~ "Missed"),
           status = factor(status, levels = names(weight_week_colours))) %>%
    filter(!is.na(trend_change))
  ggplot(d, aes(week, trend_change)) +
    geom_hline(yintercept = 0, colour = physdev$fg_subtle, linewidth = 0.3) +
    geom_col(aes(fill = status), width = 5, alpha = ifelse(d$complete, 1, 0.5)) +
    geom_errorbar(data = filter(d, in_plan), aes(ymin = goal_change, ymax = goal_change),
                  width = 6, colour = physdev$fg, linewidth = 0.7) +
    scale_fill_manual(values = weight_week_colours, name = NULL, drop = TRUE) +
    scale_x_date(date_labels = "%d %b", expand = expansion(mult = 0.02)) +
    labs(x = "Week starting", y = "Change in trend weight (kg)") +
    theme_physdev(grid = "y") +
    theme(legend.position = "top", legend.justification = "left")
}

weight_intake_metrics <- c(kcal = "Calories (kcal a day)", protein_g = "Protein (g a day)",
                           steps = "Steps a day")

plot_weight_intake <- function(weekly, metric = "kcal", goal_value = NULL) {
  d <- weekly %>% mutate(week = as.Date(week), value = .data[[metric]]) %>% filter(!is.na(value))
  p <- ggplot(d, aes(week, value)) +
    geom_col(fill = physdev$cat[[2]], width = 5, alpha = ifelse(d$complete, 1, 0.5)) +
    scale_x_date(date_labels = "%d %b", expand = expansion(mult = 0.02)) +
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.08))) +
    labs(x = "Week starting", y = weight_intake_metrics[[metric]]) +
    theme_physdev(grid = "y")
  if (!is.null(goal_value)) {
    p <- p + geom_hline(yintercept = goal_value, colour = physdev$coral, linewidth = 0.6, linetype = "22") +
      annotate("text", x = min(d$week), y = goal_value, label = "goal", hjust = 0, vjust = -0.6,
               size = 3, colour = physdev$coral)
  }
  p
}
