library(dplyr)
library(ggplot2)

# Fitness: three stacked charts on one shared date axis, each with its own
# y-axis (never a dual axis):
#   plot_fitness_seasons  watts at the reference heart rate, per season: a
#                         band over the season's dates (its interval) with
#                         the estimate as a line
#   plot_fitness_load     6-week riding and running hours per week
#   plot_fitness_weight   MacroFactor trend weight (a model output)
# Needs R/theme_physdev.R.

fitness_x_scale <- function(xlim) {
  scale_x_date(limits = xlim, date_labels = "%b %Y", expand = expansion(mult = 0.01))
}

plot_fitness_seasons <- function(seasons, xlim, ref_hr_bpm) {
  d <- seasons %>%
    mutate(first_date = as.Date(first_date), last_date = as.Date(last_date),
           label = sprintf("%d: %.0f W (%.0f–%.0f)", season, watts_ref, ci_low, ci_high))
  ggplot(d) +
    geom_rect(aes(xmin = first_date, xmax = last_date, ymin = ci_low, ymax = ci_high),
              fill = physdev$coral, alpha = 0.15) +
    geom_segment(aes(x = first_date, xend = last_date, y = watts_ref, yend = watts_ref),
                 colour = physdev$coral, linewidth = 1) +
    geom_text(aes(x = first_date, y = ci_high, label = label),
              hjust = 0, vjust = -0.6, size = 3.2, colour = physdev$fg) +
    fitness_x_scale(xlim) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
    labs(x = NULL, y = sprintf("Watts at %s bpm", ref_hr_bpm)) +
    theme_physdev(grid = "y")
}

plot_fitness_load <- function(load, xlim) {
  d <- load %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= xlim[1], date <= xlim[2]) %>%
    select(date, Riding = ride_6w_hpw, Running = run_6w_hpw) %>%
    tidyr::pivot_longer(-date, names_to = "kind", values_to = "hpw")
  ggplot(d, aes(date, hpw, colour = kind)) +
    geom_line(linewidth = 0.7) +
    fitness_x_scale(xlim) +
    scale_colour_manual(values = c(Riding = physdev$cat[[2]], Running = physdev$cat[[3]]), name = NULL) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Hours per week (6-week mean)") +
    theme_physdev(grid = "y") +
    theme(legend.position = "top", legend.justification = "left")
}

# Trend weight (line, a model output) and weigh-ins (dots, measurements):
# a long stretch without dots is MacroFactor interpolating.
plot_fitness_weight <- function(weight, xlim) {
  d <- weight %>% mutate(date = as.Date(date)) %>% filter(date >= xlim[1], date <= xlim[2])
  ggplot(d, aes(date)) +
    geom_point(aes(y = scale_kg), colour = physdev$fg_subtle, size = 0.9, na.rm = TRUE) +
    geom_line(aes(y = trend_kg), colour = physdev$fg_muted, linewidth = 0.7) +
    fitness_x_scale(xlim) +
    labs(x = NULL, y = "Weight (kg)") +
    theme_physdev(grid = "y")
}

# Shared date range: from a little before the first modelled season to the
# last day of data.
fitness_xlim <- function(seasons, load) {
  c(min(as.Date(seasons$first_date)) - 60, max(as.Date(load$date)))
}
