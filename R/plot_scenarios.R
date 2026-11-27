library(dplyr)
library(ggplot2)

# Scenario figures (R/scenarios.R): what happened (ink) and the projection
# from today (a line with its range as a band), against the target
# (horizontal) and the deadline (vertical). Projections are model outputs:
# the captions say so. Needs R/theme_physdev.R.
#   plot_scenario_weight  trend weight, then "on the plan" and "at the recent pace"
#   plot_scenario_lift    heavy single on the bar, then the forecast p10-p90
#   plot_scenario_km      km this year, then at the km a week set and the recent pace

scenario_model_colours <- function() c(plan = physdev$cat[1], set = physdev$cat[1], recent = physdev$cat[2],
                                       forecast = physdev$cat[1])

# Target, deadline (and window) marks shared by the figures
scenario_marks <- function(target, deadline, window_start = NULL, unit = "kg") {
  list(
    if (!is.null(window_start)) annotate("rect", xmin = as.Date(window_start), xmax = as.Date(deadline),
                                         ymin = -Inf, ymax = Inf, fill = physdev$fg_subtle, alpha = 0.25),
    geom_hline(yintercept = target, colour = physdev$fg_muted, linewidth = 0.3, linetype = "22"),
    geom_vline(xintercept = as.Date(deadline), colour = physdev$fg_muted, linewidth = 0.3),
    # At the left edge: the projections' end labels sit at the deadline
    annotate("text", x = as.Date(-Inf), y = target,
             label = sprintf("%s %s by %s", fmt_goal(target), unit, sub("^0", "", format(as.Date(deadline), "%d %b"))),
             hjust = -0.05, vjust = -0.6, size = 3.2, colour = physdev$fg_muted)
  )
}

# One projection per model: the band (fast-slow) and the central line,
# labelled at its end
scenario_projection_layers <- function(proj, colours) {
  wide <- proj %>% tidyr::pivot_wider(id_cols = c(model, label, date), names_from = band, values_from = value)
  # Legend in series order: the plan (or the set km, the forecast) first
  ends <- wide %>% group_by(model) %>% slice_max(date, n = 1) %>% ungroup() %>%
    arrange(match(model, c("plan", "set", "forecast", "recent")))
  list(
    geom_ribbon(data = wide, aes(x = date, ymin = pmin(fast, slow), ymax = pmax(fast, slow), fill = label),
                alpha = 0.18, inherit.aes = FALSE),
    geom_line(data = wide, aes(x = date, y = mid, colour = label), linewidth = 0.8, inherit.aes = FALSE),
    geom_text(data = ends, aes(x = date, y = mid, label = model_short(model), colour = label),
              hjust = -0.1, size = 3.2, show.legend = FALSE, inherit.aes = FALSE),
    scale_colour_manual(values = stats::setNames(colours[ends$model], ends$label), breaks = ends$label, name = NULL),
    scale_fill_manual(values = stats::setNames(colours[ends$model], ends$label), breaks = ends$label, name = NULL)
  )
}
model_short <- function(m) c(plan = "plan", recent = "recent", set = "set", forecast = "p50")[m]

plot_scenario_weight <- function(sw, weight, quest, history_days = 120) {
  proj <- weight_projection(sw, quest$deadline)
  hist <- weight %>% mutate(date = as.Date(date)) %>% filter(date > sw$from - history_days, date <= sw$from)
  ggplot() +
    scenario_marks(quest$target, quest$deadline, quest$window_start) +
    geom_line(data = hist, aes(date, trend_kg), colour = physdev$fg, linewidth = 0.8) +
    scenario_projection_layers(proj, scenario_model_colours()) +
    scale_x_date(date_labels = "%b %Y", expand = expansion(mult = c(0.01, 0.08))) +
    labs(x = NULL, y = "Trend weight (kg)") +
    theme_physdev(grid = "y") + theme(legend.position = "top")
}

plot_scenario_lift <- function(sl, strength, quest, history_days = 365) {
  ratio <- as.numeric(quest$single_ratio %||% 1)
  hist <- strength %>% filter(lift == quest$exercise) %>% mutate(date = as.Date(date), value = e1rm * ratio) %>%
    filter(date > max(date) - history_days)
  until <- as.Date(quest$deadline) + 90
  proj <- sl$projection %>% filter(date <= until) %>%
    mutate(model = "forecast", label = "Strength forecast: p50, band p10 to p90")
  ggplot() +
    scenario_marks(sl$target, quest$deadline) +
    geom_point(data = hist, aes(date, value), colour = physdev$fg, size = 1.6) +
    scenario_projection_layers(proj, scenario_model_colours()) +
    scale_x_date(date_labels = "%b %Y", expand = expansion(mult = c(0.01, 0.08))) +
    labs(x = NULL, y = "Heavy single on the bar (kg)") +
    theme_physdev(grid = "y") + theme(legend.position = "top")
}

plot_scenario_km <- function(sk, rides, quest, cfg) {
  year <- as.integer(cfg$year %||% format(sk$start, "%Y"))
  first <- as.Date(sprintf("%d-01-01", year))
  hist <- rides %>% mutate(date = as.Date(date)) %>% filter(date >= first, date <= sk$start) %>%
    arrange(date) %>% mutate(km = cumsum(km))
  days <- seq(sk$start, sk$deadline, by = 7)
  frac <- as.numeric(days - sk$start) / 7
  proj <- purrr::pmap(sk$models, function(model, label, km_week, mid, fast, slow, ...) {
    span <- max(1e-9, sk$weeks_left)
    tibble::tibble(model = model, label = label, date = rep(days, 3),
                   band = rep(c("mid", "fast", "slow"), each = length(days)),
                   value = c(sk$so_far + km_week * frac,
                             sk$so_far + km_week * frac + (fast - mid) * sqrt(frac / span),
                             sk$so_far + km_week * frac - (mid - slow) * sqrt(frac / span)))
  }) %>% purrr::list_rbind()
  ggplot() +
    scenario_marks(sk$target, sk$deadline, unit = "km") +
    {if (nrow(hist)) geom_step(data = hist, aes(date, km), colour = physdev$fg, linewidth = 0.8)} +
    scenario_projection_layers(proj, scenario_model_colours()) +
    scale_x_date(date_labels = "%b %Y", expand = expansion(mult = c(0.01, 0.08))) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x = NULL, y = sprintf("Km ridden in %d", year)) +
    theme_physdev(grid = "y") + theme(legend.position = "top")
}
