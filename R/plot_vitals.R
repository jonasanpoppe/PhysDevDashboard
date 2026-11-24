library(dplyr)
library(ggplot2)

# Vitals small multiples: one panel per metric, each with its own y-axis,
# shared date axis. Device eras are separate series in their own colour,
# with a labelled rule at the device switch; nothing is spliced.
# Daily values are faint points, the 7-day mean (within an era) is the line,
# and HRV carries its band (Garmin's baseline, or the derived Fitbit band).
#
# `vitals` is vitals_daily (see R/build_vitals_daily.R). Needs
# R/theme_physdev.R for the palette and theme.

vitals_panels <- tibble::tribble(
  ~metric,         ~label,                                ~scale,
  "rhr",           "Resting heart rate (bpm)",                 1,
  "hrv",           "HRV, overnight RMSSD (ms)",                1,
  "sleep_s",       "Sleep (h)",                         1 / 3600,
  "resp",          "Breathing rate in sleep (br/min)",         1,
  "spo2",          "SpO2 in sleep (%)",                        1,
  "skin_temp_dev", "Skin temperature, deviation (°C)",         1
)

vitals_era_colours <- function() {
  c(fitbit = physdev$cat[[1]], garmin = physdev$cat[[2]])
}

vitals_era_labels <- c(fitbit = "Fitbit", garmin = "Garmin")

# First day of the Garmin era across all metrics (NA if there is none).
vitals_switch_date <- function(vitals) {
  g <- vitals$date[vitals$era == "garmin"]
  if (length(g) == 0) as.Date(NA) else min(as.Date(g))
}

plot_vitals <- function(vitals, from = NULL, to = NULL,
                        switch_date = vitals_switch_date(vitals),
                        metrics = vitals_panels$metric, show_points = TRUE) {
  panels <- filter(vitals_panels, metric %in% metrics)
  d <- vitals %>%
    mutate(date = as.Date(date)) %>%
    inner_join(panels, by = "metric") %>%
    mutate(
      across(c(value, mean_7d, band_low, band_high), \(x) x * scale),
      panel = factor(label, levels = panels$label)
    )
  if (!is.null(from)) d <- filter(d, date >= from)
  if (!is.null(to))   d <- filter(d, date <= to)
  d <- mutate(d, panel = droplevels(panel))

  colours <- vitals_era_colours()
  span_days <- as.numeric(diff(range(d$date)))
  point_size <- if (span_days > 365) 0.35 else 1.1

  p <- ggplot(d, aes(date, colour = era, fill = era, group = era)) +
    geom_ribbon(
      data = filter(d, !is.na(band_low)),
      aes(ymin = band_low, ymax = band_high),
      colour = NA, alpha = 0.15
    ) +
    # Hidden points get no data (not alpha 0), so the axes fit the lines
    geom_point(data = if (show_points) d else d[0, ],
               aes(y = value), size = point_size, alpha = 0.35, stroke = 0) +
    geom_line(aes(y = mean_7d), linewidth = 0.7, na.rm = TRUE) +
    facet_wrap(~panel, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = colours, labels = vitals_era_labels,
                        name = "Device", drop = TRUE) +
    scale_fill_manual(values = colours, guide = "none") +
    scale_x_date(expand = expansion(mult = 0.01)) +
    labs(x = NULL, y = NULL) +
    guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1, linewidth = 0.7))) +
    theme_physdev(grid = "y") +
    theme(legend.position = "top", legend.justification = "left")

  if (!is.na(switch_date) && switch_date >= min(d$date) && switch_date <= max(d$date)) {
    p <- p +
      geom_vline(xintercept = switch_date, colour = physdev$fg_muted,
                 linewidth = 0.4, linetype = "22") +
      # Labelled once, in the first panel
      geom_text(
        data = tibble::tibble(date = switch_date, panel = levels(d$panel)[1]),
        aes(x = date, y = Inf, label = "Fitbit → Garmin"), inherit.aes = FALSE,
        hjust = -0.05, vjust = 1.5, size = 3, colour = physdev$fg_muted
      )
  }
  p
}
