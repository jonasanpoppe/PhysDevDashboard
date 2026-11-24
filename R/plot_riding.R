library(dplyr)
library(ggplot2)

# Riding charts. Needs R/theme_physdev.R and R/plot_engine.R (year colours).
#   plot_rides_cumulative  km so far by day of year, a line per year, the
#                          target years' even pace dashed
#   plot_rides_monthly     km per month, years side by side
#   plot_benchmark         attempts on the benchmark climb: estimated W/kg
#                          with its range, time and heart rate as labels
#   plot_plan_bands        hours per week in the plan's heart-rate bands

plot_rides_cumulative <- function(cumulative, targets, years = NULL) {
  d <- cumulative
  if (!is.null(years)) d <- filter(d, year %in% years)
  colours <- engine_year_colours(sort(unique(cumulative$year)))
  d <- d %>% mutate(year = factor(year, levels = names(colours)))
  ends <- d %>% slice_max(doy, n = 1, by = year)
  # Even pace to the target, for target years that are shown
  shown_years <- as.integer(as.character(unique(d$year)))
  pace <- targets %>%
    filter(year %in% shown_years) %>%
    tidyr::crossing(doy = c(1, 365)) %>%
    mutate(km = target_km * (doy - 1) / 364, year = factor(year, levels = names(colours)))
  p <- ggplot(d, aes(doy, km_cum, colour = year)) +
    geom_step(linewidth = 0.8) +
    geom_text(data = ends, aes(label = sprintf("%s: %s km", year, format(round(km_cum), big.mark = ","))),
              hjust = -0.05, size = 3, show.legend = FALSE) +
    scale_colour_manual(values = colours, name = NULL, drop = TRUE) +
    scale_x_continuous(breaks = c(1, 91, 182, 274, 365), labels = c("Jan", "Apr", "Jul", "Oct", "Dec"),
                       limits = c(1, 420)) +
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Km so far this year") +
    theme_physdev(grid = "y") +
    theme(legend.position = "top", legend.justification = "left")
  if (nrow(pace) > 0) {
    p <- p + geom_line(data = pace, aes(doy, km, group = year), colour = physdev$coral,
                       linetype = "22", linewidth = 0.6, inherit.aes = FALSE) +
      geom_text(data = filter(pace, doy == 365), aes(doy, km, label = sprintf("%s target %s km", year, format(target_km, big.mark = ","))),
                inherit.aes = FALSE, hjust = -0.05, size = 3, colour = physdev$coral)
  }
  p
}

plot_rides_monthly <- function(rides, years = NULL) {
  colours <- engine_year_colours(sort(unique(rides$year)))
  d <- rides
  if (!is.null(years)) d <- filter(d, year %in% years)
  d <- d %>% summarise(.by = c(year, month), km = sum(km)) %>%
    mutate(year = factor(year, levels = names(colours)))
  ggplot(d, aes(month, km, fill = year)) +
    geom_col(position = position_dodge2(preserve = "single", padding = 0.1), width = 0.85) +
    scale_fill_manual(values = colours, name = NULL, drop = TRUE) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Km per month") +
    theme_physdev(grid = "y") +
    theme(legend.position = "top", legend.justification = "left")
}

plot_benchmark <- function(attempts) {
  d <- attempts %>% mutate(date = as.Date(date), clean = status %in% c("clean", "top"),
                           wkg_low = wkg * power_low / power_w, wkg_high = wkg * power_high / power_w,
                           label = sprintf("%s · %s bpm%s", fmt_mmss(moving_s), round(hr_end),
                                           ifelse(stop_s > 2, sprintf(" · %ss stop", round(stop_s)), "")))
  ggplot(d, aes(date, wkg)) +
    geom_linerange(aes(ymin = wkg_low, ymax = wkg_high), colour = physdev$coral, alpha = 0.35, linewidth = 0.8) +
    geom_point(data = filter(d, clean), shape = 21, size = 3.5, fill = physdev$coral, colour = physdev$bg) +
    geom_point(data = filter(d, !clean), shape = 21, size = 3.5, fill = physdev$bg, colour = physdev$coral, stroke = 1.2) +
    geom_text(aes(label = label), vjust = -1.4, size = 3, colour = physdev$fg) +
    scale_x_date(date_labels = "%b %Y", expand = expansion(mult = 0.1)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
    labs(x = NULL, y = "Estimated W/kg") +
    theme_physdev(grid = "y")
}

plan_band_colours <- c("Zone 2" = "#7A9E7E", "Sweet spot" = "#5B7FA6", "Above the cap" = "#B5533C")

plot_plan_bands <- function(bands, from = NULL) {
  d <- bands %>% mutate(date = as.Date(date))
  if (!is.null(from)) d <- filter(d, date >= from)
  d <- d %>% mutate(week = date - (as.integer(format(date, "%u")) - 1L)) %>%
    summarise(.by = week, `Zone 2` = sum(z2_s) / 3600, `Sweet spot` = sum(sweet_spot_s) / 3600,
              `Above the cap` = sum(above_cap_s) / 3600) %>%
    tidyr::pivot_longer(-week, names_to = "band", values_to = "hours") %>%
    mutate(band = factor(band, levels = names(plan_band_colours)))
  ggplot(d, aes(week, hours, fill = band)) +
    geom_col(width = 5.5) +
    scale_fill_manual(values = plan_band_colours, name = NULL) +
    scale_x_date(date_labels = "%b %Y", expand = expansion(mult = 0.01)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Week starting", y = "Hours in band") +
    theme_physdev(grid = "y") +
    theme(legend.position = "top", legend.justification = "left")
}
