library(dplyr)
library(ggplot2)

# Engine: estimated W/kg against effort duration (log scale), one dot per
# clean ascent, with its uncertainty as a whisker. Background bands: Garmin's
# FTP categories for men (after Allen & Coggan, Training and Racing with a
# Power Meter, 2010). They describe threshold power (~1 hour): only dots far
# to the right compare directly; shorter efforts naturally sit higher.
# Needs R/theme_physdev.R.

ftp_categories_men <- tibble::tribble(
  ~category,   ~low,  ~high,
  "Untrained", -Inf,  2.23,
  "Fair",      2.23,  2.79,
  "Good",      2.79,  3.93,
  "Excellent", 3.93,  5.05,
  "Superior",  5.05,  Inf
)

# Fixed colour per calendar year, anchored at 2023: a year has the same
# colour in every view and whatever is filtered (colour follows the entity,
# not its rank among the years shown).
engine_year_colours <- function(years) {
  years <- sort(unique(as.integer(years)))
  stats::setNames(physdev$cat[((years - 2023L) %% length(physdev$cat)) + 1L], years)
}

plot_engine <- function(efforts, year_colours = engine_year_colours(lubridate_year(efforts$date)),
                        show_range = TRUE, label_climbs = character()) {
  d <- efforts %>%
    filter(!is.na(wkg), moving_s > 0) %>%
    mutate(
      year = factor(lubridate_year(date), levels = names(year_colours)),
      wkg_low = wkg * power_low / power_w,
      wkg_high = wkg * power_high / power_w,
      # log10 by hand on a plain axis: bands can then span -Inf..Inf
      log_min = log10(moving_s / 60)
    )

  # Visible range: bands are clipped to it, so each label sits in its band
  y_top <- max(c(d$wkg_high, 5.3), na.rm = TRUE)
  y_bottom <- max(1.5, min(d$wkg_low, na.rm = TRUE) - 0.2)
  bands <- ftp_categories_men %>%
    mutate(ymin = pmax(low, y_bottom), ymax = pmin(high, y_top),
           mid = (ymin + ymax) / 2, shade = row_number() %% 2 == 0) %>%
    filter(ymax > ymin)

  p <- ggplot(d, aes(log_min, wkg)) +
    geom_rect(data = bands, inherit.aes = FALSE,
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
              fill = ifelse(bands$shade, physdev$bg_card, physdev$bg), colour = NA) +
    geom_text(data = bands, inherit.aes = FALSE,
              aes(x = Inf, y = mid, label = category),
              hjust = 1.1, size = 3, colour = physdev$fg_muted)

  if (show_range) {
    p <- p + geom_linerange(aes(ymin = wkg_low, ymax = wkg_high, colour = year),
                            alpha = 0.35, linewidth = 0.6)
  }

  p <- p +
    # Filled dots; short efforts hollow (they read high)
    geom_point(data = filter(d, !short), aes(fill = year), shape = 21, size = 2.8,
               colour = physdev$bg, stroke = 0.6) +
    geom_point(data = filter(d, short), aes(colour = year), shape = 21, size = 2.8,
               fill = physdev$bg, stroke = 1) +
    scale_x_continuous(breaks = log10(c(0.5, 1, 2, 3, 5, 10, 20, 30)),
                       labels = c("30 s", "1 min", "2", "3", "5", "10", "20", "30 min")) +
    scale_y_continuous(breaks = seq(0, 7, 0.5), expand = expansion(mult = c(0, 0.02))) +
    coord_cartesian(ylim = c(y_bottom, y_top)) +
    scale_fill_manual(values = year_colours, name = NULL, drop = TRUE) +
    scale_colour_manual(values = year_colours, guide = "none", drop = TRUE) +
    labs(x = "Moving time on the climb (log scale)", y = "Estimated W/kg") +
    theme_physdev(grid = "none") +
    theme(legend.position = "top", legend.justification = "left")

  lab <- d %>% filter(climb %in% label_climbs) %>% slice_max(moving_s, n = 1, by = climb)
  if (nrow(lab) > 0) {
    # Left of the dot: the band labels own the right edge
    p <- p + geom_text(data = lab, aes(label = climb), hjust = 1.15, vjust = -0.6,
                       size = 3.2, colour = physdev$fg)
  }
  p
}

lubridate_year <- function(date) as.integer(format(as.Date(date), "%Y"))
