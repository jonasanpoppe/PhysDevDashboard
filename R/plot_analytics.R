library(dplyr)
library(ggplot2)

# Analytics charts. Needs R/theme_physdev.R and R/plot_engine.R (year colours).
#   plot_analytics_time     up to three series stacked on one date axis (never a
#                           dual axis), or z-scores overlaid in one panel
#   plot_analytics_scatter  X against Y (Y lagged), points coloured by year,
#                           with the chosen fit and its band (straight line,
#                           quadratic, log of X, or LOESS for the eye)
#   plot_analytics_matrix   Spearman's rho for all pairs, diverging around 0

analytics_axis_label <- function(s) {
  model <- if (s$kind == "model output") " (model)" else ""
  sprintf("%s%s, %s", s$series_label, model, s$unit)
}

# d: date, value, panel (up to three panels, stacked, never a dual axis).
# Lines break at gaps over a week; sparse series (e1RM, W/kg) are drawn as
# points. Optional columns:
#   trend  drawn thin and grey over the series (the trend a detrend removes)
#   edge   TRUE where a detrend's window was cut short: drawn lighter (the
#          trend line when one is shown, else the series)
# detrended: panels that get a zero line. overlay: all series in one panel,
# one colour each (for z-scores, which share a scale), ylab on the axis.
plot_analytics_time <- function(d, sparse = character(), detrended = character(),
                                overlay = FALSE, ylab = NULL) {
  if (!"trend" %in% names(d)) d$trend <- NA_real_
  if (!"edge" %in% names(d)) d$edge <- FALSE
  d <- d %>%
    mutate(panel = factor(panel, levels = unique(panel)), edge = edge %in% TRUE) %>%   # X on top, as given
    arrange(panel, date) %>%
    mutate(run = cumsum(c(TRUE, diff(date) > 7)), .by = panel)
  has_trend <- any(!is.na(d$trend))
  d$fade <- if (has_trend) FALSE else d$edge
  dense <- filter(d, !panel %in% sparse)
  alpha <- scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.35), guide = "none")
  p <- ggplot(d, aes(date, value))
  zero <- if (overlay) unique(d$panel) else intersect(detrended, unique(as.character(d$panel)))
  if (length(zero) > 0) {
    p <- p + geom_hline(data = tibble(panel = factor(zero, levels = levels(d$panel))),
                        aes(yintercept = 0), colour = physdev$fg_muted, linewidth = 0.4)
  }
  if (overlay) {
    cols <- setNames(physdev$cat[seq_along(levels(d$panel))], levels(d$panel))
    p <- p +
      geom_point(data = filter(d, panel %in% sparse), aes(colour = panel), size = 2) +
      geom_line(data = dense, aes(group = interaction(panel, run), colour = panel, alpha = fade),
                linewidth = 0.6) +
      scale_colour_manual(values = cols, name = NULL) +
      theme_physdev(grid = "y") +
      theme(legend.position = "top", legend.justification = "left")
  } else {
    p <- p +
      geom_point(data = filter(d, panel %in% sparse), colour = physdev$accent, size = 2) +
      geom_line(data = dense, aes(group = run, alpha = fade), colour = physdev$accent, linewidth = 0.7) +
      facet_wrap(~panel, ncol = 1, scales = "free_y") +
      theme_physdev(grid = "y") +
      theme(strip.text = element_text(hjust = 0, face = "bold"))
  }
  if (has_trend) {
    p <- p + geom_line(data = filter(d, !is.na(trend)), aes(y = trend, group = run, alpha = edge),
                       colour = physdev$fg_muted, linewidth = 0.6)
  }
  p + alpha + scale_x_date(date_labels = "%b %Y") + labs(x = NULL, y = ylab)
}

plot_analytics_scatter <- function(pair, xlab, ylab, fit = "linear") {
  colours <- engine_year_colours(sort(unique(pair$year)))
  d <- pair %>% mutate(year = factor(year, levels = names(colours)))
  if (fit == "log") d <- filter(d, x > 0)
  smooth <- if (fit == "loess") {
    geom_smooth(method = "loess", formula = y ~ x, span = 0.75, colour = physdev$fg_muted,
                fill = physdev$fg_subtle, alpha = 0.25, linewidth = 0.7)
  } else {
    geom_smooth(method = "lm", formula = analytics_fit_formula(fit), colour = physdev$fg_muted,
                fill = physdev$fg_subtle, alpha = 0.25, linewidth = 0.7)
  }
  ggplot(d, aes(x, y)) +
    smooth +
    geom_point(aes(fill = year), shape = 21, size = 2.6, colour = physdev$bg, stroke = 0.4, alpha = 0.85) +
    scale_fill_manual(values = colours, name = NULL) +
    labs(x = xlab, y = ylab) +
    theme_physdev(grid = "both") +
    theme(legend.position = "top", legend.justification = "left")
}

# m: x, y, rho, n (series keys); labels: named vector key -> label.
plot_analytics_matrix <- function(m, labels) {
  # Leave out series without a single pair (e.g. a sparse lift with few days)
  paired <- m %>% filter(x != y, !is.na(rho))
  lv <- names(labels)[names(labels) %in% unique(c(paired$x, paired$y))]
  m <- m %>% filter(x %in% lv, y %in% lv)
  d <- m %>% mutate(x = factor(x, levels = lv), y = factor(y, levels = rev(lv)),
                    text = ifelse(is.na(rho), "", sub("^(-?)0\\.", "\\1.", sprintf("%.1f", rho))))
  ggplot(d, aes(x, y, fill = rho)) +
    geom_tile(colour = physdev$bg, linewidth = 0.6) +
    geom_text(aes(label = text), size = 2.4, colour = physdev$fg) +
    scale_fill_gradient2(low = physdev$div_high, mid = physdev$bg_card, high = physdev$div_low, midpoint = 0,
                         limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1), na.value = physdev$bg,
                         name = "Spearman's rho",
                         guide = guide_colourbar(barwidth = grid::unit(14, "lines"), title.vjust = 0.8)) +
    scale_x_discrete(labels = labels, position = "top") +
    scale_y_discrete(labels = labels) +
    labs(x = "Y (lagged)", y = "X") +
    coord_equal() +
    theme_physdev(grid = "none") +
    theme(axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0),
          legend.position = "bottom", panel.grid = element_blank(),
          plot.margin = margin(5, 80, 5, 5))
}

# The strongest pairs from a matrix, each pair once (at lag 0 the matrix is
# symmetric). Same-family pairs are already blank in the matrix.
analytics_top_pairs <- function(m, series, k = 12, lag = 0L) {
  m %>%
    filter(!is.na(rho)) %>%
    filter(lag > 0 | x < y) %>%
    arrange(desc(abs(rho))) %>%
    head(k)
}
