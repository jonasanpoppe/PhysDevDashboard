library(dplyr)
library(ggplot2)

# Analytics charts. Needs R/theme_physdev.R and R/plot_engine.R (year colours).
#   plot_analytics_time     X and Y stacked on one date axis (never a dual axis)
#   plot_analytics_scatter  X against Y (Y lagged), points coloured by year,
#                           a straight trend line with its band
#   plot_analytics_matrix   Spearman's rho for all pairs, diverging around 0

analytics_axis_label <- function(s) {
  model <- if (s$kind == "model output") " (model)" else ""
  sprintf("%s%s, %s", s$series_label, model, s$unit)
}

# d: date, value, panel (one or two panels). Lines break at gaps over a week;
# sparse series (e1RM, W/kg) are drawn as points.
plot_analytics_time <- function(d, sparse = character()) {
  d <- d %>%
    mutate(panel = factor(panel, levels = unique(panel))) %>%   # X on top, as given
    arrange(panel, date) %>%
    mutate(run = cumsum(c(TRUE, diff(date) > 7)), .by = panel)
  dense <- filter(d, !panel %in% sparse)
  p <- ggplot(d, aes(date, value)) +
    geom_point(data = filter(d, panel %in% sparse), colour = physdev$coral, size = 2) +
    geom_line(data = dense, aes(group = run), colour = physdev$coral, linewidth = 0.7) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_x_date(date_labels = "%b %Y") +
    labs(x = NULL, y = NULL) +
    theme_physdev(grid = "y") +
    theme(strip.text = element_text(hjust = 0, face = "bold"))
  p
}

plot_analytics_scatter <- function(pair, xlab, ylab) {
  colours <- engine_year_colours(sort(unique(pair$year)))
  d <- pair %>% mutate(year = factor(year, levels = names(colours)))
  ggplot(d, aes(x, y)) +
    geom_smooth(method = "lm", formula = y ~ x, colour = physdev$fg_muted, fill = physdev$fg_subtle,
                alpha = 0.25, linewidth = 0.7) +
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
    scale_fill_gradient2(low = "#5B7FA6", mid = "#ECEAE3", high = "#C96442", midpoint = 0,
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
