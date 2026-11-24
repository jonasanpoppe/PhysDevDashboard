library(dplyr)
library(ggplot2)

# Strength charts. Needs R/theme_physdev.R.
#   plot_strength_lifts      e1RM per session for the lifts of one group, the
#                            main lift's target dashed
#   plot_strength_targets    each group's main lift now (best of the recent
#                            window) against its target and the ceilings
#   plot_strength_standards  the same, as multiples of bodyweight, against
#                            public standards

# "Now" for a lift: the best e1RM in the last `window_days` before the latest
# session of any lift; if the lift has none there, its last session (dated).
strength_now <- function(sessions, window_days = 84) {
  latest <- max(as.Date(sessions$date))
  sessions %>%
    mutate(date = as.Date(date)) %>%
    filter(as.logical(main)) %>%
    mutate(recent = date > latest - window_days) %>%
    group_by(group, lift) %>%
    group_modify(\(d, k) {
      pick <- if (any(d$recent)) d[d$recent, ] else d[d$date == max(d$date), ]
      pick[which.max(pick$e1rm), c("date", "e1rm", "x_bodyweight", "bodyweight_kg", "recent")]
    }) %>%
    ungroup()
}

plot_strength_lifts <- function(sessions, group_name, target_kg = NA) {
  d <- sessions %>% filter(group == group_name) %>% mutate(date = as.Date(date))
  # Fixed colour per lift: main lift first, then by number of sessions
  order <- d %>% count(lift, main) %>% arrange(desc(as.logical(main)), desc(n)) %>% pull(lift)
  colours <- stats::setNames(rep_len(physdev$cat, length(order)), order)
  # Lines only between sessions less than 6 weeks apart: a long gap is a
  # break, not a trend
  d <- d %>% mutate(lift = factor(lift, levels = order)) %>%
    arrange(lift, date) %>%
    mutate(run = cumsum(c(TRUE, diff(date) > 42)), .by = lift)
  p <- ggplot(d, aes(date, e1rm, colour = lift)) +
    geom_line(aes(linewidth = as.logical(main), group = interaction(lift, run)), alpha = 0.8) +
    geom_point(size = 1.8) +
    scale_linewidth_manual(values = c(`TRUE` = 1.1, `FALSE` = 0.5), guide = "none") +
    scale_colour_manual(values = colours, name = NULL) +
    scale_x_date(date_labels = "%b %Y", expand = expansion(mult = 0.02)) +
    labs(x = NULL, y = "Estimated 1RM, bar weight (kg)") +
    theme_physdev(grid = "y") +
    theme(legend.position = "top", legend.justification = "left")
  if (!is.na(target_kg)) {
    p <- p + geom_hline(yintercept = target_kg, colour = physdev$coral, linetype = "22", linewidth = 0.6) +
      annotate("text", x = min(d$date), y = target_kg, label = paste0("target ", target_kg, " kg (main lift)"),
               hjust = 0, vjust = -0.6, size = 3, colour = physdev$coral)
  }
  p
}

plot_strength_targets <- function(now, targets, ceilings) {
  groups <- targets$group
  d <- now %>% filter(group %in% groups) %>%
    left_join(targets %>% select(group, target_kg), by = "group") %>%
    mutate(label = sprintf("%s: %.0f kg%s", lift, e1rm, ifelse(recent, "", format(date, " (last: %b %Y)"))))
  ce <- ceilings %>% filter(group %in% groups)
  profiles <- unique(ce$profile)
  shades <- stats::setNames(c(physdev$bg_card, physdev$fg_subtle)[seq_along(profiles)], profiles)
  y <- function(g) match(g, groups)
  ggplot() +
    geom_rect(data = ce, aes(xmin = low, xmax = high, ymin = y(group) - 0.3, ymax = y(group) + 0.3, fill = profile),
              alpha = 0.6) +
    geom_segment(data = d, aes(x = target_kg, xend = target_kg, y = y(group) - 0.35, yend = y(group) + 0.35),
                 colour = physdev$coral, linewidth = 1) +
    geom_point(data = d, aes(x = e1rm, y = y(group)), size = 3.5, colour = physdev$fg) +
    geom_text(data = d, aes(x = e1rm, y = y(group) + 0.42, label = label), size = 3, colour = physdev$fg,
              hjust = 0.5) +
    scale_y_continuous(breaks = seq_along(groups), labels = groups, expand = expansion(add = 0.6)) +
    scale_x_continuous(expand = expansion(mult = c(0.15, 0.05))) +   # room for labels at the left
    scale_fill_manual(values = shades, name = "Ceiling (3–5 years)",
                      labels = gsub("_", " ", names(shades))) +
    labs(x = "Estimated 1RM, bar weight (kg)", y = NULL) +
    theme_physdev(grid = "x") +
    theme(legend.position = "top", legend.justification = "left")
}

plot_strength_standards <- function(now, standards) {
  groups <- intersect(c("Bench", "Squat", "Deadlift and hinge"), now$group)
  y <- function(g) match(g, groups)
  bands <- standards %>% filter(group %in% groups, !is.na(high))
  ticks <- standards %>% filter(group %in% groups, is.na(high))
  d <- now %>% filter(group %in% groups) %>%
    mutate(label = sprintf("%s: %.2f×", lift, x_bodyweight))
  tick_kinds <- unique(ticks$standard)
  tick_cols <- stats::setNames(c(physdev$cat[[2]], physdev$cat[[4]])[seq_along(tick_kinds)], tick_kinds)
  ggplot() +
    geom_rect(data = bands, aes(xmin = low, xmax = high, ymin = y(group) - 0.3, ymax = y(group) + 0.3,
                                fill = standard)) +
    scale_fill_manual(values = stats::setNames(physdev$bg_card, unique(bands$standard)), name = NULL) +
    geom_segment(data = ticks, aes(x = low, xend = low, y = y(group) - 0.35, yend = y(group) + 0.35,
                                   colour = standard), linewidth = 1) +
    geom_point(data = d, aes(x = x_bodyweight, y = y(group)), size = 3.5, colour = physdev$fg) +
    geom_text(data = d, aes(x = x_bodyweight, y = y(group) + 0.42, label = label), size = 3,
              colour = physdev$fg) +
    scale_colour_manual(values = tick_cols, name = NULL) +
    scale_y_continuous(breaks = seq_along(groups), labels = groups, expand = expansion(add = 0.6)) +
    scale_x_continuous(expand = expansion(mult = c(0.12, 0.05))) +
    labs(x = "Estimated 1RM ÷ trend weight", y = NULL) +
    theme_physdev(grid = "x") +
    theme(legend.position = "top", legend.justification = "left")
}
