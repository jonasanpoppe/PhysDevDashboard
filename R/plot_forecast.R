library(dplyr)
library(ggplot2)

# Strength forecast chart. Needs R/theme_physdev.R and R/forecast.R.
#
#   history      earlier rounds: monthly bests (or every session), grey
#   first round  the cached exponential fit, solid where it was fitted and
#                dashed after, with the range to the logarithmic shape
#   this round   session bests of the exact lift, coral
#   forecast     the chosen aptitude, with the p10-p90 band
#   target       dotted; the threshold on the bar dashed, with its attempt
#                level (the e1RM at which a single at the threshold is likely)

forecast_colours <- function() {
  list(history = physdev$fg_subtle, first = physdev$cat[[2]], round = physdev$coral,
       forecast = physdev$cat[[3]], target = physdev$cat[[5]], threshold = physdev$fg_muted)
}

# Best session e1RM per calendar month (date = the month's first day).
monthly_bests <- function(sessions) {
  if (nrow(sessions) == 0) return(tibble(date = as.Date(character()), e1rm = numeric(), target_rpe = logical()))
  sessions %>%
    mutate(date = as.Date(date), month = as.Date(format(date, "%Y-%m-01"))) %>%
    summarise(e1rm = max(e1rm), target_rpe = any(coalesce(rir_basis == "target RPE", FALSE)), .by = month) %>%
    rename(date = month)
}

# sessions: strength_sessions; fl: forecast_lift() with bands p10, chosen, p90;
# first: strength_forecast_first_round rows for the lift (or NULL).
plot_lift_forecast <- function(sessions, fl, first = NULL, target_kg = NA, threshold_kg = NA,
                               attempt_kg = NA, from = NULL, monthly = TRUE) {
  col <- forecast_colours()
  ex <- fl$params$exercise[1]
  round_start <- min(fl$round$date, fl$from)
  hist <- sessions %>% filter(lift == ex, as.Date(date) < round_start) %>% mutate(date = as.Date(date))
  hist <- if (monthly) monthly_bests(hist) else
    transmute(hist, date, e1rm, target_rpe = coalesce(rir_basis == "target RPE", FALSE))
  chosen <- fl$params$band[2]
  band <- fl$curve %>% filter(band %in% c("p10", "p90")) %>%
    select(date, band, e1rm) %>% tidyr::pivot_wider(names_from = band, values_from = e1rm)
  mid <- fl$curve %>% filter(band == chosen)
  horizon <- max(fl$curve$date)
  from <- as.Date(from %||% min(c(hist$date, fl$round$date, fl$from)))

  p <- ggplot()
  if (!is.null(first) && nrow(first) > 0) {
    first <- first %>% filter(date >= from, date <= horizon)
    p <- p +
      geom_ribbon(data = first, aes(date, ymin = pmin(exponential, logarithmic), ymax = pmax(exponential, logarithmic)),
                  fill = col$first, alpha = 0.12) +
      geom_line(data = filter(first, fitted_part), aes(date, exponential), colour = col$first, linewidth = 0.7) +
      geom_line(data = filter(first, !fitted_part), aes(date, exponential), colour = col$first, linewidth = 0.7,
                linetype = "22")
  }
  p <- p +
    geom_ribbon(data = band, aes(date, ymin = p10, ymax = p90), fill = col$forecast, alpha = 0.2) +
    geom_line(data = mid, aes(date, e1rm), colour = col$forecast, linewidth = 1) +
    geom_point(data = filter(hist, date >= from), aes(date, e1rm, shape = target_rpe), colour = col$history,
               fill = physdev$bg, size = if (monthly) 1.8 else 1.3) +
    geom_point(data = fl$round, aes(date, e1rm), colour = col$round, shape = 15, size = 2.2) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 21), guide = "none")
  lab_x <- from
  if (!is.na(target_kg)) {
    p <- p + geom_hline(yintercept = target_kg, colour = col$target, linetype = "13", linewidth = 0.8) +
      annotate("text", x = lab_x, y = target_kg, label = sprintf("target %g kg", target_kg), hjust = 0,
               vjust = -0.5, size = 3, colour = col$target)
  }
  if (!is.na(threshold_kg)) {
    p <- p + geom_hline(yintercept = threshold_kg, colour = col$threshold, linetype = "42", linewidth = 0.5) +
      annotate("text", x = lab_x, y = threshold_kg, label = sprintf("threshold %g kg on the bar", threshold_kg),
               hjust = 0, vjust = 1.5, size = 3, colour = col$threshold)
    if (!is.na(attempt_kg)) {
      p <- p + annotate("segment", x = horizon - 120, xend = horizon, y = attempt_kg, yend = attempt_kg,
                        colour = col$threshold, linewidth = 0.5) +
        annotate("text", x = horizon - 120, y = attempt_kg, label = sprintf("attempt from e1RM %.0f", attempt_kg),
                 hjust = 1.05, size = 3, colour = col$threshold)
    }
  }
  p +
    scale_x_date(limits = c(from, horizon), date_labels = "%Y", date_breaks = "1 year",
                 expand = expansion(mult = 0.01)) +
    labs(x = NULL, y = "Estimated 1RM, bar weight (kg)") +
    theme_physdev(grid = "y")
}
