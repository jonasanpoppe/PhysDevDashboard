library(dplyr)

# The Quest Board's small charts: a card's number over time (quest_history
# in R/quests.R, the same quest_now() as the card's headline), as inline SVG
# so a card stays light and sharp at any size. Colours come from the page's
# CSS (classes spark-*, app/www/oath.css), so the look switch recolours them.
#
#   the line      the card's value, week by week, up to today
#   dashed        the target (ride km: the even pace to the year's target)
#   dotted        where a rate counts as met (met_at), when below the target
#   shaded        the time still to go, up to the end of the plan's year

quest_spark <- function(h, kind, target, from, to, as_of, met_at = NA_real_, year = NA_integer_,
                        width = 300, height = 84) {
  from <- as.Date(from); to <- as.Date(to); as_of <- as.Date(as_of)
  if (nrow(h) == 0 && is.na(target)) return(NULL)
  span <- max(1, as.numeric(to - from))
  x <- function(d) round(pmin(width, pmax(0, as.numeric(as.Date(d) - from) / span * width)), 1)
  pace <- if (identical(kind, "cumulative") && !is.na(target) && !is.na(year)) {
    first <- as.Date(sprintf("%d-01-01", year)); last <- as.Date(sprintf("%d-12-31", year))
    list(x = x(c(first, last)), v = c(0, target))
  }
  ys <- c(h$value, if (is.null(pace)) target else pace$v, met_at)
  ys <- ys[!is.na(ys)]
  lo <- min(ys); hi <- max(ys)
  pad <- if (hi > lo) (hi - lo) * 0.12 else max(1, abs(hi) * 0.05)
  lo <- lo - pad; hi <- hi + pad
  y <- function(v) round(height - (v - lo) / (hi - lo) * height, 1)
  # Presentation attributes as defaults (the stylesheet's classes win), so
  # the chart stays readable even without the stylesheet
  # The chart stretches to its box (a fixed height on wide screens):
  # strokes keep their width and the dot stays round (non-scaling strokes)
  nss <- "non-scaling-stroke"
  line <- function(x1, y1, x2, y2, cls) tags$line(x1 = x1, y1 = y1, x2 = x2, y2 = y2, class = cls,
                                                  stroke = "currentColor", `stroke-opacity` = 0.5,
                                                  `stroke-dasharray` = if (cls != "spark-today") "5 4",
                                                  `vector-effect` = nss)

  today <- x(as_of)
  shapes <- list(
    if (today < width) tags$rect(x = today, y = 0, width = width - today, height = height, class = "spark-future",
                                 fill = "currentColor", `fill-opacity` = 0.05),
    if (!is.null(pace)) line(pace$x[1], y(0), pace$x[2], y(target), "spark-target")
    else if (!is.na(target)) line(0, y(target), width, y(target), "spark-target"),
    if (!is.na(met_at) && !is.na(target) && met_at != target) line(0, y(met_at), width, y(met_at), "spark-met"),
    line(today, 0, today, height, "spark-today")
  )
  if (nrow(h) > 0) {
    h <- arrange(h, date)
    pts <- paste(x(h$date), y(h$value), sep = ",", collapse = " ")
    shapes <- c(shapes, list(
      tags$polyline(points = pts, class = "spark-line", fill = "none", stroke = "currentColor", `stroke-width` = 2.5,
                    `vector-effect` = nss),
      # The dot: a zero-length line with a round cap, round at any stretch
      tags$path(d = sprintf("M%s %s l0 0", x(utils::tail(h$date, 1)), y(utils::tail(h$value, 1))),
                class = "spark-now", stroke = "currentColor", `stroke-width` = 8, `stroke-linecap` = "round",
                `vector-effect` = nss)))
  }
  tag("svg", c(list(class = "oath-spark", viewBox = sprintf("0 0 %d %d", width, height), preserveAspectRatio = "none", role = "img",
                    `aria-label` = "The quest's value over time against its target"), shapes))
}

# Months on the chart's ends and what the dashed line is
quest_spark_axis <- function(from, to, target_text) {
  div(class = "oath-spark-axis",
      span(format(as.Date(from), "%b %Y")),
      if (!is.na(target_text) && nzchar(target_text)) span(class = "oath-spark-key", target_text),
      span(format(as.Date(to), "%b %Y")))
}

# Event and step quests: one pip per stage, filled once done
quest_pips <- function(done, n) {
  if (is.na(n) || n < 1) return(NULL)
  div(class = "oath-pips", lapply(seq_len(n), \(i) span(class = paste("oath-pip", if (i <= done) "oath-pip-done"))))
}
