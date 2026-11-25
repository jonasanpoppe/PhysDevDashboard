# A plot that fills the space its card has, never shorter than min_height.
# Pair it with renderPlot() without a height: the default ("auto") draws at
# the size the browser reports, so the figure rescales with the window.
fill_plot <- function(id, min_height = 300) {
  tagAppendAttributes(
    plotOutput(id, height = "100%", fill = TRUE),
    style = sprintf("min-height: %dpx;", min_height)
  )
}
