# Plan: hours per week in the training plan's heart-rate bands (Zone 2,
# sweet spot, above the cap). Chart: plot_plan_bands() in R/plot_riding.R.

plan_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(
      class = "d-flex flex-wrap align-items-center gap-3",
      "Time in the plan's heart-rate bands",
      radioButtons(ns("period"), NULL, inline = TRUE,
                   choices = c("Last 6 months" = "6m", "Last year" = "1y", "Everything" = "all"))
    ),
    plotOutput(ns("plot"), height = "340px"),
    card_footer(class = "small text-muted", textOutput(ns("footer"), inline = TRUE))
  )
}

plan_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    bands <- query("SELECT * FROM strava_plan_bands")
    limits <- query("SELECT * FROM riding_plan_bands")
    last <- max(as.Date(bands$date))
    output$plot <- renderPlot({
      from <- switch(input$period %||% "6m", "6m" = last - 182, "1y" = last - 365, all = NULL)
      plot_plan_bands(bands, from)
    }, res = 96)
    output$footer <- renderText(sprintf(
      "Seconds per ride at each heart rate, summed per week. Zone 2: %s–%s bpm; sweet spot: %s–%s bpm; above the cap: over %s bpm. Judge rides on time in band, not average heart rate. Heart rate before the chest strap came from a wrist sensor and reads differently.",
      limits$z2_low, limits$z2_high, limits$ss_low, limits$ss_high, limits$cap))
  })
}
