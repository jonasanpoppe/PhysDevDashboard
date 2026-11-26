# Plan: hours per week in the training plan's heart-rate bands (Zone 2,
# sweet spot, above the cap). Chart: plot_plan_bands() in R/plot_riding.R.

plan_controls <- function(id) {
  ns <- NS(id)
  radioButtons(ns("period"), "Period",
               choices = c("Last 6 months" = "6m", "Last year" = "1y", "Everything" = "all"))
}

plan_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("Time in the plan's heart-rate bands"),
    fill_plot(ns("plot"), min_height = 300),
    card_footer(class = "small text-muted", textOutput(ns("footer"), inline = TRUE))
  )
}

plan_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    bands <- query("SELECT * FROM strava_plan_bands")
    limits <- query("SELECT * FROM riding_plan_bands")
    last <- max(as.Date(bands$date))
    output$plot <- renderPlot({
      ui_mode()   # redraw when corporate mode flips (colours come from physdev)
      from <- switch(input$period %||% "6m", "6m" = last - 182, "1y" = last - 365, all = NULL)
      plot_plan_bands(bands, from)
    }, res = 96)
    output$footer <- renderText(sprintf(
      "Seconds per ride at each heart rate, summed per week. Zone 2: %s–%s bpm; sweet spot: %s–%s bpm; above the cap: over %s bpm. Judge rides on time in band, not average heart rate. Heart rate before the chest strap came from a wrist sensor and reads differently.",
      limits$z2_low, limits$z2_high, limits$ss_low, limits$ss_high, limits$cap))
  })
}
