# Vitals module: resting HR, HRV with its band, sleep, breathing rate, SpO2
# and skin temperature, with Fitbit and Garmin as separate series.
# The chart itself is plot_vitals() in R/plot_vitals.R.

vitals_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      open = sidebar_open,   # closed on phones: the figure first
      title = "Vitals",
      radioButtons(
        ns("range"), "Period",
        choices = c("Last 90 days" = "90", "Last year" = "365", "Everything" = "all"),
        selected = "90"
      ),
      checkboxGroupInput(
        ns("metrics"), "Metrics",
        choices = setNames(vitals_panels$metric, vitals_panels$label),
        selected = vitals_panels$metric
      ),
      checkboxInput(ns("points"), "Show nightly values", value = TRUE)
    ),
    card(
      full_screen = TRUE,
      fill_plot(ns("plot")),
      # Its minimum height follows the number of panel rows (set below)
      uiOutput(ns("plot_min_height"), fill = FALSE),
      card_footer(
        class = "small text-muted",
        "Points are nightly values; lines are 7-day means within one device.",
        "HRV band: Garmin's own baseline in the Garmin era; in the Fitbit era a",
        "derived band (mean ± 1 SD of ln RMSSD over the previous 60 nights).",
        "Fitbit skin temperature is a deviation from Fitbit's 30-day baseline",
        "(derived). Devices are never spliced: the dashed line marks the switch."
      )
    )
  )
}

vitals_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Read once per session; the build replaces the database, not the app.
    vitals <- query("SELECT * FROM vitals_daily")
    switch_date <- vitals_switch_date(vitals)
    last_day <- max(as.Date(vitals$date))

    metrics <- reactive({
      validate(need(length(input$metrics) > 0, "Pick at least one metric."))
      input$metrics
    })

    output$plot <- renderPlot({
      ui_mode()   # redraw when corporate mode flips (colours come from physdev)
      from <- if (input$range == "all") NULL else last_day - as.numeric(input$range) + 1
      plot_vitals(vitals, from = from, switch_date = switch_date,
                  metrics = metrics(), show_points = isTRUE(input$points))
    }, res = 96) %>%
      bindCache(plot_key(input, "plot"))

    # Fills the card, but never below ~200 px per row of two panels plus
    # room for the legend; past that the card scrolls.
    output$plot_min_height <- renderUI({
      rows <- ceiling(max(length(input$metrics), 1) / 2)
      tags$style(sprintf("#%s { min-height: %dpx !important; }", session$ns("plot"), 60 + 200 * rows))
    })
  })
}
