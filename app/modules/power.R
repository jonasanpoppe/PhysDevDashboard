# Power by duration: estimated W/kg against duration for every clean ascent,
# against Garmin's FTP categories. Chart: plot_engine() in R/plot_engine.R.

power_controls <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("year_filter")),
    checkboxInput(ns("range"), "Show the ±15% range", value = TRUE),
    checkboxInput(ns("short"), "Include efforts under 90 s", value = TRUE)
  )
}

power_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(textOutput(ns("summary"), inline = TRUE) %>% tagAppendAttributes(class = "small text-muted")),
    fill_plot(ns("plot"), min_height = 420),
    card_footer(
      class = "small text-muted",
      "One dot per clean ascent; hollow dots are under 90 s and read high (run-up momentum).",
      "All watts are estimates from the climb's rise, length and surface, your trend weight and the bike's mass (±15%).",
      "Wind is ignored. Strava's own estimate runs 10–15% lower, but ratios between efforts hold.",
      "Bands: Garmin's FTP categories for men (after Allen & Coggan); they describe about an hour's effort, so only the long climbs compare directly."
    )
  )
}

power_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    efforts <- query("SELECT * FROM strava_climb_efforts WHERE status IN ('clean', 'top') AND wkg IS NOT NULL")
    labelled <- query("SELECT name FROM climbs WHERE engine_label")$name
    years <- sort(unique(lubridate_year(efforts$date)))
    colours <- engine_year_colours(years)

    output$year_filter <- renderUI({
      checkboxGroupInput(ns("years"), "Years", choices = years, selected = years, inline = TRUE)
    })
    shown <- reactive({
      req(input$years)
      e <- efforts[lubridate_year(efforts$date) %in% as.integer(input$years), ]
      if (!isTRUE(input$short)) e <- e[!e$short, ]
      validate(need(nrow(e) > 0, "No clean ascents with these filters."))
      e
    })
    output$summary <- renderText({
      e <- shown(); sprintf("%d ascents · %d climbs", nrow(e), length(unique(e$climb)))
    })
    output$plot <- renderPlot({
      ui_mode()   # redraw when corporate mode flips (colours come from physdev)
      plot_engine(shown(), year_colours = colours, show_range = isTRUE(input$range),
                  label_climbs = labelled)
    }, res = 96)
  })
}
