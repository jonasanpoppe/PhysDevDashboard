# Rides: distance against the year's target, month by month, and the best
# rides. Data: strava_rides, riding_targets (R/riding.R); charts:
# R/plot_riding.R.

rides_controls <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("view"), "Show",
                 choices = c("Km so far this year" = "cumulative", "Month by month" = "monthly",
                             "Best rides" = "best")),
    uiOutput(ns("year_filter"))
  )
}

rides_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    conditionalPanel("input.view != 'best'", ns = ns, plotOutput(ns("plot"), height = "380px")),
    conditionalPanel("input.view == 'best'", ns = ns, div(class = "table-responsive", uiOutput(ns("best")))),
    card_footer(class = "small text-muted", textOutput(ns("footer"), inline = TRUE))
  )
}

rides_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rides <- query("SELECT * FROM strava_rides")
    targets <- query("SELECT * FROM riding_targets")
    cumulative <- rides_cumulative(rides)
    years <- sort(unique(rides$year), decreasing = TRUE)

    output$year_filter <- renderUI({
      checkboxGroupInput(ns("years"), "Years", choices = years, selected = head(years, 2), inline = TRUE)
    })
    shown_years <- reactive(as.integer(input$years %||% head(years, 2)))

    output$plot <- renderPlot({
      ui_mode()   # redraw when corporate mode flips (colours come from physdev)
      validate(need(length(shown_years()) > 0, "Pick at least one year."))
      if (identical(input$view, "monthly")) plot_rides_monthly(rides, shown_years())
      else plot_rides_cumulative(cumulative, targets, shown_years())
    }, res = 96)

    output$best <- renderUI({
      r <- rides %>% filter(year %in% shown_years())
      row <- function(x) tags$tr(tags$td(format(as.Date(x$date), "%d %b %Y")), tags$td(x$name),
                                 tags$td(class = "text-end", sprintf("%.0f km", x$km)),
                                 tags$td(class = "text-end", sprintf("%.0f m", x$elev_m)),
                                 tags$td(class = "text-end", sprintf("%.1f km/h", x$avg_kmh)))
      table_of <- function(title, d) tagList(
        h6(class = "px-3 pt-3", title),
        tags$table(class = "table table-sm mb-2",
                   tags$thead(tags$tr(tags$th("Date"), tags$th("Ride"), tags$th(class = "text-end", "Distance"),
                                      tags$th(class = "text-end", "Climbing"), tags$th(class = "text-end", "Average"))),
                   tags$tbody(lapply(seq_len(nrow(d)), \(k) row(d[k, ])))))
      tagList(table_of("Longest", r %>% slice_max(km, n = 5)),
              table_of("Most climbing", r %>% slice_max(elev_m, n = 5)))
    })

    output$footer <- renderText({
      switch(input$view %||% "cumulative",
        cumulative = "Strava rides (all ride types), moving distance. Dashed: an even pace to the year's target from config/goals.yml.",
        monthly = "Km per calendar month, years side by side.",
        best = "The longest rides and the rides with the most climbing in the years picked.")
    })
  })
}
