# Engine module: how strong (power by duration) and whether it is changing
# (watts at the same heart rate, training load). One figure at a time, chosen
# from a dropdown; each figure's options show only while it is selected.
# Charts: R/plot_engine.R and R/plot_fitness.R; model: R/fitness_model.R.

engine_figures <- c("Power by duration" = "power",
                    "Watts at the same heart rate" = "fitness",
                    "Training load" = "load")

engine_heights <- c(power = 520, fitness = 320, load = 300)

engine_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(
      class = "d-flex flex-wrap align-items-center gap-3",
      selectInput(ns("figure"), NULL, choices = engine_figures, width = "260px"),
      textOutput(ns("summary"), inline = TRUE) %>% tagAppendAttributes(class = "small text-muted")
    ),
    conditionalPanel(
      "input.figure == 'power'", ns = ns,
      div(class = "d-flex flex-wrap align-items-center gap-4 px-3 pt-2",
          uiOutput(ns("year_filter")),
          checkboxInput(ns("range"), "Show the ±15% range", value = TRUE),
          checkboxInput(ns("short"), "Include efforts under 90 s", value = TRUE))
    ),
    conditionalPanel("input.figure == 'fitness'", ns = ns, uiOutput(ns("headline"))),
    plotOutput(ns("plot"), height = "auto"),
    card_footer(class = "small text-muted", uiOutput(ns("footer")))
  )
}

engine_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    figure <- reactive(input$figure %||% "power")

    # Power by duration
    efforts <- query("SELECT * FROM strava_climb_efforts WHERE status IN ('clean', 'top') AND wkg IS NOT NULL")
    labelled <- query("SELECT name FROM climbs WHERE engine_label")$name
    years <- sort(unique(lubridate_year(efforts$date)))
    colours <- engine_year_colours(years)

    # Fitness and load
    seasons <- query("SELECT * FROM fitness_seasons ORDER BY season")
    coefs   <- query("SELECT * FROM fitness_coefficients")
    check   <- query("SELECT * FROM fitness_load_check")
    load    <- query("SELECT * FROM strava_daily_load")
    xlim <- fitness_xlim(seasons, load)
    ref <- seasons[1, c("ref_hr_bpm", "ref_duration_s", "ref_hours_into_ride")]

    output$year_filter <- renderUI({
      checkboxGroupInput(ns("years"), NULL, choices = years, selected = years, inline = TRUE)
    })

    shown <- reactive({
      req(input$years)
      e <- efforts[lubridate_year(efforts$date) %in% as.integer(input$years), ]
      if (!isTRUE(input$short)) e <- e[!e$short, ]
      validate(need(nrow(e) > 0, "No clean ascents with these filters."))
      e
    })

    output$summary <- renderText({
      switch(figure(),
        power = { e <- shown(); sprintf("%d ascents · %d climbs", nrow(e), length(unique(e$climb))) },
        fitness = sprintf("%d clean chest-strap ascents", sum(seasons$n)),
        load = "Riding and running, 6-week mean")
    })

    output$headline <- renderUI({
      if (nrow(seasons) < 2) return(NULL)
      a <- seasons[nrow(seasons) - 1, ]; b <- seasons[nrow(seasons), ]
      term <- coefs[coefs$term == paste0("season", b$season), ]
      p(class = "px-3 pt-2 mb-0",
        sprintf("At %s bpm, %s-minute climb, %s h into a ride: %.0f W in %d against %.0f W in %d (%+.0f W, 95%% interval %+.0f to %+.0f).",
                ref$ref_hr_bpm, ref$ref_duration_s / 60, ref$ref_hours_into_ride,
                b$watts_ref, b$season, a$watts_ref, a$season,
                term$estimate, term$ci_low, term$ci_high),
        if (nrow(term) && term$ci_low < 0 && term$ci_high > 0) span(class = "text-muted", "The interval includes zero: a plausible change, not a proven one."))
    })

    output$plot <- renderPlot({
      switch(figure(),
        power = plot_engine(shown(), year_colours = colours, show_range = isTRUE(input$range),
                            label_climbs = labelled),
        fitness = plot_fitness_seasons(seasons, xlim, ref$ref_hr_bpm),
        load = plot_fitness_load(load, xlim))
    }, height = function() engine_heights[[figure()]], res = 96)

    output$footer <- renderUI({
      switch(figure(),
        power = tagList(
          "One dot per clean ascent; hollow dots are under 90 s and read high (run-up momentum).",
          "All watts are estimates from the climb's rise, length and surface, your trend weight and the bike's mass (±15%).",
          "Wind is ignored. Strava's own estimate runs 10–15% lower, but ratios between efforts hold.",
          "Bands: Garmin's FTP categories for men (after Allen & Coggan); they describe about an hour's effort, so only the long climbs compare directly."),
        fitness = {
          hr <- coefs[coefs$term == "hr_end", ]
          tagList(
            sprintf("Model: estimated power ~ heart rate at the end of the climb + log(duration) + hours into the ride + season, on %d clean chest-strap ascents of 90 s or more, one bike (%s).",
                    sum(seasons$n), paste(sprintf("%d: %d", seasons$season, seasons$n), collapse = ", ")),
            sprintf("Each extra bpm at the top ≈ %+.1f W. Bands are bootstrap intervals (how well the model is pinned down); the watts themselves are estimates (±15%%).", hr$estimate))
        },
        load = sprintf("Load check: %+.0f W per extra weekly hour of riding and running in the 6 weeks before (95%% interval %+.0f to %+.0f, %d ascents). %s",
                       check$w_per_weekly_hour, check$ci_low, check$ci_high, check$n,
                       if (check$ci_low > 0) "More training went with more watts."
                       else if (check$ci_high < 0) "More training went with fewer watts."
                       else "No clear effect in these ascents."))
    })
  })
}
