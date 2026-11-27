# Scenarios tab: "am I on track?" for every quest, under assumptions set in
# the sidebar, and one figure at a time (the quest's projection). Verdicts
# rerun live from R/scenarios.R on the tables in the database; the Home
# chips show the same verdicts with the default assumptions (quest_scenarios).
# Projections are model outputs: the footers say what each assumes.

scenario_figure_metrics <- c("weight_trend_kg", "e1rm_bar_kg", "ride_km_ytd")

scenario_icons <- c(on_track = "✓", at_risk = "!", behind = "✕", done = "✓", due = "○",
                    overdue = "!", none = "–")

# A verdict as a chip: icon plus words, never colour alone
verdict_chip <- function(verdict, label = scenario_verdict_labels[[verdict]]) {
  span(class = paste("verdict", paste0("verdict-", verdict)), span(class = "verdict-icon", scenario_icons[[verdict]]), label)
}

scenarios_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Assumptions",
      selectInput(ns("quest"), "Figure", choices = NULL),
      conditionalPanel(
        "output.kind == 'weight_trend_kg'", ns = ns,
        sliderInput(ns("rate"), "Planned loss (kg a week)", min = 0.2, max = 1, value = 0.5, step = 0.05),
        sliderInput(ns("adherence"), "Adherence", min = 0.5, max = 1, value = 0.8, step = 0.05),
        sliderInput(ns("recent_weeks"), "Recent pace over (weeks)", min = 4, max = 12, value = 8, step = 1)
      ),
      conditionalPanel(
        "output.kind == 'ride_km_ytd'", ns = ns,
        numericInput(ns("km_week"), "Km a week (empty: an even pace from today)", value = NA, min = 0, step = 10)
      ),
      conditionalPanel(
        "output.kind == 'e1rm_bar_kg'", ns = ns,
        p(class = "small text-muted", "Lifts follow the strength forecast as it is (Strength tab): no assumption to set.")
      )
    ),
    card(card_header("Am I on track?"), uiOutput(ns("verdicts"))),
    card(
      full_screen = TRUE,
      fill_plot(ns("plot"), min_height = 300),
      card_footer(class = "small text-muted", textOutput(ns("footer"), inline = TRUE))
    )
  )
}

scenarios_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    cfg <- jsonlite::fromJSON(query("SELECT json FROM oath_config")$json, simplifyVector = FALSE)
    events <- if (db_has_table("relic_events")) query("SELECT * FROM relic_events") else NULL
    data <- list(
      weight = query("SELECT date, trend_kg FROM weight_daily"),
      strength = query("SELECT date, lift, e1rm FROM strength_sessions"),
      rides = query("SELECT date, km, elev_m FROM strava_rides"),
      sessions = query("SELECT date, kind FROM sessions_daily"),
      climbs = query("SELECT date, climb, status, bike_id FROM strava_climb_efforts"),
      events = events,
      weekly = query("SELECT week, trend_change, in_plan, complete FROM weight_weekly"),
      goal = as.list(query("SELECT * FROM weight_goal")),
      forecast = if (db_has_table("strength_forecast_params")) list(
        params = query("SELECT * FROM strength_forecast_params"),
        curve = query("SELECT lift, band, date, e1rm FROM strength_forecast_curve")))
    as_of <- if (db_has_table("quest_scenarios")) query("SELECT max(as_of) AS d FROM quest_scenarios")$d else Sys.Date()
    defaults <- scenario_defaults(cfg)
    quests <- cfg$quests
    with_fig <- Filter(function(q) q$metric %in% scenario_figure_metrics, quests)
    updateSelectInput(session, "quest", choices = stats::setNames(
      vapply(with_fig, `[[`, "", "id"), vapply(with_fig, \(q) q$label, "")))
    updateSliderInput(session, "rate", value = defaults$rate_kg_week)
    updateSliderInput(session, "adherence", value = defaults$adherence)
    updateSliderInput(session, "recent_weeks", value = defaults$recent_weeks)
    if (!is.null(defaults$km_week) && !is.na(defaults$km_week)) updateNumericInput(session, "km_week", value = defaults$km_week)

    quest <- reactive({
      req(input$quest)
      Filter(function(q) identical(q$id, input$quest), quests)[[1]]
    })
    # The figure's kind, for the sidebar's panels
    output$kind <- renderText(quest()$metric)
    outputOptions(output, "kind", suspendWhenHidden = FALSE)

    a <- reactive({
      utils::modifyList(defaults, list(
        rate_kg_week = input$rate %||% defaults$rate_kg_week,
        adherence = input$adherence %||% defaults$adherence,
        recent_weeks = input$recent_weeks %||% defaults$recent_weeks,
        km_week = if (is.null(input$km_week) || is.na(input$km_week)) NA_real_ else input$km_week))
    })
    board <- reactive(quest_scenarios(cfg, data, as_of, a()))

    output$verdicts <- renderUI({
      plain <- identical(ui_mode(), "corporate")
      b <- board()
      tags$table(
        class = "table table-sm scenario-table mb-0",
        tags$thead(tags$tr(tags$th("Quest"), tags$th("Verdict"), tags$th("Projection"))),
        tags$tbody(lapply(seq_len(nrow(b)), function(i) tags$tr(
          tags$td(if (plain) b$plain[i] else b$label[i]),
          tags$td(verdict_chip(b$verdict[i])),
          tags$td(class = "small", b$summary[i])))),
        tags$caption(class = "small text-muted", sprintf(
          "As of %s. Projections are model outputs. On track: the central projection makes it by the deadline; at risk: only the optimistic end of the range does; behind: not even that.",
          fmt_day(as_of)))
      )
    })

    output$plot <- renderPlot({
      ui_mode()
      q <- quest()
      switch(q$metric,
        weight_trend_kg = plot_scenario_weight(scenario_weight(q, data, as_of, a()), data$weight, q),
        e1rm_bar_kg = {
          sl <- scenario_lift(q, data, as_of)
          validate(need(!is.null(sl$projection), "No strength forecast for this lift"))
          plot_scenario_lift(sl, data$strength, q)
        },
        ride_km_ytd = plot_scenario_km(scenario_km(q, data, as_of, a(), cfg), data$rides, q, cfg))
    })

    output$footer <- renderText({
      q <- quest()
      switch(q$metric,
        weight_trend_kg = {
          sw <- scenario_weight(q, data, as_of, a())
          sprintf("Ink: MacroFactor's trend weight (a model output). Lines: the projection from today, %s; bands: p10 to p90 from the SD of weekly change over %d weeks (%s). The pause at %s kg is included. Shaded: the window.",
                  "on the plan (planned loss × adherence) and at the recent pace (the trend's slope)",
                  sw$sd$weeks, sw$sd$basis, sw$goal$pause_at_kg %||% "–")
        },
        e1rm_bar_kg = sprintf("Dots: session e1RM × %s (the heavy single on the bar, an estimate). Line and band: the strength forecast's plateau curve at aptitude p50, band p10 to p90%s.",
                              formatC(as.numeric(q$single_ratio %||% 1), format = "f", digits = 3),
                              if (!is.null(q$forecast$variant_ratio)) sprintf("; no forecast of its own: the %s curve's ceiling × %s, from this lift's e1RM now", q$forecast$lift, q$forecast$variant_ratio) else ""),
        ride_km_ytd = "Step: km ridden this year (all ride types). Lines: the km a week set in the sidebar and the recent pace (last 8 weeks); bands: p10 to p90 from the SD of weekly km over the last year. The chip on Home follows the recent pace.")
    })
  })
}
