# Strength module: one figure at a time, chosen from a dropdown. Data:
# R/strength.R (strength_sessions, strength_targets, strength_ceilings) and
# R/forecast.R (strength_forecast_*); charts: R/plot_strength.R,
# R/plot_forecast.R. The forecast is recomputed live from its settings when
# the aptitude or the gap rule changes.

strength_figures <- c("e1RM by lift" = "lifts",
                      "Forecast: main lifts to targets" = "forecast",
                      "Main lifts against targets and ceilings" = "targets",
                      "Strength standards (× bodyweight)" = "standards")

forecast_lift_names <- c(Bench = "bench", Squat = "squat", Deadlift = "deadlift")


strength_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Strength",
      selectInput(ns("figure"), "Figure", choices = strength_figures),
      conditionalPanel("input.figure == 'lifts'", ns = ns, uiOutput(ns("group_select"))),
      conditionalPanel(
        "input.figure == 'forecast'", ns = ns,
        selectInput(ns("fc_lift"), "Lift", choices = forecast_lift_names),
        sliderInput(ns("fc_aptitude"), "Aptitude (share of the elite line)", min = 0.6, max = 1.1,
                    value = 0.8, step = 0.01),
        radioButtons(ns("fc_period"), "Period", choices = c("This round" = "round", "Since 2020" = "all")),
        radioButtons(ns("fc_history"), "Earlier rounds", choices = c("Monthly bests" = "monthly", "Every session" = "sessions")),
        selectInput(ns("fc_gap"), "Gap rule (assumption)",
                    choices = c("A gap counts at most 14 days" = "14", "At most 28 days" = "28",
                                "Calendar time" = "Inf"))
      )
    ),
    card(
      full_screen = TRUE,
      card_header(textOutput(ns("summary"), inline = TRUE) %>% tagAppendAttributes(class = "small text-muted")),
      fill_plot(ns("plot"), min_height = 320),
      uiOutput(ns("below"), fill = FALSE),
      card_footer(class = "small text-muted", uiOutput(ns("footer")))
    )
  )
}

# The forecast's target is an e1RM derived from the target on the bar
# (goals.yml strength; R/plan_config.R): say both
forecast_target_label <- function(st) {
  if (is.na(st$target_kg)) return(NULL)
  sprintf("%s kg on the bar (e1RM %.1f)", format(round(st$target_kg * st$single_ratio)), st$target_kg)
}

strength_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    figure <- reactive(input$figure %||% "lifts")
    sessions <- query("SELECT * FROM strength_sessions")
    targets  <- query("SELECT * FROM strength_targets")
    ceilings <- query("SELECT * FROM strength_ceilings")
    now <- strength_now(sessions)
    formula <- unique(sessions$formula)[1]
    groups <- unique(c(targets$group, sort(unique(sessions$group))))

    output$group_select <- renderUI(selectInput(ns("group"), "Lift group", choices = groups))

    # ── Forecast ──
    fc_settings <- query("SELECT * FROM strength_forecast_settings") %>%
      mutate(across(c(round_start, horizon, start_date, first_round_origin, first_round_fitted_until), as.Date))
    fc_first <- query("SELECT * FROM strength_forecast_first_round") %>% mutate(date = as.Date(date))
    fc_thresholds <- query("SELECT * FROM strength_forecast_thresholds")
    updateSelectInput(session, "fc_lift",
                      choices = forecast_lift_names[forecast_lift_names %in% fc_settings$lift])
    fc_st <- reactive({
      lift <- if (isTRUE(input$fc_lift %in% fc_settings$lift)) input$fc_lift else fc_settings$lift[1]
      fc_settings[fc_settings$lift == lift, ]
    })
    # The slider starts at the lift's p50
    observeEvent(input$fc_lift, {
      updateSliderInput(session, "fc_aptitude", value = fc_st()$p50)
    })
    fc <- reactive({
      st <- fc_st()
      apt <- input$fc_aptitude %||% st$p50
      forecast_lift(sessions, st, aptitude = c(st$p10, apt, st$p90), bands = c("p10", "chosen", "p90"),
                    gap_cap_days = as.numeric(input$fc_gap %||% st$gap_cap_days))
    })
    fc_threshold <- reactive(fc_thresholds[fc_thresholds$lift == fc_st()$lift, ])

    output$summary <- renderText({
      sprintf("%d session bests · %s formula · last session %s",
              nrow(sessions), tools::toTitleCase(formula), format(max(as.Date(sessions$date)), "%d %b %Y"))
    })

    output$plot <- renderPlot({
      ui_mode()   # redraw when corporate mode flips (colours come from physdev)
      switch(figure(),
        lifts = {
          g <- input$group %||% groups[1]
          i <- match(g, targets$group)
          plot_strength_lifts(sessions, g, targets$target_e1rm_kg[i], targets$target_label[i])
        },
        forecast = {
          st <- fc_st()
          th <- fc_threshold()
          plot_lift_forecast(sessions, fc(), fc_first[fc_first$lift == st$lift, ], st$target_kg,
                             th$threshold_kg, th$attempt_e1rm_kg,
                             from = if (identical(input$fc_period, "all")) NULL else st$round_start - 90,
                             monthly = !identical(input$fc_history, "sessions"),
                             target_label = forecast_target_label(st))
        },
        targets = plot_strength_targets(now, targets, ceilings),
        standards = plot_strength_standards(now, strength_standards))
    }, res = 96)

    # Under the forecast: the time to the target and the threshold on the bar
    output$below <- renderUI({
      if (figure() != "forecast") return(NULL)
      f <- fc()
      st <- fc_st()
      th <- fc_threshold()
      when <- function(d) ifelse(is.na(d), "never", format(d, "%b %Y"))
      apt <- f$params$aptitude[2]
      cells <- if (!is.na(st$target_kg)) {
        tt <- forecast_targets(f, st$target_kg)
        tags$tr(tags$td(paste("Target", forecast_target_label(st))),
                tags$td(when(tt$date[tt$band == "p10"])), tags$td(when(tt$date[tt$band == "chosen"])),
                tags$td(when(tt$date[tt$band == "p90"])), tags$td(when(tt$date[tt$band == "straight line"])))
      }
      attempt <- time_to_target(f$params, th$attempt_e1rm_kg, f$t_now, f$from)
      best <- function(kg, reps, date) if (is.na(kg)) "–" else sprintf("%g × %d (%s)", kg, as.integer(reps), format(as.Date(date), "%b %Y"))
      tagList(
        tags$table(
          class = "table table-sm small mb-2",
          tags$thead(tags$tr(tags$th(""), tags$th(sprintf("p10 (%.2f)", st$p10)), tags$th(sprintf("Chosen (%.2f)", apt)),
                             tags$th(sprintf("p90 (%.2f)", st$p90)), tags$th("Straight line"))),
          tags$tbody(
            cells,
            tags$tr(tags$td(sprintf("Attempt level, e1RM %.0f", th$attempt_e1rm_kg)),
                    tags$td(when(attempt$date[1])), tags$td(when(attempt$date[2])), tags$td(when(attempt$date[3])),
                    tags$td(""))
          )
        ),
        tags$table(
          class = "table table-sm small mb-0",
          tags$thead(tags$tr(tags$th("On the bar"), tags$th("Threshold"), tags$th("This round"), tags$th("Ever"),
                             tags$th("Status"))),
          tags$tbody(tags$tr(
            tags$td(tools::toTitleCase(st$lift)),
            tags$td(sprintf("%g kg (%g × %g kg)", th$threshold_kg, th$threshold_bw, th$bodyweight_kg)),
            tags$td(best(th$round_best_kg, th$round_best_reps, th$round_best_date)),
            tags$td(best(th$ever_best_kg, th$ever_best_reps, th$ever_best_date)),
            tags$td(if (isTRUE(th$on_the_bar)) "✓ lifted" else "not yet")
          ))
        )
      )
    })

    output$footer <- renderUI({
      common <- sprintf("Estimated one-rep max from the best working set of each session (%s formula, RIR 3 or less, bar weight incl. machine base weight): an estimate, not a tested max.",
                        tools::toTitleCase(formula))
      switch(figure(),
        forecast = {
          st <- fc_st()
          tagList(
            "A model, not a measurement. Curve: e1RM = c − (c − y0)·exp(−t/τ), with the ceiling c = aptitude × the elite line (fat-free mass per cm, Nuckols) and t the months spent training this exact lift (gaps capped by the gap rule).",
            if (isTRUE(fc()$fitted)) sprintf("Fitted to %d sessions since %s.", nrow(fc()$round), format(st$round_start, "%d %b %Y"))
            else sprintf("Too few sessions to fit: starts at %g kg on %s (%s) with τ = %.1f months from the first round.",
                         st$start_e1rm_kg, format(st$start_date, "%d %b %Y"), st$start_note, st$tau_start),
            "Green: the chosen aptitude, band p10–p90. Blue: the first round's fit (solid where fitted, dashed after; shaded to the logarithmic shape). Dates assume training goes on without further gaps; read them to within a season.",
            "On the bar counts loads actually lifted; the attempt level is the e1RM at which a single at the threshold is likely."
          )
        },
        lifts = tagList(common, "The thick line is the group's main lift; the dashed line its target. Lines break where sessions are more than 6 weeks apart."),
        targets = tagList(common, sprintf("Dot: the main lift's best of the last 12 weeks (or its last session, dated). Red tick: the target for %s. Band: the ceiling at aptitude p10–p90 (see the forecast).",
                                          format(as.Date(targets$target_date[1]), "%Y"))),
        standards = tagList(common, "Divided by the MacroFactor trend weight on the day. The standards are for the back squat, bench press and deadlift: a front squat reads lower than a back squat.",
                            "Sources: Barbell Medicine strength standards; Stronger by Science, “What is strong?”; normative powerlifting data (PubMed 39060209)."))
    })
  })
}
