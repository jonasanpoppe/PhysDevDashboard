# Analytics: compare any two daily variables. Three views, one at a time:
# over time (stacked), scatter (with lag and Spearman's rho), and an overview
# matrix of all pairs. Data: analytics_daily, analytics_variables
# (R/analytics.R); charts: R/plot_analytics.R.

analytics_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Analytics",
      selectInput(ns("view"), "View", choices = c("Scatter" = "scatter", "Over time" = "time",
                                                  "Overview: all pairs" = "matrix")),
      selectInput(ns("preset"), "Preset", choices = NULL),
      conditionalPanel("input.view != 'matrix'", ns = ns,
                       selectInput(ns("x"), "X", choices = NULL),
                       selectInput(ns("y"), "Y", choices = NULL)),
      conditionalPanel("input.view != 'time'", ns = ns,
                       sliderInput(ns("lag"), "Y how many days after X", min = 0, max = 14, value = 0, step = 1)),
      radioButtons(ns("smooth"), "Smoothing",
                   choices = c("Daily" = "daily", "7-day" = "7d", "Weekly" = "weekly"), selected = "7d"),
      radioButtons(ns("period"), "Period",
                   choices = c("Last 90 days" = "90", "Last year" = "365", "Everything" = "all"),
                   selected = "all")
    ),
    card(
      full_screen = TRUE,
      # Not fillable: the chart keeps its own height and the table goes below
      # it (a fillable body squeezes the chart and the table lands on top).
      card_body(
        fillable = FALSE,
        uiOutput(ns("stat")),
        plotOutput(ns("plot"), height = "auto"),
        conditionalPanel("input.view == 'matrix'", ns = ns, uiOutput(ns("top")))
      ),
      card_footer(class = "small text-muted", textOutput(ns("footer"), inline = TRUE))
    )
  )
}

analytics_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    daily <- query("SELECT * FROM analytics_daily") %>% mutate(date = as.Date(date))
    variables <- query("SELECT * FROM analytics_variables")
    series <- analytics_series(daily, variables)
    labels <- setNames(series$series_label, series$series)
    choices <- lapply(split(series, factor(series$group, levels = unique(series$group))),
                      \(s) setNames(s$series, s$series_label))
    presets <- analytics_presets %>%
      mutate(x = vapply(x, \(v) resolve_series(series, v), ""),
             y = vapply(y, \(v) resolve_series(series, v), "")) %>%
      filter(!is.na(x), !is.na(y))
    last_day <- max(daily$date)
    first <- presets[1, ]

    updateSelectInput(session, "preset", choices = c("Pick a preset" = "", presets$preset))
    updateSelectInput(session, "x", choices = choices, selected = first$x %||% series$series[1])
    updateSelectInput(session, "y", choices = choices, selected = first$y %||% series$series[2])

    observeEvent(input$preset, {
      p <- presets[presets$preset == input$preset, ]
      if (nrow(p) != 1) return()
      updateSelectInput(session, "x", selected = p$x)
      updateSelectInput(session, "y", selected = p$y)
      updateSliderInput(session, "lag", value = p$lag)
      updateRadioButtons(session, "smooth", selected = p$smooth)
    }, ignoreInit = TRUE)

    from <- reactive(if (identical(input$period, "all")) NULL else last_day - as.numeric(input$period) + 1)
    smooth <- reactive(input$smooth %||% "7d")
    lag <- reactive(if (identical(input$view, "time")) 0L else as.integer(input$lag %||% 0))
    sx <- reactive({ req(input$x %in% series$series); series[series$series == input$x, ] })
    sy <- reactive({ req(input$y %in% series$series); series[series$series == input$y, ] })

    pair <- reactive({
      validate(need(!analytics_same_family(series, sx()$series, sy()$series),
                    "These two are the same quantity, or one is part of the other: pick another pair."))
      p <- analytics_pair(daily, series, sx()$series, sy()$series, lag = lag(), smooth = smooth(), from = from())
      validate(need(nrow(p) >= 3, paste(
        "Too few days where both have a value. Two devices never overlap in time,",
        "so a Fitbit series and a Garmin series have no days in common.")))
      p
    })
    stat <- reactive(spearman_block(pair()$x, pair()$y, block = analytics_block_len(smooth())))
    matrix_data <- reactive(analytics_matrix(daily, series, lag = lag(), smooth = smooth(), from = from()))

    output$stat <- renderUI({
      req(identical(input$view, "scatter"))
      s <- stat()
      unit <- if (smooth() == "weekly") "weeks" else "days"
      eff_lag <- if (smooth() == "weekly") 7L * round(lag() / 7) else lag()
      ci <- if (is.na(s$low)) "too few values for an interval"
            else sprintf("95%% interval %.2f to %.2f", s$low, s$high)
      div(class = "px-3 pt-3", tags$strong(sprintf("Spearman's rho %.2f", s$rho)),
          span(class = "text-muted", sprintf(" · %s · n = %d %s · Y %d days after X", ci, s$n, unit, eff_lag)))
    })

    output$plot <- renderPlot({
      switch(input$view %||% "scatter",
        scatter = plot_analytics_scatter(pair(), analytics_axis_label(sx()), analytics_axis_label(sy())),
        time = {
          get <- function(s) daily %>% filter(variable == s$variable, era == s$era) %>%
            smooth_series(s$agg, smooth()) %>% mutate(panel = analytics_axis_label(s))
          d <- bind_rows(get(sx()), get(sy()))
          if (!is.null(from())) d <- filter(d, date >= from())
          validate(need(nrow(d) > 0, "No values in this period."))
          sparse <- unique(d$panel[d$panel %in% c(if (sx()$agg == "sparse") analytics_axis_label(sx()),
                                                   if (sy()$agg == "sparse") analytics_axis_label(sy()))])
          plot_analytics_time(d, sparse = sparse)
        },
        matrix = {
          m <- matrix_data()
          validate(need(any(!is.na(m$rho)), "Too few overlapping days in this period."))
          plot_analytics_matrix(m, labels)
        })
    }, height = function() switch(input$view %||% "scatter", matrix = 900, time = 560, 460), res = 96)

    output$top <- renderUI({
      top <- analytics_top_pairs(matrix_data(), series, lag = lag())
      if (nrow(top) == 0) return(NULL)
      rows <- lapply(seq_len(nrow(top)), \(k) tags$tr(
        tags$td(labels[[top$x[k]]]), tags$td(labels[[top$y[k]]]),
        tags$td(class = "text-end", sprintf("%.2f", top$rho[k])),
        tags$td(class = "text-end", top$n[k])))
      tagList(h6(class = "px-3 pt-2", "Strongest pairs (candidates to check in the scatter)"),
              div(class = "table-responsive", tags$table(class = "table table-sm mb-2",
                tags$thead(tags$tr(tags$th("X"), tags$th("Y"), tags$th(class = "text-end", "rho"),
                                   tags$th(class = "text-end", "n"))),
                tags$tbody(rows))))
    })

    output$footer <- renderText({
      base <- paste(
        "Associations, not causes: a third thing (season, a training block) often drives both.",
        "Neighbouring days are alike, so the interval comes from a block bootstrap and is wider than a naive one.",
        "Smoothing: levels take a 7-day mean, hours and sessions a 7-day total, e1RM and W/kg the mean of what falls in the window.",
        "Devices are separate series, never pooled; model outputs are marked (model).")
      if (identical(input$view, "matrix"))
        paste("With this many pairs some look strong by chance: use the overview to find candidates, the scatter to check them.",
              "Blank: fewer than 30 days (10 weeks) in common, or the same quantity (scale and trend weight; sleep and its stages; hours and the loads built from them).", base)
      else base
    })
  })
}
