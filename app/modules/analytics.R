# Analytics: compare daily variables. Three views, one at a time: over time
# (up to three series stacked), scatter (X against Y, with lag and Spearman's
# rho), and an overview matrix of all pairs. The X and Y selectors double as
# Series 1 and 2 in the time view, so switching views keeps the choice. Data: analytics_daily, analytics_variables
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
      conditionalPanel("input.view == 'time'", ns = ns,
                       selectInput(ns("s3"), "Series 3", choices = NULL)),
      conditionalPanel("input.view == 'scatter'", ns = ns,
                       selectInput(ns("fit"), "Fit", choices = analytics_fits)),
      conditionalPanel("input.view != 'time'", ns = ns,
                       sliderInput(ns("lag"), "Y how many days after X", min = 0, max = 14, value = 0, step = 1)),
      radioButtons(ns("smooth"), "Smoothing",
                   choices = c("Daily" = "daily", "7-day" = "7d", "Weekly" = "weekly"), selected = "7d"),
      # One detrend setting for all three views.
      selectInput(ns("detrend"), "Detrend", choices = analytics_detrends, selected = "none"),
      conditionalPanel("input.detrend == 'rolling'", ns = ns,
                       radioButtons(ns("window"), "Window",
                                    choices = setNames(analytics_detrend_windows,
                                                       paste(analytics_detrend_windows, "days")),
                                    selected = 56L, inline = TRUE)),
      conditionalPanel("input.view == 'time'", ns = ns,
                       checkboxInput(ns("overlay"), "One panel, in SDs (z-scores)", FALSE)),
      conditionalPanel("input.view == 'time' && (input.detrend == 'rolling' || input.detrend == 'loess') && !input.overlay",
                       ns = ns,
                       checkboxInput(ns("show_trend"), "Show the trend instead of removing it", FALSE)),
      radioButtons(ns("period"), "Period",
                   choices = c("Last 90 days" = "90", "Last year" = "365", "Everything" = "all"),
                   selected = "all")
    ),
    card(
      full_screen = TRUE,
      # Scatter and time fill the card's height. The overview keeps a fixed,
      # tall matrix with the table below it and scrolls inside the card body
      # (filling would squeeze the matrix and put the table on top of it).
      card_body(
        uiOutput(ns("stat")),
        as_fill_carrier(conditionalPanel("input.view != 'matrix'", ns = ns,
                                         fill_plot(ns("plot"), min_height = 360))),
        conditionalPanel("input.view == 'matrix'", ns = ns,
                         plotOutput(ns("matrix"), height = "900px"),
                         uiOutput(ns("top")))
      ),
      card_footer(class = "small text-muted", textOutput(ns("footer"), inline = TRUE))
    )
  )
}

analytics_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    daily <- query("SELECT * FROM analytics_daily") %>% mutate(date = as.Date(date))
    variables <- query("SELECT * FROM analytics_variables")
    era_steps <- tryCatch(query("SELECT * FROM vitals_era_steps WHERE applied"), error = \(e) NULL)
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
    updateSelectInput(session, "s3", choices = c(list("None" = ""), choices), selected = "")

    observeEvent(input$view, {
      time <- identical(input$view, "time")
      updateSelectInput(session, "x", label = if (time) "Series 1" else "X")
      updateSelectInput(session, "y", label = if (time) "Series 2" else "Y")
    })

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
    detrend <- reactive(input$detrend %||% "none")
    window <- reactive(as.integer(input$window %||% 56L))
    # A series' label with what was removed from it.
    label_of <- function(s) paste0(analytics_axis_label(s), analytics_detrend_label(s, detrend(), window(), smooth()))
    sx <- reactive({ req(input$x %in% series$series); series[series$series == input$x, ] })
    sy <- reactive({ req(input$y %in% series$series); series[series$series == input$y, ] })

    pair <- reactive({
      validate(need(!analytics_same_family(series, sx()$series, sy()$series),
                    "These two are the same quantity, or one is part of the other: pick another pair."))
      p <- analytics_pair(daily, series, sx()$series, sy()$series, lag = lag(), smooth = smooth(),
                         from = from(), detrend = detrend(), window = window())
      validate(need(nrow(p) >= 3, paste(
        "Too few days where both have a value. Two devices never overlap in time,",
        "so a Fitbit series and a Garmin series have no days in common.")))
      p
    })
    fit <- reactive(input$fit %||% "linear")
    stat <- reactive(analytics_fit_stats(pair()$x, pair()$y, fit = fit(), block = analytics_block_len(smooth())))
    matrix_data <- reactive(analytics_matrix(daily, series, lag = lag(), smooth = smooth(), from = from(),
                                                     detrend = detrend(), window = window()))

    output$stat <- renderUI({
      if (identical(input$view, "time")) return(if (!is.null(notes())) div(class = "px-3 pt-3", notes()))
      req(identical(input$view, "scatter"))
      s <- stat()
      unit <- if (smooth() == "weekly") "weeks" else "days"
      eff_lag <- if (smooth() == "weekly") 7L * round(lag() / 7) else lag()
      num <- function(v) format(signif(v, 2), big.mark = ",", scientific = FALSE, trim = TRUE)
      interval <- function(lo, hi, f = num) if (is.na(lo)) "" else sprintf(" (95%% interval %s to %s)", f(lo), f(hi))
      two <- function(v) sprintf("%.2f", v)
      fit_line <- switch(s$fit,
        linear = list(sprintf("Straight line: %s %s per %s", num(s$effect), sy()$unit, sx()$unit),
                      paste0(interval(s$effect_low, s$effect_high), sprintf(" · R\u00B2 %.2f", s$r2))),
        log = if (!is.na(s$note)) list("Log of X", paste(" ·", s$note)) else
          list(sprintf("Log of X: %s %s per doubling of X", num(s$effect), sy()$unit),
               paste0(interval(s$effect_low, s$effect_high), sprintf(" · R\u00B2 %.2f", s$r2))),
        quadratic = list(sprintf("Quadratic: R\u00B2 %.2f", s$r2),
                         if (is.na(s$turn)) " · no turning point within the data"
                         else sprintf(" · turns at X = %s %s", num(s$turn), sx()$unit)),
        loess = list("LOESS", " · drawn for the eye only, no numbers"))
      div(class = "px-3 pt-3",
          div(tags$strong(sprintf("Spearman's rho %.2f", s$rho)),
              span(class = "text-muted", interval(s$rho_low, s$rho_high, two))),
          div(tags$strong(fit_line[[1]]), span(class = "text-muted", fit_line[[2]])),
          div(class = "small text-muted",
              sprintf("n = %d %s · Y %d days after X%s%s", s$n, unit, eff_lag,
                      if (detrend() == "none") "" else
                        paste(" · both detrended:", analytics_detrend_name(detrend(), window(), smooth())),
                      if (is.na(s$rho_low)) " · too few values for intervals" else "")),
          notes())
    })

    # Time view: Series 1 and 2 (the X and Y inputs) plus an optional third;
    # a series picked twice is drawn once.
    time_series <- reactive({
      picked <- unique(c(input$x, input$y, input$s3))
      lapply(picked[picked %in% series$series], \(v) series[series$series == v, ])
    })

    notes <- reactive({
      shown <- if (identical(input$view, "time")) time_series() else list(sx(), sy())
      n <- unique(unlist(lapply(shown, homogenised_note, steps = era_steps)))
      if (length(n) == 0) NULL else div(class = "small text-muted", lapply(n, div))
    })

    # No height: the plot is drawn at the size its (filling) container has.
    output$plot <- renderPlot({
      switch(input$view %||% "scatter",
        scatter = plot_analytics_scatter(pair(), label_of(sx()), label_of(sy()), fit()),
        time = {
          ss <- time_series()
          validate(need(length(ss) > 0, "Pick a series."))
          overlay <- isTRUE(input$overlay)
          show_trend <- !overlay && isTRUE(input$show_trend) && detrend() %in% c("rolling", "loess")
          # Titles: what was removed, or (showing the trend) what the grey line is.
          title <- function(s) {
            if (!show_trend || s$agg == "sparse") return(label_of(s))
            paste0(analytics_axis_label(s), " \u00B7 grey: ", analytics_detrend_name(detrend(), window()))
          }
          d <- bind_rows(lapply(ss, \(s) {
            v <- analytics_values(daily, s, smooth(), detrend(), window())
            if (!is.null(from())) v <- filter(v, date >= from())
            v <- if (show_trend) mutate(v, value = raw) else mutate(v, trend = NA_real_)
            if (overlay) v <- mutate(v, value = analytics_z(value))
            mutate(v, panel = title(s))
          }))
          validate(need(nrow(d) > 0, "No values in this period."))
          sparse <- vapply(Filter(\(s) s$agg == "sparse", ss), title, "")
          detrended <- if (show_trend || detrend() == "none") character() else
            vapply(Filter(\(s) s$agg != "sparse", ss), title, "")
          plot_analytics_time(d, sparse = sparse, detrended = detrended, overlay = overlay,
                              ylab = if (overlay) "SDs from the mean" else NULL)
        },
        matrix = NULL)
    }, res = 96)

    output$matrix <- renderPlot({
      req(identical(input$view, "matrix"))
      m <- matrix_data()
      validate(need(any(!is.na(m$rho)), "Too few overlapping days in this period."))
      plot_analytics_matrix(m, labels)
    }, res = 96)

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
        "Rho measures any steady rise or fall, whatever the fit. Straight line: Y's change per unit of X; log of X: per doubling of X (diminishing returns); quadratic: a curve that can turn; R\u00B2: how much of Y the fit explains. LOESS follows the points and is for the eye only.",
        "Neighbouring days are alike, so the intervals come from a block bootstrap and are wider than naive ones.",
        "Smoothing: levels take a 7-day mean, hours and sessions a 7-day total, e1RM and W/kg the mean of what falls in the window.",
        "Detrending (all views) removes each series' slow course and keeps its departures from it: a centred rolling mean, a LOESS curve, or the change from the step before. Steps and riding lose their weekday rhythm first (daily values only); e1RM and W/kg are never detrended. Near the ends the window is cut short, drawn lighter. The removed trend is a model, not a measurement. In the scatter and overview, detrending keeps shared seasons and blocks from making two series look related.",
        "Devices are separate series, never pooled; model outputs are marked (model).",
        "A homogenised series is derived: the Fitbit era moved onto Garmin's scale by the step at the switch, then Garmin as measured. The intervals above do not include the uncertainty of that step.")
      if (identical(input$view, "matrix"))
        paste("With this many pairs some look strong by chance: use the overview to find candidates, the scatter to check them.",
              "Blank: fewer than 30 days (10 weeks) in common, or the same quantity (scale and trend weight; sleep and its stages; hours and the loads built from them).", base)
      else base
    })
  })
}
