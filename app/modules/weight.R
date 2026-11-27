# Weight module: status tiles, then one figure at a time, chosen from a
# dropdown; each figure's own options show only while it is selected.
# Data: R/weight_plan.R; charts: R/plot_weight.R.

weight_figures <- c("Trend weight and goal pace" = "trend",
                    "Week by week against the plan" = "weeks",
                    "Intake and steps" = "intake")

weight_footers <- list(
  trend = "Dots are weigh-ins (measurements); the line is MacroFactor's trend weight, a model output that MacroFactor recalculates; dashed is the goal pace, with its pause.",
  weeks = "Each bar is the change in trend weight over a Monday-to-Sunday week; the black tick is the planned change for that week. Grey: before the plan. Pale: the week isn't over.",
  intake = "Weekly averages over logged days. The day of the export is left out: it wasn't finished. Steps are the wrist device's count (Fitbit, then Garmin), not MacroFactor's; a week spanning the switch has none."
)

weight_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Weight",
      selectInput(ns("figure"), "Figure", choices = weight_figures),
      conditionalPanel(
        "input.figure == 'trend'", ns = ns,
        radioButtons(ns("period"), "Period",
                     choices = c("Since 90 days before the plan" = "plan", "Everything" = "all"))
      ),
      conditionalPanel(
        "input.figure == 'intake'", ns = ns,
        selectInput(ns("metric"), "Metric",
                    choices = setNames(names(weight_intake_metrics), weight_intake_metrics))
      )
    ),
    uiOutput(ns("tiles")),
    card(
      full_screen = TRUE,
      fill_plot(ns("plot"), min_height = 280),
      card_footer(class = "small text-muted", textOutput(ns("footer"), inline = TRUE))
    )
  )
}

weight_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    daily  <- query("SELECT * FROM weight_daily")
    weekly <- query("SELECT * FROM weight_weekly")
    st     <- query("SELECT * FROM weight_status")
    goal   <- as.list(query("SELECT * FROM weight_goal"))

    output$tiles <- renderUI({
      tick <- function(ok) if (is.na(ok)) "" else if (ok) "✓ " else "✕ "
      tile <- function(title, value, note, ok = NA) {
        value_box(
          title = title,
          value = tagList(span(class = if (isTRUE(ok)) "text-success" else if (isFALSE(ok)) "text-danger", tick(ok)), value),
          p(class = "small mb-0", note)
        )
      }
      layout_column_wrap(
        width = "150px", fill = FALSE, gap = "0.6rem", class = "mb-3 tiles-compact",
        tile("Trend weight", sprintf("%.1f kg", st$trend_kg), sprintf("On %s (MacroFactor's trend, a model output)", fmt_day(st$date))),
        # The same rate, window and 80% rule as the Scenarios' recent pace
        tile(sprintf("Rate, last %d weeks", st$recent_weeks), sprintf("%+.2f kg/wk", st$rate_recent),
             sprintf("Goal %+.2f a week; met at %+.2f (%d%%)", st$goal_rate, st$goal_rate_met, round(100 * st$adherence)),
             ok = st$rate_recent <= st$goal_rate_met + 1e-9),
        tile("Left to goal", sprintf("%.1f kg", st$left_kg),
             sprintf("From today, pause included: %s at %d%% of the goal pace (Scenarios: on the plan), %s at the full pace",
                     fmt_day(st$eta_met), round(100 * st$adherence), fmt_day(st$eta))),
        tile(sprintf("On %s at goal pace", fmt_day(st$checkpoint)), sprintf("%.1f kg", st$on_checkpoint),
             "From today's trend weight"),
        tile("Protein, last 7 days", sprintf("%.0f g a day", st$protein_7d),
             sprintf("Goal: %.0f g", st$protein_goal), ok = st$protein_7d >= st$protein_goal),
        tile("Calories, last 7 days", sprintf("%s kcal a day", format(round(st$kcal_7d), big.mark = ",")),
             sprintf("Expenditure (MacroFactor estimate): %s kcal", format(round(st$expenditure), big.mark = ","))),
        # Met at 80%, as on Home; BJJ counted to today (Strava), lifting to the export's last day
        tile("BJJ a week", sprintf("%.1f", st$bjj_per_week),
             sprintf("Goal %s, met at %s. Last 28 days to %s", st$bjj_goal, format(st$bjj_met_at), fmt_day(st$as_of)),
             ok = st$bjj_per_week >= st$bjj_met_at - 1e-9),
        tile("Lifting a week", sprintf("%.1f", st$lifting_per_week),
             sprintf("Goal %s, met at %s. Last 28 days to %s (the MacroFactor export)", st$lifting_goal,
                     format(st$lifting_met_at), fmt_day(st$lifting_as_of)),
             ok = st$lifting_per_week >= st$lifting_met_at - 1e-9)
      )
    })

    output$plot <- renderPlot({
      ui_mode()   # redraw when corporate mode flips (colours come from physdev)
      switch(input$figure,
        trend = plot_weight_trend(daily, goal,
                                  from = if (identical(input$period, "all")) NULL else as.Date(goal$start_date) - 90),
        weeks = plot_weight_weeks(weekly),
        intake = plot_weight_intake(weekly, input$metric %||% "kcal",
                                    if (identical(input$metric, "protein_g")) st$protein_goal else NULL)
      )
    }, res = 96)

    output$footer <- renderText(weight_footers[[input$figure %||% "trend"]])
  })
}
