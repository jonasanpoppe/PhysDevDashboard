# Weight module: status tiles, trend weight against the goal pace, week by
# week against the plan, and intake and steps. Data: R/weight_plan.R;
# charts: R/plot_weight.R.

weight_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tiles")),
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        "Trend weight and the goal pace",
        radioButtons(ns("period"), NULL, inline = TRUE,
                     choices = c("Since 90 days before the plan" = "plan", "Everything" = "all"))
      ),
      plotOutput(ns("trend"), height = "320px"),
      card_footer(class = "small text-muted",
                  "Dots are weigh-ins (measurements); the line is MacroFactor's trend weight, a model output that MacroFactor recalculates; dashed is the goal pace, with its pause.")
    ),
    card(
      full_screen = TRUE,
      card_header("Week by week against the plan"),
      plotOutput(ns("weeks"), height = "260px"),
      card_footer(class = "small text-muted",
                  "Each bar is the change in trend weight over a Monday-to-Sunday week; the black tick is the planned change for that week. Pale bars: the week isn't over.")
    ),
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        "Intake and steps, weekly averages",
        selectInput(ns("metric"), NULL, choices = setNames(names(weight_intake_metrics), weight_intake_metrics),
                    width = "220px")
      ),
      plotOutput(ns("intake"), height = "240px"),
      card_footer(class = "small text-muted",
                  "Averages over logged days. The day of the export is left out: it wasn't finished.")
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
        width = "220px", fill = FALSE, class = "mb-3",
        tile("Trend weight", sprintf("%.1f kg", st$trend_kg), sprintf("On %s", format(st$date, "%d %b %Y"))),
        tile("Rate, last 4 weeks", sprintf("%+.2f kg/wk", st$rate_4w),
             sprintf("Goal: %+.2f kg a week", st$goal_rate), ok = st$rate_4w <= st$goal_rate),
        tile("Left to goal", sprintf("%.1f kg", st$left_kg),
             sprintf("At goal pace from today: %s, pause included", format(st$eta, "%d %b %Y"))),
        tile(sprintf("On %s at goal pace", format(st$checkpoint, "%d %b %Y")), sprintf("%.1f kg", st$on_checkpoint),
             "From today's trend weight"),
        tile("Protein, last 7 days", sprintf("%.0f g a day", st$protein_7d),
             sprintf("Goal: %.0f g", st$protein_goal), ok = st$protein_7d >= st$protein_goal),
        tile("Calories, last 7 days", sprintf("%s kcal a day", format(round(st$kcal_7d), big.mark = ",")),
             sprintf("Expenditure (MacroFactor estimate): %s kcal", format(round(st$expenditure), big.mark = ","))),
        tile("BJJ a week", sprintf("%.1f", st$bjj_per_week),
             sprintf("Goal: %s. Last 28 days", st$bjj_goal), ok = st$bjj_per_week >= st$bjj_goal),
        tile("Lifting a week", sprintf("%.1f", st$lifting_per_week),
             sprintf("Goal: %s. Last 28 days", st$lifting_goal), ok = st$lifting_per_week >= st$lifting_goal)
      )
    })

    output$trend <- renderPlot({
      from <- if (input$period == "plan") as.Date(goal$start_date) - 90 else NULL
      plot_weight_trend(daily, goal, from = from)
    }, res = 96)
    output$weeks <- renderPlot(plot_weight_weeks(weekly), res = 96)
    output$intake <- renderPlot({
      g <- if (input$metric == "protein_g") st$protein_goal else NULL
      plot_weight_intake(weekly, input$metric, g)
    }, res = 96)
  })
}
