# Engine: everything on the bike, one view at a time from a grouped dropdown.
# Each view is its own module (app/modules/<view>.R): <view>_ui is the chart,
# <view>_controls its options, shown in the sidebar under the dropdown. The
# dropdown switches both (bslib navset_hidden).

engine_views <- list(
  Climbs  = c("Climb record" = "climbs", "Power by duration" = "power", "Benchmark" = "benchmark"),
  Fitness = c("Watts at the same heart rate" = "fitness", "Training load" = "load"),
  Riding  = c("Distance and best rides" = "rides", "Plan: time in heart-rate bands" = "plan")
)

engine_ui <- function(id) {
  ns <- NS(id)
  # Views without options of their own show only the dropdown. Built here,
  # not at the top: modules are sourced alphabetically, after this file.
  engine_controls <- list(climbs = climbs_controls, power = power_controls, rides = rides_controls,
                          plan = plan_controls)
  views <- unlist(engine_views, use.names = FALSE)
  layout_sidebar(
    sidebar = sidebar(
      title = "Engine",
      selectInput(ns("view"), "View", choices = engine_views),
      # Each view's own options, under the dropdown (same ids as its module)
      do.call(navset_hidden, c(list(id = ns("controls")), lapply(views, \(v) {
        nav_panel_hidden(v, if (!is.null(engine_controls[[v]])) engine_controls[[v]](ns(v)))
      })))
    ),
    navset_hidden(
      id = ns("views"),
      nav_panel_hidden("climbs", climbs_ui(ns("climbs"))),
      nav_panel_hidden("power", power_ui(ns("power"))),
      nav_panel_hidden("benchmark", benchmark_ui(ns("benchmark"))),
      nav_panel_hidden("fitness", fitness_ui(ns("fitness"), "fitness")),
      nav_panel_hidden("load", fitness_ui(ns("load"), "load")),
      nav_panel_hidden("rides", rides_ui(ns("rides"))),
      nav_panel_hidden("plan", plan_ui(ns("plan")))
    )
  )
}

engine_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$view, {
      nav_select("views", input$view)
      nav_select("controls", input$view)
    })
    climbs_server("climbs")
    power_server("power")
    benchmark_server("benchmark")
    fitness_server("fitness", "fitness")
    fitness_server("load", "load")
    rides_server("rides")
    plan_server("plan")
  })
}
