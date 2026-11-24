# PhysDevDashboard: Shiny app. Run with `just app`.
#
# One nav_panel per module; modules live in app/modules/. Shared plotting and
# theme code lives in R/ so it can be tested without Shiny.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# runApp("app") sets the working directory to app/
project_root <- normalizePath("..")

# local = TRUE: runApp() evaluates this file in its own environment, so the
# sourced functions must live there too, next to project_root.
source(file.path(project_root, "R", "theme_physdev.R"), local = TRUE)
source(file.path(project_root, "R", "plot_vitals.R"), local = TRUE)
source(file.path(project_root, "R", "climb_record.R"), local = TRUE)
source(file.path(project_root, "R", "plot_engine.R"), local = TRUE)
source(file.path(project_root, "R", "plot_fitness.R"), local = TRUE)
source(file.path(project_root, "R", "weight_plan.R"), local = TRUE)
source(file.path(project_root, "R", "plot_weight.R"), local = TRUE)
source(file.path(project_root, "R", "e1rm.R"), local = TRUE)
source(file.path(project_root, "R", "strength.R"), local = TRUE)
source(file.path(project_root, "R", "plot_strength.R"), local = TRUE)
source("db.R", local = TRUE)
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f, local = TRUE)

ui <- page_navbar(
  title = "PhysDevDashboard",
  theme = bs_theme(
    bg = physdev$bg,
    fg = physdev$fg,
    primary = physdev$coral,
    secondary = physdev$fg_muted
  ),
  nav_panel("Weight", weight_ui("weight")),
  nav_panel("Strength", strength_ui("strength")),
  nav_panel("Vitals", vitals_ui("vitals")),
  nav_panel("Climbs", climbs_ui("climbs")),
  # Engine: how strong (power by duration) and whether it's changing (fitness)
  nav_panel("Engine", engine_ui("engine"))
)

server <- function(input, output, session) {
  weight_server("weight")
  strength_server("strength")
  vitals_server("vitals")
  climbs_server("climbs")
  engine_server("engine")
}

shinyApp(ui, server)
