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
source(file.path(project_root, "R", "forecast.R"), local = TRUE)
source(file.path(project_root, "R", "plot_forecast.R"), local = TRUE)
source(file.path(project_root, "R", "plot_strength.R"), local = TRUE)
source(file.path(project_root, "R", "riding.R"), local = TRUE)
source(file.path(project_root, "R", "plot_riding.R"), local = TRUE)
source(file.path(project_root, "R", "analytics.R"), local = TRUE)
source(file.path(project_root, "R", "plot_analytics.R"), local = TRUE)
source(file.path(project_root, "R", "homogenise.R"), local = TRUE)
source(file.path(project_root, "R", "quests.R"), local = TRUE)
source("db.R", local = TRUE)
source("fill_plot.R", local = TRUE)
physdev_font()   # register the web font once at start-up, not on the first figure
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f, local = TRUE)

ui <- page_navbar(
  id = "main",
  title = "PhysDevDashboard",
  # The Oath of Glory scheme on every tab (palette in R/theme_physdev.R).
  # Fonts linked from Google, not downloaded: Cinzel for headings,
  # Cormorant Garamond for text.
  theme = bs_add_rules(
    bs_theme(
      bg = physdev$bg,
      fg = physdev$fg,
      primary = physdev$purple,
      secondary = physdev$bronze,
      base_font = font_google("Cormorant Garamond", wght = c(500, 600, 700), local = FALSE),
      heading_font = font_google("Cinzel", wght = c(500, 700), local = FALSE),
      font_scale = 1.1,
      "navbar-bg" = physdev$purple,
      "card-bg" = physdev$bg_card,
      "border-color" = physdev$fg_subtle
    ),
    # Compact status tiles: a row of eight fits on a laptop screen
    ".tiles-compact .value-box-area { padding: 0.6rem 0.75rem !important; }
     .tiles-compact .value-box-title { font-size: 0.8rem; margin-bottom: 0.15rem; }
     .tiles-compact .value-box-value { font-size: 1.25rem; line-height: 1.2; margin-bottom: 0.25rem; }
     .tiles-compact p { font-size: 0.7rem; line-height: 1.25; }
     .card { border-color: #C9B48C; }
     .navbar .navbar-brand { font-family: Cinzel, Georgia, serif; letter-spacing: 1px; font-size: 1.5rem; }
     /* Tab names half as large again: Cormorant runs small */
     .navbar .nav-link { font-size: 1.5rem; }
     /* Phones: smaller type, and wide tables scroll inside their card
        instead of pushing the page past its margins */
     @media (max-width: 576px) {
       html { font-size: 14px; }
       .tiles-compact .value-box-value { font-size: 1.05rem; }
       .card-body, .bslib-sidebar-layout > .main { overflow-x: auto; }
       table { font-size: 0.85rem; }
     }"
  ),
  # Home: the oath and the Quest Board (config/oath.yml)
  nav_panel("🛡️ Home", value = "oath", oath_ui("oath")),
  nav_panel("⚖️ Weight", value = "weight", weight_ui("weight")),
  nav_panel("🏋️ Strength", value = "strength", strength_ui("strength")),
  nav_panel("❤️ Vitals", value = "vitals", vitals_ui("vitals")),
  # Engine: everything on the bike (climbs, fitness, riding)
  nav_panel("🚴 Engine", value = "engine", engine_ui("engine")),
  # Analytics: any two variables side by side
  nav_panel("🔍 Analytics", value = "analytics", analytics_ui("analytics"))
)

server <- function(input, output, session) {
  # A click on a quest card opens the tab with its data
  oath_follow(oath_server("oath"), session)
  weight_server("weight")
  strength_server("strength")
  vitals_server("vitals")
  engine_server("engine")
  analytics_server("analytics")
}

shinyApp(ui, server)
