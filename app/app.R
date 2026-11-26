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
source(file.path(project_root, "R", "relics.R"), local = TRUE)
source(file.path(project_root, "R", "roadmap.R"), local = TRUE)
source("db.R", local = TRUE)
source("fill_plot.R", local = TRUE)
physdev_font()   # register the web font once at start-up, not on the first figure
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f, local = TRUE)

# Two looks, switched in the navbar: the Oath of Glory scheme (parchment,
# Cinzel and Cormorant Garamond, linked from Google) and corporate mode
# (white and slate, system sans). Charts take their colours from `physdev`,
# swapped with the mode; ui_mode() makes every figure redraw. Both are
# app-wide, not per session: fine for a one-person app, but two browsers in
# different modes would follow the last switch.
ui_mode <- reactiveVal("oath")

app_rules <- function(mode) {
  shared <- "
     .tiles-compact .value-box-area { padding: 0.6rem 0.75rem !important; }
     .tiles-compact .value-box-title { font-size: 0.8rem; margin-bottom: 0.15rem; }
     .tiles-compact .value-box-value { font-size: 1.25rem; line-height: 1.2; margin-bottom: 0.25rem; }
     .tiles-compact p { font-size: 0.7rem; line-height: 1.25; }
     .navbar .form-switch { margin: 0 0.5rem; }
     /* Phones: smaller type, and wide tables scroll inside their card
        instead of pushing the page past its margins */
     @media (max-width: 576px) {
       html { font-size: 14px; }
       .tiles-compact .value-box-value { font-size: 1.05rem; }
       .card-body, .bslib-sidebar-layout > .main { overflow-x: auto; }
       table { font-size: 0.85rem; }
     }"
  look <- if (mode == "oath") "
     .navbar .navbar-brand { font-family: Cinzel, Georgia, serif; letter-spacing: 1px; font-size: 1.5rem; }
     /* Tab names half as large again: Cormorant runs small */
     .navbar .nav-link { font-size: 1.5rem; }" else "
     .navbar .navbar-brand { font-size: 1.25rem; }
     .navbar .nav-link { font-size: 1.1rem; }"
  paste(shared, look, sprintf(".card { border-color: %s; }", physdev_palettes[[mode]]$fg_subtle))
}

app_theme <- function(mode) {
  p <- physdev_palettes[[mode]]
  fonts <- if (mode == "oath") list(
    base_font = font_google("Cormorant Garamond", wght = c(500, 600, 700), local = FALSE),
    heading_font = font_google("Cinzel", wght = c(500, 700), local = FALSE),
    font_scale = 1.1
  ) else list(
    base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    heading_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    font_scale = 1
  )
  bs_add_rules(
    do.call(bs_theme, c(list(
      bg = p$bg, fg = p$fg, primary = p$purple, secondary = p$bronze,
      "navbar-bg" = p$purple, "card-bg" = p$bg_card, "border-color" = p$fg_subtle
    ), fonts)),
    app_rules(mode)
  )
}
app_themes <- list(oath = app_theme("oath"), corporate = app_theme("corporate"))

# The switch remembers its state in this browser (localStorage; without it,
# the app opens in the oath look)
corporate_js <- "
$(document).on('shiny:connected', function() {
  var on = false;
  try { on = localStorage.getItem('pdd-corporate') === '1'; } catch (e) {}
  if (on) $('#corporate').prop('checked', true).trigger('change');
});
$(document).on('change', '#corporate', function() {
  try { localStorage.setItem('pdd-corporate', this.checked ? '1' : '0'); } catch (e) {}
});"

ui <- page_navbar(
  id = "main",
  title = "PhysDevDashboard",
  theme = app_themes$oath,
  header = tags$head(tags$script(HTML(corporate_js))),
  # Home: the oath and the Quest Board (config/oath.yml)
  nav_panel("🛡️ Home", value = "oath", oath_ui("oath")),
  # Reliquary: relics unlocked by rules (config/oath.yml, R/relics.R)
  nav_panel("🏺 Reliquary", value = "reliquary", reliquary_ui("reliquary")),
  # Roadmap: the year's phases, today and the gates
  nav_panel("🗺️ Roadmap", value = "roadmap", roadmap_ui("roadmap")),
  nav_panel("⚖️ Weight", value = "weight", weight_ui("weight")),
  nav_panel("🏋️ Strength", value = "strength", strength_ui("strength")),
  nav_panel("❤️ Vitals", value = "vitals", vitals_ui("vitals")),
  # Engine: everything on the bike (climbs, fitness, riding)
  nav_panel("🚴 Engine", value = "engine", engine_ui("engine")),
  # Analytics: any two variables side by side
  nav_panel("🔍 Analytics", value = "analytics", analytics_ui("analytics")),
  nav_spacer(),
  nav_item(input_switch("corporate", "Corporate mode", value = FALSE))
)

server <- function(input, output, session) {
  # Corporate mode: theme, chart colours and the Home tab's words
  observeEvent(input$corporate, {
    mode <- if (isTRUE(input$corporate)) "corporate" else "oath"
    physdev <<- physdev_palettes[[mode]]
    session$setCurrentTheme(app_themes[[mode]])
    ui_mode(mode)
  }, ignoreInit = TRUE)

  # A click on a quest card opens the tab with its data
  oath_follow(oath_server("oath"), session)
  reliquary_server("reliquary")
  roadmap_server("roadmap")
  weight_server("weight")
  strength_server("strength")
  vitals_server("vitals")
  engine_server("engine")
  analytics_server("analytics")
}

shinyApp(ui, server)
