# PhysDevDashboard: Shiny app. Run with `just app`.
#
# Four navbar tabs by kind (the oath, the body, the bike, exploring); a tab
# with more than one page shows them as pills. One page per module; modules
# live in app/modules/. Shared plotting and theme code lives in R/ so it can
# be tested without Shiny.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# Shiny's validate() and need(), bound here so a package attached later
# can't mask them: Posit Connect's launcher attaches jsonlite, whose
# validate(txt) checks JSON text (every validate(need(...)) then failed)
validate <- shiny::validate
need <- shiny::need

# runApp("app") sets the working directory to app/
project_root <- normalizePath("..")

# local = TRUE: runApp() evaluates this file in its own environment, so the
# sourced functions must live there too, next to project_root.
source(file.path(project_root, "R", "common.R"), local = TRUE)   # formatters, the session count
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
source(file.path(project_root, "R", "scenarios.R"), local = TRUE)
source(file.path(project_root, "R", "plot_scenarios.R"), local = TRUE)
source("db.R", local = TRUE)
source("fill_plot.R", local = TRUE)
source("pages.R", local = TRUE)    # app_pages: every page and its navbar tab
physdev_font()   # register the web font once at start-up, not on the first figure
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f, local = TRUE)

# Two looks, switched in the navbar: the Oath of Glory scheme (parchment,
# Cinzel and Cormorant Garamond, linked from Google) and corporate mode
# (white and slate, system sans). Charts take their colours from `physdev`,
# swapped with the mode; ui_mode() makes every figure redraw. Both are
# app-wide, not per session: fine for a one-person app, but two browsers in
# different modes would follow the last switch.
# PDD_LOOK = "corporate" (the demo) locks the corporate look: the page,
# the charts and the words start corporate (no oath look while it loads),
# the switch is hidden and nothing remembered in the browser applies.
look_locked <- identical(Sys.getenv("PDD_LOOK"), "corporate")
start_mode <- if (look_locked) "corporate" else "oath"
physdev <- physdev_palettes[[start_mode]]
ui_mode <- reactiveVal(start_mode)

# Sidebars: open on desktop, closed on phones so the figure comes first
sidebar_open <- list(desktop = "open", mobile = "closed")

app_rules <- function(mode) {
  shared <- "
     .tiles-compact .value-box-area { padding: 0.6rem 0.75rem !important; }
     .tiles-compact .value-box-title { font-size: 0.8rem; margin-bottom: 0.15rem; }
     .tiles-compact .value-box-value { font-size: 1.25rem; line-height: 1.2; margin-bottom: 0.25rem; }
     .tiles-compact p { font-size: 0.7rem; line-height: 1.25; }
     .navbar .form-switch { margin: 0 0.5rem; }
     /* Tab and pill names in both looks (look_label) */
     body.pdd-corporate .look-oath, body:not(.pdd-corporate) .look-plain { display: none; }
     /* The pills of a tab with several pages, above the page */
     #oath_page, #body_page { padding: 0.5rem 1rem 0; gap: 0.25rem; flex-wrap: wrap; }
     /* A page under the pills looks as it did as a tab of its own: no frame */
     #oath_page + .tab-content .bslib-sidebar-layout, #body_page + .tab-content .bslib-sidebar-layout {
       border: none; border-radius: 0; }
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
# the app opens in the oath look; locked: see look_locked above)
corporate_js <- paste0(sprintf("
$(document).on('shiny:connected', function() {
  var locked = %s, on = locked;
  if (!locked) try { on = localStorage.getItem('pdd-corporate') === '1'; } catch (e) {}
  if (on) $('#corporate').prop('checked', true).trigger('change');
});", tolower(look_locked)), "
$(document).on('change', '#corporate', function() {
  document.body.classList.toggle('pdd-corporate', this.checked);
  try { localStorage.setItem('pdd-corporate', this.checked ? '1' : '0'); } catch (e) {}
});")

# A dropped connection (a phone asleep, another app, wifi to mobile data
# over a VPN) ends the session; the page would stay grey until reloaded.
# Reload as soon as the page is visible again; at most once per 10 s, so a
# stopped app shows the browser's error instead of reloading in a loop. The
# reload lands on the same page (the URL keeps it, see the server).
reconnect_js <- "
$(document).on('shiny:disconnected', function() {
  var reload = function() {
    var last = 0;
    try { last = +sessionStorage.getItem('pdd-reload') || 0; } catch (e) {}
    if (Date.now() - last < 10000) return;
    try { sessionStorage.setItem('pdd-reload', String(Date.now())); } catch (e) {}
    location.reload();
  };
  if (document.visibilityState === 'visible') setTimeout(reload, 500);
  document.addEventListener('visibilitychange', function() {
    if (document.visibilityState === 'visible') reload();
  });
});"

# A label in both looks: the oath word, or the corporate one (the switch sets
# body.pdd-corporate; app_rules() shows one of the two)
look_label <- function(icon, oath, plain = oath) {
  tagList(icon, " ", span(class = "look-oath", oath), span(class = "look-plain", plain))
}

ui <- page_navbar(
  id = "main",
  title = "PhysDevDashboard",
  theme = app_themes[[start_mode]],
  # The --oath-* colour tokens for oath.css, from the one palette (R/theme_physdev.R)
  header = tagList(
    tags$head(tags$script(HTML(corporate_js)), tags$script(HTML(reconnect_js)),
              tags$style(HTML(physdev_css_vars()))),
    # Locked: the page's class before anything is drawn (corporate_js sets it
    # on the switch otherwise, after the connection)
    if (look_locked) tags$script(HTML("document.body.classList.add('pdd-corporate');")),
    # A spinner on a figure while it redraws, so a slow page doesn't look frozen
    useBusyIndicators()),
  # The oath: Home (the hub: poster, Today, Quest Board, relic shelf), then
  # its full pages (config/oath.yml; R/quests.R, R/relics.R, R/roadmap.R,
  # R/scenarios.R)
  nav_panel(look_label("🛡️", "Oath", "Plan"), value = "oath",
    navset_pill(id = "oath_page",
      nav_panel(look_label("📜", "Quest Board", "Goals"), value = "oath", oath_ui("oath")),
      nav_panel(look_label("🏺", "Reliquary", "Milestones"), value = "reliquary", reliquary_ui("reliquary")),
      nav_panel(look_label("🗺️", "Roadmap", "Timeline"), value = "roadmap", roadmap_ui("roadmap")),
      # Am I on track, under adjustable assumptions
      nav_panel(look_label("🧭", "Scenarios"), value = "scenarios", scenarios_ui("scenarios")))),
  # The body: the measurements per domain
  nav_panel(look_label("💪", "Body", "Health"), value = "body",
    navset_pill(id = "body_page",
      nav_panel(look_label("⚖️", "Weight"), value = "weight", weight_ui("weight")),
      nav_panel(look_label("🏋️", "Strength"), value = "strength", strength_ui("strength")),
      nav_panel(look_label("❤️", "Vitals"), value = "vitals", vitals_ui("vitals")))),
  # Engine: everything on the bike (climbs, fitness, riding), its own selector
  nav_panel(look_label("🚴", "Engine"), value = "engine", engine_ui("engine")),
  # Analytics: any two variables side by side
  nav_panel(look_label("🔍", "Analytics"), value = "analytics", analytics_ui("analytics")),
  nav_spacer(),
  # Hidden, not removed, when the look is locked: the input still switches the theme
  nav_item(div(style = if (look_locked) "display: none;", input_switch("corporate", "Corporate mode", value = look_locked)))
)

server <- function(input, output, session) {
  # Corporate mode: theme, chart colours and the oath pages' words (the tab
  # and pill names switch in the browser, corporate_js)
  observeEvent(input$corporate, {
    mode <- if (isTRUE(input$corporate)) "corporate" else "oath"
    physdev <<- physdev_palettes[[mode]]
    session$setCurrentTheme(app_themes[[mode]])
    ui_mode(mode)
  }, ignoreInit = TRUE)

  # The tab and page in the URL (?tab=body&page=weight): a reload, or a
  # bookmark, opens where you were
  restore <- parseQueryString(isolate(session$clientData$url_search))
  if (!is.null(restore$tab) && restore$tab %in% unlist(app_pages)) {
    nav_select("main", restore$tab, session = session)
    if (!is.null(restore$page) && identical(app_pages[[restore$page]], restore$tab) && restore$page != restore$tab) {
      nav_select(paste0(restore$tab, "_page"), restore$page, session = session)
    }
  }
  observeEvent(list(input$main, input$oath_page, input$body_page), {
    req(input$main)
    page <- switch(input$main, oath = input$oath_page, body = input$body_page, NULL)
    updateQueryString(paste0("?tab=", input$main, if (!is.null(page)) paste0("&page=", page)),
                      mode = "replace", session = session)
  }, ignoreInit = TRUE)

  # A click on a quest card opens the tab with its data
  oath_follow(oath_server("oath"), session, app_pages)
  reliquary_server("reliquary")
  roadmap_server("roadmap")
  scenarios_server("scenarios")
  weight_server("weight")
  strength_server("strength")
  vitals_server("vitals")
  engine_server("engine")
  analytics_server("analytics")
}

shinyApp(ui, server)
