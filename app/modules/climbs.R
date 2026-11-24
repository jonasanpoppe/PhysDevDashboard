# Climb record module: per climb the ascents (clean), the best clean effort,
# the last attempt and every attempt as a status icon. Pinned climbs first.
# The table itself is built by climb_record() in R/climb_record.R.

climb_status_icons <- list(
  clean = list(icon = "✓", class = "text-success", label = "clean"),
  top   = list(icon = "✓", class = "text-success", label = "clean (stop at the top)"),
  foot  = list(icon = "◐", class = "text-warning", label = "foot down"),
  early = list(icon = "✕", class = "text-danger",  label = "stopped"),
  walk  = list(icon = "⇣", class = "text-danger",  label = "walked")
)

climb_surface_labels <- c(cobbles = "Cobbles", part_cobbles = "Part cobbles",
                          asphalt = "Asphalt", unknown = "Not set yet")

climbs_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Climbs",
      checkboxGroupInput(
        ns("surface"), "Surface",
        choices = setNames(names(climb_surface_labels), climb_surface_labels),
        selected = names(climb_surface_labels)
      ),
      uiOutput(ns("bike_filter")),
      checkboxInput(ns("scope"), "Only rides that count for the record", value = FALSE),
      helpText("The record counts rides with enough climbing in total",
               "(scope_min_gain_m in config/athlete.yml).")
    ),
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-baseline flex-wrap gap-2",
        "Climb record",
        textOutput(ns("summary"), inline = TRUE) %>% tagAppendAttributes(class = "small text-muted")
      ),
      div(class = "table-responsive", uiOutput(ns("table"))),
      card_footer(
        class = "small text-muted",
        tagList(lapply(names(climb_status_icons), \(s) {
          i <- climb_status_icons[[s]]
          span(class = "me-3", span(class = i$class, i$icon), " ", i$label)
        })),
        br(),
        "Clean = no stop, or a stop only at the very top. Best = shortest moving time",
        "of a clean ascent; W/kg is an estimate (\u00B115%, see Engine).",
        "History runs oldest to newest; hover an icon for its date."
      )
    )
  )
}

climbs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    efforts <- query("SELECT * FROM strava_climb_efforts")
    climbs  <- query("SELECT * FROM climbs")
    bikes   <- query("SELECT * FROM bikes")

    ridden_bikes <- bikes %>% filter(bike_id %in% efforts$bike_id)
    output$bike_filter <- renderUI({
      checkboxGroupInput(
        ns("bikes"), "Bike",
        choices = setNames(ridden_bikes$bike_id, coalesce(ridden_bikes$nickname, ridden_bikes$bike)),
        selected = ridden_bikes$bike_id
      )
    })

    record <- reactive({
      req(input$surface)
      climb_record(efforts, climbs, bikes,
                   surfaces = input$surface,
                   bike_ids = input$bikes %||% ridden_bikes$bike_id,
                   scope_only = isTRUE(input$scope))
    })

    output$summary <- renderText({
      r <- record()
      sprintf("%d climbs · %d ascents · %d clean", nrow(r), sum(r$ascents), sum(r$clean))
    })

    output$table <- renderUI({
      r <- record()
      if (nrow(r) == 0) return(p(class = "text-muted m-3", "No ascents with these filters."))
      tags$table(
        class = "table table-sm align-middle mb-0",
        tags$thead(tags$tr(
          tags$th("Climb"), tags$th(class = "text-end", "Ascents (clean)"),
          tags$th("Best clean"), tags$th("Last attempt"), tags$th("History")
        )),
        tags$tbody(lapply(seq_len(nrow(r)), \(k) climb_row(r[k, ])))
      )
    })
  })
}

climb_row <- function(x) {
  status_icon <- function(status, date = NULL, extra = NULL) {
    i <- climb_status_icons[[status]] %||% list(icon = "?", class = "", label = status)
    span(class = paste(i$class, "me-1"), title = paste(c(format(date, "%d %b %Y"), i$label, extra), collapse = " · "),
         i$icon)
  }
  h <- x$history[[1]]
  tags$tr(
    tags$td(
      if (isTRUE(x$pinned)) span(class = "text-muted me-1", title = "pinned", "★"),
      strong(x$climb), br(),
      span(class = "small text-muted",
           sprintf("%s · %s m · %s%%", climb_surface_labels[[x$surface]],
                   format(round(x$length_m), big.mark = ","), x$avg_grade_pct))
    ),
    tags$td(class = "text-end", sprintf("%d (%d)", x$ascents, x$clean)),
    tags$td(
      if (is.na(x$best_s)) span(class = "text-muted", "none yet")
      else tagList(strong(fmt_mmss(x$best_s)),
                   if (!is.na(x$best_wkg)) span(class = "text-muted", sprintf(" \u00B7 ~%.1f W/kg", x$best_wkg)),
                   br(),
                   span(class = "small text-muted", format(x$best_date, "%d %b %Y"), "·", x$best_bike))
    ),
    tags$td(status_icon(x$last_status), format(x$last_date, "%d %b %Y")),
    tags$td(lapply(seq_len(nrow(h)), \(j) {
      status_icon(h$status[j], h$date[j],
                  c(fmt_mmss(h$moving_s[j]), if (h$stop_s[j] > 0) paste0(round(h$stop_s[j]), " s stopped")))
    }))
  )
}
