# Roadmap tab: the year as lanes x 12 months (config/oath.yml, `roadmap:`),
# with a today line, phases done / current / ahead, and the gates ticked
# when their relic unlocks. Tables: roadmap_phases, roadmap_gates (built by
# R/roadmap.R). Style: app/www/oath.css.

roadmap_months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

# Phase text: escaped, with **bold** kept as bold
roadmap_text <- function(x) {
  HTML(gsub("\\*\\*(.+?)\\*\\*", "<b>\\1</b>", htmltools::htmlEscape(x), perl = TRUE))
}

roadmap_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("page"))
}

roadmap_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    cfg <- jsonlite::fromJSON(query("SELECT json FROM oath_config")$json, simplifyVector = FALSE)
    rm <- cfg$roadmap %||% list()
    phases <- if (db_has_table("roadmap_phases")) query("SELECT * FROM roadmap_phases ORDER BY ord") else NULL
    gates <- if (db_has_table("roadmap_gates")) query("SELECT * FROM roadmap_gates ORDER BY ord") else NULL

    output$page <- renderUI({
      plain <- identical(ui_mode(), "corporate")
      pc <- rm$plain %||% list()
      if (is.null(phases) || nrow(phases) == 0) {
        return(div(class = "oath", div(class = "oath-frame",
                   p("No roadmap yet: add `roadmap:` to config/oath.yml and run just build."))))
      }
      as_of <- phases$as_of[1]
      today <- roadmap_today(cfg, as_of)
      lanes <- rm$lanes
      title <- if (plain) pc$title %||% "Plan" else rm$title %||% "The Roadmap"
      div(
        class = paste(c("oath", "roadmap", if (plain) "oath-corporate"), collapse = " "),
        div(
          class = "oath-frame oath-frame-wide",
          div(class = "relic-head",
              div(if (!plain) div(class = "oath-kicker", rm$kicker),
                  h1(if (plain) title else tagList(span(class = "oath-cap", substr(title, 1, 1)), substring(title, 2)))),
              div(class = "relic-intro", if (plain) pc$intro %||% rm$intro else rm$intro)),
          div(class = "road-scroll",
              div(class = "road-grid",
                  div(class = "road-month road-corner"),
                  lapply(seq_along(roadmap_months), \(m) div(class = "road-month", style = sprintf("grid-column: %d;", m + 1), roadmap_months[m])),
                  lapply(seq_along(lanes), \(i) div(
                    class = "road-lane", style = sprintf("grid-row: %d; color: %s;", i + 1, lanes[[i]]$colour %||% "inherit"),
                    if (plain) lanes[[i]]$plain %||% lanes[[i]]$label else lanes[[i]]$label)),
                  lapply(seq_len(nrow(phases)), \(i) {
                    ph <- phases[i, ]
                    div(class = sprintf("road-phase road-%s road-%s", ph$style, ph$state),
                        style = sprintf("grid-column: %d / span %d; grid-row: %d;", ph$col, ph$span, ph$lane_ord + 1),
                        title = sprintf("%s to %s (%s)", fmt_day(ph$from), fmt_day(ph$to), ph$state),
                        roadmap_text(ph$text))
                  }),
                  if (!is.na(today)) div(class = "road-today",
                      style = sprintf("left: calc(var(--road-label) + 6px + (100%% - var(--road-label) - 6px) * %.4f);", today),
                      span(sprintf("Today · %s", fmt_day(as_of)))))),
          # Outside the year there is no today line; say why
          if (is.na(today)) div(class = "oath-note", sprintf("As of %s: the plan's year %s",
              fmt_day(as_of), if (as_of < as.Date(sprintf("%s-01-01", cfg$year))) "hasn't started yet" else "is over")),
          if (!is.null(gates) && nrow(gates)) div(class = "road-gates",
              tags$b(if (plain) "Checkpoints: " else "Gates: "),
              lapply(seq_len(nrow(gates)), \(i) {
                g <- gates[i, ]
                span(class = paste("road-gate", if (g$ticked) "road-gate-done"),
                     sprintf("%s %s%s", if (g$ticked) "☑" else "☐", g$label,
                             if (g$ticked) sprintf(" (%s)", fmt_day(g$ticked_on)) else
                               if (!is.na(g$date)) sprintf(" (%s)", format(g$date, "%b")) else ""))
              })),
          if (!plain && !is.null(rm$motto)) div(class = "oath-footer",
              div(class = "oath-footer-left", "Past phases are dimmed; the current one is outlined."),
              div(class = "oath-footer-right relic-motto", rm$motto))
        )
      )
    })
  })
}
