# Reliquary tab: one card per relic, sealed or unlocked, and the level path.
# Status from the database (relic_status; R/relics.R builds it), copy from
# oath_config (config/oath.yml, `relic_copy:`). Card art, made only once a
# relic is unlocked, lives outside the repo in config/relics/<id>.jpg|png and
# is served under relic-art/. Style: app/www/oath.css.

# (PDD_RELIC_ART points elsewhere, for the tests)
relic_art_dir <- Sys.getenv("PDD_RELIC_ART", file.path(project_root, "config", "relics"))

# The art file for a relic, or NULL
relic_art <- function(id) {
  for (ext in c("jpg", "jpeg", "png", "webp")) {
    f <- file.path(relic_art_dir, paste0(id, ".", ext))
    if (file.exists(f)) return(paste0("relic-art/", basename(f)))
  }
  NULL
}

# The status table, or NULL before the first build that writes it
relic_table <- function() {
  if (!db_has_table("relic_status")) return(NULL)
  query("SELECT * FROM relic_status ORDER BY ord")
}

reliquary_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("page"))
}

relic_card <- function(r, plain = FALSE) {
  art <- if (r$unlocked) relic_art(r$id)
  panel_style <- if (!is.null(art)) sprintf("background-image: url('%s');", art) else
    sprintf("background: linear-gradient(160deg, %s, %s);", r$colour, physdev_palettes$oath$fg)
  div(
    class = paste(c("relic-card", if (r$unlocked) "relic-unlocked" else "relic-sealed"), collapse = " "),
    div(class = paste(c("relic-art", if (!is.null(art)) "relic-has-art"), collapse = " "), style = panel_style,
        if (is.null(art)) div(class = "relic-num", r$id),
        div(class = "relic-state", if (r$unlocked) "UNLOCKED" else "SEALED")),
    if (plain) div(class = "relic-pt", r$plain) else tagList(
      div(class = "relic-pt", r$name_pt),
      div(class = "relic-en", r$name_en)),
    div(class = "relic-unlock", r$unlock),
    div(class = "relic-hint", r$hint),
    div(class = "relic-foot",
        div(class = "relic-palette", if (plain) "" else r$palette),
        div(class = "relic-level", r$level_text))
  )
}

reliquary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    if (dir.exists(relic_art_dir)) addResourcePath("relic-art", relic_art_dir)
    status <- relic_table()
    cfg <- jsonlite::fromJSON(query("SELECT json FROM oath_config")$json, simplifyVector = FALSE)
    copy <- cfg$relic_copy %||% list()

    output$page <- renderUI({
      plain <- identical(ui_mode(), "corporate")
      pc <- copy$plain %||% list()
      if (is.null(status) || nrow(status) == 0) {
        return(div(class = "oath", div(class = "oath-frame",
                   p("No relics yet: add `relics:` to config/oath.yml and run just build."))))
      }
      level <- relic_level(status, cfg)
      div(
        class = paste(c("oath", "reliquary", if (plain) "oath-corporate"), collapse = " "),
        div(
          class = "oath-frame oath-frame-wide",
          div(class = "relic-head",
              div(
                if (!plain) div(class = "oath-kicker", copy$kicker),
                h1(if (plain) pc$title %||% "Milestones" else
                  tagList(span(class = "oath-cap", substr(copy$title %||% "The Reliquary", 1, 1)),
                          substring(copy$title %||% "The Reliquary", 2)))),
              div(class = "relic-intro", if (plain) pc$intro %||% copy$intro else copy$intro)),
          div(class = "relic-grid",
              lapply(seq_len(nrow(status)), \(i) relic_card(status[i, ], plain)),
              div(class = "relic-path",
                  div(class = "relic-path-title", if (plain) "Level" else copy$path_title %||% "The Level Path"),
                  div(class = "relic-path-now", sprintf("Level %d", level)),
                  div(class = "relic-path-head", copy$path_head),
                  div(class = "relic-path-body", sprintf("%d of %d relics unlocked. %s",
                                                         sum(status$unlocked), nrow(status),
                                                         if (plain) "" else copy$path_body %||% "")),
                  if (!plain && !is.null(copy$path_note)) div(class = "relic-path-note", copy$path_note))),
          div(class = "oath-footer",
              div(class = "oath-footer-left", copy$prompt %||% ""),
              if (!plain) div(class = "oath-footer-right relic-motto", copy$motto %||% ""))
        )
      )
    })
  })
}
