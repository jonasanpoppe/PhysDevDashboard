# Home tab: the Oath poster with a live Quest Board. Copy and quests come
# from config/oath.yml, through the database (oath_config, quest_board;
# R/quests.R builds them). Style: app/www/oath.css, scoped to this tab.

oath_accents <- c("#4B2A6B", "#5E3550", "#6B3F2A", "#7A4A22")

oath_fonts <- "https://fonts.googleapis.com/css2?family=Cinzel:wght@500;700&family=Cormorant+Garamond:ital,wght@0,500;0,600;0,700;1,500&family=UnifrakturMaguntia&display=swap"

oath_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = oath_fonts),
      tags$link(rel = "stylesheet", href = "oath.css")
    ),
    uiOutput(ns("page"))
  )
}

# A tenet with its first letter as a blackletter capital
oath_tenet <- function(text, late) {
  div(class = paste("oath-tenet", if (late) "oath-late"),
      span(class = "oath-cap", substr(text, 1, 1)), substr(text, 2, nchar(text)))
}

oath_card <- function(q) {
  accent <- oath_accents[min(max(q$accent, 1L), length(oath_accents))]
  div(
    class = "oath-card", style = sprintf("--accent: %s;", accent),
    div(class = "oath-card-label", q$label),
    div(class = "oath-card-headline", q$headline),
    div(class = "oath-card-by", q$by),
    if (!is.na(q$share)) div(class = "oath-bar", div(style = sprintf("width: %.0f%%;", 100 * q$share))),
    if (nzchar(q$progress)) div(class = "oath-card-progress", q$progress),
    if (nzchar(q$extras)) div(class = "oath-card-extra", q$extras),
    if (nzchar(q$source_note)) div(class = "oath-card-source", q$source_note)
  )
}

oath_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    board <- query("SELECT * FROM quest_board ORDER BY ord")
    cfg <- jsonlite::fromJSON(query("SELECT json FROM oath_config")$json, simplifyVector = FALSE)
    copy <- cfg$copy

    output$page <- renderUI({
      main <- board %>% filter(role == "main")
      side <- board %>% filter(role != "main")
      # Level: the base plus levels earned in the Reliquary (not built yet)
      level <- cfg$level_base %||% 1
      tenets <- unlist(copy$tenets)
      div(
        class = "oath",
        div(
          class = "oath-frame",
          div(class = "oath-head",
              div(class = "oath-kicker", copy$kicker),
              h1(copy$title),
              div(class = "oath-subtitle", copy$subtitle),
              div(class = "oath-rule")),
          if (length(tenets)) div(class = "oath-tenets",
              lapply(seq_along(tenets), \(i) oath_tenet(tenets[i], late = i > ceiling(length(tenets) / 2)))),
          div(class = "oath-main",
              div(class = "oath-main-label", copy$main_label %||% "Main quest"),
              div(class = "oath-main-body",
                  div(class = "oath-main-quest", copy$main_quest),
                  div(class = "oath-main-sub", copy$main_sub),
                  if (nrow(main)) div(class = "oath-main-now",
                      sprintf("%s now: %s. %s", main$label[1], main$headline[1], main$progress[1])))),
          div(class = "oath-section",
              div(class = "oath-section-head",
                  h2(copy$board_title %||% "The Quest Board"),
                  div(class = "oath-note", sprintf("%s \u00B7 as of %s", copy$board_note %||% "now → target",
                                                   fmt_day(board$as_of[1])))),
              div(class = "oath-board", lapply(seq_len(nrow(side)), \(i) oath_card(side[i, ])))),
          if (!is.null(copy$rules)) div(class = "oath-section",
              h2(copy$rules_title %||% "Rules"),
              div(class = "oath-rules", copy$rules)),
          div(class = "oath-footer",
              div(class = "oath-footer-left", copy$sworn),
              div(class = "oath-footer-right", sub("{level}", level, copy$footer %||% "", fixed = TRUE)))
        )
      )
    })
  })
}
