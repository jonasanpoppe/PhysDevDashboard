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
  div(class = paste(c("oath-tenet", if (late) "oath-late"), collapse = " "),
      span(class = "oath-cap", substr(text, 1, 1)), substr(text, 2, nchar(text)))
}

# A card; with a link, clicking it sends {tab, inputs} to input$goto (the
# app's server switches the tab and sets the inputs there)
oath_card <- function(q, ns, plain = FALSE) {
  accent <- oath_accents[min(max(q$accent, 1L), length(oath_accents))]
  linked <- !is.na(q$link_tab)
  click <- if (linked) sprintf(
    "Shiny.setInputValue('%s', {tab: '%s', inputs: %s}, {priority: 'event'})",
    ns("goto"), q$link_tab, q$link_inputs)
  tags$div(
    class = paste(c("oath-card", if (linked) "oath-link"), collapse = " "), style = sprintf("--accent: %s;", accent),
    role = if (linked) "link", tabindex = if (linked) "0", onclick = click,
    onkeydown = if (linked) sprintf("if (event.key === 'Enter') { %s }", click),
    title = if (linked) "Open the data behind this quest",
    div(class = "oath-card-label", if (plain) q$plain else q$label),
    div(class = "oath-card-headline", if (plain) q$plain_headline else q$headline),
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
      # Corporate mode: the same numbers in plain words, without the poster's
      # flavour (kicker, tenets, level footer); plain copy from copy$plain
      plain <- identical(ui_mode(), "corporate")
      pc <- copy$plain %||% list()
      main <- board %>% filter(role == "main")
      side <- board %>% filter(role != "main")
      # Level: the base plus levels earned in the Reliquary (not built yet)
      level <- cfg$level_base %||% 1
      tenets <- if (plain) character(0) else unlist(copy$tenets)
      rules <- if (plain) pc$rules %||% copy$rules else copy$rules
      div(
        class = paste(c("oath", if (plain) "oath-corporate"), collapse = " "),
        div(
          class = "oath-frame",
          div(class = "oath-head",
              if (!plain) div(class = "oath-kicker", copy$kicker),
              h1(if (plain) pc$title %||% "Goals" else copy$title),
              div(class = "oath-subtitle", if (plain) pc$subtitle %||% "" else copy$subtitle),
              if (!plain) div(class = "oath-rule")),
          if (length(tenets)) div(class = "oath-tenets",
              lapply(seq_along(tenets), \(i) oath_tenet(tenets[i], late = i > ceiling(length(tenets) / 2)))),
          div(class = "oath-main",
              div(class = "oath-main-label", if (plain) pc$main_label %||% "Main goal" else copy$main_label %||% "Main quest"),
              div(class = "oath-main-body",
                  div(class = "oath-main-quest", if (plain) pc$main_quest %||% copy$main_quest else copy$main_quest),
                  div(class = "oath-main-sub", if (plain) pc$main_sub %||% copy$main_sub else copy$main_sub),
                  if (nrow(main)) div(class = "oath-main-now",
                      sprintf("%s now: %s. %s", if (plain) main$plain[1] else main$label[1],
                              main$headline[1], main$progress[1])))),
          div(class = "oath-section",
              div(class = "oath-section-head",
                  h2(if (plain) pc$board_title %||% "Goals" else copy$board_title %||% "The Quest Board"),
                  div(class = "oath-note", sprintf("%s \u00B7 as of %s", copy$board_note %||% "now \u2192 target",
                                                   fmt_day(board$as_of[1])))),
              div(class = "oath-board", lapply(seq_len(nrow(side)), \(i) oath_card(side[i, ], session$ns, plain)))),
          if (!is.null(rules)) div(class = "oath-section",
              h2(if (plain) pc$rules_title %||% "Rules" else copy$rules_title %||% "Rules"),
              div(class = "oath-rules", rules)),
          if (!plain) div(class = "oath-footer",
              div(class = "oath-footer-left", copy$sworn),
              div(class = "oath-footer-right", sub("{level}", level, copy$footer %||% "", fixed = TRUE)))
        )
      )
    })

    # Clicked cards, for the app's server: list(tab, inputs)
    reactive(input$goto)
  })
}

# Switch to a quest's tab and set its inputs (ids inside that tab's module).
# Called from the app's server, which owns the navbar.
oath_follow <- function(goto, session, navbar_id = "main") {
  force(goto)   # run the module now, not when the observer first fires
  observeEvent(goto(), {
    g <- goto()
    for (name in names(g$inputs)) {
      session$sendInputMessage(paste0(g$tab, "-", name), list(value = g$inputs[[name]]))
    }
    nav_select(navbar_id, g$tab, session = session)
  })
}
