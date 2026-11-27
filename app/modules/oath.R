# Home (the Oath tab's first page): the hub. The oath poster (tenets folded),
# a Today strip (level, the roadmap's current phase and next gate), the live
# Quest Board, the relic shelf; each strip links to its full page. Copy and
# quests come from config/oath.yml, through the database (oath_config,
# quest_board, relic_status, roadmap_*; R/quests.R, R/relics.R and
# R/roadmap.R build them), shown through the same functions as the full
# pages. Style: app/www/oath.css.


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

# Sends {tab, inputs} to input$goto; the app's server opens that page (tab
# is a page name, app_pages in app/app.R) and sets the inputs there
goto_js <- function(ns, tab, inputs = "{}") {
  sprintf("Shiny.setInputValue('%s', {tab: '%s', inputs: %s}, {priority: 'event'})", ns("goto"), tab, inputs)
}

# A link from a hub strip to its full page
oath_more <- function(ns, tab, label) {
  tags$a(class = "oath-more", href = "#", onclick = paste0(goto_js(ns, tab), "; return false;"), label)
}

# A card; with a link, clicking it opens the page with its data
oath_card <- function(q, ns, plain = FALSE) {
  accents <- physdev_palettes$oath$quest_accents   # corporate mode recolours them in oath.css
  accent <- accents[min(max(q$accent, 1L), length(accents))]
  linked <- !is.na(q$link_tab)
  click <- if (linked) goto_js(ns, q$link_tab, q$link_inputs)
  tags$div(
    class = paste(c("oath-card", if (linked) "oath-link"), collapse = " "), style = sprintf("--accent: %s;", accent),
    role = if (linked) "link", tabindex = if (linked) "0", onclick = click,
    onkeydown = if (linked) sprintf("if (event.key === 'Enter') { %s }", click),
    title = if (linked) "Open the data behind this quest",
    div(class = "oath-card-top",
        div(class = "oath-card-label", if (plain) q$plain else q$label),
        if (!is.null(q$verdict) && !is.na(q$verdict)) verdict_chip(q$verdict)),
    div(class = "oath-card-headline", if (plain) q$plain_headline else q$headline),
    div(class = "oath-card-by", q$by),
    if (!is.na(q$share)) div(class = "oath-bar", div(style = sprintf("width: %.0f%%;", 100 * q$share))),
    if (nzchar(q$progress)) div(class = "oath-card-progress", q$progress),
    if (nzchar(q$extras)) div(class = "oath-card-extra", q$extras),
    if (nzchar(q$source_note)) div(class = "oath-card-source", q$source_note)
  )
}

# Today: the level, the roadmap's current phase(s) and the next gate
oath_today <- function(level, now, cfg, as_of, ns, plain) {
  year <- as.integer(cfg$year %||% format(as_of, "%Y"))
  phase <- if (length(now$current)) paste(now$current, collapse = " \u00B7 ") else
    if (as_of < as.Date(sprintf("%d-01-01", year))) sprintf("the year starts on %s", fmt_day(as.Date(sprintf("%d-01-01", year)))) else
      "no phase"
  gate <- if (!is.na(now$gate)) sprintf("%s%s", now$gate,
                                        if (!is.na(now$gate_date)) format(now$gate_date, " (%b)") else "")
  div(class = "oath-strip oath-today",
      div(class = "oath-strip-label", "Today"),
      div(class = "oath-strip-body",
          if (!plain) span(class = "oath-today-level", sprintf("Level %d", level)),
          span(tags$b(if (plain) "Now: " else "Phase: "), phase),
          if (!is.null(gate)) span(tags$b(if (plain) "Next checkpoint: " else "Next gate: "), gate)),
      oath_more(ns, "roadmap", if (plain) "Timeline \u2192" else "Roadmap \u2192"))
}

# The relic shelf: a small tile per relic (its art once unlocked), the hint
# on hover; the shelf opens the Reliquary
oath_shelf <- function(relics, ns, plain) {
  div(class = "oath-strip oath-shelf-strip",
      div(class = "oath-strip-label", if (plain) "Milestones" else "Relics"),
      div(class = "oath-shelf",
          lapply(seq_len(nrow(relics)), function(i) {
            r <- relics[i, ]
            art <- if (r$unlocked) relic_art(r$id)
            div(class = paste("shelf-relic", if (r$unlocked) "relic-unlocked" else "relic-sealed"),
                style = if (!is.null(art)) sprintf("background-image: url('%s');", art) else
                  sprintf("background: linear-gradient(160deg, %s, %s);", r$colour, physdev_palettes$oath$fg),
                title = sprintf("%s: %s", if (plain) r$plain else r$name_en, r$hint),
                if (is.null(art)) span(r$id))
          })),
      div(class = "oath-strip-foot",
          span(class = "oath-note", sprintf("%d of %d unlocked", sum(relics$unlocked), nrow(relics))),
          oath_more(ns, "reliquary", if (plain) "Milestones \u2192" else "Reliquary \u2192")))
}

oath_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    board <- query("SELECT * FROM quest_board ORDER BY ord")
    # The verdict per quest with the default assumptions (Scenarios tab)
    if (db_has_table("quest_scenarios")) {
      board <- board %>% left_join(query("SELECT id, verdict, summary AS scenario FROM quest_scenarios"), by = "id")
    }
    cfg <- jsonlite::fromJSON(query("SELECT json FROM oath_config")$json, simplifyVector = FALSE)
    copy <- cfg$copy
    phases <- if (db_has_table("roadmap_phases")) query("SELECT * FROM roadmap_phases ORDER BY ord") else NULL
    gates <- if (db_has_table("roadmap_gates")) query("SELECT * FROM roadmap_gates ORDER BY ord") else NULL
    now <- roadmap_now(phases, gates)

    output$page <- renderUI({
      # Corporate mode: the same numbers in plain words, without the poster's
      # flavour (kicker, tenets, level footer); plain copy from copy$plain
      plain <- identical(ui_mode(), "corporate")
      pc <- copy$plain %||% list()
      main <- board %>% filter(role == "main")
      side <- board %>% filter(role != "main")
      # Level: the base plus levels earned in the Reliquary
      relics <- relic_table()
      level <- if (is.null(relics)) cfg$level_base %||% 1 else relic_level(relics, cfg)
      # Unlocked relics without card art yet: a note to make the card
      no_art <- if (is.null(relics)) character(0) else
        relics$id[relics$unlocked & !vapply(relics$id, \(i) !is.null(relic_art(i)), logical(1))]
      tenets <- if (plain) character(0) else unlist(copy$tenets)
      rules <- if (plain) pc[["rules"]] %||% copy$rules else copy$rules   # [[ ]]: $ would match rules_title
      div(
        class = paste(c("oath", if (plain) "oath-corporate"), collapse = " "),
        div(
          class = "oath-frame",
          div(class = "oath-head",
              if (!plain) div(class = "oath-kicker", copy$kicker),
              h1(if (plain) pc$title %||% "Goals" else copy$title),
              div(class = "oath-subtitle", if (plain) pc$subtitle %||% "" else copy$subtitle),
              if (!plain) div(class = "oath-rule")),
          if (length(no_art)) div(class = "oath-relic-note", role = "status",
              sprintf("New relic unlocked: %s. Generate its card (config/relics/%s.jpg).",
                      paste(no_art, collapse = ", "), no_art[1])),
          # The tenets, folded: the hub opens on what's next
          if (length(tenets)) tags$details(class = "oath-tenets-fold",
              tags$summary(copy$tenets_title %||% "The tenets"),
              div(class = "oath-tenets",
                  lapply(seq_along(tenets), \(i) oath_tenet(tenets[i], late = i > ceiling(length(tenets) / 2))))),
          oath_today(level, now, cfg, as.Date(board$as_of[1]), session$ns, plain),
          div(class = "oath-main",
              div(class = "oath-main-label", if (plain) pc$main_label %||% "Main goal" else copy$main_label %||% "Main quest"),
              div(class = "oath-main-body",
                  div(class = "oath-main-quest", if (plain) pc$main_quest %||% copy$main_quest else copy$main_quest),
                  div(class = "oath-main-sub", if (plain) pc$main_sub %||% copy$main_sub else copy$main_sub),
                  if (nrow(main)) div(class = "oath-main-now",
                      sprintf("%s now: %s. %s", if (plain) main$plain[1] else main$label[1],
                              main$headline[1], main$progress[1]),
                      if (!is.null(main$verdict) && !is.na(main$verdict[1])) verdict_chip(main$verdict[1])))),
          div(class = "oath-section",
              div(class = "oath-section-head",
                  h2(if (plain) pc$board_title %||% "Goals" else copy$board_title %||% "The Quest Board"),
                  div(class = "oath-note", sprintf("%s \u00B7 as of %s", copy$board_note %||% "now \u2192 target",
                                                   fmt_day(board$as_of[1])))),
              div(class = "oath-board", lapply(seq_len(nrow(side)), \(i) oath_card(side[i, ], session$ns, plain))),
              div(class = "oath-strip-foot", span(),
                  oath_more(session$ns, "scenarios", "What if? \u2192"))),
          if (!is.null(relics) && nrow(relics)) oath_shelf(relics, session$ns, plain),
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

# Open a page and set its inputs (ids inside that page's module). g$tab is a
# page; pages maps it to its navbar tab, and a tab with more than one page
# holds them as pills in the navset <tab>_page. Called from the app's server, which owns the navbar.
oath_follow <- function(goto, session, pages, navbar_id = "main") {
  force(goto)   # run the module now, not when the observer first fires
  observeEvent(goto(), {
    g <- goto()
    tab <- pages[[g$tab]]
    if (is.null(tab)) {
      warning("Quest link to an unknown page '", g$tab, "' (app_pages in app/app.R)")
      return()
    }
    for (name in names(g$inputs)) {
      session$sendInputMessage(paste0(g$tab, "-", name), list(value = g$inputs[[name]]))
    }
    nav_select(navbar_id, tab, session = session)
    if (sum(unlist(pages) == tab) > 1) nav_select(paste0(tab, "_page"), g$tab, session = session)
  })
}
