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
      # ?v=: the file's time, so a browser fetches a changed stylesheet
      # instead of keeping its cached copy
      tags$link(rel = "stylesheet", href = sprintf("oath.css?v=%s", format(file.mtime("www/oath.css"), "%Y%m%d%H%M%S")))
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

# The picture on a card: the chart of its number (quest_history) with the
# target, or one pip per stage. `w` is the chart window (quest_window()).
oath_card_picture <- function(q, hist, w, year) {
  if (q$kind %in% c("event", "steps")) return(quest_pips(q$now_value, q$n_stages))
  h <- hist[hist$id == q$id, c("date", "value")]
  unit <- quest_metric_units[q$metric] %||% ""
  digits <- quest_metric_digits[q$metric] %||% 0
  key <- if (is.na(q$target_value)) "" else if (identical(q$kind, "cumulative"))
    sprintf("- - even pace to %s %s", fmt_goal(q$target_value), unit) else
      sprintf("- - target %s %s", fmt_goal(q$target_value, digits), unit)
  spark <- quest_spark(h, q$kind, q$target_value, w$from, w$to, q$as_of, met_at = q$met_at, year = year)
  if (is.null(spark)) return(NULL)
  div(class = "oath-card-picture", spark, quest_spark_axis(w$from, w$to, key))
}

# A card; with a link, clicking it opens the page with its data. The
# headline and the picture up front; the progress line, notes and source
# folded under Details.
oath_card <- function(q, ns, plain = FALSE, hist = NULL, w = NULL, year = NA) {
  accents <- physdev_palettes$oath$quest_accents   # corporate mode recolours them in oath.css
  accent <- accents[min(max(q$accent, 1L), length(accents))]
  linked <- !is.na(q$link_tab)
  click <- if (linked) goto_js(ns, q$link_tab, q$link_inputs)
  notes <- Filter(nzchar, c(progress = q$progress, extra = q$extras, source = q$source_note))
  picture <- if (!is.null(hist) && !is.null(w)) oath_card_picture(q, hist, w, year)
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
    # Without a picture, the share of the way as a bar (as before)
    if (is.null(picture) && !is.na(q$share)) div(class = "oath-bar", div(style = sprintf("width: %.0f%%;", 100 * q$share))),
    picture,
    if (length(notes)) tags$details(class = "oath-card-more", onclick = "event.stopPropagation();",
        tags$summary("Details"),
        lapply(names(notes), \(k) div(class = paste0("oath-card-", k), notes[[k]])))
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
    # The cards' charts (no table in an older database: cards without charts)
    hist <- if (db_has_table("quest_history")) query("SELECT * FROM quest_history") else NULL
    # The verdict per quest with the default assumptions (Scenarios tab)
    if (db_has_table("quest_scenarios")) {
      board <- board %>% left_join(query("SELECT id, verdict, summary AS scenario FROM quest_scenarios"), by = "id")
    }
    cfg <- jsonlite::fromJSON(query("SELECT json FROM oath_config")$json, simplifyVector = FALSE)
    copy <- cfg$copy
    phases <- if (db_has_table("roadmap_phases")) query("SELECT * FROM roadmap_phases ORDER BY ord") else NULL
    gates <- if (db_has_table("roadmap_gates")) query("SELECT * FROM roadmap_gates ORDER BY ord") else NULL
    now <- roadmap_now(phases, gates)
    win <- quest_window(cfg, board$as_of[1])
    year <- as.integer(cfg$year %||% format(as.Date(board$as_of[1]), "%Y"))

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
      # Unlocked relics without card art yet: a note to make the card (only
      # where there is an art folder: the demo has none, and no note)
      no_art <- if (is.null(relics) || !dir.exists(relic_art_dir)) character(0) else
        relics$id[relics$unlocked & !vapply(relics$id, \(i) !is.null(relic_art(i)), logical(1))]
      tenets <- if (plain) character(0) else unlist(copy$tenets)
      rules <- if (plain) pc[["rules"]] %||% copy$rules else copy$rules   # [[ ]]: $ would match rules_title
      div(
        class = paste(c("oath", if (plain) "oath-corporate"), collapse = " "),
        div(
          class = "oath-frame",
          # Two columns on desktop: the poster, and the board with the rules
          # (on narrower screens the wrappers dissolve and oath.css orders
          # the blocks: head, Today, main quest, board, relics, rules, footer)
          div(class = "oath-poster",
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
                        if (!is.null(main$verdict) && !is.na(main$verdict[1])) verdict_chip(main$verdict[1])),
                    if (nrow(main) && !is.null(hist)) div(class = "oath-main-picture", oath_card_picture(main[1, ], hist, win, year)))),
            if (!is.null(relics) && nrow(relics)) oath_shelf(relics, session$ns, plain),
            if (!plain) div(class = "oath-footer",
                div(class = "oath-footer-left", copy$sworn),
                div(class = "oath-footer-right", sub("{level}", level, copy$footer %||% "", fixed = TRUE)))
          ),
          div(class = "oath-board-col",
            div(class = "oath-section oath-board-section",
                div(class = "oath-section-head",
                    h2(if (plain) pc$board_title %||% "Goals" else copy$board_title %||% "The Quest Board"),
                    div(class = "oath-note", sprintf("%s \u00B7 as of %s", copy$board_note %||% "now \u2192 target",
                                                     fmt_day(board$as_of[1])))),
                div(class = "oath-board", lapply(seq_len(nrow(side)), \(i) oath_card(side[i, ], session$ns, plain, hist, win, year))),
                div(class = "oath-strip-foot", span(),
                    oath_more(session$ns, "scenarios", "What if? \u2192"))),
            if (!is.null(rules)) div(class = "oath-section oath-rules-section",
                h2(if (plain) pc$rules_title %||% "Rules" else copy$rules_title %||% "Rules"),
                div(class = "oath-rules", rules))
          )
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
