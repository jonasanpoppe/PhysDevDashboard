# Strength module: one figure at a time, chosen from a dropdown. Data:
# R/strength.R (strength_sessions, strength_targets, strength_ceilings);
# charts: R/plot_strength.R.

strength_figures <- c("e1RM by lift" = "lifts",
                      "Main lifts against targets and ceilings" = "targets",
                      "Strength standards (× bodyweight)" = "standards")

strength_heights <- c(lifts = 380, targets = 340, standards = 340)

strength_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Strength",
      selectInput(ns("figure"), "Figure", choices = strength_figures),
      conditionalPanel("input.figure == 'lifts'", ns = ns, uiOutput(ns("group_select")))
    ),
    card(
      full_screen = TRUE,
      card_header(textOutput(ns("summary"), inline = TRUE) %>% tagAppendAttributes(class = "small text-muted")),
      plotOutput(ns("plot"), height = "auto"),
      card_footer(class = "small text-muted", uiOutput(ns("footer")))
    )
  )
}

strength_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    figure <- reactive(input$figure %||% "lifts")
    sessions <- query("SELECT * FROM strength_sessions")
    targets  <- query("SELECT * FROM strength_targets")
    ceilings <- query("SELECT * FROM strength_ceilings")
    now <- strength_now(sessions)
    formula <- unique(sessions$formula)[1]
    groups <- unique(c(targets$group, sort(unique(sessions$group))))

    output$group_select <- renderUI(selectInput(ns("group"), "Lift group", choices = groups))

    output$summary <- renderText({
      sprintf("%d session bests · %s formula · last session %s",
              nrow(sessions), tools::toTitleCase(formula), format(max(as.Date(sessions$date)), "%d %b %Y"))
    })

    output$plot <- renderPlot({
      switch(figure(),
        lifts = {
          g <- input$group %||% groups[1]
          plot_strength_lifts(sessions, g, targets$target_kg[match(g, targets$group)])
        },
        targets = plot_strength_targets(now, targets, ceilings),
        standards = plot_strength_standards(now, strength_standards))
    }, height = function() strength_heights[[figure()]], res = 96)

    output$footer <- renderUI({
      common <- sprintf("Estimated one-rep max from the best working set of each session (%s formula, RIR 3 or less, bar weight incl. machine base weight): an estimate, not a tested max.",
                        tools::toTitleCase(formula))
      switch(figure(),
        lifts = tagList(common, "The thick line is the group's main lift; the dashed line its target. Lines break where sessions are more than 6 weeks apart."),
        targets = tagList(common, sprintf("Dot: the main lift's best of the last 12 weeks (or its last session, dated). Red tick: the target for %s. Bands: the 3–5 year ceilings.",
                                          format(as.Date(targets$target_date[1]), "%Y"))),
        standards = tagList(common, "Divided by the MacroFactor trend weight on the day. The standards are for the back squat, bench press and deadlift: a front squat reads lower than a back squat.",
                            "Sources: Barbell Medicine strength standards; Stronger by Science, “What is strong?”; normative powerlifting data (PubMed 39060209)."))
    })
  })
}
