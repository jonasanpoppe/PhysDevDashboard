# Fitness: watts at the same heart rate per season ("fitness"), or training
# load ("load"). Model: R/fitness_model.R; charts: R/plot_fitness.R.

fitness_ui <- function(id, which = c("fitness", "load")) {
  which <- match.arg(which)
  ns <- NS(id)
  card(
    full_screen = TRUE,
    if (which == "fitness") uiOutput(ns("headline")),
    fill_plot(ns("plot"), min_height = 300),
    card_footer(class = "small text-muted", uiOutput(ns("footer")))
  )
}

fitness_server <- function(id, which = c("fitness", "load")) {
  which <- match.arg(which)
  moduleServer(id, function(input, output, session) {
    seasons <- query("SELECT * FROM fitness_seasons ORDER BY season")
    coefs   <- query("SELECT * FROM fitness_coefficients")
    check   <- query("SELECT * FROM fitness_load_check")
    load    <- query("SELECT * FROM strava_daily_load")
    xlim <- fitness_xlim(seasons, load)
    ref <- seasons[1, c("ref_hr_bpm", "ref_duration_s", "ref_hours_into_ride")]

    output$headline <- renderUI({
      if (nrow(seasons) < 2) return(NULL)
      a <- seasons[nrow(seasons) - 1, ]; b <- seasons[nrow(seasons), ]
      term <- coefs[coefs$term == paste0("season", b$season), ]
      p(class = "px-3 pt-3 mb-0",
        sprintf("At %s bpm, %s-minute climb, %s h into a ride: %.0f W in %d against %.0f W in %d (%+.0f W, 95%% interval %+.0f to %+.0f).",
                ref$ref_hr_bpm, ref$ref_duration_s / 60, ref$ref_hours_into_ride,
                b$watts_ref, b$season, a$watts_ref, a$season,
                term$estimate, term$ci_low, term$ci_high),
        if (nrow(term) && term$ci_low < 0 && term$ci_high > 0) span(class = "text-muted", "The interval includes zero: a plausible change, not a proven one."))
    })

    output$plot <- renderPlot({
      ui_mode()   # redraw when corporate mode flips (colours come from physdev)
      if (which == "fitness") plot_fitness_seasons(seasons, xlim, ref$ref_hr_bpm) else plot_fitness_load(load, xlim)
    }, res = 96)

    output$footer <- renderUI({
      if (which == "fitness") {
        hr <- coefs[coefs$term == "hr_end", ]
        tagList(
          sprintf("Model: estimated power ~ heart rate at the end of the climb + log(duration) + hours into the ride + season, on %d clean chest-strap ascents of 90 s or more, one bike (%s).",
                  sum(seasons$n), paste(sprintf("%d: %d", seasons$season, seasons$n), collapse = ", ")),
          sprintf("Each extra bpm at the top ≈ %+.1f W. Bands are bootstrap intervals (how well the model is pinned down); the watts themselves are estimates (±15%%).", hr$estimate))
      } else {
        sprintf("Load check: %+.0f W per extra weekly hour of riding and running in the 6 weeks before (95%% interval %+.0f to %+.0f, %d ascents). %s",
                check$w_per_weekly_hour, check$ci_low, check$ci_high, check$n,
                if (check$ci_low > 0) "More training went with more watts."
                else if (check$ci_high < 0) "More training went with fewer watts."
                else "No clear effect in these ascents.")
      }
    })
  })
}
