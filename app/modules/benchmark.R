# Benchmark: every attempt on the benchmark climb (riding.benchmark_climb in
# goals.yml): estimated W/kg with its range, time and heart rate at the top.
# Chart: plot_benchmark() in R/plot_riding.R.

benchmark_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(textOutput(ns("title"), inline = TRUE)),
    plotOutput(ns("plot"), height = "340px"),
    div(class = "table-responsive", uiOutput(ns("table"))),
    card_footer(class = "small text-muted",
                "Filled: clean ascents; hollow: stopped on the way. Watts are estimates (±15%), wind ignored: compare attempts ridden fresh and steady. Heart rate: the last 30 s of the climb.")
  )
}

benchmark_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    climb <- query("SELECT benchmark_climb FROM riding_benchmark")$benchmark_climb[1]
    attempts <- query("SELECT * FROM strava_climb_efforts WHERE climb = ? ORDER BY date",
                      params = list(climb))
    output$title <- renderText(sprintf("Benchmark: %s · %d attempts", climb, nrow(attempts)))
    output$plot <- renderPlot({
      validate(need(nrow(attempts) > 0, "No attempts on the benchmark climb yet."))
      plot_benchmark(attempts)
    }, res = 96)
    output$table <- renderUI({
      if (nrow(attempts) == 0) return(NULL)
      a <- attempts %>% arrange(desc(date))
      tags$table(class = "table table-sm mb-0",
        tags$thead(tags$tr(tags$th("Date"), tags$th(class = "text-end", "Moving"), tags$th(class = "text-end", "Stopped"),
                           tags$th(class = "text-end", "Est. W/kg"), tags$th(class = "text-end", "HR at the top"),
                           tags$th("Status"))),
        tags$tbody(lapply(seq_len(nrow(a)), \(k) tags$tr(
          tags$td(format(as.Date(a$date[k]), "%d %b %Y")), tags$td(class = "text-end", fmt_mmss(a$moving_s[k])),
          tags$td(class = "text-end", sprintf("%.0f s", a$stop_s[k])),
          tags$td(class = "text-end", sprintf("%.1f", a$wkg[k])),
          tags$td(class = "text-end", sprintf("%.0f", a$hr_end[k])), tags$td(a$status[k])))))
    })
  })
}
