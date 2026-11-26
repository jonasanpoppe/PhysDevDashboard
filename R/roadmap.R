library(dplyr)

# The Roadmap tab: lanes x 12 months, phases as blocks, a today line and
# gates that tick when their relic unlocks. Config: config/oath.yml,
# `roadmap:` (lanes, phases, gates). Pure functions, no Shiny; the app reads
# the result from the database (roadmap_phases, roadmap_gates).
#
# A phase is {lane, from, to, text, style}: from/to are dates (a month is
# enough: 2027-03 means March); the block spans the months it touches.
# style picks the fill (light, mid, pale, dark, ink, accent, outline, dashed).
# A gate is {label, date, relic}: ticked once that relic is unlocked.

`%||%` <- function(a, b) if (is.null(a)) b else a

# "2027-03" or "2027-03-15" as a Date (a month alone: its first day)
roadmap_date <- function(x) {
  x <- as.character(x)
  as.Date(ifelse(grepl("^\\d{4}-\\d{2}$", x), paste0(x, "-01"), x))
}
roadmap_month_end <- function(d) {
  d <- as.Date(d)
  first_next <- as.Date(format(d + 31 - as.integer(format(d, "%d")) + 1, "%Y-%m-01"))
  first_next - 1
}

# One row per phase: its grid column (2 = January) and span, and whether it
# is done, current or ahead on as_of
roadmap_phases <- function(cfg, as_of = Sys.Date()) {
  rm <- cfg$roadmap %||% list()
  year <- as.integer(cfg$year %||% format(as.Date(as_of), "%Y"))
  lanes <- vapply(rm$lanes %||% list(), \(l) l$id, character(1))
  rows <- lapply(seq_along(rm$phases %||% list()), function(i) {
    p <- rm$phases[[i]]
    from <- roadmap_date(p$from)
    to_raw <- as.character(p$to %||% p$from)
    to <- if (grepl("^\\d{4}-\\d{2}$", to_raw)) roadmap_month_end(roadmap_date(to_raw)) else roadmap_date(to_raw)
    if (!p$lane %in% lanes) stop("Roadmap phase '", p$text, "': no lane '", p$lane, "'")
    m_from <- max(1L, as.integer(format(from, "%m")) + 12L * (as.integer(format(from, "%Y")) - year))
    m_to <- min(12L, as.integer(format(to, "%m")) + 12L * (as.integer(format(to, "%Y")) - year))
    tibble::tibble(
      ord = i, lane = p$lane, lane_ord = match(p$lane, lanes), text = p$text, style = p$style %||% "light",
      from = from, to = to, col = m_from + 1L, span = max(1L, m_to - m_from + 1L),
      state = if (as.Date(as_of) > to) "done" else if (as.Date(as_of) >= from) "current" else "ahead",
      as_of = as.Date(as_of)
    )
  })
  dplyr::bind_rows(rows)
}

# Where today sits across the 12 months: 0 on 1 January, just under 1 on 31 December;
# NA outside the year
roadmap_today <- function(cfg, as_of = Sys.Date()) {
  year <- as.integer(cfg$year %||% format(as.Date(as_of), "%Y"))
  first <- as.Date(sprintf("%d-01-01", year))
  days <- as.numeric(as.Date(sprintf("%d-12-31", year)) - first) + 1
  f <- as.numeric(as.Date(as_of) - first) / days
  if (f < 0 || f >= 1) NA_real_ else f
}

# One row per gate, ticked when its relic is unlocked (relic_status)
roadmap_gates <- function(cfg, relics = NULL) {
  g <- cfg$roadmap$gates %||% list()
  if (!length(g)) {
    return(tibble::tibble(ord = integer(), label = character(), date = as.Date(character()),
                          relic = character(), ticked = logical(), ticked_on = as.Date(character())))
  }
  dplyr::bind_rows(lapply(seq_along(g), function(i) {
    r <- g[[i]]$relic %||% NA_character_
    hit <- if (!is.null(relics) && !is.na(r)) relics[relics$id == r, ] else NULL
    tibble::tibble(ord = i, label = g[[i]]$label, date = roadmap_date(g[[i]]$date %||% NA),
                   relic = r, ticked = !is.null(hit) && nrow(hit) == 1 && isTRUE(hit$unlocked),
                   ticked_on = if (!is.null(hit) && nrow(hit) == 1) as.Date(hit$date) else as.Date(NA))
  }))
}

write_roadmap_tables <- function(phases, gates, db_path = "db/health.db") {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "roadmap_phases", phases, overwrite = TRUE)
  DBI::dbWriteTable(con, "roadmap_gates", gates, overwrite = TRUE)
  message("✓ roadmap tables written (", nrow(phases), " phases, ", sum(gates$ticked), " of ",
          nrow(gates), " gates ticked)")
  invisible(TRUE)
}
