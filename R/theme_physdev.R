# ============================================================
#  theme_physdev.R
#  The PhysDevDashboard ggplot2 + bslib theme: the Oath of Glory
#  scheme (parchment, ink, purple and bronze) on every tab.
#
#  Usage:
#    source("R/theme_physdev.R")
#    ggplot(...) + theme_physdev()
# ============================================================

# ── Palette ──────────────────────────────────────────────────────────────────

physdev <- list(

  # Core: the Oath of Glory scheme (parchment, ink), on every tab
  bg          = "#F4EBD6",   # Parchment page background
  bg_card     = "#EFE3C8",   # Card surface
  fg          = "#2A1E14",   # Ink: body text
  fg_muted    = "#5C4632",   # Secondary text, axis labels
  fg_subtle   = "#C9B48C",   # Gridlines, borders (never text)

  # UI colours (navbar, buttons, links). Gold only for borders: it fails
  # contrast as text.
  purple      = "#4B2A6B",
  bronze      = "#7A4A22",
  gold        = "#9A7020",

  # Accent: the highlighted series (current round, the main line)
  accent       = "#7046A8",
  accent_light = "#A98BD0",
  accent_dark  = "#4B2A6B",

  # Categorical, fixed order, validated on the parchment surface (lightness,
  # chroma, colour-blind separation of neighbours, contrast >= 3:1). Bronze
  # and ochre look alike to deuteranopes: never make them neighbours.
  cat = c(
    "#7046A8",   # Purple — series 1
    "#C0621E",   # Bronze — series 2
    "#1F7FB8",   # Blue   — series 3
    "#5C8F2A",   # Olive  — series 4
    "#B23F78",   # Plum   — series 5
    "#9A7A12"    # Ochre  — series 6
  ),

  # Sequential (light → dark purple) and diverging poles (bronze ↔ blue)
  seq_low  = "#ECE3F3",
  seq_high = "#3E2259",
  div_low  = "#A4521A",
  div_high = "#1F7FB8"
)

# ── ggplot2 theme ─────────────────────────────────────────────────────────────

# The web font, registered once per R session. font_add_google() downloads
# the font each time it is called (seconds per figure), so the result is
# kept; with no showtext or no network it falls back to "sans".
physdev_font <- local({
  family <- NULL
  function() {
    if (is.null(family)) {
      family <<- tryCatch({
        if (requireNamespace("showtext", quietly = TRUE) &&
            requireNamespace("sysfonts", quietly = TRUE)) {
          if (!"dm_sans" %in% sysfonts::font_families()) sysfonts::font_add_google("DM Sans", "dm_sans")
          showtext::showtext_auto()
          "dm_sans"
        } else "sans"
      }, error = function(e) "sans")
    }
    family
  }
})

theme_physdev <- function(base_size = 12, grid = "y") {

  base_family <- physdev_font()

  half_line <- base_size / 2

  t <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # ── Plot area
      plot.background  = ggplot2::element_rect(fill = physdev$bg,      colour = NA),
      panel.background = ggplot2::element_rect(fill = physdev$bg,      colour = NA),
      panel.border     = ggplot2::element_blank(),

      # ── Grid
      panel.grid.major.x = if (grid %in% c("x", "both"))
        ggplot2::element_line(colour = physdev$fg_subtle, linewidth = 0.35)
      else ggplot2::element_blank(),
      panel.grid.major.y = if (grid %in% c("y", "both"))
        ggplot2::element_line(colour = physdev$fg_subtle, linewidth = 0.35)
      else ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),

      # ── Axes
      # Both axes drawn, with short ticks, so each panel reads as a frame
      axis.line        = ggplot2::element_line(colour = physdev$fg_muted, linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(colour = physdev$fg_muted, linewidth = 0.4),
      axis.ticks.length = grid::unit(3, "pt"),
      axis.text        = ggplot2::element_text(colour = physdev$fg_muted, size = ggplot2::rel(0.85)),
      axis.title       = ggplot2::element_text(colour = physdev$fg,       size = ggplot2::rel(0.9),
                                               margin = ggplot2::margin(t = half_line)),
      axis.title.x     = ggplot2::element_text(margin = ggplot2::margin(t = half_line)),
      axis.title.y     = ggplot2::element_text(margin = ggplot2::margin(r = half_line), angle = 90),

      # ── Titles
      plot.title       = ggplot2::element_text(colour = physdev$fg,      size = ggplot2::rel(1.3),
                                               face = "bold",
                                               margin = ggplot2::margin(b = half_line * 0.75)),
      plot.subtitle    = ggplot2::element_text(colour = physdev$fg_muted, size = ggplot2::rel(0.95),
                                               margin = ggplot2::margin(b = half_line * 1.5)),
      plot.caption     = ggplot2::element_text(colour = physdev$fg_subtle, size = ggplot2::rel(0.75),
                                               hjust = 0,
                                               margin = ggplot2::margin(t = half_line)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      # ── Legend
      legend.background = ggplot2::element_rect(fill = physdev$bg, colour = NA),
      legend.key        = ggplot2::element_rect(fill = physdev$bg, colour = NA),
      legend.title      = ggplot2::element_text(colour = physdev$fg,      size = ggplot2::rel(0.85), face = "bold"),
      legend.text       = ggplot2::element_text(colour = physdev$fg_muted, size = ggplot2::rel(0.82)),
      legend.position   = "top",
      legend.justification = "left",
      legend.key.size   = ggplot2::unit(0.9, "lines"),

      # ── Facets
      strip.background = ggplot2::element_rect(fill = physdev$bg_card, colour = NA),
      strip.text       = ggplot2::element_text(colour = physdev$fg, size = ggplot2::rel(0.88), face = "bold"),

      # ── Spacing
      plot.margin = ggplot2::margin(half_line * 2, half_line * 2, half_line * 2, half_line * 2)
    )

  t
}

# ── ggplot2 colour/fill scales ────────────────────────────────────────────────

#' Categorical colour scale (up to 6 series)
scale_colour_physdev <- function(...) {
  ggplot2::scale_colour_manual(values = physdev$cat, ...)
}
scale_color_physdev <- scale_colour_physdev   # alias

#' Categorical fill scale
scale_fill_physdev <- function(...) {
  ggplot2::scale_fill_manual(values = physdev$cat, ...)
}

#' Sequential fill scale (single variable, e.g. heatmap)
scale_fill_physdev_seq <- function(...) {
  ggplot2::scale_fill_gradient(
    low  = physdev$seq_low,
    high = physdev$seq_high,
    ...
  )
}

#' Diverging fill scale (e.g. correlation matrix)
scale_fill_physdev_div <- function(midpoint = 0, ...) {
  ggplot2::scale_fill_gradient2(
    low      = physdev$div_low,
    mid      = physdev$bg,
    high     = physdev$div_high,
    midpoint = midpoint,
    ...
  )
}

# ── bslib Shiny theme ─────────────────────────────────────────────────────────

# Uncomment when using in a Shiny app:
#
# library(bslib)
#
# shiny_theme_physdev <- bs_theme(
#   bg         = physdev$bg,
#   fg         = physdev$fg,
#   primary    = physdev$accent,
#   secondary  = physdev$fg_muted,
#   success    = "#7A9E7E",
#   warning    = "#D97706",
#   danger     = "#C0392B",
#   base_font  = font_google("DM Sans"),
#   heading_font = font_google("DM Serif Display"),
#   `border-radius` = "0.5rem",
#   `box-shadow`    = "0 1px 3px rgba(0,0,0,0.06)"
# )

# ── Quick preview ─────────────────────────────────────────────────────────────

# Run this to preview the theme:
#
# library(ggplot2)
# source("R/theme_physdev.R")
#
# ggplot(mpg, aes(displ, hwy, colour = class)) +
#   geom_point(size = 2.5, alpha = 0.85) +
#   scale_colour_physdev() +
#   theme_physdev() +
#   labs(
#     title    = "Engine displacement vs fuel efficiency",
#     subtitle = "Each point is a car model; colour encodes vehicle class",
#     x        = "Displacement (L)",
#     y        = "Highway MPG",
#     colour   = NULL,
#     caption  = "Source: EPA fuel economy data"
#   )

