library(dplyr)

# Estimated power per climb effort, from the physics of the climb. No power
# meter: these are model outputs for comparing efforts with each other, shown
# with their uncertainty (± `uncertainty`), never as measurements.
#
#   E = m g H  +  m g Crr L  +  ½ ρ CdA v² L      v = L / moving time
#   P = E / moving time / drivetrain efficiency
#
# - m: rider (MacroFactor trend weight on the day, or the last one before)
#   + bike (bikes.csv, on the day) + kit.
# - H, L: the climb's rise and length, the same for every effort on it, so
#   efforts compare like for like. Rise = the segment's highest minus lowest
#   point (rise_m, see climb_rise()), not its cumulative gain: the small ups
#   and downs of a rolling climb don't cost the energy their sum suggests.
# - Crr by surface; a climb without a surface gets crr_unknown_surface and
#   crr_source says "assumed".
# - Wind is ignored; under min_duration_s the run-up carries the rider and
#   the estimate reads high: short = TRUE.

estimate_power <- function(efforts, climbs, weights, settings) {
  s <- power_settings(settings)
  crr_of <- function(surface) {
    known <- !is.na(surface) & surface %in% names(s$crr)
    list(
      crr = ifelse(known, unlist(s$crr)[surface], s$crr[[s$crr_unknown_surface]]),
      source = ifelse(known, "surface", paste0("assumed (", s$crr_unknown_surface, ")"))
    )
  }

  w <- weights %>%
    transmute(w_date = as.Date(date), rider_kg = trend_kg) %>%
    filter(!is.na(rider_kg))

  e <- efforts %>%
    mutate(date = as.Date(date)) %>%
    left_join(climbs %>% transmute(name, H = coalesce(rise_m, gain_m), L = length_m, surface),
              by = c(climb = "name")) %>%
    left_join(w, by = join_by(closest(date >= w_date))) %>%
    select(-w_date)

  cr <- crr_of(e$surface)
  e %>%
    mutate(
      crr        = cr$crr,
      crr_source = cr$source,
      system_kg  = rider_kg + bike_mass_kg + s$kit_mass_kg,
      v_ms       = if_else(moving_s > 0, L / moving_s, NA_real_),
      energy_j   = system_kg * s$g_ms2 * H +
                   system_kg * s$g_ms2 * crr * L +
                   0.5 * s$rho_kg_m3 * s$cda_m2 * v_ms^2 * L,
      power_w    = energy_j / moving_s / s$drivetrain_eff,
      power_low  = power_w * (1 - s$uncertainty),
      power_high = power_w * (1 + s$uncertainty),
      wkg        = power_w / rider_kg,
      short      = moving_s < s$min_duration_s
    ) %>%
    select(-H, -L, -surface, -v_ms, -energy_j)
}

# The power block of config/athlete.yml, with the spec's values as defaults.
power_settings <- function(cfg = list()) {
  defaults <- list(
    kit_mass_kg = 2,
    crr = list(cobbles = 0.018, part_cobbles = 0.012, asphalt = 0.006),
    crr_unknown_surface = "asphalt",
    cda_m2 = 0.45, rho_kg_m3 = 1.2, drivetrain_eff = 0.97, g_ms2 = 9.81,
    uncertainty = 0.15, min_duration_s = 90
  )
  s <- utils::modifyList(defaults, cfg %||% list())
  if (!s$crr_unknown_surface %in% names(s$crr)) {
    stop("power: crr_unknown_surface must be one of ", paste(names(s$crr), collapse = ", "))
  }
  s
}

# MacroFactor's trend weight per day (a model output, see docs/sources.md).
rider_weight_table <- function(macrofactor_daily, db_path = "db/health.db") {
  out <- macrofactor_daily %>%
    transmute(date = as.Date(date), trend_kg = trend_weight_kg) %>%
    filter(!is.na(trend_kg)) %>%
    arrange(date)
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "rider_weight", out, overwrite = TRUE)
  invisible(out)
}

# Adds rise_m (highest minus lowest point of the climb's Strava segment) to
# the climb list. Climbs whose segment isn't in strava_segments keep NA, and
# the power model falls back to their gain.
climb_rise <- function(climbs, segments) {
  rise <- segments %>%
    transmute(segment_id = as.character(segment_id), rise_m = round(elev_high_m - elev_low_m))
  climbs %>%
    select(-any_of("rise_m")) %>%
    left_join(rise, by = "segment_id")
}
