library(dplyr)
library(purrr)

# Climb matching on full Strava streams, and the metrics of each effort.
# Spec: private cycling spec, sections 4.2–4.4. Defaults are the spec's
# constants; the pipeline passes them from config/athlete.yml.
#
#   read_strava_stream(path)        stream file -> one row per sample
#   match_climb(stream, climb)      -> index pairs (i0, i1), one per ascent
#   effort_metrics(stream, i0, i1)  -> stops, speeds, heart rate, status
#   climb_efforts(stream, climbs)   -> all efforts of one ride

read_strava_stream <- function(path) {
  s <- jsonlite::read_json(path, simplifyVector = TRUE)
  if (is.null(s$latlng) || is.null(s$time) || is.null(s$distance)) return(NULL)
  ll <- s$latlng$data
  n <- length(s$time$data)
  if (!is.matrix(ll) || nrow(ll) != n) return(NULL)
  tibble(
    t    = as.numeric(s$time$data),
    lat  = ll[, 1],
    lon  = ll[, 2],
    dist = as.numeric(s$distance$data),
    alt  = if (is.null(s$altitude)) NA_real_ else as.numeric(s$altitude$data),
    hr   = if (is.null(s$heartrate)) NA_real_ else as.numeric(s$heartrate$data)
  )
}

# Metres east (x) and north (y) of an origin: equirectangular projection
# around the origin's latitude, accurate to well under a metre over a few km.
project_xy <- function(lat, lon, lat0, lon0) {
  list(
    x = (lon - lon0) * 111320 * cos(lat0 * pi / 180),
    y = (lat - lat0) * 111320
  )
}

# Every ascent of `climb` (a row with start_lat, start_lon, end_lat, end_lon,
# length_m) in `stream`:
# - samples within `radius_m` of the climb start form passes; the closest
#   approach in each pass is i0;
# - from i0, the end is the closest approach to the climb end, within
#   `radius_m`, at a path distance between min_frac × L and
#   max_frac × L + pad_m (first such pass only).
# Ascents never overlap: a pass that starts before the previous i1 is skipped.
match_climb <- function(stream, climb, radius_m = 30,
                        min_frac = 0.75, max_frac = 1.35, pad_m = 40) {
  none <- tibble(i0 = integer(), i1 = integer())
  p   <- project_xy(stream$lat, stream$lon, climb$start_lat, climb$start_lon)
  end <- project_xy(climb$end_lat, climb$end_lon, climb$start_lat, climb$start_lon)
  d_start <- sqrt(p$x^2 + p$y^2)
  d_end   <- sqrt((p$x - end$x)^2 + (p$y - end$y)^2)

  near <- which(d_start <= radius_m)
  if (length(near) == 0) return(none)
  passes <- split(near, cumsum(c(1, diff(near) > 1)))

  out <- list()
  last_i1 <- 0L
  for (pass in passes) {
    if (min(pass) <= last_i1) next
    i0 <- pass[which.min(d_start[pass])]
    path <- stream$dist - stream$dist[i0]
    cand <- which(seq_along(path) > i0 &
                  path >= min_frac * climb$length_m &
                  path <= max_frac * climb$length_m + pad_m &
                  d_end <= radius_m)
    if (length(cand) == 0) next
    first_pass <- cand[cumsum(c(1, diff(cand) > 1)) == 1]
    i1 <- first_pass[which.min(d_end[first_pass])]
    out[[length(out) + 1]] <- tibble(i0 = as.integer(i0), i1 = as.integer(i1))
    last_i1 <- i1
  }
  bind_rows(none, out)
}

# Metrics of one effort between samples i0 and i1.
# - stop_s: time in intervals slower than stop_speed_ms (auto-pause gaps
#   count: the distance barely moves while the clock does)
# - crawl10_kmh: slowest 10 s, on the stream resampled to 1 s
# - hr_avg: time-weighted; hr_end: time-weighted over the last 30 s
# - status, first match wins:
#     walk   a stretch of >= walk_min_s at walking speed (pushing the bike)
#     clean  stop_s <= max_stop_s
#     top    every stop at or after top_frac of the climb
#     foot   no stop longer than foot_max_s (a foot down, then on)
#     early  any other stop
effort_metrics <- function(stream, i0, i1,
                           stop_speed_ms = 0.5, max_stop_s = 2, top_frac = 0.95,
                           foot_max_s = 10, walk_speed_ms = c(0.6, 1.5),
                           walk_min_s = 20) {
  s  <- stream[i0:i1, ]
  n  <- nrow(s)
  dt <- diff(s$t)
  dd <- diff(s$dist)
  v  <- ifelse(dt > 0, dd / dt, NA_real_)

  stopped <- !is.na(v) & v < stop_speed_ms
  stop_s  <- sum(dt[stopped])
  elapsed <- s$t[n] - s$t[1]
  climb_m <- s$dist[n] - s$dist[1]
  frac    <- (s$dist[-n] - s$dist[1]) / climb_m

  # Stops as runs of stopped intervals: duration and where they start
  run_id <- cumsum(c(1, diff(stopped) != 0))
  stops <- tibble(run = run_id, stopped, dt, frac) %>%
    filter(stopped) %>%
    summarise(.by = run, dur = sum(dt), at = first(frac))

  # 1 Hz resample for rolling speeds
  tg <- seq(s$t[1], s$t[n], by = 1)
  dg <- stats::approx(s$t, s$dist, tg, ties = "ordered")$y
  crawl10 <- if (length(dg) > 10) {
    min((dg[11:length(dg)] - dg[1:(length(dg) - 10)]) / 10) * 3.6
  } else NA_real_
  v1 <- diff(dg)
  walking <- v1 >= walk_speed_ms[1] & v1 <= walk_speed_ms[2]
  walk_s <- if (any(walking)) max(rle(walking)$lengths[rle(walking)$values]) else 0

  hr_iv  <- s$hr[-n]
  ok     <- !is.na(hr_iv) & dt > 0
  hr_avg <- if (any(ok)) sum(hr_iv[ok] * dt[ok]) / sum(dt[ok]) else NA_real_
  tail30 <- ok & s$t[-n] >= s$t[n] - 30
  hr_end <- if (any(tail30)) sum(hr_iv[tail30] * dt[tail30]) / sum(dt[tail30]) else NA_real_

  status <- if (walk_s >= walk_min_s) "walk"
    else if (stop_s <= max_stop_s) "clean"
    else if (all(stops$at >= top_frac)) "top"
    else if (max(stops$dur) <= foot_max_s) "foot"
    else "early"

  tibble(
    elapsed_s     = elapsed,
    stop_s        = stop_s,
    moving_s      = elapsed - stop_s,
    climb_m       = climb_m,
    crawl10_kmh   = crawl10,
    walk_s        = walk_s,
    hr_avg        = hr_avg,
    hr_end        = hr_end,
    hrs_into_ride = (stream$t[i0] - stream$t[1]) / 3600,
    status        = status
  )
}

# All efforts on all climbs in one ride. `climbs` has a name column plus the
# columns match_climb() needs; `...` goes to effort_metrics().
climb_efforts <- function(stream, climbs, activity_id, radius_m = 30, ...) {
  empty <- tibble(activity_id = character(), climb = character(),
                  i0 = integer(), i1 = integer())
  if (is.null(stream) || nrow(stream) < 2) return(empty)
  climbs %>%
    split(seq_len(nrow(.))) %>%
    map(\(cl) {
      m <- match_climb(stream, cl, radius_m = radius_m)
      if (nrow(m) == 0) return(NULL)
      m %>%
        mutate(activity_id = as.character(activity_id), climb = cl$name, .before = 1) %>%
        bind_cols(map(seq_len(nrow(m)), \(k) effort_metrics(stream, m$i0[k], m$i1[k], ...)) %>%
                    list_rbind())
    }) %>%
    list_rbind() %>%
    bind_rows(empty, .)
}
