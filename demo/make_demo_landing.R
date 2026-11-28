# Synthetic raw files for the demo database: every source the pipeline reads
# (Fitbit Takeout, Garmin API, Strava API, the lifting backfill, MacroFactor),
# in the landing layout of docs/sources.md, for an invented person. Nothing
# here comes from real data; the climbs, bikes and goals are the example
# configs (config/*.example.*).
#
# The story the tabs tell: two and a half years ending on DEMO$as_of. Fitbit
# until the switch to Garmin; the lifting history in Strong until MacroFactor
# starts; rides past the three example climbs with a chest strap from mid
# 2028; a cut from January 2030 (goals.example.yml). Values follow slow
# trends plus noise, so the models have something to find.
#
# make_demo_landing(root) writes root/data/landing/ and root/config/.
# Needs fixtures/macrofactor_schema.R and fixtures/make_macrofactor_fixture.R.

DEMO <- list(
  start       = as.Date("2028-01-01"),
  as_of       = as.Date("2030-07-15"),
  garmin_from = as.Date("2029-03-01"),   # Fitbit's last day is the day before
  mf_from     = as.Date("2028-04-01"),   # MacroFactor: weight, intake and sets from here; rides too (power needs its weight)
  chest_from  = as.Date("2028-06-01"),   # rides recorded with the bike computer and chest strap
  cut_from    = as.Date("2030-01-06"),   # weight_goal.start_date in goals.example.yml
  tz          = "Europe/Berlin",         # home_timezone in athlete.example.yml
  # Next to the example climbs (config/climbs.example.csv), at open sea:
  # invented coordinates that are no real place
  home        = c(lat = 45.00, lon = -30.00),
  seed        = 42
)

wj <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
}
wcsv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path, na = "")
}
# Smooth noise: an AR(1) series with standard deviation `sd`
ar1 <- function(n, sd, phi = 0.8) {
  x <- as.numeric(stats::filter(rnorm(n, 0, sd * sqrt(1 - phi^2)), phi, method = "recursive"))
  x
}
years_in <- function(d) as.numeric(as.Date(d) - DEMO$start) / 365.25
# Fitness: 0 at the start, rising towards 1
fitness_of <- function(d) 1 - exp(-years_in(d) / 1.2)
# Trend weight (kg): ~85 drifting to ~82 by the plan's start, then the cut:
# 0.36 kg a week to 77.5 kg (90% of the plan's pace), slower after
trend_weight <- function(d) {
  d <- as.Date(d)
  t <- pmin(years_in(d), years_in(DEMO$cut_from))
  pre <- 85 - 3 * t / years_in(DEMO$cut_from) + 0.4 * sin(2 * pi * years_in(d))
  wk <- pmax(0, as.numeric(d - DEMO$cut_from)) / 7
  wk1 <- (82 - 77.5) / 0.36
  pre - 0.36 * pmin(wk, wk1) - 0.15 * pmax(0, wk - wk1) + 0.15 * sin(wk / 2)
}
local_time <- function(d, hour) as.POSIXct(paste(d, "00:00:00"), tz = DEMO$tz) + hour * 3600
fmt_local <- function(t) format(t, "%Y-%m-%dT%H:%M:%S", tz = DEMO$tz)
fmt_utc <- function(t, suffix = "Z") paste0(format(t, "%Y-%m-%dT%H:%M:%S", tz = "UTC"), suffix)

# ── Nights and days (both devices) ───────────────────────────────────────────
demo_days <- function() {
  d <- seq(DEMO$start, DEMO$as_of, by = "day")
  n <- length(d)
  f <- fitness_of(d)
  wkend <- format(d, "%u") %in% c("6", "7")
  bed <- 23.2 + ar1(n, 0.5) + 0.6 * wkend               # hours after midnight of the day before
  sleep_h <- pmin(9.5, pmax(4.5, 7.2 + ar1(n, 0.6) + 0.5 * wkend))
  tibble::tibble(
    date = d, era = ifelse(d < DEMO$garmin_from, "fitbit", "garmin"),
    rhr = round(58 - 5 * f + ar1(n, 1.5)),
    hrv = pmax(15, 42 + 10 * f + ar1(n, 6, 0.6)),
    resp = 14.6 - 0.4 * f + ar1(n, 0.4),
    spo2 = pmin(99, 95.8 + ar1(n, 0.6)),
    skin = ar1(n, 0.3, 0.5),
    bed_start = local_time(d - 1, bed),
    sleep_s = round(sleep_h * 3600),
    deep = round(sleep_h * 3600 * (0.15 + rnorm(n, 0, 0.02))),
    rem = round(sleep_h * 3600 * (0.21 + rnorm(n, 0, 0.03))),
    awake = round(pmax(5, rnorm(n, 45, 15)) * 60),
    steps = round(pmax(1500, 9800 - 1200 * years_in(d) + ar1(n, 2200, 0.3))),
    stress = round(pmin(95, pmax(40, 72 + ar1(n, 8, 0.5))))
  ) %>%
    dplyr::mutate(light = sleep_s - deep - rem,
                  bed_end = bed_start + sleep_s + awake,
                  score = round(pmin(95, pmax(50, 60 + 3 * (sleep_s / 3600 - 6) + (hrv - 40) / 3 + rnorm(dplyr::n(), 0, 4)))))
}

write_fitbit <- function(days, active, root) {
  fb <- file.path(root, "data/landing/fitbit/Takeout/Fitbit")
  pa <- file.path(fb, "Physical Activity_GoogleData")
  x <- dplyr::filter(days, era == "fitbit")
  n <- nrow(x)
  label <- paste0(x$date, "T00:00:00Z")   # Fitbit's daily label (not a real UTC midnight)
  src <- "Fitbit Example"
  # Fitbit reads resting HR a little higher and HRV a little lower than Garmin
  wcsv(tibble::tibble(timestamp = label, `beats per minute` = x$rhr + 1, `data source` = src),
       file.path(pa, "daily_resting_heart_rate.csv"))
  wcsv(tibble::tibble(timestamp = label, `average heart rate variability milliseconds` = round(x$hrv * 0.85, 1),
                      `data source` = src), file.path(pa, "daily_heart_rate_variability.csv"))
  wcsv(tibble::tibble(timestamp = label, `breaths per minute` = round(x$resp, 1), `data source` = src),
       file.path(pa, "daily_respiratory_rate.csv"))
  wcsv(tibble::tibble(timestamp = label, `nightly temperature celsius` = round(34.2 + x$skin, 2),
                      `baseline temperature celsius` = 34.2, `relative nightly stddev 30d celsius` = 0.3,
                      `data source` = src), file.path(pa, "daily_sleep_temperature_derivations.csv"))
  month <- format(x$date, "%Y-%m")
  for (m in unique(month)) {
    i <- month == m
    day1 <- paste0(m, "-01")
    noon <- paste0(x$date[i], "T11:00:00Z")
    wcsv(tibble::tibble(timestamp = label[i], `average percentage` = round(x$spo2[i], 1)),
         file.path(pa, sprintf("daily_oxygen_saturation_%s.csv", m)))
    wcsv(tibble::tibble(timestamp = noon, steps = x$steps[i], `data source` = src),
         file.path(pa, sprintf("steps_%s.csv", day1)))
    wcsv(tibble::tibble(timestamp = noon, distance = round(x$steps[i] * 0.75), `data source` = src),
         file.path(pa, sprintf("distance_%s.csv", day1)))
    wcsv(tibble::tibble(timestamp = noon, calories = round(1800 + x$steps[i] * 0.05 + rnorm(sum(i), 0, 80)),
                        `data source` = src), file.path(pa, sprintf("calories_%s.csv", day1)))
    # Sleep logs, one file a month
    j <- which(i)
    wj(lapply(j, function(k) list(
      logId = 30000000000 + k, dateOfSleep = format(x$date[k]),
      startTime = paste0(fmt_local(x$bed_start[k]), ".000"), endTime = paste0(fmt_local(x$bed_end[k]), ".000"),
      minutesAsleep = round(x$sleep_s[k] / 60), minutesAwake = round(x$awake[k] / 60),
      efficiency = round(100 * x$sleep_s[k] / (x$sleep_s[k] + x$awake[k])), type = "stages", mainSleep = TRUE,
      levels = list(summary = list(deep = list(minutes = round(x$deep[k] / 60)),
                                   light = list(minutes = round(x$light[k] / 60)),
                                   rem = list(minutes = round(x$rem[k] / 60)))))),
      file.path(fb, "Global Export Data", sprintf("sleep-%s.json", day1)))
    wcsv(tibble::tibble(date = format(x$date[i]),
                        readiness_score_value = round(pmin(100, pmax(20, 60 + (x$hrv[i] - 45) + rnorm(sum(i), 0, 8))))),
         file.path(fb, "Daily Readiness", sprintf("Daily Readiness Score - %s.csv", m)))
    wcsv(tibble::tibble(date_time = rep(paste0(x$date[i], "T18:00"), 2),
                        heart_zone_id = rep(c("FAT_BURN", "CARDIO"), each = sum(i)),
                        total_minutes = c(round(20 + 40 * (x$date[i] %in% active)), round(5 + 30 * (x$date[i] %in% active)))),
         file.path(fb, "Active Zone Minutes (AZM)", sprintf("Active Zone Minutes - %s.csv", day1)))
  }
  wcsv(tibble::tibble(score_time = paste0(x$date, "T07:00:00Z"), overall_score = x$score,
                      duration_score = round(pmin(50, x$score * 0.5)), composition_score = round(x$score * 0.25),
                      revitalization_score = round(x$score * 0.2)),
       file.path(fb, "Health Fitness Data_GoogleData", sprintf("UserSleepScores_%s.csv", DEMO$start)))
  wcsv(tibble::tibble(DATE = paste0(x$date, "T00:00:00"), STRESS_SCORE = x$stress),
       file.path(fb, "Stress Score", "Stress Score.csv"))
  # Weigh-ins on the Fitbit scale until MacroFactor starts (real UTC instants)
  w <- dplyr::filter(days, date < DEMO$mf_from, as.integer(date) %% 3 == 0)
  ts <- fmt_utc(local_time(w$date, 7.1), "Z")
  wcsv(tibble::tibble(timestamp = ts, `weight grams` = round((trend_weight(w$date) + rnorm(nrow(w), 0, 0.5)) * 1000),
                      `data source` = "Example Scale"), file.path(pa, "weight.csv"))
  wcsv(tibble::tibble(timestamp = ts[seq(1, length(ts), by = 10)], `body fat percentage` = 19,
                      `data source` = "Example Scale"), file.path(pa, sprintf("body_fat_%s.csv", DEMO$start)))
}

write_garmin <- function(days, active, root) {
  x <- dplyr::filter(days, era == "garmin")
  n <- nrow(x)
  base <- as.numeric(stats::filter(x$hrv, rep(1 / 21, 21), sides = 1))
  base[is.na(base)] <- x$hrv[is.na(base)]
  status <- ifelse(x$hrv < base * 0.85, "UNBALANCED", "BALANCED")
  recs <- lapply(seq_len(n), function(k) list(
    date = format(x$date[k]), rhr = x$rhr[k], hrv = round(base[k]),
    hrv_last_night_avg = round(x$hrv[k]), hrv_last_night_5_min_high = round(x$hrv[k] * 1.4),
    hrv_baseline_low = round(base[k] * 0.88), hrv_baseline_high = round(base[k] * 1.15),
    hrv_status = status[k],
    im_moderate = round(10 + 30 * (x$date[k] %in% active)), im_vigorous = round(2 + 25 * (x$date[k] %in% active)),
    im_week_goal = 150, steps = x$steps[k], step_goal = 8000, distance_m = round(x$steps[k] * 0.78),
    floors_ascended = round(runif(1, 2, 12)),
    kcal_total = round(2400 + x$steps[k] * 0.04), kcal_active = round(500 + x$steps[k] * 0.04), kcal_bmr = 1900,
    spo2_avg = round(x$spo2[k]), spo2_lowest = round(x$spo2[k] - 5),
    resp_waking_avg = round(x$resp[k] + 1), resp_lowest = round(x$resp[k] - 3), resp_highest = round(x$resp[k] + 6),
    sleep_start_utc = fmt_utc(x$bed_start[k], "+00:00"), sleep_end_utc = fmt_utc(x$bed_end[k], "+00:00"),
    sleep_start_local = fmt_local(x$bed_start[k]), sleep_end_local = fmt_local(x$bed_end[k]),
    sleep_s = x$sleep_s[k], sleep_deep_s = x$deep[k], sleep_light_s = x$light[k], sleep_rem_s = x$rem[k],
    sleep_awake_s = x$awake[k], sleep_unmeasurable_s = 0, nap_s = 0, sleep_awake_count = round(runif(1, 0, 4)),
    sleep_score = x$score[k], sleep_avg_hr = x$rhr[k] + 4, sleep_avg_stress = round(runif(1, 12, 30)),
    sleep_spo2_avg = round(x$spo2[k], 1), sleep_spo2_lowest = round(x$spo2[k] - 6),
    sleep_resp_avg = round(x$resp[k], 1), sleep_resp_lowest = round(x$resp[k] - 2), sleep_resp_highest = round(x$resp[k] + 4),
    sleep_skin_temp_dev_c = round(x$skin[k], 1), sleep_restless_moments = round(runif(1, 20, 60)),
    vo2max = if (format(x$date[k], "%u") == "7") round(48 + 4 * fitness_of(x$date[k])) else NULL
  ))
  wj(recs, file.path(root, "data/landing/garmin", sprintf("%s_%s_garmin_api.json", min(x$date), max(x$date))))
}

# ── Rides, runs and BJJ (Strava) ─────────────────────────────────────────────
# Local metres around home (equirectangular; fine at this scale)
to_xy <- function(lat, lon) cbind(x = (lon - DEMO$home[["lon"]]) * 111320 * cos(DEMO$home[["lat"]] * pi / 180),
                                  y = (lat - DEMO$home[["lat"]]) * 111320)
to_ll <- function(xy) cbind(lat = DEMO$home[["lat"]] + xy[, 2] / 111320,
                            lon = DEMO$home[["lon"]] + xy[, 1] / (111320 * cos(DEMO$home[["lat"]] * pi / 180)))

# A climb's road: start → a bend → end, as long as the climb's length_m
climb_path <- function(cl) {
  s <- to_xy(cl$start_lat, cl$start_lon); e <- to_xy(cl$end_lat, cl$end_lon)
  d <- sqrt(sum((e - s)^2)); h <- sqrt(max(0, (cl$length_m / 2)^2 - (d / 2)^2))
  nrm <- c(-(e - s)[2], (e - s)[1]) / d
  rbind(s, (s + e) / 2 + h * nrm, e)
}
# Points every `step` m along a polyline
densify <- function(pts, step = 5) {
  out <- pts[1, , drop = FALSE]
  for (i in seq_len(nrow(pts) - 1)) {
    a <- pts[i, ]; b <- pts[i + 1, ]; L <- sqrt(sum((b - a)^2))
    k <- max(1, ceiling(L / step))
    f <- (1:k) / k
    out <- rbind(out, cbind(a[1] + f * (b[1] - a[1]), a[2] + f * (b[2] - a[2])))
  }
  out
}
# Speed (m/s) for a power on a grade: the physics of R/power_model.R
climb_speed <- function(power_w, grade, crr, mass_kg) {
  th <- atan(grade)
  f <- function(v) 0.97 * power_w - v * (mass_kg * 9.81 * (sin(th) + crr * cos(th)) + 0.5 * 1.2 * 0.45 * v^2)
  stats::uniroot(f, c(0.3, 20))$root
}

# One activity's stream and summary numbers. legs: a list of
# list(pts = xy matrix, climb = row of the climb list or NULL)
simulate_activity <- function(legs, start, v_flat, hr_flat, weight_kg, climbs_cfg, sport = "Ride") {
  pts <- NULL; kind <- NULL; climb_i <- NULL
  for (k in seq_along(legs)) {
    p <- densify(legs[[k]]$pts)
    if (!is.null(pts)) p <- p[-1, , drop = FALSE]
    pts <- rbind(pts, p)
    kind <- c(kind, rep(if (is.null(legs[[k]]$climb)) "flat" else "climb", nrow(p)))
    climb_i <- c(climb_i, rep(if (is.null(legs[[k]]$climb)) NA else k, nrow(p)))
  }
  n <- nrow(pts)
  step <- c(0, sqrt(rowSums(diff(pts)^2)))
  dist <- cumsum(step)
  # Altitude: rolling country on the flat (only flat metres count, so a
  # climb's gain is its own), each climb rising its gain, a descent after it
  flat_m <- cumsum(ifelse(kind == "flat", step, 0))
  alt <- 15 + 6 * sin(2 * pi * flat_m / 4000)
  offset <- numeric(n); top <- 0; since_top <- Inf
  hr_target <- rep(hr_flat, n); v <- rep(v_flat, n); efforts <- list()
  for (i in seq_len(n)) {
    if (kind[i] == "climb") {
      leg <- legs[[climb_i[i]]]
      seg <- which(climb_i == climb_i[i])
      frac <- (dist[i] - dist[seg[1]]) / (dist[max(seg)] - dist[seg[1]])
      offset[i] <- frac * leg$climb$gain_m
      top <- offset[i]; since_top <- 0
      v[i] <- leg$speed
      hr_target[i] <- hr_flat + (leg$hr_end - hr_flat) * min(1, frac / 0.6)
    } else {
      since_top <- since_top + step[i]
      offset[i] <- top * max(0, 1 - since_top / 2500)
      if (since_top < 2500) v[i] <- v_flat * 1.25
    }
  }
  alt <- alt + offset
  dt <- c(0, step[-1] / v[-1])
  # Stops: a few seconds standing at a set point (a junction, or a foot down on a climb)
  for (k in seq_along(legs)) {
    if (!is.null(legs[[k]]$stop_s) && legs[[k]]$stop_s > 0) {
      seg <- which(climb_i == k)
      j <- seg[ceiling(length(seg) * legs[[k]]$stop_at)]
      dt[j] <- dt[j] + legs[[k]]$stop_s
    }
  }
  time <- cumsum(dt)
  # Heart rate lags the effort (tau 25 s)
  hr <- numeric(n); hr[1] <- hr_flat - 15
  for (i in 2:n) hr[i] <- hr[i - 1] + (hr_target[i] - hr[i - 1]) * (1 - exp(-dt[i] / 25))
  hr <- hr + rnorm(n, 0, 1.2)
  # Resample to one point every 2 s (a stop keeps the point where it stopped)
  grid <- seq(0, max(time), by = 2)
  at <- function(y) stats::approx(time, y, xout = grid, ties = "ordered")$y
  dist_g <- at(dist)
  ll <- to_ll(cbind(at(pts[, 1]), at(pts[, 2])))
  alt_g <- at(alt); hr_g <- round(at(hr))
  spd <- c(0, diff(dist_g) / 2)
  # Segment efforts for the climbs on the route
  for (k in seq_along(legs)) {
    if (is.null(legs[[k]]$climb)) next
    seg <- which(climb_i == k)
    i0 <- which(grid >= time[seg[1]])[1]; i1 <- utils::tail(which(grid <= time[max(seg)]), 1)
    efforts[[length(efforts) + 1]] <- list(climb = legs[[k]]$climb, i0 = i0 - 1, i1 = i1 - 1,
                                           elapsed = grid[i1] - grid[i0], dist = dist_g[i1] - dist_g[i0],
                                           hr_avg = mean(hr_g[i0:i1]), hr_max = max(hr_g[i0:i1]),
                                           start = start + grid[i0])
  }
  gain <- sum(pmax(0, diff(stats::filter(alt_g, rep(1 / 5, 5), sides = 2)[!is.na(stats::filter(alt_g, rep(1 / 5, 5), sides = 2))])))
  moving <- sum(spd > 0.5) * 2
  list(
    stream = list(time = list(data = grid), latlng = list(data = round(ll, 6)), distance = list(data = round(dist_g, 1)),
                  altitude = list(data = round(alt_g, 1)), heartrate = list(data = hr_g),
                  velocity_smooth = list(data = round(spd, 2))),
    distance = max(dist_g), moving = moving, elapsed = max(grid), gain = round(gain, 1),
    avg_hr = round(mean(hr_g), 1), max_hr = max(hr_g), max_speed = max(spd), efforts = efforts
  )
}

# Ride days: Wednesday and Saturday all year, Sunday from May to August,
# about a third skipped; now and then a long Sunday ride
plan_rides <- function() {
  d <- seq(DEMO$mf_from, DEMO$as_of, by = "day")
  dow <- format(d, "%u"); mon <- as.integer(format(d, "%m"))
  ride <- (dow %in% c("3", "6") | (dow == "7" & mon %in% 5:8)) & runif(length(d)) < 0.5
  d[ride]
}

demo_strava <- function(root, climbs_cfg) {
  strava <- file.path(root, "data/landing/strava")
  rides <- plan_rides()
  run_days <- seq(as.Date("2029-09-07"), DEMO$as_of, by = "day")
  run_days <- run_days[format(run_days, "%u") == "5" & runif(length(run_days)) < 0.4]
  bjj <- seq(as.Date("2028-09-01"), DEMO$as_of, by = "day")
  bjj <- bjj[format(bjj, "%u") %in% c("2", "4") & runif(length(bjj)) < 0.9]
  surface_crr <- c(cobbles = 0.018, part_cobbles = 0.012, asphalt = 0.006)
  summaries <- list(); id <- 1000000000; effort_id <- 2000000000000
  all_efforts <- list()
  home <- c(0, 0)

  for (k in seq_along(rides)) {
    d <- rides[k]; id <- id + 1
    f <- fitness_of(d); w <- trend_weight(d)
    chest <- d >= DEMO$chest_from
    long <- format(d, "%u") == "7" && runif(1) < 0.35
    target_km <- if (long) runif(1, 85, 110) else runif(1, 22, 38)
    pick <- climbs_cfg[sample(nrow(climbs_cfg), sample(1:3, 1, prob = c(0.55, 0.35, 0.1))), ]
    start <- local_time(d, if (format(d, "%u") %in% c("6", "7")) 9.5 else 17.75) + runif(1, -1800, 1800)
    hr_flat <- rnorm(1, 128, 4)
    legs <- list(); cur <- home; hours <- 0
    v_flat <- rnorm(1, 7.4 + 0.8 * f, 0.3)
    for (j in seq_len(nrow(pick))) {
      cl <- pick[j, ]
      path <- climb_path(cl)
      # An outward loop on the way there, so the ride reaches its length
      legs[[length(legs) + 1]] <- list(pts = rbind(cur, path[1, ]), climb = NULL)
      hours <- hours + sqrt(sum((path[1, ] - cur)^2)) / v_flat / 3600
      hr_end <- rnorm(1, 162, 5)
      power <- 225 + 70 * f + 3.5 * (hr_end - 160) - 8 * hours + rnorm(1, 0, 10)
      crr <- surface_crr[[if (is.na(cl$surface) || cl$surface == "") "asphalt" else cl$surface]]
      speed <- climb_speed(power, cl$gain_m / cl$length_m, crr, w + 8.8 + 2)
      # A foot down now and then, less as fitness grows; the cobbled hill
      # only goes clean from spring 2030 (the oath's climb quest and relic)
      p_stop <- if (cl$surface %in% "cobbles") {
        if (d < as.Date("2030-03-15")) 1 else 0.45
      } else 0.12 * (1 - 0.7 * f)
      stop_s <- if (runif(1) < p_stop) round(runif(1, 15, 40)) else 0
      legs[[length(legs) + 1]] <- list(pts = path, climb = cl, speed = speed, hr_end = hr_end,
                                       stop_s = stop_s, stop_at = runif(1, 0.3, 0.8))
      cur <- path[3, ]
    }
    # Home, through a waypoint that makes up the distance
    so_far <- sum(vapply(legs, function(l) sum(sqrt(rowSums(diff(l$pts)^2))), numeric(1)))
    rest <- target_km * 1000 - so_far
    c2 <- sqrt(sum((cur - home)^2)) / 2
    if (rest / 2 > c2 + 500) {
      mid <- (cur + home) / 2
      dir <- if (c2 > 0) c(-(home - cur)[2], (home - cur)[1]) / (2 * c2) else c(1, 0)
      b <- sqrt((rest / 2)^2 - c2^2)
      wp <- mid + sample(c(-1, 1), 1) * b * dir
      legs[[length(legs) + 1]] <- list(pts = rbind(cur, wp, home), climb = NULL)
    } else {
      legs[[length(legs) + 1]] <- list(pts = rbind(cur, home), climb = NULL)
    }
    a <- simulate_activity(legs, start, v_flat, hr_flat, w, climbs_cfg)
    gear <- if (d <= as.Date("2029-12-31") && runif(1) < 0.12) NA else if (runif(1) < 0.1) "b1000002" else "b1000001"
    device <- if (chest) "Example Bike Computer" else "Example Watch"
    kj <- round(a$moving * (150 + 25 * f) / 1000)
    summaries[[length(summaries) + 1]] <- list(
      id = id, start_date = fmt_utc(start), start_date_local = paste0(fmt_local(start), "Z"),
      timezone = "(GMT+01:00) Europe/Berlin", name = if (long) "Long ride" else "Evening loop",
      sport_type = "Ride", trainer = FALSE, commute = FALSE, manual = FALSE,
      distance = round(a$distance, 1), moving_time = a$moving, elapsed_time = a$elapsed,
      total_elevation_gain = a$gain, average_speed = round(a$distance / a$moving, 3), max_speed = round(a$max_speed, 2),
      has_heartrate = TRUE, average_heartrate = a$avg_hr, max_heartrate = a$max_hr, device_watts = FALSE,
      average_watts = round(kj * 1000 / a$moving, 1), weighted_average_watts = NULL, kilojoules = kj,
      average_cadence = round(rnorm(1, 84, 3), 1), suffer_score = round(a$moving / 60 * 0.8),
      gear_id = if (is.na(gear)) NULL else gear, device_name = device)
    efforts <- lapply(a$efforts, function(e) {
      effort_id <<- effort_id + 1
      all_efforts[[length(all_efforts) + 1]] <<- list(id = effort_id, segment = e$climb$segment_id, elapsed = e$elapsed,
                                                      date = d, activity = id)
      list(id = effort_id, activity = list(id = id),
           segment = list(id = as.numeric(e$climb$segment_id), name = e$climb$name, activity_type = "Ride",
                          distance = e$climb$length_m, average_grade = round(100 * e$climb$gain_m / e$climb$length_m, 1),
                          maximum_grade = round(160 * e$climb$gain_m / e$climb$length_m, 1),
                          elevation_low = 15, elevation_high = 15 + e$climb$gain_m, climb_category = 0,
                          city = "Example Town", country = "Exampleland",
                          start_latlng = c(e$climb$start_lat, e$climb$start_lon), end_latlng = c(e$climb$end_lat, e$climb$end_lon),
                          hazardous = FALSE, private = FALSE),
           start_date = fmt_utc(e$start), start_date_local = paste0(fmt_local(e$start), "Z"),
           distance = round(e$dist, 1), elapsed_time = e$elapsed, moving_time = e$elapsed,
           average_heartrate = round(e$hr_avg, 1), max_heartrate = e$hr_max, device_watts = FALSE,
           average_watts = NULL, average_cadence = NULL, pr_rank = NULL, kom_rank = NULL, hidden = FALSE,
           start_index = e$i0, end_index = e$i1)
    })
    detail <- list(id = id, calories = round(kj * 1.05), perceived_exertion = NULL, description = "",
                   laps = list(list(id = id * 10, activity = list(id = id), lap_index = 1, start_date = fmt_utc(start),
                                    start_date_local = paste0(fmt_local(start), "Z"), distance = round(a$distance, 1),
                                    moving_time = a$moving, elapsed_time = a$elapsed, total_elevation_gain = a$gain,
                                    average_speed = round(a$distance / a$moving, 3), max_speed = round(a$max_speed, 2),
                                    average_heartrate = a$avg_hr, max_heartrate = a$max_hr, device_watts = FALSE,
                                    average_watts = NULL, average_cadence = NULL, start_index = 0,
                                    end_index = length(a$stream$time$data) - 1)),
                   segment_efforts = efforts, best_efforts = list())
    wj(detail, file.path(strava, "details", paste0(id, ".json")))
    wj(a$stream, file.path(strava, "streams", paste0(id, ".json")))
  }

  # Runs: a loop near home, away from the climbs, with best efforts
  for (d in as.list(run_days)) {
    id <- id + 1
    start <- local_time(d, 7.5)
    r <- runif(1, 700, 1000); ang <- seq(0, 2 * pi, length.out = 13)
    loop <- cbind(-r + r * cos(ang), r * sin(ang)) * c(1, 1)
    v <- rnorm(1, 3.0 + 0.3 * fitness_of(d), 0.1)
    laps <- sample(2:3, 1)
    a <- simulate_activity(rep(list(list(pts = loop, climb = NULL)), laps), start, v, 145, trend_weight(d), climbs_cfg, "Run")
    summaries[[length(summaries) + 1]] <- list(
      id = id, start_date = fmt_utc(start), start_date_local = paste0(fmt_local(start), "Z"),
      timezone = "(GMT+01:00) Europe/Berlin", name = "Morning run", sport_type = "Run",
      trainer = FALSE, commute = FALSE, manual = FALSE, distance = round(a$distance, 1),
      moving_time = a$moving, elapsed_time = a$elapsed, total_elevation_gain = a$gain,
      average_speed = round(a$distance / a$moving, 3), max_speed = round(a$max_speed, 2),
      has_heartrate = TRUE, average_heartrate = a$avg_hr, max_heartrate = a$max_hr, device_watts = FALSE,
      average_cadence = 82, suffer_score = round(a$moving / 60), device_name = "Example Watch")
    be <- function(name, m) list(id = id * 100 + m %/% 1000, activity = list(id = id), name = name,
                                 start_date = fmt_utc(start), start_date_local = paste0(fmt_local(start), "Z"),
                                 distance = m, elapsed_time = round(m / v), moving_time = round(m / v),
                                 pr_rank = NULL, start_index = 0, end_index = round(m / v / 2))
    wj(list(id = id, calories = round(a$distance / 1000 * 70), description = "", laps = list(),
            segment_efforts = list(), best_efforts = Filter(Negate(is.null), list(be("1K", 1000),
              if (a$distance >= 5000) be("5K", 5000)))),
       file.path(strava, "details", paste0(id, ".json")))
    wj(a$stream, file.path(strava, "streams", paste0(id, ".json")))
  }

  # BJJ: Workout sessions without a stream
  for (d in as.list(bjj)) {
    id <- id + 1
    start <- local_time(d, 19.5)
    summaries[[length(summaries) + 1]] <- list(
      id = id, start_date = fmt_utc(start), start_date_local = paste0(fmt_local(start), "Z"),
      timezone = "(GMT+01:00) Europe/Berlin", name = "BJJ", sport_type = "Workout",
      trainer = FALSE, commute = FALSE, manual = FALSE, distance = 0, moving_time = 5400, elapsed_time = 5400,
      total_elevation_gain = 0, average_speed = 0, max_speed = 0, has_heartrate = TRUE,
      average_heartrate = round(rnorm(1, 138, 6)), max_heartrate = round(rnorm(1, 178, 5)), device_watts = FALSE,
      suffer_score = 90, device_name = "Example Watch")
    wj(list(id = id, calories = 750, description = "", laps = list(), segment_efforts = list(), best_efforts = list()),
       file.path(strava, "details", paste0(id, ".json")))
  }

  # PR ranks: an effort among the three fastest so far on its segment
  ef <- dplyr::bind_rows(lapply(all_efforts, tibble::as_tibble)) %>% dplyr::arrange(date)
  ef <- ef %>% dplyr::group_by(segment) %>%
    dplyr::mutate(rank = vapply(seq_along(elapsed), function(i) sum(elapsed[seq_len(i)] < elapsed[i]) + 1, numeric(1))) %>%
    dplyr::ungroup()
  for (aid in unique(ef$activity[ef$rank <= 3])) {
    p <- file.path(strava, "details", paste0(aid, ".json"))
    det <- jsonlite::read_json(p)
    for (i in seq_along(det$segment_efforts)) {
      r <- ef$rank[ef$id == det$segment_efforts[[i]]$id]
      if (length(r) && r <= 3) det$segment_efforts[[i]]$pr_rank <- r
    }
    wj(det, p)
  }
  # The full segment files: totals and the PR stats
  for (s in unique(ef$segment)) {
    cl <- climbs_cfg[climbs_cfg$segment_id == s, ]
    mine <- ef[ef$segment == s, ]
    best <- mine[which.min(mine$elapsed), ]
    wj(list(id = as.numeric(s), total_elevation_gain = cl$gain_m, starred = TRUE, athlete_count = 1500,
            effort_count = 9000, athlete_segment_stats = list(pr_elapsed_time = best$elapsed,
                                                              pr_date = format(best$date), effort_count = nrow(mine)),
            map = list(polyline = NULL)),
       file.path(strava, "segments", paste0(s, ".json")))
  }
  wj(summaries, file.path(strava, paste0(format(DEMO$as_of, "%Y-%m-%d"), "T120000Z_strava_activities.json")))
  dir.create(file.path(strava, "segments"), showWarnings = FALSE)
  c(rides, run_days, bjj)
}

# ── Lifting: Strong backfill, then MacroFactor ───────────────────────────────
# e1RM on a plateau curve per lift; a stall during the cut
true_e1rm <- function(lift, d) {
  p <- list(squat = c(100, 145), bench = c(75, 102), deadlift = c(125, 172))[[lift]]
  t <- pmin(years_in(d), years_in(DEMO$cut_from) + 0.1 * pmax(0, years_in(d) - years_in(DEMO$cut_from)))
  p[2] - (p[2] - p[1]) * exp(-t / 1.1)
}
demo_sets <- function() {
  d <- seq(DEMO$start + 6, DEMO$as_of, by = "day")
  dow <- format(d, "%u")
  d <- d[dow %in% c("1", "3", "5") & runif(length(d)) < 0.88]
  plan <- list(
    "1" = list(workout = "Day A", lifts = list(c("Barbell Back Squat", "squat", 1, 5, 3), c("Barbell Bench Press", "bench", 1, 5, 3),
                                               c("Barbell Romanian Deadlift", "deadlift", 0.72, 8, 3))),
    "3" = list(workout = "Day B", lifts = list(c("Conventional Deadlift", "deadlift", 1, 3, 3), c("Pause Barbell Bench Press", "bench", 0.9, 5, 3),
                                               c("Front Squat", "squat", 0.8, 5, 3))),
    "5" = list(workout = "Day C", lifts = list(c("Barbell Back Squat", "squat", 1, 8, 3), c("Barbell Bench Press", "bench", 1, 8, 4)))
  )
  rows <- list()
  for (k in seq_along(d)) {
    day <- plan[[format(d[k], "%u")]]
    for (l in day$lifts) {
      e1 <- true_e1rm(l[2], d[k]) * as.numeric(l[3]) * (1 + rnorm(1, 0, 0.02))
      reps <- as.integer(l[4]); rir <- sample(1:3, 1, prob = c(0.3, 0.5, 0.2))
      top <- 2.5 * round(e1 / (1 + (reps + rir) / 30) / 2.5)
      main <- as.numeric(l[3]) == 1
      warm <- if (main) c(0.5, 0.7) * top else numeric(0)
      rows[[length(rows) + 1]] <- tibble::tibble(
        Date = d[k], Workout = day$workout, Exercise = l[1],
        `Set Type` = c(rep("Warm-Up Set", length(warm)), rep("Standard Set", as.integer(l[5]))),
        `Weight (kg)` = c(2.5 * round(warm / 2.5), rep(top, as.integer(l[5]))),
        Reps = c(rep(5L, length(warm)), rep(reps, as.integer(l[5]))),
        RIR = c(rep(5, length(warm)), pmax(0, rir - c(0, 0, 1, 1)[seq_len(as.integer(l[5]))])))
    }
  }
  dplyr::bind_rows(rows)
}
write_backfill <- function(sets, root) {
  bf <- file.path(root, "data/landing/backfill")
  cols <- function(x) tibble::tibble(
    Date = x$Date, `Workout Duration` = 3900, Workout = x$Workout, Exercise = x$Exercise,
    `Exercise Base Weight (kg)` = NA_real_, `Set Type` = x$`Set Type`, `Weight (kg)` = x$`Weight (kg)`,
    Reps = x$Reps, RIR = x$RIR, Duration = NA_character_, `Distance short (m)` = NA_real_, `Distance long (km)` = NA_real_,
    Source = "strong", `Source Workout` = x$Workout, `Source Exercise` = x$Exercise, `Exercise Match` = "exact",
    `RIR Basis` = "logged", `Date Confidence` = "exact", `Date Earliest` = x$Date, `Date Latest` = x$Date,
    Program = "Example Strength Program", `Program Week` = NA_character_, `Source Ref` = NA_character_)
  wcsv(cols(sets), file.path(bf, "strong_workout_log.csv"))
  wcsv(cols(sets[0, ]), file.path(bf, "drive_templates_workout_log.csv"))
}

# ── Config: the example twins, with the edits a self-consistent demo needs ──
write_demo_config <- function(root, repo = ".") {
  cfg <- file.path(root, "config")
  dir.create(file.path(cfg, "relics"), recursive = TRUE, showWarnings = FALSE)
  for (f in c("athlete.yml", "oath.yml", "climbs.csv", "bikes.csv", "lifts.csv", "programs.csv", "relic_events.csv")) {
    ex <- sub("\\.(yml|csv)$", ".example.\\1", f)
    file.copy(file.path(repo, "config", ex), file.path(cfg, f), overwrite = TRUE)
  }
  # goals: the squat round starts from the data, not the example's invented override
  g <- readLines(file.path(repo, "config", "goals.example.yml"))
  i <- grep("^  start_override:", g)
  if (length(i)) g <- g[-c(i, i + 1)]
  writeLines(g, file.path(cfg, "goals.yml"))
  # Overrides: none (the examples point at rows that only exist in their own story)
  for (f in c("bike_overrides", "effort_overrides", "lifting_overrides")) {
    h <- readLines(file.path(repo, "config", paste0(f, ".example.csv")), n = 1)
    writeLines(h, file.path(cfg, paste0(f, ".csv")))
  }
  invisible(cfg)
}

make_demo_landing <- function(root, repo = ".") {
  set.seed(DEMO$seed)
  write_demo_config(root, repo)
  climbs_cfg <- utils::read.csv(file.path(root, "config", "climbs.csv"), stringsAsFactors = FALSE,
                                colClasses = c(segment_id = "character"))
  message("demo: Strava")
  active <- demo_strava(root, climbs_cfg)
  message("demo: Fitbit and Garmin")
  days <- demo_days()
  write_fitbit(days, active, root)
  write_garmin(days, active, root)
  message("demo: lifting and MacroFactor")
  sets <- demo_sets()
  write_backfill(dplyr::filter(sets, Date < DEMO$mf_from), root)
  mf_days <- seq(DEMO$mf_from, DEMO$as_of, by = "day")
  make_macrofactor_fixture(file.path(root, "data/landing/macrofactor"), start = DEMO$mf_from,
                           days = length(mf_days), seed = DEMO$seed, trend = trend_weight(mf_days),
                           sets = dplyr::filter(sets, Date >= DEMO$mf_from))
  invisible(root)
}
