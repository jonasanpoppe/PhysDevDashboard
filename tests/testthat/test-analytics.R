source(testthat::test_path("..", "..", "R", "analytics.R"))

test_that("a homogenised series sits beside the eras, marked as a model output, never a preset's pick", {
  vit <- tibble::tibble(date = as.Date("2030-01-01") + 0:1, metric = "rhr",
                        era = c("fitbit", "garmin"), value = c(60, 50))
  hom <- tibble::tibble(date = as.Date("2030-01-01") + 0:1, metric = "rhr", era = "homogenised",
                        source_era = c("fitbit", "garmin"), value = c(52, 50),
                        value_low = c(51, 50), value_high = c(53, 50))
  empty <- function(...) tibble::tibble(date = as.Date(character()), ...)
  d <- build_analytics_daily(
    tibble::tibble(date = as.Date("2030-01-01"), scale_kg = 80, trend_kg = 80, kcal = NA,
                   protein_g = NA, steps = NA, expenditure_kcal = NA),
    vit,
    tibble::tibble(date = as.Date("2030-01-01") + 0:1, ride_h = 0, run_h = 0, ride_6w_hpw = 0,
                   run_6w_hpw = 0, fitness_h = 0, fatigue_h = 0),
    empty(kind = character()), empty(z2_s = numeric(), sweet_spot_s = numeric()),
    tibble::tibble(date = as.Date("2030-01-01"), group = "Bench", main = TRUE, e1rm = 100),
    tibble::tibble(date = as.Date("2030-01-01"), status = "clean", short = FALSE, wkg = 4),
    vitals_homogenised = hom)
  expect_equal(d$value[d$variable == "rhr" & d$era == "homogenised"], c(52, 50))
  expect_equal(d$value[d$variable == "rhr" & d$era == "fitbit"], 60)         # original untouched
  s <- analytics_series(d, analytics_variables(d))
  expect_setequal(s$series[s$variable == "rhr"], c("rhr@fitbit", "rhr@garmin", "rhr@homogenised"))
  expect_equal(s$kind[s$series == "rhr@homogenised"], "model output")
  expect_equal(s$kind[s$series == "rhr@garmin"], "measurement")
  expect_equal(resolve_series(s, "rhr"), "rhr@garmin")
})

test_that("build_analytics_daily: eras kept, sleep in hours, volumes 0 on rest days", {
  wd <- tibble::tibble(date = as.Date("2030-01-01") + 0:2, scale_kg = c(80, NA, 79.8), trend_kg = 80,
                       kcal = 2500, protein_g = 150, steps = 8000, expenditure_kcal = 2700)
  vit <- tibble::tibble(date = as.Date(c("2030-01-01", "2030-01-02", "2030-01-02")),
                        metric = c("hrv", "hrv", "sleep_s"), era = c("fitbit", "garmin", "garmin"),
                        value = c(50, 60, 7.5 * 3600))
  load <- tibble::tibble(date = as.Date("2030-01-01") + 0:2, ride_h = c(1, 0, 2), run_h = 0,
                         ride_6w_hpw = 3, run_6w_hpw = 1, fitness_h = 0.5, fatigue_h = 0.6)
  sess <- tibble::tibble(date = as.Date("2030-01-02"), kind = "bjj", source = "strava")
  bands <- tibble::tibble(activity_id = c("1", "2"), date = as.Date(c("2030-01-01", "2030-01-03")),
                          z2_s = c(1800, 3600), sweet_spot_s = 0, above_cap_s = 0, hr_s = 3600)
  ss <- tibble::tibble(date = as.Date("2030-01-03"), group = c("Bench", "Bench", "Curls"),
                       main = c(TRUE, TRUE, FALSE), e1rm = c(100, 102, 40))
  eff <- tibble::tibble(date = as.Date("2030-01-03"), status = c("clean", "early", "clean"),
                        short = c(FALSE, FALSE, TRUE), wkg = c(4, 5, 6))

  d <- build_analytics_daily(wd, vit, load, sess, bands, ss, eff)
  get <- function(v, e = "all") d$value[d$variable == v & d$era == e]

  expect_equal(get("scale_kg"), c(80, 79.8))                  # missing days stay missing
  expect_equal(get("hrv", "fitbit"), 50)
  expect_equal(get("hrv", "garmin"), 60)
  expect_equal(get("sleep_h", "garmin"), 7.5)
  expect_equal(get("load_6w_hpw"), c(4, 4, 4))
  expect_equal(get("bjj"), c(0, 1, 0))                         # zero-filled over the span
  expect_equal(get("lifting"), c(0, 0, 0))
  expect_equal(get("z2_h"), c(0.5, 0, 1))
  expect_equal(get("e1rm_bench"), 102)                         # main lifts only, best of the day
  expect_false(any(startsWith(d$variable, "e1rm_curls")))
  expect_equal(get("wkg"), 4)                                  # clean or top, not short

  v <- analytics_variables(d, e1rm_groups = c("Bench"))
  expect_equal(v$label[v$variable == "e1rm_bench"], "e1RM: Bench")
  expect_false("spo2" %in% v$variable)                         # absent variables dropped
  s <- analytics_series(d, v)
  expect_setequal(s$series[s$variable == "hrv"], c("hrv@fitbit", "hrv@garmin"))
  expect_equal(s$series_label[s$series == "hrv@garmin"], "HRV · garmin")
  expect_equal(s$series[s$variable == "bjj"], "bjj")
})

test_that("smoothing: 7-day mean needs 4 values, volumes sum, weekly runs Monday to Sunday", {
  days <- as.Date("2030-01-07") + 0:13                         # 2030-01-07 is a Monday
  lv <- tibble::tibble(date = days, value = seq_along(days))
  m <- smooth_series(lv, "level", "7d")
  expect_equal(m$date[1], days[4])                             # first day with 4 values
  expect_equal(m$value[m$date == days[7]], mean(1:7))
  gappy <- lv[c(1, 2, 3, 10, 11, 12, 13, 14), ]
  g <- smooth_series(gappy, "level", "7d")
  expect_false(any(g$date %in% days[4:12]))                    # under 4 values in the window

  vol <- tibble::tibble(date = days, value = rep(c(1, 0), 7))
  expect_equal(smooth_series(vol, "volume", "7d")$value[1], 4) # 1+0+1+0+1+0+1
  w <- smooth_series(vol, "volume", "weekly")
  expect_equal(w$date, days[c(1, 8)])
  expect_equal(w$value, c(4, 3))
  expect_equal(smooth_series(lv, "level", "weekly")$value, c(4, 11))

  sp <- tibble::tibble(date = days[c(2, 9)], value = c(100, 110))
  expect_equal(nrow(smooth_series(sp, "sparse", "7d")), 8)     # every day with a value in the last 7
})

test_that("pairs: the lag finds the delayed relationship", {
  d <- fake_analytics_daily()
  s <- analytics_series(d, analytics_variables(d))
  rho_at <- function(lag) {
    p <- analytics_pair(d, s, "scale_kg", "rhr@garmin", lag = lag, smooth = "daily")
    cor(p$x, p$y, method = "spearman")
  }
  expect_gt(rho_at(3), 0.95)
  expect_gt(rho_at(3), rho_at(0))
  p <- analytics_pair(d, s, "scale_kg", "rhr@garmin", lag = 3, smooth = "daily")
  expect_true(all(p$date >= as.Date("2030-06-01") - 3))        # garmin era only
  # weekly: the lag rounds to whole weeks
  pw <- analytics_pair(d, s, "scale_kg", "steps", lag = 9, smooth = "weekly")
  pw1 <- analytics_pair(d, s, "scale_kg", "steps", lag = 7, smooth = "weekly")
  expect_equal(pw, pw1)
  expect_error(analytics_pair(d, s, "hrv", "rhr@garmin"), "unknown series")
})

test_that("fit stats: rho and the line's slope, each with an interval; none for tiny samples", {
  d <- fake_analytics_daily()
  s <- analytics_series(d, analytics_variables(d))
  p <- analytics_pair(d, s, "sleep_h@fitbit", "hrv@fitbit", smooth = "daily")
  r <- analytics_fit_stats(p$x, p$y, block = 14, reps = 300)
  expect_gt(r$rho, 0.8)
  expect_true(r$rho_low <= r$rho && r$rho <= r$rho_high)
  expect_equal(r$effect, unname(coef(lm(y ~ x, p))[2]))       # the line in the scatter
  expect_true(r$effect_low <= r$effect && r$effect <= r$effect_high)
  expect_equal(r$effect, 8, tolerance = 0.1)                    # hrv = 40 + 8 * sleep + noise
  expect_equal(r$r2, summary(lm(y ~ x, p))$r.squared)
  noise <- analytics_pair(d, s, "steps", "scale_kg", smooth = "7d")
  rn <- analytics_fit_stats(noise$x, noise$y, reps = 300)
  expect_true(rn$rho_high - rn$rho_low > 0.1)
  tiny <- analytics_fit_stats(1:5, c(2, 1, 4, 3, 5))
  expect_equal(tiny$n, 5)
  expect_true(is.na(tiny$rho_low) && is.na(tiny$effect_low))
  expect_identical(analytics_fit_stats(p$x, p$y, reps = 100), analytics_fit_stats(p$x, p$y, reps = 100))
})

test_that("fits: log per doubling, quadratic turning point, loess has no numbers", {
  x <- rep(c(1, 2, 4, 8, 16), 6)
  r <- analytics_fit_stats(x, 3 + 5 * log(x), fit = "log", reps = 50)
  expect_equal(r$effect, 5 * log(2))                            # per doubling of X
  expect_equal(r$r2, 1)
  q <- analytics_fit_stats(1:30, (1:30 - 12)^2, fit = "quadratic", reps = 50)
  expect_equal(q$turn, 12)
  expect_true(is.na(q$effect))
  expect_true(is.na(analytics_fit_stats(1:30, 30:1, fit = "quadratic", reps = 50)$turn))
  expect_match(analytics_fit_stats(c(0, x), c(1, x), fit = "log")$note, "above 0")
  l <- analytics_fit_stats(1:30, (1:30)^2, fit = "loess", reps = 50)
  expect_equal(l$rho, 1)
  expect_true(is.na(l$r2) && is.na(l$effect))
  expect_error(analytics_fit_stats(1:30, 1:30, fit = "cubic"))
})

test_that("matrix: no variable against itself or its family, eras never pooled, thin pairs blank", {
  d <- fake_analytics_daily()
  d <- dplyr::bind_rows(d, dplyr::filter(d, variable == "scale_kg") %>% dplyr::mutate(variable = "trend_kg"))
  s <- analytics_series(d, analytics_variables(d))
  m <- analytics_matrix(d, s, lag = 0, smooth = "daily")
  expect_true(all(is.na(m$rho[m$x == m$y])))
  expect_true(is.na(m$rho[m$x == "scale_kg" & m$y == "trend_kg"]))          # weight against weight
  expect_true(is.na(m$rho[m$x == "sleep_h@garmin" & m$y == "sleep_h@fitbit"]))
  expect_false(is.na(m$rho[m$x == "sleep_h@garmin" & m$y == "hrv@garmin"]))
  expect_true(analytics_same_family(s, "scale_kg", "trend_kg"))
  expect_false(analytics_same_family(s, "scale_kg", "steps"))
  # fitbit and garmin eras never overlap in time: no pairs, blank cell
  expect_true(is.na(m$rho[m$x == "hrv@fitbit" & m$y == "hrv@garmin"]))
  expect_equal(m$n[m$x == "hrv@fitbit" & m$y == "hrv@garmin"], 0)
  m3 <- analytics_matrix(d, s, lag = 3, smooth = "daily")
  at <- function(mm) mm$rho[mm$x == "scale_kg" & mm$y == "rhr@garmin"]
  expect_gt(at(m3), at(m))
  p <- analytics_pair(d, s, "scale_kg", "rhr@garmin", lag = 3, smooth = "daily")
  expect_equal(at(m3), cor(p$x, p$y, method = "spearman"))     # same pairing as the scatter
  late <- analytics_matrix(d, s, smooth = "daily", from = max(d$date) - 20)
  expect_true(all(is.na(late$rho)))                             # under 30 pairs
})

test_that("detrending: rolling mean, LOESS, change, weekday effect, edges and gaps", {
  days <- seq(as.Date("2025-01-01"), by = "day", length.out = 200)
  set.seed(1)
  lin <- tibble(date = days, value = 50 + 0.1 * seq_along(days) + rnorm(200, sd = 0.5))

  for (m in c("rolling", "loess")) {
    r <- detrend_series(lin, m, window = 56L)
    mid <- filter(r, !edge)
    expect_lt(abs(mean(mid$value)), 0.2)                                      # centred on 0
    expect_lt(abs(coef(lm(value ~ as.numeric(date), mid))[[2]]), 0.005)       # no slope left
    expect_equal(r$raw + 0, lin$value[match(r$date, lin$date)])               # raw kept
    expect_equal(mid$value, mid$raw - mid$trend)
  }

  # A constant leaves nothing; ends of a centred window are flagged
  const <- tibble(date = days, value = 7)
  r <- detrend_series(const, "rolling", window = 28L)
  expect_true(all(abs(r$value) < 1e-12))
  expect_equal(sum(r$edge), 2 * 7)                  # a half of the window under half filled
  expect_false(any(r$edge[8:193]))

  # Change from the day before; no trend to show
  d <- detrend_series(tibble(date = days[1:5], value = c(1, 3, 6, 10, 15)), "diff")
  expect_equal(d$value, c(2, 3, 4, 5))
  expect_true(all(is.na(d$trend)))

  # A weekday rhythm is removed when asked, daily values only
  wk <- tibble(date = days, value = 100 + ifelse(format(days, "%u") %in% c("6", "7"), 50, 0))
  expect_lt(max(abs(detrend_series(wk, "rolling", weekday = TRUE)$value)), 1e-9)
  expect_gt(max(abs(detrend_series(wk, "rolling", weekday = FALSE)$value)), 10)

  # A gap: days whose window is under half full get no value
  gappy <- filter(lin, !(date >= as.Date("2025-03-01") & date < as.Date("2025-05-01")))
  g <- detrend_series(gappy, "rolling", window = 28L)
  expect_false(any(g$date >= as.Date("2025-03-01") & g$date < as.Date("2025-05-01")))
  expect_true(all(!is.na(g$value)))
  expect_true(all(g$edge[g$date %in% (as.Date("2025-03-01") + c(-1, 60))]))   # beside the gap: lighter

  # Weekly values: the window is in weeks (56 days = 4 weeks each side, cut at the ends)
  weekly <- tibble(date = seq(as.Date("2025-01-06"), by = "week", length.out = 30), value = 1:30)
  w <- detrend_series(weekly, "rolling", window = 56L, step = 7L)
  expect_equal(sum(w$edge), 4)
  expect_equal(w$value[5:26], rep(0, 22))           # full window: a straight line leaves 0
})

test_that("detrending through the views: sparse series untouched, labels say what was removed", {
  d <- fake_analytics_daily()
  s <- analytics_series(d, analytics_variables(d))
  sp <- s[s$agg == "sparse", ][1, ]
  if (nrow(sp) == 1 && !is.na(sp$series)) {
    expect_equal(analytics_values(d, sp, "daily", "rolling")$value,
                 analytics_values(d, sp, "daily", "none")$value)
    expect_equal(analytics_detrend_label(sp, "rolling"), "")
  }
  st <- s[s$variable == "steps", ][1, ]
  expect_match(analytics_detrend_label(st, "rolling", 56L, "daily"), "weekday effect .* 56-day mean")
  expect_match(analytics_detrend_label(st, "diff", 56L, "weekly"), "week before")
  p0 <- analytics_pair(d, s, "scale_kg", "steps", smooth = "daily")
  p1 <- analytics_pair(d, s, "scale_kg", "steps", smooth = "daily", detrend = "loess")
  expect_false(isTRUE(all.equal(p0$x, p1$x[match(p0$date, p1$date)])))
  m <- analytics_matrix(d, s, smooth = "daily", detrend = "rolling", window = 28L)
  expect_equal(nrow(m), nrow(s)^2)
})
