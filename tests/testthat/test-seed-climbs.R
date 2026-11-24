source(testthat::test_path("..", "..", "R", "geo.R"))
source(testthat::test_path("..", "..", "R", "seed_climbs.R"))

seg <- function(id, name, lat, lon, len = 800, grade = 6, gain = 50,
                country = "Belgium", suspect = FALSE) {
  tibble::tibble(
    segment_id = id, name = name, activity_type = "Ride", country = country,
    elev_suspect = suspect, avg_grade_pct = grade, max_grade_pct = grade + 5,
    elev_gain_m = gain, distance_m = len, climb_category = 0,
    start_lat = lat, start_lng = lon, end_lat = lat + 0.005, end_lng = lon
  )
}

test_that("variants of one climb share a group, every segment keeps its row", {
  segments <- dplyr::bind_rows(
    seg(1, "Example Berg", 50.80, 3.80),
    seg(2, "Example Berg from the river", 50.8005, 3.80, len = 900),  # ~55 m apart
    seg(3, "Flat road", 50.70, 3.70, grade = 0.5),                   # not a climb
    seg(4, "Suspect Hill", 50.60, 3.60, suspect = TRUE),             # flagged
    seg(5, "Unridden Berg", 50.50, 3.50),                           # no effort
    seg(6, "Kortestraat", 50.40, 3.40)                               # climb, no climb name
  )
  efforts <- tibble::tibble(
    segment_id = c(1, 1, 2, 3, 4, 6), activity_id = c(10, 11, 10, 10, 10, 12),
    date = as.Date("2024-05-01")
  )
  out <- seed_climb_candidates(segments, efforts)
  expect_setequal(out$segment_id, c("1", "2", "6"))
  hill <- dplyr::filter(out, segment_id %in% c("1", "2"))
  expect_equal(length(unique(hill$group)), 1)
  expect_equal(unique(hill$group_size), 2L)
  expect_true(all(hill$suggest_keep))
  expect_false(out$suggest_keep[out$segment_id == "6"])
  expect_equal(out$segment_id[1:2], c("2", "1"))   # suggested first, longest first
  expect_true(all(is.na(out$keep)))
})

test_that("name key is the first word, accents included", {
  expect_equal(climb_name_key(c("Muur - Kapelmuur", "  Bosberg (last 300m)", "Côte de X")),
               c("muur", "bosberg", "côte"))
})

# Google polyline encoder, for synthetic routes (the reverse of decode_polyline)
encode_polyline <- function(lat, lon) {
  enc1 <- function(v) {
    v <- if (v < 0) bitwNot(bitwShiftL(v, 1L)) else bitwShiftL(v, 1L)
    out <- integer()
    while (v >= 32L) { out <- c(out, bitwOr(bitwAnd(v, 31L), 32L) + 63L); v <- bitwShiftR(v, 5L) }
    intToUtf8(c(out, v + 63L))
  }
  la <- round(lat * 1e5); lo <- round(lon * 1e5)
  dla <- diff(c(0L, la)); dlo <- diff(c(0L, lo))
  paste0(vapply(seq_along(la), \(i) paste0(enc1(as.integer(dla[i])), enc1(as.integer(dlo[i]))), ""), collapse = "")
}

test_that("polyline decoding matches Google's reference example", {
  p <- decode_polyline("_p~iF~ps|U_ulLnnqC_mqNvxq`@")
  expect_equal(unname(p[, 1]), c(38.5, 40.7, 43.252))
  expect_equal(unname(p[, 2]), c(-120.2, -120.95, -126.453))
  q <- decode_polyline(encode_polyline(c(50.8, 50.801, 50.8025), c(3.8, 3.8012, 3.79)))
  expect_equal(unname(q[, 1]), c(50.8, 50.801, 50.8025))
})

test_that("a route lying on another is detected, a parallel road is not", {
  north <- function(lat0, n = 20, lon = 3.8) cbind(lat0 + (0:(n - 1)) * 0.0005, lon)
  long <- north(50.80)                       # ~1 km due north
  part <- north(50.803, n = 8)               # the middle 350 m of it
  apart <- north(50.80, lon = 3.8015)        # ~105 m east, parallel
  expect_equal(share_on_route(part, long, 25), 1)
  expect_equal(share_on_route(apart, long, 25), 0)
})

test_that("one pick per climb: the classic segment, not a partial or a long variant", {
  route <- function(lat0, n, lon = 3.8) {
    lat <- lat0 + (0:(n - 1)) * 0.0005
    list(lat = lat, lon = rep(lon, n))
  }
  mk <- function(id, name, r, rides = 1) tibble::tibble(
    segment_id = id, name = name, group = id, suggest_keep = TRUE, rides = rides,
    length_m = round((length(r$lat) - 1) * 55.66),
    polyline = encode_polyline(r$lat, r$lon)
  )
  segs <- dplyr::bind_rows(
    # As in the real data: the town name is the shared word, the classic
    # name is bare (<= 3 words), a short partial has more rides.
    mk("1", "Muur van Teststad", route(50.800, 20)),
    mk("2", "MUUR aantrekken", route(50.803, 8), rides = 3),
    mk("3", "Dender - Muur van Teststad via Steenstraat", route(50.799, 22)),
    mk("7", "Voet Muur van Teststad tot Kapel", route(50.8005, 12)),
    mk("4", "Otherberg", route(50.900, 15)),                            # another climb
    mk("5", "great otherberg climb", route(50.899, 18)),
    mk("6", "Otherberg", route(50.9005, 14))
  )
  cands <- dplyr::select(segs, -polyline)
  polys <- dplyr::select(segs, segment_id, polyline)
  out <- propose_climb_picks(cands, polys)
  picks <- dplyr::filter(out, pick)
  expect_equal(nrow(picks), 2)
  expect_setequal(picks$name, c("Muur van Teststad", "Otherberg"))
  expect_equal(max(out$cluster_size), 4)
})

test_that("climb words drop generic words, the top word wins per cluster", {
  expect_equal(climb_words("great bosberg climb"), "bosberg")
  expect_equal(cluster_top_word(c("Koppenberg", "koppenberg full climb", "Koppenberg kasseien")),
               "koppenberg")
})
