library(dplyr)

# Needs project_xy() from R/geo.R.

# Seeds the climb library once from Strava segments, for review by hand.
# Not part of the pipeline: run it, review the output, save the kept rows as
# config/climbs.csv (see config/climbs.example.csv).
#
# 1. Candidates: ride segments in `countries`, not flagged elev_suspect, that
#    look like a climb (grade, gain, length) and that you have ridden.
# 2. Segments whose start AND end lie within `same_m` of each other describe
#    the same climb (e.g. the classic segment and a "from the river" one);
#    they share a `group`. Every segment keeps its own row, so the review can
#    pick any variant.
# 3. `suggest_keep` pre-marks groups where a name looks like a named climb
#    (Strava climb category, or a word like "berg", "muur", "kwaremont").
#    It is a hint for the review, nothing more.
# 4. Rows are sorted so a group's variants sit together, longest first, and
#    groups by the first word of their best-known name.

seed_climb_candidates <- function(
    segments, efforts,
    countries  = c("Belgium", "België", "Belgique"),
    min_grade  = 3,      # %
    min_gain   = 15,     # m
    min_length = 150,    # m
    max_length = 4000,   # m
    same_m     = 150     # m: start and end both this close = same climb
) {
  rides <- efforts %>%
    summarise(.by = segment_id,
              rides = n_distinct(activity_id),
              first_ridden = min(date), last_ridden = max(date))

  cand <- segments %>%
    inner_join(rides, by = "segment_id") %>%
    filter(
      activity_type == "Ride", country %in% countries, !elev_suspect,
      avg_grade_pct >= min_grade, elev_gain_m >= min_gain,
      distance_m >= min_length, distance_m <= max_length
    ) %>%
    arrange(desc(rides), desc(distance_m))

  cand$group <- group_same_climb(cand, same_m)

  cand %>%
    mutate(
      .by = group,
      group_size   = n(),
      # The most-ridden variant names the group (cand is sorted that way)
      group_key    = climb_name_key(first(name)),
      suggest_keep = any(climb_category > 0 | looks_like_named_climb(name))
    ) %>%
    transmute(
      keep = NA,               # to fill in: TRUE on one row per climb
      surface = NA_character_, # to fill in: cobbles / part_cobbles / asphalt
      name, segment_id = as.character(segment_id),
      group, group_size, group_key, suggest_keep,
      start_lat, start_lon = start_lng, end_lat, end_lon = end_lng,
      length_m = round(distance_m), gain_m = round(elev_gain_m),
      avg_grade_pct, max_grade_pct, climb_category,
      rides, first_ridden, last_ridden
    ) %>%
    arrange(desc(suggest_keep), group_key, group, desc(length_m))
}

# Single-linkage grouping on start and end points, in projected metres
# (equirectangular around each pair's latitude; fine at these distances).
group_same_climb <- function(cand, same_m) {
  n <- nrow(cand)
  group <- seq_len(n)
  near <- function(lat1, lon1, lat2, lon2) {
    dx <- (lon1 - lon2) * 111320 * cos(lat1 * pi / 180)
    dy <- (lat1 - lat2) * 111320
    sqrt(dx^2 + dy^2) < same_m
  }
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      if (near(cand$start_lat[i], cand$start_lng[i], cand$start_lat[j], cand$start_lng[j]) &&
          near(cand$end_lat[i], cand$end_lng[i], cand$end_lat[j], cand$end_lng[j])) {
        group[group == group[j]] <- group[i]
      }
    }
  }
  group
}

climb_name_key <- function(name) {
  # (*UCP): \w also matches accented letters ("Côte")
  tolower(sub("(*UCP)^\\W*(\\w+).*$", "\\1", name, perl = TRUE))
}

looks_like_named_climb <- function(name) {
  grepl("berg|muur|kwaremont|dries|helling|kouter|houppe|c[oô]te|mont\\b",
        name, ignore.case = TRUE)
}

# ── First pick: one segment per climb, proposed for review ──────────────────
#
# Partial segments ("Vesten + Muur", "last 300 m") start or end elsewhere, so
# group_same_climb() keeps them apart. propose_climb_picks() joins them by
# route (decoded Strava polylines) and picks one segment per climb:
#
# - Segments are ranked by name: fewest "partial" words (via, tot, first,
#   last, +, …); then a bare climb name ("Koppenberg", "Muur van
#   Geraardsbergen": letters only, at most three words, no "full"), which is
#   usually the classic segment the climb is known by; then a "full climb"
#   word; then most rides; then longest.
# - Going down that ranking, a segment joins the first anchor it shares a
#   route with: >= contained_frac of the shorter one lies on the longer one.
#   Otherwise it becomes an anchor (a climb) itself.
# - Greedy anchoring, not single linkage: a long segment over two climbs
#   joins one of them and cannot chain them together.
# The result is a proposal: config/climbs.csv gets reviewed = FALSE.
# It's a heuristic on how people name Strava segments, not a rule: it gets
# the well-known climbs right and needs a human for the rest.

climb_partial_words <- "\\bvia\\b|part|first|last|\\btot\\b|\\bto\\b|until|depuis|\\bfrom\\b|\\+|&| - |sprint|\\bkom\\b|cup|segment|half|\\btop\\b|bottom|voet|start|finish|down|descent|afdaling|achterkant|\\(|[^[:ascii:]]"
climb_full_words <- "full|volledig|complete|entire"

propose_climb_picks <- function(candidates, polylines, on_route_m = 25,
                                contained_frac = 0.8, name_overlap_frac = 0.3) {
  cand <- candidates %>%
    filter(suggest_keep) %>%
    left_join(polylines, by = "segment_id") %>%
    mutate(
      partial = lengths(regmatches(name, gregexpr(climb_partial_words, name,
                                                  ignore.case = TRUE, perl = TRUE))),
      is_full = grepl(climb_full_words, name, ignore.case = TRUE),
      is_bare = partial == 0 & !is_full &
        grepl("^[[:alpha:]' ]+$", trimws(name)) &
        lengths(strsplit(trimws(name), " +")) <= 3
    ) %>%
    arrange(partial, desc(is_bare), desc(is_full), desc(rides), desc(length_m))
  pts <- lapply(cand$polyline, decode_polyline)

  anchor <- integer(nrow(cand))
  anchors <- integer()
  for (k in seq_len(nrow(cand))) {
    joined <- FALSE
    for (a in anchors) {
      long  <- if (cand$length_m[a] >= cand$length_m[k]) a else k
      short <- if (long == a) k else a
      if (cand$group[a] == cand$group[k] ||
          share_on_route(pts[[short]], pts[[long]], on_route_m) >= contained_frac) {
        anchor[k] <- a
        joined <- TRUE
        break
      }
    }
    if (!joined) {
      anchor[k] <- k
      anchors <- c(anchors, k)
    }
  }

  # Second pass: anchors that name the same climb ("Bosberg", "great bosberg
  # climb") and overlap on the road (>= name_overlap_frac of the shorter one)
  # merge into the better-ranked one.
  for (b in rev(anchors)) {
    for (a in anchors[anchors < b]) {
      if (anchor[a] != a || anchor[b] != b) next
      long  <- if (cand$length_m[a] >= cand$length_m[b]) a else b
      short <- if (long == a) b else a
      if (length(intersect(climb_words(cand$name[a]), climb_words(cand$name[b]))) > 0 &&
          share_on_route(pts[[short]], pts[[long]], on_route_m) >= name_overlap_frac) {
        anchor[anchor == b] <- a
        break
      }
    }
  }

  # The pick, per cluster: the climb word most of its names share (e.g.
  # "koppenberg"); a segment named exactly that wins, then a bare name that
  # contains it, then the ranking above.
  cand %>%
    mutate(cluster = anchor, rank = row_number()) %>%
    mutate(
      .by = cluster,
      top_word = cluster_top_word(name),
      exact    = tolower(trimws(name)) == top_word,
      has_word = vapply(name, \(nm) top_word[1] %in% climb_words(nm), logical(1))
    ) %>%
    arrange(cluster, desc(exact), desc(has_word & is_bare), rank) %>%
    mutate(
      .by = cluster,
      pick = row_number() == 1,
      cluster_size = n(),
      cluster_names = paste(name, collapse = " ¦ ")
    ) %>%
    select(-polyline, -rank, -exact, -has_word)
}

# Distinctive words of a segment name: 5+ letters, lowercased, minus the
# generic ones. "great bosberg climb" -> "bosberg".
climb_words <- function(name) {
  w <- tolower(unlist(regmatches(name, gregexpr("(*UCP)\\w{5,}", name, perl = TRUE))))
  setdiff(w, c("climb", "great", "full", "volledig", "kasseien", "steilste",
               "stuk", "start", "sprint", "ronde", "vlaanderen", "hellingen",
               "cotacol", "cotacolhunting", "uitloper", "campus", "ineos",
               "challenge", "national", "cycling", "wall"))
}

# The most frequent distinctive word over a cluster's names (ties: longer).
cluster_top_word <- function(names) {
  w <- unlist(lapply(names, \(n) unique(climb_words(n))))
  if (length(w) == 0) return(NA_character_)
  tab <- table(w)
  best <- names(tab)[tab == max(tab)]
  best[which.max(nchar(best))]
}

# Google encoded polyline -> matrix of lat, lon.
decode_polyline <- function(enc) {
  if (is.na(enc) || !nzchar(enc)) return(matrix(numeric(), ncol = 2))
  bytes <- utf8ToInt(enc) - 63L
  vals <- numeric()
  acc <- 0; shift <- 0
  for (b in bytes) {
    acc <- acc + bitwAnd(b, 31L) * 2^shift
    if (b < 32L) {
      v <- if (acc %% 2 == 1) -(acc + 1) / 2 else acc / 2
      vals <- c(vals, v)
      acc <- 0; shift <- 0
    } else {
      shift <- shift + 5
    }
  }
  lat <- cumsum(vals[c(TRUE, FALSE)]) / 1e5
  lon <- cumsum(vals[c(FALSE, TRUE)]) / 1e5
  cbind(lat, lon)
}

# Share of the points of route b that lie within `tol_m` of route a
# (distance to a's line segments, in projected metres).
share_on_route <- function(b, a, tol_m) {
  if (nrow(a) < 2 || nrow(b) == 0) return(0)
  lat0 <- a[1, 1]; lon0 <- a[1, 2]
  pa <- project_xy(a[, 1], a[, 2], lat0, lon0)
  pb <- project_xy(b[, 1], b[, 2], lat0, lon0)
  # quick reject: bounding boxes more than tol apart
  if (min(pb$x) > max(pa$x) + tol_m || max(pb$x) < min(pa$x) - tol_m ||
      min(pb$y) > max(pa$y) + tol_m || max(pb$y) < min(pa$y) - tol_m) return(0)
  x1 <- pa$x[-length(pa$x)]; y1 <- pa$y[-length(pa$y)]
  x2 <- pa$x[-1];            y2 <- pa$y[-1]
  dx <- x2 - x1; dy <- y2 - y1
  len2 <- pmax(dx^2 + dy^2, 1e-9)
  near <- vapply(seq_along(pb$x), \(k) {
    t <- pmin(1, pmax(0, ((pb$x[k] - x1) * dx + (pb$y[k] - y1) * dy) / len2))
    min((x1 + t * dx - pb$x[k])^2 + (y1 + t * dy - pb$y[k])^2) <= tol_m^2
  }, logical(1))
  mean(near)
}
