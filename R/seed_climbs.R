library(dplyr)

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
