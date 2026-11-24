library(dplyr)

# Seeds the climb library once from Strava segments, for review by hand.
# Not part of the pipeline: run it, review the output, save the kept rows as
# config/climbs.csv (see config/climbs.example.csv).
#
# 1. Candidates: ride segments in `countries`, not flagged elev_suspect, that
#    look like a climb (grade, gain, length) and that you have ridden.
# 2. Segments whose start AND end lie within `same_m` of each other describe
#    the same climb; they form one group. The representative is the segment
#    ridden on most activities (then the longest); the others are listed in
#    `alt_segment_ids` and `alt_names`.
# 3. `suggest_keep` pre-marks groups whose name looks like a named climb
#    (Strava climb category, or a word like "berg", "muur", "kwaremont").
#    It is a hint for the review, nothing more.

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
    summarise(
      .by = group,
      # Alternates first: below, name and segment_id are overwritten by the
      # representative's value, and later expressions would see only that.
      alt_segment_ids = paste(as.character(segment_id)[-1], collapse = " "),
      alt_names       = paste(setdiff(unique(name), name[1]), collapse = " | "),
      name            = first(name),
      segment_id      = first(as.character(segment_id)),
      start_lat       = first(start_lat),
      start_lon       = first(start_lng),
      end_lat         = first(end_lat),
      end_lon         = first(end_lng),
      length_m        = round(first(distance_m)),
      gain_m          = round(first(elev_gain_m)),
      avg_grade_pct   = first(avg_grade_pct),
      max_grade_pct   = first(max_grade_pct),
      climb_category  = max(climb_category, na.rm = TRUE),
      rides           = max(rides),
      first_ridden    = min(first_ridden),
      last_ridden     = max(last_ridden)
    ) %>%
    relocate(alt_segment_ids, alt_names, .after = last_ridden) %>%
    mutate(
      suggest_keep = climb_category > 0 | looks_like_named_climb(paste(name, alt_names)),
      keep = NA,           # to fill in: TRUE / FALSE
      surface = NA_character_  # to fill in: cobbles / part_cobbles / asphalt
    ) %>%
    select(-group) %>%
    # Variants of one climb (full, partial, "last 300 m") usually share the
    # first word of their name: sort them together, longest first, so the
    # review is a matter of picking one row per climb.
    mutate(name_key = climb_name_key(name), .before = 1) %>%
    arrange(desc(suggest_keep), name_key, desc(length_m))
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
