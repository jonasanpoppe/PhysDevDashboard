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
