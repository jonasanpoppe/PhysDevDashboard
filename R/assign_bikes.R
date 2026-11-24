library(dplyr)

# Which bike each ride was on, from config/bikes.csv and
# config/bike_overrides.csv (see their *.example.csv twins).
#
# A bikes.csv row claims rides in its date range (valid_from / valid_to, empty
# = open) by its `assign` rule:
#   dates    every ride in the range, whatever Strava says (a rental)
#   gear     rides with that Strava gear id
#   no_gear  rides without a gear id in Strava
# First match wins: override > dates > gear > no_gear. Two rows of the same
# rule claiming one ride, or a ride no row claims, stop the build: a wrong
# bike mass would silently skew the power model.
assign_bikes <- function(rides, bikes, overrides = NULL) {
  rides <- rides %>%
    transmute(activity_id = as.character(activity_id), date = as.Date(date),
              gear_id = na_if(as.character(gear_id), ""))
  bikes <- bikes %>%
    mutate(
      strava_gear_id = na_if(as.character(strava_gear_id), ""),
      valid_from = as.Date(valid_from), valid_to = as.Date(valid_to),
      rule_rank = match(assign, c("dates", "gear", "no_gear"))
    )
  if (anyNA(bikes$rule_rank)) {
    stop("bikes.csv: unknown assign rule: ",
         paste(unique(bikes$assign[is.na(bikes$rule_rank)]), collapse = ", "))
  }

  claims <- cross_join(rides, bikes) %>%
    filter(
      is.na(valid_from) | date >= valid_from,
      is.na(valid_to) | date <= valid_to,
      (assign == "dates") |
        (assign == "gear" & !is.na(gear_id) & gear_id == strava_gear_id) |
        (assign == "no_gear" & is.na(gear_id))
    ) %>%
    slice_min(rule_rank, n = 1, with_ties = TRUE, by = activity_id)

  ties <- claims %>% count(activity_id) %>% filter(n > 1)
  if (nrow(ties) > 0) {
    stop("bikes.csv: more than one row claims ride(s) ",
         paste(head(ties$activity_id, 5), collapse = ", "))
  }

  by_rule <- claims %>% transmute(activity_id, bike_id, bike_source = assign)
  if (!is.null(overrides) && nrow(overrides) > 0) {
    ov <- overrides %>%
      transmute(activity_id = as.character(activity_id), bike_id, bike_source = "override")
    unknown <- setdiff(ov$bike_id, bikes$bike_id)
    if (length(unknown) > 0) stop("bike_overrides.csv: unknown bike_id ", paste(unknown, collapse = ", "))
    by_rule <- by_rule %>% filter(!activity_id %in% ov$activity_id) %>% bind_rows(ov)
  }

  out <- rides %>% left_join(by_rule, by = "activity_id")
  missing <- out$activity_id[is.na(out$bike_id)]
  if (length(missing) > 0) {
    stop("No bike for ride(s) ", paste(head(missing, 5), collapse = ", "),
         if (length(missing) > 5) paste0(" and ", length(missing) - 5, " more"),
         ". Add a rule to config/bikes.csv or a row to config/bike_overrides.csv.")
  }

  # Bike attributes as valid on the ride's date (a bike can change over time)
  attrs <- bikes %>% distinct(bike_id, valid_from, valid_to, mass_kg, lowest_gear_m)
  res <- out %>%
    inner_join(attrs, by = "bike_id", relationship = "many-to-many") %>%
    filter(is.na(valid_from) | date >= valid_from, is.na(valid_to) | date <= valid_to) %>%
    distinct(activity_id, .keep_all = TRUE)
  lost <- setdiff(out$activity_id, res$activity_id)
  if (length(lost) > 0) {
    stop("bikes.csv: no row of the assigned bike covers the date of ride(s) ",
         paste(head(lost, 5), collapse = ", "))
  }
  res %>% select(activity_id, bike_id, bike_source, bike_mass_kg = mass_kg, lowest_gear_m)
}
