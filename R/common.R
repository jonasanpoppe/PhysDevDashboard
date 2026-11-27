# Shared helpers: one definition each, used by the pipeline, the app and the
# tests (the Home tab, Reliquary, Scenarios and Weight tiles all print and
# count the same way). `%||%` comes from base R (4.4 and later).

# Numbers with a thousands separator: 2,000 and 82.4
fmt_num <- function(x, digits = 0) {
  formatC(round(x, digits), format = "f", digits = digits, big.mark = ",")
}
# Targets and milestones: whole numbers without decimals (98, not 98.0)
fmt_goal <- function(x, digits = 0) fmt_num(x, if (isTRUE(all.equal(x, round(x)))) 0 else digits)
# A plan number as people write it: 2,000 / 75 / 0.4 / 1.6 (no trailing zeros)
fmt_plain <- function(x) {
  ifelse(isTRUE(all.equal(x, round(x))) & abs(x - round(x)) < 1e-9, fmt_num(x, 0),
         sub("\\.?0+$", "", formatC(x, format = "f", digits = 2, big.mark = ",")))
}
# Days as "1 May 2027"; `na` for a missing date ("never" for a projection
# that never gets there)
fmt_day <- function(d, na = NA_character_) {
  out <- sub("^0", "", format(as.Date(d), "%d %b %Y"))
  out[is.na(d)] <- na
  out
}

# Sessions of `kind` a week over the `days` up to as_of: the one count behind
# the Home card, the Scenarios and the Weight tiles
sessions_per_week <- function(sessions, kind, as_of, days = 28) {
  d <- as.Date(sessions$date)
  sum(sessions$kind == kind & d > as.Date(as_of) - days & d <= as.Date(as_of)) / (days / 7)
}
