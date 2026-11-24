# Metres east (x) and north (y) of an origin: equirectangular projection
# around the origin's latitude, accurate to well under a metre over a few km.
project_xy <- function(lat, lon, lat0, lon0) {
  list(
    x = (lon - lon0) * 111320 * cos(lat0 * pi / 180),
    y = (lat - lat0) * 111320
  )
}
