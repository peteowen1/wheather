#' Generate a sequence of dates
#'
#' @param start Start date (Date or character YYYY-MM-DD)
#' @param end End date (Date or character YYYY-MM-DD)
#' @return Character vector of dates in YYYY-MM-DD format
#' @export
date_seq <- function(start, end) {
  start <- as.Date(start)
  end <- as.Date(end)
  format(seq.Date(start, end, by = "day"), "%Y-%m-%d")
}

#' Round coordinates for cache key consistency
#' @noRd
round_coords <- function(lat, lon, digits = 2) {
  list(lat = round(lat, digits), lon = round(lon, digits))
}
