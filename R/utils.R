#' Generate a sequence of dates
#' @param start Start date (Date or character YYYY-MM-DD)
#' @param end End date (Date or character YYYY-MM-DD)
#' @return Character vector of dates in YYYY-MM-DD format
#' @noRd
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

#' Validate that a start/end date pair is sensible
#' @noRd
validate_date_range <- function(start, end, label = NULL) {
  s <- tryCatch(as.Date(start), error = function(e) NA)
  e <- tryCatch(as.Date(end),   error = function(e) NA)
  prefix <- if (!is.null(label)) paste0(label, ": ") else ""
  if (is.na(s)) cli::cli_abort("{prefix}{.arg start} is not a valid date: {.val {start}}")
  if (is.na(e)) cli::cli_abort("{prefix}{.arg end} is not a valid date: {.val {end}}")
  if (s > e) cli::cli_abort("{prefix}{.arg start} ({start}) must be before {.arg end} ({end})")
}

#' Split a sorted Date vector into contiguous runs
#'
#' Returns a list of Date vectors, each representing a contiguous
#' block of consecutive days. Used to avoid re-fetching cached gaps.
#' @noRd
split_contiguous <- function(dates) {
  dates <- sort(dates)
  gaps <- which(diff(dates) > 1)
  starts <- c(1L, gaps + 1L)
  ends <- c(gaps, length(dates))
  mapply(function(s, e) dates[s:e], starts, ends, SIMPLIFY = FALSE)
}
