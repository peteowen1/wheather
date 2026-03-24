#' Get the cache directory path
#'
#' Defaults to ~/.wheather/cache, configurable via
#' options(wheather.cache_dir = "...").
#'
#' @return Path string
#' @export
cache_dir <- function() {
  getOption("wheather.cache_dir", file.path(Sys.getenv("HOME"), ".wheather", "cache"))
}

#' Build the cache file path for a location
#' @noRd
cache_path <- function(lat, lon) {
  file.path(cache_dir(), sprintf("%.2f_%.2f.parquet", lat, lon))
}

#' Retrieve cached weather data for a location
#'
#' @param lat Latitude (rounded to 2 decimal places)
#' @param lon Longitude (rounded to 2 decimal places)
#' @return A data.table or NULL if not cached
#' @export
get_cached <- function(lat, lon) {
  path <- cache_path(lat, lon)
  if (!file.exists(path)) return(NULL)
  tryCatch({
    dt <- data.table::as.data.table(arrow::read_parquet(path))
    if ("date" %in% names(dt)) dt[, date := as.Date(date)]
    dt
  }, error = function(e) {
    unlink(path)
    NULL
  })
}

#' Save weather data to the local cache
#'
#' @param data A data.table of daily weather data
#' @param lat Latitude
#' @param lon Longitude
#' @export
save_to_cache <- function(data, lat, lon) {
  path <- cache_path(lat, lon)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(data, path)
}

#' Clear the entire cache or cache for a specific location
#'
#' @param lat Optional latitude — if provided with lon, only clears that location
#' @param lon Optional longitude
#' @export
clear_cache <- function(lat = NULL, lon = NULL) {
  if (!is.null(lat) && !is.null(lon)) {
    target <- cache_path(lat, lon)
    if (file.exists(target)) {
      unlink(target)
      cli::cli_alert_success("Cache cleared: {.path {target}}")
    } else {
      cli::cli_alert_info("No cache found at {.path {target}}")
    }
  } else {
    target <- cache_dir()
    if (dir.exists(target)) {
      unlink(target, recursive = TRUE)
      cli::cli_alert_success("Cache cleared: {.path {target}}")
    } else {
      cli::cli_alert_info("No cache found at {.path {target}}")
    }
  }
}
