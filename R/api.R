#' Fetch daily weather data from Open-Meteo archive
#'
#' Uses the Open-Meteo Historical Weather API. Free, no API key required.
#' Returns daily aggregates for the full date range in a single API call.
#'
#' @param lat Latitude
#' @param lon Longitude
#' @param start Start date (YYYY-MM-DD)
#' @param end End date (YYYY-MM-DD)
#' @return A data.table with one row per day
#' @export
fetch_weather <- function(lat, lon, start, end) {
  if (lat < -90 || lat > 90) cli::cli_abort("{.arg lat} must be between -90 and 90, got {lat}")
  if (lon < -180 || lon > 180) cli::cli_abort("{.arg lon} must be between -180 and 180, got {lon}")
  coords <- round_coords(lat, lon)
  start_d <- as.Date(start)
  end_d   <- as.Date(end)

  # Check cache first
  cached <- get_cached(coords$lat, coords$lon)
  requested <- seq.Date(start_d, end_d, by = "day")

  if (!is.null(cached)) {
    cached_dates <- as.Date(cached$date)
    missing <- requested[!requested %in% cached_dates]
  } else {
    missing <- requested
  }

  if (length(missing) == 0) {
    cli::cli_alert_success("All {length(requested)} days loaded from cache")
    return(cached[as.Date(cached$date) >= start_d & as.Date(cached$date) <= end_d, ])
  }

  # Split missing dates into contiguous ranges to avoid re-fetching cached gaps
  ranges <- split_contiguous(missing)
  cli::cli_alert_info("Fetching {length(missing)} missing days in {length(ranges)} API call{?s}")

  new_parts <- lapply(ranges, function(rng) {
    fetch_open_meteo(coords$lat, coords$lon, rng[1], rng[length(rng)])
  })
  new_data <- data.table::rbindlist(new_parts, use.names = TRUE, fill = TRUE)

  # Merge with cache and save
  if (!is.null(cached)) {
    all_data <- data.table::rbindlist(list(cached, new_data), use.names = TRUE, fill = TRUE)
    all_data <- unique(all_data, by = "date")
    data.table::setorder(all_data, date)
  } else {
    all_data <- new_data
  }
  save_to_cache(all_data, coords$lat, coords$lon)

  cli::cli_alert_success("Got {length(requested)} days ({length(missing)} fetched, {length(requested) - length(missing)} cached)")
  all_data[as.Date(all_data$date) >= start_d & as.Date(all_data$date) <= end_d, ]
}

#' Call Open-Meteo Historical Weather API
#' @noRd
fetch_open_meteo <- function(lat, lon, start, end) {
  daily_vars <- paste(
    "temperature_2m_max", "temperature_2m_min", "temperature_2m_mean",
    "apparent_temperature_max", "apparent_temperature_min",
    "precipitation_sum", "precipitation_hours",
    "rain_sum", "snowfall_sum",
    "wind_speed_10m_max", "wind_gusts_10m_max",
    "relative_humidity_2m_mean",
    "shortwave_radiation_sum",
    "sunshine_duration", "daylight_duration",
    "cloud_cover_mean",
    sep = ","
  )

  resp <- httr2::request("https://archive-api.open-meteo.com/v1/archive") |>
    httr2::req_url_query(
      latitude = lat,
      longitude = lon,
      start_date = format(as.Date(start), "%Y-%m-%d"),
      end_date = format(as.Date(end), "%Y-%m-%d"),
      daily = daily_vars,
      timezone = "auto"
    ) |>
    httr2::req_retry(max_tries = 5, backoff = ~ 5 * 2^(.x - 1)) |>
    httr2::req_perform()

  json <- httr2::resp_body_json(resp)

  if (is.null(json$daily)) {
    cli::cli_abort("No daily data returned from Open-Meteo. Check coordinates and date range.")
  }

  parse_open_meteo(json)
}

#' Parse Open-Meteo JSON response into a data.table
#' @noRd
parse_open_meteo <- function(json) {
  d <- json$daily
  n <- length(d$time)

  # Helper to safely extract a vector, replacing NULL elements with NA
  safe_vec <- function(x) {
    if (is.null(x)) return(rep(NA_real_, n))
    vapply(x, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))
  }

  data.table::data.table(
    date               = as.Date(unlist(d$time)),
    temp_max           = safe_vec(d$temperature_2m_max),
    temp_min           = safe_vec(d$temperature_2m_min),
    temp_mean          = safe_vec(d$temperature_2m_mean),
    apparent_temp_max  = safe_vec(d$apparent_temperature_max),
    apparent_temp_min  = safe_vec(d$apparent_temperature_min),
    precip_total       = safe_vec(d$precipitation_sum),
    precip_hours       = safe_vec(d$precipitation_hours),
    rain_sum           = safe_vec(d$rain_sum),
    snowfall_sum       = safe_vec(d$snowfall_sum),
    wind_max_kmh       = safe_vec(d$wind_speed_10m_max),
    wind_gust_kmh      = safe_vec(d$wind_gusts_10m_max),
    humidity_mean      = safe_vec(d$relative_humidity_2m_mean),
    radiation_sum      = safe_vec(d$shortwave_radiation_sum),
    sunshine_secs      = safe_vec(d$sunshine_duration),
    daylight_secs      = safe_vec(d$daylight_duration),
    cloud_cover        = safe_vec(d$cloud_cover_mean)
  )
}
