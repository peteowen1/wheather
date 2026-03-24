#' Look up coordinates for a city name
#'
#' Uses Open-Meteo's Geocoding API (free, no key required).
#' Tries the built-in lookup table first for instant results.
#'
#' @param city City name (e.g., "Sydney", "London")
#' @return A data.table with columns: name, lat, lon, country
#' @export
geocode <- function(city) {
  # Try built-in lookup first
  match <- lookup_city(city)
  if (!is.null(match)) return(match)

  # Fall back to Open-Meteo geocoding API (free, no key)
  resp <- httr2::request("https://geocoding-api.open-meteo.com/v1/search") |>
    httr2::req_url_query(
      name = city,
      count = 5,
      language = "en",
      format = "json"
    ) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  json <- httr2::resp_body_json(resp)
  results <- json$results
  if (is.null(results) || length(results) == 0) {
    cli::cli_abort("No results found for {.val {city}}")
  }

  data.table::data.table(
    name    = vapply(results, \(x) x$name %||% NA_character_, character(1)),
    lat     = vapply(results, \(x) x$latitude %||% NA_real_, numeric(1)),
    lon     = vapply(results, \(x) x$longitude %||% NA_real_, numeric(1)),
    country = vapply(results, \(x) x$country %||% NA_character_, character(1)),
    state   = vapply(results, \(x) x$admin1 %||% NA_character_, character(1))
  )
}

#' Built-in city lookup table
#' @noRd
common_cities <- function() {
  data.table::data.table(
    name    = c("Sydney",  "Melbourne", "Brisbane",  "Perth",    "Adelaide",
                "London",  "New York",  "Tokyo",     "Paris",    "Berlin",
                "Auckland","Singapore", "Los Angeles","Chicago", "Toronto"),
    lat     = c(-33.87,   -37.81,      -27.47,      -31.95,    -34.93,
                51.51,     40.71,       35.68,       48.86,      52.52,
                -36.85,    1.35,        34.05,       41.88,      43.65),
    lon     = c(151.21,   144.96,      153.03,      115.86,    138.60,
                -0.13,    -74.01,      139.69,       2.35,      13.41,
                174.76,   103.82,     -118.24,      -87.63,    -79.38),
    country = c("AU","AU","AU","AU","AU",
                "GB","US","JP","FR","DE",
                "NZ","SG","US","US","CA"),
    state   = c("New South Wales","Victoria","Queensland","Western Australia","South Australia",
                "England","New York","Tokyo","Ile-de-France","Berlin",
                "Auckland","Singapore","California","Illinois","Ontario")
  )
}

#' Case-insensitive lookup in the built-in city table
#' @noRd
lookup_city <- function(city) {
  cities <- common_cities()
  city_lower <- tolower(trimws(city))
  idx <- which(tolower(cities[["name"]]) == city_lower)
  if (length(idx) > 0) return(cities[idx, ])
  NULL
}
