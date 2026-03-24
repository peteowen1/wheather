#' Default scoring weights
#'
#' Controls how much each weather component contributes to the
#' overall day score. Weights must sum to 1.
#'
#' @return Named list of weights
#' @export
default_weights <- function() {
  list(
    temp     = 0.25,
    rain     = 0.20,
    sunshine = 0.20,
    cloud    = 0.10,
    humidity = 0.15,
    wind     = 0.10
  )
}

#' Default scoring parameters
#'
#' Controls the thresholds and breakpoints for each scoring component.
#' Tweak these to change what "good weather" means.
#'
#' @return Named list of parameter lists
#' @export
default_params <- function() {
  list(
    temp = list(
      ideal_min = 22,    # lower bound of ideal range (°C)
      ideal_max = 26,    # upper bound of ideal range (°C)
      decay_rate = 5,    # how fast score drops outside ideal
      decay_power = 1.5  # curvature of decay (1 = linear, 2 = quadratic)
    ),
    rain = list(
      light_mm = 2,      # below this = light rain
      heavy_mm = 10      # above this = heavy rain
    ),
    humidity = list(
      ideal_min = 30,    # comfortable lower bound (%)
      ideal_max = 60     # comfortable upper bound (%)
    ),
    wind = list(
      pleasant_max = 15, # max pleasant wind speed (km/h)
      strong = 30,       # strong wind threshold
      extreme = 50       # extreme wind — score 0
    )
  )
}

#' Score temperature comfort
#' @noRd
score_temp <- function(temp_mean, temp_min, temp_max, params) {
  p <- params$temp
  if (is.na(temp_mean)) return(NA_real_)

  # Use mean temp as primary signal
  if (temp_mean >= p$ideal_min && temp_mean <= p$ideal_max) {
    base_score <- 100
  } else {
    dist <- if (temp_mean < p$ideal_min) {
      p$ideal_min - temp_mean
    } else {
      temp_mean - p$ideal_max
    }
    base_score <- max(0, 100 - p$decay_rate * dist^p$decay_power)
  }

  # Penalty for extreme min/max
  penalty <- 0
  if (!is.na(temp_min) && temp_min < 5)  penalty <- penalty + 10
  if (!is.na(temp_max) && temp_max > 38) penalty <- penalty + 15

  max(0, base_score - penalty)
}

#' Score rainfall
#' @noRd
score_rain <- function(precip_total, params) {
  if (is.na(precip_total)) return(NA_real_)
  p <- params$rain
  if (precip_total == 0) return(100)
  k <- -log(0.15) / p$heavy_mm
  max(0, 100 * exp(-k * precip_total))
}

#' Score sunshine hours
#' @noRd
score_sunshine <- function(sunshine_secs, daylight_secs) {
  if (is.na(sunshine_secs) || is.na(daylight_secs) || daylight_secs == 0) return(NA_real_)
  # Sunshine as % of possible daylight
  pct <- (sunshine_secs / daylight_secs) * 100
  # Direct mapping: 100% sunshine = 100, 0% = 0
  min(100, max(0, pct))
}

#' Score cloud cover
#' @noRd
score_cloud <- function(cloud_cover) {
  if (is.na(cloud_cover)) return(NA_real_)
  max(0, 100 - cloud_cover)
}

#' Score humidity comfort
#' @noRd
score_humidity <- function(humidity, params) {
  if (is.na(humidity)) return(NA_real_)
  p <- params$humidity
  if (humidity >= p$ideal_min && humidity <= p$ideal_max) return(100)
  if (humidity < p$ideal_min) {
    dist <- p$ideal_min - humidity
  } else {
    dist <- humidity - p$ideal_max
  }
  max(0, 100 - 2.5 * dist)
}

#' Score wind conditions
#' @noRd
score_wind <- function(wind_max_kmh, params) {
  if (is.na(wind_max_kmh)) return(NA_real_)
  p <- params$wind
  # Open-Meteo already returns km/h
  if (wind_max_kmh <= p$pleasant_max) return(100)
  if (wind_max_kmh >= p$extreme) return(0)
  100 * (p$extreme - wind_max_kmh) / (p$extreme - p$pleasant_max)
}

#' Score a single day's weather
#'
#' Takes a single row of weather data (as from [fetch_weather()])
#' and returns component scores plus a weighted total.
#'
#' @param day_data A one-row data.table of daily weather
#' @param weights Scoring weights (from [default_weights()])
#' @param params Scoring parameters (from [default_params()])
#' @return Named list: total + component scores (all 0-100)
#' @export
score_day <- function(day_data, weights = default_weights(), params = default_params()) {
  scores <- list(
    temp     = score_temp(day_data$temp_mean, day_data$temp_min, day_data$temp_max, params),
    rain     = score_rain(day_data$precip_total, params),
    sunshine = score_sunshine(day_data$sunshine_secs, day_data$daylight_secs),
    cloud    = score_cloud(day_data$cloud_cover),
    humidity = score_humidity(day_data$humidity_mean, params),
    wind     = score_wind(day_data$wind_max_kmh, params)
  )

  scores$total <- sum(
    scores$temp     * weights$temp,
    scores$rain     * weights$rain,
    scores$sunshine * weights$sunshine,
    scores$cloud    * weights$cloud,
    scores$humidity * weights$humidity,
    scores$wind     * weights$wind,
    na.rm = TRUE
  )

  scores
}

#' Score all days in a dataset
#'
#' Adds score columns to a weather data.table.
#'
#' @param data A data.table from [fetch_weather()]
#' @param weights Scoring weights
#' @param params Scoring parameters
#' @return The input data.table with added score columns
#' @export
score_period <- function(data, weights = default_weights(), params = default_params()) {
  out <- data.table::copy(data)

  day_scores <- lapply(seq_len(nrow(out)), function(i) {
    score_day(out[i], weights, params)
  })

  out[, score_total    := vapply(day_scores, \(s) s$total,    numeric(1))]
  out[, score_temp     := vapply(day_scores, \(s) s$temp,     numeric(1))]
  out[, score_rain     := vapply(day_scores, \(s) s$rain,     numeric(1))]
  out[, score_sunshine := vapply(day_scores, \(s) s$sunshine, numeric(1))]
  out[, score_cloud    := vapply(day_scores, \(s) s$cloud,    numeric(1))]
  out[, score_humidity := vapply(day_scores, \(s) s$humidity, numeric(1))]
  out[, score_wind     := vapply(day_scores, \(s) s$wind,     numeric(1))]

  out
}
