#' @importFrom data.table := .N .SD set
NULL

#' Default scoring weights
#'
#' Controls how much each weather component contributes to the
#' overall day score. Weights must sum to 1. Five components:
#' temp, rain, sky (merged sunshine+cloud), humidity, wind.
#'
#' @return Named list of weights
#' @export
default_weights <- function() {
  list(
    temp     = 0.25,
    rain     = 0.25,
    sky      = 0.20,
    humidity = 0.10,
    wind     = 0.20
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
      # Ideal ranges for each metric (°C)
      mean_ideal_min = 21,  mean_ideal_max = 25,
      max_ideal_min  = 24,  max_ideal_max  = 28,
      min_ideal_min  = 16,  min_ideal_max  = 20,
      # Weighting: max matters most (what you feel), mean second, min least
      w_max = 0.40, w_mean = 0.35, w_min = 0.25,
      decay_rate = 4,     # per-degree penalty outside ideal
      decay_power = 1.5   # curvature (1 = linear, 2 = quadratic)
    ),
    rain = list(
      heavy_mm = 10      # above this = heavy rain (scores ~15)
    ),
    sky = list(
      cloud_sweet_min = 10,  # some scattered clouds are ideal
      cloud_sweet_max = 25,  # above this, starts feeling overcast
      cloud_penalty_rate = 1.25,  # how fast score drops above sweet spot
      radiation_max = NULL   # MJ/m² for a clear day; NULL = auto-detect from data
    ),
    humidity = list(
      ideal_min = 40,    # sweet spot lower bound (%)
      ideal_max = 60,    # sweet spot upper bound (%)
      dry_decay = 1.5,   # per-% penalty below ideal (dry is tolerable)
      humid_decay = 3.0  # per-% penalty above ideal (muggy is worse)
    ),
    wind = list(
      pleasant_max = 15,    # max pleasant sustained wind (km/h)
      strong = 30,          # strong wind threshold
      extreme = 50,         # sustained wind — score 0
      gust_threshold = 25,  # gusts above this start penalizing
      gust_extreme = 60     # gusts at/above this = max gust penalty
    )
  )
}

#' Score a single temperature value against an ideal range
#' @noRd
score_temp_single <- function(value, ideal_min, ideal_max, decay_rate, decay_power) {
  if (is.na(value)) return(NA_real_)
  if (value >= ideal_min && value <= ideal_max) return(100)
  dist <- if (value < ideal_min) ideal_min - value else value - ideal_max
  max(0, 100 - decay_rate * dist^decay_power)
}

#' Score temperature comfort (weighted average of max, mean, min)
#' @noRd
score_temp <- function(temp_mean, temp_min, temp_max, params) {
  p <- params$temp
  s_mean <- score_temp_single(temp_mean, p$mean_ideal_min, p$mean_ideal_max, p$decay_rate, p$decay_power)
  s_max  <- score_temp_single(temp_max,  p$max_ideal_min,  p$max_ideal_max,  p$decay_rate, p$decay_power)
  s_min  <- score_temp_single(temp_min,  p$min_ideal_min,  p$min_ideal_max,  p$decay_rate, p$decay_power)

  w_max  <- p$w_max
  w_mean <- p$w_mean
  w_min  <- p$w_min

  vals    <- c(s_max, s_mean, s_min)
  weights <- c(w_max, w_mean, w_min)
  valid   <- !is.na(vals)
  if (!any(valid)) return(NA_real_)

  # Renormalise weights for available metrics
  sum(vals[valid] * weights[valid]) / sum(weights[valid])
}

#' Score rainfall (total mm + duration + snowfall)
#' @noRd
score_rain <- function(precip_total, precip_hours = NA, snowfall_sum = NA, params) {
  if (is.na(precip_total)) return(NA_real_)
  p <- params$rain

  # Base score from total precipitation (exponential decay)
  if (precip_total == 0) {
    base <- 100
  } else {
    k <- -log(0.15) / p$heavy_mm
    base <- max(0, 100 * exp(-k * precip_total))
  }

  # Duration penalty: rain spread over many hours is worse than a quick burst
  hours_penalty <- 0
  if (!is.na(precip_hours) && precip_hours > 2) {
    hours_penalty <- min(15, (precip_hours - 2) * 2)
  }

  # Snowfall penalty: any snow tanks the score (for outdoor/beach scorer)
  snow_penalty <- 0
  if (!is.na(snowfall_sum) && snowfall_sum > 0) {
    snow_penalty <- min(30, snowfall_sum * 10)
  }

  max(0, base - hours_penalty - snow_penalty)
}

#' Score sky quality (merged sunshine + cloud + radiation)
#'
#' Combines sunshine duration, cloud cover, and solar radiation
#' into a single "sky" score. Replaces separate sunshine/cloud components.
#' @noRd
score_sky <- function(sunshine_secs, daylight_secs, cloud_cover,
                      radiation_sum = NA, params) {
  # --- Sunshine component (50% of sky score) ---
  if (!is.na(sunshine_secs) && !is.na(daylight_secs) && daylight_secs > 0) {
    pct <- (sunshine_secs / daylight_secs) * 100
    sun_score <- if (pct >= 85) 100 else max(0, pct / 85 * 100)
  } else {
    sun_score <- NA_real_
  }

  # --- Cloud component (30% of sky score) ---
  # Sweet spot: 10-25% cloud (scattered cumulus = ideal beach sky)
  # Below 10%: no penalty (clear is fine too)
  # Above 25%: ramps down
  if (!is.na(cloud_cover)) {
    p <- params$sky
    if (cloud_cover <= p$cloud_sweet_max) {
      cloud_score <- 100
    } else {
      cloud_score <- max(0, 100 - (cloud_cover - p$cloud_sweet_max) * p$cloud_penalty_rate)
    }
  } else {
    cloud_score <- NA_real_
  }

  # --- Radiation component (20% of sky score) ---
  # Scale against radiation_max (configurable; 28 MJ/m² is a Sydney summer default)
  rad_max <- if (!is.null(params$sky$radiation_max)) params$sky$radiation_max else 28
  if (!is.na(radiation_sum)) {
    rad_score <- min(100, max(0, radiation_sum / rad_max * 100))
  } else {
    rad_score <- NA_real_
  }

  # Weighted blend of available components
  vals    <- c(sun_score, cloud_score, rad_score)
  weights <- c(0.50, 0.30, 0.20)
  valid   <- !is.na(vals)
  if (!any(valid)) return(NA_real_)
  sum(vals[valid] * weights[valid]) / sum(weights[valid])
}

#' Score humidity comfort
#' @noRd
score_humidity <- function(humidity, params) {
  if (is.na(humidity)) return(NA_real_)
  p <- params$humidity
  if (humidity >= p$ideal_min && humidity <= p$ideal_max) return(100)
  if (humidity < p$ideal_min) {
    dist <- p$ideal_min - humidity
    decay <- p$dry_decay
  } else {
    dist <- humidity - p$ideal_max
    decay <- p$humid_decay
  }
  max(0, 100 - decay * dist)
}

#' Score wind conditions (sustained + gusts)
#' @noRd
score_wind <- function(wind_max_kmh, wind_gust_kmh = NA, params) {
  if (is.na(wind_max_kmh)) return(NA_real_)
  p <- params$wind

  # Sustained wind score
  if (wind_max_kmh <= p$pleasant_max) {
    sustained_score <- 100
  } else if (wind_max_kmh >= p$extreme) {
    sustained_score <- 0
  } else {
    sustained_score <- 100 * (p$extreme - wind_max_kmh) / (p$extreme - p$pleasant_max)
  }

  # Gust penalty: gusts above threshold erode the score further
  gust_penalty <- 0
  if (!is.na(wind_gust_kmh) && wind_gust_kmh > p$gust_threshold) {
    gust_frac <- min(1, (wind_gust_kmh - p$gust_threshold) / (p$gust_extreme - p$gust_threshold))
    gust_penalty <- 40 * gust_frac
  }

  max(0, sustained_score - gust_penalty)
}

#' Compute weighted total with NA normalisation
#'
#' If a component is NA, its weight is redistributed to the others
#' so the total still scales 0-100.
#' @noRd
weighted_total <- function(scores, weights) {
  vals <- unlist(scores[names(weights)])
  wts  <- unlist(weights)
  valid <- !is.na(vals)
  if (!any(valid)) return(NA_real_)
  sum(vals[valid] * wts[valid]) / sum(wts[valid])
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
  # Use [[ ]] for reliable scalar extraction from data.table rows
  val <- function(col) {
    v <- day_data[[col]]
    if (length(v) == 0) NA_real_ else v[1L]
  }

  scores <- list(
    temp     = score_temp(val("temp_mean"), val("temp_min"), val("temp_max"), params),
    rain     = score_rain(val("precip_total"), val("precip_hours"), val("snowfall_sum"), params),
    sky      = score_sky(val("sunshine_secs"), val("daylight_secs"), val("cloud_cover"),
                         val("radiation_sum"), params),
    humidity = score_humidity(val("humidity_mean"), params),
    wind     = score_wind(val("wind_max_kmh"), val("wind_gust_kmh"), params)
  )

  scores$total <- weighted_total(scores, weights)
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
  n <- nrow(out)

  # Extract columns as plain vectors to avoid data.table row-subsetting issues
  v <- function(col) as.numeric(out[[col]])
  tm   <- v("temp_mean");    tmin <- v("temp_min");     tmax <- v("temp_max")
  prcp <- v("precip_total"); phrs <- v("precip_hours"); snow <- v("snowfall_sum")
  sun  <- v("sunshine_secs"); day <- v("daylight_secs"); rad <- v("radiation_sum")
  cld  <- v("cloud_cover")
  hum  <- v("humidity_mean")
  wnd  <- v("wind_max_kmh"); gst <- v("wind_gust_kmh")

  # Auto-detect radiation_max from data if not explicitly set
  if (is.null(params$sky$radiation_max)) {
    observed_max <- max(rad, na.rm = TRUE)
    if (is.finite(observed_max) && observed_max > 0) {
      params$sky$radiation_max <- observed_max
    }
  }

  s_temp     <- vapply(seq_len(n), \(i) score_temp(tm[i], tmin[i], tmax[i], params), numeric(1))
  s_rain     <- vapply(seq_len(n), \(i) score_rain(prcp[i], phrs[i], snow[i], params), numeric(1))
  s_sky      <- vapply(seq_len(n), \(i) score_sky(sun[i], day[i], cld[i], rad[i], params), numeric(1))
  s_humidity <- vapply(seq_len(n), \(i) score_humidity(hum[i], params), numeric(1))
  s_wind     <- vapply(seq_len(n), \(i) score_wind(wnd[i], gst[i], params), numeric(1))

  data.table::set(out, j = "score_temp",     value = s_temp)
  data.table::set(out, j = "score_rain",     value = s_rain)
  data.table::set(out, j = "score_sky",      value = s_sky)
  data.table::set(out, j = "score_humidity", value = s_humidity)
  data.table::set(out, j = "score_wind",     value = s_wind)

  # Vectorised weighted total with NA normalisation
  score_mat <- cbind(s_temp, s_rain, s_sky, s_humidity, s_wind)
  wt_vec <- c(weights$temp, weights$rain, weights$sky, weights$humidity, weights$wind)
  valid_mat <- !is.na(score_mat)
  weighted_sums <- rowSums(score_mat * rep(wt_vec, each = n), na.rm = TRUE)
  weight_sums   <- rowSums(valid_mat * rep(wt_vec, each = n))
  s_total <- ifelse(weight_sums > 0, weighted_sums / weight_sums, NA_real_)
  data.table::set(out, j = "score_total", value = s_total)

  out
}
