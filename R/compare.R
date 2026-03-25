#' Detect radiation_max from combined data so both periods use the same scale
#' @noRd
unify_radiation_max <- function(params, data1, data2) {
  if (!is.null(params$sky$radiation_max)) return(params)
  rad <- c(as.numeric(data1[["radiation_sum"]]), as.numeric(data2[["radiation_sum"]]))
  rad <- rad[!is.na(rad)]
  if (length(rad) > 0 && max(rad) > 0) {
    params$sky$radiation_max <- max(rad)
  }
  params
}

#' Assemble a wheather_comparison from two scored datasets
#' @noRd
build_comparison <- function(scored1, scored2, weights, params, location) {
  # Copy to avoid mutating the caller's data.tables by reference
  s1 <- data.table::copy(scored1)
  s2 <- data.table::copy(scored2)
  data.table::set(s1, j = "day_index", value = seq_len(nrow(s1)))
  data.table::set(s2, j = "day_index", value = seq_len(nrow(s2)))

  result <- list(
    data     = data.table::rbindlist(list(s1, s2)),
    period1  = s1,
    period2  = s2,
    summary  = build_summary(scored1, scored2),
    weights  = weights,
    params   = params,
    location = location
  )
  class(result) <- "wheather_comparison"
  result
}

#' Compare weather quality between two time periods
#'
#' Fetches (or loads from cache) weather data for two periods,
#' scores each day, and produces a statistical comparison.
#'
#' @param city City name (e.g., "Sydney"). Alternative to lat/lon.
#' @param lat Latitude (used if city is NULL)
#' @param lon Longitude (used if city is NULL)
#' @param start1,end1 First period (e.g., "2024-12-01" to "2025-02-28")
#' @param start2,end2 Second period (e.g., "2023-12-01" to "2024-02-29")
#' @param weights Scoring weights
#' @param params Scoring parameters
#' @return A list with class "wheather_comparison" containing scored data and summary
#' @export
compare_periods <- function(city = NULL, lat = NULL, lon = NULL,
                            start1, end1,
                            start2, end2,
                            weights = default_weights(),
                            params = default_params()) {
  validate_date_range(start1, end1, "Period 1")
  validate_date_range(start2, end2, "Period 2")
  loc <- resolve_location(city, lat, lon)
  cli::cli_h1("{loc$label}")

  label_for <- function(s, e) paste(format(as.Date(s), "%b %Y"), "-", format(as.Date(e), "%b %Y"))

  cli::cli_h2("Period 1: {start1} to {end1}")
  data1 <- fetch_weather(loc$lat, loc$lon, start1, end1)
  cli::cli_h2("Period 2: {start2} to {end2}")
  data2 <- fetch_weather(loc$lat, loc$lon, start2, end2)

  # Auto-detect radiation_max from BOTH periods so they're scored on the same scale
  params <- unify_radiation_max(params, data1, data2)

  scored1 <- score_period(data1, weights, params)
  data.table::set(scored1, j = "period", value = "Period 1")
  data.table::set(scored1, j = "label",  value = label_for(start1, end1))

  scored2 <- score_period(data2, weights, params)
  data.table::set(scored2, j = "period", value = "Period 2")
  data.table::set(scored2, j = "label",  value = label_for(start2, end2))

  build_comparison(scored1, scored2, weights, params,
                   list(lat = loc$lat, lon = loc$lon, label = loc$label))
}

#' Compare weather quality between two cities for the same time period
#'
#' @param city1,city2 City names (e.g., "Sydney", "Melbourne")
#' @param start,end Date range to compare (e.g., "2025-12-01" to "2026-02-28")
#' @param weights Scoring weights
#' @param params Scoring parameters
#' @return A list with class "wheather_comparison"
#' @export
compare_cities <- function(city1, city2, start, end,
                           weights = default_weights(),
                           params = default_params()) {
  validate_date_range(start, end)
  loc1 <- resolve_location(city1)
  loc2 <- resolve_location(city2)

  cli::cli_h2("{loc1$label}: {start} to {end}")
  data1 <- fetch_weather(loc1$lat, loc1$lon, start, end)
  cli::cli_h2("{loc2$label}: {start} to {end}")
  data2 <- fetch_weather(loc2$lat, loc2$lon, start, end)

  # Auto-detect radiation_max from BOTH cities so they're scored on the same scale
  params <- unify_radiation_max(params, data1, data2)

  scored1 <- score_period(data1, weights, params)
  data.table::set(scored1, j = "period", value = loc1$label)
  data.table::set(scored1, j = "label",  value = loc1$label)

  scored2 <- score_period(data2, weights, params)
  data.table::set(scored2, j = "period", value = loc2$label)
  data.table::set(scored2, j = "label",  value = loc2$label)

  build_comparison(scored1, scored2, weights, params,
                   list(city1 = list(lat = loc1$lat, lon = loc1$lon, label = loc1$label),
                        city2 = list(lat = loc2$lat, lon = loc2$lon, label = loc2$label)))
}

#' Build comparison summary statistics
#' @noRd
build_summary <- function(scored1, scored2) {
  s1 <- scored1$score_total[!is.na(scored1$score_total)]
  s2 <- scored2$score_total[!is.na(scored2$score_total)]

  label1 <- scored1$label[1]
  label2 <- scored2$label[1]

  # Guard against empty score vectors (all NA data)
  if (length(s1) == 0 || length(s2) == 0) {
    return(list(
      period1  = list(label = label1, mean = NaN, sd = NA_real_,
                      median = NA_real_, best = -Inf, worst = Inf),
      period2  = list(label = label2, mean = NaN, sd = NA_real_,
                      median = NA_real_, best = -Inf, worst = Inf),
      t_test   = NULL,
      cohens_d = NA_real_,
      verdict  = "Insufficient data: one or both periods had no scoreable days"
    ))
  }

  # Need at least 2 observations per group for a t-test
  can_test <- length(s1) >= 2 && length(s2) >= 2

  if (can_test) {
    tt <- stats::t.test(s1, s2)
    pooled_sd <- sqrt((stats::var(s1) + stats::var(s2)) / 2)
    cohens_d <- if (pooled_sd > 0) (mean(s1) - mean(s2)) / pooled_sd else 0
    sig <- tt$p.value < 0.05
  } else {
    tt <- NULL
    cohens_d <- NA_real_
    sig <- FALSE
  }

  diff <- mean(s1) - mean(s2)

  if (!can_test) {
    verdict <- sprintf("Too few days for statistical test (%s: %.1f vs %s: %.1f)",
                       label1, mean(s1), label2, mean(s2))
  } else if (!sig) {
    verdict <- sprintf("No significant difference (%s: %.1f vs %s: %.1f, p=%.3f)",
                       label1, mean(s1), label2, mean(s2), tt$p.value)
  } else if (diff > 0) {
    verdict <- sprintf("%s was significantly better (%.1f vs %.1f, p=%.3f, d=%.2f)",
                       label1, mean(s1), mean(s2), tt$p.value, abs(cohens_d))
  } else {
    verdict <- sprintf("%s was significantly better (%.1f vs %.1f, p=%.3f, d=%.2f)",
                       label2, mean(s2), mean(s1), tt$p.value, abs(cohens_d))
  }

  period_stats <- function(scores, label) {
    list(label = label, mean = mean(scores), sd = stats::sd(scores),
         median = stats::median(scores), best = max(scores), worst = min(scores))
  }

  list(
    period1  = period_stats(s1, label1),
    period2  = period_stats(s2, label2),
    t_test   = tt,
    cohens_d = cohens_d,
    verdict  = verdict
  )
}

#' Print method for wheather comparisons
#'
#' Prints the summary verdict. Returns a debug-friendly data.table
#' invisibly, with raw weather values alongside component scores.
#' Capture it with `dt <- print(result)` to inspect the scoring.
#'
#' @export
print.wheather_comparison <- function(x, ...) {
  s <- x$summary
  cli::cli_h1("Weather Comparison")
  cli::cli_text("")

  cli::cli_h2(s$period1$label)
  cli::cli_text("  Mean: {.strong {sprintf('%.1f', s$period1$mean)}}  SD: {sprintf('%.1f', s$period1$sd)}  Best: {sprintf('%.0f', s$period1$best)}  Worst: {sprintf('%.0f', s$period1$worst)}")

  cli::cli_h2(s$period2$label)
  cli::cli_text("  Mean: {.strong {sprintf('%.1f', s$period2$mean)}}  SD: {sprintf('%.1f', s$period2$sd)}  Best: {sprintf('%.0f', s$period2$best)}  Worst: {sprintf('%.0f', s$period2$worst)}")

  cli::cli_text("")
  cli::cli_h2("Verdict")
  cli::cli_alert_success(s$verdict)

  # Build debug table: raw values | component scores | total
  dt <- data.table::copy(x$data)
  # Convert sunshine seconds to hours before defining column list
  data.table::set(dt, j = "sunshine_hrs", value = round(dt[["sunshine_secs"]] / 3600, 1))
  debug_cols <- c(
    "period", "date",
    "temp_min", "temp_mean", "temp_max", "score_temp",
    "precip_total", "precip_hours", "score_rain",
    "sunshine_hrs", "cloud_cover", "radiation_sum", "score_sky",
    "humidity_mean", "score_humidity",
    "wind_max_kmh", "wind_gust_kmh", "score_wind",
    "score_total"
  )

  # Round numeric columns for cleaner output
  round_cols <- c("temp_min", "temp_mean", "temp_max", "precip_total",
                  "cloud_cover", "radiation_sum", "humidity_mean",
                  "wind_max_kmh", "wind_gust_kmh",
                  "score_temp", "score_rain", "score_sky",
                  "score_humidity", "score_wind", "score_total")
  for (col in round_cols) {
    data.table::set(dt, j = col, value = round(dt[[col]], 1))
  }

  invisible(dt[, debug_cols, with = FALSE])
}
