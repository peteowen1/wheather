#' Compare weather quality between two time periods
#'
#' Fetches (or loads from cache) weather data for two periods,
#' scores each day, and produces a statistical comparison.
#'
#' @param lat Latitude
#' @param lon Longitude
#' @param start1,end1 First period (e.g., "2024-12-01" to "2025-02-28")
#' @param start2,end2 Second period (e.g., "2023-12-01" to "2024-02-29")
#' @param weights Scoring weights
#' @param params Scoring parameters
#' @return A list with class "wheather_comparison" containing scored data and summary
#' @export
compare_periods <- function(lat, lon,
                            start1, end1,
                            start2, end2,
                            weights = default_weights(),
                            params = default_params()) {
  cli::cli_h2("Period 1: {start1} to {end1}")
  data1 <- fetch_weather(lat, lon, start1, end1)
  scored1 <- score_period(data1, weights, params)
  scored1[, period := "Period 1"]
  scored1[, label := paste(format(as.Date(start1), "%b %Y"), "-", format(as.Date(end1), "%b %Y"))]

  cli::cli_h2("Period 2: {start2} to {end2}")
  data2 <- fetch_weather(lat, lon, start2, end2)
  scored2 <- score_period(data2, weights, params)
  scored2[, period := "Period 2"]
  scored2[, label := paste(format(as.Date(start2), "%b %Y"), "-", format(as.Date(end2), "%b %Y"))]

  # Day-of-period index for aligned plotting
  scored1[, day_index := seq_len(.N)]
  scored2[, day_index := seq_len(.N)]

  combined <- data.table::rbindlist(list(scored1, scored2))

  summary <- build_summary(scored1, scored2)

  result <- list(
    data     = combined,
    period1  = scored1,
    period2  = scored2,
    summary  = summary,
    weights  = weights,
    params   = params,
    location = list(lat = lat, lon = lon)
  )
  class(result) <- "wheather_comparison"
  result
}

#' Build comparison summary statistics
#' @noRd
build_summary <- function(scored1, scored2) {
  s1 <- scored1$score_total
  s2 <- scored2$score_total

  # Welch's t-test
  tt <- stats::t.test(s1, s2)

  # Cohen's d effect size
  pooled_sd <- sqrt((stats::var(s1) + stats::var(s2)) / 2)
  cohens_d <- if (pooled_sd > 0) (mean(s1) - mean(s2)) / pooled_sd else 0

  label1 <- scored1$label[1]
  label2 <- scored2$label[1]

  # Determine verdict
  diff <- mean(s1) - mean(s2)
  sig <- tt$p.value < 0.05

  if (!sig) {
    verdict <- sprintf("No significant difference (%s: %.1f vs %s: %.1f, p=%.3f)",
                       label1, mean(s1), label2, mean(s2), tt$p.value)
  } else if (diff > 0) {
    verdict <- sprintf("%s was significantly better (%.1f vs %.1f, p=%.3f, d=%.2f)",
                       label1, mean(s1), mean(s2), tt$p.value, abs(cohens_d))
  } else {
    verdict <- sprintf("%s was significantly better (%.1f vs %.1f, p=%.3f, d=%.2f)",
                       label2, mean(s2), mean(s1), tt$p.value, abs(cohens_d))
  }

  list(
    period1 = list(
      label  = label1,
      mean   = mean(s1),
      sd     = stats::sd(s1),
      median = stats::median(s1),
      best   = max(s1),
      worst  = min(s1)
    ),
    period2 = list(
      label  = label2,
      mean   = mean(s2),
      sd     = stats::sd(s2),
      median = stats::median(s2),
      best   = max(s2),
      worst  = min(s2)
    ),
    t_test   = tt,
    cohens_d = cohens_d,
    verdict  = verdict
  )
}

#' Print method for wheather comparisons
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

  invisible(x)
}
