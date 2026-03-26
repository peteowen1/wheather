#!/usr/bin/env Rscript
# Batch weather data fetcher for wheather package
# Fetches 60 cities x 1 year per run

setwd("/home/user/wheather")

# Load required packages (arrow is a shim using RDS; install if missing)
suppressPackageStartupMessages({
  library(data.table)
  library(httr2)
  library(cli)
  library(maps)
  library(jsonlite)
  library(arrow)  # shim: reads/writes RDS under .parquet filenames
})

# Set cache directory inside repo
options(wheather.cache_dir = "data/cache")
dir.create("data/cache", recursive = TRUE, showWarnings = FALSE)

# Source package functions directly (avoids devtools/dependency issues)
# Provide helpers that api.R and cache.R expect
round_coords <- function(lat, lon) list(lat = round(lat, 2), lon = round(lon, 2))
split_contiguous <- function(dates) {
  if (length(dates) == 0) return(list())
  dates <- sort(dates)
  groups <- cumsum(c(1, diff(as.integer(dates))) > 1)
  split(dates, groups)
}
source("R/cache.R")
source("R/api.R")
source("R/batch.R")

# Override fetch_open_meteo to not retry on permanent network failures (403)
# The default in api.R retries 5x with exponential backoff which is too slow
# when the proxy blocks all requests.
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
      latitude  = lat,
      longitude = lon,
      start_date = format(as.Date(start), "%Y-%m-%d"),
      end_date   = format(as.Date(end),   "%Y-%m-%d"),
      daily      = daily_vars,
      timezone   = "auto"
    ) |>
    # No retries for the batch agent - fast-fail on network errors
    httr2::req_retry(max_tries = 1) |>
    httr2::req_timeout(15) |>
    httr2::req_perform()

  json <- httr2::resp_body_json(resp)
  if (is.null(json$daily)) {
    cli::cli_abort("No daily data returned from Open-Meteo.")
  }
  parse_open_meteo(json)
}

# Read progress file
progress_file <- "data/batch_progress.json"
if (file.exists(progress_file)) {
  progress <- jsonlite::fromJSON(progress_file)
} else {
  progress <- list(current_year = 2025, next_city_index = 1, completed = list())
}

current_year <- progress$current_year
next_city_index <- progress$next_city_index
completed <- progress$completed

cat(sprintf("Starting batch fetch: year=%d, starting at city index=%d\n",
            current_year, next_city_index))

# Stop if we've gone past 2015
if (current_year < 2015) {
  cat("All years complete (reached 2015). Nothing to do.\n")
  quit(status = 0)
}

# Get top 1000 cities
cities <- top_cities(1000)
cat(sprintf("Total cities available: %d\n", nrow(cities)))

# Select batch of 60
end_idx <- min(next_city_index + 59, nrow(cities))
batch <- cities[next_city_index:end_idx, ]
batch_size <- nrow(batch)
cat(sprintf("Fetching %d cities (indices %d-%d) for year %d\n",
            batch_size, next_city_index, end_idx, current_year))

# Set date range
start_date <- sprintf("%d-01-01", current_year)
if (current_year == as.integer(format(Sys.Date(), "%Y"))) {
  end_date <- format(Sys.Date() - 1, "%Y-%m-%d")
} else {
  end_date <- sprintf("%d-12-31", current_year)
}
cat(sprintf("Date range: %s to %s\n", start_date, end_date))

# Fetch each city
n_success <- 0
n_fail <- 0
failed_cities <- character(0)

for (i in seq_len(batch_size)) {
  city <- batch[i, ]
  city_name <- paste0(city$name, ", ", city$country)

  tryCatch({
    cat(sprintf("[%d/%d] Fetching %s (%.2f, %.2f)...",
                i, batch_size, city_name, city$lat, city$lon))

    result <- fetch_weather(city$lat, city$lon, start_date, end_date)

    if (!is.null(result) && nrow(result) > 0) {
      cat(sprintf(" OK (%d days)\n", nrow(result)))
      n_success <- n_success + 1
    } else {
      cat(" EMPTY\n")
      n_fail <- n_fail + 1
      failed_cities <- c(failed_cities, city_name)
    }

    # 2-second delay between successful calls (skip on failure to stay fast)
    if (i < batch_size) Sys.sleep(2)

  }, error = function(e) {
    msg <- conditionMessage(e)
    # On 429, wait 30s and retry once (skipped if network is blocked)
    if (grepl("429|rate limit|too many", msg, ignore.case = TRUE)) {
      cat(sprintf(" 429 rate limit! Waiting 30s...\n"))
      Sys.sleep(30)
      tryCatch({
        result <- fetch_weather(city$lat, city$lon, start_date, end_date)
        if (!is.null(result) && nrow(result) > 0) {
          cat(sprintf("[%d/%d] Retry %s OK (%d days)\n", i, batch_size, city_name, nrow(result)))
          n_success <<- n_success + 1
        } else {
          cat(sprintf("[%d/%d] Retry %s EMPTY\n", i, batch_size, city_name))
          n_fail <<- n_fail + 1
          failed_cities <<- c(failed_cities, city_name)
        }
      }, error = function(e2) {
        cat(sprintf("[%d/%d] Retry %s FAILED: %s\n", i, batch_size, city_name, conditionMessage(e2)))
        n_fail <<- n_fail + 1
        failed_cities <<- c(failed_cities, city_name)
      })
    } else {
      # Trim long error messages (e.g., curl tunnel errors)
      short_msg <- substr(gsub("\n.*", "", msg), 1, 80)
      cat(sprintf(" FAILED: %s\n", short_msg))
      n_fail <<- n_fail + 1
      failed_cities <<- c(failed_cities, city_name)
    }
  })
}

# Update progress
new_next_idx <- end_idx + 1
if (new_next_idx > nrow(cities)) {
  # Finished this year, move to next
  new_next_idx <- 1
  new_year <- current_year - 1
  cat(sprintf("\nCompleted year %d! Moving to year %d.\n", current_year, new_year))
} else {
  new_year <- current_year
}

# Update completed count
completed_key <- as.character(current_year)
if (!is.null(completed[[completed_key]])) {
  completed[[completed_key]] <- completed[[completed_key]] + n_success
} else {
  completed[[completed_key]] <- n_success
}

new_progress <- list(
  current_year = new_year,
  next_city_index = new_next_idx,
  completed = completed
)

jsonlite::write_json(new_progress, progress_file, auto_unbox = TRUE, pretty = TRUE)

# Print summary
cat(sprintf("\n=== BATCH COMPLETE ===\n"))
cat(sprintf("Year: %d\n", current_year))
cat(sprintf("Cities: %d-%d (%d total)\n", next_city_index, end_idx, batch_size))
cat(sprintf("Success: %d\n", n_success))
cat(sprintf("Failed: %d\n", n_fail))
if (length(failed_cities) > 0) {
  cat(sprintf("Failed cities: %s\n", paste(failed_cities, collapse = ", ")))
}
cat(sprintf("Next run: year=%d, starting at city=%d\n", new_year, new_next_idx))
