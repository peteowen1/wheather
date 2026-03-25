#' Get top cities by population
#'
#' Returns a data.table of the most populated cities worldwide,
#' sourced from the `maps` package's world.cities dataset.
#'
#' @param n Number of cities to return (default 1000)
#' @return A data.table with columns: name, country, lat, lon, pop, capital
#' @export
top_cities <- function(n = 1000) {
  if (!requireNamespace("maps", quietly = TRUE)) {
    cli::cli_abort("Install the {.pkg maps} package: {.code install.packages('maps')}")
  }
  data("world.cities", package = "maps", envir = environment())
  wc <- data.table::as.data.table(world.cities)
  data.table::setorder(wc, -pop)
  wc <- wc[seq_len(min(n, nrow(wc)))]
  data.table::setnames(wc, c("country.etc", "long"), c("country", "lon"))
  wc[, capital := capital == 1L]
  wc
}

#' Batch-fetch weather data for multiple cities
#'
#' Fetches historical weather for a list of cities in year-sized chunks,
#' using the cache to skip data already downloaded. Resumes cleanly if
#' interrupted. Designed to stay within Open-Meteo's free-tier rate limits.
#'
#' @param cities A data.table with at least columns: name, lat, lon, country.
#'   Use [top_cities()] to generate one.
#' @param start Start date (default "1940-01-01" — as far back as Open-Meteo goes)
#' @param end End date (default yesterday)
#' @param chunk_years Size of each date chunk in years (default 10).
#'   Smaller chunks = smaller API responses = fewer rate limits.
#' @param delay Seconds between API calls (default 1.5). Backs off on 429s.
#' @return Invisibly returns a data.table summarising what was fetched/cached per city
#' @export
batch_fetch <- function(cities,
                        start = "1940-01-01",
                        end = as.character(Sys.Date() - 1),
                        chunk_years = 10,
                        delay = 1.5) {
  n <- nrow(cities)
  start_date <- as.Date(start)
  end_date <- as.Date(end)

  # Build year chunks
  chunks <- list()
  chunk_start <- start_date
  while (chunk_start <= end_date) {
    chunk_end <- min(end_date, as.Date(paste0(as.integer(format(chunk_start, "%Y")) + chunk_years - 1, "-12-31")))
    chunks[[length(chunks) + 1]] <- list(start = chunk_start, end = chunk_end)
    chunk_start <- chunk_end + 1
  }
  n_chunks <- length(chunks)

  total_years <- as.numeric(end_date - start_date) / 365.25
  cli::cli_h1("Batch fetch: {n} cities x {round(total_years)}yr ({n_chunks} chunks each)")
  cli::cli_alert_info("Max API calls: {n * n_chunks} | Delay: {delay}s")

  status <- character(n)
  api_calls <- 0L
  skipped_calls <- 0L
  errors <- character(0)
  # Mutable state shared across closures — environment avoids assign() fragility
  state <- new.env(parent = emptyenv())
  state$delay <- delay

  for (i in seq_len(n)) {
    city_label <- paste0(cities$name[i], ", ", cities$country[i])
    city_errors <- 0L
    city_fetched <- 0L

    for (ch in seq_along(chunks)) {
      cs <- as.character(chunks[[ch]]$start)
      ce <- as.character(chunks[[ch]]$end)

      # Check if this chunk needs an API call (has uncached dates)
      coords <- round_coords(cities$lat[i], cities$lon[i])
      cached <- get_cached(coords$lat, coords$lon)
      requested <- as.Date(date_seq(cs, ce))
      needs_fetch <- is.null(cached) || !all(requested %in% as.Date(cached$date))

      result <- tryCatch({
        fetch_weather(cities$lat[i], cities$lon[i], cs, ce)
        "ok"
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("429", msg, fixed = TRUE)) {
          backoff <- min(120, state$delay * 4)
          cli::cli_alert_warning("[{i}/{n}] {city_label} chunk {ch}: rate limited, waiting {round(backoff)}s")
          Sys.sleep(backoff)
          state$delay <- min(30, state$delay * 1.5)
          tryCatch({
            fetch_weather(cities$lat[i], cities$lon[i], cs, ce)
            "ok"
          }, error = function(e2) conditionMessage(e2))
        } else {
          msg
        }
      })

      if (identical(result, "ok")) {
        if (needs_fetch) api_calls <- api_calls + 1L
        city_fetched <- city_fetched + 1L
        if (!needs_fetch) skipped_calls <- skipped_calls + 1L
        state$delay <- max(delay, state$delay * 0.9)
      } else {
        city_errors <- city_errors + 1L
        errors <- c(errors, paste0(city_label, " [", cs, "]: ", result))
      }

      if (needs_fetch) Sys.sleep(state$delay)
    }

    if (city_errors == 0) {
      status[i] <- "complete"
      cli::cli_alert_success("[{i}/{n}] {city_label} ({city_fetched} chunks)")
    } else if (city_fetched > 0) {
      status[i] <- "partial"
      cli::cli_alert_warning("[{i}/{n}] {city_label} ({city_fetched} ok, {city_errors} failed)")
    } else {
      status[i] <- "error"
      cli::cli_alert_danger("[{i}/{n}] {city_label} (all chunks failed)")
    }
  }

  cli::cli_h2("Done")
  cli::cli_alert_success("Complete: {sum(status == 'complete')} | Partial: {sum(status == 'partial')} | Failed: {sum(status == 'error')}")
  cli::cli_alert_info("API calls: {api_calls} | Cache hits: {skipped_calls}")

  if (length(errors) > 0) {
    cli::cli_h2("{length(errors)} chunk errors (re-run to retry)")
  }

  cities_dt <- data.table::copy(cities)
  data.table::set(cities_dt, j = "status", value = status)
  invisible(cities_dt)
}
