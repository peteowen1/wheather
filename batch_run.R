#!/usr/bin/env Rscript
#
# Batch weather fetcher — the single implementation.
#
# Run by .github/workflows/batch-fetch.yml and by the Claude Code remote
# trigger. There used to be two copies of this logic, this file and an inline
# heredoc in the workflow; they drifted, and only one of them had the 429
# circuit breaker. Do not reintroduce a second copy.
#
# MUST be run from the repo root - every path here is relative and this script
# deliberately does not setwd() (setwd inside an Rscript segfaults on this
# setup; see C:/dev/.claude/rules/r-datatable-gotchas.md). CI runs it from the
# checkout root; the remote trigger must cd first.
#
# Reads and writes data/batch_progress.json. Expects the parquet cache at
# data/cache — in CI that is restored from the 'cache' release before this runs
# and re-published after.

suppressMessages(devtools::load_all(quiet = TRUE))
library(data.table)

# --- configuration -----------------------------------------------------------

SLOTS               <- as.integer(Sys.getenv("WHEATHER_BATCH_SLOTS", "60"))
N_CITIES            <- 1000L
STOP_YEAR           <- 2015L   # backfill runs backwards and stops before this
DELAY_SECONDS       <- as.numeric(Sys.getenv("WHEATHER_BATCH_DELAY", "2"))
RETRY_429_WAIT      <- as.numeric(Sys.getenv("WHEATHER_BATCH_429_WAIT", "30"))
MAX_CONSECUTIVE_429 <- 3L
QUEUE_CAP           <- 1000L   # refuse to let the retry queue grow without bound

CACHE_DIR     <- "data/cache"
PROGRESS_FILE <- "data/batch_progress.json"
COMMIT_MSG    <- "data/.commit_msg"

options(wheather.cache_dir = CACHE_DIR)

# --- progress file -----------------------------------------------------------

read_progress <- function() {
  if (!file.exists(PROGRESS_FILE)) {
    return(list(current_year = 2025L, next_city_index = 1L,
                completed = list(), retry_queue = list()))
  }
  p <- jsonlite::fromJSON(PROGRESS_FILE, simplifyVector = FALSE)
  p$current_year    <- as.integer(p$current_year)
  p$next_city_index <- as.integer(p$next_city_index)
  if (is.null(p$completed))   p$completed   <- list()
  if (is.null(p$retry_queue)) p$retry_queue <- list()
  p
}

# Queue entries carry their own year. Indices alone would be ambiguous the
# moment the pointer rolls over into the next year, and a failure from 2022
# must not be retried against 2021.
queue_entry  <- function(year, index) list(year = as.integer(year), index = as.integer(index))
queue_key    <- function(e) paste0(e$year, ":", e$index)

# --- coverage ----------------------------------------------------------------

# `completed` used to be a running tally of successes, which drifts from reality
# whenever a run is lost — that is exactly how the 2022 gap stayed invisible.
# Derive it from the files instead, and fall back to the tally only if the scan
# itself fails.
derive_completed <- function(fallback) {
  tryCatch({
    files <- list.files(CACHE_DIR, pattern = "\\.parquet$", full.names = TRUE)
    if (!length(files)) return(fallback)
    cov <- rbindlist(lapply(files, function(f) {
      rf <- arrow::ReadableFile$create(f)
      on.exit(rf$close(), add = TRUE)
      dt <- as.data.table(arrow::read_parquet(rf, col_select = "date"))
      if (!nrow(dt)) return(NULL)
      unique(dt[, .(year = as.integer(format(as.Date(date), "%Y")))])
    }), use.names = TRUE)
    if (!nrow(cov)) return(fallback)
    tally <- cov[, .N, by = year][order(-year)]
    out <- as.list(setNames(as.integer(tally$N), as.character(tally$year)))
    cat(sprintf("Derived coverage from %d cache files\n", length(files)))
    out
  }, error = function(e) {
    cat(sprintf("WARNING: coverage scan failed (%s); keeping the previous counts\n",
                conditionMessage(e)))
    fallback
  })
}

# --- work selection ----------------------------------------------------------

progress   <- read_progress()
start_idx  <- progress$next_city_index
year_now   <- progress$current_year

if (year_now < STOP_YEAR) {
  cat(sprintf("All years complete (reached before %d). Nothing to do.\n", STOP_YEAR))
  quit(status = 0)
}

cities <- top_cities(N_CITIES)

# Retries come first and consume slots, so a backlog drains instead of growing
# while the pointer runs ahead of it.
queue      <- progress$retry_queue
retry_take <- if (length(queue)) queue[seq_len(min(length(queue), SLOTS))] else list()
queue_tail <- if (length(queue) > length(retry_take)) queue[-seq_along(retry_take)] else list()

free_slots <- SLOTS - length(retry_take)
new_take <- list()
if (free_slots > 0L && start_idx <= N_CITIES) {
  end_idx <- min(start_idx + free_slots - 1L, N_CITIES)
  new_take <- lapply(start_idx:end_idx, function(i) queue_entry(year_now, i))
}

new_keys <- vapply(new_take, queue_key, character(1))

# A queued entry can also fall inside this run's new range. Fetch it once:
# fetching twice is only wasteful, but counting it twice over-advances the
# pointer and skips a city, which is the whole class of bug this file exists
# to stop.
work <- c(retry_take, new_take)
work <- work[!duplicated(vapply(work, queue_key, character(1)))]

cat(sprintf("Year %d, pointer at city %d | %d retries + %d new = %d to fetch\n",
            year_now, start_idx, length(retry_take), length(new_take), length(work)))

if (!length(work)) {
  cat("Nothing to fetch this run.\n")
  quit(status = 0)
}

# --- fetch -------------------------------------------------------------------

n_success <- 0L
n_fail    <- 0L
attempted <- character(0)
failed    <- list()
reasons   <- character(0)
consecutive_429 <- 0L
quota_exhausted <- FALSE

# Never request past yesterday. The archive lags real time by several days, so
# asking for the rest of an in-progress year returns nothing useful and would
# cache a short year as though it were complete. The old inline copy in the
# workflow had this guard; it must not be lost with it.
year_end <- function(year) {
  min(as.Date(sprintf("%d-12-31", year)), Sys.Date() - 1)
}

fetch_one <- function(idx, year) {
  tryCatch({
    fetch_weather(cities$lat[idx], cities$lon[idx],
                  sprintf("%d-01-01", year), as.character(year_end(year)))
    "ok"
  }, error = function(e) conditionMessage(e))
}

for (i in seq_along(work)) {
  item  <- work[[i]]
  idx   <- item$index
  year  <- item$year
  label <- sprintf("%s, %s [%d]", cities$name[idx], cities$country[idx], year)

  result <- fetch_one(idx, year)

  if (grepl("429", result, fixed = TRUE)) {
    cat(sprintf("  [%d/%d] %s: rate limited, waiting %ds\n",
                i, length(work), label, RETRY_429_WAIT))
    Sys.sleep(RETRY_429_WAIT)
    result <- fetch_one(idx, year)
  }

  attempted <- c(attempted, queue_key(item))

  if (identical(result, "ok")) {
    n_success <- n_success + 1L
    consecutive_429 <- 0L
    cat(sprintf("  [%d/%d] OK: %s\n", i, length(work), label))
  } else {
    n_fail  <- n_fail + 1L
    failed  <- c(failed, list(item))
    reasons <- c(reasons, result)
    if (grepl("429", result, fixed = TRUE)) {
      consecutive_429 <- consecutive_429 + 1L
      cat(sprintf("  [%d/%d] FAIL (429 #%d): %s\n",
                  i, length(work), consecutive_429, label))
      if (consecutive_429 >= MAX_CONSECUTIVE_429) {
        cat(sprintf("  !! %d consecutive 429s - quota likely exhausted, stopping early\n",
                    MAX_CONSECUTIVE_429))
        quota_exhausted <- TRUE
        break
      }
    } else {
      consecutive_429 <- 0L
      cat(sprintf("  [%d/%d] FAIL: %s - %s\n", i, length(work), label, result))
    }
  }

  Sys.sleep(DELAY_SECONDS)
}

# --- advance the pointer -----------------------------------------------------

# Only new work moves the pointer; retries are already behind it. Count
# DISTINCT new keys, so an overlap between the queue and the new range cannot
# inflate the advance.
n_new_attempted <- length(intersect(unique(attempted), new_keys))

if (n_success == 0L) {
  # A systematic failure (proxy block, quota, outage) must not march through the
  # city list. Hold everything and let the next run try the same range.
  new_idx  <- start_idx
  new_year <- year_now
  cat("No cities succeeded - holding the pointer for retry.\n")
} else {
  new_idx  <- start_idx + n_new_attempted
  new_year <- year_now
  if (new_idx > N_CITIES) {
    new_idx  <- 1L
    new_year <- year_now - 1L
    cat(sprintf("Finished year %d, moving to %d.\n", year_now, new_year))
  }
}

# Anything that failed goes back on the queue, ahead of whatever did not fit
# this run.
#
# The 429 circuit breaker can stop the loop with selected retries still
# unreached. Those are in none of `attempted`, `failed` or `queue_tail`, so
# without this they would be dropped entirely - the same silent loss this file
# exists to prevent. Unattempted NEW work needs no such rescue: the pointer
# only advances over what was attempted, so it is picked up again next run.
work_keys  <- vapply(work, queue_key, character(1))
retry_keys <- if (length(retry_take)) vapply(retry_take, queue_key, character(1)) else character(0)
stranded   <- work[!(work_keys %in% attempted) & (work_keys %in% retry_keys)]
if (length(stranded)) {
  cat(sprintf("Preserving %d selected retries the run never reached.\n", length(stranded)))
}

new_queue <- c(failed, stranded, queue_tail)
n_dropped <- 0L
if (length(new_queue) > QUEUE_CAP) {
  # Keep this run's failures; drop the oldest untouched backlog.
  n_dropped <- length(new_queue) - QUEUE_CAP
  cat(sprintf("WARNING: retry queue at %d entries, dropping the %d oldest. Failures are outpacing retries - investigate.\n",
              length(new_queue), n_dropped))
  new_queue <- new_queue[seq_len(QUEUE_CAP)]
}

# --- write progress ----------------------------------------------------------

progress$completed       <- derive_completed(progress$completed)
progress$current_year    <- new_year
progress$next_city_index <- new_idx
progress$retry_queue     <- new_queue
progress$cache_files     <- length(list.files(CACHE_DIR, pattern = "\\.parquet$"))
progress$last_run        <- as.character(Sys.Date())
progress$last_run_status <- if (quota_exhausted) {
  "quota_exhausted"
} else if (n_success == 0L) {
  "failed"
} else if (n_fail > 0L) {
  "partial"
} else {
  "ok"
}
progress$last_run_summary <- sprintf(
  "year=%d attempted=%d (%d new, %d retry) success=%d fail=%d queue=%d%s",
  year_now, length(attempted), n_new_attempted,
  length(attempted) - n_new_attempted, n_success, n_fail, length(new_queue),
  if (quota_exhausted) " [stopped: quota exhausted]" else "")
# Always rewritten, so a stale error cannot survive into a report about a run
# that did not produce it. Distinct reasons, not just the first: a benign
# one-off ahead of a systematic failure would otherwise be all anyone sees.
progress$last_run_error <- {
  parts <- character(0)
  if (n_dropped > 0L) {
    # A truncation permanently drops city-years. A warning that lives only in
    # one day's Action log is a silent failure by another name.
    parts <- c(parts, sprintf("QUEUE OVERFLOW: dropped %d entries from the retry queue", n_dropped))
  }
  if (n_fail > 0L) parts <- c(parts, paste(unique(reasons), collapse = " | "))
  substr(paste(parts, collapse = " || "), 1L, 2000L)
}

dir.create("data", showWarnings = FALSE)
jsonlite::write_json(progress, PROGRESS_FILE, auto_unbox = TRUE, pretty = TRUE)

writeLines(
  sprintf("batch-fetch: %d (%d ok, %d failed, %d queued)",
          year_now, n_success, n_fail, length(new_queue)),
  COMMIT_MSG
)

cat(sprintf("\n=== %s ===\n", toupper(progress$last_run_status)))
cat(progress$last_run_summary, "\n")
if (n_fail > 0L) cat("reasons:", progress$last_run_error, "\n")
cat(sprintf("next run: year=%d city=%d, %d queued for retry\n",
            new_year, new_idx, length(new_queue)))
