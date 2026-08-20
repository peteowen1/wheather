# Exercises batch_run.R's work-selection, pointer and retry-queue logic against
# a stubbed fetch_weather, in a throwaway working directory. No network, no
# touching the real data/batch_progress.json.

# Run from the repo root: Rscript tools/test_batch_queue.R
REPO <- normalizePath(getwd(), winslash = "/")
SANDBOX <- file.path(tempdir(), paste0("bq", as.integer(runif(1, 1, 1e6))))

setup <- function(progress) {
  unlink(SANDBOX, recursive = TRUE)
  dir.create(file.path(SANDBOX, "data", "cache"), recursive = TRUE)
  # Minimal package skeleton so devtools::load_all() works in the sandbox.
  for (f in c("DESCRIPTION", "NAMESPACE")) file.copy(file.path(REPO, f), SANDBOX)
  file.copy(file.path(REPO, "R"), SANDBOX, recursive = TRUE)
  jsonlite::write_json(progress, file.path(SANDBOX, "data", "batch_progress.json"),
                       auto_unbox = TRUE, pretty = TRUE)
}

# `fail_on` is a set of "year:index" keys the stub should fail for.
run_case <- function(name, progress, fail_on = character(0), slots = 10L, expect) {
  setup(progress)
  old <- setwd(SANDBOX); on.exit(setwd(old), add = TRUE)

  Sys.setenv(WHEATHER_BATCH_SLOTS = as.character(slots),
             WHEATHER_BATCH_DELAY = "0",
             WHEATHER_BATCH_429_WAIT = "0")

  # Stubs live in globalenv, which is searched before the attached package.
  assign("top_cities", function(n) {
    data.table::data.table(name = paste0("City", seq_len(n)), country = "XX",
                           lat = seq_len(n) / 100, lon = seq_len(n) / 100)
  }, envir = globalenv())
  assign("FAIL_ON", fail_on, envir = globalenv())
  assign("fetch_weather", function(lat, lon, start, end) {
    idx <- as.integer(round(lat * 100))
    yr  <- as.integer(substr(start, 1, 4))
    if (paste0(yr, ":", idx) %in% get("FAIL_ON", envir = globalenv())) {
      stop("stubbed failure for ", yr, ":", idx)
    }
    data.table::data.table(date = as.Date(paste0(yr, "-01-01")))
  }, envir = globalenv())

  out <- capture.output(
    suppressWarnings(try(source(file.path(REPO, "batch_run.R"), local = new.env()),
                         silent = TRUE))
  )

  got <- jsonlite::fromJSON(file.path(SANDBOX, "data", "batch_progress.json"),
                            simplifyVector = FALSE)
  qlen <- length(got$retry_queue)
  qkeys <- if (qlen) vapply(got$retry_queue, function(e) paste0(e$year, ":", e$index),
                            character(1)) else character(0)

  actual <- list(year = got$current_year, idx = got$next_city_index,
                 status = got$last_run_status, queue = qlen)
  ok <- identical(actual$year, expect$year) && identical(actual$idx, expect$idx) &&
        identical(actual$status, expect$status) && identical(actual$queue, expect$queue)

  cat(sprintf("%-46s %s\n", name, if (ok) "PASS" else "**FAIL**"))
  cat(sprintf("   year=%s idx=%s status=%-8s queue=%d %s\n",
              actual$year, actual$idx, actual$status, qlen,
              if (qlen && qlen <= 6) paste0("[", paste(qkeys, collapse = ","), "]") else ""))
  if (!ok) cat(sprintf("   expected: year=%s idx=%s status=%s queue=%s\n",
                       expect$year, expect$idx, expect$status, expect$queue))
  invisible(ok)
}

base <- list(current_year = 2020L, next_city_index = 1L,
             completed = list(), retry_queue = list())

cat("=== retry queue / pointer behaviour ===\n\n")
results <- c(

  run_case("clean run: all 10 succeed",
           base,
           expect = list(year = 2020L, idx = 11L, status = "ok", queue = 0L)),

  run_case("partial: 3 fail -> pointer advances, 3 queued",
           base, fail_on = c("2020:2", "2020:5", "2020:9"),
           expect = list(year = 2020L, idx = 11L, status = "partial", queue = 3L)),

  run_case("total failure: pointer HOLDS, all 10 queued",
           base, fail_on = paste0("2020:", 1:10),
           expect = list(year = 2020L, idx = 1L, status = "failed", queue = 10L)),

  run_case("queue drains first, consuming slots",
           list(current_year = 2020L, next_city_index = 11L, completed = list(),
                retry_queue = list(list(year = 2020L, index = 2L),
                                   list(year = 2020L, index = 5L))),
           expect = list(year = 2020L, idx = 19L, status = "ok", queue = 0L)),

  run_case("queue survives a year rollover with its own year",
           list(current_year = 2020L, next_city_index = 995L, completed = list(),
                retry_queue = list(list(year = 2022L, index = 7L))),
           fail_on = "2022:7",
           expect = list(year = 2019L, idx = 1L, status = "partial", queue = 1L)),

  run_case("re-failed retry stays queued, not duplicated",
           list(current_year = 2020L, next_city_index = 1L, completed = list(),
                retry_queue = list(list(year = 2020L, index = 3L))),
           fail_on = "2020:3",
           expect = list(year = 2020L, idx = 10L, status = "partial", queue = 1L))
)

cat(sprintf("\n%d/%d passed\n", sum(results), length(results)))
unlink(SANDBOX, recursive = TRUE)
if (!all(results)) quit(status = 1)
