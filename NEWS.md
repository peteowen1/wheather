# wheather 0.1.4

Repository made public. Batch pipeline reworked after a review found it was
losing city-years silently.

## Batch pipeline

* **Failures are no longer discarded.** `batch_run.R` incremented its failure
  counters inside a `tryCatch` handler, so the writes landed in the handler's
  own scope and vanished. A run where all 60 cities failed reported
  "Success: 0, Failed: 0".
* **The progress pointer no longer skips failed work.** It previously advanced
  unconditionally, so a systematic failure marched through the city list leaving
  gaps nothing retried. Commit `f1fed20` lost 60 city-years of 2022 that way.
  A run that succeeds for zero cities now holds its pointer, and cities that
  fail go onto a persistent `retry_queue` which the next run drains before
  taking new work. Queue entries carry their own year, so they survive a
  year rollover.
* **One implementation.** The workflow carried an inline copy of the fetch logic
  that had drifted from `batch_run.R`; only one of the two had the 429 circuit
  breaker. The workflow now calls the script.
* `completed` counts are derived from the Parquet files rather than tallied as
  runs succeed, so they cannot drift from actual coverage.
* `last_run_error` is rewritten every run and records every distinct failure
  reason, rather than only the first.
* A retry-queue overflow permanently drops city-years, so it is now recorded in
  `last_run_error` instead of only warning to the run log.
* Restored the cap that stops the fetcher requesting dates past the archive's
  multi-day lag when working on an in-progress calendar year. The inline copy in
  the workflow had this guard and the merged script briefly lost it.
* Fixed two pointer bugs found while building the queue: an entry present in both
  the queue and the new range was counted twice and skipped a city, and retries
  selected for a run but never reached by the 429 circuit breaker were dropped
  entirely.
* `tools/test_batch_queue.R` covers the queue and pointer logic against a stubbed
  fetcher, including both circuit-breaker paths.

## Data

* The Parquet cache moved out of git onto the `cache` GitHub Release. It was
  being committed to `main` on every run; one file per city holding every year
  meant each yearly append rewrote the whole file and stored a fresh blob.
* The 66 city-years lost to the pointer bug were backfilled. 2018-2025 is now
  complete for 1000 cities, verified by reading the Parquet files rather than
  trusting the counters.

## Documentation

* Added a README, and the MIT licence text, which the repository previously
  claimed without shipping.
* Corrected the documented cache location. `R/cache.R`, `CLAUDE.md` and the
  README all said `~/.wheather/cache`; `cache_dir()` actually returns
  `tools::R_user_dir("wheather", "cache")`.
* Documented that `compare_periods()` applies Welch's t-test to consecutive
  daily scores, which are not independent observations, so its p-values are
  optimistic.
