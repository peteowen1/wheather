# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

`wheather` is an R package that scores daily weather "goodness" (0-100) using configurable weights across 5 components (temperature, rain, sky, humidity, wind), then statistically compares periods across years using Welch's t-test and Cohen's d. Includes a Shiny app for interactive exploration.

## Build & Development Commands

```bash
# Install package locally (from the project root)
R CMD INSTALL .

# Or from within R:
devtools::install()
devtools::load_all()       # load without installing (iterative dev)
devtools::document()       # regenerate NAMESPACE and man/ from roxygen2
devtools::check()          # full R CMD check

# Run tests
devtools::test()           # all tests
testthat::test_file("tests/testthat/test-score.R")  # single file

# Launch Shiny app
wheather::run_app()
```

## Architecture

### Data Pipeline

`geocode()` → `fetch_weather()` → `score_period()` → `compare_periods()`

1. **R/locations.R** — `geocode()` resolves city names to lat/lon. `resolve_location()` accepts either city name or lat/lon. Built-in table of 15 cities + Open-Meteo geocoding fallback.
2. **R/api.R** — `fetch_weather()` checks Parquet cache first, fetches only missing dates from Open-Meteo's Historical Weather API, merges, and re-caches. 5 retries with exponential backoff on 429s.
3. **R/cache.R** — Parquet files at `{cache_dir}/{lat}_{lon}.parquet`, where `cache_dir()` defaults to `tools::R_user_dir("wheather", "cache")` — the OS user-cache directory, **not** `~/.wheather/cache` (configurable via `options(wheather.cache_dir)`). Uses `arrow::ReadableFile` (not mmap) to avoid Windows file-locking.
4. **R/score.R** — Five `score_*()` functions (temp, rain, sky, humidity, wind), combined by `weighted_total()` with NA renormalisation. `score_period()` adds `score_*` columns to the data.table.
5. **R/compare.R** — `compare_periods()` and `compare_cities()` both accept city names. Shared `build_comparison()` helper. Returns S3 `wheather_comparison` with `print()` method that returns a debug data.table invisibly.
6. **R/batch.R** — `top_cities(n)` from `maps::world.cities`. `batch_fetch()` handles rate limits, backoff, and progress tracking.

### Shiny App (`inst/shiny/app.R`)

Uses bslib (Bootstrap 5, "flatly" theme). Three tabs: Overview (verdict + timeline), Components (bar chart + faceted timelines), Data (DT table). Weight sliders are in the sidebar; weights should sum to 1.0 (validated with a warning).

## Conventions

- **data.table idiom everywhere** — use `:=`, `.SD`, `rbindlist`, etc. Do not introduce dplyr.
- **roxygen2 with markdown** — all exported functions have roxygen docs; internal helpers use `@noRd`.
- **NAMESPACE is auto-generated** — never edit directly; run `devtools::document()`.
- Open-Meteo API is free and keyless — the `.Renviron.example` referencing `OPENWEATHER_API_KEY` is a leftover and not used by the code.
- Scoring functions are intentionally simple and pure (no side effects) for easy testing.

## Batch Data Collection

`top_cities(1000)` returns the 1000 most populated cities worldwide (from the `maps` package). `batch_fetch()` fetches historical weather for multiple cities with rate-limit handling.

**Scheduled run** (`.github/workflows/batch-fetch.yml`): runs daily at 17:00 UTC, fetching 60 cities × 1 year per run, working backwards from 2025 to 2015. The cron is fixed in UTC and does not follow Sydney's daylight saving, so that is 3am during AEST (April-October) and 4am during AEDT (October-April). Manage at: https://claude.ai/code/scheduled

**Where the data lives.** The parquet cache is **not in git** — it is a single `cache.tar.gz` asset on the `cache` GitHub Release, per the release-as-data-bus pattern. Each run restores it, fetches, and re-uploads with `--clobber`. Two consequences worth knowing:

- The release `createdAt` is meaningless; check the **asset** `updatedAt`.
- **The expected file count lives in `batch_progress.json` (`cache_files`), not in the release API.** A failed `gh` query and a genuinely absent asset look identical from the API alone, so trusting it would let one transient outage restore nothing and then publish that nothing over a good archive. The restore step hard-fails whenever it unpacks fewer files than the committed count; a cold start is legitimate only when that count is 0. The upload step re-checks the same invariant before `--clobber`.

To work with the cache locally: `gh release download cache --pattern cache.tar.gz && tar -xzf cache.tar.gz -C data`.

**Progress accounting.** `data/batch_progress.json` is the only thing committed (to `dev`). If a run succeeds for zero cities, the pointer **holds** rather than advancing — a systematic failure (proxy block, quota, outage) must not march through the city list leaving gaps nothing retries. **Partial failures are still skipped:** when some cities succeed, the pointer advances past the ones that did not, and they are recorded only in `last_run_error`. There is no retry queue yet, so a `partial` status is a signal to re-run those cities by hand. `last_run_error` is rewritten on every run that reaches the progress write, so it cannot disagree with `last_run_status`, and it records every distinct failure reason rather than only the first. The one exception is the terminal `current_year < 2015` path, which `quit()`s before writing anything and leaves the last value in place.

> **CURRENT STATUS (2026-08-20):** The pipeline is running normally, working through 2017. Read the live position from `completed` / `next_city_index` in `data/batch_progress.json` rather than trusting a number written here — it moves every day.
>
> **Read the pointer from the branch CI writes to, after fetching — never from a local checkout.** `data/batch_progress.json` moves on every run. Before this change CI wrote it to `main`; from this change on it writes to `dev`, so `main` is the copy that goes stale between merges. Either way a checkout can sit a hundred-plus runs behind and make the pipeline look stalled when it is not — that misreading happened on 2026-08-20 and very nearly published a one-year-old cache snapshot over nine years of data. `git fetch` first, then read `origin/dev:data/batch_progress.json`.
>
> **Gaps from the pointer bug: backfilled 2026-08-20.** `completed` had read 2022=940 (commit `f1fed20` — cities 181-240, all 60 failed and the pointer advanced anyway), 2020=999, 2018=999, 2025=996. All 66 city-years were re-fetched directly by deriving lat/lon from the cache filenames, which is exactly the request the batch job makes, and coverage was then re-verified by reading the Parquet files rather than trusting the counters. 2018-2025 is now 1000/1000 for every year. If it happens again, that is the method: scan the files for `(city, year)` pairs with no rows, re-fetch those, re-scan. `fetch_weather()` only requests dates it does not already hold, so it is idempotent and cheap.
>
> The Claude Code remote trigger (`batch_run.R`) is separately blocked by a proxy allowlist that does not permit `open-meteo.com` (403). That block does not affect the GitHub Actions run, which is what has been making progress.

**Open-Meteo rate limits** are weighted, not per-request: `weight = max(vars/10, vars/10 * days/7)`. With 16 variables, 1 year ≈ 83 call-equivalents. Free tier = 10,000/day, so ~120 city-years per day max. Keep batch sizes ≤60 cities per run.

## Scoring System (v2)

5 components (not 6 — cloud and sunshine were merged into "sky"):
- **temp** (25%): weighted avg of max/mean/min scores (40/35/25) against separate ideal ranges
- **rain** (25%): exponential decay on mm + duration penalty + snowfall penalty
- **sky** (20%): 50% sunshine duration + 30% cloud cover (sweet spot 10-25%) + 20% radiation (auto-scaled)
- **humidity** (10%): ideal 40-60%, asymmetric decay (muggy penalised 2x harder than dry)
- **wind** (20%): sustained score + up to 40pt gust penalty

NA scores are renormalised (weights redistributed to non-NA components).

## Note on the methodology doc

`inst/quarto/methodology.qmd` documents the scoring math and is excluded from the package build (via `.Rbuildignore`). It is outdated — the code is authoritative for current scoring logic.
