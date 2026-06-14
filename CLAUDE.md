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
3. **R/cache.R** — Parquet files at `~/.wheather/cache/{lat}_{lon}.parquet` (configurable via `options(wheather.cache_dir)`). Uses `arrow::ReadableFile` (not mmap) to avoid Windows file-locking.
4. **R/score.R** — Five `score_*()` functions (temp, rain, sky, humidity, wind), combined by `weighted_total()` with NA renormalisation. `score_period()` adds `score_*` columns to the data.table.
5. **R/compare.R** — `compare_periods()` and `compare_cities()` both accept city names. Shared `build_comparison()` helper. Returns S3 `wheather_comparison` with `print()` method that returns a debug data.table invisibly.
6. **R/batch.R** — `top_cities(n)` from `maps::world.cities`. `batch_fetch()` handles rate limits, backoff, and progress tracking.

### Shiny App (`inst/shiny/app.R`)

Uses bslib (Bootstrap 5, "flatly" theme). Three tabs: Overview (verdict + timeline), Components (bar chart + faceted timelines), Data (DT table). Weight sliders are in the sidebar; weights should sum to 1.0 (validated with a warning).

### Key Dependencies

- `httr2` for API calls (with retry)
- `data.table` throughout (not tibble/dplyr)
- `arrow` for Parquet cache I/O
- `cli` for user-facing messages
- `ggplot2`, `shiny`, `bslib`, `bsicons`, `DT` for the app

## Conventions

- **data.table idiom everywhere** — use `:=`, `.SD`, `rbindlist`, etc. Do not introduce dplyr.
- **roxygen2 with markdown** — all exported functions have roxygen docs; internal helpers use `@noRd`.
- **NAMESPACE is auto-generated** — never edit directly; run `devtools::document()`.
- Open-Meteo API is free and keyless — the `.Renviron.example` referencing `OPENWEATHER_API_KEY` is a leftover and not used by the code.
- Scoring functions are intentionally simple and pure (no side effects) for easy testing.

## Batch Data Collection

`top_cities(1000)` returns the 1000 most populated cities worldwide (from the `maps` package). `batch_fetch()` fetches historical weather for multiple cities with rate-limit handling.

**Scheduled agent** (`wheather-batch-fetch`): A Claude Code remote trigger runs daily at 3am Sydney time, fetching 60 cities × 1 year per run. It tracks progress in `data/batch_progress.json`, caches parquet files in `data/cache/`, and commits/pushes after each run. Working backwards from 2025 to 2015. Manage at: https://claude.ai/code/scheduled

> **CURRENT STATUS (stalled):** The batch agent is presently blocked — the proxy allowlist does not permit `open-meteo.com`, so every run fails its Open-Meteo fetch (`last_run_error` 403). Progress has been stuck for ~8 weeks (around 2024, cities 361-420) and will not advance until the proxy allows `open-meteo.com`.

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
