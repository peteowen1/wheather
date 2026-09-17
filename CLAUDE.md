# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

```r
devtools::load_all()      # load for development
devtools::document()      # regenerate NAMESPACE/docs after changing exported functions
devtools::test()          # tests/testthat/ has test-score.R, test-utils.R
devtools::check()
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

See the `batch-data-collection` skill for the pipeline schedule, cache/release mechanics, progress accounting, and known pointer-staleness gotchas.

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
