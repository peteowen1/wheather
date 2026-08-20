# wheather

Was this summer actually better than last summer, or does it just feel that way?

`wheather` is an R package that scores each day's weather on a 0-100 "goodness" scale
from five components, then compares two periods — different years in one city, or two
cities over the same dates.

```r
# install.packages("remotes")
remotes::install_github("peteowen1/wheather")

library(wheather)

compare_periods(
  city   = "Sydney",
  start1 = "2024-12-01", end1 = "2025-02-28",
  start2 = "2023-12-01", end2 = "2024-02-29"
)
```

There is also a Shiny app for poking at the weights interactively:

```r
wheather::run_app()
```

## How a day is scored

Five components, each 0-100, combined with configurable weights. If a component is
missing for a day, its weight is redistributed across the others so the total still
scales 0-100.

| Component | Default weight | What it looks at |
|---|---|---|
| `temp` | 25% | Max, mean and min against separate ideal ranges (max weighted heaviest) |
| `rain` | 25% | Exponential decay on total mm, plus penalties for duration and snowfall |
| `sky` | 20% | Sunshine duration, cloud cover, and solar radiation |
| `humidity` | 10% | Ideal 40-60%, with muggy penalised harder than dry |
| `wind` | 20% | Sustained wind, plus a gust penalty |

Both the weights and the thresholds are arguments — see `default_weights()` and
`default_params()`. "Good weather" here means *a nice day outside*, which is a
judgement, not a fact; change the numbers if you disagree.

## Caveats worth knowing before you quote a p-value

- `compare_periods()` reports Welch's t-test and Cohen's d, which assume independent
  observations. **Consecutive days of weather are not independent** — a wet week is one
  weather system, not seven draws. The effective sample size is well below the number of
  days, so p-values are optimistic and "significantly better" fires more readily than it
  should. Treat the mean difference as the real output and the p-value as decoration.
- The `sky` component's radiation sub-score is normalised against the brightest day in
  the data being scored unless you set `params$sky$radiation_max` explicitly. That makes
  scores comparable *within* one call but not across separate calls.
- Nothing checks that the two periods you compare are the same length or the same season.
  Comparing December to June will happily produce a confident verdict.

## Data

Weather data comes from the [Open-Meteo Historical Weather API](https://open-meteo.com/),
which is free and requires no API key. Open-Meteo's historical data is derived from
ERA5 and ERA5-Land reanalysis produced by [Copernicus Climate Change Service /
ECMWF](https://climate.copernicus.eu/).

Open-Meteo data is licensed **[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)**.
The cached data published by this repository is derived from it and carries the same
attribution requirement — if you use it, credit Open-Meteo and ECMWF/Copernicus.

Responses are cached as Parquet under `~/.wheather/cache` (configurable via
`options(wheather.cache_dir)`), so re-running a comparison costs no API calls.

### Bulk cache

A daily GitHub Actions run backfills the 1000 most populated cities, working backwards
from 2025. The result is **not stored in git** — it is published as a single asset on
the [`cache` release](https://github.com/peteowen1/wheather/releases/tag/cache):

```bash
gh release download cache --pattern cache.tar.gz --repo peteowen1/wheather
tar -xzf cache.tar.gz -C data     # lands at data/cache/{lat}_{lon}.parquet
```

Coverage as of August 2026: 2018-2025 complete for ~1000 cities, 2017 in progress.
A handful of city-years are missing where an early version of the batch job advanced
its progress pointer past failed runs; see `CLAUDE.md` for the specifics.

## Development

```r
devtools::load_all()
devtools::test()
devtools::document()   # never hand-edit NAMESPACE
devtools::check()
```

`data.table` throughout — please don't introduce dplyr.

## License

MIT, see [LICENSE](LICENSE). This covers the code; the cached weather data is CC BY 4.0
as described above.
