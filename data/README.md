# Data

The analysis uses the **NOAA HURDAT2** Atlantic-basin best-track dataset.

## Source

* **Provider:** NOAA National Hurricane Center
* **URL:** <https://www.nhc.noaa.gov/data/#hurdat>
* **Format:** HURDAT2 fixed-width text
* **Coverage:** 1851–2025 (most recent archive file as of writing)

## `fetch_data.R`

```bash
Rscript data/fetch_data.R
```

Downloads the latest HURDAT2 Atlantic file from NOAA, parses it into a tidy
data frame, and saves it to `data/hurdat2_tidy.RData`. The file is
gitignored — re-run the script to refresh it.

The output `hurrs` is a `data.frame` with one row per storm observation:

| Column     | Type      | Description |
|------------|-----------|-------------|
| `id`        | character | Storm id, e.g. `AL122005` (basin + number + year) |
| `name`      | character | Storm name, e.g. `KATRINA`, or `UNNAMED` |
| `year`      | integer   | Year of the observation |
| `month`     | integer   | Month (1–12) |
| `longitude` | numeric   | Decimal degrees, negative west of the prime meridian |
| `latitude`  | numeric   | Decimal degrees, negative south of the equator |
| `windspeed` | numeric   | Sustained wind in knots (`NA` where the source value is `-999`) |
| `isLandfall` | logical  | `TRUE` where the HURDAT2 record identifier is `L` |

## Citation

If you publish work using this dataset, cite NOAA / NHC:

> National Hurricane Center (NHC). HURDAT2 Atlantic basin hurricane database.
> <https://www.nhc.noaa.gov/data/#hurdat>

Format reference: <https://www.nhc.noaa.gov/data/hurdat/hurdat2-format-atlantic.pdf>
