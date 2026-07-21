# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Overview

`cmhc` is an R package (CRAN-published) that wraps the Canadian Mortgage
and Housing Corporation (CMHC) Housing Market Information Portal (HMIP)
web interface, giving programmatic and reproducible access to CMHC
housing data. It is designed to interoperate with `cancensus`, `cansim`,
and `tongfen` via a shared `GeoUID` column keyed on Census geographies.

## Development commands

This is a standard R package built with `roxygen2` and checked with
`R CMD check`. There is no test suite (`tests/` does not exist); CI
(`.github/workflows/R-CMD-check.yaml`) runs `R CMD check` across
macOS/Windows/Linux and several R versions.

``` r

# Regenerate NAMESPACE and man/*.Rd from roxygen comments — run after editing any roxygen block
devtools::document()      # or roxygen2::roxygenise()

# Load the package for interactive work without installing
devtools::load_all()

# Full CRAN-style check (what CI runs)
devtools::check()         # or R CMD check from the shell after R CMD build

# Build vignettes / pkgdown site
devtools::build_vignettes()
pkgdown::build_site()
```

There are no lint or unit-test commands; correctness is validated by
`R CMD check` and by running the example queries in the vignettes
(`vignettes/`), which hit the live CMHC server.

## Architecture

### The core request flow (`R/cmhc.R`)

[`get_cmhc()`](https://mountainmath.github.io/cmhc/reference/get_cmhc.md)
is the single entry point for fetching data. A call specifies a CMHC
table by the four-part key **survey → series → dimension → breakdown**
(plus `geoFilter`), and a Census `geo_uid`. The flow:

1.  Look up the matching row in the table registry via
    `list_cmhc_tables(short=FALSE)`, validating each key part in turn
    and producing progressively specific error messages.
2.  Translate the Census `geo_uid` into CMHC’s internal geography
    parameters (`geography_id`, `geography_type_id`, `MetId`) via
    `cmhc_region_params_from_census()`. Named `geo_uid` vectors
    (e.g. `c(CMA=..., Neighbourhood=...)`) select sub-metro regions.
3.  Apply table-code special cases (national-level tables,
    `Starts (SAAR)` geography rules).
4.  POST a form to `.../ExportTable` requesting `exportType="csv"`,
    writing to a per-query cache file in
    [`tempdir()`](https://rdrr.io/r/base/tempfile.html) keyed by
    `digest::digest(query_params)` (skip the network if cached unless
    `refresh=TRUE`).
5.  Parse the CMHC CSV (note: **latin1-encoded, not UTF-8**; comma
    thousands-separators inside `$` amounts; `—`/`++`/`n/a`/`**`
    sentinel values handled by `parse_numeric()`), split the value
    columns from their paired `- Quality` columns, and `pivot_longer`
    into a tidy long tibble. Region/title/subtitle from the CSV header
    become tibble [`attr()`](https://rdrr.io/r/base/attr.html)s.

Two request shapes: **snapshot** (pass `year`/`month`/`quarter`) vs
**time series** (`breakdown="Historical Time Periods"`, optionally with
`frequency`). Frequency is inferred from whichever temporal argument is
supplied.

### The table registry (`R/cmhc_tables.R`)

There is **no live table-discovery API** — the full catalogue of
available CMHC tables is hardcoded as
[`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html)
literals inside
[`list_cmhc_tables()`](https://mountainmath.github.io/cmhc/reference/list_cmhc_tables.md),
mapping each survey/series/dimension/breakdown/geoFilter combination to
a CMHC `TableCode` (e.g. `"1.1.1.9"`) and its available filters.
**Adding support for a new CMHC table means adding rows here.** The
`list_cmhc_*()` discovery functions (`_surveys`, `_series`,
`_dimensions`, `_breakdowns`, `_filters`) and the interactive
[`select_cmhc_table()`](https://mountainmath.github.io/cmhc/reference/select_cmhc_table.md)
query builder all read from this same registry.
[`list_cmhc_periods()`](https://mountainmath.github.io/cmhc/reference/list_cmhc_periods.md)
is the exception — it probes the live server for available time periods.

### Geography translation (`R/cmhc_geography.R`)

Converts between Census `GeoUID`s and CMHC’s internal geocodes at CMA /
CSD / CT / Zone / Neighbourhood levels, using the bundled translation
tables. Geographic level is inferred from the number of characters in
the id.
[`get_cmhc_geography()`](https://mountainmath.github.io/cmhc/reference/get_cmhc_geography.md)
/ `download_geographies()` fetch large (~55 MB) CMHC-specific spatial
files (Survey Zones, Neighbourhoods) from a public S3 bucket over plain
HTTPS (via `httr` and the S3 REST list/get API — see
`list_s3_bucket_keys()`) into the `CMHC_CACHE_PATH` cache directory
(managed by `R/user_settings.R`). This cache is only for spatial
geographies — ordinary
[`get_cmhc()`](https://mountainmath.github.io/cmhc/reference/get_cmhc.md)
data uses [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

### Bundled data (`data/*.rda`, documented in `R/helpers.R`)

The `cmhc_cma_translation_data`, `cmhc_csd_translation_data`,
`cmhc_csd_translation_data_2023`, and `cmhc_ct_translation_data`
datasets are the Census↔︎CMHC geocode lookup tables consumed by the
geography functions. They are generated from the raw inputs in
`data_raw/` (CMHC code books / OLAP extracts); regenerate the `.rda`
files if those change. `cmhc_quality_labels` maps CMHC’s single-letter
quality codes to plain text.

## Conventions

- Exported functions carry roxygen doc comments; internal helpers use
  `@noRd`. Always run
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  after changing any roxygen block — `NAMESPACE` and `man/` are
  generated, never edit by hand.
- Uses base-R pipe `|>` (package `Depends: R >= 4.1`) in newer code and
  magrittr `%>%` in older code; both appear. Column references inside
  dplyr verbs use `.data$Column` to satisfy `R CMD check`.
- Version lives in `DESCRIPTION`; user-facing changes go in `NEWS.md`.
  The current dev branch is named after the target version
  (e.g. `v0.2.11`); `master` is the release branch.
