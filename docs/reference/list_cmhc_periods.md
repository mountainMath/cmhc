# List available time periods for a CMHC table

Queries the CMHC web interface to determine which time periods are
available for a given table. This reveals periods that may not appear in
the "Historical Time Periods" breakdown (e.g., April Rms surveys
2007-2015).

## Usage

``` r
list_cmhc_periods(survey, series, dimension, breakdown, geo_uid = NULL)
```

## Arguments

- survey:

  The CMHC survey, consult \`list_cmhc_surveys()\` for possible values.

- series:

  The CMHC data series, consult \`list_cmhc_series()\` for possible
  values.

- dimension:

  The dimension, consult \`list_cmhc_dimensions()\` for possible values.

- breakdown:

  The geographic breakdown, consult \`list_cmhc_breakdowns()\` for
  possible values.

- geo_uid:

  Optional Census geographic identifier (e.g., CSD "5907047"). If NULL
  (default), uses a default probe geography (Vancouver CMA for CMA-level
  breakdowns, Canada for national-level). Available periods can vary by
  geography — smaller or newer CSDs may have fewer periods.

## Value

A tibble listing available time periods with Year, Month, Quarter,
Season, and Caption columns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Primary Rental Market
list_cmhc_periods("Rms", "Vacancy Rate", "Bedroom Type", "Census Subdivision")
list_cmhc_periods("Rms", "Average Rent", "Bedroom Type", "Provinces")

# Starts and Completions Survey
list_cmhc_periods("Scss", "Starts", "Dwelling Type", "Centres")
list_cmhc_periods("Scss", "Completions", "Intended Market", "Provinces")

# Seniors Housing Survey
list_cmhc_periods("Seniors", "Rental Housing Vacancy Rates", "Unit Type", "Snapshot")

# Census-based tables
list_cmhc_periods("Census", "Income", "Average and Median", "Survey Zones")

# Periods for a specific geography (may differ from the default)
list_cmhc_periods("Rms", "Vacancy Rate", "Bedroom Type", "Census Subdivision",
                  geo_uid = "5907047")
} # }
```
