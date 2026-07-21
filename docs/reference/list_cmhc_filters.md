# List available CMHC filters

List available CMHC filters

## Usage

``` r
list_cmhc_filters(
  survey = NULL,
  series = NULL,
  dimension = NULL,
  breakdown = NULL
)
```

## Arguments

- survey:

  Optional survey to filter by

- series:

  Optional series to filter by

- dimension:

  Optional dimension to filter by

- breakdown:

  Optional breakdown to filter by

## Value

A data frame with available filters

## Examples

``` r
list_cmhc_filters("Rms","Vacancy Rate","Bedroom Type","Historical Time Periods")
#> # A tibble: 1 × 5
#>   Survey Series       Dimension    Breakdown               Filters         
#>   <chr>  <chr>        <chr>        <chr>                   <list>          
#> 1 Rms    Vacancy Rate Bedroom Type Historical Time Periods <named list [2]>
```
