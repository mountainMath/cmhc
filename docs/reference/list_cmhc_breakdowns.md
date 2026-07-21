# List available CMHC breakdowns

List available CMHC breakdowns

## Usage

``` r
list_cmhc_breakdowns(survey = NULL, series = NULL, dimension = NULL)
```

## Arguments

- survey:

  Optional survey to filter by

- series:

  Optional series to filter by

- dimension:

  Optional dimension to filter by

## Value

A data frame with survey names, series names, dimension names and
available series breakdowns.

## Examples

``` r
list_cmhc_breakdowns("Rms","Vacancy Rate","Bedroom Type")
#> # A tibble: 7 × 4
#>   Survey Series       Dimension    Breakdown              
#>   <chr>  <chr>        <chr>        <chr>                  
#> 1 Rms    Vacancy Rate Bedroom Type Provinces              
#> 2 Rms    Vacancy Rate Bedroom Type Centres                
#> 3 Rms    Vacancy Rate Bedroom Type Survey Zones           
#> 4 Rms    Vacancy Rate Bedroom Type Census Subdivision     
#> 5 Rms    Vacancy Rate Bedroom Type Neighbourhoods         
#> 6 Rms    Vacancy Rate Bedroom Type Census Tracts          
#> 7 Rms    Vacancy Rate Bedroom Type Historical Time Periods
```
