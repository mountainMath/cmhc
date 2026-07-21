# List available CMHC dimensions

List available CMHC dimensions

## Usage

``` r
list_cmhc_dimensions(survey = NULL, series = NULL)
```

## Arguments

- survey:

  Optional survey to filter by

- series:

  Optional series to filter by

## Value

A data frame with survey names, series names, and available dimension
names.

## Examples

``` r
list_cmhc_dimensions("Rms","Vacancy Rate")
#> # A tibble: 5 × 3
#>   Survey Series       Dimension           
#>   <chr>  <chr>        <chr>               
#> 1 Rms    Vacancy Rate Bedroom Type        
#> 2 Rms    Vacancy Rate Year of Construction
#> 3 Rms    Vacancy Rate Structure Size      
#> 4 Rms    Vacancy Rate Rent Ranges         
#> 5 Rms    Vacancy Rate Rent Quartiles      
```
