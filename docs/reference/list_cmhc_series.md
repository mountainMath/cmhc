# List available CMHC series

List available CMHC series

## Usage

``` r
list_cmhc_series(survey = NULL)
```

## Arguments

- survey:

  Optional survey to filter by

## Value

A data frame with survey names, and available series names.

## Examples

``` r
list_cmhc_series("Rms")
#> # A tibble: 7 × 2
#>   Survey Series             
#>   <chr>  <chr>              
#> 1 Rms    Vacancy Rate       
#> 2 Rms    Availability Rate  
#> 3 Rms    Average Rent       
#> 4 Rms    Average Rent Change
#> 5 Rms    Median Rent        
#> 6 Rms    Rental Universe    
#> 7 Rms    Summary Statistics 
```
