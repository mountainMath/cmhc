# List available CMHC tables

List available CMHC tables

## Usage

``` r
list_cmhc_tables(short = TRUE)
```

## Arguments

- short:

  Logical, determines how much detail is returned. Default is \`TRUE\`.

## Value

A tibble listing all available CMHC data tables

## Examples

``` r
list_cmhc_tables()
#> # A tibble: 309 × 6
#>    Survey Series Dimension       Breakdown          GeoFilter Filters         
#>    <chr>  <chr>  <chr>           <chr>              <chr>     <list>          
#>  1 Scss   Starts Dwelling Type   Provinces          Default   <named list [2]>
#>  2 Scss   Starts Dwelling Type   Centres            Default   <named list [2]>
#>  3 Scss   Starts Dwelling Type   Survey Zones       Default   <named list [2]>
#>  4 Scss   Starts Dwelling Type   Census Subdivision Default   <named list [2]>
#>  5 Scss   Starts Dwelling Type   Neighbourhoods     Default   <named list [2]>
#>  6 Scss   Starts Dwelling Type   Census Tracts      Default   <named list [2]>
#>  7 Scss   Starts Intended Market Provinces          Default   <named list [2]>
#>  8 Scss   Starts Intended Market Centres            Default   <named list [2]>
#>  9 Scss   Starts Intended Market Survey Zones       Default   <named list [2]>
#> 10 Scss   Starts Intended Market Census Subdivision Default   <named list [2]>
#> # ℹ 299 more rows
```
