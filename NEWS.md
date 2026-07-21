## cmhc v0.2.12
### Minor changes

* remove dependency on the `aws.s3` package, geographic data is now downloaded directly over HTTPS
* remove a stale hardcoded session cookie from data requests
* fix a broken error message for invalid named `geo_uid` values
* clarify the `refresh` argument documentation in `get_cmhc()`
* silence a spurious many-to-many join warning when building the table list
* memoize the (static) table registry so it is only assembled once per session
* add timeouts to all requests to the CMHC and geography servers
* add a `testthat` test suite covering the parsing and geography helpers

# cmhc v0.2.0

* Initial CRAN release, reworked workflows for a simpler way to access CMHC data tables.

# cmhc v0.2.1

* rename functions to set and show cache path to avoid conflicts with other packages
* add functionality for additional tables

# cmhc v0.2.2

* fix table snafu for Scss by intended market
* add vignette for rental universe

# cmhc v0.2.3

* fix more table sanfu for Scss by intended market
* add query builder helper function
* add access to census and core housing need tables

# cmhc v0.2.4

* fixes an issue with "Rent Ranges" dimension not properly parsing (Thanks Maxime Bélanger De Blois!)
* add access to secondary market tables
* fix table codes for absorbed units
* improved query builder tool

# cmhc v0.2.5

* conditional check of vignettes to account for CMHC server issues

# cmhc v0.2.6

* add tables
* adjust to changes with CMHC portal that require MetId to be specified (only partial fix at this point)
* fix link to CMHC terms of use that moved

## cmhc v0.2.7

* add CMHC lookup table for met codes

## cmhc v0.2.8

* expand tables
* improve handling for national level data
* update to new CMHC internal METCODES

## cmhc v0.2.9
### Minor changes

* adapt to changes to CMHC interface
* intelligent guessing of frequencies for data series retrieval based on temporal arguments supplied

## cmhc v0.2.10
### Minor changes

* enable data access to more tables

## cmhc v0.2.11
### Minor changes

* enable data access to SAAR tables
* more informative error messages when data is not available
* code cleaning to adhere to tidyselect updates
* conveninence function to probe for available time periods for tables

