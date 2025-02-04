## cmhc v0.2.10
### Minor changes

* enable data access to more tables

## cmhc v0.2.9
### Minor changes

* adapt to changes to CMHC interface
* intelligent guessing of frequencies for data series retrieval based on temporal arguments supplied

## cmhc v0.2.8
### Minor changes

* expand tables
* improve handling for national level data
* update to new CMHC internal METCODES

## cmhc v0.2.7
### Minor changes

* add CMHC lookup table for met codes


## cmhc v0.2.6
### Minor changes

* add tables
* adjust to changes with CMHC portal that require MetId to be specified (only partial fix at this point)
* fix link to CMHC terms of use that moved

## cmhc v0.2.5
### Minor changes

* conditional check of vignettes to account for CMHC server issues

## cmhc v0.2.4
### Minor changes

* fixes an issue with "Rent Ranges" dimension not properly parsing
* add access to secondary market tables
* fix table codes for absorbed units
* improved query builder tool

## cmhc v0.2.3
### Minor changes

* fix more table snafu for Scss by intended market
* add query builder helper function
* add access to census and core housing need tables

## cmhc v0.2.2
### Minor changes

* fix table sanfu for Scss by intended market
* add vignette for rental universe

## cmhc v0.2.1
### Minor changes

* add functionality for additional tables
* rename functions to set and show cache path to avoid conflicts with other packages

## cmhc v0.2.0

* This is a new release.
Local checks on R version 4.2
Standard GitHub action checks on macOS-latest, windows-lastest, ubuntu-latest (release, devel, oldrel-1)
0 errors | 0 warnings | 0 notes

* Cleaned up the Rd files for better and more consistent documentation.
* Clarified that by default data is only downloaded to tempdir(), if spatial data is also needed a local cache path
needs to be set explicitly in order to download and cache the data permanenetly.
* Adding a link to the data source in angled brackets in the description field of the DESCRIPTION file by appending <https://www03.cmhc-schl.gc.ca/hmip-pimh/en> triggered a note in the automatic CRAN checks "Malformed Description field: should contain one or more complete sentences.", possibly due to the addition of the URL messing up the parsing of the field. Instead of adding the link to the description field I have added it to the URL section.

