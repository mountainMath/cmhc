## cmhc v0.2.4
### Minor changes

* fixes an issue with "Rent Ranges" dimension not properly parsing

## cmhc v0.2.3
### Minor changes

* fix more table sanfu for Scss by intended market
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

