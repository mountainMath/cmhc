# Set persistent cmhc cache location for geographic data

The cmhc package provides access to custom cmhc geographies, these are
large files and should be stored in a permanent location. This function
sets the CMHC_CACHE_PATH environment variable and optionally installs it
in the .Renviron file for future use. This is only needed when using the
\`get_cmhc_geography()\` function.

## Usage

``` r
set_cmhc_cache_path(cache_path, overwrite = FALSE, install = FALSE)
```

## Arguments

- cache_path:

  a local directory to use for saving cached data

- overwrite:

  Option to overwrite any existing cache path already stored locally.

- install:

  Option to install permanently for use across sessions.

## Value

a character string with the CMHC cache path

## Examples

``` r
if (FALSE) { # \dontrun{
# This sets the cache path for the duration of the current session
set_cmhc_cache_path("~/cmhc_cache")

# This will set the cache path permanently until overwritten again
set_cmhc_cache_path("~/cmhc_cache", install = TRUE)
} # }
```
