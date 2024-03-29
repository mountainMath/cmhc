% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_settings.R
\name{set_cmhc_cache_path}
\alias{set_cmhc_cache_path}
\title{Set persistent cmhc cache location for geographic data}
\usage{
set_cmhc_cache_path(cache_path, overwrite = FALSE, install = FALSE)
}
\arguments{
\item{cache_path}{a local directory to use for saving cached data}

\item{overwrite}{Option to overwrite any existing cache path already stored locally.}

\item{install}{Option to install permanently for use across sessions.}
}
\value{
a character string with the CMHC cache path
}
\description{
The cmhc package provides access to custom cmhc geographies, these are large files and should be
stored in a permanent location. This function sets the CMHC_CACHE_PATH environment variable and optionally
installs it in the .Renviron file for future use. This is only needed when using the `get_cmhc_geography()` function.
}
\examples{
\dontrun{
# This sets the cache path for the duration of the current session
set_cmhc_cache_path("~/cmhc_cache")

# This will set the cache path permanently until overwritten again
set_cmhc_cache_path("~/cmhc_cache", install = TRUE)
}

}
