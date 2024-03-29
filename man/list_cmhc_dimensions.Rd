% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmhc_tables.R
\name{list_cmhc_dimensions}
\alias{list_cmhc_dimensions}
\title{List available CMHC dimensions}
\usage{
list_cmhc_dimensions(survey = NULL, series = NULL)
}
\arguments{
\item{survey}{Optional survey to filter by}

\item{series}{Optional series to filter by}
}
\value{
A data frame with survey names, series names, and available dimension names.
}
\description{
List available CMHC dimensions
}
\examples{
list_cmhc_dimensions("Rms","Vacancy Rate")

}
