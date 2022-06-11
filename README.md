# cmhc

<!-- badges: start -->
[![R-CMD-check](https://github.com/mountainMath/cmhc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mountainMath/cmhc/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/cmhc)](https://cran.r-project.org/package=cmhc)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/cmhc)](https://cranlogs.r-pkg.org/badges/cmhc)
<!-- badges: end -->


API wrapper for extracting CMHC data out of the [CMHC Housing Market Information Portal](https://www.cmhc-schl.gc.ca/hmiportal).


### Reference
Documentation is [available on the GitHub pages](https://mountainmath.github.io/cmhc/).

The example vignettes contain some [common use cases](https://mountainmath.github.io/cmhc/articles/basic_usage.html).

## Installation
```
remotes::install_github("mountainmath/cmhc")
```

## Usage
Consult the example vignette for more information. As an example, this is how to extract time series information
for vacancy rate data by bedroom type for the Vancouver Census Metropolitan Area ("59933").

```
library(cmhc)
vacancy_data <- get_cmhc(survey="Rms",series="Vacancy Rate",dimension="Bedroom Type",
                         breakdown="Historical Time Periods",  geo_uid="59933")

```

## Contributing

* We encourage contributions to improve this project. The best way is through issues and pull requests.
* If you want to get in touch, we are pretty good at responding via email or via twitter at [@vb_jens](https://twitter.com/vb_jens). 

### Cite **cmhc**

If you wish to cite cmhc:

  von Bergmann, J. cmhc: R package to
  access, retrieve, and work with CMHC data. v0.2.0.


A BibTeX entry for LaTeX users is
```
  @Manual{,
    author = {Jens {von Bergmann}},
    title = {cmhc: R package to access, retrieve, and work with CMHC data},
    year = {2022},
    note = {R package version 0.2.0},
    url = {https://mountainmath.github.io/cmhc/},
  }
```
### Related packages

The cmhc package is designed to work well with the [cancensus package](https://mountainmath.github.io/cancensus/) working with Canadian Census data and matches the census geographies.
