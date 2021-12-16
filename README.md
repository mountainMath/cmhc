# cmhc
Wrapper for hack into CMHC data

To discover table parameters, open CMHC data portal and set breakpoint on line 74 of asset TableMapChart.js. Then "export" table, type and
inspect variable "model" in the console to read off the parameters.

## Documentation
Documentation is somewhat sparce, but [available on the GitHub pages](https://mountainmath.github.io/cmhc/).

Check you the example vignettes for common use cases.

##Usage
```
remotes::install_github("mountainmath/cmhc")
library(cmhc)
get_cmhc(...)
```


