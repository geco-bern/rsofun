[![R build status](https://github.com/geco-bern/rsofun/workflows/R-CMD-check/badge.svg)](https://github.com/geco-bern/rsofun/actions)
[![codecov](https://codecov.io/gh/geco-bern/rsofun/branch/master/graph/badge.svg)](https://codecov.io/gh/geco-bern/rsofun)

# rsofun

An R Simulating Optimal FUNctioning (RSOFUN) framework for site-scale simulations of ecosystem processes. The package contains the following modules:

- P-model for leaf-level acclimation of photosynthesis from [Stocker et al. (2019)](https://www.geosci-model-dev-discuss.net/gmd-2019-200/).
- SPLASH for bioclimatic variables, including the surface radiation budget and the soil water balance from [Davis et al. (2017)](https://doi.org/10.5194/gmd-10-689-2017).
- BiomeE for comprehensive simulations of ecosystem carbon and water cycling, tree growth, and tree cohort-explicit forest dynamics following the Perfect Plasticity Approximation, from [Weng et al., (2015)](https://doi.org/10.5194/bg-12-2655-2015).

## Installation

### stable release

To install the current stable release use a CRAN repository:

``` r
install.packages("rsofun")
library("rsofun")
```

### development release

To install the development releases of the package run the following
commands:

``` r
if(!require(remotes)){install.packages("remotes")}
remotes::install_github("bluegreen-labs/rsofun")
library("rsofun")
```

Vignettes are not rendered by default, if you want to include additional
documentation please use:

``` r
if(!require(remotes)){install.packages("remotes")}
remotes::install_github("bluegreen-labs/rsofun", build_vignettes = TRUE)
library("rsofun")
```

## References

Stocker, B. D., Wang, H., Smith, N. G., Harrison, S. P., Keenan, T. F., Sandoval, D., Davis, T., and Prentice, I. C.: P-model v1.0: an optimality-based light use efficiency model for simulating ecosystem gross primary production, Geosci. Model Dev., 13, 1545–1581, https://doi.org/10.5194/gmd-13-1545-2020, 2020.

Davis, T. W., Prentice, I. C., Stocker, B. D., Thomas, R. T., Whitley, R. J., Wang, H., Evans, B. J., Gallego-Sala, A. V., Sykes, M. T., and Cramer, W.: Simple process-led algorithms for simulating habitats (SPLASH v.1.0): robust indices of radiation, evapotranspiration and plant-available moisture, Geoscientific Model Development, 10, 689–708, doi:10.5194/gmd-10-689-2017, URL http: //www.geosci-model-dev.net/10/689/2017/, 2017.

Weng, E. S., Malyshev, S., Lichstein, J. W., Farrior, C. E., Dybzinski, R., Zhang, T., Shevliakova, E., and Pacala, S. W.: Scaling from individual trees to forests in an Earth system modeling framework using a mathematically tractable model of height-structured competition, Biogeosciences, 12, 2655–2694, https://doi.org/10.5194/bg-12-2655-2015, 2015.

## Acknowledgements

The {rsofun} is part of the LEMONTREE project and funded by Schmidt Futures and under the umbrella of the Virtual Earth System Research Institute (VESRI).