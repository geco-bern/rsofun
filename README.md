[![R build status](https://github.com/computationales/rsofun/workflows/R-CMD-check/badge.svg)](https://github.com/computationales/rsofun/actions)
[![codecov](https://codecov.io/gh/bluegreen-labs/rsofun/branch/master/graph/badge.svg?token=5RJtJmDVV7)](https://codecov.io/gh/bluegreen-labs/rsofun)

# rsofun

A modelling framework for site-scale simulations of ecosystem processes, implemented as an R package (low-level routines in Fortran 90). Implements the following models:

- P-model for leaf-level acclimation of photosynthesis from [Stocker et al. (2019)](https://www.geosci-model-dev-discuss.net/gmd-2019-200/).
- SPLASH for bioclimatic variables, including the surface radiation budget and the soil water balance from [Davis et al. (2017)](https://doi.org/10.5194/gmd-10-689-2017).
- BiomeE for comprehensive simulations of ecosystem carbon and water cycling, tree growth, and tree cohort-explicit forest dynamics following the Perfect Plasticity Approximation, from [Weng et al., (2015)](https://doi.org/10.5194/bg-12-2655-2015).

## Installation

To install and load the rsofun package using the latest release run the following command in your R terminal: 
```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("computationales/rsofun@v4.3")
library(rsofun)
```
Problems with compilation? Make sure to have gfortran installed and paths to the compiler and libraries properly specified (see e.g., [here](https://github.com/computationales/rsofun/issues/58)). 

## Example

See vignette [Example for using rsofun](./articles/pmodel_use.html) for how to run the model, and the [ingestr R package](https://github.com/computationales/ingestr) for collecting the forcing data to run rsofun.

## Usage and contribution

The developers (Beni Stocker, Koen Hufkens, Pepa Aran) would appreciate if your developments can be fed back to this repository. Please make pull requests. Thanks.

Tutorials for developing the source code are available on our [YouTube Channel](https://www.youtube.com/@geco-group/playlists) (see Playlist 'rsofun').

## References

Stocker, B. D., Wang, H., Smith, N. G., Harrison, S. P., Keenan, T. F., Sandoval, D., Davis, T., and Prentice, I. C.: P-model v1.0: an optimality-based light use efficiency model for simulating ecosystem gross primary production, Geosci. Model Dev., 13, 1545–1581, https://doi.org/10.5194/gmd-13-1545-2020, 2020.

Davis, T. W., Prentice, I. C., Stocker, B. D., Thomas, R. T., Whitley, R. J., Wang, H., Evans, B. J., Gallego-Sala, A. V., Sykes, M. T., and Cramer, W.: Simple process-led algorithms for simulating habitats (SPLASH v.1.0): robust indices of radiation, evapotranspiration and plant-available moisture, Geoscientific Model Development, 10, 689–708, doi:10.5194/gmd-10-689-2017, URL http: //www.geosci-model-dev.net/10/689/2017/, 2017.

Weng, E. S., Malyshev, S., Lichstein, J. W., Farrior, C. E., Dybzinski, R., Zhang, T., Shevliakova, E., and Pacala, S. W.: Scaling from individual trees to forests in an Earth system modeling framework using a mathematically tractable model of height-structured competition, Biogeosciences, 12, 2655–2694, https://doi.org/10.5194/bg-12-2655-2015, 2015.
