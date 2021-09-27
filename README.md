[![R build status](https://github.com/bluegreen-labs/rsofun/workflows/R-CMD-check/badge.svg)](https://github.com/bluegreen-labs/rsofun/actions)
[![codecov](https://codecov.io/gh/bluegreen-labs/rsofun/branch/master/graph/badge.svg?token=5RJtJmDVV7)](https://codecov.io/gh/bluegreen-labs/rsofun)

# rsofun

A modelling framework for site-scale simulations of ecosystem processes, implemented as an R package (low-level routines in Fortran 90). Implements the following models:

- P-model for leaf-level acclimation of photosynthesis from [Stocker et al. (2019)](https://www.geosci-model-dev-discuss.net/gmd-2019-200/).
- SPLASH for bioclimatic variables, including the surface radiation budget and the soil water balance from [Davis et al. (2017)](https://doi.org/10.5194/gmd-10-689-2017).
- LM3-PPA for comprehensive simulations of ecosystem carbon and water cycling, tree growth, and tree cohort-explicit forest dynamics following the Perfect Plasticity Approximation, from [Weng et al., (2015)](https://doi.org/10.5194/bg-12-2655-2015).

## Installation

To install and load the rsofun package using the latest release run the following command in your R terminal: 
```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("stineb/rsofun")
library(rsofun)
```
## Example

See vignette [Example for using rsofun](./articles/example.html) for how to run the model, and the [ingestr R package ](https://stineb.github.io/ingestr/) for collecting the forcing data to run rsofun.

## Usage and contribution

This package is designed to be extendible to ingesting other data types (sources). The developer (Beni Stocker) would appreciate if you made sure that your developments can be fed back to this repository. To do so, please use git. See [here](http://rogerdudler.github.io/git-guide/) for a brief introduction to git. 
