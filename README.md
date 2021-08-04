[![R build status](https://github.com/bluegreen-labs/rsofun/workflows/R-CMD-check/badge.svg)](https://github.com/bluegreen-labs/rsofun/actions)
[![codecov](https://codecov.io/gh/bluegreen-labs/rsofun/branch/master/graph/badge.svg?token=5RJtJmDVV7)](https://codecov.io/gh/bluegreen-labs/rsofun)

# rsofun

Provides a modelling framework that implements the P-model for leaf-level acclimation of photosynthesis for site-scale simulations, with SPLASH used for simulating the soil water balance (see also [Stocker et al., 2019 GMDD](https://www.geosci-model-dev-discuss.net/gmd-2019-200/)). The package provides the following functionalities:

- Calibrating model parameters
- Running the model and getting outputs directly back into R ("tidy" data)
- Evaluating outputs (benchmarking)

Model forcing and calibration data is collected using the [ingestr](https://stineb.github.io/ingestr/) package. See [here](https://rpubs.com/stineb/rsofun) for an example.

Parallelisation for a large number of site-level simulations is provided using the *multidplyr* R package. Calibration uses the *GenSA* R package.  

The P-model is implemented in different repositories for different purposes:

- **rsofun** (this package): Is for site-scale simulations (large ensemble of sites can be run in parallelised mode), forced by time series of meteorological data. Acclimation of photosynthesis is assumed at a user-defined time scale. I.e., the P-model optimality criterion (Wang et al., 2017; Prentice et al., 2014) is solved daily with "damped" daily variations in the forcing data (similar to a running mean). All model code is implemented in Fortran.
- [**rpmodel**](https://stineb.github.io/rpmodel/): Implements the same equation as rsofun (all native R), but solves the optimality criterion for each time step independently (instantaneous acclimation). This can be used for hypothesis generation, exploration, and illustrations. Transient simulations of acclimation and GPP should be done using rsofun.
- [**SOFUN**](https://stineb.github.io/sofun/): This is for P-model simulations on a (global) spatial grid and is purely in Fortran. Forcing data is read from NetCDF files and outputs are written to NetCDF files.

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
