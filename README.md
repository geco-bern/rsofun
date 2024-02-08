[![R build status](https://github.com/geco-bern/rsofun/workflows/R-CMD-check/badge.svg)](https://github.com/geco-bern/rsofun/actions)
[![codecov](https://codecov.io/gh/geco-bern/rsofun/branch/master/graph/badge.svg)](https://app.codecov.io/gh/geco-bern/rsofun)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3712928.svg)](https://doi.org/10.5281/zenodo.3712928)

# rsofun

An R Simulating Optimal FUNctioning (RSOFUN) framework for site-scale simulations of ecosystem processes. The package contains the following modules:

- P-model for leaf-level acclimation of photosynthesis from [Stocker et al. (2019)](https://gmd.copernicus.org/preprints/gmd-2019-200/).
- SPLASH for bioclimatic variables, including the surface radiation budget and the soil water balance from [Davis et al. (2017)](https://doi.org/10.5194/gmd-10-689-2017).
- BiomeE for comprehensive simulations of ecosystem carbon and water cycling, tree growth, and tree cohort-explicit forest dynamics following the Perfect Plasticity Approximation, from [Weng et al., (2015)](https://doi.org/10.5194/bg-12-2655-2015).

## Installation

### Stable release

To install the current stable release use a CRAN repository:

**WARNING: rsofun is not currently available on CRAN.** We're working on it. Until it's available again, the command below will not work.

``` r
install.packages("rsofun")
library("rsofun")
```

### Development release

To install the latest development release of the package run the following commands to install rsofun directly from GitHub:

``` r
if(!require(remotes)){install.packages("remotes")}
remotes::install_github("geco-bern/rsofun")
library("rsofun")
```

**NOTE:** Installing from GitHub requires compilation of Fortran and C source code contained in {rsofun}. To enable compiling source code, install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) on Windows, or [Xcode](https://developer.apple.com/xcode/) and the [GNU Fortran compiler on Mac](https://github.com/fxcoudert/gfortran-for-macOS) (see also 'Mandatory tools' [here](https://mac.r-project.org/tools/)). On Linux, the gfortran compiler is usually installed already.


Vignettes are not rendered by default, if you want to include additional documentation please use:

``` r
if(!require(remotes)){install.packages("remotes")}
remotes::install_github("geco-bern/rsofun", build_vignettes = TRUE)
library("rsofun")
```

## Use

Below sections show the ease of use of the package in terms of model parameter specification and running both a single run or optimizing the parameters for a given site (or multiple sites). For an in depth discussion we refer to the [vignettes](https://geco-bern.github.io/rsofun/articles/).

### Running model

With all data prepared we can run the P-model using `runread_pmodel_f()`. This function takes the nested data structure and runs the model site by site, returning nested model output results matching the input drivers.

``` r
# define model parameter values from previous
# work
params_modl <- list(
    kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )

# run the model for these parameters
output <- rsofun::runread_pmodel_f(
  p_model_drivers,
  par = params_modl
  )
```

### Parameter optimization

To optimize new parameters based upon driver data and a validation dataset we must first specify an optimization strategy and settings, as well as a cost function and parameter ranges.

``` r
settings <- list(
  method              = "GenSA",
  metric              = cost_rmse_pmodel,
  control = list(
    maxit = 100),
  par = list(
    kphio = list(lower=0.02, upper=0.2, init = 0.05)
    )
)
```

`rsofun` supports both optimization using the `GenSA` and `BayesianTools` packages. The above statement provides settings for a `GenSA` optimization approach. For this example the maximum number of iterations is kept artificially low. In a real scenario you will have to increase this value orders of magnitude. Keep in mind that optimization routines rely on a cost function, which, depending on its structure influences parameter selection. A limited set of cost functions is provided but the model structure is transparent and custom cost functions can be easily written. More details can be found in the "Parameter calibration and cost functions" vignette.

In addition starting values and ranges are provided for the free parameters in the model. Free parameters include: parameters for the quantum yield efficiency `kphio`, `kphio_par_a` and `kphio_par_b`, soil moisture stress parameters `soilm_thetastar` and `soilm_betao`, and also `beta_unitcostratio`, `rd_to_vcmax`, `tau_acclim` and `kc_jmax` (see `?runread_pmodel_f`). Be mindful that with newer versions of `rsofun` additional parameters might be introduced, so re-check vignettes and function documentation when updating existing code.

With all settings defined the optimization function `calib_sofun()` can be called with driver data and observations specified. Extra arguments for the cost function (like what variable should be used as target to compute the root mean squared error (RMSE) and previous values for the parameters that aren't calibrated, which are needed to run the P-model).

``` r
# calibrate the model and optimize free parameters
pars <- calib_sofun(
    drivers = p_model_drivers,  
    obs = p_model_validation,
    settings = settings,
    # extra arguments passed to the cost function:
    targets = "gpp",             # define target variable GPP
    par_fixed = params_modl[-1]  # fix non-calibrated parameters to previous 
                                 # values, removing kphio
  )
```

## References

Stocker, B. D., Wang, H., Smith, N. G., Harrison, S. P., Keenan, T. F., Sandoval, D., Davis, T., and Prentice, I. C.: P-model v1.0: an optimality-based light use efficiency model for simulating ecosystem gross primary production, Geosci. Model Dev., 13, 1545–1581, https://doi.org/10.5194/gmd-13-1545-2020, 2020.

Davis, T. W., Prentice, I. C., Stocker, B. D., Thomas, R. T., Whitley, R. J., Wang, H., Evans, B. J., Gallego-Sala, A. V., Sykes, M. T., and Cramer, W.: Simple process-led algorithms for simulating habitats (SPLASH v.1.0): robust indices of radiation, evapotranspiration and plant-available moisture, Geoscientific Model Development, 10, 689–708, doi:10.5194/gmd-10-689-2017, URL http: //www.geosci-model-dev.net/10/689/2017/, 2017.

Weng, E. S., Malyshev, S., Lichstein, J. W., Farrior, C. E., Dybzinski, R., Zhang, T., Shevliakova, E., and Pacala, S. W.: Scaling from individual trees to forests in an Earth system modeling framework using a mathematically tractable model of height-structured competition, Biogeosciences, 12, 2655–2694, https://doi.org/10.5194/bg-12-2655-2015, 2015.

## Acknowledgements

The {rsofun} is part of the LEMONTREE project and funded by Schmidt Futures and under the umbrella of the Virtual Earth System Research Institute (VESRI).
