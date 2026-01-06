# RMSE cost function for BiomeE

Cost function for parameter calibration, which computes the root mean
squared error (RMSE) between BiomeE simulations (using the input set of
parameters) and observed target variables. Cost function for parameter
calibration, which computes the RMSE for the biomee model fitting target
variables `'GPP','LAI','Density'` and `'Biomass'` for a given set of
parameters.

## Usage

``` r
cost_rmse_biomee(par, obs, drivers)
```

## Arguments

- par:

  A vector containing parameter values for
  `'phiRL', 'LAI_light', 'tf_base', 'par_mort'` in that order.

- obs:

  A nested data frame of observations, following the structure of
  `biomee_validation`, for example.

- drivers:

  A nested data frame of driver data, for example
  `biomee_gs_leuning_drivers`.

## Value

The root mean squared error (RMSE) between the observed and simulated
values of `'GPP','LAI','Density'` and `'Biomass'` (all variables have
the same weight). Relative errors (difference divided by observed
values) are used instead of absolute errors. The cost function performs
a BiomeE model run for parameter values `par` and model drivers
`drivers` given as arguments, producing the simulated values used to
compute the RMSE.

## Examples

``` r
 # do not run long-running simulations
# Compute RMSE for a set of
# model parameter values
# and example data
cost_rmse_biomee(
 par = c(3.5, 3.5, 1, 1),
 obs = biomee_validation,
 drivers = biomee_p_model_drivers
)
#> [1] 0.2008596
```
