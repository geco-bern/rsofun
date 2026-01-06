# Log-likelihood cost function for BiomeE with different targets

Cost function for parameter calibration, which computes the
log-likelihood for the biomee model fitting several target variables for
a given set of parameters.

## Usage

``` r
cost_likelihood_biomee(par, obs, drivers, targets)
```

## Arguments

- par:

  A named vector containing parameter values for
  `'phiRL', 'LAI_light', 'tf_base', 'par_mort'` in that order, and for
  the error terms corresponding to the target variables, e.g.
  `'err_GPP'` if GPP is a target. Make sure that the order of the error
  terms in `par` coincides with the order provided in the `targets`
  argument.

- obs:

  A nested data frame of observations, following the structure of
  `biomee_validation`, for example.

- drivers:

  A nested data frame of driver data, for example
  `biomee_gs_leuning_drivers`.

- targets:

  A character vector indicating the target variables for which the
  optimization will be done. This should be a subset of
  `c("GPP", "LAI", "Density", "Biomass")`.

## Value

The log-likelihood of the simulated targets by the biomee model versus
the observed targets.

## Details

The cost function performs a BiomeE model run for the value of `par`
given as argument. The likelihood is calculated assuming that the
predicted targets are independent, normally distributed and centered on
the observations. The optimization should be run using `BayesianTools`,
so the likelihood is maximized.

## Examples

``` r
 # do not run long-running simulations
# Compute the likelihood for a set of
# BiomeE model parameter values
# and the example data
cost_likelihood_biomee(
 par = c(phiRL = 3.5, 
         LAI_light = 3.5, 
         tf_base = 1, 
         par_mort = 1,    # model params
         err_GPP = 0.5),  # err_GPP
 obs = biomee_validation,
 drivers = biomee_p_model_drivers,
 targets = c("GPP")
)
#> [1] -0.4226299
```
