# Cost function computing RMSE for calibration of P-model parameters

The cost function performs a P-model run for the input drivers and
parameter values, and compares the output to observations of various
targets by computing the root mean squared error (RMSE).

## Usage

``` r
cost_rmse_pmodel(
  par,
  obs,
  drivers,
  targets,
  par_fixed = NULL,
  target_weights = NULL,
  parallel = FALSE,
  ncores = 2
)
```

## Arguments

- par:

  A vector of values for the parameters to be calibrated (a subset of
  those described in
  [`runread_pmodel_f`](https://geco-bern.github.io/rsofun/dev/reference/runread_pmodel_f.md),
  in order).

- obs:

  A nested data.frame of observations, with columns `'sitename'` and
  `'data'` (see
  [`p_model_validation`](https://geco-bern.github.io/rsofun/dev/reference/p_model_validation.md)
  or
  [`p_model_validation_vcmax25`](https://geco-bern.github.io/rsofun/dev/reference/p_model_validation_vcmax25.md)
  to check their structure).

- drivers:

  A nested data.frame of driver data. See
  [`p_model_drivers`](https://geco-bern.github.io/rsofun/dev/reference/p_model_drivers.md)
  for a description of the data structure.

- targets:

  A character vector indicating the target variables for which the
  optimization will be done and the RMSE computed. This string must be a
  column name of the `data` data.frame belonging to the validation
  nested data.frame (for example 'gpp').

- par_fixed:

  A named list of model parameter values to keep fixed during the
  calibration. These should complement the input `par` such that all
  model parameters are passed on to
  [`runread_pmodel_f`](https://geco-bern.github.io/rsofun/dev/reference/runread_pmodel_f.md).

- target_weights:

  A vector of weights to be used in the computation of the RMSE if using
  several targets. By default (`target_weights = NULL`) the RMSE is
  computed separately for each target and then averaged. The provided
  weights are used to compute a weighted average of RMSE across targets.

- parallel:

  A logical specifying whether simulations are to be parallelised
  (sending data from a certain number of sites to each core). Defaults
  to `FALSE`.

- ncores:

  An integer specifying the number of cores used for parallel computing.
  Defaults to 2.

## Value

The root mean squared error (RMSE) between observed values and P-model
predictions. The RMSE is computed for each target separately and then
aggregated (mean or weighted average).

## Details

To run the P-model, all model parameters must be given. The cost
function uses arguments `par` and `par_fixed` such that, in the
calibration routine, `par` can be updated by the optimizer and
`par_fixed` are kept unchanged throughout calibration.

If the validation data contains a "date" column (fluxes), the simulated
target time series is compared to the observed values on those same
dates (e.g. for GPP). Otherwise, there should only be one observed value
per site (leaf traits), and the outputs (averaged over the growing
season, weighted by predicted GPP) will be compared to this single value
representative of the site (e.g. Vcmax25). As an exception, when the
date of a trait measurement is available, it will be compared to the
trait value predicted on that date.

## Examples

``` r
# Compute RMSE for a set
# of model parameter values
# and example data
cost_rmse_pmodel(
 par = c(0.05, -0.01, 0.5),  # kphio related parameters
 obs = p_model_validation,
 drivers = p_model_drivers,
 targets = c('gpp'),
 par_fixed = list(
  soilm_thetastar    = 0.6 * 240,  # old setup with soil moisture stress
  soilm_betao        = 0.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41
 )
)
#> [1] 3.837109
```
