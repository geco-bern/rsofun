# Cost function computing a log-likelihood for calibration of P-model parameters

The cost function performs a P-model run for the input drivers and model
parameter values, and computes the outcome's normal log-likelihood
centered at the input observed values and with standard deviation given
as an input parameter (calibratable).

## Usage

``` r
cost_likelihood_pmodel(
  par,
  obs,
  drivers,
  targets,
  par_fixed = NULL,
  parallel = FALSE,
  ncores = 2
)
```

## Arguments

- par:

  A named vector of values for the parameters to be calibrated,
  including a subset of model parameters (described in
  [`runread_pmodel_f`](https://geco-bern.github.io/rsofun/dev/reference/runread_pmodel_f.md)),
  in order, and error terms for each target variable (for example
  `'gpp_err'`), in the same order as the targets appear in `targets`.

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

- parallel:

  A logical specifying whether simulations are to be parallelised
  (sending data from a certain number of sites to each core). Defaults
  to `FALSE`.

- ncores:

  An integer specifying the number of cores used for parallel computing.
  Defaults to 2.

## Value

The log-likelihood of the observed target values, assuming that they are
independent, normally distributed and centered on the predictions made
by the P-model run with standard deviation given as input (via \`par\`
because the error terms are estimated through the calibration with
\`BayesianTools\`, as shown in the "Parameter calibration and cost
functions" vignette).

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
# Compute the likelihood for a set of 
# model parameter values involved in the
# temperature dependence of kphio 
# and example data
cost_likelihood_pmodel(
par = c(kphio       = 0.05, 
        kphio_par_a = -0.01, 
        kphio_par_b = 1,     # model parameters
        err_gpp     = 2),    # err_gpp
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
#> [1] -6208.172
```
