# Calibrates SOFUN model parameters

This is the main function that handles the calibration of SOFUN model
parameters.

## Usage

``` r
calib_sofun(drivers, obs, settings, optim_out = TRUE, ...)
```

## Arguments

- drivers:

  A data frame with driver data. See
  [`p_model_drivers`](https://geco-bern.github.io/rsofun/dev/reference/p_model_drivers.md)
  for a description of the data structure. Additional columns can
  optionally be provided to `drivers` to control e.g. the processing
  within a personalized cost function.

- obs:

  A data frame containing observational data used for model calibration.
  See
  [`p_model_validation`](https://geco-bern.github.io/rsofun/dev/reference/p_model_validation.md)
  for a description of the data structure. Additional columns can
  optionally be provided to `obs` to control e.g. the processing within
  a personalized cost function.

- settings:

  A list containing model calibration settings. See the 'P-model usage'
  vignette for more information and examples.

  `method`

  :   A string indicating the optimization method, either `'GenSA'` or
      `'BayesianTools'`.

  `par`

  :   A list of model parameters. For each parameter, an initial value
      and lower and upper bounds should be provided. The calibratable
      parameters include model parameters 'kphio', 'kphio_par_a',
      'kphio_par_b', 'soilm_thetastar', 'soilm_betao',
      'beta_costunitratio', 'rd_to_vcmax', 'tau_acclim', 'kc_jmax' and
      'rootzone_whc' , and (if doing Bayesian calibration) error
      parameters for each target variable, named for example 'err_gpp'.
      This list must match the input parameters of the calibration
      metric and the parameters should be given in the order above.

  `metric`

  :   A cost function. See the 'Cost functions for parameter
      calibration' vignette for examples.

  `control`

  :   A list of arguments passed on to the optimization function. If
      `method = 'GenSA'`, see
      [GenSA](https://rdrr.io/pkg/GenSA/man/GenSA.html). If
      `method = 'BayesianTools'` the list should include at least
      `settings` and `sampler`, see
      [BayesianTools::runMCMC](https://rdrr.io/pkg/BayesianTools/man/runMCMC.html).

- optim_out:

  A logical indicating whether the function returns the raw output of
  the optimization functions (defaults to TRUE).

- ...:

  Optional arguments, simply passed on to the cost function.

## Value

A named list containing the calibrated parameter vector \`par\` and the
output object from the optimization \`mod\`. For more details on this
output and how to evaluate it, see
[runMCMC](https://rdrr.io/pkg/BayesianTools/man/runMCMC.html) (also
[this
post](https://florianhartig.github.io/BayesianTools/articles/BayesianTools.html))
and [GenSA](https://rdrr.io/pkg/GenSA/man/GenSA.html).

## Examples

``` r
# Fix model parameters that won't be calibrated
params_fix <- list(
  kphio_par_a        = 0,
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6*240,
  soilm_betao        = 0.01,
  beta_unitcostratio = 146,
  rd_to_vcmax        = 0.014,
  tau_acclim         = 30,
  kc_jmax            = 0.41
)

# Define calibration settings
settings <- list(
  method = "BayesianTools",
  par = list(
    kphio = list(lower=0.04, upper=0.09, init=0.05),
    err_gpp = list(lower = 0.01, upper = 4, init = 2)
  ),
  metric = rsofun::cost_likelihood_pmodel,
  control = list(
    sampler = "DEzs",
    settings = list(
      nrChains = 1,
      burnin = 0,        
      iterations = 50     # kept artificially low
    )
  )
 )
 
 # Run the calibration for GPP data
 calib_output <- rsofun::calib_sofun(
   drivers = rsofun::p_model_drivers,
   obs = rsofun::p_model_validation,
   settings = settings,
   # extra arguments for the cost function
   par_fixed = params_fix,
   targets = c("gpp")
 )
#>  Running DEzs-MCMC, chain  1 iteration 51 of 51 . Current logp  -3669.919 -3238.148 -3965.871 . Please wait! 
#> runMCMC terminated after 1.539seconds
```
