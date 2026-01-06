# Run P-model

Runs P-model for multiple sites.

## Usage

``` r
runread_pmodel_f(drivers, par, makecheck = TRUE, parallel = FALSE, ncores = 1)
```

## Arguments

- drivers:

  A nested data frame with one row for each site and columns named
  according to the arguments of function `runread_pmodel_f`. Namely
  `sitename, params_siml, site_info` and `forcing`.

- par:

  A named list of free (calibratable) model parameters.

  kphio

  :   The quantum yield efficiency at optimal temperature \\\varphi_0\\,
      in mol mol\\^{-1}\\. When temperature dependence is used, it
      corresponds to the multiplicative parameter \\c\\ (see Details).

  kphio_par_a

  :   The shape parameter \\a\\ of the temperature-dependency of quantum
      yield efficiency (see Details). To disable the temperature
      dependence, set `kphio_par_a = 0`.

  kphio_par_b

  :   The optimal temperature parameter \\b\\ of the temperature
      dependent quantum yield efficiency (see Details), in \\^o\\C.

  soilm_thetastar

  :   The threshold parameter \\\theta^{\*}\\ in the soil moisture
      stress function (see Details), given in mm. To turn off the soil
      moisture stress, set `soilm_thetastar = 0`.

  soilm_betao

  :   The intercept parameter \\\beta\_{0}\\ in the soil moisture stress
      function (see Details). This is the parameter calibrated in
      Stocker et al. 2020 GMD.

  beta_unitcostratio

  :   The unit cost of carboxylation, corresponding to \\\beta = b /
      a'\\ in Eq. 3 of Stocker et al. 2020 GMD.

  rd_to_vcmax

  :   Ratio of Rdark (dark respiration) to Vcmax25.

  tau_acclim

  :   Acclimation time scale of photosynthesis, in days.

  kc_jmax

  :   Parameter for Jmax cost ratio (corresponding to c\\^\*\\ in
      Stocker et al. 2020 GMD).

- makecheck:

  A logical specifying whether checks are performed to verify forcings
  and model parameters. `TRUE` by default.

- parallel:

  A logical specifying whether simulations are to be parallelised
  (sending data from a certain number of sites to each core). Defaults
  to `FALSE`.

- ncores:

  An integer specifying the number of cores used for parallel computing
  (by default `ncores = 2`).

## Value

A data frame (tibble) with one row for each site, site information
stored in the nested column `site_info` and outputs stored in the nested
column `data`. See
[`run_pmodel_f_bysite`](https://geco-bern.github.io/rsofun/dev/reference/run_pmodel_f_bysite.md)
for a detailed description of the outputs. Example outputs are provided
as
[`biomee_p_model_output`](https://geco-bern.github.io/rsofun/dev/reference/biomee_p_model_output.md)
and
[`biomee_gs_leuning_output`](https://geco-bern.github.io/rsofun/dev/reference/biomee_gs_leuning_output.md).

## Details

Depending on the input model parameters, it's possible to run the
different P-model setups presented in Stocker et al. 2020 GMD. The
P-model version implemented in this package allows more flexibility than
the one presented in the paper, with the following functions:

The temperature dependence of the quantum yield efficiency is given
by:  
\\\varphi_0 (T) = c (1 + a (T - b)^2 ) \\ if \\0 \< c (1 + a (T - b)^2 )
\< 1\\,  
\\\varphi_0 (T) = 0 \\ if \\ c (1 + a (T - b)^2 ) \leq 0\\, and  
\\\varphi_0 (T) = 1 \\ if \\ c (1 + a (T - b)^2 ) \geq 1\\.  
The ORG setup can be reproduced by setting `kphio_par_a = 0` and
calibrating the `kphio` parameter only. The BRC setup (which calibrates
\\c_L = \frac{a_L b_L}{4}\\ in Eq. 18) is more difficult to reproduce,
since the temperature-dependency has been reformulated and a custom cost
function would be necessary for calibration. The new parameters are
related to \\c_L\\ as follows:  
\\a = -0.0004919819\\  
\\b = 32.35294\\  
\\c = 0.6910823 c_L\\

The soil moisture stress is implemented as  
\\\beta(\theta) = \frac{\beta_0 - 1}{{\theta^{\*}}^2} (\theta -
\theta^{\*})^2 + 1 \\ if \\ 0 \leq \theta \leq \theta^{\*}\\ and  
\\\beta(\theta) = 1\\ if \\ \theta \> \theta^{\*}\\.  
In Stocker et al. 2020 GMD, the threshold plant-available soil water is
set as \\\theta^{\*}\\ `= 0.6 * whc` where `whc` is the site's water
holding capacity. Also, the \\\beta\\ reduction at low soil moisture
(\\\beta_0 = \beta(0)\\) was parameterized as a linear function of mean
aridity (Eq. 20 in Stocker et al. 2020 GMD) but is considered a constant
model parameter in this package. Hence, the FULL calibration setup
cannot be exactly replicated.

## Examples

``` r
# Define model parameter values from previous work
params_modl <- list(
  kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.0,        # disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6 * 240,  # old setup with soil moisture stress
  soilm_betao        = 0.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41
)

# Run the model for these parameters and the example drivers
output <- rsofun::runread_pmodel_f(
  drivers = rsofun::p_model_drivers,
  par = params_modl)
output_vcmax25 <- rsofun::runread_pmodel_f(
  drivers = rsofun::p_model_drivers_vcmax25,
  par = params_modl)
```
