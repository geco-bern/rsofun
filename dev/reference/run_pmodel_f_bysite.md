# Run P-model (time series)

Run P-model on a single site for a forcing time series.

## Usage

``` r
run_pmodel_f_bysite(
  sitename,
  params_siml,
  site_info,
  forcing,
  params_modl,
  makecheck = TRUE,
  verbose = TRUE
)
```

## Arguments

- sitename:

  Site name.

- params_siml:

  Simulation parameters.

- site_info:

  Site meta info in a data.frame.

- forcing:

  A data frame of forcing climate data, used as input.

- params_modl:

  A named list of free (calibratable) model parameters. See
  [`runread_pmodel_f`](https://geco-bern.github.io/rsofun/dev/reference/runread_pmodel_f.md)

- makecheck:

  A logical specifying whether checks are performed to verify forcings
  and model parameters. `TRUE` by default.

- verbose:

  A logical specifying whether to print warnings. Defaults to `TRUE`.

  For further specifications of above inputs and examples see
  [`p_model_drivers`](https://geco-bern.github.io/rsofun/dev/reference/p_model_drivers.md)
  or
  [`p_model_drivers_vcmax25`](https://geco-bern.github.io/rsofun/dev/reference/p_model_drivers_vcmax25.md)

## Value

Model output is provided as a tidy dataframe, with columns:

- `date`:

  Date of the observation in YYYY-MM-DD format.

- `year_dec`:

  Decimal representation of year and day of the year (for example,
  2007.000 corresponds to 2007-01-01 and 2007.003 to 2007-01-02.

- `fapar`:

  Fraction of photosynthetic active radiation (fAPAR), taking values
  between 0 and 1.

- `gpp`:

  Gross Primary Productivity (GPP) for each time stamp (in gC m\\^{-2}\\
  d\\^{-1}\\).

- `aet`:

  Actual evapotranspiration (AET), calculated by SPLASH following
  Priestly-Taylor (in mm d\\^{-1}\\).

- `le`:

  Latent heat flux (in J m\\^{-2}\\ d\\^{-1}\\).

- `pet`:

  Potential evapotranspiration (PET), calculated by SPLASH following
  Priestly-Taylor (in mm d\\^{-1}\\).

- `vcmax`:

  Maximum rate of RuBisCO carboxylation (Vcmax) (in mol C m\\^{-2}\\
  s\\^{-1}\\).

- `jmax`:

  Maximum rate of electron transport for RuBP regeneration (in mol
  CO\\\_2\\ m\\^{-2}\\ s\\^{-1}\\).

- `vcmax25`:

  Maximum rate of carboxylation (Vcmax), normalised to 25\\^o\\C (in mol
  C m\\^{-2}\\ s\\^{-1}\\).

- `jmax25`:

  Maximum rate of electron transport, normalised to 25\\^o\\C (in mol C
  m\\^{-2}\\ s\\^{-1}\\).

- `gs_accl`:

  Acclimated stomatal conductance (in mol C (mol photons)\\^{-1}\\
  Pa\\^{-1}\\. (Multiply by ppfd (mol photons m\\^{-2}\\ d\\^{-1}\\) and
  fapar to express per unit ground area and time.)

- `wscal`:

  Relative soil water content, between 0 (permanent wilting point, PWP)
  and 1 (field capacity, FC).

- `chi`:

  Ratio of leaf-internal to ambient CO\\\_{2}\\, ci:ca (unitless).

- `iwue`:

  Intrinsic water use efficiency (iWUE) (unitless, multiply with patm
  (Pa) to get iWUE in Pa).

- `rd`:

  Dark respiration (Rd) in gC m\\^{-2}\\ s\\^{-1}\\. (Multiply by 1/12
  (mol C / gC) to convert to mol C m\\^{-2}\\ s\\^{-1}\\.)

- `tsoil`:

  Soil temperature, in \\^{o}\\C.

- `netrad`:

  Net radiation, in W m\\^{-2}\\. WARNING: this is currently ignored as
  a model forcing. Instead, net radiation is internally calculated by
  SPLASH.

- `wcont`:

  Soil water content, in mm.

- `snow`:

  Snow water equivalents, in mm.

- `cond`:

  Water input by condensation, in mm d\\^{-1}\\

- `cleaf`:

  C mass of a virtual leaf carbon pool to keep track of isotopic
  composition, in gC m\\^{-2}\\

- `cleafd13c`:

  13C isotopic signature (delta) of `cleaf`, in permil.

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

# Run the Fortran P-model 
mod_output <- run_pmodel_f_bysite(
  # unnest drivers example data
  sitename = p_model_drivers$sitename[1],
  params_siml = p_model_drivers$params_siml[[1]],
  site_info = p_model_drivers$site_info[[1]],
  forcing = p_model_drivers$forcing[[1]],
  params_modl = params_modl
 )
```
