# Run P-model (single time step)

Run P-model on a single site for a single time step. This does not
include the simulation of ecosystem-level quantities, water limitation,
nor a simulation of water fluxes. Instead, this corresponds to a
leaf-level representation of the acclimation of photosynthesis.

## Usage

``` r
run_pmodel_onestep_f_bysite(lc4, forcing, params_modl, makecheck = TRUE)
```

## Arguments

- lc4:

  Locigical specifying whether P-model simulation is for C4 (as opposed
  to C3). Defaults to `FALSE`.

- forcing:

  A data frame of forcing climate data, used as input (single row).

- params_modl:

  A named list of free (calibratable) model parameters. See
  [`runread_pmodel_f`](https://geco-bern.github.io/rsofun/dev/reference/runread_pmodel_f.md)

- makecheck:

  A logical specifying whether checks are performed to verify forcings
  and model parameters. `TRUE` by default.

  For further specifications of above inputs and examples see
  [`p_model_drivers`](https://geco-bern.github.io/rsofun/dev/reference/p_model_drivers.md)
  or
  [`p_model_drivers_vcmax25`](https://geco-bern.github.io/rsofun/dev/reference/p_model_drivers_vcmax25.md)

## Value

Model output is provided as a tidy dataframe, with columns:

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

- `chi`:

  Ratio of leaf-internal to ambient CO\\\_{2}\\, ci:ca (unitless).

- `iwue`:

  Intrinsic water use efficiency (iWUE) (unitless, multiply with patm
  (Pa) to get iWUE in Pa).

- `rd`:

  Dark respiration (Rd) in gC m\\^{-2}\\ s\\^{-1}\\. (Multiply by 1/12
  (mol C / gC) to convert to mol C m\\^{-2}\\ s\\^{-1}\\.)

- `bigdelta`:

  13C isotope discrimination of leaf assimilates against atmospheric
  signature (permil).

## Details

TBC

## Examples

``` r
# Define model parameter values from previous work
params_modl <- list(
  kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.0,        # disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
  kc_jmax            = 0.41
)

# Run the Fortran P-model 
run_pmodel_onestep_f_bysite(
  lc4 = FALSE,
  forcing = data.frame(
    temp  = 20,           # temperature, deg C
    vpd   = 1000,         # Pa,
    ppfd  = 300/10^6,     # mol/m2/s
    co2   = 400,          # ppm,
    patm  = 101325        # Pa
  ),
  params_modl = list(
    kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.0,        # disable temperature-dependence of kphio
    kphio_par_b        = 1.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
    kc_jmax            = 0.41
  ),
  makecheck = TRUE
)
#> # A tibble: 1 × 9
#>       vcmax      jmax   vcmax25    jmax25 gs_accl   chi    iwue      rd bigdelta
#>       <dbl>     <dbl>     <dbl>     <dbl>   <dbl> <dbl>   <dbl>   <dbl>    <dbl>
#> 1 0.0000177 0.0000400 0.0000275 0.0000518 0.00158 0.694 7.64e-5 3.11e-6     19.4
```
