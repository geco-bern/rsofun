# rsofun (development version)

## BiomeEP: New features
* Cohorts are now less aggressively merged. Cohort merging is needed for 
  lowering memory footprint of a simulation. Criteria for merging eligibility 
  of two cohorts now use modified limits for DBH differences:
  - for trees below 5 cm: criteria _remain as before_: i.e. either <= 0.001 m or <= 20%, and
  - for trees above 5 cm: criteria were changed to absolute DBH difference of <= 0.01 m.
* Added `output_daily_tile$Tksoil`, i.e. daily output of soil temperature
* Added parameters allowing restarting a previous simulation where it has ended:
  * Added missing state variables to `output_annual_cohorts`:
    `seedN`,`leafN`,`rootN`,`sapwoodN`,`heartwoodN` (note d13C currently still not output)
  * Added optional parameter `init_cohort$init_cohort_age` to specify a corresponding 
    initial age distribution in years (default = 0). Default ensures backwards compatibility.
    Age affects reproduction through `params_species$matural_age`
  * Added optional parameter to `init_cohort`, to enable specification of initial plant
    nitrogen pools. Namely: `init_cohort_bl_n14`, `init_cohort_br_n14`,
    `init_cohort_bsw_n14`,`init_cohort_bHW_n14`,`init_cohort_seedC_n14`,
    `init_cohort_nsc_n14`. If not provided, default values ensure backwards compatibility.
  * Added optional parameter to `init_soil`, to enable specification of initial soil
    nitrogen pools. Namely: `init_fast_soil_N`, `init_slow_soil_N`, `init_pmicr_C`, 
    `init_pmicr_d13C`, `init_pmicr_N`, `init_wcl1`, `init_wcl2`, `init_wcl3`, `init_N0_ecosystem`. 
    If not provided, default values ensure backwards compatibility.

## BiomeEP: (Potentially) Breaking changes
* Removed dummy parameters in `params_species` for `run_biomee_f_bysite()`: 
  `Vmax`,`alphaBM`,`leafLS`,`lAImax`,`CNleaf0`,`gamma_L`,`Vannual`,
  `betaON`,`betaOFF`, `leaf_size` and in `params_tile`: `GR_factor`.
  If still provided, they must be NA, otherwise an error occurs.
* Removed parameter in `params_tile`  for `run_biomee_f_bysite()`: `par_mort_under` 
  and `par_mort`. Their effects can be fully specified by 
  `params_species%mortrate_d_u` and `params_species%mortrate_d_c` (for trees and for grasses)    

## BiomeEP: Non-Breaking changes
* Bugfixes: 
  * Bugfix: `phenotype` is now correctly taking into account, decoupling it from `LMA` (#329)
  * Bugfix: annual, cohort-level output had mixed up column names for variables:
    `NSN`,`seedC`,`leafC`,`rootC`,`sapwoodC`,`heartwoodC`
  * Bugfix: `init_cohort$init_cohort_nsc` is now correctly taken into account. To 
    remain backward-compatible, it was made optional. To recover previous behavior 
    do not provide this column.
  * Bugfix: `init_cohort$lu_index` is now correctly taken into account.
* Internals:
  * Added check that all `species$LMA` >= `params_tile$LMAmin`
  * Added check that all `init_cohort$species` have a corresponding entry in `params_species`

## P-model: New features
* Rewritten `calib_sofun()` that can handle more diverse prior 
  distributions of the parameters to estimate (see internal function 
  `createMixedPrior()`) and that can parallelize multiple the MCMC chains.
  The old version is still available as `calib_sofun_legacy()`.
* Rewritten `cost_likelihood_pmodel()` now handling multiple 
  target variables that require to run either `run_pmodel_f_bysite()` or 
  `run_pmodel_onestep_f_bysite()`. Thus also requires a new data format for the
  `drivers` and `obs` arguments. See below under (non-)breaking changes for the new
  format. The old likelihood function is still available as `cost_likelihood_pmodel_legacy()`.
* Rewritten `cost_rmse_pmodel()` similarly as `cost_likelihood_pmodel()` (see above).
* `runread_pmodel()` can now run onestep and daily model thanks to the new 
  `drivers` format. If the old format is used it is implicitly assuming daily 
  runs were requested. Output of `runread_pmodel()` can be split based on columns 
  present in the `data` column: 
  `df |> rowwise() |> filter("vcmax_mod_molm2s" %in% names(data)) |> ungroup()` or
  `df |> rowwise() |> filter("date" %in% names(data)) |> ungroup()`
* `pmodel_drivers` and `pmodel_validation` contain now additional sites and a 
  mix of `daily` as well as `onestep` model runs. Note that the data of 
  site `FR-Pue` were updated in FDK, so that the forcing data has slightly 
  changed and also `whc` of `FR-Pue` was modified from 432 to 260 mm.
* The old `p_model_drivers_vcmax25` and `p_model_validation_vcmax25` were 
  removed. The old `p_model_drivers` and `p_model_validation` were renamed to 
  `p_model_oldformat_drivers` and `p_model_oldformat_validation`.  See 
  below how to transform between new and old formats.
* Output of `run_pmodel_f_bysite()` (and consequently that of `runread_pmodel()`) 
  has been transformed into `tibble` for consistency

## P-model: (Potentially) Breaking changes
* `calib_sofun()` has been renamed to `calib_sofun_legacy()`, and a new 
  `calib_sofun()` has been written (see new features above). For P-model calibration 
  it is fully backwards compatible. For BiomeE calibration this still needs to be tested.
* `cost_likelihood_pmodel()` has been renamed to `cost_likelihood_pmodel_legacy()`. Both
  `cost_likelihood_pmodel()` and `cost_rmse_pmodel()` have been rewritten for the updated
  data.frame format, now simulating daily- or onestep-model.
  
## P-model: Non-Breaking changes
* New driver data.frame format for P-model: now containing the information which
  model to run (`daily` or `onestep`) as additional column `onestep` = `TRUE`/`FALSE`
  in the `params_siml` column. Moreover, in the 
  `forcing` column the nested data.frame has now additional columns for `wind` 
  (resulting in columns `ccov`,`co2`,`date`,`fapar`,`netrad`,`patm`,`ppfd`,
  `rain`,`snow`,`temp`,`tmax`,`tmin`,`vpd`,`wind`) for `daily` model runs. For 
  `onestep` model runs the required columns are (`co2`,`patm`,`ppfd`,`temp`,`vpd`).
  Each row in `pmodel_drivers` corresponds to a model run (either daily 
  or onestep depending on `params_siml$onestep` logical) 
  and (for calibration) should have a corresponding row in `pmodel_validation`.
  The example data set for `FR-Pue` now contains non-zero `ccov`, previously this was set to `0`.
* New validation (i.e. observation) data.frame format for P-model: 
  A new column `source` is required and determines in combination with the 
  `targets` argument of `calib_sofun()` which variables are fitted .
  variable(s) is/are the target of the corresponding line.
  
  Each row in `pmodel_validation` corresponds to a model run
  and should have a corresponding row in `pmodel_drivers`.
  Whether a row is a `onestep`-row or a `daily`-row is determined by the 
  corresponding row in the driver data.frame `pmodel_drivers`.
  
  For each row, the column `data` contains a single 
  `data.frame()` with a column `id` (`onestep`-row) or `date` (`daily`-row) 
  and additional columns for each target (e.g. `bigD13C_obs_permil` or `vj_obs__` 
  for a `onestep`-row or `gpp_obs` or `nee_obs` or `le_obs` for a `daily`-row). 
  The first part of these column names must correspond with names provided by 
  argument `targets` as a named vector. The target variables can contain NA if a 
  given variable is not available, they are then simply unused for the cost 
  function.
  
* Note for future: ideally, validation data and drivers could be a single data.frame.
  This would be ideal for calibration, since each row in the validation data.frame() must have a
  corresponding row in the drivers data.frame(). Having a single data.frame() 
  enforces this naturally. However, for just running the model only the drivers 
  are needed.
* Find below some code snippets to transform between new and old drivers data.frame format:

  ```
  # A) Compare with previous example data set:
  rsofun::pmodel_drivers |> dplyr::filter(sitename == "FR-Pue")
  rsofun::p_model_oldformat_drivers
  
  # bring new to old format:
  rsofun::pmodel_drivers |> dplyr::filter(sitename == "FR-Pue") |> 
    # remove new column 'onestep' from nested 'param_siml'
    dplyr::mutate(params_siml = purrr::map(params_siml, ~dplyr::select(.x, -'onestep'))) |>
    # 'site_info' remains same
    # 'forcing' has additional info `wind` that needs removing
    tidyr::unnest(forcing) |>
    dplyr::filter(date >= "2007-01-01" & date < "2013-01-01") |>
    dplyr::select(-wind) |> 
    dplyr::mutate(ccov = 0) |> # note that fapar and co2 are also different
    tidyr::nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, 
                            snow, rain, tmin, tmax, fapar, co2, ccov))

  # bring old to new format:
  rsofun::p_model_oldformat_drivers |> 
    # 'forcing' add new column `wind`
    tidyr::unnest(forcing) |> 
    dplyr::mutate(wind = 2.64, ccov = 0.485) |>
    tidyr::nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, 
                            snow, rain, tmin, tmax, wind, fapar, co2, ccov)) |>
    # add new column 'onestep' in nested 'param_siml'
    dplyr::mutate(params_siml = purrr::map(params_siml, ~dplyr::mutate(.x, onestep = FALSE)))
  ```
* Below some code snippets to transform between new and old validation (i.e. observation) data.frame format:

  ```
  # B) Compare with previous example data set:
  rsofun::pmodel_validation |> dplyr::filter(sitename == "FR-Pue")
  rsofun::p_model_oldformat_validation
  
  # bring new to old format:
  rsofun::pmodel_validation |> dplyr::filter(sitename == "FR-Pue") |> 
    # remove new column 'source'
    dplyr::select(-source) |>
    # remove new columns inside of 'data':
    tidyr::unnest(data) |>
    dplyr::select(-c(gpp_qc, nee, nee_qc, le, le_qc)) |>
    dplyr::mutate(gpp_unc = 0.01) |>
    tidyr::nest(data = c('date', 'gpp','gpp_unc')) |>
    # order columns
    dplyr::select(sitename, data)
  
  # bring old to new format:
  rsofun::p_model_oldformat_validation |> 
    # add new column 'source'
    dplyr::mutate(source = "fluxnet") |>
    # add new columns inside of 'data':
    tidyr::unnest(data) |>
    dplyr::select(-gpp_unc) |>
    dplyr::mutate(gpp_qc = 1, nee = NA, nee_qc = NA, le = NA, le_qc = NA) |>
    tidyr::nest(data = c('date', 'gpp','gpp_qc','nee','nee_qc','le','le_qc')) |>
    # order columns
    dplyr::select(sitename, source, data)
  ```

* Note that driver and validation data format for biomee has not changed. But 
could in the future be adapted accordingly.

# rsofun 5.1.0

## New features

* P-model:
  * New `run_pmodel_onestep_f_bysite()` adds single-step leaf-level simulations 
  of the P-model
  * Changed temperature dependencies of Vcmax and Jmax (see breaking changes below)
  * Added support for carbon isotope tracking
    * `run_pmodel_onestep_f_bysite()` returns `bigdelta`, i.e. the carbon 
      fractionation of fresh assimilate `cleafd13c` relative to `d13c_atm`, 
      thereby avoiding any assumptions for `d13c_atm`.
    * `run_pmodel_f_bysite()` and `runread_pmodel()` newly return `cleaf` and `cleafd13c`,
      assuming a constant atmospheric signature `d13c_atm = -8.4 permil`. In a future 
      version daily values of d13c_atm could/should be included in the input forcing.
  
* BiomeE:
  * Added support for LULUC (land use, land-use change) in BiomeE. See vignette 
  `biome_luluc` for details.
  * New BiomeE behavior to recycle last year of forcing if requested simulation 
  time span (`nyeartrend`) is longer than available forcing data.
  * Prepared support for carbon isotope tracking
    * `run_biomee_f_bysite()` and `runread_biomee()` do track carbon isotopes internally, 
      currently without output. Note that when `method_photosynth == "gs_leuning"`, then
      `bigdelta = 20`, whereas when `method_photosynth == "pmodel"` then `bigdelta` is
      computed based on the `pmodel`-derived `chi`.

* `calib_sofun()` now passes parameters as a _named_ vector to cost-functions for 
  easier processing within cost-functions. Default cost-functions were updated,
  but currently ignore the names. This is fully backward compatible, but allows
  to use the names in user-created cost functions.


## Breaking changes

* P-model now computes temperature dependencies of jmax and vcmax
  with Kumarathunge et al. (2019) instead of Kattge & Knorr (2007), effectively 
  using a dampened signal for tc_growth and long-term averages for tc_home. This 
  alters the simulation results. Formatting of inputs remains unchanged.

* Breaking changes in BiomeE drivers:
  * `init_cohort$init_n_cohorts` column has been phased out and must not be present in  
drivers to protect against data corruption.
  * `update_annualLAImax` flag was removed and the behavior is now slightly altered 
  since `LAImax` and `underLAImax` are set and kept constant at the start of the simulation:
    * `LAImax` takes value `max(LAI_light, 0.5)`
    * `underLAImax` takes value `min(LAImax, 1.2)`
  * `do_closedN_run` flag now uses initial inorganic N setting rather than arbitrary value 
  and is false by default.
  * Modified drivers to use more sensibles values
    * Species 1 is now a C4 crop
    * `N_input` is set to 0.01 to limit N starvation.

* Breaking changes in BiomeE output
  * renamed the following column names in `output_daily_tile`:
    * `totWs`=>`SoilWater`
    * `Trsp`=>`Transp`
    * `SW_C`=>`sapwoodC` and `SW_N`=>`sapwoodN`
    * `HW_C`=>`heartwoodC` and `HW_N`=>`heartwoodN`
    * `McrbC`=>`mcrbC` and `McrbN`=>`mcrbN`
    * added `NPP`
  * renamed the following column names in `output_annual_tile` and `output_annual_cell`:
    * `rain`=>`Prcp`
    * `SeedC`=>`seedC` (and for `N`)
    * `SapwoodC`=>`sapwoodC` (and for `N`)
    * `WoodC`=>`heartwoodC` (and for `N`)
    * `SlowSOM`=>`slowSOM`
    * `McrbC`=>`mcrbC` and `McrbN`=>`mcrbN`
    * added `totC`
  * renamed the following column names in `output_annual_cohorts`:
    * `nsc`=>`NSC` and `nsn`=>`NSN` 
    * `sapwC`=>`sapwoodC`
    * `woodC`=>`heartwoodC`
    * `Nupt`=>`N_uptk`
    * `Nfix`=>`N_fxed`
    * switched column positions of `NPP` and `GPP`


# rsofun v5.0.1

* This model version behaves exactly as the version before.
* Update was necessary to include description of scripts and data used for the 
model documentation paper into archive on Zenodo


# rsofun v5.0.0

* new BiomeE forcing data matching that of P-model
  * `prec` is now called `rain`
  * `rh` is now provided as `vpd`
  * See `biomee_gs_leuning_drivers` for an example
* fix Fortran modules leading to segmentation faults using BiomeE model
* improved documentation

# rsofun v4.4.1

* bugfix Fortran modules and derived types
* compilation flags for different platforms

# rsofun v4.4

* bugfix on input data type P-model
* LM3-PPA to BiomeE renaming
* cost function rewrite
* update output format (consistency)
* add water balance variables to P-model output
* new p-model calibratable parameters
* rewrite of temperature and soil moisture stress functions
* update simulation parameters to take netrad, ppfd and ccov as input forcing
* documentation parameter sensitivity analysis
* documentation on the data format
* document calibration diagnostic and model uncertainty calculation
* more transparent licensing and copyright statements in COPYING file

# rsofun v4.3

* Consistent variable names p-model / biomee
* optimization stability BayesianTools
* documentation
* Bugfixes

# rsofun v4.2

* Canopy transfer solved for BiomeE with P-model
* Bugfixes

# rsofun v4.1

* Catching aborting in FORTRAN part to avoid R session crashes

# rsofun v4.0

* Public release after refactored code
