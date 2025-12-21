# rsofun (development version)

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
