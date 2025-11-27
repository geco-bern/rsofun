# rsofun (development version)

## New features
* P-model:
  * New `calib_sofun_parallelized()` that can handle more diverse prior 
  distributions of the parameters to estimate (see internal function 
  `createMixedPrior()`) and that can be parallelized
  * New `cost_likelihood_pmodel_bigD13C_vj_gpp()` that can handle multiple 
  target variables that require to run either `run_pmodel_f_bysite()` or 
  `run_pmodel_onestep_f_bysite()`. Thus also requires a new data format for the
  `drivers` and `obs` arguments. See below under breaking changes for the new
  format.
  * `runread_pmodel()` can now run onestep and daily model thanks to the new 
  `drivers` format. If the old format is used it is implicitly assuming daily 
  runs were requested.
  * `pmodel_drivers` contain now more sites and a range of `daily` as well as 
  `onestep` model runs. Note that the data of site `FR-Pue` were updated in FDK,
  so that the forcing data has slightly changed and also `whc` was modified from 
  432 to 260 mm.
  * output of `run_pmodel_f_bysite()` (and consequently of `runread_pmodel()`) 
  has been made consistent into `tibble`
  
## (Non-)Breaking changes
* New driver data.frame format for P-model: now containing the information which
  model to run (`daily` or `onestep`) as additional column `onestep` = `TRUE`/`FALSE`
  in the `params_siml` column. Moreover, in the 
  `forcing` column the nested data.frame has now additional columns for `vwind` and non-zero `ccov` 
  (resulting in columns `ccov`,`co2`,`date`,`fapar`,`netrad`,`patm`,`ppfd`,
  `rain`,`snow`,`temp`,`tmax`,`tmin`,`vpd`,`vwind`) for `daily` model runs 
  and columns (`co2`,`patm`,`ppfd`,`temp`,`vpd`) for `onestep` model runs.
  Each row in `pmodel_drivers` corresponds to a model run (either `daily` 
  or `onestep`) and should have a corresponding row in `pmodel_validation`.
  
  ```
  # A) Compare with previous example data set:
  rsofun::pmodel_drivers |> dplyr::filter(sitename == "FR-Pue")
  rsofun::p_model_oldformat_drivers
  
  # bring new to old format:
  rsofun::pmodel_drivers |> dplyr::filter(sitename == "FR-Pue") |> 
    # remove new column 'onestep' from nested 'param_siml'
    dplyr::mutate(params_siml = purrr::map(params_siml, ~dplyr::select(.x, -'onestep'))) |>
    # 'site_info' remains same
    # 'forcing' has additional info `vwind` that needs removing
    tidyr::unnest(forcing) |>
    dplyr::filter(date >= "2007-01-01" & date < "2013-01-01") |>
    dplyr::select(-vwind) |> 
    dplyr::mutate(ccov = 0) |> # note that fapar and co2 are also different
    tidyr::nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, 
                            snow, rain, tmin, tmax, fapar, co2, ccov))

  # bring old to new format:
  rsofun::p_model_oldformat_drivers |> 
    # 'forcing' add new column `vwind`
    tidyr::unnest(forcing) |> 
    dplyr::mutate(vwind = 2.64, ccov = 0.485) |>
    tidyr::nest(forcing = c(date, temp, vpd, ppfd, netrad, patm, 
                            snow, rain, tmin, tmax, vwind, fapar, co2, ccov)) |>
    # add new column 'onestep' in nested 'param_siml'
    dplyr::mutate(params_siml = purrr::map(params_siml, ~dplyr::mutate(.x, onestep = FALSE)))
    
  rsofun::p_model_oldformat_drivers_vcmax25 |> 
    tidyr::unnest(site_info) |> mutate(latitude = lat) |> tidyr::nest(site_info = c(lon,lat,elv,year_start,year_end,whc)) |>
    # 'forcing' compute growing averages of temp,vpd,ppfd,co2,patm
    # # add tgrowth (daily average daytime temperature)
    tidyr::unnest(forcing) |> 
    rowwise() |>
    mutate(
      doy             = lubridate::yday(date),
      # tgrowth_degC (i.e. average temperature during daytime, considering daylength, assuming sinusoidal temp profile):
      # eq.5 in Peng et al., 2023 (https://onlinelibrary.wiley.com/doi/abs/10.1111/1365-2745.14208)
      # tgrowth_degC    = ingestr::calc_tgrowth(tmin,tmax,lat=latitude,doy=doy),
      tgrowth_degC = mean(tmin, tmax) # actually in rsofun::p_model_oldformat_drivers_vcmax25 tmin == tmax, so no need to calc_tgrowth
    ) |>
    group_by(sitename) |> 
    # 2ii) add growing season (if a month has tavg_monthly > 0)
    mutate(month = lubridate::month(date)) |>
    group_by(sitename, params_siml, site_info, month) |> mutate(tavg_monthly = mean(temp), 
                                                                growing_season  = tavg_monthly > 0) |> 
    group_by(sitename) |>
    # compute means across growing season (i.e. across months for which monthly tmean > 0 deg C)
    filter(growing_season) |>
    group_by(sitename, params_siml, site_info) |>
    summarise(
      # growing_season_doys = paste0(doy,collapse = ","),
      growing_season_length = length(date),
      temp = mean(tgrowth_degC),
      vpd = mean(vpd),
      ppfd=mean(ppfd),  # molm2s
      co2 = mean(co2),
      patm = mean(patm)
    ) |>
    ungroup() |> select(-growing_season_length) |>
    tidyr::nest(forcing = c(temp,vpd,ppfd,co2,patm)) |>
    # add new column 'onestep' in nested 'param_siml'
    dplyr::mutate(params_siml = purrr::map(params_siml, ~dplyr::mutate(.x, onestep = FALSE)))
  ```

* New validation (i.e. observation) data.frame format for P-model: 
  containing a new column `targets` determining which
  variable(s) is/are the target of the corresponding line and which model 
  to run (`daily` or `onestep`).
  Each row in `pmodel_validation` corresponds to a model run (either `daily` 
  or `onestep`) and should have a corresponding row in `pmodel_drivers`.
  
  For each row, the column `data` contains a single 
  `data.frame()` with a column `id` (`onestep`-row) or `date` (`daily`-row) 
  and additional columns for each target (e.g. `gpp` or `nee` or `le`). 
  
  This format assumes for `daily`-rows that each target variable is 
  available on the same dates and for
  `onestep`-rows that multiple target variables have the same number of 
  observations in each `data_frame()`. (NOTE: if in the future this appears 
  too limiting we can define NA or other fill values, or alternatively use 
  nested data.frames - i.e. one for each target value.)
  
  The exact column names of the targets in the `data.frame` in
  the `data` column must correspond to names provided as a list of strings under `targets`.
  
  ```
  # B) Compare with previous example data set:
  rsofun::pmodel_validation |> dplyr::filter(sitename == "FR-Pue")
  rsofun::p_model_oldformat_validation
  
  # bring new to old format:
  rsofun::pmodel_validation |> dplyr::filter(sitename == "FR-Pue") |> 
    # remove new column 'targets'
    dplyr::select(-targets) |>
    # remove new columns inside of 'data':
    tidyr::unnest(data) |>
    dplyr::select(-c(gpp_qc, nee, nee_qc, le, le_qc)) |>
    dplyr::mutate(gpp_unc = 0.01) |>
    tidyr::nest(data = c('date', 'gpp','gpp_unc')) |>
    # order columns
    dplyr::select(sitename, data)
  
  # bring old to new format:
  rsofun::p_model_oldformat_validation |> 
    # add new column 'targets'
    dplyr::mutate(targets = list(c("gpp"))) |>
    # add new columns inside of 'data':
    tidyr::unnest(data) |>
    dplyr::select(-gpp_unc) |>
    dplyr::mutate(gpp_qc = 1, nee = NA, nee_qc = NA, le = NA, le_qc = NA) |>
    tidyr::nest(data = c('date', 'gpp','gpp_qc','nee','nee_qc','le','le_qc')) |>
    # order columns
    dplyr::select(sitename, targets, data)
  
  rsofun::p_model_oldformat_validation_vcmax25 |>
    # add new column 'targets'
    dplyr::mutate(targets = list(c("vcmax25"))) |>
    # add new columns inside of 'data':
    tidyr::unnest(data) |>
    tidyr::nest(data = c('vcmax25', 'vcmax25_unc')) |>
    # order columns
    dplyr::select(sitename, targets, data)
  ```

* Note for future: ideally, validation data and drivers could be a single data.frame.
  This would be ideal, since each row in the validation data.frame() must have a
  corresponding row in the drivers data.frame(). Having a single data.frame() 
  enforces this naturally.


* TODO: what to do about driver and validation data format for biomee?
  
  ```
  > rsofun::biomee_validation |> unnest(data)
  # A tibble: 4 × 3
    sitename variables targets_obs
    <chr>    <chr>           <dbl>
  1 CH-Lae   GPP              1.86
  2 CH-Lae   LAI              6.49
  3 CH-Lae   Density        296.  
  4 CH-Lae   Biomass         44.5 
  
  # bring from old to new
  rsofun::biomee_validation |> unnest(data) |>
    # bring to format with single row = single model run
    tidyr::pivot_wider(values_from = targets_obs, names_from = variables) |>
    # add new column 'targets'
    dplyr::mutate(targets = list(c("GPP", "LAI", "Density", "Biomass"))) |>
    # make 'data' a list of nested data.frames
    # # this double nesting could be useful:tidyr::nest(GPP     = c('GPP'),
    # # this double nesting could be useful:            LAI     = c('LAI'),
    # # this double nesting could be useful:            Density = c('Density'),
    # # this double nesting could be useful:            Biomass = c('Biomass')) |>
    # # this double nesting could be useful:tidyr::nest(data = c('GPP', 'LAI', 'Density', 'Biomass')) |>
    tidyr::nest(data = c('GPP', 'LAI', 'Density', 'Biomass')) |>
    # order columns
    dplyr::select(sitename, targets, data)
  ```
  ```
  rsofun::biomee_gs_leuning_drivers |> 
      tidyr::unnest(forcing) |> 
      tidyr::nest(forcing = c(date, hod, temp, vpd, ppfd, patm, 
                              rain, temp, wind, co2)) # TODO: note here it is called wind instead of vwind
  rsofun::biomee_p_model_drivers |> 
      tidyr::unnest(forcing) |> 
      tidyr::nest(forcing = c(date, hod, temp, vpd, ppfd, patm, 
                              rain, temp, wind, co2)) # TODO: note here it is called wind instead of vwind
  rsofun::biomee_p_model_luluc_drivers |> 
      tidyr::unnest(forcing) |> 
      tidyr::nest(forcing = c(date, hod, temp, vpd, ppfd, patm, 
                              rain, temp, wind, co2)) # TODO: note here it is called wind instead of vwind
  ```
      
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
