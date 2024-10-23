#' Log-likelihood cost function for P-model with different targets

#' 
#' The cost function performs a P-model run for the input drivers and model parameter
#' values, and computes the outcome's log-likelihood (ll).
#' Separate observational error models are defined for each target variable.
#' Default (and currently only option) is to assume the observational error 
#' to be normally distributed centered around the model output
#' and with standard deviation given as a calibratable input parameter (named as 
#' 'err_\[target\]').
#' 
#' @param par A named vector of values for the parameters that are being calibrated, 
#' consisiting of
#'    - parameters for the error model for each target variable (for example \code{'err_gpp'})
#'    - parameters for the model (described in \code{\link{runread_pmodel_f}}), 
#' Parameters from 'par' replace either global parameters (specified in argument 'par')
#' of the function \code{\link{runread_pmodel_f}} or then site-specific 
#' parameters defined in the driver data.frame.
#' Note that the same parameters are applied globally to all sites in the driver 
#' data.frame.
#' Note that parameters for the error model of each target must correspond to the 
#' argument `targets0`
#' 
#' @param obs A nested data.frame of observations, with columns \code{'sitename'}
#' and \code{'data'} (see \code{\link{p_model_validation}}, 
#' \code{\link{p_model_validation_vcmax25}}, or \code{\link{biomee_validation}}
#' to check their structure).
#' @param drivers A nested data.frame of driver data.
#' See\code{\link{p_model_drivers}}, \code{\link{biomee_gs_leuning_drivers}}, 
#' \code{\link{biomee_p_model_drivers}}, and \code{\link{p_model_drivers_vcmax25}}, 
#' or p_hydro_drivers for a description of the data structure.
#' @param targets A character vector indicating the target variables for which the
#' optimization will be done. This string must be a available in both model output
#' and validation data set.
#' name of the \code{data} data.frame belonging to the 'obs' data.frame 
#' (for example 'gpp').
#' TODO: specify how time-variable and constant observations are used.
#' @param par_fixed A named list of model parameter values to keep fixed during the
#' calibration. These should complement the input \code{par} such that all model
#' parameters are passed on to \code{\link{runread_pmodel_f}}.
#' Note that in BiomeE these must be NULL.
#' @param parallel A logical specifying whether simulations are to be parallelised
#' (sending data from a certain number of sites to each core). Defaults to
#' \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel
#' computing. Defaults to 2.
#' 
#' @return The log-likelihood of the observed target values, for a given error 
#' model and parameter set. 
#' The default error model assumes that observed target values are independent,
#' normally distributed, centered at the model predictions and with parametrized
#' standard deviation given as input (via `par` since this/these error model 
#' parameter(s) are also estimated with `BayesianTools`. See BayesianTools' 
#' "Parameter calibration and cost functions" vignette.).
#' 
#' @details The cost function performs a model run for the value of
#' \code{par} given as argument. 
#' These parameters must define all model parameters needed to run.
#' The optimization maximizes the likelihood and can be  be run using \code{BayesianTools}.
#' 
#' Multiple observation types can be simultaneously calibrated, provided an error model is given for each of them. 
#' Observational data can be constant (observed tratits) or time-varying (observed fluxes/states).
#' This is distinguished by the presence of time information (i.e. a column named 'date')
#' in the observational data.frame \code{obs} (i.e. in the nested column 'data' from a single row \code{obs}). 
#' (For BiomeE this is not yet implemented.)
#' Thus to calibrate simultaneously to constant and time-varying observations of
#' a single (or multiple) site, each site must be repeated in the observation data
#' \code{obs}.
#' 
#' Then the modelled time series (fluxes, states, or variable leaf traits)
#' is compared to the observed values on those same dates (e.g. for GPP). 
#' Alternatively (without time information), the predicted value is aggregated 
#' (e.g at steady state or the GPP-weighted average of acclimatized leaf traits (e.g. Vcmax25)).
#' 
#' @export
#' @importFrom stringr str_replace_all
#' 
#' @examples
#' # Compute the likelihood for a set of
#' # model parameter values for biomee:
#' # and example data
#' cost_likelihood_biomee(          # reuse likelihood cost function
#'  par = c(                        # must be named
#'    # BiomeE model params:
#'    phiRL     = 3.5,
#'    LAI_light = 3.5,
#'    tf_base   = 1,
#'    par_mort  = 1,
#'    # error model params
#'    err_GPP   = 0.5
#'  ),
#'  obs     = biomee_validation,
#'  drivers = biomee_gs_leuning_drivers,
#'  targets = c('GPP')
#' )
#' @examples
#' # Compute the likelihood for a set of
#' # model parameter values involved in the
#' # temperature dependence of kphio
#' # and example data
#' cost_likelihood_pmodel(          # reuse likelihood cost function
#'  par = c(                        # must be named
#'    # P-model params
#'    kphio       = 0.05,
#'    kphio_par_a = -0.01,
#'    kphio_par_b = 1.0,
#'    # error model params
#'    err_gpp     = 2.0
#'  ),
#'  obs     = p_model_validation,   # example data from package
#'  drivers = p_model_drivers,
#'  targets = c("gpp"),
#'  par_fixed = c(
#'   soilm_thetastar    = 0.6 * 240,# to recover old setup with soil moisture stress
#'   soilm_betao        = 0.0,
#'   beta_unitcostratio = 146.0,
#'   rd_to_vcmax        = 0.014,    # value from Atkin et al. 2015 for C3 herbaceous
#'   tau_acclim         = 30.0,
#'   kc_jmax            = 0.41
#'  )
#' )

cost_likelihood_pmodel <- function(
  par,   # model parameters & error terms for each target
  obs,
  drivers,
  targets,
  par_fixed = NULL,   # non-calibrated model parameters
  parallel = FALSE,
  ncores = 2
){
  cost_likelihood_generic_1(
    par        = par,
    obs        = obs,
    drivers    = drivers,
    targets    = targets,
    par_fixed  = par_fixed,
    parallel   = parallel,
    ncores     = ncores,
    curr_model = "p-model")
}

cost_likelihood_generic_1 <- function(
  par,   # model parameters & error terms for each target
  obs,
  drivers,
  targets,
  par_fixed = NULL,   # non-calibrated model parameters
  parallel = FALSE,
  ncores = 2,
  curr_model = c("biomee", "p-model")
){
  match.arg(curr_model, several.ok = FALSE)

  #### 1) Parse input parameters
  if(curr_model == "biomee"){ # For BiomeE only
    if(!is.null(par_fixed)){stop("For BiomeE, par_fixed must be NULL. Fixed parameters are provided through tables in the drivers.")}
  }
  ## define required parameter set 'required_param_names' based on model parameters
  if(curr_model == "biomee"){
    required_param_names <- c()# BiomeE-model needs no global parameters
  }else if(curr_model == "p-model"){
    required_param_names <- c(# P-model needs these global parameters:
                  'beta_unitcostratio',
                  'kc_jmax',
                  'kphio',
                  'kphio_par_a',
                  'kphio_par_b',
                  'rd_to_vcmax',
                  'soilm_betao', 'soilm_thetastar',
                  'tau_acclim'
    )
  }else{
    stop("Arguments 'curr_model' must be either 'biomee' or 'p-model'")
  }
  ## define valid driver parameter names
  # they were hardcoded based on the possible results of the outcommented code:
        # rsofun::biomee_gs_leuning_drivers |>
        #   dplyr::select(any_of(c("site_info", "params_tile", "params_species", "params_soil"))) |>
        #   lapply(function(col){names(col[[1]])}) |> dput() # |> unname() |> unlist() |> sort()
        # rsofun::biomee_p_model_drivers |>
        #   dplyr::select(any_of(c("site_info", "params_tile", "params_species", "params_soil"))) |>
        #   lapply(function(col){names(col[[1]])}) |> dput() # |> unname() |> unlist() |> sort()
        # rsofun::p_model_drivers |>
        #   dplyr::select(any_of(c("site_info", "params_tile", "params_species", "params_soil"))) |>
        #   lapply(function(col){names(col[[1]])}) |> dput() # |> unname() |> unlist() |> sort()
        # rsofun::p_model_drivers_vcmax25 |>
        #   dplyr::select(any_of(c("site_info", "params_tile", "params_species", "params_soil"))) |>
        #   lapply(function(col){names(col[[1]])}) |> dput() # |> unname() |> unlist() |> sort()
  if(curr_model == "biomee"){
    ## NOTE: unlike P-model, BiomeE has numerous parameters defined in the drivers on 
    ## the 'site_info'-, 'params_tile'-, 'params_species'-, 'params_soil'-level: 
    valid_par_model_driver_list <- list(
      site_info = c("sitename", "lon", "lat", "elv", "year_start",
                    "year_end", "classid", "c4", "whc", "koeppen_code", "igbp_land_use",
                    "plant_functional_type", "date_start", "date_end"),
      params_tile = c("soiltype",
                      "FLDCAP", "WILTPT", "K1", "K2", "K_nitrogen", "MLmixRatio", "etaN",
                      "LMAmin", "fsc_fine", "fsc_wood", "GR_factor", "l_fract", "retransN",
                      "f_initialBSW", "f_N_add", "tf_base", "par_mort", "par_mort_under"),
      params_species = c("lifeform", "phenotype", "pt", "alpha_FR",
                         "rho_FR", "root_r", "root_zeta", "Kw_root", "leaf_size", "Vmax",
                         "Vannual", "wet_leaf_dreg", "m_cond", "alpha_phot", "gamma_L",
                         "gamma_LN", "gamma_SW", "gamma_FR", "tc_crit", "tc_crit_on",
                         "gdd_crit", "betaON", "betaOFF", "alphaHT", "thetaHT", "alphaCA",
                         "thetaCA", "alphaBM", "thetaBM", "seedlingsize", "maturalage",
                         "v_seed", "mortrate_d_c", "mortrate_d_u", "LMA", "leafLS", "LNbase",
                         "CNleafsupport", "rho_wood", "taperfactor", "lAImax", "tauNSC",
                         "fNSNmax", "phiCSA", "CNleaf0", "CNsw0", "CNwood0", "CNroot0",
                         "CNseed0", "Nfixrate0", "NfixCost0", "internal_gap_frac", "kphio",
                         "phiRL", "LAI_light"),
      params_soil = c("type", "GMD", "GSD",
                      "vwc_sat", "chb", "psi_sat_ref", "k_sat_ref", "alphaSoil", "heat_capacity_dry")
    )
  }else if(curr_model == "p-model"){
    valid_par_model_driver_list <- list(
      site_info = c("lon", "lat", "elv", "year_start", "year_end", "whc"))
  }else{
    stop("Arguments 'curr_model' must be either 'biomee' or 'p-model'")
  }

  ## split calibrated/fixed parameters into model and error model parameters
  ## NOTE: error model parameters must start with "err_" (and model parameters must NOT)
  par_model_toCalibrate <- par[ ! grepl("^err_", names(par))] # consider only model parameters for the check
  par_error_toCalibrate <- par[   grepl("^err_", names(par))]
  par_model_fixed       <- par_fixed[ ! grepl("^err_", names(par_fixed)) ] # consider only model parameters for the check
  par_error_fixed       <- par_fixed[   grepl("^err_", names(par_fixed)) ]

  ## check parameters for model
  if ((!rlang::is_empty(par)       && is.null(names(par))) || 
      (!rlang::is_empty(par_fixed) && is.null(names(par_fixed)))){ # if par/par_fixed exist, they must be named!
    stop("Error: Input calibratable and fixed parameters need to be provided as named vectors.")
  }

  if(curr_model == "biomee"){
    # check completeness: for p-model 'par_model_toCalibrate' and 'par_model_fixed' == 'required_param_names'
    # NOT NEEDED for BiomeE since length(required_param_names) == 0
  }else if(curr_model == "p-model"){
    # check completeness: for p-model 'par_model_toCalibrate' and 'par_model_fixed' == 'required_param_names'
    if(!identical(unique(sort(c(names(par_model_toCalibrate), names(par_model_fixed)))), sort(required_param_names))){
      missing_params <- required_param_names[!(required_param_names %in% names(par_model_toCalibrate) | 
                                                 required_param_names %in% names(par_model_fixed))]
      stop(sprintf(paste0("Error: Input calibratable and fixed parameters do not ",
                          "match required model parameters:",
                          "\n         missing:            c(%s)",
                          "\n         ",
                          "\n         received par:       c(%s)",
                          "\n         received par_fixed: c(%s)",
                          "\n         required:           c(%s)"),
                   paste0(sort(missing_params), collapse = ", "),
                   paste0(sort(names(par_model_toCalibrate)), collapse = ", "),
                   paste0(sort(names(par_model_fixed)), collapse = ", "),
                   paste0(sort(required_param_names), collapse = ", ")))
    }
  }else{
    stop("Arguments 'curr_model' must be either 'biomee' or 'p-model'")
  }

  #### 2) Update inputs to runread_pmodel_f()/runread_biomee_f() with the provided parameters

  #### 2a) reorganize parameters into a group of global parameters (provided as 
  ####     'par') and site specific parameters (provided through the data.frame 
  ####     'driver')
  # NOTE: actually, we don't need to track which ones are toCalibrate and which are fixed
  #       here we now need to know where we need to apply them
  par_error <- as.list(c(par_error_fixed, par_error_toCalibrate)) # runread_pmodel_f requires a named list
  par_model <- as.list(c(par_model_fixed, par_model_toCalibrate)) # runread_pmodel_f requires a named list
  
  if(curr_model == "biomee"){
    global_pars <- c() ## NOTE: unlike the P-Model, BiomeE-model has no separate argument 'par' to
                       ##       `runread_biomee_f()`. All the params are provided through the driver
    par_model_global <- par_model[   names(par_model) %in% global_pars ]
    par_model_driver <- par_model[ ! names(par_model) %in% global_pars ]
    rm(global_pars)
  }else if(curr_model == "p-model"){
        # TODO: this was added in PHydro branch: but it can already be considered when refactoring this
        # ## if WHC is treated as calibratable, remove it from par and overwrite site 
        # ## info with the same value for (calibrated) WHC for all sites.
        # driver_pars <- c("whc")
    driver_pars <- c() ## NOTE: before the pydro model all calibratable params were global params
                       ##       i.e. the same for all sites and none were provided through the driver
                       ##       With phydro model we'll be introducing 'whc' as a site-specific parameter
                       ##       (that is potentially calibratable to a global value)
    par_model_driver <- par_model[   names(par_model) %in% driver_pars]
    par_model_global <- par_model[ ! names(par_model) %in% driver_pars]
    rm(driver_pars)
  }else{
    stop("Arguments 'curr_model' must be either 'biomee' or 'p-model'")
  }
  
  rm(par_error_fixed, par_error_toCalibrate, par_model_fixed, par_model_toCalibrate,
     par_model)
  # below only use: par_error, par_model_global, and par_model_driver

  #### 2b) prepare argument 'par' of runread_pmodel_f() with global parameters 
  # already done above: use par = par_model_global

  ## check validity of par_model_global
  valid_par_model_global_names <- required_param_names
  stopifnot(all(names(par_model_global) %in% valid_par_model_global_names)) # This check is internal
  
  #### 2c) prepare argument 'driver' of runread_pmodel_f()/runread_biomee_f() with site-specific parameters
  # Here we need to overwrite parameters specified in the driver data where necessary

  ## check validity of par_model_driver
  valid_par_model_driver <- valid_par_model_driver_list |> unname() |> unlist()
  
  # check validity: 'par_model_driver' must be a subset of 'valid_par_model_driver'
  if(!all(names(par_model_driver) %in% valid_par_model_driver)){
    surplus_params <- names(par_model_driver)[!(names(par_model_driver) %in% valid_par_model_driver)]
    stop(sprintf(paste0("Error: Input calibratable parameters do not ",
                        "match valid model parameters:",
                        "\n         surplus:            c(%s)",
                        "\n         ",
                        "\n         received par:       c(%s)",
                        "\n         received par_fixed: c(%s)",
                        "\n         valid:           c(%s)"),
                 paste0(sort(surplus_params), collapse = ", "),
                 paste0(sort(names(par_model_driver)), collapse = ", "),
                 paste0(sort(names(par_model_fixed)), collapse = ", "),
                 paste0(sort(valid_par_model_driver), collapse = ",")))
  }

  # Function to mutate a column inside the nested data.frame of the driver
  mutate_nested_column <- function(mod, column_name, new_value) {
    # check occurrence of parameters:
    all_nested_columns <- lapply(mod, function(column){names(column[[1]])}) |> unname() |> unlist()
    if(!(column_name %in% all_nested_columns)){
      stop(sprintf("Could not modify param: %s. It was not found in driver. Available params to overwrite: %s", 
                   column_name, 
                   paste0(all_nested_columns, collapse = ",")))
    }
    if (length(new_value) > 1) {stop("Please check if mutate_nested_column() correctly updates vector values. It was only tested for scalars.")}
    
    # perform replacements:
    mod |>
      dplyr::mutate(dplyr::across(
        dplyr::where(~is.list(.x)), # do it across all nested columns
        function(column) {
          purrr::map(column, 
                     function(nested_df){
                       if (column_name %in% names(nested_df)) {
                         dplyr::mutate(nested_df, !!sym(column_name) := new_value)
                       } else {
                         nested_df
                       }}
          )}
      ))
  }     # test_df <- tibble(
        #   id = 1:3,
        #   data = list(
        #     tibble(a = 1:3, b = 4:6),
        #     tibble(a = 13:15, b = 16:18)),
        #   data2 = list(
        #     tibble(c = 1:3, bd = 4:6),
        #     tibble(c = 13:15, bd = 16:18)))
        # unnest(test_df, c(data, data2))
        # test_df <- mutate_nested_column(test_df, "bd", 19); unnest(test_df, c(data, data2))
        # test_df <- mutate_nested_column(test_df, "bd_notexisting", 22); unnest(test_df, c(data, data2))
  
  # Loop over the names and values of modified parameters
  for (parname in names(par_model_driver)) {
    value <- par_model_driver[[parname]] # NOTE: this shold be a scalar, use `[[` !
    # cat("Overwriting parameter:'", parname, "' with value=", value, "\n")
    drivers <- mutate_nested_column(drivers, parname, value)
  }

  #### 3) Run the model: runread_pmodel_f()/runread_biomee_f()
  ## run the model
  if(curr_model == "biomee"){
    model_out_full <- runread_biomee_f(
      drivers = drivers,
      # par = par_model_global, # unused by BiomeE
      makecheck = TRUE,
      parallel = parallel,
      ncores = ncores
    )
  }else if(curr_model == "p-model"){
    model_out_full <- runread_pmodel_f(
      drivers,
      par = par_model_global,
      makecheck = TRUE,
      parallel = parallel,
      ncores = ncores
    )
  }else{
    stop("Arguments 'curr_model' must be either 'biomee' or 'p-model'")
  }

  #### 4) Combine modelled and observed variables
  #### 4a) POSTPROCESS model output
    # possible P-model outputs:
    # model_out_full$data[[1]] |> tibble()                       # daily forest-specific output :    date, year_dec, properties/fluxes/states/...
    # possible BiomeE-model outputs:
    # model_out_full$data[[1]]$output_daily_tile |> tibble()     # daily forest-specific output :         year, doy, properties/fluxes/states/...
    # model_out_full$data[[1]]$output_annual_tile |> tibble()    # annual forest-specific output:         year,      properties/fluxes/states/...
    # model_out_full$data[[1]]$output_annual_cohorts |> tibble() # cohort-specific output       : cohort, year,      properties/fluxes/states/...

  ## clean model output and unnest
  if(curr_model == "biomee"){
    mod <- model_out_full |>
      # NOTE: for BiomeE-model output, for each row (i.e. site) the data-column contains a list of 3 data.frames
      #       The following operation separates this data column into three nested columns
      tidyr::unnest_wider('data') |> # this keeps the three outputs: 'biomee_output_daily_tile', 'biomee_output_annual_tile', 'biomee_output_annual_cohorts'
      dplyr::rename_with(~paste0('biomee_', .x), .cols = -c('sitename'))
      
    mod <- mod |> select('sitename', 'biomee_output_annual_tile') |> 
      tidyr::unnest('biomee_output_annual_tile') |>
      # keep only target model outputs:
      dplyr::select('sitename', 'year', 
                    'GPP', 'LAI', 'Density12', 'plantC', #TODO: here we should only keep the targets. 
                    all_of(targets |> stringr::str_replace_all(    #TODO: if removed remove also @importFrom
                      c('Density' = 'Density12',  # TODO: some hardcoded renames
                        'Biomass' = 'plantC')))   # TODO: some hardcoded renames
                    ) |>
      dplyr::rename_with(~paste0(.x, '_mod'), 
                        .cols = -c('sitename', 'year'))
  }else if(curr_model == "p-model"){
    mod <- model_out_full |>
      tidyr::unnest('data') |> # this keeps the output
      # gpp is used to get average trait prediction
      dplyr::select('sitename', 'date',
                    'gpp',
                    all_of(targets)) |>
      dplyr::rename_with(~paste0(.x, '_mod'),
                        .cols = -c('sitename', 'date'))
  }else{
      stop("Arguments 'curr_model' must be either 'biomee' or 'p-model'")
  }

  if(curr_model == "biomee"){
    # drop spinup years if activated
    if (drivers$params_siml[[1]]$spinup){
      spinup_years <- drivers$params_siml[[1]]$spinupyears + 1 # TODO: why plus 1?
    } else {
      spinup_years <- 0
    }
    mod <- mod #|>
    # group_by(sitename)|> # TODO: ensure the filtering is per site
    # filter(year > spinup_years) |> # TODO: fix how spinup years are filtered
    # TODO: fix this. Do we really need to remove the spinupyears? Aren't they already removed in the fortran code?
    # TODO: fix this. Do we really need to remove the spinupyears? Aren't they already removed in the fortran code?
  }else if(curr_model == "p-model"){
    # TODO: if needed add spinup removal to p-model as well...
  }else{
      stop("Arguments 'curr_model' must be either 'biomee' or 'p-model'")
  }


  #### 4b) PREPROCESS observation data
  #### 4c) JOIN observed and modelled data # TODO: split this more clearly from 4b
  # separate validation data into sites containing
  # time series (fluxes/states) and sites containing constants (traits)
  obs_row_is_timeseries <- apply(obs, 1, function(x){ 'date' %in% colnames(x$data)})
  obs_row_is_trait <- !obs_row_is_timeseries
  timeseries_sites <- obs[obs_row_is_timeseries, ][['sitename']] # individual sites can be part of both
  trait_sites      <- obs[!obs_row_is_timeseries, ][['sitename']] # individual sites can be part of both
                                                              # NOTE: to have an individual site in both:
                                                              # obs must contain a row where data contains a data.frame with column 'date'
                                                              #              and a row where data contains a data.frame without column 'date'
  if(sum(obs_row_is_timeseries) > 0){
    if(curr_model == "biomee"){
      # TODO: for BiomeE currently no timeseries calibration is implemented
      mod_df_timeseries <- data.frame()
    }else if(curr_model == "p-model"){
      # Unnest timeseries observations for our targets
      obs_timeseries <- obs[obs_row_is_timeseries, ] |>
        dplyr::select(sitename, data) |>
        tidyr::unnest(data) |>
        dplyr::select(any_of(c('sitename', 'date', targets))) |> # NOTE: any_of silently drops unavailable targets, hence 'targets' can contain timeseries as well as traits without erroring.
        dplyr::rename_with(~paste0(.x, '_obs'),
                          .cols = -c(sitename, date))

      if(ncol(obs_timeseries) < 3){
        warning("Dated observations (fluxes/states) are missing for the chosen targets.")
        mod_df_timeseries <- data.frame()
      }else{
        # Join model output and timeseries observations
        mod_df_timeseries <- mod |>
          dplyr::filter(sitename %in% timeseries_sites) |>
          dplyr::left_join(
            obs_timeseries,
            by = c('sitename', 'date')) # observations with missing date are ignored
      }
    }else{
        stop("Arguments 'curr_model' must be either 'biomee' or 'p-model'")
    }
  }else{
    mod_df_timeseries <- data.frame()
  }
  # TODO: homogenize format rsofun::biomee_validation with rsofun::p_model_validation_vcmax25
  #       for pmodel it is a wide data structure, and for biomee it is a long data structure
        # rsofun::p_model_validation_vcmax25 |> unnest(data)
            # # A tibble: 4 × 3
            # # Groups:   sitename [4]
            # # A tibble: 4 × 3
            # # Groups:   sitename [4]
            #   sitename               vcmax25 vcmax25_unc
            #   <chr>                    <dbl>       <dbl>
            # 1 Reichetal_Colorado   0.0000339   0.0000136
            # 2 Reichetal_New_Mexico 0.0000757   0.0000163
            # 3 Reichetal_Venezuela  0.0000472   0.0000164
            # 4 Reichetal_Wisconsin  0.0000502   0.0000147
        # rsofun::biomee_validation |> unnest(data)
            #     sitename variables targets_obs
            #   <chr>    <chr>           <dbl>
            # 1 CH-Lae   GPP              1.86
            # 2 CH-Lae   LAI              6.49
            # 3 CH-Lae   Density        296.
            # 4 CH-Lae   Biomass         44.5
  if(sum(obs_row_is_trait) > 0){
    # Unnest trait observations for our targets
    obs_df_trait <- obs[obs_row_is_trait, ] |>
      dplyr::select('sitename', 'data') |>
      tidyr::unnest('data') |>
      # do this conditionally: # TODO: this is only needed for BiomeE model (see comment on wide/long data structure above)
      # source for conditional code: https://stackoverflow.com/a/76792390
      {\(.) if (curr_model == "biomee") tidyr::pivot_wider(., values_from='targets_obs', names_from='variables') else . }() |> 
      # end of conditional do
      dplyr::select(any_of(c('sitename', targets))) |> # NOTE: any_of silently drops unavailable targets, hence 'targets' can contain timeseries as well as traits without erroring.
      dplyr::rename_with(~paste0(.x, '_obs'), .cols = -c('sitename'))

    if(ncol(obs_df_trait) < 2){
      warning("Non-dated observations (traits) are missing for the chosen targets.")
      mod_df_trait <- data.frame()
    }else{
      # Join model output and trait observations
      # Derive constants form model output (traits)
      mod_df_trait <- mod |>
        dplyr::filter(.data$sitename %in% trait_sites) |>
        dplyr::group_by(.data$sitename) |>
        # do this conditionally:
        # source for conditional code: https://stackoverflow.com/a/76792390
        {\(.) if(curr_model == "biomee") {. |>
          # a) BiomeE: Aggregate variables from the model mod taking the last 500 yrs if spun up
            dplyr::slice_tail(n = max(0, 500-spinup_years)) |> # TODO: make the number of years a calibration input argument instead of hardcoding
            dplyr::summarise(
              period      = paste0("years_",paste0(range(.data$year), collapse="_to_")),
              GPP_mod     = mean(.data$GPP_mod),
              LAI_mod     = stats::quantile(.data$LAI_mod, probs = 0.95, na.rm=TRUE),
              Density_mod = mean(.data$Density12_mod), # TODO: some hardcoded renames
              Biomass_mod = mean(.data$plantC_mod)     # TODO: some hardcoded renames
            )
          }else if(curr_model == "p-model"){. |>
          # b) P-model: get growing season average traits
            dplyr::summarise(across(ends_with("_mod") & !starts_with('gpp'),
                                ~ sum(.x * .data$gpp_mod/sum(.data$gpp_mod)),
                                .names = "{.col}"))
          }else{
            stop("Arguments 'curr_model' must be either 'biomee' or 'p-model'")
          }
        }() |>
        # end of conditional do
        dplyr::left_join(
          obs_df_trait,
          by = c('sitename')        # compare yearly averages rather than daily obs
        )
    }
  }else{
    mod_df_trait <- data.frame()
  }

  #### 5) Compute log-likelihood
  # TODO: change this approach to another one based on a long data.frame containing
  # columns for 'sitename', 'target', 'error_model', 'obs_value', 'pred_value'
  # TODO: here we could also split the joining of obs-pred from the into two separate functions

  # loop over targets to compute log-likelihood ll
  ll_df <- data.frame(target = targets,
                      ll     = NaN) # initialize data.frame for ll's of the different target variables

  for (target in targets){
    target_obs <- paste0(target, '_obs')
    target_mod <- paste0(target, '_mod')

    # check (needed?):
    if(target_obs %in% colnames(mod_df_timeseries) & target_obs %in% colnames(mod_df_trait)) {
      stop(sprintf("Target '%s' cannot be simultaneously in mod_df_timeseries and df_trait.", target))
    }

    # get observations and predicted target values, without NA
    df_target <- if(target_obs %in% colnames(mod_df_timeseries)){
      mod_df_timeseries
    }else if(target_obs %in% colnames(mod_df_trait)){
      mod_df_trait
    }else{
      stop(sprintf("Target variable: '%s', was not found in the provided observations. Please check.", target))
    }
    df_target <- df_target[, c(target_mod, target_obs)] |> tidyr::drop_na()

    # calculate normal log-likelihood
    ll_df[ll_df$target == target, 'll'] <-
      sum(stats::dnorm(
        x    = df_target[[target_mod]], # model
        mean = df_target[[target_obs]], # obs
        sd   = par_error[[paste0('err_', target)]], # error model
        log  = TRUE))
  }
  ll <- sum(ll_df$ll)

  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}

  return(ll)
}

