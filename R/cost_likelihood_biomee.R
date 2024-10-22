#' Log-likelihood cost function for P-model with different targets

#' 
#' The cost function performs a BiomeE-model run for the input drivers and model parameter
#' values, and computes the outcome's log-likelihood (ll).
#' Separate observational error models are defined for each target variable.
#' Default (and currently only option) is to assume the observational error 
#' to be normally distributed centered around the model output
#' and with standard deviation given as a calibratable input parameter (named as 
#' 'err_{target}').
#' 
#' @param par A vector containing parameter values for \code{'phiRL',
#' 'LAI_light', 'tf_base', 'par_mort'} in that order, and for the error terms
#' corresponding to the target variables, e.g. \code{'err_GPP'} if GPP is a target. 
#' Make sure that
#' the order of the error terms in \code{par} coincides with the order provided in
#' the \code{targets} argument.
#' @param obs A nested data frame of observations, following the structure of \code{biomee_validation},obs
#' for example.
#' @param drivers A nested data frame of driver data, for example \code{biomee_gs_leuning_drivers}.
#' @param targets A character vector indicating the target variables for which the
#' optimization will be done. This string must be a available in both model output
#' and validation data set. 
#' This should be a subset of \code{c("GPP", "LAI", "Density", "Biomass")}.
#' @param parallel (deactivated) A logical specifying whether simulations are to be parallelised
#' (sending data from a certain number of sites to each core). Defaults to
#' \code{FALSE}.
#' @param ncores (deactivated) An integer specifying the number of cores used for parallel
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
#' 
#' @examples
#' # Compute the likelihood for a set of
#' # model parameter values
#' # and example data
#' cost_likelihood_biomee( # reuse likelihood cost function
#'   par = # par must be named
#'     c(# BiomeE model params:
#'       phiRL     = 3.5,
#'       LAI_light = 3.5,
#'       tf_base   = 1,
#'       par_mort  = 1,
#'       # error model params
#'       err_GPP =   0.5),
#'   obs     = biomee_validation,
#'   drivers = biomee_gs_leuning_drivers,
#'   targets = c('GPP'))


cost_likelihood_biomee <- function(
  par,   # model parameters & error terms for each target
  obs,
  drivers,
  targets,
  par_fixed = NULL   # non-calibrated model parameters
  # parallel = FALSE,
  # ncores = 2
){
  # NOTE(fabian): These different cost functions share a LOT of code in common. Consider consolidation for maintainability?

  # predefine variables for CRAN check compliance
  GPP <- LAI <- Density12 <- plantC <- error <- NULL
  
  #### 1) Parse input parameters
  ## define required parameter set based on model parameters
  ## NOTE: unlike P-model, BiomeE has numerous parameters defined in the drivers on 
  ## the 'site_info'-, 'params_tile'-, 'params_species'-, 'params_soil'-level: 
  required_param_list <- list(
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
  required_param_names <- required_param_list |> # BiomeE-model needs these parameters:
    unname() |> unlist()

  if(!is.null(par_fixed)){stop("For BiomeE, par_fixed must be NULL. Fixed parameters are provided through tables in the drivers.")}
  par_fixed_names <- drivers |> 
    dplyr::select(c("site_info", "params_tile", "params_species", "params_soil")) |>
    lapply(function(col){names(col[[1]])}) |> unname() |> unlist() |> sort()
  par_fixed <- structure(rep(NA, length(par_fixed_names)), .Names = par_fixed_names)
  
  ## split calibrated/fixed parameters into model and error model parameters
  ## NOTE: error model parameters must start with "err_" (and model parameters must NOT)
  par_calibrated_model      <- par[ ! grepl("^err_", names(par))] # consider only model parameters for the check
  par_calibrated_errormodel <- par[   grepl("^err_", names(par))]
  par_fixed_model      <- par_fixed[ ! grepl("^err_", names(par_fixed)) ] # consider only model parameters for the check
  par_fixed_errormodel <- par_fixed[   grepl("^err_", names(par_fixed)) ]

  ## check parameters for model
  if ((!rlang::is_empty(par)       && is.null(names(par))) || 
      (!rlang::is_empty(par_fixed) && is.null(names(par_fixed)))){ # if par/par_fixed exist, they must be named!
    stop("Error: Input calibratable and fixed parameters need to be provided as named vectors.")
  }
  if (!identical(unique(sort(c(names(par_calibrated_model), names(par_fixed_model)))), sort(required_param_names))){
    missing_params <- required_param_names[!(required_param_names %in% names(par_calibrated_model) | 
                                               required_param_names %in% names(par_fixed_model))]
    stop(sprintf(paste0("Error: Input calibratable and fixed parameters do not ",
                        "match required model parameters:",
                        "\n         missing:            c(%s)",
                        "\n         ",
                        "\n         received par:       c(%s)",
                        "\n         received par_fixed: c(%s)",
                        "\n         required:           c(%s)"),
                 
                 paste0(sort(missing_params), collapse = ", "),
                 paste0(sort(names(par_calibrated_model)), collapse = ", "),
                 paste0(sort(names(par_fixed_model)), collapse = ", "),
                 paste0(sort(required_param_names), collapse = ", ")))
  }

  #### 2) Update inputs to runread_biomee_f() with the provided parameters

  #### 2a) prepare global parameters for argument 'par' of runread_biomee_f()
  ## NOTE: unlike the P-Model, BiomeE-model has no separate argument 'par' to
  ##       `runread_biomee_f()`. All the params are provided through the driver

  #### 2b) prepare site-specific parameters for 'driver' argument of runread_biomee_f()
  # Here we need to overwrite parameters specified in the driver data where necessary
  # Option i) Manual solution
    # drivers$params_species[[1]]$phiRL[]  <- par[1]
    # drivers$params_species[[1]]$LAI_light[]  <- par[2]
    # drivers$params_tile[[1]]$tf_base <- par[3]
    # drivers$params_tile[[1]]$par_mort <- par[4]
  # Option ii) Automatization
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
  for (parname in names(par_calibrated_model)) {
    value <- par_calibrated_model[parname]
    # cat("Overwriting parameter:'", parname, "' with value=", value, "\n")
    drivers <- mutate_nested_column(drivers, parname, value)
  }  

  #### 3) Run the model: runread_biomee_f()

  ## run the model
  model_out_full <- runread_biomee_f(
    drivers,
    # par = params_modl, # unused by BiomeE
    makecheck = TRUE,
    parallel = FALSE #parallel = parallel,
    # ncores = ncores
  )

  #### 4) Combine modelled and observed variables
  #### 4a) POSTPROCESS model output
    # possible P-model outputs:
    # model_out_full$data[[1]] |> tibble()                       # daily forest-specific output :    date, year_dec, properties/fluxes/states/...
    # possible BiomeE-model outputs:
    # model_out_full$data[[1]]$output_daily_tile |> tibble()     # daily forest-specific output :         year, doy, properties/fluxes/states/...
    # model_out_full$data[[1]]$output_annual_tile |> tibble()    # annual forest-specific output:         year,      properties/fluxes/states/...
    # model_out_full$data[[1]]$output_annual_cohorts |> tibble() # cohort-specific output       : cohort, year,      properties/fluxes/states/...

  ## clean model output and unnest
  mod <- model_out_full |> 
    # NOTE: for BiomeE-model output, for each row (i.e. site) the data-column contains a list of 3 data.frames
    #       The following operation separates this data column into three nested columns
    tidyr::unnest_wider(data) |> # this keeps the three outputs: 'biomee_output_daily_tile', 'biomee_output_annual_tile', 'biomee_output_annual_cohorts'
    dplyr::rename_with(~paste0('biomee_', .x), .cols = -c('sitename'))
    
  mod <- mod |> select('sitename', biomee_output_annual_tile) |> 
    tidyr::unnest('biomee_output_annual_tile') |>
    # keep only target model outputs:
    dplyr::select('sitename', 'year', 
                  'GPP', 'LAI', 'Density12', 'plantC', #TODO: here we should only keep the targets. 
                  all_of(targets |> stringr::str_replace_all(
                    c('Density' = 'Density12',  # TODO: some hardcoded renames
                      'Biomass' = 'plantC')))   # TODO: some hardcoded renames
                  ) |>
    dplyr::rename_with(~paste0(.x, '_mod'), 
                       .cols = -c('sitename', 'year'))

  # did we spin up?
  spin_up <- drivers$params_siml[[1]]$spinup
  # drop spinup years if activated
  if (spin_up){
    spin_up_years <- drivers$params_siml[[1]]$spinupyears + 1
  } else {
    spin_up_years <- 0
  }
  mod <- mod #|>
  # group_by(sitename)|> # TODO: ensure the filtering is per site
  # filter(year > spin_up_years) |> # TODO: fix how spinup years are filtered
  # TODO: fix this. Do we really need to remove the spinupyears? Aren't they already removed in the fortran code?
  # TODO: fix this. Do we really need to remove the spinupyears? Aren't they already removed in the fortran code?


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
    # TODO: for BiomeE currently no timeseries calibraiton is implemented 
    #   # Unnest timeseries observations for our targets
    #   obs_timeseries <- obs[obs_row_is_timeseries, ] |>
    #     dplyr::select(sitename, data) |>
    #     tidyr::unnest(data) |>
    #     dplyr::select(all_of(c('sitename', 'date', targets))) |> # NOTE: any_of would silently drop unavailable
    #     dplyr::rename_with(~paste0(.x, '_obs'),
    #                        .cols = -c(sitename, date))
    #
    #   if(ncol(obs_timeseries) < 3){
    #     warning("Dated observations (fluxes/states) are missing for the chosen targets.")
    #     mod_df_timeseries <- data.frame()
    #   }else{
    #     # Join model output and timeseries observations
    #     # TODO: consider model spinup??
    #     mod_df_timeseries <- mod |>
    #       dplyr::filter(sitename %in% timeseries_sites) |>
    #       dplyr::left_join(
    #         obs_timeseries,
    #         by = c('sitename', 'date')) # observations with missing date are ignored
    #   }
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
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      tidyr::pivot_wider(values_from='targets_obs', names_from='variables') |> # TODO: this is only needed for BiomeE model (see comment on wide/long data structure above)
      dplyr::select(all_of(c('sitename', targets))) |> # NOTE: any_of would silently drop unavailable
      dplyr::rename_with(~paste0(.x, '_obs'),
                         .cols = -c(sitename)) # -c(sitename, year)

    if(ncol(obs_df_trait) < 2){
      warning("Non-dated observations (traits) are missing for the chosen targets.")
      mod_df_trait <- data.frame()
    }else{
      # Join model output and trait observations
      # Derive constants form model output (traits)
      mod_df_trait <- mod |>
        dplyr::filter(sitename %in% trait_sites) |>
        dplyr::group_by(sitename) |>
        # a) BiomeE: Aggregate variables from the model mod taking the last 500 yrs if spun up
        dplyr::slice_tail(n = max(0, 500-spin_up_years)) |> # TODO: make the number of years a calibration input argument instead of hardcoding
        dplyr::summarise(
          period      = paste0("years_",paste0(range(year), collapse="_to_")),
          GPP_mod     = mean(GPP_mod),
          LAI_mod     = stats::quantile(LAI_mod, probs = 0.95, na.rm=TRUE),
          Density_mod = mean(Density12_mod), # TODO: some hardcoded renames
          Biomass_mod = mean(plantC_mod)     # TODO: some hardcoded renames
        ) |>
        # # b) P-model: get growing season average traits
        # dplyr::summarise(across(ends_with("_mod") & !starts_with('gpp'),
        #                         ~ sum(.x * gpp_mod/sum(gpp_mod)),
        #                         .names = "{.col}")) |>
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
    par_error_sd <- c(par_calibrated_errormodel, par_fixed_errormodel)[[paste0('err_', target)]]
    ll_df[ll_df$target == target, 'll'] <-
      sum(stats::dnorm(
        x    = df_target[[target_mod]], # model
        mean = df_target[[target_obs]], # obs
        sd   = par_error_sd,            # error model
        log  = TRUE))
  }
  ll <- sum(ll_df$ll)

  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}

  return(ll)
}

