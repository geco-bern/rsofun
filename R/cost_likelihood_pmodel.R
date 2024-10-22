#' Cost function computing a log-likelihood for calibration of P-model
#' parameters
#' 
#' The cost function performs a P-model run for the input drivers and model parameter
#' values, and computes the outcome's normal log-likelihood centered at the input
#' observed values and with standard deviation given as an input parameter 
#' (calibratable).
#' 
#' @param par A vector of values for the parameters to be calibrated, including
#' a subset of model parameters (described in \code{\link{runread_pmodel_f}}),
#' in order, and error terms
#' for each target variable (for example \code{'gpp_err'}), in the same order as
#' the targets appear in \code{targets}.
#' @param obs A nested data.frame of observations, with columns \code{'sitename'}
#' and \code{'data'} (see \code{\link{p_model_validation}} or \code{\link{p_model_validation_vcmax25}}
#' to check their structure). 
#' @param drivers A nested data.frame of driver data. See \code{\link{p_model_drivers}}
#' for a description of the data structure.
#' @param targets A character vector indicating the target variables for which the
#' optimization will be done and the RMSE computed. This string must be a column 
#' name of the \code{data} data.frame belonging to the validation nested data.frame 
#' (for example 'gpp').
#' @param par_fixed A named list of model parameter values to keep fixed during the
#' calibration. These should complement the input \code{par} such that all model
#' parameters are passed on to \code{\link{runread_pmodel_f}}.
#' @param parallel A logical specifying whether simulations are to be parallelised
#' (sending data from a certain number of sites to each core). Defaults to
#' \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel
#' computing. Defaults to 2.
#' 
#' @return The log-likelihood of the observed target values, assuming that they
#' are independent, normally distributed and centered on the predictions
#' made by the P-model run with standard deviation given as input (via `par` because
#' the error terms are estimated through the calibration with `BayesianTools`,
#' as shown in the "Parameter calibration and cost functions" vignette).
#' 
#' @details To run the P-model, all model parameters must be given. The cost
#' function uses arguments \code{par} and \code{par_fixed} such that, in the
#' calibration routine, \code{par} can be updated by the optimizer and 
#' \code{par_fixed} are kept unchanged throughout calibration.
#' 
#' If the validation data contains a "date" column (fluxes/states), the simulated target time series
#' is compared to the observed values on those same dates (e.g. for GPP). Otherwise, 
#' there should only be one observed value per site (leaf traits), and the outputs 
#' (averaged over the growing season, weighted by predicted GPP) will be 
#' compared to this single value representative of the site (e.g. Vcmax25). As an exception,
#' when the date of a trait measurement is available, it will be compared to the 
#' trait value predicted on that date.
#' 
#' @export
#' 
#' @examples
#' # Compute the likelihood for a set of
#' # model parameter values involved in the
#' # temperature dependence of kphio
#' # and example data
#' cost_likelihood_pmodel(        # reuse likelihood cost function
#'  par = list(
#'     kphio              = 0.05,
#'     kphio_par_a        = -0.01,
#'     kphio_par_b        = 1.0,
#'     err_gpp            = 2.0
#'  ),                          # must be a named list
#'  obs = p_model_validation,   # example data from package
#'  drivers = p_model_drivers,
#'  targets = c("gpp"),
#'  par_fixed = list(
#'   soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
#'   soilm_betao        = 0.0,
#'   beta_unitcostratio = 146.0,
#'   rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
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
  # NOTE(fabian): These different cost functions share a LOT of code in common. Consider consolidation for maintainability?

  # predefine variables for CRAN check compliance
  sitename <- data <- gpp_mod <- NULL
  
  ## define required parameter set based on model parameters
  required_param_names <- c(# P-model needs these parameters:
                  'beta_unitcostratio', 
                  'kc_jmax', 
                  'kphio', 
                  'kphio_par_a', 
                  'kphio_par_b',
                  'rd_to_vcmax', 
                  'soilm_betao', 'soilm_thetastar',
                  'tau_acclim')

  ## split calibrated parameters into model and error parameters
  par_calibrated_model      <- par[ ! names(par) %in% c("err_gpp", "err_vcmax25") ] # consider only model parameters for the check
  # par_calibrated_errormodel <- par[   names(par) %in% c("err_gpp", "err_vcmax25") ]
  # par_fixed
  
  ## check parameters
  if (!identical(sort(c(names(par_calibrated_model), names(par_fixed))), required_param_names)){
    stop(sprintf(paste0("Error: Input calibratable and fixed parameters do not ",
                        "match required model parameters:",
                        "\n         par:       c(%s)",
                        "\n         par_fixed: c(%s)",
                        "\n         required:  c(%s)"),
                 paste0(sort(names(par_calibrated_model)), collapse = ", "),
                 paste0(sort(names(par_fixed)), collapse = ", "),
                 paste0(sort(required_param_names), collapse = ", ")))
  }
  
  # Combine fixed and estimated params to result in all the params required to run the model
  # This basically uses all params except those of the error model of the observations
  params_modl <- c(par, par_fixed)[required_param_names]

  ## run the model
  df_full <- runread_pmodel_f(
    drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = parallel,
    ncores = ncores
  )
  
  # POSTPROCESS output:
    # possible P-model outputs:
    # df_full$data[[1]] |> tibble()                       # daily forest-specific output :    date, year_dec, properties/fluxes/states/...
    # possible BiomeE-model outputs:
    # df_full$data[[1]]$output_daily_tile |> tibble()     # daily forest-specific output :         year, doy, properties/fluxes/states/...
    # df_full$data[[1]]$output_annual_tile |> tibble()    # annual forest-specific output:         year,      properties/fluxes/states/...
    # df_full$data[[1]]$output_annual_cohorts |> tibble() # cohort-specific output       : cohort, year,      properties/fluxes/states/...
    
  ## clean model output and unnest
  df <- df_full |> tidyr::unnest('data') |>
    # gpp is used to get average trait prediction
    dplyr::select(sitename, 'date', 'gpp', targets) |> 
    dplyr::rename_with(~paste0(.x, '_mod'), 
                       .cols = -c(sitename, date))
  
  # PREPROCESS observation data:
  # separate validation data into sites containing
  # time series (fluxes/states) and sites containing constants (traits)
  obs_row_is_timeseries <- apply(obs, 1, function(x){ 'date' %in% colnames(x$data)})
  timeseries_sites <- obs[obs_row_is_timeseries, ][['sitename']] # individual sites can be part of both
  trait_sites      <- obs[!obs_row_is_timeseries, ][['sitename']] # individual sites can be part of both
                                                              # NOTE: to have an individual site in both:
                                                              # obs must contain a row where data contains a data.frame with column 'date'
                                                              #              and a row where data contains a data.frame without column 'date'
  if(sum(obs_row_is_timeseries) > 0){
    # Unnest timeseries observations for our targets
    obs_timeseries <- obs[obs_row_is_timeseries, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', 'date', targets))) |> 
      dplyr::rename_with(~paste0(.x, '_obs'), 
                         .cols = -c(sitename, date))
    
    if(ncol(obs_timeseries) < 3){
      warning("Dated observations (fluxes/states) are missing for the chosen targets.")
      df_timeseries <- data.frame()
    }else{
      # Join model output and timeseries observations
      df_timeseries <- df |>
        dplyr::filter(sitename %in% timeseries_sites) |>
        dplyr::left_join(
          obs_timeseries, 
          by = c('sitename', 'date')) # observations with missing date are ignored 
    }
  }else{
    df_timeseries <- data.frame()
  }
  
  if(sum(!obs_row_is_timeseries) > 0){
    # Unnest trait observations for our targets
    obs_trait <- obs[!obs_row_is_timeseries, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', targets))) |> 
      dplyr::rename_with(~paste0(.x, '_obs'), 
                         .cols = -c(sitename)) # -c(sitename, date)
    
    if(ncol(obs_trait) < 2){
      warning("Non-dated observations (traits) are missing for the chosen targets.")
      df_trait <- data.frame()
    }else{
      # Join model output and trait observations
      df_trait <- df |>
        dplyr::filter(sitename %in% trait_sites) |>
        dplyr::group_by(sitename) |>
        # get growing season average traits
        dplyr::summarise(across(ends_with("_mod") & !starts_with('gpp'),
                                ~ sum(.x * gpp_mod/sum(gpp_mod)),
                                .names = "{.col}")) |>
        dplyr::left_join(
          obs_trait,
          by = c('sitename')        # compare yearly averages rather than daily obs
        )
    }
  }else{
    df_trait <- data.frame()
  }
  
  # loop over targets to compute log-likelihood ll
  ll_df <- data.frame(target = targets, 
                      ll     = NaN) # initialize data.frame for ll's of the different target variables

  for (target in targets){
    target_obs <- paste0(target, '_obs')
    target_mod <- paste0(target, '_mod')
    
    # check (needed?):
    if(target_obs %in% colnames(df_timeseries) & target_obs %in% colnames(df_trait)) {stop(
      sprintf("Target '%s' cannot be simultaneously in df_timeseries and df_trait.", target))
    }
    
    # get observations and predicted target values, without NA 
    df_target <- if(target_obs %in% colnames(df_timeseries)){
      df_timeseries
    }else if(target_obs %in% colnames(df_trait)){
      df_trait
    }else{
      stop(sprintf("Target variable: '%s', was not found in the provided observations. Please check.", target))
    }
    df_target <- df_target[, c(target_mod, target_obs)] |> tidyr::drop_na()
    
    # calculate normal log-likelihood
    ll_df[ll_df$target == target, 'll'] <- 
      sum(stats::dnorm(
        x    = df_target[[target_mod]], # model
        mean = df_target[[target_obs]], # obs
        sd   = par[[paste0('err_', target)]],       # error model
        log  = TRUE))
  }
  ll <- sum(ll_df$ll)

  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}
  
  return(ll)
}

