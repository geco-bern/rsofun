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
#' If the validation data contains a "date" column (fluxes), the simulated target time series
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
#' cost_likelihood_pmodel(
#'  par = c(0.05, -0.01, 1,     # model parameters
#'          2),                # err_gpp
#'  obs = p_model_validation,
#'  drivers = p_model_drivers,
#'  targets = c('gpp'),
#'  par_fixed = list(
#'   soilm_thetastar    = 0.6 * 240,  # old setup with soil moisture stress
#'   soilm_betao        = 0.0,
#'   beta_unitcostratio = 146.0,
#'   rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
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
  # predefine variables for CRAN check compliance
  sitename <- data <- gpp_mod <- NULL
  
  ## check input parameters
  if( (length(par) + length(par_fixed)) != (9 + length(targets)) ){
    stop('Error: Input calibratable and fixed parameters (par and par_fixed)
    do not match length of the required P-model parameters and target error terms.')
  }
  
  ## define parameter set based on calibrated parameters
  calib_param_names <- c('kphio', 'kphio_par_a', 'kphio_par_b',
                         'soilm_thetastar', 'soilm_betao',
                         'beta_unitcostratio', 'rd_to_vcmax', 
                         'tau_acclim', 'kc_jmax')
  
  if(!is.null(par_fixed)){
    params_modl <- list()
    # complete with calibrated values
    i <- 1 # start counter
    for(par_name in calib_param_names){
      if(is.null(par_fixed[[par_name]])){
        params_modl[[par_name]] <- par[i]   # use calibrated par value
        i <- i + 1                          # counter of calibrated params
      }else{
        params_modl[[par_name]] <- par_fixed[[par_name]]  # use fixed par value
      }
    }
  }else{
    params_modl <- as.list(par[1:9])       # all parameters calibrated
    names(params_modl) <- calib_param_names
  }
  
  ## run the model
  df <- runread_pmodel_f(
    drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = parallel,
    ncores = ncores
  )
  
  ## clean model output and unnest
  df <- df |>
    dplyr::rowwise() |>
    dplyr::reframe(
      cbind(sitename, data[, c('date', unique(c('gpp', targets)))]) |>
        stats::setNames(c('sitename', 'date', paste0(unique(c('gpp', targets)), '_mod')))
    ) # gpp is used to get average trait prediction
  
  # separate validation data into fluxes and traits, site by site
  is_flux <- apply(obs, 1, function(x){ 'date' %in% colnames(x$data)})
  
  if(sum(is_flux) > 0){
    flux_sites <- obs$sitename[is_flux]
    
    # Unnest flux observations for our targets
    obs_flux <- obs[is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', 'date', targets)))
    
    if(ncol(obs_flux) < 3){
      warning("Dated observations (fluxes) are missing for the chosen targets.")
      df_flux <- data.frame()
    }else{
      # Join P-model output and flux observations
      df_flux <- df |>
        dplyr::filter(sitename %in% flux_sites) |>
        dplyr::left_join(
          obs_flux, 
          by = c('sitename', 'date'))    # observations with missing date are ignored
    }
  }else{
    df_flux <- data.frame()
  }
  
  if(sum(!is_flux) > 0){
    trait_sites <- obs$sitename[!is_flux]
    
    # Unnest trait observations for our targets
    obs_trait <- obs[!is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', targets)))
    
    if(ncol(obs_trait) < 2){
      warning("Non-dated observations (traits) are missing for the chosen targets.")
      df_trait <- data.frame()
    }else{
      # Join output and trait observations
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
  
  # loop over targets
  ll <- lapply(seq(length(targets)), function(i){
    target <- targets[i]
    # get observations and predicted target values, without NA 
    if(target %in% colnames(df_flux)){
      df_target <- df_flux[, c(paste0(target, '_mod'), target)] |>
        tidyr::drop_na()
    }else{
      df_target <- data.frame()
    }
    if(target %in% colnames(df_trait)){
      df_target <- rbind(df_target,
                         df_trait[, c(paste0(target, '_mod'), target)] |>
                           tidyr::drop_na())
    }
    
    # calculate normal log-likelihood
    ll <- sum(stats::dnorm(
      df_target[[paste0(target, '_mod')]],
      mean = df_target[[target]],
      sd = par[length(par)-length(targets) + i],
      log = TRUE
    ))
  }) |>
    unlist() |>
    sum()

  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}
  
  return(ll)
}

