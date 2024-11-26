#' Cost function computing RMSE for calibration of P-model parameters
#' 
#' The cost function performs a P-model run for the input drivers and parameter
#' values, and compares the output to observations of various targets by computing
#' the root mean squared error (RMSE).
#' 
#' @param par A vector of values for the parameters to be calibrated (a subset of
#' those described in \code{\link{runread_pmodel_f}}, in order).
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
#' @param target_weights A vector of weights to be used in the computation of
#' the RMSE if using several targets. By default (\code{target_weights = NULL})
#' the RMSE is computed separately for each target and then averaged. The provided
#' weights are used to compute a weighted average of RMSE across targets.
#' @param parallel A logical specifying whether simulations are to be parallelised
#' (sending data from a certain number of sites to each core). Defaults to
#' \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel
#' computing. Defaults to 2.
#' 
#' @return The root mean squared error (RMSE) between observed values and P-model
#' predictions. The RMSE is computed for each target separately and then aggregated
#' (mean or weighted average).
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
#' # Compute RMSE for a set
#' # of model parameter values
#' # and example data
#' cost_rmse_pmodel(
#'  par = c(0.05, -0.01, 0.5),  # kphio related parameters
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

cost_rmse_pmodel <- function(
    par,  # ordered vector of model parameters
    obs, 
    drivers,
    targets,    
    par_fixed = NULL, # non-calibrated model parameters
    target_weights = NULL, # if using several targets, how are the individual 
    #                      # RMSE weighted? named vector
    parallel = FALSE,
    ncores = 2
){
  
  # predefine variables for CRAN check compliance
  sitename <- data <- gpp_mod <- NULL
  
  ## check input parameters
  if( (length(par) + length(par_fixed)) != 9 ){
    stop('Error: Input calibratable and fixed parameters (par and par_fixed)
    do not match length of the required P-model parameters.')
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
    params_modl <- as.list(par)       # all parameters calibrated
    names(params_modl) <- calib_param_names
  }
  
  # run the model
  df <- runread_pmodel_f(
    drivers, 
    par = params_modl,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  # clean model output and unnest
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
  
  # Calculate cost (RMSE) per target
  rmse <- lapply(targets, function(target){
    if(target %in% colnames(df_flux)){
      error <- (df_flux[[target]] - df_flux[[paste0(target, '_mod')]])^2
    }else{
      error <- c()
    }
    if(target %in% colnames(df_trait)){
      error <- c(error, 
                 (df_trait[[target]] - df_trait[[paste0(target, '_mod')]])^2)
    }
    sqrt(mean(error, na.rm = TRUE))
  }) |>
    unlist()
  
  # Aggregate RMSE over targets (weighted average)
  if(!is.null(target_weights)){
    cost <- sum(rmse * target_weights)
  }else{
    cost <- mean(rmse, na.rm = TRUE)
  }
  
  return(cost)
}
