#' Cost function computing RMSE for calibration of different simulation setups 
#' 
#' The cost function performs a P-model run for the input drivers and parameter
#' values, and compares the output to observations of various targets by computing
#' the root mean squared error (RMSE).
#' 
#' @param par A vector of values for the parameters to be calibrated. These values 
#' can be updated by the optimizer within the calibration routine.
#' @param obs A nested data.frame of observations, with columns \code{'sitename'}
#' and \code{'data'} (see \code{p_model_validation} or \code{p_model_validation_vcmax25}
#' to check their structure). 
#' @param drivers A nested data.frame of driver data. See \code{\link{p_model_drivers}}
#' for a description of the data structure.
#' @param setup A character string (\code{'BRC'} or \code{'FULL'}) indicating which
#' parameters are calibrated. For \code{setup = 'BRC'} only the quantum yield
#' efficiency \code{kphio} is calibrated; for \code{setup = 'FULL'} it also includes
#' the soil moisture stress parameter \code{soilm_par_a}
#' for calibration. 
#' @param targets A character vector indicating the target variables for which the
#' optimization will be done and the RMSE computed. This string must be a column 
#' name of the \code{data} data.frame belonging to the validation nested data.frame 
#' (for example 'gpp').
#' @param par_fixed A vector of model parameter values to keep fixed during the
#' calibration. Only necessary if \code{setup = 'BRC'}.
#' @param target_weights A vector of weights to be used in the computation of
#' the RMSE if using several targets. By default (\code{target_weights = NULL})
#' the RMSE is computed separately for each target and then averaged. The provided
#' weights are used to compute a weighted average of RMSE across targets.
#' 
#' @return The root mean squared error (RMSE) between observed values and P-model
#' predictions. The RMSE is computed for each target separately and then aggregated
#' (mean or weighted average).
#' 
#' @details To run the P-model, all model parameters must be given. The cost
#' function uses arguments \code{par} and \code{par_fixed} such that, in the
#' calibration routine, \code{par} can be updated by the optimizer and 
#' \code{par_fixed} kept unchanged throughout calibration.
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

cost_rmse_pmodel <- function(
    par, # ordered vector
    obs, 
    drivers,
    setup,
    targets,    
    par_fixed = NULL, # only necessary if setup = BRC
    target_weights = NULL # if using several targets, how are the individual RMSE weighted? named vector
){
  
  # predefine variables for CRAN check compliance
  sitename <- data <- gpp_mod <- NULL
  
  ## define parameter set based on calibration setup
  if(setup == 'BRC'){
    if(is.null(par_fixed)){
      stop('Error: par_fixed = NULL. 
            Parameter value for "soilm_par_a" must be fixed.')
    }else{
      params_modl <- list(
        kphio           = par[1],
        soilm_par_a     = par_fixed['soilm_par_a']
      )
    }
  }else if(setup == 'FULL'){
    if(length(par) < 2){
      stop('Error: Input calibratable parameter values (par) missing, cannot run P-model')
    }else{
      params_modl <- list(
        kphio           = par[1],
        soilm_par_a     = par[2]
      )
    }
  }else{
    stop('Only calibration setups "BRC" and "FULL" are supported.')
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
    dplyr::summarise(
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
  rmse <- sapply(targets, function(target){
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
  })
  
  # Aggregate RMSE over targets (weighted average)
  if(!is.null(target_weights)){
    cost <- sum(rmse * target_weights)
  }else{
    cost <- mean(rmse, na.rm = TRUE)
  }

  return(cost)
}
