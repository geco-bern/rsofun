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
#' @param target_weights A named vector of weights to be used in the computation of
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
                           # RMSE weighted? named vector
    parallel = FALSE,
    ncores = 2
){
  cost_rmse_generic(
    par        = par,
    obs        = obs,
    drivers    = drivers,
    targets    = targets,
    par_fixed  = par_fixed,
    target_weights = target_weights,
    parallel   = parallel,
    ncores     = ncores,
    curr_model = "p-model")
}

cost_rmse_generic <- function(
    par,   # model parameters & error terms for each target
    obs,
    drivers,
    targets,
    par_fixed = NULL, # non-calibrated model parameters
    target_weights = NULL, # if using several targets, how are the individual 
                           # RMSE weighted? named vector
    parallel = FALSE,
    ncores = 2,
    curr_model = c("biomee", "p-model")
){
  match.arg(curr_model, several.ok = FALSE)
  
  #### 1) Parse input parameters
  parameters <- llstep01_split_parameters(curr_model, par, par_fixed, drivers)
  # defines parameters$model and parameters$error
  # for use with the likelihood error model or for use with the simulation model
  
  #### 2) Update input args to runread_pmodel_f()/runread_biomee_f() with the provided parameters
  updated <- llstep02_update_model_args(
    curr_model, 
    parameters$model, 
    parameters$valid_names,
    drivers)
  
  #### 3) Run the model: runread_pmodel_f()/runread_biomee_f()
  if(curr_model == "biomee"){
    model_out_full <- runread_biomee_f(
      drivers   = updated$drivers,
      # par     = updated$par, # unused by BiomeE
      makecheck = TRUE,
      parallel  = parallel,
      ncores    = ncores
    )
  }else if(curr_model == "p-model"){
    model_out_full <- runread_pmodel_f(
      drivers   = updated$drivers,
      par       = updated$par,
      makecheck = TRUE,
      parallel  = parallel,
      ncores    = ncores
    )
  }else{
    stop("Arguments 'curr_model' must be either 'biomee' or 'p-model'")
  }
  
  #### 4) Combine modelled predictions and observed variables
  # drop spinup years if activated # TODO: (currently this removal is deactivated) can we get rid of this?
  spinup_years <- ifelse(updated$drivers$params_siml[[1]]$spinup,          
                         updated$drivers$params_siml[[1]]$spinupyears + 1, # TODO: why plus 1?
                         0)
  
  pred_obs_df <- llstep04_assemble_pred_vs_obs(curr_model, model_out_full, targets, obs, spinup_years)
  
  #### 5) Compute RMSE
  rmse <- llstep05_compute_RMSE(pred_obs_df    = pred_obs_df, 
                                targets        = targets, 
                                target_weights = target_weights)
  
  return(rmse)
}
llstep05_compute_RMSE <- function(pred_obs_df, targets, target_weights){
  # pred_obs_df is a data.frame containing columns for 'date', 'year_dec', 
  # 'sitename', 'targets_aggreg', 'period', 'target', 'targets_obs', 'targets_pred'

  # compute RMSE
  df_for_rmse <- pred_obs_df |>
    dplyr::filter(target %in% stringr::str_replace_all(targets, c("GPP"="gpp"))) |> 
    tidyr::drop_na('targets_obs', 'targets_pred') |> # remove NA in relevant columns  # TODO: do we need this drop_na?
    dplyr::group_by(.data$target) |>
    # dplyr::mutate(targets_se = (.data$targets_obs - .data$targets_pred)^2)
    dplyr::summarise(targets_rmse = sqrt(mean((.data$targets_obs - .data$targets_pred)^2, na.rm = TRUE)))
  
  # Aggregate RMSE over targets (weighted average)
                  #TODO  for developing: # target_weights <- c(gpp = 1.0)
  if(!is.null(target_weights)){
    cost <- sum(df_for_rmse$targets_rmse * target_weights[df_for_rmse$target])
  }else{
    cost <- mean(df_for_rmse$targets_rmse, na.rm = TRUE)
  }

  return(cost)
  # for debugging: return(df_for_ll) # for more granularity when debugging
}
