#' Calibrates SOFUN model parameters
#'
#' This is the main function that handles the 
#' calibration of SOFUN model parameters. 
#' 
#' @param drivers asdf
#' @param obs A data frame containing observational data used for model
#'  calibration. Created by function \code{get_obs_calib2()}
#' @param settings A list containing model calibration settings. 
#'  See vignette_rsofun.pdf for more information and examples.
#'
#' @return A complemented named list containing 
#'  the calibration settings and optimised parameter values.
#' @export
#' @importFrom magrittr %>%
#' @import GenSA BayesianTools
#' 
#' @examples 
#' \dontrun{ 
#' calib_sofun(
#'   drivers = filter(drivers,
#'           sitename %in% settings$sitenames),
#'   obs = obs_calib,
#'   settings = settings)
#' }
 
calib_sofun <- function(
  drivers,
  obs,
  settings
  ){

  # check input variables
  if(missing(obs) | missing(drivers) | missing(settings)){
    stop("missing input data, please check all parameters")
  }
  
  if (nrow(obs) == 0){
    warning("no validation data available, returning NA parameters")
    
    settings$par$kphio          <- NA
    settings$par$soilm_par_a <- NA
    settings$par$soilm_par_b    <- NA
    settings$par$tau_acclim_tempstress    <- NA
    settings$par$par_shape_tempstress    <- NA
    
    return(settings$par)
  }
    
  #--- GenSA ----
  if (tolower(settings$method) == "gensa"){
    
    # convert to standard cost function naming
    cost <- eval(settings$metric)
    
    # create bounds
    lower <- unlist(lapply(settings$par, function(x) x$lower))
    upper <- unlist(lapply(settings$par, function(x) x$upper))
    pars <- unlist(lapply( settings$par, function(x) x$init))
    
    out_optim <- GenSA::GenSA(
      par   = pars,
      fn    = cost,
      lower = lower,
      upper = upper,
      control = settings$control,
      ddf_obs = obs,
      df_drivers = drivers
    )
  } 
  
  #--- Bayesiantools ----
  if (tolower(settings$method) == "bayesiantools"){
    
    # convert to standard cost function naming
    cost <- eval(settings$metric)
    
    # create bounds
    lower <- unlist(lapply(settings$par, function(x) x$lower))
    upper <- unlist(lapply(settings$par, function(x) x$upper))
    
    # setup the bayes run, no message forwarding is provided
    # so wrap the function in a do.call
    setup <- BayesianTools::createBayesianSetup(
      likelihood = function(random_par){
        do.call("cost",
                list(
                  par = random_par,
                  ddf_obs = obs,
                  df_drivers = drivers,
                  inverse = TRUE
                ))},
      lower = lower,
      upper = upper
    )
    
    # set bt control parameters
    bt_settings <- settings$control$settings
    
    # calculate the runs
    out <- BayesianTools::runMCMC(bayesianSetup = setup,
                                  sampler = settings$control$sampler,
                                  settings = bt_settings)
    
    # drop last value
    bt_par <- BayesianTools::MAP(out)$parametersMAP
    bt_par <- bt_par[1:(length(bt_par))]
    out_optim <- list(par = bt_par)
    names(out_optim$par) <- names(settings$par)
  }

  return(out_optim)
}
