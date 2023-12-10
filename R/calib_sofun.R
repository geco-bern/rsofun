#' Calibrates SOFUN model parameters
#'
#' This is the main function that handles the 
#' calibration of SOFUN model parameters. 
#' 
#' @param drivers A data frame with driver data. See \code{\link{p_model_drivers}}
#' for a description of the data structure.
#' @param obs A data frame containing observational data used for model
#'  calibration. See \code{\link{p_model_validation}} for a description of the data
#'  structure.
#' @param settings A list containing model calibration settings. 
#'  See the 'P-model usage' vignette for more information and examples.
#'  \describe{
#'   \item{\code{method}}{A string indicating the optimization method, either \code{'GenSA'}
#'   or \code{'BayesianTools'}.}
#'   \item{\code{par}}{A list of model parameters. For each parameter, an initial value 
#'   and lower and upper bounds should be provided. The calibratable parameters
#'   include model parameters 'kphio', 'kphio_par_a', 'kphio_par_b', 'soilm_thetastar',
#'   'soilm_betao', 'beta_costunitratio', 'rd_to_vcmax', 'tau_acclim', 'kc_jmax'
#'   and 'rootzone_whc' , and (if
#'   doing Bayesian calibration) error parameters
#'   for each target variable, named for example 'err_gpp'. This list must match
#'   the input parameters of the calibration metric and the parameters should be
#'   given in the order above.}
#'   \item{\code{metric}}{A cost function. See the 'Cost functions for parameter
#'   calibration' vignette for examples.}
#'   \item{\code{control}}{A list of arguments passed on to the optimization function.
#'   If \code{method = 'GenSA'}, see \link[GenSA]{GenSA}. If \code{method = 'BayesianTools'}
#'   the list should include at least \code{settings} and \code{sampler}, see
#'   \link[BayesianTools:runMCMC]{BayesianTools::runMCMC}.}
#'  }
#' @param optim_out A logical indicating whether the function returns the raw
#'  output of the optimization functions (defaults to TRUE).
#' @param ... Optional arguments passed on to the cost function specified as
#'  \code{settings$metric}. 
#'  . 
#' @return A named list containing the calibrated parameter vector `par` and
#' the output object from the optimization `mod`. For more details on this
#' output and how to evaluate it, see \link[BayesianTools:runMCMC]{runMCMC} (also
#' \href{https://florianhartig.github.io/BayesianTools/articles/BayesianTools.html}{this post})
#' and \link[GenSA]{GenSA}.
#' @export
#' @importFrom magrittr %>%
#' @import GenSA BayesianTools
#' 
#' @examples
#' # Fix model parameters that won't be calibrated
#' params_fix <- list(
#'   kphio_par_a        = 0,
#'   kphio_par_b        = 1.0,
#'   soilm_thetastar    = 0.6*240,
#'   soilm_betao        = 0.01,
#'   beta_unitcostratio = 146,
#'   rd_to_vcmax        = 0.014,
#'   tau_acclim         = 30,
#'   kc_jmax            = 0.41
#' )
#' 
#' # Define calibration settings
#' settings <- list(
#'   method = "BayesianTools",
#'   par = list(
#'     kphio = list(lower=0.04, upper=0.09, init=0.05),
#'     err_gpp = list(lower = 0.01, upper = 4, init = 2)
#'   ),
#'   metric = rsofun::cost_likelihood_pmodel,
#'   control = list(
#'     sampler = "DEzs",
#'     settings = list(
#'       nrChains = 1,
#'       burnin = 0,        
#'       iterations = 50     # kept artificially low
#'     )
#'   )
#'  )
#'  
#'  # Run the calibration for GPP data
#'  calib_output <- rsofun::calib_sofun(
#'    drivers = rsofun::p_model_drivers,
#'    obs = rsofun::p_model_validation,
#'    settings = settings,
#'    # extra arguments for the cost function
#'    par_fixed = params_fix,
#'    targets = c("gpp")
#'  )

calib_sofun <- function(
    drivers,
    obs,
    settings,
    optim_out = TRUE,
    ...
){
  # predefine variables for CRAN check compliance
  cost <- lower <- upper <- pars <- out <- out_optim <- priors <- setup <- 
    bt_par <- bt_settings <- NULL
  
  # check input variables
  if(missing(obs) | missing(drivers) | missing(settings)){
    stop("missing input arguments, please check all parameters")
  }
  
  # check data structure
  if(is.data.frame(obs)){
    if (nrow(obs) == 0){
      warning("no validation data available, returning NA parameters")
      return(lapply(settings$par,
                    function(x) NA))
    }
  }else{
    stop("obs must be a (nested) data.frame")
  }
  
  #--- GenSA ----
  if (tolower(settings$method) == "gensa"){
    
    # convert to standard cost function naming
    cost <- settings$metric
    
    # create bounds
    lower <- unlist(lapply(settings$par, function(x) x$lower))
    upper <- unlist(lapply(settings$par, function(x) x$upper))
    pars <- unlist(lapply( settings$par, function(x) x$init))
    
    out <- GenSA::GenSA(
      par   = pars,
      fn    = cost,
      lower = lower,
      upper = upper,
      control = settings$control,
      obs = obs,
      drivers = drivers,
      ...
    )
    if(optim_out){
      out_optim <- list(par = out$par, mod = out)
    }else{
      out_optim <- list(par = out$par)
    }
    
  } 
  
  #--- Bayesiantools ----
  if (tolower(settings$method) == "bayesiantools"){
    
    # convert to standard cost function naming
    # cost <- eval(settings$metric)
    cost <- function(par, obs, drivers){
      eval(settings$metric)(par = par,
                            obs = obs,
                            drivers = drivers,
                            ...)
    }
    
    # reformat parameters
    pars <- as.data.frame(do.call("rbind", settings$par), row.names = FALSE)
    
    priors  <- BayesianTools::createUniformPrior(
      unlist(pars$lower),
      unlist(pars$upper),
      unlist(pars$init)
    )
    
    # setup the bayes run, no message forwarding is provided
    # so wrap the function in a do.call
    setup <- BayesianTools::createBayesianSetup(
      likelihood = function(
    random_par) {
        # cost(
        #   par = random_par,
        #   obs = obs,
        #   drivers = drivers,
        #   ...
        # )
        do.call("cost",
                list(
                  par = random_par,
                  obs = obs,
                  drivers = drivers
                ))
      },
    prior = priors,
    names = names(settings$par)
    )    
    
    # set bt control parameters
    bt_settings <- settings$control$settings
    
    # calculate the runs
    out <- BayesianTools::runMCMC(
      bayesianSetup = setup,
      sampler = settings$control$sampler,
      settings = bt_settings
    )
    
    # drop last value
    bt_par <- BayesianTools::MAP(out)$parametersMAP
    bt_par <- bt_par[1:(length(bt_par))]
    if(optim_out){
      out_optim <- list(par = bt_par, mod = out)
    }else{
      out_optim <- list(par = bt_par)
    }
    
    names(out_optim$par) <- names(settings$par)
  }
  
  return(out_optim)
}
