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
#'   \item{\code{par}}{A list of model parameters. For each parameter, an initial value 
#'   and lower and upper bounds should be provided. The calibratable parameters
#'   are 'kphio', 'soilm_par_a', 'soilm_par_b', 'tau_acclim_tempstress' and 'par_shape_tempstress'.}
#'   \item{\code{method}}{A string indicating the optimization method, either \code{'GenSA'}
#'   or \code{'BayesianTools'}.}
#'   \item{\code{metric}}{A cost function. See the 'Cost functions for parameter
#'   calibration' vignette for examples.}
#'   \item{\code{control}}{A list of arguments passed on to the optimization function.
#'   If \code{method = 'GenSA'}, see \link[GenSA]{GenSA}. If \code{method = 'BayesianTools'}
#'   the list should include at least \code{settings} and \code{sampler}, see
#'   \link[BayesianTools:runMCMC]{BayesianTools::runMCMC}.}
#'   \item{\code{targets}}{Name of the observed target variable to use in calibration
#'   (necessary if \code{method = 'BayesianTools'}).}
#'  }
#'  @param optim_out A logical indicating whether the function returns the raw
#'  output of the optimization functions (defaults to TRUE).
#'  
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
  settings,
  optim_out = TRUE
){
  # predefine variables for CRAN check compliance
  cost <- lower <- upper <- pars <- out <- out_optim <- priors <- setup <- 
    bt_par <- bt_settings <- NULL
  
  # check input variables
  if(missing(obs) | missing(drivers) | missing(settings)){
    stop("missing input data, please check all parameters")
  }
  
  if (nrow(obs) == 0){
    warning("no validation data available, returning NA parameters")
    
    settings$par$kphio <- NA
    settings$par$soilm_par_a <- NA
    settings$par$soilm_par_b <- NA
    settings$par$tau_acclim_tempstress <- NA
    settings$par$par_shape_tempstress <- NA
    
    return(settings$par)
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
      drivers = drivers
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
    cost <- eval(settings$metric)
    
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
