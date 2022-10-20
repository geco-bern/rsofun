#' Calibrates SOFUN model parameters
#'
#' This is the main function that handles the 
#' calibration of SOFUN model parameters. 
#' 
#' @param drivers A data frame with driver data. See \code{p_model_drivers} for 
#' a description of the data structure.
#' @param obs A data frame containing observational data used for model
#'  calibration. See \code{p_model_validation} for a description of the data
#'  structure.
#' @param settings A list containing model calibration settings. 
#'  See [vignettes/pmodel_use.Rmd] for more information and examples.
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
    
    out_optim <- GenSA::GenSA(
      par   = pars,
      fn    = cost,
      lower = lower,
      upper = upper,
      control = settings$control,
      obs = obs,
      drivers = drivers
    )
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
    
    ## Hack: determine whether cost function is a bayesian-style likelihood function or a "traditional" cost function
    ## traditional cost functions as implemented here have an argument 'inverse'. Use that to determine and if so
    ## set it to TRUE to use the cost function.
    ## solution with 'header' is based on https://stackoverflow.com/questions/30125590/how-can-a-function-return-its-name-and-arguments-in-r 
    ## this may break if argument 'inverse' is abandoned and a better solution should be found eventually.
    header <- function(x){
      UseMethod('header', x)
    }
    header.function <-function(x){
      y<-list(args(x))
      x<-as.character(substitute(x))
      print(sprintf('%s=%s',x,y))
    }
    if (grepl("inverse" , header(cost))){

      # setup the bayes run, no message forwarding is provided
      # so wrap the function in a do.call
      setup <- BayesianTools::createBayesianSetup(
        likelihood = function(
          random_par,
          par_names = names(settings$par)) {
                do.call("cost",
                        list(
                          par = random_par,
                          obs = obs,
                          drivers = drivers,
                          # This is a hack. BayesianTools expects a likelihood function to be calculated here, 
                          # but we're just calculating the RMSE and return its inverse (we want the RMSE 
                          # minimised, its inverse maximised - imitating likelihood maximisation)
                          inverse = TRUE     
                        ))
              },
          prior = priors,
          names = names(settings$par)
        )    
      
    } else {

      # setup the bayes run, no message forwarding is provided
      # so wrap the function in a do.call
      setup <- BayesianTools::createBayesianSetup(
        likelihood = function(
          random_par,
          par_names = names(settings$par)) {
                do.call("cost",
                        list(
                          par = random_par,
                          par_names = par_names,
                          obs = obs,
                          targets = settings$targets,
                          drivers = drivers
                        ))
              },
          prior = priors,
          names = names(settings$par)
        )   
    }
    
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
    out_optim <- list(par = bt_par)
    names(out_optim$par) <- names(settings$par)
  }
  
  return(out_optim)
}
