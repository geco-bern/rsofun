#' Creates a cost function for different simulation setups based on RMSE
#' 
#' Creates a cost function for parameter calibration, keeping non-calibrated
#' parameter values fixed and calibrating the parameters corresponding to setups
#' \code{BRC} and \code{FULL} from Stocker et al., 2020 GMD. The cost function
#' computes the root mean squared error (RMSE) on the calibrated parameters.
#' 
#' @param params_modl A list of model parameter values, including \code{'kphio',
#' 'soilm_par_a', 'soilm_par_b', 'tau_acclim_tempstress' }and \code{'par_shape_tempstress'}
#' in that order.
#' @param setup A character string (\code{'BRC'} or \code{'FULL'}) indicating which
#' parameters are calibrated. For \code{setup = 'BRC'} only the quantum yield
#' efficiency \code{kphio} is calibrated; for \code{setup = 'FULL'} it also includes
#' the soil moisture stress parameters \code{soilm_par_a} and \code{soilm_par_b}
#' for calibration.
#' @param method A character string indicating the optimization method that will
#' be used, either \code{'BayesianTools'} or \code{'GenSA'}.
#' 
#' 
#' @importFrom magrittr '%>%'
#' 
#' @return A cost function which computes the RMSE of the simulated GPP by the P-model 
#' versus the observed GPP. This cost function has as arguments a list of calibratable
#' model parameters \code{par}, a data frame of observations \code{obs}, and a
#' data frame of driver data \code{drivers}.
#' 
#' @details The resulting cost function performs a P-model run for the value of
#' \code{par} given as argument and the remaining non-calibratable parameters
#' are held constant (specified via \code{params_modl}).
#' 
#' Since the calibration routine in \code{BayesianTools} is based on maximizing 
#' a cost function and we want to minimize the RMSE, the opposite value, 
#' \code{(-1)*RMSE}, is returned if \code{method = 'BayesianTools'}. \code{GenSA}
#' minimizes the given objective function, so the plain RMSE is returned when
#' \code{method = 'GenSA'}.
#' 
#' @export
#' 
#' @examples \dontrun{
#' # Set model parameters
#' pars <- list(
#'   kphio          = 0.04,
#'   soilm_par_a    = 2.8,
#'   soilm_par_b    = 1.7,
#'   tau_acclim_tempstress  = 7.3,
#'   par_shape_tempstress   = 0.1
#'   )
#' 
#' # Write cost function
#' cost_rmse_kphio <- create_cost_rmse_pmodel(
#'   params_modl = pars,
#'   setup = 'BRC',
#'   method = 'BayesianTools'
#'   )
#' }

create_cost_rmse_pmodel <- function(
    params_modl,
    setup,
    method){
  # predefine variables for CRAN check compliance
  f <- NULL
  
  f <- "function(
    par,
    obs,
    drivers
){
  
  # predefine variables for CRAN check compliance
  sitename <- data <- NULL
  
  ## execute model for this parameter set
  params_modl <- list(
    kphio           = par[1],
    soilm_par_a     = "
  
  if(setup == "BRC"){
    f <- paste0(f,
                params_modl[2],
                ",
    soilm_par_b     = ",
                params_modl[3]
    )
  } else if(setup == "FULL"){
    f <- paste0(f,
                "par[2],
    soilm_par_b     = par[3]")
  } else {
    stop("unvalid setup, must be 'BRC' or 'FULL'")
    }
  
  f <- paste0(f,
        ",
    tau_acclim_tempstress = ",
        params_modl[4],
        ",
    par_shape_tempstress  = ",
        params_modl[5],
        "
  )
  
  # run the model
  df <- runread_pmodel_f(
    drivers, 
    par = params_modl,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  # cleanup
  df <- df %>%
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data) %>%
    dplyr::rename(
      'gpp_mod' = 'gpp'
    )
  # output[output$sitename=='FR-Pue',]$data[[1]][[1]] # alternative base R option
  
  obs <- obs %>%
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data)
  
  # left join with observations
  df <- dplyr::left_join(df, obs, by = c('sitename', 'date'))
  
  # Calculate cost (RMSE)
  cost <- sqrt( mean( (df$gpp - df$gpp_mod )^2, na.rm = TRUE ) )
        
  "
  )
  
  if(tolower(method) == 'bayesiantools'){
    f <- paste0(f,
                "return(-cost)
                }")
  }else if(tolower(method) == 'gensa'){
    f <- paste0(f,
                "return(cost)
}")
  }
  
  return(eval(parse(text = f)))
}

#' Creates a log-likelihood cost function for several setups
#' 
#' Creates a cost function for parameter calibration, keeping non-calibrated
#' parameter values fixed and calibrating the parameters corresponding to setups
#' \code{BRC} and \code{FULL} from Stocker et al., 2020 GMD. The cost function
#' computes the log-likelihood for the p-model fitting a given target variable 
#' for a given set of parameters.
#' 
#' @param params_modl A list of model parameter values, including \code{'kphio',
#' 'soilm_par_a', 'soilm_par_b', 'tau_acclim_tempstress' }and \code{'par_shape_tempstress'}
#' in that order.
#' @param setup A character string (\code{'BRC'} or \code{'FULL'}) indicating which
#' parameters are calibrated. For \code{setup = 'BRC'} only the quantum yield
#' efficiency \code{kphio} is calibrated; for \code{setup = 'FULL'} it also includes
#' the soil moisture stress parameters \code{soilm_par_a} and \code{soilm_par_b}
#' for calibration.
#' @param target A character string indicating the target variable for which the
#' optimization will be done. This string must be a column name of the \code{data}
#' data.frame belonging to the validation nested data.frame, as well as its
#' corresponding uncertainty (for example 'gpp' and 'gpp_unc').
#' 
#' @importFrom magrittr '%>%'
#' 
#' @return A cost function which computes the log-likelihood of the simulated 
#' GPP by the P-model versus the observed GPP. This cost function has as arguments 
#' a list of calibratable model parameters \code{par}, a data frame of observations 
#' \code{obs}, and a data frame of driver data \code{drivers}.
#' 
#' @details The resulting cost function performs a P-model run for the value of
#' \code{par} given as argument and the remaining non-calibratable parameters
#' are held constant (specified via \code{params_modl}).
#' 
#' The likelihood is calculated assuming that the predicted targets are independent
#' normaly distributed and centered on the observations. The optimization should be run 
#' using \code{BayesianTools}, and the likelihood is maximized.
#' 
#' @export
#'
#' @examples \dontrun{
#' # Set model parameters
#' pars <- list(
#'   kphio          = 0.04,
#'   soilm_par_a    = 2.8,
#'   soilm_par_b    = 1.7,
#'   tau_acclim_tempstress  = 7.3,
#'   par_shape_tempstress   = 0.1
#'   )
#' 
#' # Write cost function
#' cost_likelihood_kphio <- create_cost_likelihood_pmodel(
#'   params_modl = pars,
#'   setup = 'BRC',
#'   target = 'gpp'
#'   )
#' }

create_cost_likelihood_pmodel <- function(
    params_modl,
    setup,
    target
){
  # predefine variables for CRAN check compliance
  f <- NULL
  
  f <- "function(
    par,
    obs,
    drivers
){
  # predefine variables for CRAN check compliance
  sitename <- data <- NULL
  
  ## execute model for this parameter set
  params_modl <- list(
    kphio           = par[1],
    soilm_par_a     = "
  
  if(setup == 'BRC'){
    f <- paste0(f,
                params_modl[2],
                ",
    soilm_par_b     = ",
                params_modl[3]
    )
  } else if(setup == 'FULL'){
    f <- paste0(f,
                "par[2],
    soilm_par_b     = par[3]")
  } else {
    stop("unvalid setup, must be 'BRC' or 'FULL'")
  }
  
  f <- paste0(f,
              ",
    tau_acclim_tempstress = ",
              params_modl[4],
              ",
    par_shape_tempstress = ",
              params_modl[5],
              ")
              
   # run the model
  df <- runread_pmodel_f(
    drivers, 
    par = params_modl,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  # cleanup
  df <- df %>%
    dplyr::select(sitename, data) %>%
    tidyr::unnest(data) %>%
    dplyr::arrange(
      sitename, date
    )
    
  obs <- obs %>%
    dplyr::select(sitename, data) %>%
    tidyr::unnest(data) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      sitename, date
    )          
  
  # get observations and predicted target values, without NA            
  observed <- obs$",
              target,
              "
  notNAvalues <- !is.na(observed)
  observed <- observed[notNAvalues]
  
  predicted <- df$",
              target,
              "[notNAvalues]
  uncertainty <- obs$",
              target,
              "_unc[notNAvalues]

  # calculate normal log-likelihood
  ll <- sum(stats::dnorm(
    predicted,
    mean = observed,
    sd = uncertainty,
    log = TRUE
  ))
  
  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}
  
  return(ll)
}")
  return(eval(parse(text = f)))
}

#' Creates a log-likelihood cost function for biomee with different targets
#' 
#' Creates a cost function for parameter calibration, which
#' computes the log-likelihood for the biomee model fitting several target 
#' variables for a given set of parameters.
#' 
#' @param targets A character vector indicating the target variables for which the
#' optimization will be done. This should be a subset of \code{c("GPP", "LAI",
#' "Density", "Biomass")}.
#' 
#' @importFrom magrittr '%>%'
#' 
#' @return A cost function which computes the log-likelihood of the simulated 
#' targets by the biomee model versus the observed targets. This cost function has 
#' as arguments a vector of calibratable model parameters \code{par}, a data frame of
#' observations \code{obs}, and a data frame of driver data \code{drivers}.
#' 
#' @details The resulting cost function performs a biomee model run for the value of
#' \code{par} given as argument. \code{par} must always contain \code{'phiRL',
#' 'LAI_light', 'tf_base', 'par_mort'} and the error terms corresponding to the
#' target variables, e.g. \code{'err_GPP'} if GPP is a target. Make sure that
#' the order of the error terms in \code{par} coincides with the order of the targets
#' used to create the cost function.
#' 
#' The likelihood is calculated assuming that the 
#' predicted targets are independent, normaly distributed and centered on the observations. 
#' All targets have the same weight in the optimization objective. The optimization 
#' should be run using \code{BayesianTools}, and the likelihood is maximized.
#' 
#' @export

create_cost_likelihood_biomee <- function(
    targets
){
  # predefine variables for CRAN check compliance
  f <- n <- NULL
  f <- "function(
  par,
  obs,
  drivers
){
  
  # predefine variables for CRAN check compliance
  GPP <- LAI <- Density12 <- plantC <- error <- ll <- NULL
  
  # Add changed model parameters to drivers, overwriting where necessary.
  drivers$params_species[[1]]$phiRL[]  <- par[1]
  drivers$params_species[[1]]$LAI_light[]  <- par[2]
  drivers$params_tile[[1]]$tf_base <- par[3]
  drivers$params_tile[[1]]$par_mort <- par[4]

  # run model
  df <- runread_biomee_f(
    drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  # did we spin up
  spin_up <- drivers$params_siml[[1]]$spinup
  
  # drop spinup years if activated
  # see below
  if (spin_up){
    spin_up_years <- drivers$params_siml[[1]]$spinupyears + 1
  } else {
    spin_up_years <- 0
  }
  
  # Aggregate variables from the model df taking the last 500 yrs
  # if spun up
  df <- df$data[[1]]$output_annual_tile %>%
    utils::tail(500 - spin_up_years) %>%
    dplyr::summarise(
      GPP = mean(GPP),
      LAI = stats::quantile(LAI, probs = 0.95, na.rm=T),
      Density = mean(Density12),
      Biomass = mean(plantC)
    )
  
  # reshuffle observed data
  col_names <- obs$data[[1]]$variables
  obs <- data.frame(t(obs$data[[1]]$targets_obs))
  colnames(obs) <- col_names
  
  # calculate the log likelihood
  ll <- 0"
  
  n <- length(targets)
  for(i in 1:n){
    f <- paste0(f,
                "
  ll <- ll + BayesianTools::likelihoodIidNormal(
    predicted = df$", targets[i],
                ",
    observed = obs$", targets[i],
                ",
    sd = par[", 4+i, "])")
  }
  
  f <- paste0(f,
              "
              
  # trap boundary conditions
  if(is.nan(ll) || is.na(ll) | ll == 0){
    ll <- -Inf
  }
  
  return(ll)
}")
  return(eval(parse(text = f)))
}

#' Creates a cost function for different simulation setups based on RMSE
#' 
#' Creates a cost function for parameter calibration, keeping non-calibrated
#' parameter values fixed and calibrating the parameters corresponding to various
#' setups. The cost function computes the root mean squared error (RMSE) on the 
#' calibrated parameters.
#' 
#' @param params_modl A list of model parameter values, including \code{'phiRL',
#' 'LAI_light', 'tf_base' }and \code{'par_mort'} in that order.
#' @param setup A character string (for now, only \code{'FULL'}) indicating which
#' parameters are calibrated.
#' @param method A character string indicating the optimization method that will
#' be used, either \code{'BayesianTools'} or \code{'GenSA'}.
#' 
#' 
#' @importFrom magrittr '%>%'
#' 
#' @return A cost function which computes the RMSE of the simulated targets by the biomee model 
#' versus the observed target variables. This cost function has as arguments a list of calibratable
#' model parameters \code{par}, a data frame of observations \code{obs}, and a
#' data frame of driver data \code{drivers}.
#' 
#' @details The resulting cost function performs a biomee model run for the value of
#' \code{par} given as argument and the remaining non-calibratable parameters
#' are held constant (specified via \code{params_modl}).
#' 
#' Since the calibration routine in \code{BayesianTools} is based on maximizing 
#' a cost function and we want to minimize the RMSE, the opposite value, 
#' \code{(-1)*RMSE}, is returned if \code{method = 'BayesianTools'}. \code{GenSA}
#' minimizes the given objective function, so the plain RMSE is returned when
#' \code{method = 'GenSA'}.
#' 
#' @export
#' 
#' @examples \dontrun{
#' # Set model parameters
#' pars <- list(
#'   phiRL      = 0.04,
#'   LAI_light  = 2.8,
#'   tf_base    = 1.7,
#'   par_mort   = 7.3
#'   )
#' 
#' # Write cost function
#' cost_rmse <- create_cost_rmse_biomee(
#'   params_modl = pars,
#'   setup = 'FULL',
#'   method = 'BayesianTools'
#'   )
#' }

create_cost_rmse_biomee <- function(
    params_modl,
    setup,
    method){
  # predefine variables for CRAN check compliance
  f <- NULL
  
  f <- "function(
    par,
    obs,
    drivers
){
  
  # predefine variables for CRAN check compliance
  GPP <- LAI <- Density12 <- plantC <- targets_obs <-
    targets_mod <- error <- targets_obs <- NULL
  
  # Add changed model parameters to drivers, overwriting where necessary.
  drivers$params_species[[1]]$phiRL[]      <- par[1]
  drivers$params_species[[1]]$LAI_light[]  <- par[2]
  drivers$params_tile[[1]]$tf_base         <- par[3]
  drivers$params_tile[[1]]$par_mort        <- par[4]
  obs <- obs$data[[1]]
  
  df <- runread_biomee_f(
    drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  # Aggregate variables from the model df taking the last 500 yrs
  df_mod <- df$data[[1]]$output_annual_tile %>%
    utils::tail(500) %>%
    dplyr::select(
      GPP, LAI, Density12, plantC
    ) %>%
    dplyr::summarise(
      GPP = mean(GPP, na.rm = TRUE),
      LAI = stats::quantile(LAI, probs = 0.95, na.rm=TRUE),
      Density = mean(Density12, na.rm=TRUE),
      Biomass = mean(plantC, na.rm=TRUE)
    )
  
  dff <- data.frame(
    variables = c('GPP','LAI','Density','Biomass'),
    targets_mod = c(df_mod$GPP,
                    df_mod$LAI,
                    df_mod$Density,
                    df_mod$Biomass)
  ) %>%
    dplyr::left_join(obs, by = 'variables') %>%
    dplyr::mutate(error = targets_mod - targets_obs) %>%
    dplyr::mutate(error_rel = error / targets_obs)
  
  ## Calculate cost (RMSE) across the N targets
  cost <- mean(dff$error_rel^2, na.rm = TRUE)
  
  "
  if(tolower(method) == 'bayesiantools'){
    f <- paste0(f, "return(-cost)
}")
  }else if(tolower(method) == 'gensa'){
    f <- paste0(f, "return(cost)
}")
  }
  return(eval(parse(text = f)))
}