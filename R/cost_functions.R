# A set of cost functions to evaluate / calibrate
# the models with. You are free to create your own
# but this is an initial, most common, set.

#' Root mean squared error
#' 
#' Root mean squared error (RMSE) cost function on
#' the kphio (quantum yield efficiency) parameter.
#' 
#' @param par A list of model parameters, including at least \code{"kphio"}.
#' @param obs A data frame of observations.
#' @param drivers A data frame of driver data.
#' @param inverse A logical value indicating whether to invert the function (1-value).
#' \code{FALSE} by default.
#' 
#' @importFrom magrittr '%>%'
#' 
#' @return The RMSE of the simulated GPP by the P-model on the \code{kphio} parameter
#' versus the observed GPP.
#' 
#' @details A P-model run is performed for the value of \code{kphio} given as an
#' argument and the remaining calibratable parameters are held constant:
#' 
#' \code{soilm_par_a = 1.0, soilm_par_b = 0.0, tau_acclim_tempstress = 10 } and
#' \code{par_shape_tempstress = 0.0}.
#' 
#' @export

cost_rmse_kphio <- function(
  par,
  obs,
  drivers,
  inverse = FALSE
){
  
  # predefine variables for CRAN check compliance
  sitename <- data <- NULL
  
  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = par[[1]],
    soilm_par_a     = 1.0,
    soilm_par_b     = 0.0,
    tau_acclim_tempstress = 10,
    par_shape_tempstress  = 0.0
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
    tidyr::unnest(data) %>%
    dplyr::rename(
      'gpp_mod' = 'gpp'
    )
  
  obs <- obs %>%
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data)
  
  # left join with observations
  df <- dplyr::left_join(df, obs, by = c("sitename", "date"))
  
  # Calculate cost (RMSE)
  cost <- sqrt( mean( (df$gpp - df$gpp_mod )^2, na.rm = TRUE ) )
  
  if (inverse) cost <- 1.0 / cost
  
  return(cost)
}

#' Root mean squared error
#' 
#' Root mean squared error (RMSE) cost function on
#' the full parameter stack.
#'
#' @param par A list of model parameters including \code{"kphio", "soilm_par_a"}
#' and \code{"soilm_par_b"}.
#' @param obs A data frame of observed values.
#' @param drivers A data frame of drivers.
#' @param inverse A logical value indicating whether to invert the function. Defaults
#' to \code{FALSE}.
#'
#' @importFrom magrittr '%>%'
#'
#' @return The RMSE of the simulated GPP by the P-model on the given parameter
#' values versus the observed GPP.
#' 
#' @details The simulation is run for the given parameter values, but holding 
#' \code{tau_acclim_tempstress = 10 }and\code{par_shape_tempstress = 0.0} constant.
#' @export
#'

cost_rmse_fullstack <- function(
  par,
  obs,
  drivers,
  inverse = FALSE 
){
  
  # predefine variables for CRAN check compliance
  sitename <- data <- NULL
  
  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = par[1],
    soilm_par_a     = par[2],
    soilm_par_b     = par[3],
    tau_acclim_tempstress = 10,
    par_shape_tempstress  = 0.0
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
    rename(
      'gpp_obs' = 'gpp'
    )
  
  obs <- obs %>%
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data)
  
  # left join with observations
  df <- dplyr::left_join(df, obs, by = c("sitename", "date"))
  
  # Calculate cost (RMSE)
  cost <- sqrt( mean( (df$gpp - df$gpp_obs )^2, na.rm = TRUE ) )
  
  # print(paste("par =", paste(par, collapse = ", " ), "cost =", cost))
  
  if (inverse) cost <- 1.0 / cost
  
  return(cost)
}


#' Chi-squared cost function
#' 
#' Chi-squared cost function on kphio
#'
#' @param par parameters
#' @param inverse invert the function
#'
#' @return The chi-squared value on kpio
#' @export
#'

cost_chisquared_kphio <- function(par, inverse = FALSE) {
  
  stop("Cost function is not functional!")
  
  # out <- system(
  #   paste0("echo ",
  #          simsuite,
  #          " ",
  #          sprintf( "%f %f %f %f %f %f",
  #                   par[1], 1.0, 0.0, -9999.0, -9999.0, -9999.0 ),
  #          " | ./run", model ),
  #   intern = TRUE )
  # out <- utils::read_fwf(
  #   outfilnam,
  #   col_positions,
  #   col_types = cols(col_double())
  #   )
  
  # Combine obs and mod by columns
  # out <- dplyr::bind_cols(obs, out )
  # 
  # ## Calculate cost (chi-squared)
  # cost <- ((out$gpp_mod - out$gpp_obs )/(out$gpp_unc))^2
  # cost <- sum(cost, na.rm = TRUE)/sum(!is.na(cost))
  # 
  # if (inverse) cost <- 1.0 / cost
  # 
  # return(cost)
}



#' Root mean squared error
#' 
#' Root mean squared error (RMSE) cost function on
#' VPD stress.
#'
#' @param par parameters
#' @param obs observed values
#' @param drivers drivers
#' @param inverse invert the function
#'
#' @importFrom magrittr '%>%'
#' @return The RMSE on VPD
#' @export
#'

cost_rmse_vpdstress <- function(
  par,
  obs,
  drivers,
  inverse = FALSE
){
  
  # predefine variables for CRAN check compliance
  sitename <- data <- NULL
  
  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = 0.04971,
    soilm_par_a     = 1.0,
    soilm_par_b     = 0.0,
    vpdstress_par_a = par[1],
    vpdstress_par_b = par[2],
    vpdstress_par_m = par[3]
  )
  
  df <- runread_pmodel_f(
    drivers, 
    par = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
  ) %>%   
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data) %>% 
    dplyr::rename("latenth_mod" = "latenth") %>% 
    dplyr::left_join(obs, by = c("sitename", "date"))
  
  # Calculate cost (RMSE)
  cost <- sqrt( mean( (df$latenth_mod - df$latenth_obs )^2, na.rm = TRUE ) )
  
  if (inverse) cost <- 1.0 / cost  
  
  return(cost)
}

#' Chi-squared cost function
#' 
#' Cost function using the chi-squared statistic,
#' after (Keenan et al., 2012 GCB)
#'
#' @param par parameters
#' @param obs observed values
#' @param drivers drivers
#' @param inverse invert the function
#' 
#' @importFrom magrittr '%>%'
#' @return The Chi-squared value on VPD
#' @export
#'

cost_chisquared_vpdstress <- function(
  par,
  obs,
  drivers,
  inverse = FALSE
) {
  
  # predefine variables for CRAN check compliance
  sitename <- data <- NULL
  
  # execute model for this parameter set
  # For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = 0.04971,
    soilm_par_a     = 1.0,
    soilm_par_b     = 0.0,
    vpdstress_par_a = par[1],
    vpdstress_par_b = par[2],
    vpdstress_par_m = par[3]
  )
  
  df <- runread_pmodel_f(
    drivers, 
    par = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
  ) %>%   
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data) %>% 
    dplyr::rename("latenth_mod" = "latenth") %>% 
    dplyr::left_join(obs, by = c("sitename", "date"))
  
  ## Calculate cost (RMSE)
  cost <- ((df$latenth_mod - df$latenth_obs )/(2 * df$latenth_unc))^2
  cost <- sum(cost, na.rm = TRUE)/sum(!is.na(cost))
  
  # print(paste("par =", paste(par, collapse = ", " ), "cost =", cost))
  
  if (inverse) cost <- 1.0 / cost
  
  return(cost)
}

#' Linearly scaled output
#' 
#' Cost function of linearly scaled output
#'
#' @param par parameters
#' @param obs observations
#'
#' @return A linearly scaled RMSE on all output
#' @export
#'

cost_linscale_rmse <- function(par, obs){
  
  # Calculate cost (RMSE). 'modobs' is a global variable.
  cost <- sqrt(mean((par * obs$gpp_mod - obs$gpp_obs )^2,
                    na.rm = TRUE ))
  return(cost)
}

#' Mean absolute error (MAE)
#' 
#' The mean absolute error cost function
#' CHECK THIS FUNCTION
#'
#' @param par parameters
#' @param obs observations
#' @return The MAE on all output
#' @export
#'

cost_mae <- function(
  par,
  obs
){
  
  stop("Cost function is not functional!")
  
  # ## execute model for this parameter set
  # outfilnam <- system(paste0("echo ", simsuite, " ",
  #                            sprintf( "%f", par[1] ),
  #                            " | ./run", model ), intern = TRUE )
  # 
  # ## read output from calibration run
  # out <- read_fwf(outfilnam, col_positions)
  # 
  # ## Combine obs and mod by columns
  # out <- bind_cols(obs, out)
  # 
  # ## Calculate cost (RMSE)
  # cost <- mean(abs(out$gpp_mod - out$gpp_obs), na.rm = TRUE)
  # 
  # return(cost)
}

#' LM3PPA gs leuning cost function
#'
#' @param par parameters
#' @param obs observed values
#' @param drivers drivers
#' @param inverse invert the cost function metric
#'
#' @return value of a cost function, to minimize
#' @export

cost_rmse_lm3ppa_gsleuning <- function(
  par,
  obs,
  drivers,
  inverse = FALSE 
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
  
  df <- runread_lm3ppa_f(
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
    variables = c("GPP","LAI","Density","Biomass"),
    targets_mod = c(df_mod$GPP,
                    df_mod$LAI,
                    df_mod$Density,
                    df_mod$Biomass)
  ) %>%
    dplyr::left_join(obs, by = "variables") %>%
    dplyr::mutate(error = targets_mod - targets_obs) %>%
    dplyr::mutate(error_rel = error / targets_obs)
  
  ## Calculate cost (RMSE) across the N targets
  cost <- mean(dff$error_rel^2, na.rm = TRUE)
  cost <- 0.1
  
  if (inverse) cost <- 1.0 / cost
  return(cost)
}

#' Log likelihood cost function for model optimization
#'
#' The function is aimed to be maximized, to use it with optimizers which
#' minimize cost functions wrap the function as such:
#' `cost = function(...){abs(likelihood(...))}`
#'
#' @param par parameters
#' @param par_names vector with names for the parameters
#' @param obs observations
#' @param targets target observations to use in calibration
#' @param drivers driver data
#' 
#' @importFrom magrittr '%>%'
#' @return the loglikelihood comparing observed and estimated values
#' @export

likelihood_lm3ppa <- function(
  par,
  par_names,
  obs,
  targets,
  drivers
){
  
  # predefine variables for CRAN check compliance
  GPP <- LAI <- Density12 <- plantC <- error <- NULL
  
  # Add changed model parameters to drivers, overwriting where necessary.
  drivers$params_species[[1]]$phiRL[]  <- par[1]
  drivers$params_species[[1]]$LAI_light[]  <- par[2]
  drivers$params_tile[[1]]$tf_base <- par[3]
  drivers$params_tile[[1]]$par_mort <- par[4]

  # run model
  df <- runread_lm3ppa_f(
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
  logpost <- sapply(targets, function(i) {
    
    # select correct target variable
    # based on targets list
    predicted <- df %>%
      select(
        !!!i
      )
    
    observed <- obs %>%
      select(
        !!!i
      )
    
    # calculate likelihood
    # for all targets and their
    # error ranges
    ll <- likelihoodIidNormal(
      predicted,
      observed,
      par[grep(paste0('^err_', i,'$'), par_names)]
    )
    
  })
  
  # sum log likelihoods
  logpost <- sum(unlist(logpost))
  
  # trap boundary conditions
  if(is.nan(logpost) | is.na(logpost) | logpost == 0 ){
      logpost <- -Inf
    }
  
  return(logpost)
}

#' Likelihood function for the p-model
#' 
#' Calculates likelihoods for the p-model
#' Bayesiantools optimization
#' 
#' @param par parameters
#' @param par_names vector with names for the parameters
#' @param obs observations
#' @param targets target observations to use in calibration
#' @param drivers driver data
#' 
#' @importFrom magrittr '%>%'
#' 
#' @return the RMSE on the kpio parameter
#' @export

likelihood_pmodel <- function(
  par,
  par_names,
  obs,
  targets,
  drivers
){
  
  # predefine variables for CRAN check compliance
  sitename <- data <- NULL
  
  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = par[1],
    soilm_par_a     = par[2],
    soilm_par_b     = par[3],
    tau_acclim_tempstress = 10,
    par_shape_tempstress  = 0.0
  )
  
  # run the model
  df <- runread_pmodel_f(
    drivers, 
    par = params_modl,
    makecheck = TRUE,
    parallel = FALSE
  ) %>%
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
  
  # calculate the log likelihood
  logpost <- sapply(targets, function(i) {
    
    # select correct target variable
    # based on targets list
    predicted <- df %>%
      select(
        !!!i
      )
    
    observed <- obs %>%
      select(
        !!!i
      )
    
    uncertainty <- obs %>%
      select(
        !!!paste0(i,"_unc")
      )
    
    # calculate likelihood
    # for all targets and their
    # error ranges
    ll <- likelihood(
      predicted,
      observed,
      uncertainty
    )
    
  })
  
  logpost <- sum(unlist(logpost))
  
  # trap boundary conditions
  if(is.nan(logpost) | is.na(logpost) | logpost == 0 ){logpost <- -Inf}
  
  return(logpost)
}

likelihood <- function(predicted, observed, sd){
  # Helper function
  notNAvalues = !is.na(observed)
  return(
    sum(
      stats::dnorm(predicted[notNAvalues],
            mean = observed[notNAvalues], 
            sd = sd[notNAvalues],
            log = T)
    )
  )
}
