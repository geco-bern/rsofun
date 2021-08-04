# A set of cost functions to evaluate / calibrate
# the models with. You are free to create your own
# but this is an initial, most common, set.

#' Root mean squared error
#' 
#' Root mean squared error (RMSE) cost function on
#' the kphio parameter.
#' 
#' @param par parameters
#' @param ddf_obs observations
#' @param df_drivers driver data
#' @param inverse invert the function (1-value)
#' 
#' @importFrom magrittr '%>%'
#' 
#' @return the RMSE on the kpio parameter
#' @export

cost_rmse_kphio <- function(
  par,
  ddf_obs,
  df_drivers,
  inverse = FALSE
  ){
  
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
    df_drivers, 
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
  
  ddf_obs <- ddf_obs %>%
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data)
  
  # left join with observations
  df <- dplyr::left_join(df, ddf_obs, by = c("sitename", "date"))
  
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

cost_chisquared_kphio <- function( par, inverse = FALSE ){
  
  ## Full stack calibration
  out <- system(
    paste0("echo ",
           simsuite,
           " ",
           sprintf( "%f %f %f %f %f %f",
                    par[1], 1.0, 0.0, -9999.0, -9999.0, -9999.0 ),
           " | ./run", model ),
    intern = TRUE )  ## single calibration parameter
  
  # read output from calibration run
  out <- read_fwf( outfilnam, col_positions, col_types = cols( col_double() ) )
  
  ## Combine obs and mod by columns
  out <- bind_cols( obs, out )
  
  ## Calculate cost (chi-squared)
  cost <- ((out$gpp_mod - out$gpp_obs )/(out$gpp_unc))^2
  cost <- sum(cost, na.rm = TRUE)/sum(!is.na(cost))
  
  if (inverse) cost <- 1.0 / cost
  
  return(cost)
}

#' Root mean squared error
#' 
#' Root mean squared error (RMSE) cost function on
#' the full parameter stack.
#'
#' @param par parameters
#' @param ddf_obs observed values
#' @param df_drivers drivers
#' @param inverse invert the function
#'
#' @importFrom magrittr '%>%'
#'
#' @return the RMSE on the full parameter set
#' @export
#'

cost_rmse_fullstack <- function(
  par,
  ddf_obs,
  df_drivers,
  inverse = FALSE 
  ){
  
  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = par[1],
    soilm_par_a     = par[2],
    soilm_par_b     = par[3],
    vpdstress_par_a = 0.0,
    vpdstress_par_b = 0.0,
    vpdstress_par_m = 0
  )
  
  df <- runread_pmodel_f(
    df_drivers, 
    par = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
  ) %>%   
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data) %>% 
    dplyr::rename("gpp_mod" = "gpp") %>% 
    dplyr::left_join(ddf_obs,
                     by = c("sitename", "date"))
  
  ## Calculate cost (RMSE)
  cost <- sqrt( mean( (df$gpp_mod - df$gpp_obs )^2, na.rm = TRUE ) )
  
  if (inverse){
    cost <- 1.0 / cost
  }
  
  return(cost)
}

#' Root mean squared error
#' 
#' Root mean squared error (RMSE) cost function on
#' VPD stress.
#'
#' @param par parameters
#' @param ddf_obs observed values
#' @param df_drivers drivers
#' @param inverse invert the function
#'
#' @importFrom magrittr '%>%'
#' @return The RMSE on VPD
#' @export
#'

cost_rmse_vpdstress <- function(
  par,
  ddf_obs,
  df_drivers,
  inverse = FALSE
  ){
  
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
    df_drivers, 
    par = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
  ) %>%   
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data) %>% 
    dplyr::rename("latenth_mod" = "latenth") %>% 
    dplyr::left_join(ddf_obs, by = c("sitename", "date"))
  
  ## Calculate cost (RMSE)
  cost <- sqrt( mean( (df$latenth_mod - df$latenth_obs )^2, na.rm = TRUE ) )
  
  # print(paste("par =", paste(par, collapse = ", " ), "cost =", cost))
  
  if (inverse) cost <- 1.0 / cost  
  
  return(cost)
}

#' Chi-squared cost function
#' 
#' Cost function using the chi-squared statistic,
#' after (Keenan et al., 2012 GCB)
#'
#' @param par parameters
#' @param ddf_obs observed values
#' @param df_drivers drivers
#' @param inverse invert the function
#' 
#' @importFrom magrittr '%>%'
#' @return The Chi-squared value on VPD
#' @export
#'

cost_chisquared_vpdstress <- function(
  par,
  ddf_obs,
  df_drivers,
  inverse = FALSE
) {
  
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
    df_drivers, 
    par = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
  ) %>%   
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data) %>% 
    dplyr::rename("latenth_mod" = "latenth") %>% 
    dplyr::left_join(ddf_obs, by = c("sitename", "date"))
  
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
#'
#' @return A linearly scaled RMSE on all output
#' @export
#'

cost_linscale_rmse <- function( par ){
  
  ## Calculate cost (RMSE). 'modobs' is a global variable.
  cost <- sqrt(mean((par * modobs$gpp_mod - modobs$gpp_obs )^2,
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
  
  ## execute model for this parameter set
  outfilnam <- system(paste0("echo ", simsuite, " ",
                       sprintf( "%f", par[1] ),
                       " | ./run", model ), intern = TRUE )
  
  ## read output from calibration run
  out <- read_fwf(outfilnam, col_positions)
  
  ## Combine obs and mod by columns
  out <- bind_cols(obs, out)
  
  ## Calculate cost (RMSE)
  cost <- mean(abs(out$gpp_mod - out$gpp_obs), na.rm = TRUE)
  
  return(cost)
}

#' LM3PPA gs leuning cost function
#'
#' @param par parameters
#' @param ddf_obs observed values
#' @param df_drivers drivers
#' @param inverse invert the cost function metric
#'
#' @return value of a cost function, to minimize
#' @export

cost_rmse_lm3ppa_gsleuning <- function(
  par,
  ddf_obs,
  df_drivers,
  inverse = FALSE 
  ){
  
  # Add changed model parameters to df_drivers, overwriting where necessary.
  df_drivers$params_species[[1]]$phiRL[]      <- par[1]  # the same for all values
  df_drivers$params_species[[1]]$LAI_light[]  <- par[2]  # the same for all values
  df_drivers$params_tile[[1]]$tf_base         <- par[3]
  df_drivers$params_tile[[1]]$par_mort        <- par[4]

  ddf_obs <- ddf_obs$data[[1]]
  
  df <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  # Aggregate variables from the model df taking the last 500 yrs
  df_mod <- df$data[[1]]$output_annual_tile %>%
    tail(500) %>%
    dplyr::summarise(GPP = mean(GPP),
                     LAI= quantile(LAI, probs = 0.95, na.rm=T),
                     Density=mean(Density12),
                     Biomass=mean(plantC))

  dff <- data.frame(
    variables = c("GPP","LAI","Density","Biomass"),
    targets_mod = c(df_mod$GPP,
                    df_mod$LAI,
                    df_mod$Density,
                    df_mod$Biomass)
  ) %>%
    dplyr::left_join(ddf_obs, by = "variables") %>%
    mutate(error = targets_mod - targets_obs) %>%
    mutate(error_rel = error / targets_obs)
  
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
#' @param par a vector of parameter values, this is functions specific
#' @param ddf_obs observed data 
#' @param df_driver driver data
#' @param ... extra arguments to pass to the function
#' 
#' @importFrom magrittr '%>%'
#' @return the loglikelihood comparing observed and estimated values
#' @export

likelihood_lm3ppa <- function(
  par,
  ddf_obs,
  df_driver,
  ...
) {
  
  # Add changed model parameters to df_drivers, overwriting where necessary.
  df_drivers$params_species[[1]]$phiRL[]      <- par[1]
  df_drivers$params_species[[1]]$LAI_light[]  <- par[2]
  df_drivers$params_tile[[1]]$tf_base         <- par[3]
  df_drivers$params_tile[[1]]$par_mort        <- par[4]
  
  ddf_obs <- ddf_obs$data[[1]]
  
  df <- runread_lm3ppa_f(
    df_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  # Aggregate variables from the model df taking the last 500 yrs
  df_mod <- df$data[[1]]$output_annual_tile %>%
    tail(500) %>%
    dplyr::summarise(GPP = mean(GPP),
                     LAI= quantile(LAI, probs = 0.95, na.rm=T),
                     Density=mean(Density12),
                     Biomass=mean(plantC))
  
  dff <- data.frame(
    variables = c("GPP","LAI","Density","Biomass"),
    targets_mod = c(df_mod$GPP,
                    df_mod$LAI,
                    df_mod$Density,
                    df_mod$Biomass)
  ) %>%
    dplyr::left_join(ddf_obs, by = "variables") %>%
    mutate(error = targets_mod - targets_obs) %>%
    mutate(error_rel = error / targets_obs)
  
  # singlelikelihood
  singlelikelihoods <- stats::dnorm(
    dff$error_rel,
    sd = 1,
    log = TRUE)
  
  return(sum(singlelikelihoods))
}