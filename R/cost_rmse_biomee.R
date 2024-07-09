#' RMSE cost function for BiomeE
#' 
#' Cost function for parameter calibration, which computes the root mean squared
#' error (RMSE) between BiomeE simulations (using the input set of parameters)
#' and observed target variables.
#' Cost function for parameter calibration, which
#' computes the RMSE for the biomee model fitting target variables 
#' \code{'GPP','LAI','Density'} and \code{'Biomass'} for a given set of parameters.
#' 
#' @param par A vector containing parameter values for \code{'phiRL',
#' 'LAI_light', 'tf_base', 'par_mort'} in that order.
#' @param obs A nested data frame of observations, following the structure of \code{biomee_validation},
#' for example.
#' @param drivers A nested data frame of driver data, for example \code{biomee_gs_leuning_drivers}.
#' 
#' @return The root mean squared error (RMSE) between the observed and simulated
#' values of \code{'GPP','LAI','Density'} and \code{'Biomass'} (all variables
#' have the same weight). Relative errors (difference divided by observed values) are used
#' instead of absolute errors.
#' The cost function performs a BiomeE model run for parameter values
#' \code{par} and model drivers \code{drivers} given as arguments, producing the
#' simulated values used to compute the RMSE.
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # Compute RMSE for a set of
#' # model parameter values
#' # and example data
#' cost_rmse_biomee(
#'  par = c(3.5, 3.5, 1, 1),
#'  obs = biomee_validation,
#'  drivers = biomee_gs_leuning_drivers
#' )
#' }

cost_rmse_biomee <- function(
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
  df_mod <- df$data[[1]]$output_annual_tile |>
    utils::tail(500) |>
    dplyr::select(
      GPP, LAI, Density12, plantC
    ) |>
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
    dplyr::left_join(obs, by = 'variables') |>
    dplyr::mutate(error = targets_mod - targets_obs) |>
    dplyr::mutate(error_rel = error / targets_obs)
  
  ## Calculate cost (RMSE) across the N targets
  cost <- mean(dff$error_rel^2, na.rm = TRUE)
  
  return(cost)
}