#' Update model parameters
#'
#' Updates rsofun model parameters with the output of \link{calib_rsofun}.
#'
#' @param params_modl A named list specifying rsofun model parameters
#' @param settings_calib An object returned by \link{calib_rsofun}.
#'
#' @return A named list specifying rsofun model parameters, containing elements of \code{params_modl} with updated 
#' values based on \code{settings_calib}
#' @export
#' 
update_params <- function(params_modl, settings_calib){
  
  ## overwrite model parameters with parameter values from calibration output 
  for (ipar in settings_calib$par_opt %>% names()){
    params_modl[[ipar]] <- settings_calib$par_opt[[ipar]]
  }
  
  return(params_modl)
}