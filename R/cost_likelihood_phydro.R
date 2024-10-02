#' Cost function computing a log-likelihood for calibration of Phydro-model
#' parameters
#' 
#' The cost function performs a Phydro-model run for the input drivers and model parameter
#' values, and computes the outcome's normal log-likelihood centered at the input
#' observed values and with standard deviation given as an input parameter 
#' (calibratable).
#' 
#' @param par A vector of values for the parameters to be calibrated, including
#' a subset of model parameters (described in \code{\link{runread_pmodel_f}}),
#' in order, and error terms
#' for each target variable (for example \code{'gpp_err'}), in the same order as
#' the targets appear in \code{targets}.
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
#' @param parallel A logical specifying whether simulations are to be parallelised
#' (sending data from a certain number of sites to each core). Defaults to
#' \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel
#' computing. Defaults to 2.
#' 
#' @return The log-likelihood of the observed target values, assuming that they
#' are independent, normally distributed and centered on the predictions
#' made by the P-model run with standard deviation given as input (via `par` because
#' the error terms are estimated through the calibration with `BayesianTools`,
#' as shown in the "Parameter calibration and cost functions" vignette).
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
#' # Compute the likelihood for a set of
#' # model parameter values involved in the
#' # temperature dependence of kphio
#' # and example data
#' library(dplyr)
#' cost_likelihood_phydromodel(        # reuse likelihood cost function
#'   par = list(
#'     kphio              = 0.0288,
#'     kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
#'     kphio_par_b        = 1.0,
#'     rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
#'     tau_acclim         = 30.0,
#'     kc_jmax            = 0.41,
#'     phydro_K_plant     = 5e-17,
#'     phydro_p50_plant   = -0.46,
#'     phydro_gamma       = 0.065,
#'     phydro_b_plant     = 1,
#'     phydro_alpha       = 0.08,
#'     bsoil              = 3,
#'     Ssoil              = 113,
#'     whc                = 253,
#'     # kphio              = 0.09423773, # setup ORG in Stocker et al. 2020 GMD
#'     # kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
#'     # kphio_par_b        = 1.0,
#'     err_gpp            = 0.9         # value from previous simulations
#'   ),                          # must be a named list
#'   obs     = p_model_validation,   # example data from package
#'   drivers = p_model_drivers_format2024_08 %>%
#'     ungroup() %>% dplyr::mutate(params_siml = purrr::map(params_siml, ~mutate(.x, use_phydro = TRUE, use_pml = TRUE, use_gs = TRUE))),
#'   targets = "gpp",
#'   par_fixed = list()
#' )
cost_likelihood_phydromodel <- function(
    par,   # model parameters & error terms for each target
    obs,
    drivers,
    targets,
    par_fixed = NULL,   # non-calibrated model parameters
    parallel = FALSE,
    ncores = 2
){
  # NOTE(fabian): These different cost functions share a LOT of code in common. Consider consolidation for maintainability?

  # predefine variables for CRAN check compliance
  sitename <- data <- gpp_mod <- NULL
  
  if (!("use_phydro" %in% colnames(drivers$params_siml[[1]]))){
    warning("Parameter use_phydro not set. Assuming FALSE")
    using_phydro = FALSE
  } else {
    using_phydro = drivers$params_siml[[1]]$use_phydro
  }

  ## define required parameter set based on model parameters
  if (!using_phydro){
    required_param_names <- rsofun:::required_param_names$p_model
  } else {
    required_param_names <- rsofun:::required_param_names$phydro_model
  }
  
  ## split calibrated parameters into model and error parameters
  par_calibrated_model      <- par[ ! names(par) %in% c("err_gpp") ] # consider only model parameters for the check
  # par_calibrated_errormodel <- par[   names(par) %in% c("err_gpp") ]
  # par_fixed
  
  ## check parameters
  if (!identical(sort(c(names(par_calibrated_model), names(par_fixed))), required_param_names)){
    stop(sprintf(paste0("Error: Input calibratable and fixed parameters do not ",
                        "match required model parameters:",
                        "\n         par:       c(%s)",
                        "\n         par_fixed: c(%s)",
                        "\n         required:  c(%s)"),
                 paste0(sort(names(par_calibrated_model)), collapse = ", "),
                 paste0(sort(names(par_fixed)), collapse = ", "),
                 paste0(sort(required_param_names), collapse = ", ")))
  }
  
  # Combine fixed and estimated params to result in all the params required to run the model
  # This basically uses all params except those of the error model of the observations
  params_modl <- c(par, par_fixed)[required_param_names]

  ## run the model
  df <- runread_pmodel_f(
    drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = parallel,
    ncores = ncores
  )
  
  ## Calculate dpsi intercept from outputs
  df <- df |> 
    mutate(dpsi_int = purrr::map(.x=data, .f=function(d){
      # Remove extremeties (potentially spurious values)
      d_filt = d |> 
        mutate(psi_soil = psi_leaf + dpsi) |> 
        filter(psi_soil > min(psi_soil)+0.01) |>
        filter(psi_soil < max(psi_soil)-0.01)
      
      # calculate psi_soil threshold for "wet" regime
      psi_soil_max <- d_filt |>
        with(quantile(psi_soil, probs = 0.95))
      
      # calculate dpsi intercept as mean of dpsi in wet regime
      int_q = d_filt |>
        filter(psi_soil >= psi_soil_max) |>
        pull(dpsi) |>
        mean()
      
      # calculate actual dpsi intercept by fitting lm (might not work)
      dat_lm = d_filt |> 
        dplyr::select(psi_leaf, psi_soil) |> 
        tidyr::drop_na()
      
      if (nrow(dat_lm) > 5){
        mod = dat_lm |>
          with(lm(psi_leaf~psi_soil))
        mods = summary(mod)
        int_reg = -mod$coefficients[1]
        p_slope = mods$coefficients[2,4]
      } else {
        int_reg = 0
        p_slope = 1
      }
      
      # if lm gives good fit, return actual intercept, else return wet-regime mean
      dpsi_int = ifelse(p_slope < 0.05, 
                        yes = int_reg, 
                        no = int_q)
      dpsi_int
    })) 
  
  ## clean model output and unnest
  df <- df |>
    dplyr::rowwise() |>
    dplyr::reframe(
      cbind(sitename, data[, c('date', unique(c('gpp', targets)))], dpsi_int) |>
        stats::setNames(c('sitename', 'date', paste0(unique(c('gpp', targets)), '_mod'), 'dpsi_int_mod'))
    ) # gpp is used to get average trait prediction
  
  # separate validation data into fluxes and traits, site by site
  is_flux <- apply(obs, 1, function(x){ 'date' %in% colnames(x$data)})
  
  if(sum(is_flux) > 0){
    flux_sites <- obs$sitename[is_flux]
    
    # Unnest flux observations for our targets
    obs_flux <- obs[is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', 'date', targets))) |>
      mutate(dpsi_int = 1)
    
    if(ncol(obs_flux) < 3){
      warning("Dated observations (fluxes) are missing for the chosen targets.")
      df_flux <- data.frame()
    }else{
      # Join P-model output and flux observations
      df_flux <- df |>
        dplyr::filter(sitename %in% flux_sites) |>
        dplyr::left_join(
          obs_flux, 
          by = c('sitename', 'date'))    # observations with missing date are ignored
    }
  }else{
    df_flux <- data.frame()
  }
  
  if(sum(!is_flux) > 0){
    trait_sites <- obs$sitename[!is_flux]
    
    # Unnest trait observations for our targets
    obs_trait <- obs[!is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', targets)))
    
    if(ncol(obs_trait) < 2){
      warning("Non-dated observations (traits) are missing for the chosen targets.")
      df_trait <- data.frame()
    }else{
      # Join output and trait observations
      df_trait <- df |>
        dplyr::filter(sitename %in% trait_sites) |>
        dplyr::group_by(sitename) |>
        # get growing season average traits
        dplyr::summarise(across(ends_with("_mod") & !starts_with('gpp'),
                                ~ sum(.x * gpp_mod/sum(gpp_mod)),
                                .names = "{.col}")) |>
        dplyr::left_join(
          obs_trait,
          by = c('sitename')        # compare yearly averages rather than daily obs
        )
    }
  }else{
    df_trait <- data.frame()
  }
  
  # loop over targets to compute log-likelihood ll
  ll_df <- data.frame(target = targets, 
                      ll     = NaN)
  for (target in targets){
    # check (needed?):
    if(target %in% colnames(df_flux) & target %in% colnames(df_trait)) {stop(
      sprintf("Target '%s' cannot be simultatneously in df_flux and df_trait.", target))
    }
    
    # get observations and predicted target values, without NA 
    df_target <- if(target %in% colnames(df_flux)){
      df_flux[, c(paste0(target, '_mod'), target)] |> tidyr::drop_na()
    }else{
      df_trait[, c(paste0(target, '_mod'), target)] |> tidyr::drop_na()
    }
    
    # calculate normal log-likelihood
    ll_df[ll_df$target == target, 'll'] <- 
      sum(stats::dnorm(
        x    = df_target[[paste0(target, '_mod')]], # model
        mean = df_target[[target]],                 # obs
        sd   = par[[paste0('err_', target)]],       # error model
        log  = TRUE))
  }
  ll <- sum(ll_df$ll)

  # compute ll for dpsi using a Gaussian prior with mean 1 and sd 0.33
  ll_dpsi = sum(stats::dnorm(
    x    = df_flux[['dpsi_int_mod']],   # model
    mean = df_flux[['dpsi_int']],       # obs
    sd   = 0.33,                        # error model
    log  = TRUE))
  
  ll <- ll + ll_dpsi 
  
  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}
  
  return(ll)
}

