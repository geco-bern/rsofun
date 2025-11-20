#' Cost function computing a log-likelihood for calibration of P-model
#' parameters
#'
#' The cost function performs a P-model run for the input drivers and model parameter
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
#' and \code{'data'} (see \code{\link{p_model_oldformat_validation}} or \code{\link{p_model_oldformat_validation_vcmax25}}
#' to check their structure). TODO
#' @param drivers A nested data.frame of driver data. See \code{\link{p_model_oldformat_drivers}}
#' for a description of the data structure. TODO
#' 
#' TODO: remove: at-param targets A character vector indicating the target variables for which the
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
#' @param get_mod_obs TODO: document
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
#' cost_likelihood_pmodel(
#'  par = c(0.05, -0.01, 1,     # model parameters
#'          2),                # err_gpp
#'  obs = p_model_oldformat_validation,
#'  drivers = p_model_oldformat_drivers,
#'  targets = c('gpp'),
#'  par_fixed = list(
#'   soilm_thetastar    = 0.6 * 240,  # old setup with soil moisture stress
#'   soilm_betao        = 0.0,
#'   beta_unitcostratio = 146.0,
#'   rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
#'   tau_acclim         = 30.0,
#'   kc_jmax            = 0.41
#'  )
#' )

cost_likelihood_pmodel_bigD13C_vj_gpp <- function(
    par,   # model parameters & error terms for each target
    obs,
    drivers,
    par_fixed = NULL,   # non-calibrated model parameters
    parallel  = FALSE,
    ncores    = 1,
    get_mod_obs = get_mod_obs_pmodel_bigD13C_vj_gpp # default argument needed to make this function easily avaialable on parallel workers
){
  
  stopifnot(nrow(obs) > 0)     # ensure some observation data are provided
  stopifnot(nrow(drivers) > 0) # ensure some driver data are provided
  
  # A) Include current parameters ----
  stopifnot(length(intersect(names(par), names(par_fixed))) == 0) # no overlap
  params_modl_and_err <- c(par, par_fixed)
  
  
  # B,C) Run model and bring together with observed ----
  ## run the time series model for gpp/et/... time series
  ## run the onestep model for traits
  df_mod_obs <- get_mod_obs(drivers, obs, params_modl_and_err, parallel, ncores)
  
  # D) Compute likelihood ----
  # ll_normal            <- function(obs,mod,sd){stats::dnorm(            x=obs, mean = mod,                sd    = sd, log = TRUE)} # TODO: err_par_sd must be positive
  # ll_normalAdditiveBias<- function(obs,mod,sd,bias){stats::dnorm(       x=obs, mean = mod-bias,           sd    = sd, log = TRUE)} # TODO: err_par_sd must be positive, err_par_bias: if it is positive: the model has a positive bias
  ll_normalAdditScaled <- function(obs,mod,sd,bias,scale){stats::dnorm( x=obs, mean = mod*scale - bias,   sd    = sd, log = TRUE)} # TODO: err_par_sd must be positive, err_par_bias: if it is positive: the model has a positive bias
  # ll_lognormal         <- function(obs,mod,sd){stats::dlnorm(           x=obs, meanlog = mod,             sdlog = sd, log = TRUE)} # TODO: err_par_sd must be positive
  # ll_lognormal2        <- function(obs,mod,sd){stats::dlnorm(           x=obs, meanlog = log(mod) + sd^2, sdlog = sd, log = TRUE)}
  # ll_proportional      <- function(obs,mod,sd){stats::dnorm(            x=obs, mean = mod,                sd = abs(mod)*sd, log = TRUE)} # proportional: https://docs.pumas.ai/stable/model_components/error_models/
  # ll_userdefined     <- function(obs,mod,err_par1, err_par2, err_par3){}
  
  # compute ll
  df_ll <- df_mod_obs |> 
    group_by(.data$target, .data$err_par_sd, .data$err_par_bias, .data$err_par_scale) |>
    # compute loglikelihoods
    # # rowwise() |> # not needed and slowing things down
    # mutate(ll = case_when(
    #   target == "gpp"     ~ ll_normal(            obs,mod,err_par_sd),
    #   target == "bigD13C" ~ ll_normalAdditiveBias(obs,mod,err_par_sd),
    #   target == "vj"      ~ ll_normalAdditiveBias(obs,mod,err_par_sd)
    # )) |>
    # mutate(ll = ll_normalAdditiveBias(obs,mod,err_par_sd,err_par_bias)) |> 
    mutate(ll = ll_normalAdditScaled(
      .data$obs,.data$mod,.data$err_par_sd,.data$err_par_bias,.data$err_par_scale)
    ) |>
    select('sitename','run_model','target','mod','obs',
           'err_par_sd','err_par_bias','err_par_scale','ll')
  
  ll <- sum(df_ll$ll)
  
  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}
  
  return(ll)
}





get_mod_obs_pmodel_bigD13C_vj_gpp <- function(
    drivers,
    obs,
    params_modl_and_err,
    parallel,
    ncores,
    return_continuous_timeseries = FALSE # This gives daily values for time series for plotting purposes
                                         # Otherwise only values coinciding with observations are returned
){
  
  # NOTE: params_modl_and_err contains model and error parameters
  
  # B) Run model ----
  ## run the time series model for gpp/et/... time series
  df_daily <- drivers |>
    dplyr::filter(.data$run_model == "daily") |> # NOTE: this works gracefully even when no simulations are requested
    runread_pmodel_f(
      # drivers   = _, # NOTE: this is unneeded since `|>` automatically inserts first argument
      par       = params_modl_and_err,
      makecheck = FALSE,
      parallel  = parallel,
      ncores    = ncores
    )
  
  ## run the onestep model for traits
  df_onestep <- drivers |>
    # TODO: from here on unneeded computational overhead
    dplyr::filter(.data$run_model == "onestep") |>
    # dplyr::filter(FALSE) |>
    dplyr::group_by(.data$sitename) |>
    tidyr::unnest(c('params_siml', 'forcing'))
  # TODO: up until here: unneeded computational overhead
  
  # NOTE: this is to make this work gracefully even when no simulations are requested
  if (nrow(df_onestep) == 0){ # no onestep simulations requested, generate dummy output:
    df_onestep <- tibble(
      sitename                 = character(),
      vcmax_mod_molm2s         = numeric(),
      jmax_mod_molm2s          = numeric(),
      vcmax25_mod_molm2s       = numeric(),
      jmax25_mod_molm2s        = numeric(),
      gs_accl_mod_molCmolPhPa  = numeric(),
      wscal_mod__              = numeric(),
      bigD13C_mod_permil       = numeric(),
      iwue_mod__               = numeric(),
      rd_mod_gCm2s             = numeric(),
      vj_mod__                 = numeric())
  } else { # only run onestep simulations if requested, generate simulation output:
    df_onestep <- df_onestep |>
      dplyr::group_modify(~run_pmodel_onestep_f_bysite(
        lc4 = FALSE,
        # select what forcing columns to use:
        forcing =  data.frame(temp = .x$temp,
                              vpd  = .x$vpd,
                              ppfd = .x$ppfd,
                              co2  = .x$co2,
                              patm = .x$patm),
        params_modl = params_modl_and_err,
        makecheck   = FALSE)) |> # TODO: disable check
      dplyr::rename('vcmax_mod_molm2s'   = 'vcmax',
                    'jmax_mod_molm2s'    = 'jmax',
                    'vcmax25_mod_molm2s' = 'vcmax25',
                    'jmax25_mod_molm2s'  = 'jmax25',
                    'gs_accl_mod_molCmolPhPa' = 'gs_accl',
                    'bigD13C_mod_permil'      = 'bigdelta',
                    'iwue_mod__'         = 'iwue',
                    'rd_mod_gCm2s'       = 'rd') |>
      dplyr::mutate(vj_mod__ = .data$vcmax_mod_molm2s/.data$jmax_mod_molm2s)
  }
  
  # C) Bring together modelled and observed ----
  
  # NOTE: calibration targets can be controlled by providing no forcing and
  #       observation data for certain targets
  # NOTE: in that case some of the tibbles() will be empty (i.e. nrow() == 0)
  # NOTE: the unnesting operation does not work in case, since there is no
  #       nested element to infer the column names and column types
  # NOTE: below function makes unnest() work gracefully even in case nrow=0
  ensure_cols_defined <- function(df, expected_columns){
    if(nrow(df)==0){
      df |>
        # replace the unspecified column with expected columns
        select(!where(\(cl){class(cl)=="vctrs_unspecified"})) |>
        cross_join(expected_columns)
    } else {df}
  }
  
  
  join_fct <- ifelse(return_continuous_timeseries, 
                     dplyr::full_join, 
                     dplyr::left_join)
  df_mod_obs_daily <- obs |>
    dplyr::filter(.data$run_model == "daily") |>
    dplyr::select('sitename', 'run_model', 'targets', 'data') |>
    tidyr::unnest(c('data')) |>
    # make this work gracefully in case nrow=0
    ensure_cols_defined(tibble(date = as.Date(character()))) |>
    # join the modelled data
    join_fct( # this is a full_join in if (return_continuous_timeseries), otherwise a left_join
      df_daily |>
        tidyr::unnest('data') |>
        # make this work gracefully in case nrow=0
        ensure_cols_defined(tibble(date = as.Date(character()),gpp = numeric(),le = numeric())) |>
        select('sitename', 'date', gpp_mod = 'gpp', le_mod = 'le'),
      by = dplyr::join_by('sitename', 'date')) |>
    tidyr::fill("run_model") |>
    # nest again
    tidyr::nest(modobs = -c('sitename', 'run_model', 'targets'))
  
  df_mod_obs_onestep <- obs |>
    filter(.data$run_model == "onestep") |>
    select('sitename', 'run_model', 'targets', 'data') |>
    tidyr::unnest('data') |>
    # make this work gracefully in case nrow=0
    ensure_cols_defined(tibble(bigD13C = list(), vj = list())) |>
    # join the modelled data
    left_join(
      df_onestep |>
        select('sitename', 'vcmax_mod_molm2s', 'jmax_mod_molm2s', 'bigD13C_mod_permil', 'vj_mod__'),
      by = dplyr::join_by('sitename')) |>
    # nest again
    tidyr::nest(modobs = -c('sitename', 'run_model', 'targets'))
  
  # combine into single data.frame
  targets <- grep("^err_", names(params_modl_and_err), value = TRUE)
  targets_biases <- grep("^errbias", names(params_modl_and_err), value = TRUE)
  targets_scales <- grep("^errscale", names(params_modl_and_err), value = TRUE)
  
  # for (curr_target in targets){
  #   print(curr_target)
  # } # or alternativel lapply
  # or hardcode:
  df_mod_obs <- bind_rows(
    df_mod_obs_daily |> tidyr::unnest('modobs') |>
      # make this work gracefully in case nrow=0
      ensure_cols_defined(tibble(gpp_mod = numeric(), 
                                 gpp = numeric(), 
                                 date = lubridate::Date())) |>
      dplyr::rename(all_of(c(mod = "gpp_mod", obs = "gpp"))) |>
      dplyr::mutate(target  = "gpp",                                  #curr_target,
                    err_par_sd   = params_modl_and_err[["err_gpp"]],  #params_modl_and_err[[paste0("err_,"curr_target]]) |> # TODO: work here on  not hardcoding this...
                    err_par_bias = 0,
                    err_par_scale= params_modl_and_err[["errscale_gpp"]]) |>
      tidyr::nest(obs_metadata = c('date')) |>
      dplyr::select('sitename', 'run_model', 'target', 'obs_metadata', 'mod', 'obs', 
                    'err_par_sd', 'err_par_bias', 'err_par_scale'),
    
    df_mod_obs_onestep |>
      tidyr::unnest('modobs') |>
      # make this work gracefully in case nrow=0
      ensure_cols_defined(tibble(bigD13C = list(), bigD13C_mod_permil = numeric())) |>
      tidyr::unnest('bigD13C') |>
      # make this work gracefully in case nrow=0
      ensure_cols_defined(tibble(bigD13C_obs_permil = numeric(),
                                 species = character(), 
                                 year = integer())) |>
      dplyr::rename(all_of(c(mod = "bigD13C_mod_permil", obs = "bigD13C_obs_permil"))) |>
      dplyr::mutate(target  = "bigD13C",                                         #curr_target,
                    err_par_sd   = params_modl_and_err[["err_bigD13C"]],         #params_modl_and_err[[paste0("err_,"curr_target]]) |> # TODO: work here on  not hardcoding this...
                    err_par_bias = params_modl_and_err[["errbias_bigD13C"]],
                    err_par_scale= 1) |>
      tidyr::nest(obs_metadata = c('species', 'year')) |> # , Nobs, Nyears, Ndates
      dplyr::select('sitename', 'run_model', 'target', 'obs_metadata', 'mod', 'obs', 
                    'err_par_sd', 'err_par_bias', 'err_par_scale'),
    
    df_mod_obs_onestep |>
      tidyr::unnest('modobs') |>
      # make this work gracefully in case nrow=0
      ensure_cols_defined(tibble(vj = list(), vj_mod__ = numeric())) |>
      tidyr::unnest('vj') |>
      # make this work gracefully in case nrow=0
      ensure_cols_defined(tibble(vj_obs__ = numeric(),
                                 genus = character(), 
                                 species = character(), 
                                 year = integer())) |>
      dplyr::rename(all_of(c(mod = "vj_mod__", obs = "vj_obs__"))) |>
      dplyr::mutate(target  = "vj",                                         #curr_target,
                    err_par_sd   = params_modl_and_err[["err_vj"]],         #params_modl_and_err[[paste0("err_,"curr_target]]) |> # TODO: work here on  not hardcoding this...
                    err_par_bias = params_modl_and_err[["errbias_vj"]],
                    err_par_scale= 1) |>
      tidyr::nest(obs_metadata = c('genus', 'species', 'year')) |> # , Nobs, Nyears, Ndates
      dplyr::select('sitename', 'run_model', 'target', 'obs_metadata', 'mod', 'obs', 
                    'err_par_sd', 'err_par_bias', 'err_par_scale')
  )
  stopifnot(all(targets        %in% c("err_gpp", "err_bigD13C", "err_vj"))) # above hardcoded snippet is wrong if this is not the case
  stopifnot(all(targets_biases %in% c("errbias_bigD13C", "errbias_vj")))    # above hardcoded snippet is wrong if this is not the case
  stopifnot(all(targets_scales %in% c("errscale_gpp")))                     # above hardcoded snippet is wrong if this is not the case
  
  return(df_mod_obs)
}
