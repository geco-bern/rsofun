#' Cost function computing a log-likelihood for calibration of P-model
#' parameters
#'
#' The cost function performs a P-model run for the input drivers and model parameter
#' values, and computes the outcome's normal log-likelihood centered at the input
#' observed values and with standard deviation given as an input parameter
#' (calibratable).
#'
#' @param par A named list for the parameters to be calibrated, including
#' a subset of model parameters and error terms for each target variable (for 
#' example \code{'gpp_err'}).
#' @param obs A nested data.frame of observations, with columns \code{'sitename'}
#' \code{'targets'}, and \code{'data'} (see \code{\link{pmodel_validation}} to 
#' check its structure). \code{'targets'} indicates the target variable(s) for 
#' which the likelihood is computed, it must be a column name of the 
#' \code{'data'} data.frame and it must have an error term in either \code{'par'} 
#' or \code{'par_fixed'}.
#' @param drivers A nested data.frame of driver data. See \code{\link{pmodel_drivers}}
#' for a description of the data structure.
#' @param par_fixed A named list of model parameter values to keep fixed during the
#' calibration. These should complement the input \code{par} such that all model
#' parameters are uniquely defined and passed on to \code{\link{runread_pmodel_f}}.
#' @param parallel A logical specifying whether simulations are to be parallelised
#' (sending data from a certain number of sites to each core). Defaults to
#' \code{FALSE}.
#' @param ncores An integer specifying the number of cores used for parallel
#' computing. Defaults to 2.
#' @param get_mod_obs Function running P-model and combining model outputs and 
#' observations (defaults to get_mod_obs_pmodel)
#'
#' @return The log-likelihood of the observed target values, assuming that they
#' are independent, normally distributed and centered on the predictions
#' made by the P-model run with standard deviation given as input \code{par} e.g.
#' \code{gpp_err}.
#'
#' @details To run the P-model, all model parameters must be given. The cost
#' function uses arguments \code{par} and \code{par_fixed} such that, in the
#' calibration routine, \code{par} can be updated by the optimizer and
#' \code{par_fixed} are kept unchanged throughout calibration.
#'
#' If the validation data contains a "date" column (fluxes), the simulated target time series
#' is compared to the observed values on those same dates (e.g. for GPP).
#'
#' @export
#'
#' @examples
#' # Compute the likelihood for a set of
#' # model parameter values involved in the
#' # temperature dependence of kphio
#' # and example data
#' cost_likelihood_pmodel(
#'  par = c(kphio       = 0.05,
#'            kphio_par_a = -0.01,
#'            kphio_par_b = 1,
#'            # error model parameters
#'            err_gpp     = 2),
#'  obs = pmodel_validation |> dplyr::filter(sitename == "FR-Pue"),
#'  drivers = pmodel_drivers |> dplyr::filter(sitename == "FR-Pue"),
#'  par_fixed = list(
#'   soilm_thetastar    = 0.6 * 240,  # old setup with soil moisture stress
#'   soilm_betao        = 0.0,
#'   beta_unitcostratio = 146.0,
#'   rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
#'   tau_acclim         = 30.0,
#'   kc_jmax            = 0.41
#'  )
#' )

cost_likelihood_pmodel <- function(
    par,   # model parameters & error terms for each target
    obs,
    drivers,
    par_fixed = NULL,   # non-calibrated model parameters
    parallel  = FALSE,
    ncores    = 1,
    get_mod_obs = get_mod_obs_pmodel # default argument needed as 
                                                    # workaround to make this 
                                                    # function easily available 
                                                    # on parallel workers
){
  
  stopifnot(nrow(obs) > 0)     # ensure some observation data are provided
  stopifnot(nrow(drivers) > 0) # ensure some driver data are provided
  
  requested_targets    <- unique(tidyr::unnest(obs, 'targets')$targets)
  # check that targets have an error term in \code{'par'}.
  provided_error_terms <- c(
    gsub("^err_","",grep("^err_", names(par), value = TRUE)),
    gsub("^err_","",grep("^err_", names(par_fixed), value = TRUE)))
  stopifnot(all(requested_targets %in% provided_error_terms) ||
              stop("No error term provided for requested target: ",
                   paste0(setdiff(requested_targets, provided_error_terms))))
  # check that targets are a column name of the \code{'data'} 
  # NOT CHECKED
  
  
  # # ensure backwards compatibility with format without column 'onestep':
  # if ("onestep" %in% names(drivers$params_siml[[1]])) {
  #   # all good
  # } else {
  #   warning("
  #     WARNING: Assuming daily P-model run requested. To clarify please add a 
  #     column 'onestep' with 'FALSE' or 'TRUE' to the 'params_siml' data.frame.
  #     in your driver.")
  #   drivers <- drivers |> mutate(
  #     params_siml = purrr::map(params_siml, ~mutate(.x, onestep = FALSE)))
  # }
  
  # A) Include current parameters ----
  stopifnot(length(intersect(names(par), names(par_fixed))) == 0) # no overlap
  params_modl_and_err <- c(par, par_fixed)
  
  # B,C) Run model and bring together with observed ----
  ## run the time series model for gpp/le/... time series
  ## run the onestep model for traits
  df_mod_obs <- get_mod_obs(drivers, obs, params_modl_and_err, parallel, ncores)
  
  # D) Compute likelihood ----
  # ll_normal            <- function(obs,mod,sd){stats::dnorm(            x=obs, mean = mod,                sd    = sd, log = TRUE)} # TODO: err_par_sd must be positive
  # ll_normalAdditiveBias<- function(obs,mod,sd,bias){stats::dnorm(       x=obs, mean = mod-bias,           sd    = sd, log = TRUE)} # TODO: err_par_sd must be positive, err_par_bias: if it is positive: the model has a positive bias
  ll_normalAdditScaled <- function(obs,mod,sd,bias,scale){stats::dnorm( x=obs, mean = mod*scale - bias,   sd    = sd, log = TRUE)} # TODO: err_par_sd must be positive, err_par_bias: if it is positive: the model has a positive bias
  # ll_lognormal         <- function(obs,mod,sd){stats::dlnorm(           x=obs, meanlog = mod,             sdlog = sd, log = TRUE)} # TODO: err_par_sd must be positive
  # ll_lognormal2        <- function(obs,mod,sd){stats::dlnorm(           x=obs, meanlog = log(mod) + sd^2, sdlog = sd, log = TRUE)}
  # ll_proportional      <- function(obs,mod,sd){stats::dnorm(            x=obs, mean = mod,                sd = abs(mod)*sd, log = TRUE)} # proportional: https://docs.pumas.ai/stable/model_components/error_models/
  # ll_userdefined       <- function(obs,mod,err_par1, err_par2, err_par3){}
  
  # compute ll
  df_ll <- df_mod_obs |> 
    group_by(.data$target, .data$err_par_sd, .data$err_par_bias, .data$err_par_scale) |>
    mutate(ll = ll_normalAdditScaled(
      .data$obs,.data$mod,.data$err_par_sd,.data$err_par_bias,.data$err_par_scale)
    ) #|>
    # select('sitename','target','mod','obs',
    #        'err_par_sd','err_par_bias','err_par_scale','ll')
  
  ll <- sum(df_ll$ll)
  
  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}
  
  return(ll)
}



#' Function running P-model and combining model and observations
#'
#' The get_mod_obs function performs a P-model run for the input drivers and 
#' model parameter values, and collects and combines the model output with the
#' observations.
#'
#' @param drivers A nested data.frame of driver data. See \code{\link{pmodel_drivers}}
#' for a description of the data structure.
#' @param obs A nested data.frame of observations, see \code{\link{pmodel_validation}} and
#' \code{cost_likelihood_pmodel} for details.
#' @param params_modl_and_err A named list for the model parameters and error 
#' terms for each target variable, see \code{cost_likelihood_pmodel} for details.
#' @param parallel A logical specifying whether simulations are to be parallelised
#' (sending data from a certain number of sites to each core).
#' @param ncores An integer specifying the number of cores used for parallel computing.
#' @param return_continuous_timeseries A logical specifying whether modelled values
#' are returned as daily, continuous time series (including days without observations)
#' or whether modelled values are only returned when a corresponding observation 
#' is available.
get_mod_obs_pmodel <- function(
    drivers,
    obs,
    params_modl_and_err,
    parallel,
    ncores,
    return_continuous_timeseries = FALSE
){
  
  # B) Run model ----
  df <- runread_pmodel_f(
    drivers   = drivers,
    par       = params_modl_and_err,
    makecheck = FALSE,
    parallel  = parallel,
    ncores    = ncores
  )
  df_daily   <- df |> rowwise() |> filter("date" %in% names(.data$data)) |> ungroup()
  df_onestep <- df |> rowwise() |> filter("vcmax_mod_molm2s" %in% names(.data$data)) |> ungroup()
  
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
    dplyr::filter(.data$sitename %in% df_daily$sitename) |> # this drops onestep rows for a potential full_join
    dplyr::select('sitename', 'targets', 'data') |>
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
    # nest again
    tidyr::nest(modobs = -c('sitename', 'targets'))
  
  df_mod_obs_onestep <- obs |>
    dplyr::filter(.data$sitename %in% df_onestep$sitename) |> # this drops daily rows
    select('sitename', 'targets', 'data') |>
    tidyr::unnest('data') |>
    # make this work gracefully in case nrow=0
    ensure_cols_defined(tibble(bigD13C = list(), vj = list())) |>
    # join the modelled data
    left_join(
      df_onestep |>
        tidyr::unnest('data') |>
        # make this work gracefully in case nrow=0
        ensure_cols_defined(tibble(vcmax_mod_molm2s = list(), 
                                   jmax_mod_molm2s = list(),
                                   bigD13C_mod_permil = list()
                                   )) |> # vj_mod__ = list())
        select('sitename', 'vcmax_mod_molm2s', 'jmax_mod_molm2s', 'bigD13C_mod_permil'), # , 'vj_mod__'
      by = dplyr::join_by('sitename')) |>
    # nest again
    tidyr::nest(modobs = -c('sitename', 'targets'))
  
  # combine into single data.frame
  targets        <- gsub("^err_","", grep("^err_", names(params_modl_and_err), value = TRUE))
  # targets_biases <- grep("^errbias", names(params_modl_and_err), value = TRUE)
  # targets_scales <- grep("^errscale", names(params_modl_and_err), value = TRUE)
  
  list_df_mod_obs <- lapply(targets, function(curr_target){
    if (curr_target %in% c("gpp","le")) {# hardcode certain variables to daily model output
      
      df_mod_obs_daily |> 
        # subset only rows that indicate this target:
        dplyr::rowwise() |> dplyr::filter(curr_target %in% .data$targets) |> dplyr::ungroup() |>
        tidyr::unnest('modobs') |>
        # make this work gracefully in case nrow=0
        ensure_cols_defined(tibble(date    = lubridate::Date(),
                                   le_mod  = numeric(), 
                                   le      = numeric(), 
                                   gpp_mod = numeric(), 
                                   gpp     = numeric())) |>
        # keep everything needed to compute loglikelihood:
        dplyr::mutate(target        = curr_target,
                      obs           = .data[[curr_target]],
                      mod           = .data[[paste0(curr_target,"_mod")]], # e.g. gpp_mod
                      err_par_sd    = params_modl_and_err[[paste0("err_",curr_target)]], #e.g. err_gpp
                      err_par_bias  = 0,
                      err_par_scale = 1) |>
        # ensure metadata can be bound together between different target types:
        tidyr::nest(obs_metadata = c('date')) |>
        # select needed columns
        dplyr::select('sitename', 'target', 'obs_metadata', 
                      'mod', 'obs', 
                      'err_par_sd', 'err_par_bias', 'err_par_scale')
        
    } else if (curr_target %in% c("bigD13C","vj")) {# hardcode certain variables to onestep model output
      
      df_mod_obs_onestep |>
        # subset only rows that indicate this target:
        dplyr::rowwise() |> dplyr::filter(curr_target %in% .data$targets) |> dplyr::ungroup() |>
        tidyr::unnest('modobs') |>
        # make this work gracefully in case nrow=0
        ensure_cols_defined(tibble(id                 = character(),
                                   bigD13C_mod_permil = numeric(),
                                   bigD13C            = numeric())) |>
        # fix naming
        dplyr::rename(all_of(c(bigD13C_mod = "bigD13C_mod_permil"))) |> # NOTE: this was bigD13C_mod_permil
        # keep everything needed to compute loglikelihood:
        dplyr::mutate(target        = curr_target,
                      obs           = .data[[curr_target]],
                      mod           = .data[[paste0(curr_target,"_mod")]], # e.g. bigD13C_mod
                      err_par_sd    = params_modl_and_err[[paste0("err_",curr_target)]], #e.g. err_bigD13C
                      err_par_bias  = 0,
                      err_par_scale = 1) |>
        # ensure metadata can be bound together between different target types:
        tidyr::nest(obs_metadata = c('id')) |> 
        # select needed columns
        dplyr::select('sitename', 'target', 'obs_metadata', 
                      'mod', 'obs', 
                      'err_par_sd', 'err_par_bias', 'err_par_scale')
    } else {
      stop(paste0("Target '", curr_target, "' is unsupported by get_mod_obs()."))
    }
  })
  df_mod_obs <- bind_rows(list_df_mod_obs)

  return(df_mod_obs)
}
