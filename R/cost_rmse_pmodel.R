#' Cost function computing RMSE for calibration of P-model parameters
#'
#' The cost function performs a P-model run for the input drivers and parameter
#' values, and compares the output to observations of various targets by computing
#' the root mean squared error (RMSE).
#'
#' @param par A vector of values for the parameters to be calibrated (a subset of
#' those described in \code{\link{runread_pmodel_f}}, in order).
#' @param obs A nested data.frame of observations, with columns \code{'sitename'}
#' and \code{'data'} (see \code{\link{pmodel_validation}} to check its structure).
#' @param drivers A nested data.frame of driver data. See \code{\link{pmodel_drivers}}
#' for a description of the data structure.
#' @param targets A named character vector indicating the target variable(s) for
#' which the RMSE is computed. 
#' The names of \code{'target'} indicate target variables, while the values indicate the source data set to be used.
#' The values must be present in the \code{'obs$source'} column.
#' The names must be column name(s) of the \code{'obs$data'} data.frame.
#' @param par_fixed A named list of model parameter values to keep fixed during the
#' calibration. These should complement the input \code{par} such that all model
#' parameters are passed on to \code{\link{runread_pmodel_f}}.
#' @param target_weights A vector of weights to be used in the computation of
#' the RMSE if using several targets. By default (\code{target_weights = NULL})
#' the RMSE is computed separately for each target and then averaged. The provided
#' weights are used to compute a weighted average of RMSE across targets.
#'
#' @return The root mean squared error (RMSE) between observed values and P-model
#' predictions. The RMSE is computed for each target separately and then aggregated
#' (mean or weighted average).
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
#' # Compute RMSE for a set
#' # of model parameter values
#' # and example data
#' cost_rmse_pmodel(
#'   par = c(0.05, -0.01, 0.5),  # kphio related parameters
#'   obs = pmodel_validation |> dplyr::filter(sitename == "FR-Pue"),
#'   drivers = pmodel_drivers |> dplyr::filter(sitename == "FR-Pue"),
#'   targets = c("gpp" = "fluxnet"),
#'   par_fixed = list(
#'     soilm_thetastar    = 0.6 * 240,  # old setup with soil moisture stress
#'     soilm_betao        = 0.0,
#'     beta_unitcostratio = 146.0,
#'     rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
#'     tau_acclim         = 30.0,
#'     kc_jmax            = 0.41
#'   )
#' )
#'
cost_rmse_pmodel <- function(
    par,  # ordered vector of model parameters
    obs,
    drivers,
    targets,
    par_fixed = NULL, # non-calibrated model parameters
    target_weights = NULL # if using several targets, how are the individual
    #                      # RMSE weighted? named vector
    ) {
  # predefine variables for CRAN check compliance
  sitename <- data <- gpp_mod <- NULL

  # ensure backwards compatibility with format without column 'onestep':
  if ("onestep" %in% names(drivers$params_siml[[1]])) {
    # all good
  } else {
    warning("
      WARNING: Assuming daily P-model run requested. To clarify please add a
      column 'onestep' with 'FALSE' or 'TRUE' to the 'params_siml' data.frame.
      in your driver.")
    drivers <- drivers |> dplyr::mutate(
      params_siml = purrr::map(.data$params_siml, ~ mutate(.x, onestep = FALSE)))
  }
  
  # Error if data.frame also specifies targets per row. (This corresponds to a now outdated format.)
  if ("targets" %in% names(obs)) {
    stop(
      "Provided calibration targets as column in obs data.frame(). Please only provide as argument 'targets' to calib_sofun() or cost function.")
  }
  
  ## generate a list of all calibration targets (across all obs rows)
  requested_targets <- names(targets)

  ## check input parameters
  if ((length(par) + length(par_fixed)) != 9) {
    stop("Error: Input calibratable and fixed parameters (par and par_fixed)
    do not match length of the required P-model parameters.")
  }

  ## define parameter set based on calibrated parameters
  calib_param_names <- c(
    "kphio", "kphio_par_a", "kphio_par_b",
    "soilm_thetastar", "soilm_betao",
    "beta_unitcostratio", "rd_to_vcmax",
    "tau_acclim", "kc_jmax")

  if (!is.null(par_fixed)) {
    params_modl <- list()
    # complete with calibrated values
    i <- 1 # start counter
    for (par_name in calib_param_names) {
      if (is.null(par_fixed[[par_name]])) {
        params_modl[[par_name]] <- unname(par[i])   # use calibrated par value
        i <- i + 1                                  # counter of calibrated params
      } else {
        params_modl[[par_name]] <- par_fixed[[par_name]]  # use fixed par value
      }
    }
  } else {
    params_modl <- as.list(par)       # all parameters calibrated
    names(params_modl) <- calib_param_names
  }

  # run the model
  df <- runread_pmodel_f(
    drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = FALSE
  )

  ## clean model output, unnest, and append "_mod"
  df <- df |>
    tidyr::unnest(data) |>
    dplyr::rename(any_of(c("bigD13C" = "bigD13C_mod_permil"))) |>
    # always keep gpp, since is used to get average trait prediction
    dplyr::select("sitename", any_of(unique(c("date", "gpp", requested_targets)))) |>
    dplyr::rename_with(
      .cols = -any_of(c("sitename", "date")),
      .fn = paste0,
      "_mod")

  # separate validation data into fluxes (daily model) and traits (onestep model), site by site
  is_flux <- apply(obs, 1, function(x) {
    "date" %in% colnames(x$data)
  })

  if (sum(is_flux) > 0) { # for daily model
    flux_sites <- obs$sitename[is_flux]

    # Unnest flux observations for our targets
    obs_flux <- obs[is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c("sitename", "date", requested_targets)))

    if (ncol(obs_flux) < 3) {
      warning("Dated observations (fluxes) are missing for the chosen targets.")
      df_flux <- data.frame()
    } else {
      # Join P-model output and flux observations
      df_flux <- df |>
        dplyr::filter(sitename %in% flux_sites) |>
        dplyr::left_join(
          obs_flux,
          by = c("sitename", "date"))    # observations with missing date are ignored
    }
  } else {
    df_flux <- data.frame()
  }

  if (sum(!is_flux) > 0) { # for onestep model
    trait_sites <- obs$sitename[!is_flux]

    # Unnest trait observations for our targets
    obs_trait <- obs[!is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c("sitename", requested_targets)))

    if (ncol(obs_trait) < 2) {
      warning("Non-dated observations (traits) are missing for the chosen targets.")
      df_trait <- data.frame()
    } else {
      # Join onestep model output and trait observations
      df_trait <- df |>
        dplyr::filter(sitename %in% trait_sites) |>
        dplyr::left_join(
          obs_trait,
          by = c("sitename")        # compare yearly averages rather than daily obs
        )
    }
  } else {
    df_trait <- data.frame()
  }

  # Calculate cost (RMSE) per target
  rmse <- lapply(requested_targets, function(target) {
    if (target %in% colnames(df_flux)) {
      error <- (df_flux[[target]] - df_flux[[paste0(target, "_mod")]])^2
    } else {
      error <- c()
    }
    if (target %in% colnames(df_trait)) {
      error <- c(error,
        (df_trait[[target]] - df_trait[[paste0(target, "_mod")]])^2)
    }
    sqrt(mean(error, na.rm = TRUE))
  }) |>
    unlist()

  names(rmse) <- requested_targets

  # Aggregate RMSE over requested_targets (weighted average)
  # target_weights <- c("bigD13C" = 0.2, "gpp" = 1, "le" = 0.1)
  if (!is.null(target_weights)) {
    stopifnot(sort(names(rmse)) == sort(names(target_weights)))
    cost <- sum(rmse * target_weights[names(rmse)])
  } else {
    cost <- mean(rmse, na.rm = TRUE)
  }

  return(cost)
}
