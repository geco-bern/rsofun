#' Calibrates SOFUN model parameters (for parallelized applications)
#'
#' Runs the requested model calibration and returns results
#'
#' This is the main function that handles the
#' calibration of SOFUN model parameters.
#'
#' @param drivers A data frame with driver data. See \code{\link{pmodel_drivers}}
#' for a description of the data structure. Additional columns can optionally be
#' provided to \code{drivers} to control e.g. the processing within a
#' personalized cost function.
#' @param obs A data frame containing observational data used for model
#'  calibration. See \code{\link{pmodel_validation}} for a description of the data
#'  structure. Additional columns can optionally be provided to \code{obs} to
#'  control e.g. the processing within a personalized cost function.
#' @param settings_calib A list containing model calibration settings.
#'  See the 'P-model usage' vignette for more information and examples.
#'  \describe{
#'   \item{\code{method}}{A string indicating the optimization method, either \code{'GenSA'}
#'   or \code{'BayesianTools'}.}
#'   \item{\code{par}}{A list of model parameters. For each parameter, an initial value
#'   and lower and upper bounds should be provided. The calibratable parameters
#'   include model parameters 'kphio', 'kphio_par_a', 'kphio_par_b', 'soilm_thetastar',
#'   'soilm_betao', 'beta_costunitratio', 'rd_to_vcmax', 'tau_acclim', 'kc_jmax'
#'   and 'rootzone_whc' , and (if
#'   doing Bayesian calibration) error parameters
#'   for each target variable, named for example 'err_gpp'. This list must match
#'   the input parameters of the calibration metric and the parameters should be
#'   given in the order above.}
#'   \item{\code{metric}}{A cost function. See the 'Cost functions for parameter
#'   calibration' vignette for examples.}
#'   \item{\code{control}}{A list of arguments passed on to the optimization function.
#'   If \code{method = 'GenSA'}, see \link[GenSA]{GenSA}. If \code{method = 'BayesianTools'}
#'   the list should include at least \code{sampler_runMCMC} and \code{settings_runMCMC},
#'   see \link[BayesianTools:runMCMC]{BayesianTools::runMCMC}.
#'   For \code{method = 'BayesianTools'}, \code{n_parallel_nrChains} can optionally be
#'   used to specify the number of cores to be used for parallel sampling of chains.
#'   (If >=2 it overwrites \code{settings_runMCMC$nrChains.}, if 0 or 1 parallel 
#'   sampling is deactivated.}
#'  }
#' @param optim_out A logical indicating whether the function returns and stores
#'  the raw output of the optimization functions (defaults to TRUE).
#' @param logpath A path where a log-file is created and progress logged for
#' parallel sampling, defaults to a temporary directory. The path is stored in
#' the output object as simulation `name` and `logpath`. (Note that this argument
#' is only used when parallelized sampling is requested.)
#' @param ... Optional arguments, simply passed on to the cost function.
#'
#' @return
#' A named list containing the calibrated parameter vector `par`,
#' the output object from the optimization `mod`, and additional identifier
#' outputs `name`, `logpath`, and `walltime`. For more details on the `mod`
#' output and how to evaluate it, see \link[BayesianTools:runMCMC]{runMCMC} (also
#' \href{https://florianhartig.github.io/BayesianTools/articles/BayesianTools.html}{this post})
#' and \link[GenSA]{GenSA}. If parallel (`n_parallel_nrChains >= 2`) also a 
#' logfile is created at `logpath`.
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @importFrom utils globalVariables
#' @importFrom foreach %dopar% foreach
#' @importFrom methods is
#' @import GenSA BayesianTools
#' @import parallel doParallel foreach
#'
#' @examples
#' # Calib 1: use Bayesian calibration approach with likelihood:
#' # Define priors of model parameters that will be calibrated
#' params_to_estimate <- list(
#'   kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
#'   err_gpp         = list(lower = 0.01, upper = 3, init = 0.8),
#'   err_bigD13C     = list(lower = 0.5, upper = 4.0, init = 2.0)
#' )
#' # Fix model parameters that won't be calibrated
#' params_fix       <- list(
#'   kphio_par_a        = 0,
#'   kphio_par_b        = 25,
#'   soilm_thetastar    = 0.6 * 240,
#'   soilm_betao        = 0.01,
#'   beta_unitcostratio = 146,
#'   rd_to_vcmax        = 0.014,
#'   tau_acclim         = 30,
#'   kc_jmax            = 0.41
#' )
#' # Define calibration settings
#' settings_calib <- list(
#'   method  = "BayesianTools",
#'   par     = params_to_estimate,
#'   metric  = rsofun::cost_likelihood_pmodel,
#'   control = list(
#'     sampler_runMCMC = "DEzs",
#'     settings_runMCMC = list(
#'       burnin = 0,
#'       iterations = 10,     # kept artificially low
#'       nrChains = 1         # 2, # these are the independent chains
#'     ),
#'     n_parallel_nrChains = 1  # 2, this can be parallelized
#'   )
#' )
#' # Run the calibration for GPP and D13C data
#' drivers_to_use <- rsofun::pmodel_drivers    |>
#'   dplyr::filter(sitename %in% c("FR-Pue", "lon_+146.13_lat_-032.97"))
#' obs_to_use     <- rsofun::pmodel_validation |>
#'   dplyr::filter(sitename %in% c("FR-Pue", "lon_+146.13_lat_-032.97"))
#' calib_output <- rsofun::calib_sofun(
#'   drivers = drivers_to_use,
#'   obs     = obs_to_use,
#'   settings_calib = settings_calib,
#'   # extra arguments for the cost function
#'   par_fixed = params_fix
#' )
#' calib_output$mod      # BayesianTools::mcmcSamplerList
#' calib_output$par      # Named vector
#' calib_output$walltime # Benchmarked time
#' calib_output$runtime  # unused
#' calib_output$name     # optionally used calibration name
#' calib_output$fpath    # path of rds output
#'
#' # Calib 2: use GenSa optimization of RMSE
#' # Calibrate the model and optimize the free parameters using demo datasets
#' settings_calib_rmse <- list(
#'   method = "GenSA",                   # minimizes the RMSE
#'   metric = cost_rmse_pmodel,          # our cost function returning the RMSE
#'   control = list( # control parameters for optimizer GenSA
#'     maxit = 2),
#'   par = list( # bounds for the parameter space
#'     kphio = list(lower = 0.02, upper = 0.2, init = 0.05)
#'   )
#' )
#' drivers_to_use <- pmodel_drivers
#' obs_to_use     <- pmodel_validation
#' pars_calib_rmse <- calib_sofun(
#'   # calib_sofun arguments:
#'   drivers  = drivers_to_use,
#'   obs      = obs_to_use,
#'   settings_calib = settings_calib_rmse,
#'   # extra arguments passed to the cost function:
#'   par_fixed = list( # fix all other parameters
#'     kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence
#'     # of kphio, setup ORG
#'     kphio_par_b        = 1.0,
#'     soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
#'     soilm_betao        = 0.0,
#'     beta_unitcostratio = 146.0,
#'     rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
#'     tau_acclim         = 30.0,
#'     kc_jmax            = 0.41
#'   )
#' )
calib_sofun <- function(
    drivers,
    obs,
    settings_calib,
    optim_out = TRUE, # whether to return chains
    # for storing rds and log.txt
    logpath = file.path(tempdir(), paste0("out_calib_", "my_calibration_name", ".rds.log.txt")),
    ...
    ) {

  # ensure input is ungrouped:
  drivers <- drivers |> dplyr::ungroup()
  obs <- obs |> dplyr::ungroup()

  # backwards compatibility: warn and recover if 'settings' instead of 'settings_calib' was provided
  # NOTE: actually not needed since partial argument matching works between these two
  # mc <- match.call() # will do partial argument matching (https://stackoverflow.com/a/15284583)
  mc <- sys.call()     # does no partial argumant matching
  if (is.null(mc$settings_calib) && !is.null(mc$settings)) {
    warning(paste0("Argument 'settings' is deprecated.", 
                   " Please use 'settings_calib' instead.",
                   " Have now used 'settings' for backwards compatibility."))
    settings_calib <- eval.parent(mc$settings)
  }
  
  #--- GenSA or BayesianTools----
  if (tolower(settings_calib$method) == "gensa") {
    #--- GenSA ----
    # convert to standard cost function naming
    cost <- settings_calib$metric
    
    # create bounds
    lower <- unlist(lapply(settings_calib$par, function(x) x$lower))
    upper <- unlist(lapply(settings_calib$par, function(x) x$upper))
    pars <- unlist(lapply(settings_calib$par, function(x) x$init))
    
    simname <- basename(logpath)
    start_time <- Sys.time()
    out <- GenSA::GenSA(
      par   = pars,
      fn    = cost,
      lower = lower,
      upper = upper,
      control = settings_calib$control,
      obs = obs,
      drivers = drivers,
      ...
    )
    end_time <- Sys.time()
    
    return_value <- list(par = out$par)
    
    if (optim_out) { # append raw GenSA output
      return_value <- c(return_value, list(mod = out))
    }
    
    # append naming information
    return_value$name <- simname
    return_value$logpath <- "" # just "" for GenSA
    
    # append timing information
    return_value$walltime <- end_time - start_time
    
  } else if (tolower(settings_calib$method) == "bayesiantools") {
    #--- Bayesiantools ----
  
    # backwards compatibility: set default values of parallelization options
    # by default do three chains
    if (is.null(settings_calib$control$settings_runMCMC$nrChains)) {
      settings_calib$control$settings_runMCMC$nrChains <- 3
    }
    # by default de-activate parallelization of independent chains (for easier passing on CRAN)
    if (is.null(settings_calib$control$n_parallel_nrChains)) {
      settings_calib$control$n_parallel_nrChains <- 1
    }
    # deactivate internal, within-sampler parallelization
    # # further info:
    # # https://cran.r-project.org/web/packages/BayesianTools/vignettes/InterfacingAModel.html#within-sampler-parallelization
    # # https://cran.r-project.org/web/packages/BayesianTools/vignettes/BayesianTools.html#reference-on-creating-likelihoods
    n_parallel_within_sampler <- FALSE 
    
    ## Preprocess: ----

    # parse prior distributions of parameters
    parnames <- names(settings_calib$par)
    priors  <- createMixedPrior(settings_calib$par)

    # Your external data
    # drivers
    # obs

    # sampler needs a function ll(random_par) for the likelihood,
    # since data is provided as a closure (drivers, obs) we need a function factory to be able
    # create this function on each worker

    # make available get_mod_obs_pmodel so we can export it to workers
    ll_factory <- function(obs, drivers, parnames, get_mod_obs, ...) {
      function(random_par) {
        eval(settings_calib$metric)(par = setNames(random_par, parnames),
          obs = obs,
          drivers = drivers,
          get_mod_obs = get_mod_obs,
          ...)
      }
    }

    ## Run the MCMC sampler: ----
    start_time <- Sys.time()

    if (settings_calib$control$n_parallel_nrChains > 1) { # parallel MCMC sampler:
      simname <- basename(logpath)

      message(paste0("Writing MCMC sampling log to: ", logpath))
      utils::flush.console()
      cl <- parallel::makeCluster(
        settings_calib$control$n_parallel_nrChains,
        outfile = logpath) # logpath for progress logging of all workers

      doParallel::registerDoParallel(cl)

      if (settings_calib$control$n_parallel_nrChains != settings_calib$control$settings_runMCMC$nrChains) {
        warning(sprintf(
          "Requested %d indep. chains, but ran %d indep. chains as `n_parallel_nrChains` takes precedence.",
          settings_calib$control$settings_runMCMC$nrChains, settings_calib$control$n_parallel_nrChains)
        )
      }
      # since parallel sampling, fix the number of chains of runMCMC to 1,
      # but call it multiple times and combine afterwards
      settings_calib$control$settings_runMCMC$nrChains <- 1
      
      # predefine variables for CRAN check compliance
      iii <- NULL

      requireNamespace("foreach", quietly = FALSE) # instead of foreach::`%dopar%`
      indep_chains <- foreach::foreach(
        iii = 1:settings_calib$control$n_parallel_nrChains,
        .packages = c("BayesianTools", "rsofun", "dplyr", "tidyr", "lubridate"),
        .export = c("get_mod_obs_pmodel"),
        .verbose = TRUE
      ) %dopar% {    # foreach::`%dopar%`
      
        set.seed(1982 + iii) # set a different seed on each worker
        bayesianSetup <- BayesianTools::createBayesianSetup(

          # inside worker: rebuild the closure so it picks up 'obs', 'drivers',
          #                'parnames', 'get_mod_obs_pmodel'
          likelihood = ll_factory(obs, drivers, parnames,
            get_mod_obs = get_mod_obs_pmodel,
            ...),
          prior      = priors,
          names      = parnames,
          parallel   = n_parallel_within_sampler)
        BayesianTools::runMCMC(
          bayesianSetup = bayesianSetup,
          sampler       = settings_calib$control$sampler_runMCMC,
          settings      = settings_calib$control$settings_runMCMC
        )
      }
      parallel::stopCluster(cl)
      mcmc_out <- BayesianTools::createMcmcSamplerList(indep_chains) # combine the independent chains

    } else { # sequential MCMC sampler:
      simname <- basename(logpath)
      logpath <- "" # use logs only for parallelized sampling

      # setup the bayesian sampling
      bayesianSetup <- BayesianTools::createBayesianSetup(
        likelihood = ll_factory(obs, drivers, parnames,
          get_mod_obs = get_mod_obs_pmodel,
          ...),
        prior      = priors,
        names      = parnames,
        parallel   = n_parallel_within_sampler)

      # since sequential sampling, let runMCMC handle the actual number of chains
      settings_calib$control$settings_runMCMC$nrChains <- settings_calib$control$settings_runMCMC$nrChains
      # calculate the runs
      mcmc_out <- BayesianTools::runMCMC(
        bayesianSetup = bayesianSetup,
        sampler       = settings_calib$control$sampler_runMCMC,
        settings      = settings_calib$control$settings_runMCMC
      )
    }


    ## Postprocess: ----

    # ensure return value 'mcmc_out' is a mcmcSamplerList even if nrChains==1
    # (by default runMCMC returns only a mcmcSampler if nrChains==1)
    if (methods::is(mcmc_out, "mcmcSampler")) {
      mcmc_out <- BayesianTools::createMcmcSamplerList(list(mcmc_out))
      # now mcmc_out is a mcmcSamplerList
    }

    end_time <- Sys.time()

    ## Build return object: 'return_value' ----

    # Extract MAP (maximum a posteriori value) of parameters
    bt_par <- BayesianTools::MAP(mcmc_out)$parametersMAP

    return_value <- list(par = bt_par)

    if (optim_out) { # append raw MCMC output chains
      return_value <- c(return_value, list(mod = mcmc_out))
    }

    # append naming information
    return_value$name <- simname
    return_value$logpath <- logpath # just "" if not parallelized

    # append timing information
    return_value$walltime <- end_time - start_time
    # return_value$runtime  <- NaN # NOTE: get_runtime_numeric(return_value)

  } else {
    stop("Unknown method passed to calib_sofun().")
  }
  return(return_value)
}
