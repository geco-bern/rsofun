#' Calibrates SOFUN model parameters
#'
#' Runs the requested model calibration, stores to data-folder and returns results
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
#' @param settings A list containing model calibration settings. 
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
#'   the list should include at least \code{settings} and \code{sampler}, see
#'   \link[BayesianTools:runMCMC]{BayesianTools::runMCMC}.}
#'  }
#' @param optim_out A logical indicating whether the function returns the raw
#'  output of the optimization functions (defaults to TRUE).
#' @param outpath TODO: describe
#' @param logpath TODO: describe
#' @param suffix  TODO: describe
#' @param ... Optional arguments, simply passed on to the cost function. 
#' 
#' @return TODO: describe: stores the rds to outpath, stores log to logpath
#' A named list containing the calibrated parameter vector `par` and
#' the output object from the optimization `mod`. For more details on this
#' output and how to evaluate it, see \link[BayesianTools:runMCMC]{runMCMC} (also
#' \href{https://florianhartig.github.io/BayesianTools/articles/BayesianTools.html}{this post})
#' and \link[GenSA]{GenSA}.
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @importFrom utils globalVariables
#' @importFrom foreach %dopar%
#' @importFrom methods is
#' @import GenSA BayesianTools
#' @import parallel doParallel foreach
#' 
#' @examples
#'  # Define priors of model parameters that will be calibrated
#'  params_to_estimate <- list(
#'    kphio           = list(lower = 0.02, upper = 0.15, init = 0.05),
#'    err_gpp         = list(lower = 0.01, upper = 3, init = 0.8)
#'  )
#'  # Fix model parameters that won't be calibrated
#'  params_fix       <- list(
#'    kphio_par_a        = 0,
#'    kphio_par_b        = 1.0,
#'    soilm_thetastar    = 0.6*240,
#'    soilm_betao        = 0.01,
#'    beta_unitcostratio = 146,
#'    rd_to_vcmax        = 0.014,
#'    tau_acclim         = 30,
#'    kc_jmax            = 0.41,
#'    err_bigD13C        = 1.0,
#'    err_vj             = 1.0,
#'    errbias_bigD13C    = 1.0,
#'    errbias_vj         = 1.0,
#'    errscale_gpp       = 1.0
#'  )
#'  
#'  # Define calibration settings
#'  settings <- list(
#'    method  = "BayesianTools",
#'    par     = params_to_estimate,
#'    metric  = rsofun::cost_likelihood_pmodel_bigD13C_vj_gpp,
#'    control = list(
#'      sampler = "DEzs",
#'      settings = list(
#'        nrChains = 1,
#'        burnin = 0,
#'        iterations = 50     # kept artificially low
#'      ),
#'      n_chains_independent   = 1, # 2, 
#'      n_parallel_independent = 1  # 2, this can be parallelized
#'    )
#'  )
#'  # Run the calibration for GPP data
#'  calib_output <- rsofun::calib_sofun_parallelized(
#'    drivers = rsofun::pmodel_drivers    |> dplyr::filter(sitename == "FR-Pue"),
#'    obs     = rsofun::pmodel_validation |> dplyr::filter(sitename == "FR-Pue"),
#'    settings = settings,
#'    suffix = "my_calibration_name",
#'    # extra arguments for the cost function
#'    par_fixed = params_fix
#'  )
#'  calib_output$mod      # BayesianTools::mcmcSamplerList
#'  calib_output$par      # Named vector
#'  calib_output$walltime # Benchmarked time
#'  calib_output$runtime  # unused
#'  calib_output$name     # optionally used calibration name
#'  calib_output$fpath    # path of rds output
calib_sofun_parallelized <- function(
    drivers,
    obs,
    settings,
    optim_out = TRUE, # whether to return chains
    outpath = tempdir(), logpath = "", suffix = "", # for storing rds
    ...
){
  print(paste0(Sys.time(),": start sampling of ", suffix))
  
  # backwards compatibility: set default values of parallelization options
  # by default do three chains
  if(is.null(settings$control$n_chains_independent)){    settings$control$n_chains_independent <- 3}
  # by default activate parallelization of independent chains, but deactivate within-sampler paralellization
  if(is.null(settings$control$n_parallel_independent)){  settings$control$n_parallel_independent <- settings$control$n_chains_independent}
  if(is.null(settings$control$n_parallel_within_sampler)){settings$control$n_parallel_within_sampler <- 1}
  
  if(settings$control$n_parallel_within_sampler==1){
    settings$control$n_parallel_within_sampler <- FALSE
  } # When set to 1 we want to deactivate parallel running.
  #   Unfortunately runMCMC interprets 1 as TRUE (leading parallelization to n_cores - 1)
  #   Thus we need to set it manually to FALSE.
  
  # ensure input is ungrouped:
  drivers <- drivers |> dplyr::ungroup()
  obs <- obs |> dplyr::ungroup()
  
  #--- Bayesiantools ----
  if (tolower(settings$method) == "bayesiantools"){
    
    ## Preprocess: ----
    
    # parse prior distributions of parameters
    parnames <- names(settings$par)
    priors  <- createMixedPrior(settings$par)
    
    # Your external data
    # drivers
    # obs
    
    # sampler needs a function ll(random_par) for the likelihood,
    # since data is provided as a closure (drivers, obs) we need a function factory to be able
    # create this function on each worker
    
    # make available get_mod_obs_pmodel_bigD13C_vj_gpp so we can export it to workers
    ll_factory <- function(obs, drivers, parnames, get_mod_obs, ...){
      function(random_par){
        eval(settings$metric)(par = setNames(random_par, parnames),
                              obs = obs,
                              drivers = drivers,
                              get_mod_obs = get_mod_obs,
                              ...)
      }
    }
    
    
    ## Run the MCMC sampler: ----
    start_time <- Sys.time()
    
    if (settings$control$n_parallel_independent > 1){ # parallel MCMC sampler:
      
      if (logpath != "") {
        cl <- parallel::makeCluster(settings$control$n_parallel_independent, outfile = logpath)
      } else {
        cl <- parallel::makeCluster(settings$control$n_parallel_independent)
      }
      doParallel::registerDoParallel(cl)
      
      if (settings$control$n_parallel_independent != settings$control$n_chains_independent){
        warning(sprintf(
          "Requested %d indep. chains, but ran %d indep. chains as `n_parallel_independent` takes precedence.",
          settings$control$n_chains_independent, settings$control$n_parallel_independent)
        )
      }
      # since parallel sampling, fix the number of chains of runMCMC to 1, but call it multiple times
      settings$control$settings$nrChains <- 1
      
      # predefine variables for CRAN check compliance
      iii <- NULL
      
      indep_chains <- foreach::foreach(
        iii = 1:settings$control$n_parallel_independent,
        .packages=c('BayesianTools','rsofun','dplyr','tidyr','lubridate'),
        .export = c('get_mod_obs_pmodel_bigD13C_vj_gpp'),
        .verbose = TRUE
      ) %dopar% {
        
        set.seed(1982 + iii) # set a different seed on each worker
        bayesianSetup <- BayesianTools::createBayesianSetup(
          
          # inside worker: rebuild the closure so it picks up 'obs', 'drivers', 'parnames', 'get_mod_obs_pmodel_bigD13C_vj_gpp'
          likelihood = ll_factory(obs, drivers, parnames, get_mod_obs = get_mod_obs_pmodel_bigD13C_vj_gpp, ...),
          prior      = priors,
          names      = parnames,
          parallel   = settings$control$n_parallel_within_sampler)
        BayesianTools::runMCMC(
          bayesianSetup = bayesianSetup,
          sampler       = settings$control$sampler,
          settings      = settings$control$settings
        )
      }
      parallel::stopCluster(cl)
      mcmc_out <- BayesianTools::createMcmcSamplerList(indep_chains) # combine the independent chains
      
    } else { # sequential MCMC sampler:
      
      # setup the bayesian sampling
      bayesianSetup <- BayesianTools::createBayesianSetup(
        likelihood = ll_factory(obs, drivers, parnames, get_mod_obs = get_mod_obs_pmodel_bigD13C_vj_gpp, ...),
        prior      = priors,
        names      = parnames,
        parallel   = settings$control$n_parallel_within_sampler)
      
      # since sequential sampling, let runMCMC handle the actual number of chains
      settings$control$settings$nrChains <- settings$control$n_chains_independent
      # calculate the runs
      mcmc_out <- BayesianTools::runMCMC(
        bayesianSetup = bayesianSetup,
        sampler       = settings$control$sampler,
        settings      = settings$control$settings
      )
    }
    
    
    ## Postprocess: ----
    
    # ensure return value 'mcmc_out' is a mcmcSamplerList even if n_chains_independent==1
    # (by default runMCMC returns only a mcmcSampler if n_chains_independent==1)
    if(methods::is(mcmc_out, "mcmcSampler")){
      mcmc_out <- BayesianTools::createMcmcSamplerList(list(mcmc_out)) # now mcmc_out is a mcmcSamplerList
    }
    
    end_time <- Sys.time()
    
    
    ## Build return object: 'return_value' ----
    
    # Extract MAP (maximum a posteriori value) of parameters
    bt_par <- BayesianTools::MAP(mcmc_out)$parametersMAP
    
    return_value <- list(par = bt_par)
    
    if (optim_out){ # append raw MCMC chains
      return_value <- c(return_value, list(mod = mcmc_out))
    }
    # if (input_out){ # append MCMC input
    #   return_value <- c(return_value,
    #                     list(bayesianSetup = bayesianSetup,             # unneded: return_value$mod[[1]]$setup
    #                          sampler       = settings$control$sampler,  # unneded: return_value$mod[[1]]$sampler
    #                          settings      = settings$control$settings))# unneded: return_value$mod[[1]]$settings
    # }
    
    # append timing information
    return_value$walltime <- end_time - start_time
    return_value$runtime  <- NaN # TODO: get_runtime_numeric(return_value)
    # summary(return_value)
    # plot(return_value$mod)
    # print(get_runtime_numeric(return_value))
    # print(get_walltime(return_value))
    
    return_value$name <- suffix
    
    ## Store results to file: ----
    return_value$fpath <- file.path(outpath, "calibrations", paste0("out_calib_", suffix, ".rds"))
    dir.create(path = dirname(return_value$fpath), showWarnings = FALSE)
    saveRDS(return_value, file = return_value$fpath, compress = "xz")
    
  } else if (tolower(settings$method) == "gensa"){
    stop("Unknown method (GenSA) passed to calib_sofun().")
    # TODO: support again GenSA
  } else {
    stop("Unknown method passed to calib_sofun().")
  }
  
  print(paste0(Sys.time(),": end sampling of ", suffix,
               ". Written *.rds-output to: ", return_value$fpath))
  
  return(return_value)
}
























#' Checks if provided parameter list describes a uniform distribution
#' 
#' @param distr_pars A named list with parameters for a distribution
#' @return TRUE or FALSE, if the provided parameters describe a uniform distribution
#' @examples 
#' prior_definitions_mixed <- list(
#'   par = list(
#'     par_uniform        = list(lower   = 10, upper = 30, init = 15),
#'     par_normal         = list(mean    = 10, sd    = 2),
#'     par_lognormal      = list(meanlog = -4, sdlog = 1.1),
#'     par_truncnormal    = list(mean    = 10, sd    = 2, lower = 9, upper = 14),
#'     par_trunclognormal = list(meanlog = -4, sdlog = 1.1, endpoint = 2.2),
#'     par_beta           = list(shape1  = 5, shape2 = 2)
#'   ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_uniform_prior)
is_uniform_prior <- function(distr_pars){identical(sort(names(distr_pars)), c("init", "lower", "upper"))}

#' Checks if provided parameter list describes a normal distribution
#' 
#' @param distr_pars A named list with parameters for a distribution
#' @return TRUE or FALSE, if the provided parameters describe a normal distribution
#' @examples 
#' prior_definitions_mixed <- list(
#'   par = list(
#'     par_uniform        = list(lower   = 10, upper = 30, init = 15),
#'     par_normal         = list(mean    = 10, sd    = 2),
#'     par_lognormal      = list(meanlog = -4, sdlog = 1.1),
#'     par_truncnormal    = list(mean    = 10, sd    = 2, lower = 9, upper = 14),
#'     par_trunclognormal = list(meanlog = -4, sdlog = 1.1, endpoint = 2.2),
#'     par_beta           = list(shape1  = 5, shape2 = 2)
#'   ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_normal_prior)
is_normal_prior <- function(distr_pars){identical(sort(names(distr_pars)), c("mean", "sd"))}

#' Checks if provided parameter list describes a lognormal distribution
#' 
#' @param distr_pars A named list with parameters for a distribution
#' @return TRUE or FALSE, if the provided parameters describe a lognormal distribution
#' @examples 
#' prior_definitions_mixed <- list(
#'   par = list(
#'     par_uniform        = list(lower   = 10, upper = 30, init = 15),
#'     par_normal         = list(mean    = 10, sd    = 2),
#'     par_lognormal      = list(meanlog = -4, sdlog = 1.1),
#'     par_truncnormal    = list(mean    = 10, sd    = 2, lower = 9, upper = 14),
#'     par_trunclognormal = list(meanlog = -4, sdlog = 1.1, endpoint = 2.2),
#'     par_beta           = list(shape1  = 5, shape2 = 2)
#'   ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_lognormal_prior)
is_lognormal_prior      <- function(distr_pars){identical(sort(names(distr_pars)), c("meanlog","sdlog"))}

#' Checks if provided parameter list describes a truncated normal distribution
#' 
#' @param distr_pars A named list with parameters for a distribution
#' @return TRUE or FALSE, if the provided parameters describe a truncated normal distribution
#' @examples 
#' prior_definitions_mixed <- list(
#'   par = list(
#'     par_uniform        = list(lower   = 10, upper = 30, init = 15),
#'     par_normal         = list(mean    = 10, sd    = 2),
#'     par_lognormal      = list(meanlog = -4, sdlog = 1.1),
#'     par_truncnormal    = list(mean    = 10, sd    = 2, lower = 9, upper = 14),
#'     par_trunclognormal = list(meanlog = -4, sdlog = 1.1, endpoint = 2.2),
#'     par_beta           = list(shape1  = 5, shape2 = 2)
#'   ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_truncnormal_prior)
is_truncnormal_prior <- function(distr_pars){identical(sort(names(distr_pars)), c("lower", "mean", "sd", "upper"))}

#' Checks if provided parameter list describes a truncated lognormal distribution
#' 
#' @param distr_pars A named list with parameters for a distribution
#' @return TRUE or FALSE, if the provided parameters describe a truncated lognormal distribution
#' @examples 
#' prior_definitions_mixed <- list(
#'   par = list(
#'     par_uniform        = list(lower   = 10, upper = 30, init = 15),
#'     par_normal         = list(mean    = 10, sd    = 2),
#'     par_lognormal      = list(meanlog = -4, sdlog = 1.1),
#'     par_truncnormal    = list(mean    = 10, sd    = 2, lower = 9, upper = 14),
#'     par_trunclognormal = list(meanlog = -4, sdlog = 1.1, endpoint = 2.2),
#'     par_beta           = list(shape1  = 5, shape2 = 2)
#'   ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_trunclognormal_prior)
is_trunclognormal_prior <- function(distr_pars){identical(sort(names(distr_pars)), c("endpoint","meanlog","sdlog"))}

#' Checks if provided parameter list describes a beta distribution
#' 
#' @param distr_pars A named list with parameters for a distribution
#' @return TRUE or FALSE, if the provided parameters describe a beta distribution
#' @examples 
#' prior_definitions_mixed <- list(
#'   par = list(
#'     par_uniform        = list(lower   = 10, upper = 30, init = 15),
#'     par_normal         = list(mean    = 10, sd    = 2),
#'     par_lognormal      = list(meanlog = -4, sdlog = 1.1),
#'     par_truncnormal    = list(mean    = 10, sd    = 2, lower = 9, upper = 14),
#'     par_trunclognormal = list(meanlog = -4, sdlog = 1.1, endpoint = 2.2),
#'     par_beta           = list(shape1  = 5, shape2 = 2)
#'   ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_beta_prior)
is_beta_prior <- function(distr_pars){identical(sort(names(distr_pars)), c("shape1","shape2"))}


#' Internal convenience function to create a mixed prior distribution
#' 
#' Similar to BayesianTools::createUniformPrior() 
#' 
#' @param prior_definitions A named list with parameters for a distribution
#' @return TRUE or FALSE, if the provided parameters describe a beta distribution
#' @examples 
#' library(BayesianTools)
#' prior_definitions_mixed <- list(
#'   par = list(
#'     par_uniform        = list(lower   = 10, upper = 30, init = 15),
#'     par_normal         = list(mean    = 10, sd    = 2),
#'     par_lognormal      = list(meanlog = -4, sdlog = 1.1),
#'     par_truncnormal    = list(mean    = 10, sd    = 2, lower = 9, upper = 14),
#'     par_trunclognormal = list(meanlog = -4, sdlog = 1.1, endpoint = 0.5),
#'     par_beta           = list(shape1  = 5, shape2 = 2)
#'   ))
#' priorMixed  <- rsofun:::createMixedPrior(prior_definitions_mixed$par)
#' priorMixed$sampler(2000)
#' hist(priorMixed$sampler(2000)[,6])
createMixedPrior <- function(prior_definitions){

  # # This is an example from the documentation: ?createPrior
  # # see also https://github.com/florianhartig/BayesianTools/issues/180
  # density = function(par){
  #   d1 = dunif(par[1], -2,6, log =TRUE)
  #   d2 = dnorm(par[2], mean= 2, sd = 3, log =TRUE)
  #   return(d1 + d2)
  # }
  # sampler = function(n=1){
  #   d1 = runif(n, -2,6)
  #   d2 = rnorm(n, mean= 2, sd = 3)
  #   return(cbind(d1,d2))
  # }
  
  # So we adapt this in the following:
  # createOwnPriorSimple <- function(prior_definitions, best = NULL){
  #   # check that we have simple case where all prior_definitions are uniform
  #   stopifnot(all(unlist(lapply(prior_definitions, function(x) identical(names(x), c("lower","upper","init"))))))
  #   stopifnot(length(prior_definitions) == 2)
  
  #   density <- function(par) {sum(
  #     dunif(x = par[1], min=prior_definitions[[1]]$lower, max=prior_definitions[[1]]$upper, log=T),
  #     dunif(x = par[2], min=prior_definitions[[2]]$lower, max=prior_definitions[[2]]$upper, log=T)
  #   )}
  #   sampler <- function(n=1) {cbind(
  #     runif(n = n, prior_definitions[[1]]$lower, prior_definitions[[1]]$upper),
  #     runif(n = n, prior_definitions[[2]]$lower, prior_definitions[[2]]$upper)
  #   )}
  #   out <- createPrior(density = density, sampler = sampler)
  # }
  
  # parse prior_definitions
  list_unif  <- lapply(prior_definitions, is_uniform_prior)
  list_norm  <- lapply(prior_definitions, is_normal_prior)
  list_lnorm <- lapply(prior_definitions, is_lognormal_prior)
  list_tnorm <- lapply(prior_definitions, is_truncnormal_prior)
  list_tlnorm<- lapply(prior_definitions, is_trunclognormal_prior)
  list_beta  <- lapply(prior_definitions, is_beta_prior)
  
  stopifnot(length(prior_definitions) == # check that all prior types are uniquely identified
              sum(list_unif==TRUE) +
              sum(list_norm==TRUE) +
              sum(list_lnorm==TRUE) +
              sum(list_tnorm==TRUE) +
              sum(list_tlnorm==TRUE) +
              sum(list_beta==TRUE))
  
  if (all(unlist(list_unif))) {  # all uniformly distributed: recover previous behavior
    pars  <- as.data.frame(do.call("rbind", prior_definitions))
    out   <- BayesianTools::createUniformPrior(
      lower = unlist(pars$lower),
      upper = unlist(pars$upper),
      best  = unlist(pars$init)
    )
  } else { # mixed distributions in prior
    
    # prepare definition of sampler and density of prior
    prior_args <- lapply(prior_definitions, \(def){
      if(is_uniform_prior(def)){            return(list(rfct=stats::runif,   dfct=stats::dunif,   args=list(min    =def$lower,   max   =def$upper)))} #def$init is unused
      else if(is_normal_prior(def)){        return(list(rfct=stats::rnorm,   dfct=stats::dnorm,   args=list(mean   =def$mean,    sd    =def$sd)))}
      else if(is_lognormal_prior(def)){     return(list(rfct=stats::rlnorm,  dfct=stats::dlnorm,  args=list(meanlog=def$meanlog, sdlog =def$sdlog)))}
      else if(is_truncnormal_prior(def)){   return(list(rfct=msm::rtnorm,    dfct=msm::dtnorm,    args=list(mean   =def$mean,    sd    =def$sd,    lower    =def$lower, upper =def$upper)))}
      else if(is_trunclognormal_prior(def)){return(list(rfct=ReIns::rtlnorm, dfct=ReIns::dtlnorm, args=list(meanlog=def$meanlog, sdlog =def$sdlog, endpoint =def$endpoint)))}
      else if(is_beta_prior(def)){          return(list(rfct=stats::rbeta,   dfct=stats::dbeta,   args=list(shape1 =def$shape1,  shape2=def$shape2)))}
      else {stop("Unknown prior distribution,")}
    })
    
    # define and create prior
    density <- function(par) {
      stopifnot(length(prior_args) == length(par))
      sum(unlist(
        # dunif(par[1], min=prior_definitions[[1]]$lower, max=prior_definitions[[1]]$upper, log=T),
        # dunif(par[2], min=prior_definitions[[2]]$lower, max=prior_definitions[[2]]$upper, log=T)
        lapply(seq_along(prior_args), \(i){ do.call(prior_args[[i]]$dfct, c(log=TRUE, x=par[[i]], prior_args[[i]]$args)) }) # unname(par[i]) or par[[i]] needed due to error message: "Problem in the priorError in (function (x, min = 0, max = 1, log = FALSE) : unused argument (x.kphio = 0.0487171059425491)"
      ))}
    sampler <- function(n=1) {do.call(cbind,
                                      # runif(1, prior_definitions[[1]]$lower, prior_definitions[[1]]$upper),
                                      # runif(1, prior_definitions[[2]]$lower, prior_definitions[[2]]$upper)
                                      lapply(prior_args, \(def){ do.call(def$rfct, c(n = n, def$args)) })
    )}
    out <- createPrior(density = density, sampler = sampler, best = NULL)
  }
  
  return(out)
}


# When using the output of createMixedPrior() to `createBayesianSetup` the
# min-max values of the prior are not stored in the fields 'lower' and 'upper'.
# For sensitivity analysis a sensible range of the parameters is needed. Therefore,
# we write the function getPriorMinMaxRanges() to be used in combination with
# sensitivity analysis.
getPriorMinMaxRanges <- function(morrisSetup_prior, settings_par){
  # by default use BayesianTool defined ranges
  inflim_arg <- morrisSetup_prior$lower    # named vector: e.g. c(kphio = 0.02, kphio_par_a = -0.004, err_gpp = 0.1, ... )
  suplim_arg <- morrisSetup_prior$upper    # named vector: e.g. c(kphio = 0.15, kphio_par_a = -0.001, err_gpp = 3,   ... )
  
  # Fallback if this fails with more complex priors
  if(is.null(inflim_arg) || is.null(suplim_arg)){
    par_names <- names(settings_par)
    # use the currently defined distributions that are compatible with createMixedPrior():
    # - is_uniform_prior        when has names: c("init", "lower", "upper"))}
    # - is_normal_prior         when has names: c("mean", "sd"))}
    # - is_lognormal_prior      when has names: c("meanlog","sdlog"))}
    # - is_truncnormal_prior    when has names: c("lower", "mean", "sd", "upper"))}
    # - is_trunclognormal_prior when has names: c("endpoint","meanlog","sdlog"))}
    # - is_beta_prior           when has names: c("shape1","shape2"))}
    
    # make a list with lower and upper for each of these distributions:
    inflim_suplim_list <- c(
      settings_par |> purrr::keep(is_uniform_prior       ) |> lapply(function(x) list(inflim = x$lower, suplim = x$upper)),
      # settings_par |> purrr::keep(is_normal_prior        ) |> names(), # TODO: add when needed
      # settings_par |> purrr::keep(is_lognormal_prior     ) |> names(), # TODO: add when needed
      settings_par |> purrr::keep(is_truncnormal_prior   ) |> lapply(function(x) list(inflim = x$lower, suplim = x$upper))
      # settings_par |> purrr::keep(is_trunclognormal_prior) |> names(), # TODO: add when needed
      # settings_par |> purrr::keep(is_beta_prior          ) |> names() # TODO: add when needed
    )
    stopifnot(settings_par |> purrr::keep(is_normal_prior        ) |> length() == 0) # TODO: remove once above is defined
    stopifnot(settings_par |> purrr::keep(is_lognormal_prior     ) |> length() == 0) # TODO: remove once above is defined
    stopifnot(settings_par |> purrr::keep(is_trunclognormal_prior) |> length() == 0) # TODO: remove once above is defined
    stopifnot(settings_par |> purrr::keep(is_beta_prior          ) |> length() == 0) # TODO: remove once above is defined
    
    # derive two vectors: one for inflim and for suplim
    inflim_arg <- unlist(lapply(inflim_suplim_list, `[[`, "inflim"))
    suplim_arg <- unlist(lapply(inflim_suplim_list, `[[`, "suplim"))
    
    # ensure correct order
    inflim_arg <- inflim_arg[par_names]
    suplim_arg <- suplim_arg[par_names]
  }
  return(list(inflim = inflim_arg, suplim = suplim_arg))
}
