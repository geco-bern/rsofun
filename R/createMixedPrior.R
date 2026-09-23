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
#' ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_uniform_prior)
is_uniform_prior <- function(distr_pars) {
  identical(sort(names(distr_pars)), c("init", "lower", "upper"))
}

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
#' ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_normal_prior)
is_normal_prior <- function(distr_pars) {
  identical(sort(names(distr_pars)), c("mean", "sd"))
}

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
#' ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_lognormal_prior)
is_lognormal_prior      <- function(distr_pars) {
  identical(sort(names(distr_pars)), c("meanlog", "sdlog"))
}

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
#' ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_truncnormal_prior)
is_truncnormal_prior <- function(distr_pars) {
  identical(sort(names(distr_pars)), c("lower", "mean", "sd", "upper"))
}

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
#' ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_trunclognormal_prior)
is_trunclognormal_prior <- function(distr_pars) {
  identical(sort(names(distr_pars)), c("endpoint", "meanlog", "sdlog"))
}

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
#' ))
#' lapply(prior_definitions_mixed$par, rsofun:::is_beta_prior)
is_beta_prior <- function(distr_pars) {
  identical(sort(names(distr_pars)), c("shape1", "shape2"))
}


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
#'     # par_trunclognormal = list(meanlog = -4, sdlog = 1.1, endpoint = 0.5),
#'     par_beta           = list(shape1  = 5, shape2 = 2)
#' ))
#' priorMixed  <- rsofun:::createMixedPrior(prior_definitions_mixed$par)
#' priorMixed$sampler(2000)
#' hist(priorMixed$sampler(2000)[, 5])
createMixedPrior <- function(prior_definitions) {
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
  list_tlnorm <- lapply(prior_definitions, is_trunclognormal_prior)
  list_beta  <- lapply(prior_definitions, is_beta_prior)

  stopifnot(length(prior_definitions) == # check that all prior types are uniquely identified
    sum(list_unif == TRUE) +
      sum(list_norm == TRUE) +
      sum(list_lnorm == TRUE) +
      sum(list_tnorm == TRUE) +
      sum(list_tlnorm == TRUE) +
      sum(list_beta == TRUE))

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
      if (is_uniform_prior(def)) {           return(list(rfct = stats::runif,   dfct = stats::dunif,   args = list(min     = def$lower,   max   = def$upper))) # def$init is unused
      } else if (is_normal_prior(def)) {     return(list(rfct = stats::rnorm,   dfct = stats::dnorm,   args = list(mean    = def$mean,    sd    = def$sd)))
      } else if (is_lognormal_prior(def)) {  return(list(rfct = stats::rlnorm,  dfct = stats::dlnorm,  args = list(meanlog = def$meanlog, sdlog = def$sdlog)))
      } else if (is_truncnormal_prior(def)) {return(list(rfct = msm::rtnorm,    dfct = msm::dtnorm,    args = list(mean    = def$mean,    sd    = def$sd, 
                                                                                                                   lower   = def$lower,   upper = def$upper)))
      } else if (is_trunclognormal_prior(def)) {stop("Truncated lognormal temporally deactivated.")
      # } return(list(rfct=ReIns::rtlnorm, dfct=ReIns::dtlnorm, args=list(meanlog=def$meanlog, sdlog =def$sdlog, endpoint =def$endpoint)))
      } else if (is_beta_prior(def)) {       return(list(rfct = stats::rbeta,   dfct = stats::dbeta,   args = list(shape1 = def$shape1,  shape2 = def$shape2)))
      } else {stop("Unknown prior distribution,")
      }
    })

    # define and create prior
    density <- function(par) {
      stopifnot(length(prior_args) == length(par))
      sum(unlist(
        # dunif(par[1], min=prior_definitions[[1]]$lower, max=prior_definitions[[1]]$upper, log=T),
        # dunif(par[2], min=prior_definitions[[2]]$lower, max=prior_definitions[[2]]$upper, log=T)
        lapply(seq_along(prior_args), \(i){
          do.call(prior_args[[i]]$dfct, c(log = TRUE, x = par[[i]], prior_args[[i]]$args))
        }) # unname(par[i]) or par[[i]] needed due to error message: "Problem in the priorError in (function (x, min = 0, max = 1, log = FALSE) : unused argument (x.kphio = 0.0487171059425491)"
      ))
    }
    sampler <- function(n = 1) {
      do.call(cbind,
        # runif(1, prior_definitions[[1]]$lower, prior_definitions[[1]]$upper),
        # runif(1, prior_definitions[[2]]$lower, prior_definitions[[2]]$upper)
        lapply(prior_args, \(def){
          do.call(def$rfct, c(n = n, def$args))
        })
      )
    }
    out <- createPrior(density = density, sampler = sampler, best = NULL)
  }

  return(out)
}


# When using the output of createMixedPrior() to `createBayesianSetup` the
# min-max values of the prior are not stored in the fields 'lower' and 'upper'.
# For sensitivity analysis a sensible range of the parameters is needed. Therefore,
# we write the function getPriorMinMaxRanges() to be used in combination with
# sensitivity analysis.
# getPriorMinMaxRanges <- function(morrisSetup_prior, settings_par) {
#   # by default use BayesianTool defined ranges
#   inflim_arg <- morrisSetup_prior$lower    # named vector: e.g. c(kphio = 0.02, kphio_par_a = -0.004, err_gpp = 0.1, ... )
#   suplim_arg <- morrisSetup_prior$upper    # named vector: e.g. c(kphio = 0.15, kphio_par_a = -0.001, err_gpp = 3,   ... )
# 
#   # Fallback if this fails with more complex priors
#   if (is.null(inflim_arg) || is.null(suplim_arg)) {
#     par_names <- names(settings_par)
#     # use the currently defined distributions that are compatible with createMixedPrior():
#     # - is_uniform_prior        when has names: c("init", "lower", "upper"))}
#     # - is_normal_prior         when has names: c("mean", "sd"))}
#     # - is_lognormal_prior      when has names: c("meanlog","sdlog"))}
#     # - is_truncnormal_prior    when has names: c("lower", "mean", "sd", "upper"))}
#     # - is_trunclognormal_prior when has names: c("endpoint","meanlog","sdlog"))}
#     # - is_beta_prior           when has names: c("shape1","shape2"))}
# 
#     # make a list with lower and upper for each of these distributions:
#     inflim_suplim_list <- c(
#       settings_par |> purrr::keep(is_uniform_prior) |> lapply(function(x) list(inflim = x$lower, suplim = x$upper)),
#       # settings_par |> purrr::keep(is_normal_prior        ) |> names(), # NOTE: add when needed
#       # settings_par |> purrr::keep(is_lognormal_prior     ) |> names(), # NOTE: add when needed
#       settings_par |> purrr::keep(is_truncnormal_prior) |> lapply(function(x) list(inflim = x$lower, suplim = x$upper))
#       # settings_par |> purrr::keep(is_trunclognormal_prior) |> names(), # NOTE: add when needed
#       # settings_par |> purrr::keep(is_beta_prior          ) |> names() # NOTE: add when needed
#     )
#     # NOTE: suplim is currently not implemented for all distributions, hence error if on of these is requested:
#     stopifnot(settings_par |> purrr::keep(is_normal_prior) |> length() == 0) # NOTE: remove once above is defined
#     stopifnot(settings_par |> purrr::keep(is_lognormal_prior) |> length() == 0) # NOTE: remove once above is defined
#     stopifnot(settings_par |> purrr::keep(is_trunclognormal_prior) |> length() == 0) # NOTE: remove once above is defined
#     stopifnot(settings_par |> purrr::keep(is_beta_prior) |> length() == 0) # NOTE: remove once above is defined
# 
# 
#     # derive two vectors: one for inflim and for suplim
#     inflim_arg <- unlist(lapply(inflim_suplim_list, `[[`, "inflim"))
#     suplim_arg <- unlist(lapply(inflim_suplim_list, `[[`, "suplim"))
# 
#     # ensure correct order
#     inflim_arg <- inflim_arg[par_names]
#     suplim_arg <- suplim_arg[par_names]
#   }
#   return(list(inflim = inflim_arg, suplim = suplim_arg))
# }
