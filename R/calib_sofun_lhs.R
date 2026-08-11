#' Bayesian calibration over a parallel Latin hypercube design
#'
#' Draws a finite ensemble of parameter combinations with Latin hypercube
#' sampling (LHS), evaluates their likelihoods independently, and assigns each
#' combination a discrete posterior probability. This is useful when a model
#' run is expensive and adaptive MCMC would leave most available workers idle.
#'
#' @param pars Named list containing the complete model parameter set.
#' @param par_calib Named list describing the subset of `pars` to calibrate.
#'   Each element must contain `mean`, `sd`, and `distribution`; optional
#'   `lower` and `upper` values truncate the distribution. Supported
#'   distributions are `"normal"`, `"lognormal"`, and `"uniform"`.
#' @param likelihood Function returning one numeric log-likelihood (or `-Inf`). It is called
#'   as `likelihood(par, model_error, ...)`, where `par` is the complete model
#'   parameter list and `model_error` is a named list.
#' @param n_samples Number of LHS combinations.
#' @param model_error Named list of distribution specifications for error terms
#'   to tune jointly with the model parameters. It has the same format as
#'   `par_calib`, but its values are not inserted into `pars`.
#' @param n_cores Number of parallel worker processes. One evaluates serially.
#' @param seed Integer random seed used for a reproducible LHS design.
#' @param progress Logical; if `TRUE`, print design, likelihood-evaluation, and
#'   completion progress. Up to approximately 20 evaluation updates are printed;
#'   progress is disabled by default.
#' @param ... Data and other arguments passed unchanged to `likelihood`.
#'
#' @return A list with `par` (the MAP model parameters), `model_error` (the MAP
#'   error terms), and `samples`. `samples` contains the LHS values, log prior,
#'   log likelihood, log posterior density, and normalized posterior `weight`.
#'   Because the LHS design is drawn from the prior, its importance weights are
#'   proportional to the likelihood (the prior/proposal terms cancel). The
#'   returned `par` contains the complete parameter set supplied in `pars`.
#' @export
calib_sofun_lhs <- function(
    pars,
    par_calib,
    likelihood,
    n_samples,
    model_error = list(),
    n_cores = 1L,
    seed = NULL,
    progress = FALSE,
    ...
) {
  # ---- 1. Validate the calibration request ---------------------------------
  # Fail early, before changing the random-number state or starting worker
  # processes. `pars` is always the complete parameter list; `par_calib` only
  # names entries in that list which may be replaced by sampled values.
  if (!is.list(pars) || is.null(names(pars)) || any(names(pars) == "")) {
    stop("`pars` must be a named list.", call. = FALSE)
  }
  if (!is.list(par_calib) || is.null(names(par_calib)) ||
      !all(names(par_calib) %in% names(pars))) {
    stop("`par_calib` must be a named subset of `pars`.", call. = FALSE)
  }
  if (length(intersect(names(par_calib), names(model_error)))) {
    # Keeping the two namespaces separate prevents an error parameter such as
    # sigma_gpp from being passed accidentally to the compiled model interface.
    stop("Model parameters and model-error terms must have distinct names.",
      call. = FALSE)
  }
  if (!is.function(likelihood)) {
    stop("`likelihood` must be a function.", call. = FALSE)
  }
  n_samples <- as.integer(n_samples)
  n_cores <- as.integer(n_cores)
  # Coercion above accepts whole-valued numeric inputs while the checks below
  # reject missing, zero, and negative sizes.
  if (length(n_samples) != 1L || is.na(n_samples) || n_samples < 1L) {
    stop("`n_samples` must be a positive integer.", call. = FALSE)
  }
  if (length(n_cores) != 1L || is.na(n_cores) || n_cores < 1L) {
    stop("`n_cores` must be a positive integer.", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("`progress` must be TRUE or FALSE.", call. = FALSE)
  }

  # Treat model and error parameters as dimensions of one joint prior while
  # retaining their separate roles during likelihood evaluation.
  specifications <- c(par_calib, model_error)
  if (!length(specifications)) {
    stop("At least one model parameter or model-error term is required.",
      call. = FALSE)
  }
  invisible(lapply(specifications, .validate_lhs_spec))

  # ---- 2. Set up reproducible random-number generation ----------------------
  # Save and restore the caller's RNG state. Thus a seeded calibration is
  # reproducible but does not perturb random draws made after this function.
  if (!is.null(seed)) {
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv,
      inherits = FALSE)
    if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      if (old_seed_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  if (progress) {
    message(sprintf(
      "Preparing %d Latin hypercube samples for %d model parameter(s) and %d error term(s).",
      n_samples, length(par_calib), length(model_error)
    ))
  }

  # ---- 3. Construct the Latin hypercube prior design ------------------------
  # For each dimension, sample.int() independently permutes the n strata and
  # runif() chooses a point inside each stratum. Consequently every marginal
  # prior has exactly one candidate in each equal-probability interval, while
  # the independently permuted columns create the multivariate combinations.
  probabilities <- vapply(
    specifications,
    function(x) (sample.int(n_samples) - stats::runif(n_samples)) / n_samples,
    numeric(n_samples)
  )
  if (is.null(dim(probabilities))) {
    # vapply() drops the matrix dimension for a one-parameter calibration.
    probabilities <- matrix(probabilities, ncol = 1L)
  }
  colnames(probabilities) <- names(specifications)
  draws <- as.data.frame(Map(
    function(probability, specification) {
      .qlhs_prior(probability, specification)
    },
    as.data.frame(probabilities), specifications
  ), check.names = FALSE)
  # The inverse-CDF transformation in .qlhs_prior() maps uniform strata to the
  # requested marginal distribution, including any truncation bounds.
  names(draws) <- names(specifications)

  # ---- 4. Calculate prior density at every candidate ------------------------
  # Priors are independent by construction, so their joint log density is the
  # sum of marginal log densities. This quantity is needed for posterior-density
  # ranking and MAP selection, but not for the importance weights below.
  log_prior <- vapply(seq_len(n_samples), function(i) {
    sum(unlist(Map(
      function(specification, value) .dlhs_prior(value, specification),
      specifications, as.list(draws[i, , drop = FALSE])
    ), use.names = FALSE))
  }, numeric(1))

  # ---- 5. Define one model/likelihood evaluation ----------------------------
  # Capture `...`, the LHS draws, and the base parameter list in a closure. A
  # self-contained closure can be serialized once to each PSOCK worker, avoiding
  # package-global mutable state and allowing arbitrary user likelihoods.
  dots <- list(...)
  evaluate_one <- function(i) {
    # Start from the complete parameter list so fixed entries are identical in
    # every run, then replace only the explicitly calibrated subset.
    candidate <- pars
    candidate[names(par_calib)] <- as.list(draws[i, names(par_calib), drop = FALSE])
    # Error terms are supplied separately to the likelihood and never inserted
    # into the model parameter list.
    errors <- as.list(draws[i, names(model_error), drop = FALSE])
    value <- do.call(likelihood, c(list(
      par = candidate,
      model_error = errors
    ), dots))
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        identical(value, Inf)) {
      stop("`likelihood` must return one numeric log-likelihood (or -Inf).",
        call. = FALSE)
    }
    # -Inf is valid: it marks a candidate that is impossible under the data.
    as.numeric(value)
  }

  # ---- 6. Run simulations serially or across independent processes ----------
  # Every LHS candidate is known in advance and has no dependency on any other
  # candidate. This is the key distinction from serial proposal-based MCMC and
  # lets expensive C-model simulations occupy all workers concurrently.
  start_time <- Sys.time()
  n_workers <- min(n_cores, n_samples)
  # Aim for at most about 20 progress updates. A chunk is never smaller than the
  # worker count, ensuring that each parallel batch can keep all workers busy.
  chunk_size <- max(n_workers, ceiling(n_samples / 20))
  evaluation_chunks <- split(
    seq_len(n_samples),
    ceiling(seq_len(n_samples) / chunk_size)
  )
  if (progress) {
    message(sprintf(
      "Evaluating likelihoods using %d worker process%s.",
      n_workers, if (n_workers == 1L) "" else "es"
    ))
  }
  if (n_workers > 1L) {
    # PSOCK processes work across operating systems and isolate compiled model
    # calls from one another. stopCluster() is registered immediately so errors
    # in a likelihood cannot leave orphan worker processes behind.
    cluster <- parallel::makeCluster(n_workers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    # Export the large closure once rather than serializing drivers and
    # observations again for every progress chunk.
    parallel::clusterExport(cluster, "evaluate_one", envir = environment())
  }
  log_likelihood <- numeric(n_samples)
  for (chunk in evaluation_chunks) {
    values <- if (n_workers == 1L) {
      vapply(chunk, evaluate_one, numeric(1))
    } else {
      # Load balancing assigns a new candidate to a worker as soon as it becomes
      # free, which helps when simulation runtimes differ among parameter sets.
      unlist(parallel::parLapplyLB(cluster, chunk, function(i) evaluate_one(i)),
        use.names = FALSE)
    }
    log_likelihood[chunk] <- values
    if (progress) {
      message(sprintf(
        "Evaluated %d/%d samples (%d%%).",
        max(chunk), n_samples, round(100 * max(chunk) / n_samples)
      ))
    }
  }
  walltime <- Sys.time() - start_time

  # ---- 7. Form the finite posterior approximation ---------------------------
  # Bayes' rule gives log posterior density = log prior + log likelihood. The
  # highest value among the sampled candidates is the finite-design MAP.
  log_posterior <- log_prior + log_likelihood
  if (!any(is.finite(log_posterior)) || !any(is.finite(log_likelihood))) {
    stop("All sampled combinations have non-finite posterior density.",
      call. = FALSE)
  }
  # The design itself was sampled from the prior. For importance sampling,
  # posterior/proposal = (prior * likelihood)/prior, so normalized weights are
  # proportional to likelihood alone. Subtracting the maximum before exp()
  # prevents numerical underflow without changing relative weights.
  max_log_likelihood <- max(log_likelihood)
  weight <- exp(log_likelihood - max_log_likelihood)
  weight <- weight / sum(weight)
  map_index <- which.max(log_posterior)
  samples <- cbind(
    sample_id = seq_len(n_samples), draws,
    log_prior = log_prior,
    log_likelihood = log_likelihood,
    log_posterior = log_posterior,
    weight = weight
  )

  # ---- 8. Reconstruct convenient MAP inputs ---------------------------------
  # Return a full parameter list rather than only the calibrated subset. It can
  # therefore be passed directly to runread_cnmodel_f() for the final MAP run.
  map_par <- pars
  map_par[names(par_calib)] <- as.list(draws[map_index, names(par_calib),
    drop = FALSE])
  map_error <- as.list(draws[map_index, names(model_error), drop = FALSE])

  if (progress) {
    message(sprintf(
      "Calibration complete in %.1f seconds; MAP sample is %d and effective sample size is %.1f.",
      as.numeric(walltime, units = "secs"), map_index, 1 / sum(weight^2)
    ))
  }

  # ---- 9. Return MAP values and all posterior diagnostics -------------------
  # Effective sample size summarizes concentration of the normalized weights:
  # it approaches n_samples for diffuse weights and one for a dominant sample.
  list(
    par = map_par,
    model_error = map_error,
    samples = samples,
    map_index = map_index,
    effective_sample_size = 1 / sum(weight^2),
    walltime = walltime
  )
}

# ---- Distribution helpers ---------------------------------------------------

# Validate one prior specification shared by model and model-error parameters.
.validate_lhs_spec <- function(specification) {
  required <- c("mean", "sd", "distribution")
  if (!is.list(specification) || !all(required %in% names(specification))) {
    stop("Each distribution needs `mean`, `sd`, and `distribution`.",
      call. = FALSE)
  }
  if (!is.numeric(specification$mean) || length(specification$mean) != 1L ||
      !is.finite(specification$mean) || !is.numeric(specification$sd) ||
      length(specification$sd) != 1L || !is.finite(specification$sd) ||
      specification$sd <= 0) {
    stop("Distribution `mean` and `sd` must be finite scalars with `sd > 0`.",
      call. = FALSE)
  }
  distribution <- tolower(specification$distribution)
  if (!distribution %in% c("normal", "lognormal", "uniform")) {
    stop("Supported distributions are normal, lognormal, and uniform.",
      call. = FALSE)
  }
  if (distribution == "lognormal" && specification$mean <= 0) {
    stop("A lognormal mean must be positive.", call. = FALSE)
  }
  bounds <- .lhs_bounds(specification)
  # Checking resolved rather than only explicit bounds also validates the
  # mean/sd-derived interval used for an unbounded uniform specification.
  if (bounds[[1L]] >= bounds[[2L]]) {
    stop("Each distribution must have `lower < upper`.", call. = FALSE)
  }
  invisible(TRUE)
}

# Resolve explicit truncation bounds or distribution-specific defaults.
.lhs_bounds <- function(specification) {
  distribution <- tolower(specification$distribution)
  if (distribution == "uniform") {
    # A uniform distribution with mean mu and standard deviation sigma spans
    # mu +/- sqrt(3) * sigma. Explicit bounds, when supplied, take precedence.
    default <- specification$mean + c(-1, 1) * sqrt(3) * specification$sd
  } else if (distribution == "lognormal") {
    # Lognormal values are intrinsically positive; normal values are unbounded.
    default <- c(0, Inf)
  } else {
    default <- c(-Inf, Inf)
  }
  c(
    if (is.null(specification$lower)) default[[1L]] else specification$lower,
    if (is.null(specification$upper)) default[[2L]] else specification$upper
  )
}

# Convert natural-scale lognormal moments to meanlog and sdlog when necessary.
.lhs_distribution_parameters <- function(specification) {
  if (tolower(specification$distribution) != "lognormal") {
    return(c(specification$mean, specification$sd))
  }
  # R's dlnorm()/plnorm()/qlnorm() expect moments on the log scale, whereas the
  # public API deliberately asks users for interpretable natural-scale moments.
  variance <- specification$sd^2
  meanlog <- log(specification$mean^2 /
    sqrt(variance + specification$mean^2))
  sdlog <- sqrt(log1p(variance / specification$mean^2))
  c(meanlog, sdlog)
}

# Transform an LHS probability stratum through a possibly truncated prior CDF.
.qlhs_prior <- function(probability, specification) {
  distribution <- tolower(specification$distribution)
  bounds <- .lhs_bounds(specification)
  parameters <- .lhs_distribution_parameters(specification)
  if (distribution == "uniform") {
    # For a uniform prior, its support already represents the truncation bounds.
    return(stats::qunif(probability, bounds[[1L]], bounds[[2L]]))
  }
  # Convert physical bounds to cumulative-probability bounds, rescale the unit
  # LHS probability into that retained probability mass, then invert the CDF.
  probability_bounds <- if (distribution == "normal") {
    stats::pnorm(bounds, parameters[[1L]], parameters[[2L]])
  } else {
    stats::plnorm(bounds, parameters[[1L]], parameters[[2L]])
  }
  probability <- probability_bounds[[1L]] + probability *
    diff(probability_bounds)
  if (distribution == "normal") {
    stats::qnorm(probability, parameters[[1L]], parameters[[2L]])
  } else {
    stats::qlnorm(probability, parameters[[1L]], parameters[[2L]])
  }
}

# Evaluate the normalized density of a possibly truncated prior.
.dlhs_prior <- function(value, specification) {
  distribution <- tolower(specification$distribution)
  bounds <- .lhs_bounds(specification)
  parameters <- .lhs_distribution_parameters(specification)
  if (value < bounds[[1L]] || value > bounds[[2L]]) return(-Inf)
  if (distribution == "uniform") {
    return(stats::dunif(value, bounds[[1L]], bounds[[2L]], log = TRUE))
  }
  if (distribution == "normal") {
    log_density <- stats::dnorm(value, parameters[[1L]], parameters[[2L]],
      log = TRUE)
    mass <- diff(stats::pnorm(bounds, parameters[[1L]], parameters[[2L]]))
  } else {
    log_density <- stats::dlnorm(value, parameters[[1L]], parameters[[2L]],
      log = TRUE)
    mass <- diff(stats::plnorm(bounds, parameters[[1L]], parameters[[2L]]))
  }
  # Conditioning on the truncation interval divides the original density by
  # the probability mass retained between lower and upper bounds.
  log_density - log(mass)
}
