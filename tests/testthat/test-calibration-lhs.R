test_that("LHS calibration is reproducible and returns posterior weights", {
  pars <- list(a = 0, fixed = 9)
  likelihood <- function(par, model_error, observation) {
    stats::dnorm(observation, par$a, model_error$sigma, log = TRUE)
  }
  args <- list(
    pars = pars,
    par_calib = list(a = list(
      mean = 0, sd = 1, distribution = "normal", lower = -2, upper = 2
    )),
    likelihood = likelihood,
    n_samples = 20,
    model_error = list(sigma = list(
      mean = 1, sd = 0.2, distribution = "lognormal",
      lower = 0.2, upper = 2
    )),
    seed = 42,
    observation = 0.7
  )

  out <- do.call(calib_sofun_lhs, args)
  repeated <- do.call(calib_sofun_lhs, args)

  expect_equal(out$samples, repeated$samples)
  expect_equal(sum(out$samples$weight), 1)
  expect_equal(out$par$fixed, 9)
  expect_equal(out$par$a, out$samples$a[out$map_index])
  expect_equal(out$model_error$sigma, out$samples$sigma[out$map_index])
  expect_true(out$effective_sample_size >= 1)
  expect_true(all(out$samples$a >= -2 & out$samples$a <= 2))
})

test_that("each LHS dimension occupies every stratum", {
  out <- calib_sofun_lhs(
    pars = list(a = 5),
    par_calib = list(a = list(
      mean = 5, sd = 1 / sqrt(3), distribution = "uniform",
      lower = 4, upper = 6
    )),
    likelihood = function(par, model_error) 0,
    n_samples = 10,
    seed = 1
  )
  strata <- floor((out$samples$a - 4) / 2 * 10)
  expect_setequal(strata, 0:9)
})

test_that("progress output is optional and reports completion", {
  arguments <- list(
    pars = list(a = 0),
    par_calib = list(a = list(
      mean = 0, sd = 1, distribution = "normal"
    )),
    likelihood = function(par, model_error) -par$a^2,
    n_samples = 4,
    seed = 1
  )

  expect_silent(do.call(calib_sofun_lhs, arguments))
  expect_message(
    do.call(calib_sofun_lhs, c(arguments, list(progress = TRUE))),
    "Calibration complete"
  )
})

test_that("invalid LHS calibration inputs are rejected", {
  likelihood <- function(par, model_error) 0
  expect_error(calib_sofun_lhs(
    list(a = 1), list(b = list(mean = 1, sd = 1, distribution = "normal")),
    likelihood, 2
  ), "subset")
  expect_error(calib_sofun_lhs(
    list(a = 1), list(a = list(mean = 1, sd = 1, distribution = "gamma")),
    likelihood, 2
  ), "Supported")
  expect_error(calib_sofun_lhs(
    list(a = 1), list(a = list(mean = 1, sd = 1, distribution = "normal")),
    likelihood, 2, progress = NA
  ), "progress")
})
