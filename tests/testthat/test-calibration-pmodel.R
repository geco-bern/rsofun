set.seed(10)


test_that("test function definitions for prior parameter distributions (to be used with BayesianTools)", {
  skip_on_cran()

  # inputs
  prior_definitions_uniform <- list(par = list(
    par1 = list(lower = 0.5, upper = 1.5, init = 1.0),
    par2 = list(lower = 2.5, upper = 3.5, init = 3.0)))

  prior_definitions_mixed <- list(
    par = list(
      par_uniform        = list(lower   = 10, upper = 30, init = 15),
      par_normal         = list(mean    = 10, sd    = 2),
      par_lognormal      = list(meanlog = -4, sdlog = 1.1),
      par_truncnormal    = list(mean    = 10, sd    = 2, lower = 9, upper = 14),
      # par_trunclognormal = list(meanlog = -4, sdlog = 1.1, endpoint = 0.5),
      par_beta           = list(shape1  = 5, shape2 = 2)
  ))

  # tests
  # test is_xxx_prior() functions:
  make_named_list <- function(lst){ setNames(lst, names(prior_definitions_mixed$par))}
  expect_identical(make_named_list(list(TRUE, FALSE, FALSE, FALSE, FALSE)),
    lapply(prior_definitions_mixed$par, rsofun:::is_uniform_prior        ))
  expect_identical(make_named_list(list(FALSE, TRUE, FALSE, FALSE, FALSE)),
    lapply(prior_definitions_mixed$par, rsofun:::is_normal_prior         ))
  expect_identical(make_named_list(list(FALSE, FALSE, TRUE, FALSE, FALSE)),
    lapply(prior_definitions_mixed$par, rsofun:::is_lognormal_prior      ))
  expect_identical(make_named_list(list(FALSE, FALSE, FALSE, TRUE, FALSE)),
    lapply(prior_definitions_mixed$par, rsofun:::is_truncnormal_prior    ))
  # expect_identical(make_named_list(list(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
  #   lapply(prior_definitions_mixed$par, rsofun:::is_trunclognormal_prior ))
  expect_identical(make_named_list(list(FALSE, FALSE, FALSE, FALSE, TRUE)),
    lapply(prior_definitions_mixed$par, rsofun:::is_beta_prior           ))

  # test createMixedPrior() function:
  priorUnif <- rsofun:::createMixedPrior(prior_definitions_uniform$par)
  priorUnif2 <- BayesianTools::createUniformPrior(
    lower = unname(unlist(lapply(prior_definitions_uniform$par, `[`, "lower"))),
    upper = unname(unlist(lapply(prior_definitions_uniform$par, `[`, "upper")))
  )

  priorUnif$density(x = c(1.1, 2.5)) == priorUnif2$density(x = c(1.1, 2.5))
  priorUnif$density(x = c(1.1, 2.4)) == priorUnif2$density(x = c(1.1, 2.4))
  # plot_prior_density(priorUnif,  parNames = names(prior_definitions_uniform$par), n=10000)
  # plot_prior_density(priorUnif2, parNames = names(prior_definitions_uniform$par), n=10000)

  priorMixed  <- rsofun:::createMixedPrior(prior_definitions_mixed$par)
  # plot_prior_density(priorMixed, parNames = names(prior_definitions_mixed$par), n=10000)

  expect_equal(0,           priorUnif$density(c(1.0, 3.0)),                    tolerance = 1e-7)
  expect_equal(-2.4675764,  priorMixed$density(c(15, 10, exp(-4), 12, 0.75)), tolerance = 1e-7)
  expect_equal(-2.4675764,  priorMixed$density(c(10, 10, exp(-4), 12, 0.75)), tolerance = 1e-7)
  expect_equal(-2.4675764,  priorMixed$density(c(30, 10, exp(-4), 12, 0.75)), tolerance = 1e-7)

  # expect_equal(-Inf,       priorMixed$density(c(15, 10, exp(-4), 12, 0.6,     0.75)), tolerance = 1e-7)
  expect_equal(-3.59257637, priorMixed$density(c(15, 8,  exp(-4), 13, 0.75)), tolerance = 1e-7)
  expect_equal(-Inf,        priorMixed$density(c(15, 10, exp(-4), 14.1, 0.75)), tolerance = 1e-7)
})


test_that("test GPP calibration routine p-model (BT, likelihood maximization)", {
  skip_on_cran()
  drivers <- pmodel_drivers |> dplyr::filter(sitename == "FR-Pue")
  obs <- pmodel_validation |> dplyr::filter(sitename == "FR-Pue")

  params_fix <- list(
    # kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    soilm_betao        = 0.01,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )

  settings <- list(
    method              = "bayesiantools",
    targets             = c("gpp"),
    sitenames           = "FR-Pue",
    metric              = rsofun::cost_likelihood_pmodel,
    control = list(
      sampler = "DEzs",
      settings = list(
        n_chains_independent = 1,
        burnin = 1,
        iterations = 4
      )
    ),
    par = list(
      kphio = list(lower = 0.04, upper = 0.09, init = 0.05),
      err_gpp = list(lower = 0.01, upper = 4, init = 2)
    )
  )

  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    # extra arguments for the cost function
    par_fixed = params_fix,
    parallel = FALSE,
    ncores = 1
  )

  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test GPP calibration routine p-model (GenSA, rmse, all params)", {
  skip_on_cran()
  drivers <- pmodel_drivers |> dplyr::filter(sitename == "FR-Pue")
  obs <- pmodel_validation |> dplyr::filter(sitename == "FR-Pue")

  settings <- list(
    method              = "gensa",
    targets             = c("gpp"),
    sitenames           = "FR-Pue",
    metric              = rsofun::cost_rmse_pmodel,
    control = list(
      maxit = 2
    ),
    par = list(
      kphio = list(lower = 0.04, upper = 0.09, init = 0.05),
      kphio_par_a = list(lower = 0, upper = 1, init = 0.2),
      kphio_par_b = list(lower = 10, upper = 40, init = 25),
      soilm_thetastar = list(lower = 0, upper = 3000, init = 0.6 * 240),
      soilm_betao = list(lower = 0, upper = 1, init = 0.2),
      beta_unitcostratio = list(lower = 50, upper = 200, init = 146),
      rd_to_vcmax = list(lower = 0.01, upper = 0.1, init = 0.014),
      tau_acclim = list(lower = 7, upper = 60, init = 30),
      kc_jmax = list(lower = 0.2, upper = 0.8, init = 0.41)
    )
  )

  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    optim_out = FALSE
  )

  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test GPP/bigDelta13C calibration routine p-model (BT, likelihood, all params)", {
  skip_on_cran()

  # Calib 1: use Bayesian calibration approach with likelihood:
  drivers <- rsofun::pmodel_drivers    |>
    dplyr::filter(sitename %in% c("FR-Pue", "lon_+146.13_lat_-032.97"))
  obs <- rsofun::pmodel_validation |>
    dplyr::filter(sitename %in% c("FR-Pue", "lon_+146.13_lat_-032.97"))
  # Define priors of model parameters that will be calibrated
  params_to_estimate <- list(
    kphio = list(lower = 0.04, upper = 0.09, init = 0.05),
    kphio_par_a = list(lower = 0, upper = 1, init = 0.2),
    kphio_par_b = list(lower = 10, upper = 40, init = 25),
    soilm_thetastar = list(lower = 0, upper = 3000, init = 0.6 * 240),
    soilm_betao = list(lower = 0, upper = 1, init = 0.2),
    beta_unitcostratio = list(lower = 50, upper = 200, init = 146),
    rd_to_vcmax = list(lower = 0.01, upper = 0.1, init = 0.014),
    tau_acclim = list(lower = 7, upper = 60, init = 30),
    kc_jmax = list(lower = 0.2, upper = 0.8, init = 0.41),
    err_gpp         = list(lower = 0.01, upper = 3, init = 0.8),
    err_bigD13C     = list(lower = 0.5, upper = 4.0, init = 2.0)
  )
  # Fix model parameters that won't be calibrated
  params_fix       <- list() # i.e. none
  # Define calibration settings
  settings <- list(
    method  = "BayesianTools",
    par     = params_to_estimate,
    metric  = rsofun::cost_likelihood_pmodel,
    control = list(
      sampler = "DEzs",
      settings = list(
        burnin = 1,
        iterations = 4
      ),
      n_chains_independent   = 1, # 2,
      n_parallel_independent = 1  # 2, this can be parallelized
    )
  )

  # Run the calibration for GPP and D13C data
  pars <- rsofun::calib_sofun(
    drivers,
    obs,
    settings = settings,
    # extra arguments for the cost function
    par_fixed = params_fix,
    optim_out = FALSE
  )

  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test GPP/bigDelta13C calibration routine p-model (GenSA, rmse)", {
  skip_on_cran()
  # Calib 2: use GenSa optimization of RMSE
  # Calibrate the model and optimize the free parameters using demo datasets
  drivers_to_use <- pmodel_drivers
  obs_to_use     <- pmodel_validation

  params_fix <- list(
    kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    soilm_betao        = 0.01,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    # tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )
  # bounds for the parameter space
  params_to_estimate <- list(
    tau_acclim = list(lower = 7, upper = 60, init = 30)
  )
  settings_rmse <- list(
    method = "GenSA",                   # minimizes the RMSE
    metric = cost_rmse_pmodel,          # our cost function returning the RMSE
    control = list( # control parameters for optimizer GenSA
      maxit = 2),
    par = params_to_estimate
  )

  pars_calib_rmse <- calib_sofun(
    # calib_sofun arguments:
    drivers  = drivers_to_use,
    obs      = obs_to_use,
    settings = settings_rmse,
    # extra arguments passed to the cost function:
    par_fixed = params_fix
  )

  # test for correctly returned values
  expect_type(pars_calib_rmse, "list")
})
