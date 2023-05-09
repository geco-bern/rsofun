context("test calibration framework and its parameters")
set.seed(10)

test_that("test GPP calibration routine p-model (BT, likelihood maximization)", {
  skip_on_cran()
  drivers <- rsofun::p_model_drivers
  obs <- rsofun::p_model_validation
  params_fix <- list(
    kphio           = 0.04607080,
    soilm_par_a     = 2.75687824,
    soilm_par_b     = 1.68140444
  )
  
  settings <- list(
    method              = "bayesiantools",
    targets             = c("gpp"),
    sitenames           = "FR-Pue",
    metric              = rsofun::cost_likelihood_pmodel,
    control = list(
      sampler = "DEzs",
      settings = list(
        nrChains = 1,
        burnin = 1,
        iterations = 4
      )
    ),
    par = list(
      kphio = list(lower=0.04, upper=0.09, init=0.05),
      soilm_par_a = list(lower=0.5, upper=5, init=3.5),
      soilm_par_b = list(lower=2, upper=5, init=3.5),
      err_gpp = list(lower = 0.01, upper = 4, init = 2)
    )
  )
  
  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    # extra arguments for the cost function
    setup = "FULL",
    targets = c('gpp')
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test GPP calibration routine p-model (GenSA, rmse)", {
  skip_on_cran()
  drivers <- rsofun::p_model_drivers
  obs <- rsofun::p_model_validation
  params_fix <- list(
    kphio           = 0.04607080,
    soilm_par_a     = 2.75687824,
    soilm_par_b     = 1.68140444
  )
  
  settings <- list(
    method              = "gensa",
    targets             = c("gpp"),
    sitenames           = "FR-Pue",
    metric              = rsofun::cost_rmse_pmodel,
    control = list(
      maxit = 10
    ),
    par = list(
      kphio = list(lower=0.04, upper=0.09, init=0.05)
    )
  )
  
  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    # extra arguments for the cost function
    setup = "BRC",
    par_fixed = params_fix,
    targets = 'gpp'
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test Vcmax25 calibration routine p-model (BT, rmse)", {
  skip_on_cran()
  drivers <- p_model_drivers_vcmax25[1:5, ]
  obs <- rsofun::p_model_validation_vcmax25[1:5, ]
  params_fix <- list(
    kphio           = 0.04607080,
    soilm_par_a     = 2.75687824,
    soilm_par_b     = 1.68140444
  )
  
  settings <- list(
    method              = "bayesiantools",
    metric              = rsofun::cost_rmse_pmodel,
    control = list(
      sampler = "DEzs",
      settings = list(
        nrChains = 1,
        burnin = 1,
        iterations = 4
      )
    ),
    par = list(
      a = list(lower=0.04, upper=0.09, init=0.05),
      b = list(lower=0.5, upper=5, init=3.5),
      c = list(lower=2, upper=5, init=3.5)
    )
  )
  
  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    # arguments for cost function
    setup = "FULL",
    targets = 'vcmax25'
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test Vcmax25 calibration routine p-model (BT, likelihood maximization)", {
  skip_on_cran()
  drivers <- p_model_drivers_vcmax25[1:5, ]
  obs <- rsofun::p_model_validation_vcmax25[1:5, ]
  params_fix <- list(
    kphio           = 0.04607080,
    soilm_par_a     = 2.75687824,
    soilm_par_b     = 1.68140444
  )
  
  settings <- list(
    method              = "bayesiantools",
    metric              = rsofun::cost_likelihood_pmodel,
    control = list(
      sampler = "DEzs",
      settings = list(
        nrChains = 1,
        burnin = 1,
        iterations = 4
      )
    ),
    par = list(
      a = list(lower=0.04, upper=0.09, init=0.05),
      b = list (lower = 0.001, upper = 4, init = 1)
    )
  )
  
  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    # arguments for cost function
    setup = 'BRC',
    targets = 'vcmax25',
    par_fixed = params_fix
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test joint calibration routine p-model (BT, likelihood maximization)", {
  skip_on_cran()
  drivers <- rbind(gpp = rsofun::p_model_drivers, 
                  vcmax25 = rsofun::p_model_drivers_vcmax25[1:5, ])
  obs <- rbind(gpp = rsofun::p_model_validation,
              vcmax25 = rsofun::p_model_validation_vcmax25[1:5, ])
  params_fix <- list(
    kphio           = 0.04607080,
    soilm_par_a     = 2.75687824,
    soilm_par_b     = 1.68140444
  )
  
  settings <- list(
    method              = "bayesiantools",
    metric              = rsofun::cost_likelihood_pmodel,
    control = list(
      sampler = "DEzs",
      settings = list(
        nrChains = 1,
        burnin = 1,
        iterations = 4
      )
    ),
    par = list(
      a = list(lower=0.04, upper=0.09, init=0.05),
      err_gpp = list(lower = 0.01, upper = 4, init = 2),
      err_vcmax = list(lower = 0.0001, upper = 0.1, init = 0.005)
    )
  )
  
  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    targets = c('gpp', 'vcmax25'),
    setup = "BRC",
    par_fixed = params_fix
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})
