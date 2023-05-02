context("test calibration framework and its parameters")
set.seed(10)

test_that("test GPP calibration routine p-model (BT, likelihood maximization)", {
  skip_on_cran()
  drivers <- p_model_drivers
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
    metric              = rsofun::create_cost_likelihood_pmodel(params_modl = params_fix,
                                                         setup = "FULL",
                                                         target = 'gpp'),
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
  
  pars <- calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test GPP calibration routine p-model (GenSA, rmse)", {
  skip_on_cran()
  drivers <- p_model_drivers
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
    metric              = rsofun::create_cost_rmse_pmodel(params_modl = params_fix,
                                                                setup = "BRC",
                                                                method = "GenSA",
                                                                target = 'gpp'),
    control = list(
      maxit = 10
    ),
    par = list(
      a = list(lower=0.04, upper=0.09, init=0.05),
      b = list(lower=0.5, upper=5, init=3.5),
      c = list(lower=2, upper=5, init=3.5)
    )
  )
  
  pars <- calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings
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
    metric              = rsofun::create_cost_rmse_pmodel(params_modl = params_fix,
                                                                setup = "FULL",
                                                                method = "BayesianTools",
                                                                target = 'vcmax25'),
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
  
  pars <- calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings
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
    metric              = rsofun::create_cost_likelihood_pmodel(params_modl = params_fix,
                                                                setup = "BRC",
                                                                target = 'vcmax25'),
    control = list(
      sampler = "DEzs",
      settings = list(
        nrChains = 1,
        burnin = 1,
        iterations = 4
      )
    ),
    par = list(
      a = list(lower=0.04, upper=0.09, init=0.05)
    )
  )
  
  pars <- calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test joint calibration routine p-model (BT, likelihood maximization)", {
  skip_on_cran()
  drivers <- list(gpp = rsofun::p_model_drivers, 
                  vcmax25 = rsofun::p_model_drivers_vcmax25[1:5, ])
  obs <- list(gpp = rsofun::p_model_validation,
              vcmax25 = rsofun::p_model_validation_vcmax25[1:5, ])
  params_fix <- list(
    kphio           = 0.04607080,
    soilm_par_a     = 2.75687824,
    soilm_par_b     = 1.68140444
  )
  
  settings <- list(
    method              = "bayesiantools",
    metric              = rsofun::create_cost_joint_likelihood_pmodel(
      params_modl = params_fix,
      setup = "BRC"
    ),
    control = list(
      sampler = "DEzs",
      settings = list(
        nrChains = 1,
        burnin = 1,
        iterations = 4
      )
    ),
    par = list(
      a = list(lower=0.04, upper=0.09, init=0.05)
    )
  )
  
  pars <- calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})