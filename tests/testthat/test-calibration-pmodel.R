context("test calibration framework and its parameters")
set.seed(10)

test_that("test calibration routine p-model (BT, likelihood maximization)", {
  skip_on_cran()
  drivers <- p_model_drivers
  obs <- rsofun::p_model_validation
  params_fix <- list(
    kphio           = 0.04607080,
    soilm_par_a     = 2.75687824,
    soilm_par_b     = 1.68140444,
    tau_acclim_tempstress = 7.35259044,
    par_shape_tempstress  = 0.09863961
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

test_that("test calibration routine p-model (GenSA, rmse)", {
  skip_on_cran()
  drivers <- p_model_drivers
  obs <- rsofun::p_model_validation
  params_fix <- list(
    kphio           = 0.04607080,
    soilm_par_a     = 2.75687824,
    soilm_par_b     = 1.68140444,
    tau_acclim_tempstress = 7.35259044,
    par_shape_tempstress  = 0.09863961
  )
  
  settings <- list(
    method              = "gensa",
    targets             = c("gpp"),
    sitenames           = "FR-Pue",
    metric              = rsofun::create_cost_rmse_pmodel(params_modl = params_fix,
                                                                setup = "BRC",
                                                                method = "GenSA"),
    control = list(
      maxit = 100
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