context("test calibration framework and its parameters")
set.seed(10)

test_that("test calibration routine p-model (BT)", {
  skip_on_cran()
  drivers <- p_model_drivers
  obs <- rsofun::p_model_validation
  
  settings <- list(
    method              = "bayesiantools",
    targets             = c("gpp"),
    sitenames           = "FR-Pue",
    metric              = rsofun::likelihood_pmodel,
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
      c = list(lower=2, upper=5, init=3.5),
      # the observed data estimated errors
      err_gpp = list(lower = 0, upper = 30, init = 15)
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
