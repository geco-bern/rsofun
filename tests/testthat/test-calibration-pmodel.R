context("test calibration framework and its parameters")

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
        burnin = 1,
        iterations = 4
        )
    ),
    par = list(
      kphio = list(lower=0.04, upper=0.09, init=0.05),
      phiRL = list(lower=0.5, upper=5, init=3.5),
      LAI_light = list(lower=2, upper=5, init=3.5),
      tf_base = list(lower=0.5, upper=1.5, init=1),
      par_mort = list(lower=0.1, upper=2, init=1),
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
