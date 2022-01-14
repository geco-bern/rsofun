context("test calibration framework and its parameters")

test_that("test calibration routine p-model (BT)", {
  skip_on_cran()

  drivers <- p_model_drivers
  obs <- p_model_validation
  
  settings <- list(
    method              = "bayesiantools",
    targets             = c("gpp"),
    timescale           = list(targets_obs = "y"),
    sitenames           = "FR-Pue",
    metric              = likelihood_pmodel,
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

test_that("test calibration routine lm3ppa (gensa)", {
  skip_on_cran()

  df_drivers <- lm3ppa_gs_leuning_drivers
  ddf_obs <- lm3ppa_validation_2
  df_drivers$params_siml[[1]]$spinup <- FALSE

  # Mortality as DBH
  settings <- list(
    method              = "gensa",
    targetvars          = c("gpp"),
    timescale           = list(targets_obs = "y"),
    maxit               = 2,
    sitenames           = "CH-Lae",
    metric              = cost_rmse_lm3ppa_gsleuning,
    dir_results         = "./",
    name                = "ORG",
    control = list(
      max.call = 2
    ),
    par = list(
        phiRL = list(lower=0.5, upper=5, init=3.5),
        LAI_light = list(lower=2, upper=5, init=3.5),
        tf_base = list(lower=0.5, upper=1.5, init=1),
        par_mort = list(lower=0.1, upper=2, init=1))
  )

  pars <- calib_sofun(
    drivers = df_drivers,
    obs = ddf_obs,
    settings = settings
  )

  # test for correctly returned values
  expect_type(pars, "list")
})

