context("test calibration framework and its parameters")

test_that("test calibration routine p-model", {
  skip_on_cran()
  
  df_drivers <- p_model_drivers
  ddf_obs <- p_model_validation
  
  settings <- list(
    method              = "bayesiantools",
    targetvars          = c("gpp"),
    timescale           = list(targets_obs = "y"),
    sitenames           = "FR-Pue",
    metric              = cost_rmse_kphio,
    dir_results         = "./",
    name                = "ORG",
    control = list(
      sampler = "DEzs",
      settings = list(
        burnin = 10,
        iterations = 50
      )
    ),
    par                 = list(kphio = list(lower=0.04, upper=0.09, init=0.05),
                               phiRL = list(lower=0.5, upper=5, init=3.5),
                               LAI_light = list(lower=2, upper=5, init=3.5),
                               tf_base = list(lower=0.5, upper=1.5, init=1),
                               par_mort = list(lower=0.1, upper=2, init=1))
  )
  
  pars <- calib_sofun(
    df_drivers = df_drivers,  
    ddf_obs = ddf_obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
  
  settings <- list(
    method              = "gensa",
    targetvars          = c("gpp"),
    timescale           = list(targets_obs = "y"),
    maxit               = 10,
    sitenames           = "FR-Pue",
    metric              = cost_rmse_kphio,
    dir_results         = "./",
    name                = "ORG",
    control = list(
      max.call = 10
    ),
    par                 = list(kphio = list(lower=0.04, upper=0.09, init=0.05),
                               phiRL = list(lower=0.5, upper=5, init=3.5),
                               LAI_light = list(lower=2, upper=5, init=3.5),
                               tf_base = list(lower=0.5, upper=1.5, init=1),
                               par_mort = list(lower=0.1, upper=2, init=1))
  )
  
  pars <- calib_sofun(
    df_drivers = df_drivers,  
    ddf_obs = ddf_obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test calibration routine p-model", {
  skip_on_cran()
  
  df_drivers <- p_model_drivers
  ddf_obs <- p_model_validation
  
  settings <- list(
    method              = "bayesiantools",
    targetvars          = c("gpp"),
    timescale           = list(targets_obs = "y"),
    sitenames           = "FR-Pue",
    metric              = cost_rmse_kphio,
    dir_results         = "./",
    name                = "ORG",
    control = list(
      sampler = "DEzs",
      settings = list(
        burnin = 10,
        iterations = 50
      )
    ),
    par                 = list(kphio = list(lower=0.04, upper=0.09, init=0.05),
                               phiRL = list(lower=0.5, upper=5, init=3.5),
                               LAI_light = list(lower=2, upper=5, init=3.5),
                               tf_base = list(lower=0.5, upper=1.5, init=1),
                               par_mort = list(lower=0.1, upper=2, init=1))
  )
  
  pars <- calib_sofun(
    df_drivers = df_drivers,  
    ddf_obs = ddf_obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
  
  settings <- list(
    method              = "gensa",
    targetvars          = c("gpp"),
    timescale           = list(targets_obs = "y"),
    sitenames           = "FR-Pue",
    metric              = cost_rmse_kphio,
    dir_results         = "./",
    name                = "ORG",
    control = list(
      max.call = 10
    ),
    par                 = list(kphio = list(lower=0.04, upper=0.09, init=0.05),
                               phiRL = list(lower=0.5, upper=5, init=3.5),
                               LAI_light = list(lower=2, upper=5, init=3.5),
                               tf_base = list(lower=0.5, upper=1.5, init=1),
                               par_mort = list(lower=0.1, upper=2, init=1))
  )
  
  pars <- calib_sofun(
    df_drivers = df_drivers,  
    ddf_obs = ddf_obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test calibration routine p-model", {
  skip_on_cran()
  
  df_drivers <- p_model_drivers
  ddf_obs <- p_model_validation
  
  settings <- list(
    method              = "bayesiantools",
    targetvars          = c("gpp"),
    timescale           = list(targets_obs = "y"),
    sitenames           = "FR-Pue",
    metric              = cost_rmse_kphio,
    dir_results         = "./",
    name                = "ORG",
    control = list(
      sampler = "DEzs",
      settings = list(
        burnin = 10,
        iterations = 50
      )
    ),
    par                 = list(kphio = list(lower=0.04, upper=0.09, init=0.05),
                               phiRL = list(lower=0.5, upper=5, init=3.5),
                               LAI_light = list(lower=2, upper=5, init=3.5),
                               tf_base = list(lower=0.5, upper=1.5, init=1),
                               par_mort = list(lower=0.1, upper=2, init=1))
  )
  
  pars <- calib_sofun(
    df_drivers = df_drivers,  
    ddf_obs = ddf_obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
  
  settings <- list(
    method              = "gensa",
    targetvars          = c("gpp"),
    timescale           = list(targets_obs = "y"),
    sitenames           = "FR-Pue",
    metric              = cost_rmse_kphio,
    dir_results         = "./",
    name                = "ORG",
    control = list(
      max.call = 10
    ),
    par                 = list(kphio = list(lower=0.04, upper=0.09, init=0.05),
                               phiRL = list(lower=0.5, upper=5, init=3.5),
                               LAI_light = list(lower=2, upper=5, init=3.5),
                               tf_base = list(lower=0.5, upper=1.5, init=1),
                               par_mort = list(lower=0.1, upper=2, init=1))
  )
  
  pars <- calib_sofun(
    df_drivers = df_drivers,  
    ddf_obs = ddf_obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test calibration routine lm3ppa", {
  skip_on_cran()
  
  df_drivers <- lm3ppa_gs_leuning_drivers
  ddf_obs <- lm3ppa_validation_2
  
  df_drivers$params_siml[[1]]$spinup <- TRUE
  df_drivers$params_siml[[1]]$spinupyears <- 10
  
  # Mortality as DBH
  settings <- list(
    method              = "bayesiantools",
    targetvars          = c("gpp"),
    timescale           = list(targets_obs = "y"),
    sitenames           = "CH-Lae",
    metric              = cost_rmse_lm3ppa_gsleuning,
    dir_results         = "./",
    name                = "ORG",
    control = list(
      sampler = "DEzs",
      settings = list(
        burnin = 10,
        iterations = 50
      )
    ),
    par                 = list(phiRL = list(lower=0.5, upper=5, init=3.5),
                               LAI_light = list(lower=2, upper=5, init=3.5),
                               tf_base = list(lower=0.5, upper=1.5, init=1),
                               par_mort = list(lower=0.1, upper=2, init=1))
  )
  
  pars <- calib_sofun(
    df_drivers = df_drivers,  
    ddf_obs = ddf_obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

