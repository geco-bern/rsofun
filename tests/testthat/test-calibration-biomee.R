context("test calibration framework and its parameters")
set.seed(10)

test_that("test calibration routine biomee (likelihood cost + Bayesiantools)", {
  skip_on_cran()
  df_drivers <- rsofun::biomee_gs_leuning_drivers
  ddf_obs <- rsofun::biomee_validation_2
  df_drivers$params_siml[[1]]$spinup <- FALSE
  
  settings <- list(
    method              = "bayesiantools",
    targets             = c("GPP","LAI","Density","Biomass"),
    metric              = rsofun::create_cost_likelihood_biomee(
      targets = c("GPP", "LAI", "Density", "Biomass")),
    control = list(
      sampler = "DEzs",
      settings = list(
        burnin = 1,
        iterations = 1000,
        nrChains = 1
      )
    ),
    par = list(
      phiRL = list(lower = 0.5, upper = 5, init = 3.5),
      LAI_light = list(lower = 2, upper = 5, init = 3.5),
      tf_base = list(lower = 0.1, upper = 1, init = 0.5),
      par_mort = list(lower = 1, upper = 2, init = 1.1),
      
      # uncertainties
      err_GPP = list(lower = 0, upper = 30, init = 15),
      err_LAI = list(lower = 0, upper = 5, init = 3),
      err_Density = list(lower = 0, upper = 400, init = 280),
      err_Biomass = list(lower = 0, upper = 50, init = 45)
    )
  )
  
  pars <- calib_sofun(
    drivers = df_drivers,
    obs = ddf_obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test calibration routine biomee (rmse cost + GenSA)", {
  skip_on_cran()
  df_drivers <- rsofun::biomee_gs_leuning_drivers
  ddf_obs <- rsofun::biomee_validation_2
  df_drivers$params_siml[[1]]$spinup <- FALSE
  
  settings <- list(
    method              = "gensa",
    targets             = c("GPP","LAI","Density","Biomass"),
    metric              = rsofun::create_cost_rmse_biomee(
      params_modl = list(
        phiRL = 2.5,
        LAI_light = 2.3,
        tf_base   = 0.3,
        par_mort  = 1.8
      ),
      setup = 'FULL',
      method = 'GenSA'),
    control = list(
      maxit = 100
    ),
    par = list(
      phiRL = list(lower = 0.5, upper = 5, init = 3.5),
      LAI_light = list(lower = 2, upper = 5, init = 3.5),
      tf_base = list(lower = 0.1, upper = 1, init = 0.5),
      par_mort = list(lower = 1, upper = 2, init = 1.1)
    )
  )
  
  pars <- calib_sofun(
    drivers = df_drivers,
    obs = ddf_obs,
    settings = settings
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})