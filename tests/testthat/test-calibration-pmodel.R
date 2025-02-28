context("test P-model calibration framework and its parameters")
set.seed(10)

test_that("test GPP calibration routine p-model (BT, likelihood maximization)", {
  skip_on_cran()
  drivers <- rsofun::p_model_drivers_format2025_02 # TODO: NOT YET UPDATED FOR PHYDRO (still add default phydro_* parameters)
  drivers$params_siml[[1]]$use_gs     <- TRUE
  
  obs <- rsofun::p_model_validation
  params_fix <- list(
    # kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
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
        nrChains = 1,
        burnin = 1,
        iterations = 4
      )
    ),
    par = list(
      kphio   = list(lower = 0.04, upper = 0.09, init = 0.05),
      err_gpp = list(lower = 0.01, upper = 4,    init = 2)
    )
  )

  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    # extra arguments for the cost function
    par_fixed = params_fix,
    targets = c('gpp'),
    parallel = FALSE#TRUE,ncores = 2
  )
  # plot(pars$mod)
  # print(pars$mod)
  # summary(pars$mod)
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test GPP calibration routine p-model (GenSA, rmse, all params)", {
  skip_on_cran()
  drivers <- rsofun::p_model_drivers_format2025_02 # TODO: NOT YET UPDATED FOR PHYDRO (still add default phydro_* parameters)
  drivers$params_siml[[1]]$use_gs     <- TRUE
  obs <- rsofun::p_model_validation
  
  settings <- list(
    method              = "gensa",
    targets             = c("gpp"),
    sitenames           = "FR-Pue",
    metric              = rsofun::cost_rmse_pmodel,
    control = list(
      maxit = 2
    ),
    par = list(
      kphio = list(lower=0.04, upper=0.09, init=0.05),
      kphio_par_a = list(lower = 0, upper = 1, init = 0.2),
      kphio_par_b = list(lower = 10, upper = 40, init =25),
      soilm_thetastar = list(lower = 0, upper = 3000, init = 0.6*240),
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
    optim_out = FALSE,
    # extra arguments for the cost function
    par_fixed = list(),
    targets = 'gpp'
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test Vcmax25 calibration routine p-model (BT, likelihood, all params)", {
  skip_on_cran()
  drivers <- rsofun::p_model_drivers_vcmax25 |>
    # TODO: NOT YET UPDATED FOR PHYDRO
    # # specify additionally needed params_siml flags:
    dplyr::mutate(params_siml = purrr::map(params_siml, \(x)
                                    dplyr::mutate(x,
                                           use_pml    = TRUE,
                                           use_gs     = TRUE,
                                           use_phydro = FALSE))) |>
    # specify additionally needed site info:
    dplyr::mutate(site_info = purrr::map(site_info, \(x)
                                  dplyr::mutate(x,
                                         canopy_height = 5,
                                         reference_height = 10)))
  
  obs <- rsofun::p_model_validation_vcmax25
  
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
      kphio = list(lower=0.04, upper=0.09, init=0.05),
      kphio_par_a = list(lower = 0, upper = 1, init = 0.2),
      kphio_par_b = list(lower = 10, upper = 40, init =25),
      soilm_thetastar = list(lower = 0, upper = 3000, init = 0.6*240),
      # TODO: should we replace fitting sample_par$soilm_betao with sample_par$whc?
      beta_unitcostratio = list(lower = 50, upper = 200, init = 146),
      rd_to_vcmax = list(lower = 0.01, upper = 0.1, init = 0.014),
      tau_acclim = list(lower = 7, upper = 60, init = 30),
      kc_jmax = list(lower = 0.2, upper = 0.8, init = 0.41),
      err_vcmax25 = list(lower = 0.001, upper = 0.2, init = 0.1)
    )
  )
  
  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    optim_out = FALSE,
    # arguments for cost function
    par_fixed = list(), 
    targets = 'vcmax25'
  )
  # plot(pars$mod)
  # print(pars$mod)
  # summary(pars$mod)
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test Vcmax25 calibration routine p-model (GenSA, rmse)", {
  skip_on_cran()
  drivers <- rsofun::p_model_drivers_vcmax25 |>
    # TODO: NOT YET UPDATED FOR PHYDRO
    # # specify additionally needed params_siml flags:
    dplyr::mutate(params_siml = purrr::map(params_siml, \(x)
                                    dplyr::mutate(x,
                                           use_pml    = TRUE,
                                           use_gs     = TRUE,
                                           use_phydro = FALSE))) |>
    # specify additionally needed site info:
    dplyr::mutate(site_info = purrr::map(site_info, \(x)
                                  dplyr::mutate(x,
                                         canopy_height = 5,
                                         reference_height = 10)))
  
  obs <- rsofun::p_model_validation_vcmax25
  params_fix <- list(
    kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    # tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )
  
  settings <- list(
    method              = "gensa",
    metric              = rsofun::cost_rmse_pmodel,
    control = list(
      maxit = 2
    ),
    par = list(
      tau_acclim = list(lower = 7, upper = 60, init = 30)
    )
  )
  
  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    # arguments for cost function
    targets = 'vcmax25',
    par_fixed = params_fix,
    parallel = TRUE,
    ncores = 2
  )
  
  # test for correctly returned values
  expect_type(pars, "list")
})

test_that("test joint calibration routine p-model (BT, likelihood maximization)", {
  skip_on_cran()
  drivers <- rbind(
    gpp     = rsofun::p_model_drivers_format2025_02, # TODO: NOT YET UPDATED FOR PHYDRO (still add default phydro_* parameters)
    vcmax25 = rsofun::p_model_drivers_vcmax25  |>
      # TODO: NOT YET UPDATED FOR PHYDRO
      # # specify additionally needed params_siml flags:
      dplyr::mutate(params_siml = purrr::map(params_siml, \(x)
                                      dplyr::mutate(x,
                                             use_pml    = TRUE,
                                             use_gs     = TRUE,
                                             use_phydro = FALSE))) |>
      # specify additionally needed site info:
      dplyr::mutate(site_info = purrr::map(site_info, \(x)
                                    dplyr::mutate(x,
                                           canopy_height = 5,
                                           reference_height = 10))) |>
      dplyr::mutate(forcing_24h = forcing,
             forcing_daytime = forcing,
             forcing_3hrmax = forcing) # TODO: this is just to make it work
    )
  
  obs <- rbind(gpp = rsofun::p_model_validation,
              vcmax25 = rsofun::p_model_validation_vcmax25)
  params_fix <- list(
    # kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )
  
  settings <- list(
    method              = "bayesiantools",
    metric              = rsofun::cost_likelihood_pmodel,
    control = list(
      sampler = "DEzs",
      settings = list(
        nrChains = 1,
        burnin = 50,     # this was selected deliberately low for computational efficiency
        iterations = 200 # this was selected deliberately low for computational efficiency
      )
    ),
    par = list(
      kphio = list(lower=0.04, upper=0.09, init=0.05),
      err_gpp = list(lower = 0.01, upper = 4, init = 2),
      err_vcmax25 = list(lower = 0.0001, upper = 0.1, init = 0.005)
    )
  )
  set.seed(10)
  pars <- rsofun::calib_sofun(
    drivers = drivers,
    obs = obs,
    settings = settings,
    targets = c('gpp', 'vcmax25'),
    par_fixed = params_fix
  )
  # plot(pars$mod)
  # print(pars$mod)
  # summary(pars$mod)

  # test for correctly returned values
  expect_type(pars, "list")
  
  # test for same numeric results:
    # Hardcoded reference outputs.
    # NOTE: this is expected to change reasonably frequently whenever something is
    #       changed in the model.
    #       If this is expected, please update the hardcoded reference values below.
    #       To do so, simply use the commented code, making use of dput(). Thanks!
    # dput(pars$par)
  # print(dput(pars$par))
  ref_pars <- c(kphio       = 0.0453,
                err_gpp     = 1.51,
                err_vcmax25 = 0.0060)
  expect_equal(pars$par, ref_pars, tolerance = 0.1)
  
})

