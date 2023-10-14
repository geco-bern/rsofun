context("test models and their parameters")
set.seed(10)

test_that("p-model run check GPP", {
  skip_on_cran()
  
  # load parameters (valid ones)
  params_modl <- list(
    kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    soilm_betao        = 0.01,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )
  
  # read in demo data
  df_drivers <- p_model_drivers
  
  # run the SOFUN Fortran P-model
  mod <- run_pmodel_f_bysite( 
    df_drivers$sitename[1],
    df_drivers$params_siml[[1]],
    df_drivers$site_info[[1]],
    df_drivers$forcing[[1]], 
    params_modl = params_modl,
    makecheck = FALSE
  )
  
  # test if the returned values
  # are in a list (don't error / warning)
  expect_type(mod, "list")
  
  # test runread_pmodel_f
  df_output <- runread_pmodel_f(
    df_drivers,
    par = params_modl, 
    makecheck = FALSE,
    parallel = FALSE
  )

  # test for correctly returned values
  expect_type(df_output, "list")
  
  # test runread_pmodel_f
  df_output_p <- runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = TRUE
  )
  
  # test for correctly returned values
  expect_type(df_output_p, "list")
})

test_that("p-model run check Vcmax25", {
  skip_on_cran()
  
  # load parameters (valid ones)
  params_modl <- list(
    kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    soilm_betao        = 0.01,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )
  
  # read in demo data
  df_drivers <- p_model_drivers_vcmax25
  
  # run the SOFUN Fortran P-model
  mod <- run_pmodel_f_bysite( 
    df_drivers$sitename[1],
    df_drivers$params_siml[[1]],
    df_drivers$site_info[[1]],
    df_drivers$forcing[[1]], 
    params_modl = params_modl,
    makecheck = FALSE
  )
  
  # test if the returned values
  # are in a list (don't error / warning)
  expect_type(mod, "list")
  
  # test runread_pmodel_f
  df_output <- runread_pmodel_f(
    df_drivers,
    par = params_modl, 
    makecheck = FALSE,
    parallel = FALSE
  )
  
  # test for correctly returned values
  expect_type(df_output, "list")
  
  # test runread_pmodel_f
  df_output_p <- runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = TRUE,
    ncores = 1
  )
  
  # test for correctly returned values
  expect_type(df_output_p, "list")
})

test_that("biomee p-model run check", {
  skip_on_cran()

  df_drivers <- biomee_p_model_drivers
  df_drivers$params_siml[[1]]$spinup <- FALSE

  df_output <- runread_biomee_f(
    df_drivers
  )
  
  # test for correctly returned values
  expect_type(df_output, "list")
})

test_that("biomee leuning run check", {
  skip_on_cran()
  
  df_drivers <- biomee_gs_leuning_drivers
  df_drivers$params_siml[[1]]$spinup <- FALSE
  
  df_output <- runread_biomee_f(
    df_drivers,
    makecheck = FALSE,
    parallel = TRUE,
    ncores = 2
  )
  
  # test for correctly returned values
  expect_type(df_output, "list")
  
  df_output_p <- runread_biomee_f(
    df_drivers,
    makecheck = FALSE,
    parallel = TRUE,
    ncores = 1
  )
  
  # test for correctly returned values
  expect_type(df_output_p, "list")
  
})
