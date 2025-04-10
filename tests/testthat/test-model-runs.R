context("test models and their parameters")
set.seed(10)

test_that("biomee output check (p-model)", {
  skip_on_cran()

  out <- runread_biomee_f(
    biomee_p_model_drivers,
    makecheck = TRUE,
    parallel = FALSE)

  expect_true(all.equal(colMeans(out$data[[1]]$output_daily_tile), colMeans(biomee_p_model_output$data[[1]]$output_daily_tile), tolerance = 1e-4))
  expect_true(all.equal(colMeans(out$data[[1]]$output_annual_tile), colMeans(biomee_p_model_output$data[[1]]$output_annual_tile), tolerance = 1e-4))
  expect_true(all.equal(colMeans(out$data[[1]]$output_annual_cohorts), colMeans(biomee_p_model_output$data[[1]]$output_annual_cohorts), tolerance = 1e-4))

  # If this test fails it means that the output of the model is out of sync with the data in the data directory.
  # It could either mean that:
  # - the model was accidentally altered and should be fixed to deliver the expected output
  # - the model, drivers, or parameters was changed and the output data needs to be re-generetaed using the scripts in
  #   raw-data directory.
  #
  # Note: Biomee is quite sensitive with regards to which order cohorts are processed (ex: reduce stage when cohorts are merged together).
  # As a consequence, a slight change in the code may cause the final number of cohorts to be differents and therefore a large difference in the mean of the colunms of output_annual_aggregated.
  # These changes do not imply any meaningful alteration of the code functions and are simply artefacts.
})

test_that("biomee output check (p-model) with LULUC", {
  skip_on_cran()

  out <- runread_biomee_f(
    biomee_p_model_luluc_drivers,
    makecheck = TRUE,
    parallel = FALSE)

  expect_true(all.equal(colMeans(out$primary[[1]]$output_annual_tile), colMeans(biomee_p_model_luluc_output$primary[[1]]$output_annual_tile), tolerance = 1e-4))
  expect_true(all.equal(colMeans(out$secondary[[1]]$output_annual_tile), colMeans(biomee_p_model_luluc_output$secondary[[1]]$output_annual_tile), tolerance = 1e-4))
  expect_true(all.equal(colMeans(out$aggregated[[1]]), colMeans(biomee_p_model_luluc_output$aggregated[[1]]), tolerance = 1e-4))

  # If this test fails it means that the output of the model is out of sync with the data in the data directory.
  # It could either mean that:
  # - the model was accidentally altered and should be fixed to deliver the expected output
  # - the model, drivers, or parameters was changed and the output data needs to be re-generetaed using the scripts in
  #   raw-data directory.
  #
  # Note: Biomee is quite sensitive with regards to which order cohorts are processed (ex: reduce stage when cohorts are merged together).
  # As a consequence, a slight change in the code may cause the final number of cohorts to be differents and therefore a large difference in the mean of the colunms of output_annual_aggregated.
  # These changes do not imply any meaningful alteration of the code functions and are simply artefacts.
})

test_that("biomeE output check (gs leuning)", {
  skip_on_cran()

  out <- runread_biomee_f(
    biomee_gs_leuning_drivers,
    makecheck = TRUE,
    parallel = FALSE)

  expect_true(all.equal(colMeans(out$data[[1]]$output_daily_tile), colMeans(biomee_gs_leuning_output$data[[1]]$output_daily_tile), tolerance = 1e-4))
  expect_true(all.equal(colMeans(out$data[[1]]$output_annual_tile), colMeans(biomee_gs_leuning_output$data[[1]]$output_annual_tile), tolerance = 1e-4))
  expect_true(all.equal(colMeans(out$data[[1]]$output_annual_cohorts), colMeans(biomee_gs_leuning_output$data[[1]]$output_annual_cohorts), tolerance = 1e-4))

  # Cf comment above
})

test_that("biomee parallel run check (gs leuning)", {
  skip_on_cran()

  df_drivers <- biomee_p_model_drivers
  df_drivers$params_siml[[1]]$spinup <- FALSE

  df_output <- runread_biomee_f(
    df_drivers,
    makecheck = FALSE,
    parallel = TRUE,
    ncores = 2
  )

  # test for correctly returned values
  expect_type(df_output, "list")

})

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

test_that("biomeE output check (gs leuning)", {
  skip_on_cran()
  
  out <- runread_biomee_f(
    biomee_gs_leuning_drivers,
    makecheck = TRUE,
    parallel = FALSE)
  
  output_annual_tile <- out$data[[1]]$output_annual_tile
  
  expect_true(all.equal(colMeans(output_annual_tile), colMeans(biomee_gs_leuning_output$data[[1]]$output_annual_tile), tolerance = 1e-4))
  
  # If this test fails it means that the output of the model is out of sync with the data in the data directory.
  # It could either mean that:
  # - the model was accidentally altered and should be fixed to deliver the expected output
  # - the model, drivers, or parameters was changed and the output data needs to be re-generetaed using the scripts in
  #   raw-data directory.
})


test_that("biomee output check (p-model)", {
  skip_on_cran()
  
  out <- runread_biomee_f(
    biomee_p_model_drivers,
    makecheck = TRUE,
    parallel = FALSE)
  
  output_annual_tile <- out$data[[1]]$output_annual_tile
  
  expect_true(all.equal(colMeans(output_annual_tile), colMeans(biomee_p_model_output$data[[1]]$output_annual_tile), tolerance = 1e-4))
  
  # If this test fails it means that the output of the model is out of sync with the data in the data directory.
  # It could either mean that:
  # - the model was accidentally altered and should be fixed to deliver the expected output
  # - the model, drivers, or parameters was changed and the output data needs to be re-generetaed using the scripts in
  #   raw-data directory.
})

test_that("biomee parallel run check (gs leuning)", {
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
  
})


test_that("Regression tests run_biomee_f_bysite()", {
  skip_on_cran()
  
  # read in demo data
  df_drivers_BiomeE_Pmodel <- rsofun::biomee_p_model_drivers
  df_drivers_BiomeE_PLULUC <- rsofun::biomee_p_model_luluc_drivers
  df_drivers_BiomeE_gsLeun <- rsofun::biomee_gs_leuning_drivers
  
  df_drivers_BiomeE_PLULUC$params_siml[[1]]$daily_diagnostics <- TRUE
  
  # remove spinup that we can check initial conditions and transient phases
  df_drivers_BiomeE_Pmodel$params_siml[[1]]$spinupyears = 0
  df_drivers_BiomeE_PLULUC$params_siml[[1]]$spinupyears = 0
  df_drivers_BiomeE_gsLeun$params_siml[[1]]$spinupyears = 0
  df_drivers_BiomeE_Pmodel$params_siml[[1]]$nyeartrend = 251
  df_drivers_BiomeE_PLULUC$params_siml[[1]]$nyeartrend = 251
  df_drivers_BiomeE_gsLeun$params_siml[[1]]$nyeartrend = 251
  df_drivers_BiomeE_Pmodel$forcing[[1]] <- df_drivers_BiomeE_Pmodel$forcing[[1]] |>
    # repeat forcing and update dates
    list() |> rep(251) |> bind_rows(.id = "repeatedyear") |> 
    # While we could change the date of each row with below code, 
    # it is actually not needed since it is not read by run_biomee_f_bysite()
    # mutate(date = date + lubridate::years(as.numeric(repeatedyear) - 1)) |> 
    select(-repeatedyear)
  df_drivers_BiomeE_PLULUC$forcing[[1]] <- df_drivers_BiomeE_PLULUC$forcing[[1]] |>
    # repeat forcing and update dates
    list() |> rep(251) |> bind_rows(.id = "repeatedyear") |> 
    # While we could change the date of each row with below code, 
    # it is actually not needed since it is not read by run_biomee_f_bysite()
    # mutate(date = date + lubridate::years(as.numeric(repeatedyear) - 1)) |> 
    select(-repeatedyear)
  df_drivers_BiomeE_gsLeun$forcing[[1]] <- df_drivers_BiomeE_gsLeun$forcing[[1]] |>
    # repeat forcing and update dates
    list() |> rep(251) |> bind_rows(.id = "repeatedyear") |> 
    # While we could change the date of each row with below code, 
    # it is actually not needed since it is not read by run_biomee_f_bysite()
    # mutate(date = date + lubridate::years(as.numeric(repeatedyear) - 1)) |> 
    select(-repeatedyear)
  
  
  # check run_biomee_f_bysite()
  # run the SOFUN Fortran P-model using the internal function `run_biomee_f_bysite`
  mod_BiomeE_Pmodel <- run_biomee_f_bysite(
    sitename       = df_drivers_BiomeE_Pmodel$sitename[1],
    params_siml    = df_drivers_BiomeE_Pmodel$params_siml[[1]],
    site_info      = df_drivers_BiomeE_Pmodel$site_info[[1]],
    forcing        = df_drivers_BiomeE_Pmodel$forcing[[1]],
    params_tile    = df_drivers_BiomeE_Pmodel$params_tile[[1]],
    params_species = df_drivers_BiomeE_Pmodel$params_species[[1]],
    init_cohort    = df_drivers_BiomeE_Pmodel$init_cohort[[1]],
    init_soil      = df_drivers_BiomeE_Pmodel$init_soil[[1]],
    makecheck      = TRUE
  )
  mod_BiomeE_PLULUC <- run_biomee_f_bysite(
    sitename       = df_drivers_BiomeE_PLULUC$sitename[1],
    params_siml    = df_drivers_BiomeE_PLULUC$params_siml[[1]],
    site_info      = df_drivers_BiomeE_PLULUC$site_info[[1]],
    forcing        = df_drivers_BiomeE_PLULUC$forcing[[1]],
    params_tile    = df_drivers_BiomeE_PLULUC$params_tile[[1]],
    params_species = df_drivers_BiomeE_PLULUC$params_species[[1]],
    init_cohort    = df_drivers_BiomeE_PLULUC$init_cohort[[1]],
    init_soil      = df_drivers_BiomeE_PLULUC$init_soil[[1]],
    init_lu        = df_drivers_BiomeE_PLULUC$init_lu[[1]],
    luc_forcing    = df_drivers_BiomeE_PLULUC$luc_forcing[[1]],
    makecheck      = TRUE
  )
  mod_BiomeE_gsLeun <- run_biomee_f_bysite(
    sitename       = df_drivers_BiomeE_gsLeun$sitename[1],
    params_siml    = df_drivers_BiomeE_gsLeun$params_siml[[1]],
    site_info      = df_drivers_BiomeE_gsLeun$site_info[[1]],
    forcing        = df_drivers_BiomeE_gsLeun$forcing[[1]],
    params_tile    = df_drivers_BiomeE_gsLeun$params_tile[[1]],
    params_species = df_drivers_BiomeE_gsLeun$params_species[[1]],
    init_cohort    = df_drivers_BiomeE_gsLeun$init_cohort[[1]],
    init_soil      = df_drivers_BiomeE_gsLeun$init_soil[[1]],
    makecheck      = TRUE
  )

  # Rerun again (inverse order) to test memory leakage:
  mod_BiomeE_gsLeun_2ndTry <- run_biomee_f_bysite(
    sitename       = df_drivers_BiomeE_gsLeun$sitename[1],
    params_siml    = df_drivers_BiomeE_gsLeun$params_siml[[1]],
    site_info      = df_drivers_BiomeE_gsLeun$site_info[[1]],
    forcing        = df_drivers_BiomeE_gsLeun$forcing[[1]],
    params_tile    = df_drivers_BiomeE_gsLeun$params_tile[[1]],
    params_species = df_drivers_BiomeE_gsLeun$params_species[[1]],
    init_cohort    = df_drivers_BiomeE_gsLeun$init_cohort[[1]],
    init_soil      = df_drivers_BiomeE_gsLeun$init_soil[[1]],
    makecheck      = TRUE
  )
  mod_BiomeE_PLULUC_2ndTry <- run_biomee_f_bysite(
    sitename       = df_drivers_BiomeE_PLULUC$sitename[1],
    params_siml    = df_drivers_BiomeE_PLULUC$params_siml[[1]],
    site_info      = df_drivers_BiomeE_PLULUC$site_info[[1]],
    forcing        = df_drivers_BiomeE_PLULUC$forcing[[1]],
    params_tile    = df_drivers_BiomeE_PLULUC$params_tile[[1]],
    params_species = df_drivers_BiomeE_PLULUC$params_species[[1]],
    init_cohort    = df_drivers_BiomeE_PLULUC$init_cohort[[1]],
    init_soil      = df_drivers_BiomeE_PLULUC$init_soil[[1]],
    init_lu        = df_drivers_BiomeE_PLULUC$init_lu[[1]],
    luc_forcing    = df_drivers_BiomeE_PLULUC$luc_forcing[[1]],
    makecheck      = TRUE
  )
  mod_BiomeE_Pmodel_2ndTry <- run_biomee_f_bysite(
    sitename       = df_drivers_BiomeE_Pmodel$sitename[1],
    params_siml    = df_drivers_BiomeE_Pmodel$params_siml[[1]],
    site_info      = df_drivers_BiomeE_Pmodel$site_info[[1]],
    forcing        = df_drivers_BiomeE_Pmodel$forcing[[1]],
    params_tile    = df_drivers_BiomeE_Pmodel$params_tile[[1]],
    params_species = df_drivers_BiomeE_Pmodel$params_species[[1]],
    init_cohort    = df_drivers_BiomeE_Pmodel$init_cohort[[1]],
    init_soil      = df_drivers_BiomeE_Pmodel$init_soil[[1]],
    makecheck      = TRUE
  )
  
  # Testing if the returned values are in a list (don't error / warning)
  # a) expect data.frames()
  expect_type(mod_BiomeE_Pmodel, "list")
  expect_type(mod_BiomeE_PLULUC, "list")
  expect_type(mod_BiomeE_gsLeun, "list")
  expect_s3_class(mod_BiomeE_Pmodel$data$output_daily_tile, "data.frame")
  expect_s3_class(mod_BiomeE_Pmodel$data$output_annual_tile, "data.frame")
  expect_s3_class(mod_BiomeE_Pmodel$data$output_annual_cohorts, "data.frame")
  
  expect_s3_class(mod_BiomeE_PLULUC$primary$output_daily_tile, "data.frame")
  expect_s3_class(mod_BiomeE_PLULUC$primary$output_annual_tile, "data.frame")
  expect_s3_class(mod_BiomeE_PLULUC$primary$output_annual_cohorts, "data.frame")
  expect_s3_class(mod_BiomeE_PLULUC$secondary$output_daily_tile, "data.frame")
  expect_s3_class(mod_BiomeE_PLULUC$secondary$output_annual_tile, "data.frame")
  expect_s3_class(mod_BiomeE_PLULUC$secondary$output_annual_cohorts, "data.frame")
  
  expect_s3_class(mod_BiomeE_gsLeun$data$output_daily_tile, "data.frame")
  expect_s3_class(mod_BiomeE_gsLeun$data$output_annual_tile, "data.frame")
  expect_s3_class(mod_BiomeE_gsLeun$data$output_annual_cohorts, "data.frame")
  
  # b) expect no NA
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel$data$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel$data$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts))))
  
  expect_true(all(!is.na(tibble(mod_BiomeE_PLULUC$primary$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_PLULUC$secondary$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_PLULUC$primary$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_PLULUC$secondary$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_PLULUC$primary$output_annual_cohorts))))
  expect_true(all(!is.na(tibble(mod_BiomeE_PLULUC$secondary$output_annual_cohorts))))
  
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun$data$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun$data$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts))))
  
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel_2ndTry$data$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel_2ndTry$data$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel_2ndTry$data$output_annual_cohorts))))
  
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun_2ndTry$data$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun_2ndTry$data$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun_2ndTry$data$output_annual_cohorts))))

  # c) Testing memory leakage, i.e. repeatability
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_daily_tile    ), tibble(mod_BiomeE_Pmodel_2ndTry$data$output_daily_tile    ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_annual_tile   ), tibble(mod_BiomeE_Pmodel_2ndTry$data$output_annual_tile   ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts), tibble(mod_BiomeE_Pmodel_2ndTry$data$output_annual_cohorts), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_daily_tile    ), tibble(mod_BiomeE_gsLeun_2ndTry$data$output_daily_tile    ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_annual_tile   ), tibble(mod_BiomeE_gsLeun_2ndTry$data$output_annual_tile   ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts), tibble(mod_BiomeE_gsLeun_2ndTry$data$output_annual_cohorts), tolerance = 1e-6)
  
  expect_equal(tibble(mod_BiomeE_PLULUC$aggregated),                       tibble(mod_BiomeE_PLULUC_2ndTry$aggregated),                      tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$primary$output_daily_tile),        tibble(mod_BiomeE_PLULUC_2ndTry$primary$output_daily_tile),       tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$primary$output_daily_tile),        tibble(mod_BiomeE_PLULUC_2ndTry$primary$output_daily_tile),       tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$primary$output_annual_tile),       tibble(mod_BiomeE_PLULUC_2ndTry$primary$output_annual_tile),      tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$primary$output_annual_cohorts),    tibble(mod_BiomeE_PLULUC_2ndTry$primary$output_annual_cohorts),   tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$primary$output_annual_cohorts),    tibble(mod_BiomeE_PLULUC_2ndTry$primary$output_annual_cohorts),   tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$primary$output_annual_cohorts),    tibble(mod_BiomeE_PLULUC_2ndTry$primary$output_annual_cohorts),   tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$secondary$output_daily_tile),      tibble(mod_BiomeE_PLULUC_2ndTry$secondary$output_daily_tile),     tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$secondary$output_daily_tile),      tibble(mod_BiomeE_PLULUC_2ndTry$secondary$output_daily_tile),     tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$secondary$output_annual_tile),     tibble(mod_BiomeE_PLULUC_2ndTry$secondary$output_annual_tile),    tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$secondary$output_annual_cohorts),  tibble(mod_BiomeE_PLULUC_2ndTry$secondary$output_annual_cohorts), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$secondary$output_annual_cohorts),  tibble(mod_BiomeE_PLULUC_2ndTry$secondary$output_annual_cohorts), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_PLULUC$secondary$output_annual_cohorts),  tibble(mod_BiomeE_PLULUC_2ndTry$secondary$output_annual_cohorts), tolerance = 1e-6)
  
  # d) Testing numeric values against below reference values
  #    By hardcoding the outputs below:
  #       - any code changes to the numeric outputs must be reflected in below values
  #       - and thus such breaking changes are legibly tracked in the git history
  #
  # Hardcoded reference outputs.
  # NOTE: this is expected to change reasonably frequently whenever something is
  #       changed in the model.
  #       If this is expected, please update the hardcoded reference values below.
  #       To do so, simply use the commented code, making use of dput(). Thanks!
  # mod_BiomeE_Pmodel$data$output_daily_tile|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365)) |> dput()
  # mod_BiomeE_Pmodel$data$output_daily_tile|>filter(year==251, doy %in% c(1, 2, 180, 364, 365)) |> dput()
  # mod_BiomeE_Pmodel$data$output_annual_tile|>filter(           year %in% c(1, 2, 8, 9, 16, 251)) |> dput()
  # mod_BiomeE_Pmodel$data$output_annual_cohorts|>filter(year==  1) |> dput()
  # mod_BiomeE_Pmodel$data$output_annual_cohorts|>filter(year==  2) |> dput()
  # mod_BiomeE_Pmodel$data$output_annual_cohorts|>filter(year==251) |> dput()
  # mod_BiomeE_gsLeun$data$output_daily_tile|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365)) |> dput()
  # mod_BiomeE_gsLeun$data$output_daily_tile|>filter(year==251, doy %in% c(1, 2, 180, 364, 365)) |> dput()
  # mod_BiomeE_gsLeun$data$output_annual_tile|>filter(           year %in% c(1, 2, 8, 9, 16, 251)) |> dput()
  # mod_BiomeE_gsLeun$data$output_annual_cohorts|>filter(year==  1) |> dput()
  # mod_BiomeE_gsLeun$data$output_annual_cohorts|>filter(year==  2) |> dput()
  # mod_BiomeE_gsLeun$data$output_annual_cohorts|>filter(year==251) |> dput()
  ref_BiomeE_Pmodel_odt_yr1 <- tibble(
    year      = c(1,1,1,1,1),
    doy       = c(1,2,180,364,365),
    Tk        = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp      = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    totWs     = c(799.944641113281,799.947204589844,799.131042480469,799.930847167969,799.903991699219),
    Trsp      = c(0,0,0,0,0),
    Evap      = c(0.0553628616034985,0.0527696460485458,0.868921220302582,0.0691292807459831,0.0960086733102798),
    Runoff    = c(1.43654549121857,1.94845604896545,1.08043730258942,1.96995043754578,1.75696134567261),
    ws1       = c(19.944637298584,19.9472312927246,19.1310787200928,19.930871963501,19.9039936065674),
    ws2       = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3       = c(600,600,600,600,600),
    LAI       = c(0,0.000364852574421093,0.00466133235022426,0.00618919776752591,0.00619784602895379),
    GPP       = c(0,0,1.90892133105081e-05,6.17546766079613e-06,6.16563283983851e-06),
    Rauto     = c(2.97977797991678e-09,5.82730172027368e-05,4.93635025122785e-06,4.55608324045897e-06,4.57825399280409e-06),
    Rh        = c(2.77344884125341e-06,2.75237016467145e-06,1.03804013633635e-05,2.3680029244133e-06,2.36252321883512e-06),
    NSC       = c(0.00582691049203277,0.00565209938213229,0.00398708041757345,0.00424133753404021,0.00423404527828097),
    seedC     = c(0,0,0,0,0),
    leafC     = c(0,6.20249411440454e-05,0.000792426522821188,0.00105216365773231,0.00105363386683166),
    rootC     = c(0,3.70325360563584e-05,0.000473125197459012,0.000628203561063856,0.000629081332590431),
    SW_C      = c(0.00250000017695129,0.00163697078824043,0.000808653887361288,0.00114870828110725,0.00115068175364286),
    HW_C      = c(0,0.000880510022398084,0.00244240881875157,0.002835190622136,0.00283709052018821),
    NSN       = c(0.000293089426122606,0.000291046482743695,0.00025848179939203,0.000238109059864655,0.000237988890148699),
    seedN     = c(0,0,0,0,0),
    leafN     = c(0,1.06719380710274e-06,1.36344051497872e-05,1.81034265551716e-05,1.81287232408067e-05),
    rootN     = c(0,9.25813424146327e-07,1.18281122922781e-05,1.57050762936706e-05,1.57270224008244e-05),
    SW_N      = c(7.14285715730512e-06,4.67705922346795e-06,2.31044009524339e-06,3.28202372656961e-06,3.28766213897325e-06),
    HW_N      = c(0,2.5157430627587e-06,6.97831228535506e-06,8.10054189059883e-06,8.10597066447372e-06),
    McrbC     = c(3.69218014384387e-07,7.35389221517835e-07,9.97748793452047e-05,0.000234167644521222,0.000234328705118969),
    fastSOM   = c(0.009996865876019,0.00999375618994236,0.00931816641241312,0.00799902994185686,0.00799865275621414),
    slowSOM   = c(0.000999990850687027,0.000999981770291924,0.00105635263025761,0.00113423995207995,0.00113475287798792),
    McrbN     = c(3.6921800727896e-08,7.3538920730698e-08,9.97748793452047e-06,2.34167637245264e-05,2.34328708756948e-05),
    fastSoilN = c(0.000666457752231508,0.000666250416543335,0.000621211074758321,0.000531849800609052,0.000531776517163962),
    slowSoilN = c(2.49997719947714e-05,2.4999544621096e-05,2.58212767221266e-05,2.69752108579269e-05,2.69825650320854e-05),
    mineralN  = c(0.0149249145761132,0.0148505428805947,0.00587099697440863,0.00320566212758422,0.00321121863089502),
    N_uptk    = c(0,0,0,0,0))
  ref_BiomeE_Pmodel_odt_yr251 <- tibble(
    year      = c(251,251,251,251,251),
    doy       = c(1,2,180,364,365),
    Tk        = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp      = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    totWs     = c(799.960083007812,799.958984375,799.516296386719,799.951416015625,799.930053710938),
    Trsp      = c(0,0,0,0,0),
    Evap      = c(0.0399133116006851,0.0409998670220375,0.483673214912415,0.0485905706882477,0.0699304714798927),
    Runoff    = c(1.36660242080688,1.96390557289124,1.74325275421143,1.99354791641235,1.77749955654144),
    ws1       = c(19.9600887298584,19.9590015411377,19.5163269042969,19.9514083862305,19.9300708770752),
    ws2       = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3       = c(600,600,600,600,600),
    LAI       = c(3.49222874641418,3.49872827529907,3.61742806434631,3.74305081367493,3.74376726150513),
    GPP       = c(0.00227762688882649,0.00224546389654279,0.00936589110642672,0.00246698572300375,0.00246065924875438),
    Rauto     = c(6.12458461546339e-05,0.00236867531202734,0.00243186368606985,0.00197461596690118,0.00198433455079794),
    Rh        = c(0.00142561015672982,0.00141680834349245,0.00560735259205103,0.00138231716118753,0.00138092681299895),
    NSC       = c(2.14304661750793,2.13828349113464,2.130375623703,2.23058652877808,2.22722387313843),
    seedC     = c(0.00199680938385427,0.00217894976958632,0.0329008847475052,0.0699842646718025,0.0701815783977509),
    leafC     = c(0.593678891658783,0.594783842563629,0.614962697029114,0.636318624019623,0.636440515518188),
    rootC     = c(0.360928356647491,0.360858380794525,0.36590713262558,0.379916995763779,0.379989594221115),
    SW_C      = c(4.5676908493042,4.5533447265625,4.69937038421631,4.90116691589355,4.90223693847656),
    HW_C      = c(3.35169720649719,3.36794400215149,3.51899361610413,3.66643047332764,3.66720080375671),
    NSN       = c(0.061430923640728,0.061454962939024,0.0630296841263771,0.064000092446804,0.0639436542987823),
    seedN     = c(9.98404429992661e-05,0.000108947468106635,0.00164504407439381,0.0034992138389498,0.00350907910615206),
    leafN     = c(0.0102148456498981,0.0102338576689363,0.0105810556560755,0.0109485015273094,0.0109505960717797),
    rootN     = c(0.00902319513261318,0.00902144704014063,0.00914766173809767,0.00949790421873331,0.00949971843510866),
    SW_N      = c(0.0130505450069904,0.0130095556378365,0.013426773250103,0.0140033354982734,0.0140063911676407),
    HW_N      = c(0.00957628153264523,0.00962270237505436,0.0100542716681957,0.0104755219072104,0.0104777216911316),
    McrbC     = c(0.194571763277054,0.194574505090714,0.195704534649849,0.194694772362709,0.194691613316536),
    fastSOM   = c(2.17454314231873,2.17509269714355,2.18391823768616,2.01643490791321,2.01709246635437),
    slowSOM   = c(76.9140014648438,76.9135589599609,76.7412033081055,76.3388214111328,76.3383941650391),
    McrbN     = c(0.0194571763277054,0.0194574501365423,0.0195704530924559,0.0194694772362709,0.0194691605865955),
    fastSoilN = c(0.11321485042572,0.113226071000099,0.112775862216949,0.106537438929081,0.106550887227058),
    slowSoilN = c(1.50097978115082,1.50098061561584,1.50067782402039,1.49933409690857,1.49933516979218),
    mineralN  = c(0.00281932600773871,0.00275931577198207,0.00167982396669686,0.00281613017432392,0.0028474279679358),
    N_uptk    = c(0,9.1266272647772e-05,0,0,0))
  ref_BiomeE_Pmodel_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00416006147861481,0.0051026726141572,0.013795854523778,0.0158615466207266,0.0471739545464516,1.34490144252777),
    LAI             = c(0.00620649103075266,0.00972458533942699,0.043438233435154,0.0499434173107147,0.144030258059502,3.74448251724243),
    Density         = c(500,494.722686767578,462.535614013672,456.990539550781,1980.70007324219,46017.57421875),
    DBH             = c(0.67508590221405,0.77904599905014,1.58127772808075,1.74943602085114,1.17045211791992,1.0599946975708),
    Density12       = c(0,0,0,0,0,214.249755859375),
    DBH12           = c(0,0,0,0,0,21.6128597259521),
    QMD12           = c(0,0,0,0,0,21.6495342254639),
    NPP             = c(0.00230822968296707,0.00474032014608383,0.0180877950042486,0.0207991693168879,0.0605944767594337,1.43212604522705),
    GPP             = c(0.00443602073937654,0.00687614921480417,0.0263272896409035,0.0303885489702225,0.0890411883592606,2.20210695266724),
    Rauto           = c(0.00212779105640948,0.00213582930155098,0.00823949463665485,0.00958937965333462,0.028446713462472,0.769980907440186),
    Rh              = c(0.00236286479048431,0.00206716731190681,0.00342911528423429,0.00401711370795965,0.0118518937379122,1.29935908317566),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903991699219,799.904052734375,799.904724121094,799.904907226562,799.90673828125,799.930053710938),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.599700927734,159.441146850586,157.805053710938,157.495330810547,153.281402587891,94.2941207885742),
    Runoff          = c(428.025726318359,428.088317871094,429.724395751953,430.034057617188,434.247924804688,493.235260009766),
    plantC          = c(0.00990187004208565,0.0133129954338074,0.0604372769594193,0.0732633098959923,0.244614720344543,11.881664276123),
    soilC           = c(0.00937039777636528,0.0086310775950551,0.0202441606670618,0.0241893790662289,0.0789121612906456,78.5517883300781),
    plantN          = c(0.000283176312223077,0.000251700810622424,0.00107309536542743,0.00126743607688695,0.00361612509004772,0.112349756062031),
    soilN           = c(0.00379347242414951,0.00338095729239285,0.00351151009090245,0.00362917082384229,0.00529967527836561,1.62824010848999),
    totN            = c(0.00407664850354195,0.00363265816122293,0.00458460533991456,0.00489660678431392,0.00891580060124397,1.74058985710144),
    NSC             = c(0.00422516884282231,0.00518226251006126,0.0209579616785049,0.0248370189219713,0.0757390111684799,2.22338700294495),
    SeedC           = c(0,0,0.00153295940253884,0.00226585427299142,0.00386109319515526,0.0703785941004753),
    leafC           = c(0.0010551034938544,0.00165317952632904,0.00738449953496456,0.00849038176238537,0.0244851429015398,0.636562049388885),
    rootC           = c(0.000629958754871041,0.00098704535048455,0.00440898025408387,0.00506925676018,0.0146190682426095,0.380062103271484),
    SapwoodC        = c(0.00115265452768654,0.00201194896362722,0.0154777374118567,0.0192941222339869,0.0739047154784203,4.90330505371094),
    WoodC           = c(0.00283898459747434,0.00347855873405933,0.0106751378625631,0.0133066764101386,0.05200569704175,3.66797018051147),
    NSN             = c(0.000237868676776998,0.000182893098099157,0.000684443046338856,0.000788181787356734,0.00227655982598662,0.0638872385025024),
    SeedN           = c(0,0,7.66480443417095e-05,0.00011329265544191,0.000193054700503126,0.00351893017068505),
    leafN           = c(1.8154007193516e-05,2.84444759017788e-05,0.000127057341160253,0.000146085236337967,0.000421290373196825,0.0109526887536049),
    rootN           = c(1.57489575940417e-05,2.46760791924316e-05,0.000110224362288136,0.000126731349155307,0.000365475978469476,0.00950152985751629),
    SapwoodN        = c(3.29329850501381e-06,5.74842533751507e-06,4.42221098637674e-05,5.51260636711959e-05,0.000211156337172724,0.0140094431117177),
    WoodN           = c(8.11138215794927e-06,9.93873254628852e-06,3.05004105030093e-05,3.80190831492655e-05,0.00014858775830362,0.0104799186810851),
    McrbC           = c(0.000234328705118969,0.000333363102981821,0.00064427312463522,0.000751479179598391,0.00220471736975014,0.194691613316536),
    fastSOM         = c(0.00800079107284546,0.00688059162348509,0.012499668635428,0.0145922340452671,0.0426619611680508,2.01838397979736),
    SlowSOM         = c(0.00113527732901275,0.00141712243203074,0.00710021937265992,0.0088456654921174,0.0340454839169979,76.3387145996094),
    McrbN           = c(2.34328708756948e-05,3.33363095705863e-05,6.44273095531389e-05,7.51479165046476e-05,0.000220471731154248,0.0194691605865955),
    fastSoilN       = c(0.000531830999534577,0.000451567728305236,0.000561318185646087,0.000647874025162309,0.00186739815399051,0.106583803892136),
    slowSoilN       = c(2.69899937848095e-05,3.19643804687075e-05,0.000138284027343616,0.000170996616361663,0.000630303169600666,1.49933969974518),
    mineralN        = c(0.00321121863089502,0.00286408886313438,0.00274748052470386,0.00273515214212239,0.00258150254376233,0.0028474279679358),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000328926253132522,0.000375810253899544,0.00102232070639729,0.0221474543213844),
    N_yrMin         = c(-0.0117887742817402,-0.000347130466252565,0.000317916361382231,0.000363483210094273,0.00100469228345901,0.0222071725875139),
    N_P2S           = c(2.14073552342597e-05,3.33384523401037e-05,0.000169757404364645,0.000197003231733106,0.000581549596972764,0.0272013004869223),
    N_loss          = c(0.0219152644276619,0.010445348918438,0.00973915681242943,0.0096988333389163,0.00916272588074207,0.00924919173121452),
    totseedC        = c(0,0,0,0,0.00356994615867734,0.0649071559309959),
    totseedN        = c(0,0,0,0,0.000178497357410379,0.00324535788968205),
    Seedling_C      = c(0,0,0,0,0.00356994615867734,0.0649071559309959),
    Seedling_N      = c(0,0,0,0,0.000178497342858464,0.0032453581225127),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627334594727),
    MaxVolume       = c(0.000215764259337448,0.000299950072076172,0.00152817298658192,0.0019280546111986,0.00737557932734489,0.652382910251617),
    MaxDBH          = c(0.00675085838884115,0.00779045978561044,0.0158127769827843,0.0174943599849939,0.0313498750329018,0.220105022192001),
    NPPL            = c(0.00121877959463745,0.0008825832628645,0.00248827692121267,0.002812419552356,0.00808483455330133,0.169744521379471),
    NPPW            = c(0.00149163894820958,0.00154099601786584,0.00566299306228757,0.00676144752651453,0.02101119607687,0.651887118816376),
    n_deadtrees     = c(4.3513900891412e-06,4.85187956655864e-06,2.36964751820778e-05,2.83990775642451e-05,9.53025082708336e-05,0.0140079222619534),
    c_deadtrees     = c(0.00010314847168047,0.0001401223562425,0.000713716843165457,0.000889366143383086,0.0035840324126184,0.899706423282623),
    m_turnover      = c(0.00010314847168047,0.0001401223562425,0.000713716843165457,0.000889366143383086,0.0035840324126184,0.899706423282623),
    c_turnover_time = c(1.90326523780823,2.25734448432922,1.88506996631622,1.96802186965942,2.47514224052429,5.62669515609741),
    lu_fraction     = c(1,1,1,1,1,1))
  ref_BiomeE_Pmodel_oac_yr1 <- tibble(
    cohort      = 1,
    year        = 1,
    cID         = 1,
    PFT         = 2,
    layer       = 1,
    density     = 500,
    flayer      = 0.00416006147861481,
    DBH         = 0.675085842609406,
    dDBH        = 0.124270185828209,
    height      = 2.95789003372192,
    age         = 1.00000047683716,
    BA          = 3.57938079105224e-05,
    dBA         = 1.19649921543896e-05,
    Acrown      = 0.0832012295722961,
    Aleaf       = 0.124129816889763,
    nsc         = 0.0845033749938011,
    seedC       = 0.00475737359374762,
    leafC       = 0,
    rootC       = 0.0211020689457655,
    sapwC       = 0.0125991748645902,
    woodC       = 0.0230530891567469,
    nsn         = 0.0567796900868416,
    treeG       = 0.0781993493437767,
    fseed       = 0,
    fleaf       = 0.311710923910141,
    froot       = 0.306792557239532,
    fwood       = 0.381496518850327,
    GPP         = 0.0887204110622406,
    NPP         = 0.0461645908653736,
    Rauto       = 0.042555820196867,
    Nupt        = 0,
    Nfix        = 0,
    deathrate   = 0.0105546750128269,
    n_deadtrees = 4.3513900891412e-06,
    c_deadtrees = 0.00010314847168047)
  ref_BiomeE_Pmodel_oac_yr2 <- tibble(
    cohort      = 1,
    year        = 2,
    cID         = 1,
    PFT         = 2,
    layer       = 1,
    density     = 494.722686767578,
    flayer      = 0.0051026726141572,
    DBH         = 0.77904599905014,
    dDBH        = 0.103960141539574,
    height      = 3.17748880386353,
    age         = 1.99997925758362,
    BA          = 4.76668064948171e-05,
    dBA         = 1.18729985842947e-05,
    Acrown      = 0.103142082691193,
    Aleaf       = 0.196566388010979,
    nsc         = 0.104750856757164,
    seedC       = 0.00369688123464584,
    leafC       = 0,
    rootC       = 0.033416286110878,
    sapwC       = 0.0199514869600534,
    woodC       = 0.0406682156026363,
    nsn         = 0.0703133046627045,
    treeG       = 0.0755703151226044,
    fseed       = 0,
    fleaf       = 0.236070990562439,
    froot       = 0.35174748301506,
    fwood       = 0.412181466817856,
    GPP         = 0.138989970088005,
    NPP         = 0.0958177149295807,
    Rauto       = 0.0431722514331341,
    Nupt        = 0,
    Nfix        = 0,
    deathrate   = 0.0106876138597727,
    n_deadtrees = 4.85187956655864e-06,
    c_deadtrees = 0.0001401223562425)
  ref_BiomeE_Pmodel_oac_yr251 <- tibble(
    cohort      = c(2,3,1,4,5,6),
    year        = c(251,251,251,251,251,251),
    cID         = c(399,405,453,451,433,25),
    PFT         = c(2,2,2,2,2,2),
    layer       = c(1,1,1,2,2,2),
    density     = c(194.83171081543,19.4180297851562,6186.80615234375,1437.65539550781,133.359893798828,38045.50390625),
    flayer      = c(0.301783740520477,0.0215486716479063,0.752436757087708,0.137903496623039,0.00330994161777198,0.12791895866394),
    DBH         = c(22.0105018615723,17.6231174468994,4.03591012954712,3.44524002075195,1.39895117282867,0.369000434875488),
    dDBH        = c(0.415582954883575,0.391332805156708,0.24162270128727,0.0729762017726898,0.00261068344116211,0.0290364492684603),
    height      = c(16.8895263671875,15.1127624511719,7.2322473526001,6.68208932876587,4.25798177719116,2.18683457374573),
    age         = c(77.7627258300781,77.7627334594727,25.6355724334717,25.635570526123,22.6215229034424,2.81209373474121),
    BA          = c(0.0380495749413967,0.0243924465030432,0.00127930147573352,0.000932242430280894,0.00015370748587884,1.06940851765103e-05),
    dBA         = c(0.00142327323555946,0.00107127241790295,0.000148593680933118,3.90747445635498e-05,5.73156285099685e-07,1.61680509336293e-06),
    Acrown      = c(15.4894561767578,11.0972499847412,1.21619582176208,0.959224998950958,0.248196184635162,0.0336226224899292),
    Aleaf       = c(48.7879486083984,34.9530982971191,3.83005237579346,1.51069128513336,0.0880051031708717,0.036306157708168),
    nsc         = c(35.3928833007812,24.9768371582031,2.28830218315125,0.331827044487,0.000859242165461183,0.00575008522719145),
    seedC       = c(0.76550430059433,0.54928994178772,0.0601722933351994,0.0238187629729509,0.00613935943692923,0.00188525673002005),
    leafC       = c(1.15043652057648,0.80964070558548,0.0717583149671555,0.0115929879248142,0.0247553531080484,0),
    rootC       = c(8.2939510345459,5.94202709197998,0.651108920574188,0.256817519664764,0.0149608682841063,0.00617204699665308),
    sapwC       = c(4.95197582244873,3.54773926734924,0.388750284910202,0.153335139155388,0.00871927756816149,0.00368507485836744),
    woodC       = c(142.90104675293,85.6981048583984,2.88789677619934,1.03218019008636,0.129873126745224,0.00418471964076161),
    nsn         = c(98.4806365966797,59.0609397888184,1.99099254608154,2.35829377174377,0.296707808971405,0.0157136470079422),
    treeG       = c(19.4165687561035,13.7977523803711,1.38240838050842,0.350733578205109,0.0369470864534378,0.0105907050892711),
    fseed       = c(0.059250246733427,0.058679174631834,0.0519081875681877,0.0330535434186459,0.670021772384644,0),
    fleaf       = c(0.0987088829278946,0.101414486765862,0.134158194065094,0.0458624847233295,0.0212229881435633,0.313251167535782),
    froot       = c(0.308788985013962,0.311794072389603,0.346759855747223,0.45660537481308,0.259258568286896,0.363957613706589),
    fwood       = c(0.533251881599426,0.528112292289734,0.46717369556427,0.464478641748428,0.0494967177510262,0.322791159152985),
    GPP         = c(31.5417327880859,22.540979385376,2.39950299263,0.247082218527794,0.0197645165026188,0.00617480790242553),
    NPP         = c(20.4554824829102,14.665210723877,1.60801732540131,0.0583550482988358,0.012063343077898,0.000449975952506065),
    Rauto       = c(11.0862493515015,7.87576818466187,0.791485667228699,0.188727170228958,0.00770117342472076,0.00572483194991946),
    Nupt        = c(0.290791153907776,0.208818271756172,0.0258084423840046,0.000735382083803415,0.000264835951384157,0),
    Nfix        = c(0,0,0,0,0,0),
    deathrate   = c(0.113263040781021,0.0839816629886627,0.0181079730391502,0.152398899197578,0.266000390052795,0.351924955844879),
    n_deadtrees = c(0.00616554636508226,0.000313321856083348,0.00199735490605235,0.00161606096662581,3.86729298043065e-05,0.0038769650273025),
    c_deadtrees = c(0.640294253826141,0.029240844771266,0.0918554291129112,0.0901064053177834,0.00168161757756025,0.0465278774499893))
  ref_BiomeE_gsLeun_odt_yr1 <- tibble(
    year      = c(1,1,1,1,1),
    doy       = c(1,2,180,364,365),
    Tk        = c(273.533660888672,271.508483886719,290.34521484375,271.626373291016,273.387603759766),
    Prcp      = c(1.43654537200928,2.00381803512573,2.4158182144165,2.05754518508911,1.8260909318924),
    totWs     = c(799.998291015625,799.998168945312,799.987243652344,799.997924804688,799.997619628906),
    Trsp      = c(0,1.49713048358535e-06,0.00434475671499968,6.02663967583794e-05,0.000113291425805073),
    Evap      = c(0.0556573569774628,0.0521354898810387,0.931808114051819,0.070465937256813,0.100166037678719),
    Runoff    = c(1.38256895542145,1.95178186893463,0.626788854598999,1.98649656772614,1.72622036933899),
    ws1       = c(19.9983215332031,19.9982204437256,19.9872646331787,19.9979648590088,19.997652053833),
    ws2       = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3       = c(600,600,600,600,600),
    LAI       = c(0,0.000364852749044076,0.00462460890412331,0.00595784047618508,0.00596493016928434),
    GPP       = c(0,6.26106597678699e-08,1.37182714752271e-05,4.56526322523132e-07,1.09515008261951e-06),
    Rauto     = c(2.98780267193877e-09,5.82782486162614e-05,5.65045684197685e-06,3.59244927494728e-06,3.69437634617498e-06),
    Rh        = c(2.7720184334612e-06,2.75420779871638e-06,1.03837501228554e-05,2.36792357100057e-06,2.36069990933174e-06),
    NSC       = c(0.00582691328600049,0.00565216084942222,0.00336788222193718,0.00269267871044576,0.00268313591368496),
    seedC     = c(0,0,0,0,0),
    leafC     = c(0,6.20249702478759e-05,0.000786183518357575,0.0010128328576684,0.00101403810549527),
    rootC     = c(0,3.70325542462524e-05,0.000469397753477097,0.000604720728006214,0.000605440291110426),
    SW_C      = c(0.00250000017695129,0.00163697078824043,0.000798859749920666,0.00108340277802199,0.00108491943683475),
    HW_C      = c(0,0.000880510022398084,0.00241249590180814,0.0026732471305877,0.00267418939620256),
    NSN       = c(0.000293089426122606,0.000291046482743695,0.000258813262917101,0.000240256078541279,0.000240150795434602),
    seedN     = c(0,0,0,0,0),
    leafN     = c(0,1.06719437553693e-06,1.35269801830873e-05,1.7426687918487e-05,1.74474243976874e-05),
    rootN     = c(0,9.2581376520684e-07,1.17349400170497e-05,1.51180129250861e-05,1.51360018207924e-05),
    SW_N      = c(7.14285715730512e-06,4.67705922346795e-06,2.28245676225924e-06,3.0954361136537e-06,3.09976962853398e-06),
    HW_N      = c(0,2.5157430627587e-06,6.89284070176654e-06,7.63785374147119e-06,7.64054584578844e-06),
    McrbC     = c(3.68914641057927e-07,7.35217042802105e-07,9.97279857983813e-05,0.00023395313473884,0.000234114078921266),
    fastSOM   = c(0.00999687053263187,0.00999375898391008,0.00931745953857899,0.00799086131155491,0.00799041334539652),
    slowSOM   = c(0.00099999166559428,0.000999983283691108,0.00105622306000441,0.00113227101974189,0.00113276392221451),
    McrbN     = c(3.68914641057927e-08,7.35217042802105e-08,9.97279857983813e-06,2.33953142014798e-05,2.34114086197224e-05),
    fastSoilN = c(0.000666458043269813,0.000666250591166317,0.000621163984760642,0.000531450554262847,0.000531375815626234),
    slowSoilN = c(2.49997920036549e-05,2.49995828198735e-05,2.58189174928702e-05,2.69468200713163e-05,2.6953917767969e-05),
    mineralN  = c(0.013301195576787,0.0117978919297457,0.0002205292112194,0.000225525363930501,0.000225552576011978),
    N_uptk    = c(0,0,0,0,0))
  ref_BiomeE_gsLeun_odt_yr251 <- tibble(
    year      = c(251,251,251,251,251),
    doy       = c(1,2,180,364,365),
    Tk        = c(273.533660888672,271.508483886719,290.34521484375,271.626373291016,273.387603759766),
    Prcp      = c(1.43654537200928,2.00381803512573,2.4158182144165,2.05754518508911,1.8260909318924),
    totWs     = c(772.028015136719,773.965087890625,734.197143554688,765.2724609375,766.966064453125),
    Trsp      = c(0.0336724147200584,0.0256269779056311,2.63298463821411,0.0330498181283474,0.0602992475032806),
    Evap      = c(0.0405291430652142,0.041136659681797,0.506393134593964,0.0494572818279266,0.0719896703958511),
    Runoff    = c(0,0,0,0,0),
    ws1       = c(19.9981021881104,19.9979820251465,19.9848365783691,19.9976768493652,19.997314453125),
    ws2       = c(179.9990234375,179.998977661133,145.755126953125,179.998764038086,179.998565673828),
    ws3       = c(572.030883789062,573.968139648438,568.457153320312,565.276000976562,566.97021484375),
    LAI       = c(3.17385053634644,3.17505621910095,2.96196961402893,3.32277703285217,3.32116079330444),
    GPP       = c(0.000514672428835183,0.000216541229747236,0.00816829968243837,0.000230770980124362,0.000548396143130958),
    Rauto     = c(0.000122513782116584,0.000825319089926779,0.00337915029376745,0.000533785903826356,0.000583853456191719),
    Rh        = c(0.000783492461778224,0.000780773989390582,0.00316037237644196,0.000775871332734823,0.000774720567278564),
    NSC       = c(0.0960038751363754,0.0938681289553642,0.23992320895195,0.105098225176334,0.104145802557468),
    seedC     = c(0.000521281093824655,0.000532967096660286,0.00329699972644448,0.00936300028115511,0.00937547907233238),
    leafC     = c(0.539554536342621,0.539759516716003,0.503534972667694,0.564872026443481,0.564597308635712),
    rootC     = c(0.321693032979965,0.321416169404984,0.298694550991058,0.334000617265701,0.333609193563461),
    SW_C      = c(4.03481388092041,4.03196668624878,4.10744714736938,4.23716974258423,4.23724794387817),
    HW_C      = c(2.90424752235413,2.90732026100159,2.96926951408386,3.06195759773254,3.06203436851501),
    NSN       = c(0.00387309351935983,0.00385869620367885,0.0037118443287909,0.00392623618245125,0.00392232090234756),
    seedN     = c(2.60640535998391e-05,2.66483530140249e-05,0.000164849960128777,0.000468149839434773,0.000468773767352104),
    leafN     = c(0.00928351562470198,0.00928704161196947,0.00866376608610153,0.0097191222012043,0.00971439387649298),
    rootN     = c(0.00804232433438301,0.00803540181368589,0.00746735604479909,0.00835000909864902,0.00834022276103497),
    SW_N      = c(0.0115280393511057,0.011519905179739,0.011735562235117,0.0121061997488141,0.0121064223349094),
    HW_N      = c(0.00829772558063269,0.00830650608986616,0.00848350767046213,0.00874833483248949,0.00874855183064938),
    McrbC     = c(0.116997882723808,0.116998240351677,0.11753498762846,0.11704309284687,0.11704221367836),
    fastSOM   = c(1.39589846134186,1.39655709266663,1.43743860721588,1.37191891670227,1.37262809276581),
    slowSOM   = c(35.9507446289062,35.9507369995117,35.893726348877,35.7258644104004,35.7258682250977),
    McrbN     = c(0.0116997882723808,0.0116998236626387,0.0117534985765815,0.011704308912158,0.011704221367836),
    fastSoilN = c(0.0695651769638062,0.069581612944603,0.0706783831119537,0.0686835572123528,0.0687012448906898),
    slowSoilN = c(0.696448504924774,0.696449398994446,0.696558713912964,0.696262776851654,0.69626384973526),
    mineralN  = c(0.000202503710170276,0.000203007861273363,0.000181513591087423,0.000201431088498794,0.000201760674826801),
    N_uptk    = c(1.52307702592225e-05,1.5113606423256e-05,5.51313569303602e-05,1.55996094690636e-05,1.55702146003023e-05))
  ref_BiomeE_gsLeun_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00400206865742803,0.00448572542518377,0.00750343827530742,0.00824577640742064,0.015996016561985,1.11649715900421),
    LAI             = c(0.00597201427444816,0.00855128560215235,0.0236289892345667,0.0259668566286564,0.0479755438864231,3.31954503059387),
    Density         = c(500,494.733184814453,463.607696533203,458.471374511719,874.311767578125,11093.77734375),
    DBH             = c(0.65788322687149,0.714902698993683,1.05199158191681,1.12863123416901,1.0255788564682,2.71105289459229),
    Density12       = c(0,0,0,0,0,78.2982482910156),
    DBH12           = c(0,0,0,0,0,18.7279586791992),
    QMD12           = c(0,0,0,0,0,18.7279586791992),
    NPP             = c(0.000455248868092895,0.00205271318554878,0.00669513875618577,0.00733748869970441,0.0132791725918651,0.892530381679535),
    GPP             = c(0.00266986410133541,0.00399639410898089,0.0115679241716862,0.0127315586432815,0.0236653909087181,1.6037632226944),
    Rauto           = c(0.00221461523324251,0.00194368103984743,0.0048727854155004,0.00539406994357705,0.010386218316853,0.711232841014862),
    Rh              = c(0.00236317655071616,0.00205406406894326,0.00225871428847313,0.00246398570016026,0.00459936168044806,0.727962970733643),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.997619628906,799.997619628906,799.997619628906,799.997619628906,799.997619628906,766.966064453125),
    Transp          = c(0.704922735691071,1.0637092590332,3.14139652252197,3.45765209197998,6.422682762146,432.322967529297),
    Evap            = c(176.998413085938,176.84342956543,175.920715332031,175.784408569336,174.523239135742,101.844375610352),
    Runoff          = c(409.847991943359,409.645812988281,408.499145507812,408.317230224609,406.602905273438,57.0608177185059),
    plantC          = c(0.00805916078388691,0.00890088453888893,0.0226384159177542,0.0258787497878075,0.0573301501572132,8.30959415435791),
    soilC           = c(0.00935985427349806,0.00851541943848133,0.0132362861186266,0.0148639548569918,0.0316112264990807,37.2169494628906),
    plantN          = c(0.000283414934528992,0.000254236190812662,0.000544839538633823,0.00060827867127955,0.0011718823807314,0.0432677865028381),
    soilN           = c(0.000807353353593498,0.000738562841434032,0.000750088249333203,0.000797824701294303,0.00134164839982986,0.776903927326202),
    totN            = c(0.00109076825901866,0.000992799061350524,0.00129492778796703,0.00140610337257385,0.00251353066414595,0.820171713829041),
    NSC             = c(0.00267619988881052,0.00207317643798888,0.0055290344171226,0.00627270294353366,0.0132943727076054,0.103241950273514),
    SeedC           = c(0,0,0.000427327468059957,0.000620634760707617,0.000398915872210637,0.00938774738460779),
    leafC           = c(0.00101524242199957,0.00145371851976961,0.00401692790910602,0.00441436562687159,0.00815584324300289,0.564322710037231),
    rootC           = c(0.00060615933034569,0.000867955386638641,0.00239834189414978,0.00263563543558121,0.00486951740458608,0.333208173513412),
    SapwoodC        = c(0.00108643516432494,0.00165165890939534,0.0060768686234951,0.00706456881016493,0.0178665891289711,4.2373251914978),
    WoodC           = c(0.00267512421123683,0.00285437563434243,0.00418991595506668,0.00487084407359362,0.0127449110150337,3.06210851669312),
    NSN             = c(0.000240045483224094,0.000194650419871323,0.000365065789083019,0.000401301484089345,0.000802407972514629,0.00390312308445573),
    SeedN           = c(0,0,2.13663643080508e-05,3.10317336698063e-05,1.99457972485106e-05,0.000469387188786641),
    leafN           = c(1.74681463249726e-05,2.50125358434161e-05,6.91151726641692e-05,7.59534523240291e-05,0.000140329400892369,0.00970966927707195),
    rootN           = c(1.5153977983573e-05,2.16988610191038e-05,5.99584927840624e-05,6.58908247714862e-05,0.000121737823064905,0.00833019893616438),
    SapwoodN        = c(3.10409996018279e-06,4.71902512799716e-06,1.736248304951e-05,2.01844832190545e-05,5.10473946633283e-05,0.0121066430583596),
    WoodN           = c(7.64321612223284e-06,8.1553607742535e-06,1.19711958177504e-05,1.39167123052175e-05,3.64140396413859e-05,0.00874876510351896),
    McrbC           = c(0.000234114078921266,0.000331670715240762,0.00045759734348394,0.000494277977850288,0.000908287474885583,0.11704221367836),
    fastSOM         = c(0.00799247156828642,0.00679078698158264,0.00790248159319162,0.00863124523311853,0.0160052757710218,1.37376236915588),
    SlowSOM         = c(0.00113326858263463,0.00139296159613878,0.00487620709463954,0.00573843158781528,0.0146976625546813,35.726146697998),
    McrbN           = c(2.34114086197224e-05,3.31670707964804e-05,4.5759734348394e-05,4.94277992402203e-05,9.08287474885583e-05,0.011704221367836),
    fastSoilN       = c(0.000531428260728717,0.000448343256721273,0.000382687663659453,0.000410115695558488,0.000740505522117019,0.0687301605939865),
    slowSoilN       = c(2.69610682153143e-05,3.15931683871895e-05,9.86106460914016e-05,0.00011543767323019,0.000288777897367254,0.696267783641815),
    mineralN        = c(0.000225552576011978,0.00022545934189111,0.000223030219785869,0.000222843518713489,0.000221536232857034,0.000201760674826801),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.00014465456479229,0.000158942959387787,0.000267393799731508,0.0133648561313748),
    N_yrMin         = c(-0.0147743243724108,-9.3362132247421e-08,0.000144465084304102,0.000158757669851184,0.0002671989495866,0.013364665210247),
    N_P2S           = c(2.11112201213837e-05,3.07755581161473e-05,9.27835862967186e-05,0.000102327991044149,0.000192590814549476,0.0159935280680656),
    N_loss          = c(0.0249017998576164,0.0100992163643241,0.00990797579288483,0.00989404693245888,0.00981944892555475,0.00874032080173492),
    totseedC        = c(0,0,0,0,0,0.000826646690256894),
    totseedN        = c(0,0,0,0,0,4.13323286920786e-05),
    Seedling_C      = c(0,0,0,0,0,0.000826646690256894),
    Seedling_N      = c(0,0,0,0,0,4.13323250540998e-05),
    MaxAge          = c(0.999922752380371,2.00033664703369,7.9944634437561,8.9969425201416,16.0144596099854,130.408660888672),
    MaxVolume       = c(0.00020332750864327,0.000246162409894168,0.000598524697124958,0.000703596277162433,0.00190511276014149,0.449968069791794),
    MaxDBH          = c(0.00657883239910007,0.00714902672916651,0.0105199161916971,0.0112863117828965,0.0174035467207432,0.187279582023621),
    NPPL            = c(0.001176628167741,0.000700533681083471,0.00118906644638628,0.00130050932057202,0.00258257193490863,0.13407064974308),
    NPPW            = c(0.00126155931502581,0.000784097821451724,0.00155806133989245,0.00178237247746438,0.00397400138899684,0.360370218753815),
    n_deadtrees     = c(4.29384454037063e-06,4.58221984445117e-06,1.14814083644887e-05,1.28608389786677e-05,2.63639722106745e-05,0.00486104283481836),
    c_deadtrees     = c(8.35835744510405e-05,9.25029235077091e-05,0.000245365663431585,0.000283768051303923,0.000688058091327548,0.35599160194397),
    m_turnover      = c(8.35835744510405e-05,9.25029235077091e-05,0.000245365663431585,0.000283768051303923,0.000688058091327548,0.35599160194397),
    c_turnover_time = c(2.12049031257629,3.64033102989197,2.68918538093567,2.7327868938446,3.20707249641418,8.49711894989014),
    lu_fraction     = c(1,1,1,1,1,1))
  ref_BiomeE_gsLeun_oac_yr1 <- tibble(
    cohort      = 1,
    year        = 1,
    cID         = 1,
    PFT         = 2,
    layer       = 1,
    density     = 500,
    flayer      = 0.00400206865742803,
    DBH         = 0.65788322687149,
    dDBH        = 0.107067592442036,
    height      = 2.91996002197266,
    age         = 0.999922752380371,
    BA          = 3.39928483299445e-05,
    dBA         = 1.01640325738117e-05,
    Acrown      = 0.0800413712859154,
    Aleaf       = 0.119440279901028,
    nsc         = 0.0535239949822426,
    seedC       = 0.00480090966448188,
    leafC       = 0,
    rootC       = 0.0203048475086689,
    sapwC       = 0.0121231861412525,
    woodC       = 0.0217287018895149,
    nsn         = 0.053502481430769,
    treeG       = 0.0721193701028824,
    fseed       = 0,
    fleaf       = 0.326300173997879,
    froot       = 0.323846757411957,
    fwood       = 0.349853128194809,
    GPP         = 0.0533972829580307,
    NPP         = 0.00910497829318047,
    Rauto       = 0.0442923046648502,
    Nupt        = 0,
    Nfix        = 0,
    deathrate   = 0.0105336084961891,
    n_deadtrees = 4.29384454037063e-06,
    c_deadtrees = 8.35835744510405e-05)
  ref_BiomeE_gsLeun_oac_yr2 <- tibble(
    cohort      = 1,
    year        = 2,
    cID         = 1,
    PFT         = 2,
    layer       = 1,
    density     = 494.733184814453,
    flayer      = 0.00448572542518377,
    DBH         = 0.714902698993683,
    dDBH        = 0.0570194348692894,
    height      = 3.04386901855469,
    age         = 2.00033664703369,
    BA          = 4.01405886805151e-05,
    dBA         = 6.14774035057053e-06,
    Acrown      = 0.0906695872545242,
    Aleaf       = 0.172846406698227,
    nsc         = 0.04190494120121,
    seedC       = 0.00393445231020451,
    leafC       = 0,
    rootC       = 0.0293838884681463,
    sapwC       = 0.0175439082086086,
    woodC       = 0.0333848409354687,
    nsn         = 0.0576952509582043,
    treeG       = 0.0531104505062103,
    fseed       = 0,
    fleaf       = 0.266610950231552,
    froot       = 0.434975028038025,
    fwood       = 0.298414021730423,
    GPP         = 0.0807787701487541,
    NPP         = 0.0414913110435009,
    Rauto       = 0.0392874591052532,
    Nupt        = 0,
    Nfix        = 0,
    deathrate   = 0.0106044635176659,
    n_deadtrees = 4.58221984445117e-06,
    c_deadtrees = 9.25029235077091e-05)
  ref_BiomeE_gsLeun_oac_yr251 <- tibble(
    cohort      = c(2,1,3,4,5,6,7,8,9,10,11),
    year        = c(251,251,251,251,251,251,251,251,251,251,251),
    cID         = c(125,405,403,379,357,319,368,382,388,396,402),
    PFT         = c(2,2,2,2,2,2,2,2,2,2,2),
    layer       = c(1,1,2,2,2,2,2,2,2,2,2),
    density     = c(78.2982482910156,3553.80078125,159.064239501953,72.9291458129883,5.53848838806152,11.6919250488281,63.3968238830566,113.339073181152,629.55126953125,1719.81091308594,4686.35595703125),
    flayer      = c(0.095187284052372,0.940410912036896,0.0377802364528179,0.00984324049204588,0.000544983777217567,0.000319657468935475,0.00108020985499024,0.00136600492987782,0.00499200820922852,0.00947774946689606,0.0154948951676488),
    DBH         = c(18.7279586791992,6.77676105499268,6.30570888519287,4.32610607147217,3.50429534912109,1.49212455749512,1.08867835998535,0.864279568195343,0.653785526752472,0.512966275215149,0.364900708198547),
    dDBH        = c(0.186491012573242,0.165215134620667,0.0880129635334015,0.0763203948736191,0.0718042254447937,0.0464653596282005,0.0593569129705429,0.053615216165781,0.0480782240629196,0.0461678951978683,0.0880683287978172),
    height      = c(15.5792922973633,9.37159633636475,9.0400218963623,7.48774528503418,6.73911476135254,4.39749193191528,3.75623083114624,3.34679889678955,2.91085195541382,2.57837986946106,2.17465257644653),
    age         = c(130.408660888672,63.5554428100586,63.5554351806641,49.9869232177734,46.4089012145996,22.2525749206543,14.8682641983032,11.028341293335,6.93639469146729,3.94537758827209,1.02991378307343),
    BA          = c(0.027546776458621,0.00360690127126873,0.00312289735302329,0.00146988779306412,0.000964475679211318,0.000174863846041262,9.30870155571029e-05,5.86676069360692e-05,3.35707045451272e-05,2.066652814392e-05,1.04577757156221e-05),
    dBA         = c(0.000545887276530266,0.000173726119101048,8.65685287863016e-05,5.14056300744414e-05,3.91199137084186e-05,1.07210944406688e-05,9.87386738415807e-06,7.05306956660934e-06,4.75591332360636e-06,3.55264455720317e-06,4.43878570877132e-06),
    Acrown      = c(12.1570138931274,2.64621162414551,2.37515592575073,1.34969913959503,0.983993649482727,0.273400217294693,0.170388638973236,0.120523743331432,0.0792947039008141,0.0551092512905598,0.0330638438463211),
    Aleaf       = c(38.2941856384277,8.18433570861816,3.74065852165222,2.1256217956543,1.54965925216675,0.267021626234055,0.183970302343369,0.130132019519806,0.0856120735406876,0.0594961829483509,0.0356826260685921),
    nsc         = c(3.25315260887146,0.0912651419639587,1.29592955112457,0.696176409721375,0.498082220554352,0.00340645317919552,0.0890594348311424,0.0609861686825752,0.0380494482815266,0.0260681603103876,0.0239663012325764),
    seedC       = c(0.0440724268555641,0.00463279383257031,0.0252405237406492,0.0150645114481449,0.0113848047330976,0.000138801638968289,0.000172674073837698,0.000604929693508893,0.00104854581877589,0.00171835196670145,0.0021857840474695),
    leafC       = c(0.116151906549931,0.0223901625722647,0.020531203597784,0.0245242454111576,0.028617724776268,0,0,0,0,0,0),
    rootC       = c(6.51001167297363,1.39133715629578,0.63591194152832,0.361355721950531,0.263442069292068,0.0453936755657196,0.0312749519944191,0.0221224445849657,0.0145540526136756,0.0101143512874842,0.00606604618951678),
    sapwC       = c(3.88685965538025,0.825080215930939,0.379676789045334,0.215750589966774,0.157290399074554,0.027024433016777,0.00960195902734995,0.0067928908392787,0.00446894904598594,0.00310570048168302,0.0018626325763762),
    woodC       = c(98.5694198608398,9.5140495300293,4.14513826370239,1.74248075485229,1.07329976558685,0.104069709777832,0.0503905601799488,0.0296327881515026,0.0155936805531383,0.00892535969614983,0.00407628435641527),
    nsn         = c(67.9187622070312,6.55560302734375,9.47067165374756,3.98124647140503,2.45233345031738,0.39070463180542,0.189230293035507,0.11128319054842,0.0585641041398048,0.0335229001939297,0.0153172705322504),
    treeG       = c(9.95716667175293,2.17798924446106,1.02454698085785,0.597918748855591,0.444437295198441,0.0767199397087097,0.0494772829115391,0.0340375751256943,0.0218845680356026,0.0154258143156767,0.0164758320897818),
    fseed       = c(0.0116651561111212,0.010280198417604,0.0200392995029688,0.0410160161554813,0.0643909201025963,0,0,0,0,0,0),
    fleaf       = c(0.142092421650887,0.150075674057007,0.120601415634155,0.13859786093235,0.138166487216949,0.144526332616806,0.175531715154648,0.186846002936363,0.201027512550354,0.212091729044914,0.420727610588074),
    froot       = c(0.465765953063965,0.432468563318253,0.436583042144775,0.436410248279572,0.42855840921402,0.402888000011444,0.238550826907158,0.246126890182495,0.253026336431503,0.251300901174545,0.0257815010845661),
    fwood       = c(0.380476415157318,0.407175570726395,0.422776222229004,0.383975863456726,0.368884176015854,0.452585637569427,0.585917472839355,0.56702709197998,0.54594612121582,0.536607325077057,0.553490877151489),
    GPP         = c(19.4798011779785,3.92785358428955,1.87679970264435,1.06279635429382,0.773025810718536,0.136646389961243,0.0963694676756859,0.0677627623081207,0.0441600009799004,0.0303058940917253,0.0166350100189447),
    NPP         = c(11.1903800964355,2.1817364692688,1.04719960689545,0.591447949409485,0.431097775697708,0.0768940448760986,0.0567255988717079,0.0402793809771538,0.0264093391597271,0.0179261341691017,0.00577194057404995),
    Rauto       = c(8.28942108154297,1.74611699581146,0.829600095748901,0.471348375082016,0.341928035020828,0.059752345085144,0.039643868803978,0.0274833831936121,0.0177506618201733,0.0123797599226236,0.0108630694448948),
    Nupt        = c(0.166139543056488,0.0329931974411011,0.0163769423961639,0.00927567481994629,0.0067480094730854,0.00106944539584219,0.000401989731471986,0.00028281714185141,0,0,0),
    Nfix        = c(0,0,0,0,0,0,0,0,0,0,0),
    deathrate   = c(0.0910467579960823,0.0276414100080729,0.0983433052897453,0.126650810241699,0.150307968258858,0.259013503789902,0.290360301733017,0.308912336826324,0.326902389526367,0.339194059371948,0.352289497852325),
    n_deadtrees = c(0.00109159736894071,0.00261754961684346,0.000255618302617222,8.17141481093131e-05,5.2934706218366e-06,2.59323269347078e-06,1.00544957604143e-05,1.49316856550286e-05,7.00404489180073e-05,0.000194186737644486,0.000517463544383645),
    c_deadtrees = c(0.127931907773018,0.179072052240372,0.024825369939208,0.00644462741911411,0.000369689951185137,0.000171116291312501,0.000673233938869089,0.000798657594714314,0.00266409432515502,0.00469590350985527,0.00834496784955263))

  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_Pmodel$data$output_daily_tile )|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365)),  ref_BiomeE_Pmodel_odt_yr1)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_Pmodel$data$output_daily_tile )|>filter(year==251, doy %in% c(1, 2, 180, 364, 365)),  ref_BiomeE_Pmodel_odt_yr251)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_Pmodel$data$output_annual_tile)|>filter(           year %in% c(1, 2, 8, 9, 16, 251)), ref_BiomeE_Pmodel_oat)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts)|>filter(year==  1)|>arrange(-DBH),                 ref_BiomeE_Pmodel_oac_yr1)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts)|>filter(year==  2)|>arrange(-DBH),                 ref_BiomeE_Pmodel_oac_yr2)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts)|>filter(year==251)|>arrange(-DBH),                 ref_BiomeE_Pmodel_oac_yr251)
  
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_gsLeun$data$output_daily_tile )|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365)),  ref_BiomeE_gsLeun_odt_yr1)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_gsLeun$data$output_daily_tile )|>filter(year==251, doy %in% c(1, 2, 180, 364, 365)),  ref_BiomeE_gsLeun_odt_yr251)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_gsLeun$data$output_annual_tile)|>filter(           year %in% c(1, 2, 8, 9, 16, 251)), ref_BiomeE_gsLeun_oat)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts)|>filter(year==  1)|>arrange(-DBH),                 ref_BiomeE_gsLeun_oac_yr1)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts)|>filter(year==  2)|>arrange(-DBH),                 ref_BiomeE_gsLeun_oac_yr2)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts)|>filter(year==251)|>arrange(-DBH),                 ref_BiomeE_gsLeun_oac_yr251)
  
  
  # Hardcoded reference outputs.
  # NOTE: this is expected to change reasonably frequently whenever something is
  #       changed in the model.
  #       If this is expected, please update the hardcoded reference values below.
  #       To do so, simply use the commented code, making use of dput(). Thanks!
  # mod_BiomeE_PLULUC$aggregated|>filter(                           year %in% c(1, 2, 8, 9, 16, 251)) |> dput()
  # mod_BiomeE_PLULUC$primary$output_daily_tile|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365)) |> dput()
  # mod_BiomeE_PLULUC$primary$output_daily_tile|>filter(year==251, doy %in% c(1, 2, 180, 364, 365)) |> dput()
  # mod_BiomeE_PLULUC$primary$output_annual_tile|>filter(           year %in% c(1, 2, 8, 9, 16, 251)) |> dput()
  # mod_BiomeE_PLULUC$primary$output_annual_cohorts|>filter(year==  1) |> dput()
  # mod_BiomeE_PLULUC$primary$output_annual_cohorts|>filter(year==  2) |> dput()
  # mod_BiomeE_PLULUC$primary$output_annual_cohorts|>filter(year==251) |> dput()
  # mod_BiomeE_PLULUC$secondary$output_daily_tile|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365)) |> dput()
  # mod_BiomeE_PLULUC$secondary$output_daily_tile|>filter(year==251, doy %in% c(1, 2, 180, 364, 365)) |> dput()
  # mod_BiomeE_PLULUC$secondary$output_annual_tile|>filter(           year %in% c(1, 2, 8, 9, 16, 251)) |> dput()
  # mod_BiomeE_PLULUC$secondary$output_annual_cohorts|>filter(year==  1) |> dput()
  # mod_BiomeE_PLULUC$secondary$output_annual_cohorts|>filter(year==  2) |> dput()
  # mod_BiomeE_PLULUC$secondary$output_annual_cohorts|>filter(year==251) |> dput()
  ref_BiomeE_PLULUC_aggregated <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00416006147861481,0.0051026726141572,0.013795854523778,0.0158615466207266,0.0471739545464516,1.34490156173706),
    LAI             = c(0.00620649103075266,0.00972458533942699,0.043438233435154,0.0499434173107147,0.144030258059502,3.74448251724243),
    Density         = c(500,494.722717285156,462.53564453125,456.990539550781,1980.70007324219,46017.578125),
    DBH             = c(0.67508590221405,0.779046058654785,1.58127772808075,1.74943614006042,1.17045211791992,1.05999457836151),
    Density12       = c(0,0,0,0,0,214.249755859375),
    DBH12           = c(0,0,0,0,0,21.6128616333008),
    QMD12           = c(0,0,0,0,0,21.6495342254639),
    NPP             = c(0.00230822991579771,0.00474032014608383,0.0180877968668938,0.0207991693168879,0.0605944767594337,1.43212604522705),
    GPP             = c(0.00443602073937654,0.00687614921480417,0.0263272896409035,0.0303885489702225,0.0890411883592606,2.20210719108582),
    Rauto           = c(0.00212779105640948,0.00213582930155098,0.00823949463665485,0.00958937965333462,0.0284467153251171,0.769981026649475),
    Rh              = c(0.0028953910805285,0.00248773163184524,0.00351402210071683,0.00408122735098004,0.0118605736643076,1.29935908317566),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.904052734375,799.904052734375,799.904724121094,799.904907226562,799.90673828125,799.930053710938),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.599700927734,159.441146850586,157.805053710938,157.495330810547,153.281402587891,94.2941207885742),
    Runoff          = c(428.025756835938,428.088317871094,429.724426269531,430.034057617188,434.247924804688,493.235290527344),
    plantC          = c(0.00990187004208565,0.0133129954338074,0.0604372769594193,0.0732633098959923,0.244614720344543,11.881664276123),
    soilC           = c(0.0111686438322067,0.010008754208684,0.0205024518072605,0.0243835598230362,0.0789381265640259,78.5517730712891),
    plantN          = c(0.000283176312223077,0.000251700810622424,0.00107245030812919,0.00126676075160503,0.00361416721716523,0.112356156110764),
    soilN           = c(0.00390609027817845,0.00347790773957968,0.00353616685606539,0.00364831881597638,0.00530428625643253,1.62823057174683),
    totN            = c(0.00418926635757089,0.00372960884124041,0.00460861716419458,0.0049150800332427,0.00891845300793648,1.74058675765991),
    NSC             = c(0.0042251693084836,0.00518226251006126,0.0209579616785049,0.0248370207846165,0.0757390111684799,2.22338700294495),
    SeedC           = c(0,0,0.00153295951895416,0.00226585427299142,0.00386109342798591,0.0703786015510559),
    leafC           = c(0.0010551034938544,0.00165317952632904,0.00738450000062585,0.00849038176238537,0.024485144764185,0.636562049388885),
    rootC           = c(0.000629958754871041,0.00098704535048455,0.00440898025408387,0.00506925676018,0.0146190682426095,0.380062103271484),
    SapwoodC        = c(0.00115265452768654,0.00201194896362722,0.0154777374118567,0.0192941222339869,0.0739047154784203,4.9033055305481),
    WoodC           = c(0.00283898459747434,0.00347855873405933,0.0106751378625631,0.0133066764101386,0.0520057007670403,3.66797018051147),
    NSN             = c(0.000237868691328913,0.000182893098099157,0.000683798047248274,0.000787506462074816,0.00227460172027349,0.0638936385512352),
    SeedN           = c(0,0,7.66480516176671e-05,0.00011329265544191,0.000193054700503126,0.00351893017068505),
    leafN           = c(1.81540090125054e-05,2.84444759017788e-05,0.000127057341160253,0.000146085250889882,0.000421290402300656,0.01095269061625),
    rootN           = c(1.57489594130311e-05,2.46760791924316e-05,0.000110224369564094,0.000126731349155307,0.000365475978469476,0.00950152892619371),
    SapwoodN        = c(3.29329850501381e-06,5.74842579226242e-06,4.42221135017462e-05,5.51260673091747e-05,0.00021115635172464,0.0140094440430403),
    WoodN           = c(8.11138215794927e-06,9.93873254628852e-06,3.05004105030093e-05,3.80190831492655e-05,0.00014858775830362,0.0104799186810851),
    McrbC           = c(0.000287109753116965,0.000403904035920277,0.000671996036544442,0.000772989937104285,0.00220785499550402,0.194691598415375),
    fastSOM         = c(0.00974625535309315,0.00818772707134485,0.0127302370965481,0.0147649068385363,0.0426847860217094,2.01838397979736),
    SlowSOM         = c(0.00113527732901275,0.00141712243203074,0.00710021937265992,0.00884566642343998,0.0340454839169979,76.3387069702148),
    McrbN           = c(2.87109760392923e-05,4.03904014092404e-05,6.71996021992527e-05,7.729899516562e-05,0.000220785499550402,0.0194691587239504),
    fastSoilN       = c(0.000636344775557518,0.000536472711246461,0.000580763793550432,0.000662768143229187,0.00186954694800079,0.106583580374718),
    slowSoilN       = c(2.69899937848095e-05,3.19643804687075e-05,0.000138284027343616,0.000170996616361663,0.000630303169600666,1.49933898448944),
    mineralN        = c(0.00321404449641705,0.00286908028647304,0.00274991942569613,0.00273725506849587,0.00258365087211132,0.00283893477171659),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000328844995237887,0.000375772069673985,0.00102190545294434,0.0221600160002708),
    N_yrMin         = c(-0.0117859477177262,-0.000344965694239363,0.000317465397529304,0.000363108585588634,0.00100461498368531,0.0221987571567297),
    N_P2S           = c(2.14073552342597e-05,3.33384523401037e-05,0.000169749677297659,0.000196994922589511,0.000581519561819732,0.0272029004991055),
    N_loss          = c(0.0219198539853096,0.0104610156267881,0.00974620040506124,0.00970437098294497,0.00916359946131706,0.00925747770816088),
    totseedC        = c(0,0,0,0,0.00356994639150798,0.0649071633815765),
    totseedN        = c(0,0,0,0,0.000178497371962294,0.00324535788968205),
    Seedling_C      = c(0,0,0,0,0.00356994639150798,0.0649071633815765),
    Seedling_N      = c(0,0,0,0,0.000178497342858464,0.0032453581225127),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627334594727),
    MaxVolume       = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627334594727),
    MaxDBH          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627334594727),
    NPPL            = c(0.00121877959463745,0.0008825832628645,0.00248827692121267,0.002812419552356,0.00808483455330133,0.169744536280632),
    NPPW            = c(0.00149163906462491,0.00154099613428116,0.00566299352794886,0.00676144752651453,0.02101119607687,0.651887178421021),
    n_deadtrees     = c(4.3513900891412e-06,4.85187956655864e-06,2.36887444771128e-05,2.83907611446921e-05,9.52724803937599e-05,0.0140095250681043),
    c_deadtrees     = c(0.00010314847168047,0.0001401223562425,0.000713716843165457,0.000889366143383086,0.00358403264544904,0.899706423282623),
    m_turnover      = c(0.00010314847168047,0.0001401223562425,0.000713716843165457,0.000889366143383086,0.00358403264544904,0.899706423282623),
    c_turnover_time = c(1.90326523780823,2.25734448432922,1.88507008552551,1.96802186965942,2.47514224052429,5.62669467926025),
    lu_fraction     = c(1,1,1,1,1,1),
    prod_pool_1_C   = c(0.000375000003259629,0.000227448996156454,1.13240193968522e-05,6.86836483509978e-06,2.07406671393073e-07,1.40129846432482e-45),
    prod_pool_1_N   = c(1.07142852812103e-06,6.49854257517291e-07,3.23543396518744e-08,1.96238989502717e-08,5.92590532200177e-10,1.40129846432482e-45),
    prod_pool_2_C   = c(0.000375000003259629,0.000356711039785296,0.000264258094830438,0.000251370074693114,0.000177137553691864,1.39750544470019e-09),
    prod_pool_2_N   = c(1.07142852812103e-06,1.01917441952537e-06,7.55023165766033e-07,7.18200283245096e-07,5.06107255660027e-07,3.99287460733921e-12))
  ref_BiomeE_PLULUC_primary_odt_yr1 <- tibble(
    year            = c(1,1,1,1,1),
    doy             = c(1,2,180,364,365),
    Tk              = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp            = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    totWs           = c(799.944641113281,799.947204589844,799.131042480469,799.930847167969,799.903991699219),
    Trsp            = c(0,0,0,0,0),
    Evap            = c(0.0553628616034985,0.0527696460485458,0.868921220302582,0.0691292807459831,0.0960086733102798),
    Runoff          = c(1.43654549121857,1.94845604896545,1.08043730258942,1.96995043754578,1.75696134567261),
    ws1             = c(19.944637298584,19.9472312927246,19.1310787200928,19.930871963501,19.9039936065674),
    ws2             = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3             = c(600,600,600,600,600),
    LAI             = c(0,0.000364852574421093,0.00466133235022426,0.00618919776752591,0.00619784602895379),
    GPP             = c(0,0,1.90892133105081e-05,6.17546766079613e-06,6.16563283983851e-06),
    Rauto           = c(2.97977797991678e-09,5.82730172027368e-05,4.93635025122785e-06,4.55608324045897e-06,4.57825399280409e-06),
    Rh              = c(2.77344884125341e-06,2.75237016467145e-06,1.03804013633635e-05,2.3680029244133e-06,2.36252321883512e-06),
    NSC             = c(0.00582691049203277,0.00565209938213229,0.00398708041757345,0.00424133753404021,0.00423404527828097),
    seedC           = c(0,0,0,0,0),
    leafC           = c(0,6.20249411440454e-05,0.000792426522821188,0.00105216365773231,0.00105363386683166),
    rootC           = c(0,3.70325360563584e-05,0.000473125197459012,0.000628203561063856,0.000629081332590431),
    SW_C            = c(0.00250000017695129,0.00163697078824043,0.000808653887361288,0.00114870828110725,0.00115068175364286),
    HW_C            = c(0,0.000880510022398084,0.00244240881875157,0.002835190622136,0.00283709052018821),
    NSN             = c(0.000293089426122606,0.000291046482743695,0.00025848179939203,0.000238109059864655,0.000237988890148699),
    seedN           = c(0,0,0,0,0),
    leafN           = c(0,1.06719380710274e-06,1.36344051497872e-05,1.81034265551716e-05,1.81287232408067e-05),
    rootN           = c(0,9.25813424146327e-07,1.18281122922781e-05,1.57050762936706e-05,1.57270224008244e-05),
    SW_N            = c(7.14285715730512e-06,4.67705922346795e-06,2.31044009524339e-06,3.28202372656961e-06,3.28766213897325e-06),
    HW_N            = c(0,2.5157430627587e-06,6.97831228535506e-06,8.10054189059883e-06,8.10597066447372e-06),
    McrbC           = c(3.69218014384387e-07,7.35389221517835e-07,9.97748793452047e-05,0.000234167644521222,0.000234328705118969),
    fastSOM         = c(0.009996865876019,0.00999375618994236,0.00931816641241312,0.00799902994185686,0.00799865275621414),
    slowSOM         = c(0.000999990850687027,0.000999981770291924,0.00105635263025761,0.00113423995207995,0.00113475287798792),
    McrbN           = c(3.6921800727896e-08,7.3538920730698e-08,9.97748793452047e-06,2.34167637245264e-05,2.34328708756948e-05),
    fastSoilN       = c(0.000666457752231508,0.000666250416543335,0.000621211074758321,0.000531849800609052,0.000531776517163962),
    slowSoilN       = c(2.49997719947714e-05,2.4999544621096e-05,2.58212767221266e-05,2.69752108579269e-05,2.69825650320854e-05),
    mineralN        = c(0.0149249155074358,0.0148505438119173,0.00587099697440863,0.00320566212758422,0.00321121863089502),
    N_uptk          = c(0,0,0,0,0))
  ref_BiomeE_PLULUC_primary_odt_yr251 <- tibble(
    year            = c(251,251,251,251,251),
    doy             = c(1,2,180,364,365),
    Tk              = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp            = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    totWs           = c(799.960083007812,799.958984375,799.516296386719,799.951416015625,799.930053710938),
    Trsp            = c(0,0,0,0,0),
    Evap            = c(0.0399133116006851,0.0409998670220375,0.483673214912415,0.0485905706882477,0.0699304714798927),
    Runoff          = c(1.36660242080688,1.96390557289124,1.74325275421143,1.99354791641235,1.77749955654144),
    ws1             = c(19.9600887298584,19.9590015411377,19.5163269042969,19.9514083862305,19.9300708770752),
    ws2             = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3             = c(600,600,600,600,600),
    LAI             = c(3.49222874641418,3.49872827529907,3.61742806434631,3.74305081367493,3.74376726150513),
    GPP             = c(0.00227762688882649,0.00224546389654279,0.00936589110642672,0.00246698572300375,0.00246065924875438),
    Rauto           = c(6.12458461546339e-05,0.00236867531202734,0.00243186368606985,0.00197461596690118,0.00198433455079794),
    Rh              = c(0.00142561015672982,0.00141680834349245,0.00560735259205103,0.00138231716118753,0.00138092681299895),
    NSC             = c(2.14304661750793,2.13828349113464,2.130375623703,2.23058652877808,2.22722387313843),
    seedC           = c(0.00199680938385427,0.00217894976958632,0.0329008847475052,0.0699842646718025,0.0701815783977509),
    leafC           = c(0.593678891658783,0.594783842563629,0.614962697029114,0.636318624019623,0.636440515518188),
    rootC           = c(0.360928356647491,0.360858380794525,0.36590713262558,0.379916995763779,0.379989594221115),
    SW_C            = c(4.5676908493042,4.5533447265625,4.69937038421631,4.90116691589355,4.90223693847656),
    HW_C            = c(3.35169720649719,3.36794400215149,3.51899361610413,3.66643047332764,3.66720080375671),
    NSN             = c(0.061430923640728,0.061454962939024,0.0630296841263771,0.064000092446804,0.0639436542987823),
    seedN           = c(9.98404429992661e-05,0.000108947468106635,0.00164504407439381,0.0034992138389498,0.00350907910615206),
    leafN           = c(0.0102148456498981,0.0102338576689363,0.0105810556560755,0.0109485015273094,0.0109505960717797),
    rootN           = c(0.00902319513261318,0.00902144704014063,0.00914766173809767,0.00949790421873331,0.00949971843510866),
    SW_N            = c(0.0130505450069904,0.0130095556378365,0.013426773250103,0.0140033354982734,0.0140063911676407),
    HW_N            = c(0.00957628153264523,0.00962270237505436,0.0100542716681957,0.0104755219072104,0.0104777216911316),
    McrbC           = c(0.194571763277054,0.194574505090714,0.195704534649849,0.194694772362709,0.194691613316536),
    fastSOM         = c(2.17454314231873,2.17509269714355,2.18391823768616,2.01643490791321,2.01709246635437),
    slowSOM         = c(76.9140014648438,76.9135589599609,76.7412033081055,76.3388214111328,76.3383941650391),
    McrbN           = c(0.0194571763277054,0.0194574501365423,0.0195704530924559,0.0194694772362709,0.0194691605865955),
    fastSoilN       = c(0.11321485042572,0.113226071000099,0.112775862216949,0.106537438929081,0.106550887227058),
    slowSoilN       = c(1.50097978115082,1.50098061561584,1.50067782402039,1.49933409690857,1.49933516979218),
    mineralN        = c(0.00281932600773871,0.00275931577198207,0.00167982396669686,0.00281613017432392,0.0028474279679358),
    N_uptk          = c(0,9.1266272647772e-05,0,0,0))
  ref_BiomeE_PLULUC_primary_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00416006147861481,0.0051026726141572,0.013795854523778,0.0158615466207266,0.0471739545464516,1.34490144252777),
    LAI             = c(0.00620649103075266,0.00972458533942699,0.043438233435154,0.0499434173107147,0.144030258059502,3.74448251724243),
    Density         = c(500,494.722686767578,462.535614013672,456.990539550781,1980.70007324219,46017.57421875),
    DBH             = c(0.67508590221405,0.77904599905014,1.58127772808075,1.74943602085114,1.17045211791992,1.0599946975708),
    Density12       = c(0,0,0,0,0,214.249755859375),
    DBH12           = c(0,0,0,0,0,21.6128597259521),
    QMD12           = c(0,0,0,0,0,21.6495342254639),
    NPP             = c(0.00230822968296707,0.00474032014608383,0.0180877950042486,0.0207991693168879,0.0605944767594337,1.43212604522705),
    GPP             = c(0.00443602073937654,0.00687614921480417,0.0263272896409035,0.0303885489702225,0.0890411883592606,2.20210695266724),
    Rauto           = c(0.00212779105640948,0.00213582930155098,0.00823949463665485,0.00958937965333462,0.028446713462472,0.769980907440186),
    Rh              = c(0.00236286479048431,0.00206716731190681,0.00342911528423429,0.00401711370795965,0.0118518937379122,1.29935908317566),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903991699219,799.904052734375,799.904724121094,799.904907226562,799.90673828125,799.930053710938),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.599700927734,159.441146850586,157.805053710938,157.495330810547,153.281402587891,94.2941207885742),
    Runoff          = c(428.025726318359,428.088317871094,429.724395751953,430.034057617188,434.247924804688,493.235260009766),
    plantC          = c(0.00990187004208565,0.0133129954338074,0.0604372769594193,0.0732633098959923,0.244614720344543,11.881664276123),
    soilC           = c(0.00937039777636528,0.0086310775950551,0.0202441606670618,0.0241893790662289,0.0789121612906456,78.5517883300781),
    plantN          = c(0.000283176312223077,0.000251700810622424,0.00107309536542743,0.00126743607688695,0.00361612509004772,0.112349756062031),
    soilN           = c(0.00379347242414951,0.00338095729239285,0.00351151009090245,0.00362917082384229,0.00529967527836561,1.62824010848999),
    totN            = c(0.00407664850354195,0.00363265816122293,0.00458460533991456,0.00489660678431392,0.00891580060124397,1.74058985710144),
    NSC             = c(0.00422516884282231,0.00518226251006126,0.0209579616785049,0.0248370189219713,0.0757390111684799,2.22338700294495),
    SeedC           = c(0,0,0.00153295940253884,0.00226585427299142,0.00386109319515526,0.0703785941004753),
    leafC           = c(0.0010551034938544,0.00165317952632904,0.00738449953496456,0.00849038176238537,0.0244851429015398,0.636562049388885),
    rootC           = c(0.000629958754871041,0.00098704535048455,0.00440898025408387,0.00506925676018,0.0146190682426095,0.380062103271484),
    SapwoodC        = c(0.00115265452768654,0.00201194896362722,0.0154777374118567,0.0192941222339869,0.0739047154784203,4.90330505371094),
    WoodC           = c(0.00283898459747434,0.00347855873405933,0.0106751378625631,0.0133066764101386,0.05200569704175,3.66797018051147),
    NSN             = c(0.000237868676776998,0.000182893098099157,0.000684443046338856,0.000788181787356734,0.00227655982598662,0.0638872385025024),
    SeedN           = c(0,0,7.66480443417095e-05,0.00011329265544191,0.000193054700503126,0.00351893017068505),
    leafN           = c(1.8154007193516e-05,2.84444759017788e-05,0.000127057341160253,0.000146085236337967,0.000421290373196825,0.0109526887536049),
    rootN           = c(1.57489575940417e-05,2.46760791924316e-05,0.000110224362288136,0.000126731349155307,0.000365475978469476,0.00950152985751629),
    SapwoodN        = c(3.29329850501381e-06,5.74842533751507e-06,4.42221098637674e-05,5.51260636711959e-05,0.000211156337172724,0.0140094431117177),
    WoodN           = c(8.11138215794927e-06,9.93873254628852e-06,3.05004105030093e-05,3.80190831492655e-05,0.00014858775830362,0.0104799186810851),
    McrbC           = c(0.000234328705118969,0.000333363102981821,0.00064427312463522,0.000751479179598391,0.00220471736975014,0.194691613316536),
    fastSOM         = c(0.00800079107284546,0.00688059162348509,0.012499668635428,0.0145922340452671,0.0426619611680508,2.01838397979736),
    SlowSOM         = c(0.00113527732901275,0.00141712243203074,0.00710021937265992,0.0088456654921174,0.0340454839169979,76.3387145996094),
    McrbN           = c(2.34328708756948e-05,3.33363095705863e-05,6.44273095531389e-05,7.51479165046476e-05,0.000220471731154248,0.0194691605865955),
    fastSoilN       = c(0.000531830999534577,0.000451567728305236,0.000561318185646087,0.000647874025162309,0.00186739815399051,0.106583803892136),
    slowSoilN       = c(2.69899937848095e-05,3.19643804687075e-05,0.000138284027343616,0.000170996616361663,0.000630303169600666,1.49933969974518),
    mineralN        = c(0.00321121863089502,0.00286408886313438,0.00274748052470386,0.00273515214212239,0.00258150254376233,0.0028474279679358),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000328926253132522,0.000375810253899544,0.00102232070639729,0.0221474543213844),
    N_yrMin         = c(-0.0117887761443853,-0.000347130466252565,0.000317916361382231,0.000363483210094273,0.00100469228345901,0.0222071725875139),
    N_P2S           = c(2.14073552342597e-05,3.33384523401037e-05,0.000169757404364645,0.000197003231733106,0.000581549596972764,0.0272013004869223),
    N_loss          = c(0.021915266290307,0.010445348918438,0.00973915681242943,0.0096988333389163,0.00916272588074207,0.00924919173121452),
    totseedC        = c(0,0,0,0,0.00356994615867734,0.0649071559309959),
    totseedN        = c(0,0,0,0,0.000178497357410379,0.00324535788968205),
    Seedling_C      = c(0,0,0,0,0.00356994615867734,0.0649071559309959),
    Seedling_N      = c(0,0,0,0,0.000178497342858464,0.0032453581225127),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627334594727),
    MaxVolume       = c(0.000215764259337448,0.000299950072076172,0.00152817298658192,0.0019280546111986,0.00737557932734489,0.652382910251617),
    MaxDBH          = c(0.00675085838884115,0.00779045978561044,0.0158127769827843,0.0174943599849939,0.0313498750329018,0.220105022192001),
    NPPL            = c(0.00121877959463745,0.0008825832628645,0.00248827692121267,0.002812419552356,0.00808483455330133,0.169744521379471),
    NPPW            = c(0.00149163894820958,0.00154099601786584,0.00566299306228757,0.00676144752651453,0.02101119607687,0.651887118816376),
    n_deadtrees     = c(4.3513900891412e-06,4.85187956655864e-06,2.36964751820778e-05,2.83990775642451e-05,9.53025082708336e-05,0.0140079222619534),
    c_deadtrees     = c(0.00010314847168047,0.0001401223562425,0.000713716843165457,0.000889366143383086,0.0035840324126184,0.899706423282623),
    m_turnover      = c(0.00010314847168047,0.0001401223562425,0.000713716843165457,0.000889366143383086,0.0035840324126184,0.899706423282623),
    c_turnover_time = c(1.90326523780823,2.25734448432922,1.88506996631622,1.96802186965942,2.47514224052429,5.62669515609741),
    lu_fraction     = c(0.600000023841858,0.600000023841858,0.600000023841858,0.600000023841858,0.600000023841858,0.600000023841858))
  ref_BiomeE_PLULUC_primary_oac_yr1 <- tibble(
    cohort          = 1,
    year            = 1,
    cID             = 1,
    PFT             = 2,
    layer           = 1,
    density         = 500,
    flayer          = 0.00416006147861481,
    DBH             = 0.675085842609406,
    dDBH            = 0.124270185828209,
    height          = 2.95789003372192,
    age             = 1.00000047683716,
    BA              = 3.57938079105224e-05,
    dBA             = 1.19649921543896e-05,
    Acrown          = 0.0832012295722961,
    Aleaf           = 0.124129816889763,
    nsc             = 0.0845033749938011,
    seedC           = 0.00475737359374762,
    leafC           = 0,
    rootC           = 0.0211020689457655,
    sapwC           = 0.0125991748645902,
    woodC           = 0.0230530891567469,
    nsn             = 0.0567796900868416,
    treeG           = 0.0781993493437767,
    fseed           = 0,
    fleaf           = 0.311710923910141,
    froot           = 0.306792557239532,
    fwood           = 0.381496518850327,
    GPP             = 0.0887204110622406,
    NPP             = 0.0461645908653736,
    Rauto           = 0.042555820196867,
    Nupt            = 0,
    Nfix            = 0,
    deathrate       = 0.0105546750128269,
    n_deadtrees     = 4.3513900891412e-06,
    c_deadtrees     = 0.00010314847168047)
  ref_BiomeE_PLULUC_primary_oac_yr2 <- tibble(
    cohort          = 1,
    year            = 2,
    cID             = 1,
    PFT             = 2,
    layer           = 1,
    density         = 494.722686767578,
    flayer          = 0.0051026726141572,
    DBH             = 0.77904599905014,
    dDBH            = 0.103960141539574,
    height          = 3.17748880386353,
    age             = 1.99997925758362,
    BA              = 4.76668064948171e-05,
    dBA             = 1.18729985842947e-05,
    Acrown          = 0.103142082691193,
    Aleaf           = 0.196566388010979,
    nsc             = 0.104750856757164,
    seedC           = 0.00369688123464584,
    leafC           = 0,
    rootC           = 0.033416286110878,
    sapwC           = 0.0199514869600534,
    woodC           = 0.0406682156026363,
    nsn             = 0.0703133046627045,
    treeG           = 0.0755703151226044,
    fseed           = 0,
    fleaf           = 0.236070990562439,
    froot           = 0.35174748301506,
    fwood           = 0.412181466817856,
    GPP             = 0.138989970088005,
    NPP             = 0.0958177149295807,
    Rauto           = 0.0431722514331341,
    Nupt            = 0,
    Nfix            = 0,
    deathrate       = 0.0106876138597727,
    n_deadtrees     = 4.85187956655864e-06,
    c_deadtrees     = 0.0001401223562425)
  ref_BiomeE_PLULUC_primary_oac_yr251 <- tibble(
    cohort          = c(1,2,3,4,5,6),
    year            = c(251,251,251,251,251,251),
    cID             = c(453,399,405,451,433,25),
    PFT             = c(2,2,2,2,2,2),
    layer           = c(1,1,1,2,2,2),
    density         = c(6186.80615234375,194.83171081543,19.4180297851562,1437.65539550781,133.359893798828,38045.50390625),
    flayer          = c(0.752436757087708,0.301783740520477,0.0215486716479063,0.137903496623039,0.00330994161777198,0.12791895866394),
    DBH             = c(4.03591012954712,22.0105018615723,17.6231174468994,3.44524002075195,1.39895117282867,0.369000434875488),
    dDBH            = c(0.24162270128727,0.415582954883575,0.391332805156708,0.0729762017726898,0.00261068344116211,0.0290364492684603),
    height          = c(7.2322473526001,16.8895263671875,15.1127624511719,6.68208932876587,4.25798177719116,2.18683457374573),
    age             = c(25.6355724334717,77.7627258300781,77.7627334594727,25.635570526123,22.6215229034424,2.81209373474121),
    BA              = c(0.00127930147573352,0.0380495749413967,0.0243924465030432,0.000932242430280894,0.00015370748587884,1.06940851765103e-05),
    dBA             = c(0.000148593680933118,0.00142327323555946,0.00107127241790295,3.90747445635498e-05,5.73156285099685e-07,1.61680509336293e-06),
    Acrown          = c(1.21619582176208,15.4894561767578,11.0972499847412,0.959224998950958,0.248196184635162,0.0336226224899292),
    Aleaf           = c(3.83005237579346,48.7879486083984,34.9530982971191,1.51069128513336,0.0880051031708717,0.036306157708168),
    nsc             = c(2.28830218315125,35.3928833007812,24.9768371582031,0.331827044487,0.000859242165461183,0.00575008522719145),
    seedC           = c(0.0601722933351994,0.76550430059433,0.54928994178772,0.0238187629729509,0.00613935943692923,0.00188525673002005),
    leafC           = c(0.0717583149671555,1.15043652057648,0.80964070558548,0.0115929879248142,0.0247553531080484,0),
    rootC           = c(0.651108920574188,8.2939510345459,5.94202709197998,0.256817519664764,0.0149608682841063,0.00617204699665308),
    sapwC           = c(0.388750284910202,4.95197582244873,3.54773926734924,0.153335139155388,0.00871927756816149,0.00368507485836744),
    woodC           = c(2.88789677619934,142.90104675293,85.6981048583984,1.03218019008636,0.129873126745224,0.00418471964076161),
    nsn             = c(1.99099254608154,98.4806365966797,59.0609397888184,2.35829377174377,0.296707808971405,0.0157136470079422),
    treeG           = c(1.38240838050842,19.4165687561035,13.7977523803711,0.350733578205109,0.0369470864534378,0.0105907050892711),
    fseed           = c(0.0519081875681877,0.059250246733427,0.058679174631834,0.0330535434186459,0.670021772384644,0),
    fleaf           = c(0.134158194065094,0.0987088829278946,0.101414486765862,0.0458624847233295,0.0212229881435633,0.313251167535782),
    froot           = c(0.346759855747223,0.308788985013962,0.311794072389603,0.45660537481308,0.259258568286896,0.363957613706589),
    fwood           = c(0.46717369556427,0.533251881599426,0.528112292289734,0.464478641748428,0.0494967177510262,0.322791159152985),
    GPP             = c(2.39950299263,31.5417327880859,22.540979385376,0.247082218527794,0.0197645165026188,0.00617480790242553),
    NPP             = c(1.60801732540131,20.4554824829102,14.665210723877,0.0583550482988358,0.012063343077898,0.000449975952506065),
    Rauto           = c(0.791485667228699,11.0862493515015,7.87576818466187,0.188727170228958,0.00770117342472076,0.00572483194991946),
    Nupt            = c(0.0258084423840046,0.290791153907776,0.208818271756172,0.000735382083803415,0.000264835951384157,0),
    Nfix            = c(0,0,0,0,0,0),
    deathrate       = c(0.0181079730391502,0.113263040781021,0.0839816629886627,0.152398899197578,0.266000390052795,0.351924955844879),
    n_deadtrees     = c(0.00199735490605235,0.00616554636508226,0.000313321856083348,0.00161606096662581,3.86729298043065e-05,0.0038769650273025),
    c_deadtrees     = c(0.0918554291129112,0.640294253826141,0.029240844771266,0.0901064053177834,0.00168161757756025,0.0465278774499893))
  ref_BiomeE_PLULUC_secondary_odt_yr1 <- tibble(
    year            = c(1,1,1,1,1),
    doy             = c(1,2,180,364,365),
    Tk              = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp            = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    totWs           = c(799.944641113281,799.947204589844,799.131042480469,799.930847167969,799.903991699219),
    Trsp            = c(0,0,0,0,0),
    Evap            = c(0.0553628727793694,0.0527696460485458,0.868921220302582,0.0691292807459831,0.0960086733102798),
    Runoff          = c(1.43660509586334,1.94845604896545,1.08043730258942,1.96995043754578,1.75696134567261),
    ws1             = c(19.944637298584,19.9472312927246,19.1310787200928,19.930871963501,19.9039936065674),
    ws2             = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3             = c(600,600,600,600,600),
    LAI             = c(0,0.000364852574421093,0.00466133235022426,0.00618919776752591,0.00619784602895379),
    GPP             = c(0,0,1.90892133105081e-05,6.17546766079613e-06,6.16563283983851e-06),
    Rauto           = c(2.97977797991678e-09,5.82730172027368e-05,4.93635025122785e-06,4.55608324045897e-06,4.57825399280409e-06),
    Rh              = c(4.38448614659137e-06,4.35115134678199e-06,1.62614360306179e-05,3.65712071470625e-06,3.64831771548779e-06),
    NSC             = c(0.00582691049203277,0.00565209938213229,0.00398708041757345,0.00424133753404021,0.00423404527828097),
    seedC           = c(0,0,0,0,0),
    leafC           = c(0,6.20249411440454e-05,0.000792426522821188,0.00105216365773231,0.00105363386683166),
    rootC           = c(0,3.70325360563584e-05,0.000473125197459012,0.000628203561063856,0.000629081332590431),
    SW_C            = c(0.00250000017695129,0.00163697078824043,0.000808653887361288,0.00114870828110725,0.00115068175364286),
    HW_C            = c(0,0.000880510022398084,0.00244240881875157,0.002835190622136,0.00283709052018821),
    NSN             = c(0.000293089426122606,0.000291046482743695,0.00025848179939203,0.000238109059864655,0.000237988890148699),
    seedN           = c(0,0,0,0,0),
    leafN           = c(0,1.06719380710274e-06,1.36344051497872e-05,1.81034265551716e-05,1.81287232408067e-05),
    rootN           = c(0,9.25813424146327e-07,1.18281122922781e-05,1.57050762936706e-05,1.57270224008244e-05),
    SW_N            = c(7.14285715730512e-06,4.67705922346795e-06,2.31044009524339e-06,3.28202372656961e-06,3.28766213897325e-06),
    HW_N            = c(0,2.5157430627587e-06,6.97831228535506e-06,8.10054189059883e-06,8.10597066447372e-06),
    McrbC           = c(5.84022927796468e-07,1.16322576104722e-06,0.000156941576278768,0.00036603509215638,0.000366281310562044),
    fastSOM         = c(0.0158219542354345,0.0158170331269503,0.0146148949861526,0.0123640624806285,0.0123623143881559),
    slowSOM         = c(0.000999990850687027,0.000999981770291924,0.00105635263025761,0.00113423995207995,0.00113475287798792),
    McrbN           = c(5.84022927796468e-08,1.16322574683636e-07,1.56941569002811e-05,3.6603509215638e-05,3.66281310562044e-05),
    fastSoilN       = c(0.000959546654485166,0.000959338853135705,0.000903414911590517,0.000793190556578338,0.000793060928117484),
    slowSoilN       = c(2.49997719947714e-05,2.4999544621096e-05,2.58212767221266e-05,2.69752108579269e-05,2.69825650320854e-05),
    mineralN        = c(0.0149248940870166,0.0148505019024014,0.00587352132424712,0.00321272760629654,0.00321828341111541),
    N_uptk          = c(0,0,0,0,0))
  ref_BiomeE_PLULUC_secondary_odt_yr251 <- tibble(
    year            = c(251,251,251,251,251),
    doy             = c(1,2,180,364,365),
    Tk              = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp            = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    totWs           = c(799.960083007812,799.958984375,799.516296386719,799.951416015625,799.930053710938),
    Trsp            = c(0,0,0,0,0),
    Evap            = c(0.0399133116006851,0.0409998670220375,0.483673214912415,0.0485905706882477,0.0699304714798927),
    Runoff          = c(1.36660242080688,1.96390557289124,1.74325275421143,1.99354791641235,1.77749955654144),
    ws1             = c(19.9600887298584,19.9590015411377,19.5163269042969,19.9514083862305,19.9300708770752),
    ws2             = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3             = c(600,600,600,600,600),
    LAI             = c(3.49222898483276,3.49872851371765,3.61742806434631,3.74305105209351,3.74376749992371),
    GPP             = c(0.00227762712165713,0.00224546412937343,0.00936589296907187,0.00246698595583439,0.00246065971441567),
    Rauto           = c(6.12458316027187e-05,0.00236864062026143,0.00243193446658552,0.00197466905228794,0.00198433012701571),
    Rh              = c(0.00142560992389917,0.00141680822707713,0.00560735166072845,0.00138231704477221,0.00138092646375299),
    NSC             = c(2.14304709434509,2.1382839679718,2.130375623703,2.23058581352234,2.22722291946411),
    seedC           = c(0.00199680402874947,0.00217894441448152,0.0329008847475052,0.0699842646718025,0.0701815709471703),
    leafC           = c(0.593678891658783,0.594783782958984,0.614962816238403,0.636318683624268,0.636440515518188),
    rootC           = c(0.360928356647491,0.360858350992203,0.365907192230225,0.379917025566101,0.37998965382576),
    SW_C            = c(4.56769132614136,4.55334520339966,4.69937133789062,4.90116834640503,4.90223836898804),
    HW_C            = c(3.35169768333435,3.36794447898865,3.51899337768555,3.66643047332764,3.66720056533813),
    NSN             = c(0.0614155009388924,0.0614405274391174,0.0627359002828598,0.0640160739421844,0.0639596432447433),
    seedN           = c(9.9840181064792e-05,0.000108947206172161,0.00164504384156317,0.00349921360611916,0.00350907887332141),
    leafN           = c(0.0102148419246078,0.010233853943646,0.0105810556560755,0.0109485043212771,0.0109505988657475),
    rootN           = c(0.00902318954467773,0.00902143865823746,0.00914766173809767,0.00949790142476559,0.00949971750378609),
    SW_N            = c(0.013050545938313,0.0130095593631268,0.0134267751127481,0.0140033364295959,0.0140063930302858),
    HW_N            = c(0.00957628432661295,0.00962270237505436,0.0100542698055506,0.0104755200445652,0.0104777198284864),
    McrbC           = c(0.19457171857357,0.19457446038723,0.195704430341721,0.194694697856903,0.19469153881073),
    fastSOM         = c(2.17454314231873,2.17509269714355,2.18391823768616,2.01643490791321,2.01709246635437),
    slowSOM         = c(76.9139709472656,76.9135284423828,76.7411727905273,76.3387908935547,76.3383636474609),
    McrbN           = c(0.0194571726024151,0.019457446411252,0.0195704437792301,0.0194694697856903,0.0194691531360149),
    fastSoilN       = c(0.113213919103146,0.113225139677525,0.112775057554245,0.106536857783794,0.106550313532352),
    slowSoilN       = c(1.5009777545929,1.50097858905792,1.50067591667175,1.49933218955994,1.49933326244354),
    mineralN        = c(0.00285031390376389,0.00278910622000694,0.00198135781101882,0.00279475073330104,0.0028261945117265),
    N_uptk          = c(0,9.22516119317152e-05,0,0,0))
  ref_BiomeE_PLULUC_secondary_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00416006147861481,0.0051026726141572,0.013795854523778,0.0158615466207266,0.0471739545464516,1.34490156173706),
    LAI             = c(0.00620649103075266,0.00972458533942699,0.043438233435154,0.0499434173107147,0.144030258059502,3.74448251724243),
    Density         = c(500,494.722686767578,462.535614013672,456.990539550781,1980.70007324219,46017.578125),
    DBH             = c(0.67508590221405,0.77904599905014,1.58127772808075,1.74943602085114,1.17045211791992,1.05999433994293),
    Density12       = c(0,0,0,0,0,214.249740600586),
    DBH12           = c(0,0,0,0,0,21.6128616333008),
    QMD12           = c(0,0,0,0,0,21.6495323181152),
    NPP             = c(0.00230822968296707,0.00474032014608383,0.0180877950042486,0.0207991693168879,0.0605944767594337,1.43212604522705),
    GPP             = c(0.00443602073937654,0.00687614921480417,0.0263272896409035,0.0303885489702225,0.0890411883592606,2.20210719108582),
    Rauto           = c(0.00212779105640948,0.00213582930155098,0.00823949463665485,0.00958937965333462,0.028446713462472,0.76998108625412),
    Rh              = c(0.00369418039917946,0.00311857811175287,0.00364138255827129,0.0041773971170187,0.0118735916912556,1.29935908317566),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903991699219,799.904052734375,799.904724121094,799.904907226562,799.90673828125,799.930053710938),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.599700927734,159.441146850586,157.805053710938,157.495330810547,153.281402587891,94.2941131591797),
    Runoff          = c(428.025787353516,428.088317871094,429.724395751953,430.034057617188,434.247924804688,493.235260009766),
    plantC          = c(0.00990187004208565,0.0133129954338074,0.0604372769594193,0.0732633098959923,0.244614720344543,11.8816652297974),
    soilC           = c(0.0138660119846463,0.0120752677321434,0.0208898894488811,0.0246748328208923,0.0789770632982254,78.5517578125),
    plantN          = c(0.000283176312223077,0.000251700810622424,0.00107148278038949,0.0012657477054745,0.00361122959293425,0.112365745007992),
    soilN           = c(0.00407501682639122,0.00362333352677524,0.00357315177097917,0.00367704103700817,0.00531120225787163,1.62821638584137),
    totN            = c(0.00435819290578365,0.00387503439560533,0.00464463466778398,0.00494278874248266,0.00892243161797523,1.74058210849762),
    NSC             = c(0.00422516884282231,0.00518226251006126,0.0209579616785049,0.0248370189219713,0.0757390111684799,2.22338676452637),
    SeedC           = c(0,0,0.00153295940253884,0.00226585427299142,0.00386109319515526,0.0703786090016365),
    leafC           = c(0.0010551034938544,0.00165317952632904,0.00738449953496456,0.00849038176238537,0.0244851429015398,0.636561989784241),
    rootC           = c(0.000629958754871041,0.00098704535048455,0.00440898025408387,0.00506925676018,0.0146190682426095,0.380062103271484),
    SapwoodC        = c(0.00115265452768654,0.00201194896362722,0.0154777374118567,0.0192941222339869,0.0739047154784203,4.9033055305481),
    WoodC           = c(0.00283898459747434,0.00347855873405933,0.0106751378625631,0.0133066764101386,0.05200569704175,3.66796970367432),
    NSN             = c(0.000237868676776998,0.000182893098099157,0.00068283051950857,0.000786493357736617,0.00227166432887316,0.063903234899044),
    SeedN           = c(0,0,7.66480443417095e-05,0.00011329265544191,0.000193054700503126,0.00351893017068505),
    leafN           = c(1.8154007193516e-05,2.84444759017788e-05,0.000127057341160253,0.000146085236337967,0.000421290373196825,0.0109526915475726),
    rootN           = c(1.57489575940417e-05,2.46760791924316e-05,0.000110224362288136,0.000126731349155307,0.000365475978469476,0.00950152799487114),
    SapwoodN        = c(3.29329850501381e-06,5.74842533751507e-06,4.42221098637674e-05,5.51260636711959e-05,0.000211156337172724,0.0140094440430403),
    WoodN           = c(8.11138215794927e-06,9.93873254628852e-06,3.05004105030093e-05,3.80190831492655e-05,0.00014858775830362,0.0104799177497625),
    McrbC           = c(0.000366281310562044,0.000509715406224132,0.000713580346200615,0.000805256073363125,0.0022125612013042,0.19469153881073),
    fastSOM         = c(0.0123644527047873,0.0101484293118119,0.013076089322567,0.0150239123031497,0.0427190214395523,2.01838397979736),
    SlowSOM         = c(0.00113527732901275,0.00141712243203074,0.00710021937265992,0.0088456654921174,0.0340454839169979,76.3386840820312),
    McrbN           = c(3.66281310562044e-05,5.09715391672216e-05,7.13580375304446e-05,8.0525605881121e-05,0.000221256123040803,0.0194691531360149),
    fastSoilN       = c(0.000793115410488099,0.000663830142002553,0.000609932118095458,0.000685109349433333,0.00187277025543153,0.10658323019743),
    slowSoilN       = c(2.69899937848095e-05,3.19643804687075e-05,0.000138284027343616,0.000170996616361663,0.000630303169600666,1.49933779239655),
    mineralN        = c(0.00321828341111541,0.00287656742148101,0.00275357742793858,0.00274040945805609,0.00258687301538885,0.0028261945117265),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000328723050188273,0.000375714793335646,0.00102128251455724,0.0221788547933102),
    N_yrMin         = c(-0.0117817036807537,-0.000341718521667644,0.000316788908094168,0.000362546648830175,0.00100449880119413,0.0221861302852631),
    N_P2S           = c(2.14073552342597e-05,3.33384523401037e-05,0.000169738064869307,0.000196982437046245,0.000581474509090185,0.0272052995860577),
    N_loss          = c(0.021926736459136,0.0104845138266683,0.00975676625967026,0.00971267651766539,0.00916490983217955,0.00926990620791912),
    totseedC        = c(0,0,0,0,0.00356994615867734,0.0649071559309959),
    totseedN        = c(0,0,0,0,0.000178497357410379,0.00324535765685141),
    Seedling_C      = c(0,0,0,0,0.00356994615867734,0.0649071559309959),
    Seedling_N      = c(0,0,0,0,0.000178497342858464,0.00324535788968205),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627334594727),
    MaxVolume       = c(0.000215764259337448,0.000299950072076172,0.00152817298658192,0.0019280546111986,0.00737557932734489,0.652382910251617),
    MaxDBH          = c(0.00675085838884115,0.00779045978561044,0.0158127769827843,0.0174943599849939,0.0313498750329018,0.220105022192001),
    NPPL            = c(0.00121877959463745,0.0008825832628645,0.00248827692121267,0.002812419552356,0.00808483455330133,0.169744551181793),
    NPPW            = c(0.00149163894820958,0.00154099601786584,0.00566299306228757,0.00676144752651453,0.02101119607687,0.651887238025665),
    n_deadtrees     = c(4.3513900891412e-06,4.85187956655864e-06,2.36771447816864e-05,2.83782865153626e-05,9.52274349401705e-05,0.0140119269490242),
    c_deadtrees     = c(0.00010314847168047,0.0001401223562425,0.000713716843165457,0.000889366143383086,0.0035840324126184,0.899706304073334),
    m_turnover      = c(0.00010314847168047,0.0001401223562425,0.000713716843165457,0.000889366143383086,0.0035840324126184,0.899706304073334),
    c_turnover_time = c(1.90326523780823,2.25734448432922,1.88506996631622,1.96802186965942,2.47514224052429,5.62669372558594),
    lu_fraction     = c(0.400000005960464,0.400000005960464,0.400000005960464,0.400000005960464,0.400000005960464,0.400000005960464))
  ref_BiomeE_PLULUC_secondary_oac_yr1 <- tibble(
    cohort          = 1,
    year            = 1,
    cID             = 1,
    PFT             = 2,
    layer           = 1,
    density         = 500,
    flayer          = 0.00416006147861481,
    DBH             = 0.675085842609406,
    dDBH            = 0.124270185828209,
    height          = 2.95789003372192,
    age             = 1.00000047683716,
    BA              = 3.57938079105224e-05,
    dBA             = 1.19649921543896e-05,
    Acrown          = 0.0832012295722961,
    Aleaf           = 0.124129816889763,
    nsc             = 0.0845033749938011,
    seedC           = 0.00475737359374762,
    leafC           = 0,
    rootC           = 0.0211020689457655,
    sapwC           = 0.0125991748645902,
    woodC           = 0.0230530891567469,
    nsn             = 0.0567796900868416,
    treeG           = 0.0781993493437767,
    fseed           = 0,
    fleaf           = 0.311710923910141,
    froot           = 0.306792557239532,
    fwood           = 0.381496518850327,
    GPP             = 0.0887204110622406,
    NPP             = 0.0461645908653736,
    Rauto           = 0.042555820196867,
    Nupt            = 0,
    Nfix            = 0,
    deathrate       = 0.0105546750128269,
    n_deadtrees     = 4.3513900891412e-06,
    c_deadtrees     = 0.00010314847168047)
  ref_BiomeE_PLULUC_secondary_oac_yr2 <- tibble(
    cohort          = 1,
    year            = 2,
    cID             = 1,
    PFT             = 2,
    layer           = 1,
    density         = 494.722686767578,
    flayer          = 0.0051026726141572,
    DBH             = 0.77904599905014,
    dDBH            = 0.103960141539574,
    height          = 3.17748880386353,
    age             = 1.99997925758362,
    BA              = 4.76668064948171e-05,
    dBA             = 1.18729985842947e-05,
    Acrown          = 0.103142082691193,
    Aleaf           = 0.196566388010979,
    nsc             = 0.104750856757164,
    seedC           = 0.00369688123464584,
    leafC           = 0,
    rootC           = 0.033416286110878,
    sapwC           = 0.0199514869600534,
    woodC           = 0.0406682156026363,
    nsn             = 0.0703133046627045,
    treeG           = 0.0755703151226044,
    fseed           = 0,
    fleaf           = 0.236070990562439,
    froot           = 0.35174748301506,
    fwood           = 0.412181466817856,
    GPP             = 0.138989970088005,
    NPP             = 0.0958177149295807,
    Rauto           = 0.0431722514331341,
    Nupt            = 0,
    Nfix            = 0,
    deathrate       = 0.0106876138597727,
    n_deadtrees     = 4.85187956655864e-06,
    c_deadtrees     = 0.0001401223562425)
  ref_BiomeE_PLULUC_secondary_oac_yr251 <- tibble(
    cohort          = c(1,2,3,4,5,6),
    year            = c(251,251,251,251,251,251),
    cID             = c(453,399,405,451,433,25),
    PFT             = c(2,2,2,2,2,2),
    layer           = c(1,1,1,2,2,2),
    density         = c(6186.8037109375,194.83171081543,19.4180202484131,1437.65393066406,133.359298706055,38045.51171875),
    flayer          = c(0.752436935901642,0.301783740520477,0.0215486623346806,0.137903317809105,0.00330992136150599,0.12791895866394),
    DBH             = c(4.03591156005859,22.0105018615723,17.6231174468994,3.44523930549622,1.39894962310791,0.369000375270844),
    dDBH            = c(0.241623073816299,0.415582954883575,0.391329824924469,0.0729750841856003,0.00261003151535988,0.0290364250540733),
    height          = c(7.23224830627441,16.8895263671875,15.1127624511719,6.68208789825439,4.25797939300537,2.18683457374573),
    age             = c(25.6355743408203,77.7627258300781,77.7627334594727,25.6355743408203,22.6215171813965,2.81209349632263),
    BA              = c(0.00127930240705609,0.0380495749413967,0.0243924465030432,0.000932242081034929,0.000153707136632875,1.06940833575209e-05),
    dBA             = c(0.000148594030179083,0.00142327323555946,0.00107126496732235,3.90742206946015e-05,5.73010765947402e-07,1.61680509336293e-06),
    Acrown          = c(1.21619653701782,15.4894561767578,11.0972499847412,0.959224700927734,0.248195767402649,0.0336226150393486),
    Aleaf           = c(3.83005428314209,48.7879486083984,34.9530982971191,1.5106908082962,0.0880047231912613,0.0363061539828777),
    nsc             = c(2.2883026599884,35.3928833007812,24.9768085479736,0.331826537847519,0.000859237799886614,0.00575008150190115),
    seedC           = c(0.0601347424089909,0.767691552639008,0.548487424850464,0.0238057114183903,0.00614411663264036,0.00188525626435876),
    leafC           = c(0.0717583522200584,1.15043652057648,0.809639990329742,0.0115929869934916,0.0247552022337914,0),
    rootC           = c(0.651109218597412,8.2939510345459,5.94202709197998,0.256817430257797,0.014960803091526,0.00617204606533051),
    sapwC           = c(0.388750463724136,4.95197582244873,3.54773926734924,0.153335094451904,0.0087192403152585,0.00368507415987551),
    woodC           = c(2.88789963722229,142.90104675293,85.6981048583984,1.03217899799347,0.129872813820839,0.00418471451848745),
    nsn             = c(1.99099385738373,98.4806365966797,59.0609588623047,2.35829281806946,0.296707034111023,0.0157136432826519),
    treeG           = c(1.38240885734558,19.4165687561035,13.7977504730225,0.350733309984207,0.0369468815624714,0.0105907050892711),
    fseed           = c(0.0519081987440586,0.059250246733427,0.0586791299283504,0.0330535657703876,0.670021414756775,0),
    fleaf           = c(0.134158238768578,0.0987088829278946,0.101414784789085,0.0458623692393303,0.0212228074669838,0.313251227140427),
    froot           = c(0.346759766340256,0.308788985013962,0.31179416179657,0.456605494022369,0.259258955717087,0.363957554101944),
    fwood           = c(0.467173844575882,0.533251881599426,0.528111934661865,0.464478582143784,0.0494967699050903,0.322791188955307),
    GPP             = c(2.39950466156006,31.5417327880859,22.5409812927246,0.24708203971386,0.0197644270956516,0.00617480464279652),
    NPP             = c(1.60801827907562,20.4554824829102,14.665210723877,0.0583549737930298,0.0120632881298661,0.000449972227215767),
    Rauto           = c(0.791486382484436,11.0862493515015,7.8757700920105,0.18872706592083,0.0077011389657855,0.00572483241558075),
    Nupt            = c(0.0257927291095257,0.292662113904953,0.209771111607552,0.000754328153561801,0.000272400298854336,0),
    Nfix            = c(0,0,0,0,0,0),
    deathrate       = c(0.0181079767644405,0.113263040781021,0.0839816629886627,0.152398928999901,0.266000509262085,0.351924955844879),
    n_deadtrees     = c(0.00199693441390991,0.0061703734099865,0.000313190830638632,0.00161577318795025,3.86895844712853e-05,0.00387696595862508),
    c_deadtrees     = c(0.0918554738163948,0.640294253826141,0.0292408317327499,0.0901062712073326,0.00168160616885871,0.0465278774499893))
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$aggregated|>filter(                           year %in% c(1, 2, 8, 9, 16, 251))),   ref_BiomeE_PLULUC_aggregated)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$primary$output_daily_tile|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365))),     ref_BiomeE_PLULUC_primary_odt_yr1)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$primary$output_daily_tile|>filter(year==251, doy %in% c(1, 2, 180, 364, 365))),     ref_BiomeE_PLULUC_primary_odt_yr251)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$primary$output_annual_tile|>filter(           year %in% c(1, 2, 8, 9, 16, 251))),   ref_BiomeE_PLULUC_primary_oat)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$primary$output_annual_cohorts|>filter(year==  1)),                                  ref_BiomeE_PLULUC_primary_oac_yr1)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$primary$output_annual_cohorts|>filter(year==  2)),                                  ref_BiomeE_PLULUC_primary_oac_yr2)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$primary$output_annual_cohorts|>filter(year==251)),                                  ref_BiomeE_PLULUC_primary_oac_yr251)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$secondary$output_daily_tile|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365))),     ref_BiomeE_PLULUC_secondary_odt_yr1)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$secondary$output_daily_tile|>filter(year==251, doy %in% c(1, 2, 180, 364, 365))),     ref_BiomeE_PLULUC_secondary_odt_yr251)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$secondary$output_annual_tile|>filter(           year %in% c(1, 2, 8, 9, 16, 251))),   ref_BiomeE_PLULUC_secondary_oat)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$secondary$output_annual_cohorts|>filter(year==  1)),                                  ref_BiomeE_PLULUC_secondary_oac_yr1)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$secondary$output_annual_cohorts|>filter(year==  2)),                                  ref_BiomeE_PLULUC_secondary_oac_yr2)
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$secondary$output_annual_cohorts|>filter(year==251)),                                  ref_BiomeE_PLULUC_secondary_oac_yr251)
})


