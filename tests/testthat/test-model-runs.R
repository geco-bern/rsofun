context("test models and their parameters")
set.seed(10)

# Output checks
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
  expect_true(all.equal(colMeans(out$aggregated[[1]]$output_annual_cell), colMeans(biomee_p_model_luluc_output$aggregated[[1]]$output_annual_cell), tolerance = 1e-4))

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

# Parallel run checks
test_that("biomee parallel run check (p-model)", {
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
  
  df_drivers_BiomeE_PLULUC$params_siml[[1]]$do_daily_diagnostics <- TRUE
  
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
  
  expect_equal(tibble(mod_BiomeE_PLULUC$aggregated$output_annual_cell),    tibble(mod_BiomeE_PLULUC_2ndTry$aggregated$output_annual_cell),   tolerance = 1e-6)
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
    LAI       = c(3.4922297000885,3.49872899055481,3.61742806434631,3.74305200576782,3.74376797676086),
    GPP       = c(0.00227762758731842,0.00224546459503472,0.00936589390039444,0.00246698618866503,0.00246065971441567),
    Rauto     = c(6.12458534305915e-05,0.00236861989833415,0.0024318634532392,0.00197463692165911,0.00198430172167718),
    Rh        = c(0.00146035756915808,0.00145140045788139,0.00574139412492514,0.00141523755155504,0.001413878868334),
    NSC       = c(2.14304780960083,2.13828492164612,2.1303768157959,2.2305862903595,2.22722339630127),
    seedC     = c(0.0019968063570559,0.0021789469756186,0.0329009033739567,0.0699842870235443,0.070181593298912),
    leafC     = c(0.593679130077362,0.594783842563629,0.614962816238403,0.636318802833557,0.636440634727478),
    rootC     = c(0.360928446054459,0.36085844039917,0.365907251834869,0.379917114973068,0.379989683628082),
    SW_C      = c(4.5676908493042,4.55334377288818,4.69937181472778,4.90116739273071,4.90223789215088),
    HW_C      = c(3.35169792175293,3.36794519424438,3.51899361610413,3.66643118858337,3.66720104217529),
    NSN       = c(0.0614702217280865,0.0614091902971268,0.0626659020781517,0.0639448463916779,0.0638884156942368),
    seedN     = c(9.98403047560714e-05,0.000108947337139398,0.00164504500571638,0.00349921477027237,0.00350908027030528),
    leafN     = c(0.0102148456498981,0.0102338567376137,0.0105810575187206,0.0109485015273094,0.0109505970031023),
    rootN     = c(0.00902319885790348,0.00902144890278578,0.00914766546338797,0.00949791260063648,0.00949972867965698),
    SW_N      = c(0.0130505440756679,0.0130095547065139,0.0134267760440707,0.0140033354982734,0.0140063930302858),
    HW_N      = c(0.00957628153264523,0.00962270423769951,0.0100542698055506,0.0104755172505975,0.0104777161031961),
    McrbC     = c(0.163020133972168,0.163022756576538,0.164115786552429,0.163002550601959,0.162999197840691),
    fastSOM   = c(2.19501852989197,2.19556164741516,2.2025363445282,2.03177905082703,2.03243160247803),
    slowSOM   = c(72.7654800415039,72.765007019043,72.5841827392578,72.1644821166992,72.1640319824219),
    McrbN     = c(0.0163020137697458,0.0163022764027119,0.0164115782827139,0.016300255432725,0.0162999201565981),
    fastSoilN = c(0.113785140216351,0.113796405494213,0.113353185355663,0.107105150818825,0.107118561863899),
    slowSoilN = c(0.675333976745605,0.675336599349976,0.675630569458008,0.675470352172852,0.675473272800446),
    mineralN  = c(0.00221035978756845,0.00223766081035137,0.00152802339289337,0.00226603355258703,0.00229924730956554),
    N_uptk    = c(0.000141792901558802,6.19682805336197e-06,0,0,0))
  ref_BiomeE_Pmodel_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00416006147861481,0.0051026726141572,0.013795854523778,0.0158615466207266,0.0471739545464516,1.34490180015564),
    LAI             = c(0.00620649103075266,0.00972458533942699,0.043438233435154,0.0499434173107147,0.144030258059502,3.74448347091675),
    Density         = c(500,494.722686767578,462.535614013672,456.990539550781,1980.70007324219,46017.58984375),
    DBH             = c(0.67508590221405,0.77904599905014,1.58127772808075,1.74943602085114,1.17045211791992,1.05999422073364),
    Density12       = c(0,0,0,0,0,214.249237060547),
    DBH12           = c(0,0,0,0,0,21.6128730773926),
    QMD12           = c(0,0,0,0,0,21.6495418548584),
    NPP             = c(0.00230822968296707,0.00474032014608383,0.0180877950042486,0.0207991693168879,0.0605944767594337,1.43212604522705),
    GPP             = c(0.00443602073937654,0.00687614921480417,0.0263272896409035,0.0303885489702225,0.0890411883592606,2.20210719108582),
    Rauto           = c(0.00212779105640948,0.00213582930155098,0.00823949463665485,0.00958937965333462,0.028446713462472,0.769981145858765),
    Rh              = c(0.00236286479048431,0.0020676152780652,0.00343759660609066,0.00402783416211605,0.0118935955688357,1.33055472373962),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903991699219,799.904052734375,799.904724121094,799.904907226562,799.90673828125,799.930053710938),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.599700927734,159.441146850586,157.805053710938,157.495330810547,153.281402587891,94.2941131591797),
    Runoff          = c(428.025726318359,428.088317871094,429.724395751953,430.034057617188,434.247924804688,493.235260009766),
    plantC          = c(0.00990187004208565,0.0133129954338074,0.0604372769594193,0.0732633098959923,0.244614720344543,11.8816652297974),
    soilC           = c(0.00937039777636528,0.00863198284059763,0.0202523395419121,0.0241976678371429,0.0789027363061905,74.3610763549805),
    plantN          = c(0.000283176312223077,0.000251700810622424,0.00107308442238718,0.00126732815988362,0.00361604942008853,0.112294517457485),
    soilN           = c(0.00379347242414951,0.00337960757315159,0.00347751541994512,0.00358472322113812,0.00509759178385139,0.801228404045105),
    totN            = c(0.00407664850354195,0.00363130844198167,0.00455059995874763,0.00485205138102174,0.00871364120393991,0.913522899150848),
    NSC             = c(0.00422516884282231,0.00518226251006126,0.0209579616785049,0.0248370189219713,0.0757390111684799,2.22338652610779),
    SeedC           = c(0,0,0.00153295940253884,0.00226585427299142,0.00386109319515526,0.0703786239027977),
    leafC           = c(0.0010551034938544,0.00165317952632904,0.00738449953496456,0.00849038176238537,0.0244851429015398,0.63656222820282),
    rootC           = c(0.000629958754871041,0.00098704535048455,0.00440898025408387,0.00506925676018,0.0146190682426095,0.380062222480774),
    SapwoodC        = c(0.00115265452768654,0.00201194896362722,0.0154777374118567,0.0192941222339869,0.0739047154784203,4.90330600738525),
    WoodC           = c(0.00283898459747434,0.00347855873405933,0.0106751378625631,0.0133066764101386,0.05200569704175,3.6679699420929),
    NSN             = c(0.000237868676776998,0.000182893098099157,0.000684432161506265,0.000788073870353401,0.00227648415602744,0.0638319998979568),
    SeedN           = c(0,0,7.66480443417095e-05,0.00011329265544191,0.000193054700503126,0.00351893133483827),
    leafN           = c(1.8154007193516e-05,2.84444759017788e-05,0.000127057341160253,0.000146085236337967,0.000421290373196825,0.01095269061625),
    rootN           = c(1.57489575940417e-05,2.46760791924316e-05,0.000110224362288136,0.000126731349155307,0.000365475978469476,0.00950154103338718),
    SapwoodN        = c(3.29329850501381e-06,5.74842533751507e-06,4.42221098637674e-05,5.51260636711959e-05,0.000211156337172724,0.0140094440430403),
    WoodN           = c(8.11138215794927e-06,9.93873254628852e-06,3.05004105030093e-05,3.80190831492655e-05,0.00014858775830362,0.0104799130931497),
    McrbC           = c(0.000234328705118969,0.000333371630404145,0.000644201005343348,0.000751312880311161,0.00220260047353804,0.162999197840691),
    fastSOM         = c(0.00800079107284546,0.0068816039711237,0.0125162834301591,0.014612790197134,0.0427354797720909,2.03372311592102),
    SlowSOM         = c(0.00113527732901275,0.00141700753010809,0.00709185423329473,0.008833565749228,0.0339646562933922,72.1643524169922),
    McrbN           = c(2.34328708756948e-05,3.3337164495606e-05,6.44200990791433e-05,7.51312909414992e-05,0.000220260044443421,0.0162999201565981),
    fastSoilN       = c(0.000531830999534577,0.000451580359367654,0.000561729830224067,0.000648404587991536,0.00186946208123118,0.107151478528976),
    slowSoilN       = c(2.69899937848095e-05,3.06047149933875e-05,0.00010392205149401,0.000125899474369362,0.000426499871537089,0.675477743148804),
    mineralN        = c(0.00321121863089502,0.00286408537067473,0.00274744350463152,0.00273528788238764,0.00258136983029544,0.00229924730956554),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000328867841744795,0.000375712901586667,0.0010222572600469,0.0221947133541107),
    N_yrMin         = c(-0.0117887742817402,-0.000347134715411812,0.000317854312015697,0.000363559287507087,0.00100462301634252,0.0221746861934662),
    N_P2S           = c(2.00447993847774e-05,3.11766561935656e-05,0.000158925526193343,0.000184209915460087,0.000538545835297555,0.0222442373633385),
    N_loss          = c(0.0219152644276619,0.010445331223309,0.00973904691636562,0.00969855580478907,0.00916206929832697,0.00753329368308187),
    totseedC        = c(0,0,0,0,0.00356994615867734,0.0649071782827377),
    totseedN        = c(0,0,0,0,0.000178497357410379,0.00324535882100463),
    Seedling_C      = c(0,0,0,0,0.00356994615867734,0.0649071782827377),
    Seedling_N      = c(0,0,0,0,0.000178497342858464,0.00324535858817399),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627563476562),
    MaxVolume       = c(0.000215764259337448,0.000299950072076172,0.00152817298658192,0.0019280546111986,0.00737557932734489,0.652383387088776),
    MaxDBH          = c(0.00675085838884115,0.00779045978561044,0.0158127769827843,0.0174943599849939,0.0313498750329018,0.220105081796646),
    NPPL            = c(0.00121877959463745,0.0008825832628645,0.00248827692121267,0.002812419552356,0.00808483455330133,0.169744402170181),
    NPPW            = c(0.00149163894820958,0.00154099601786584,0.00566299306228757,0.00676144752651453,0.02101119607687,0.651887357234955),
    n_deadtrees     = c(2.9888342396589e-06,2.69008114628377e-06,1.28646079247119e-05,1.56057722051628e-05,5.22986738360487e-05,0.00905085541307926),
    c_deadtrees     = c(0.000104511025710963,0.000142284159664996,0.000724548590369523,0.000902158091776073,0.00362703530117869,0.904662847518921),
    m_turnover      = c(0.000104511025710963,0.000142284159664996,0.000724548590369523,0.000902158091776073,0.00362703530117869,0.904662847518921),
    c_turnover_time = c(1.90326523780823,2.25734448432922,1.88506996631622,1.96802186965942,2.47514224052429,5.62669277191162),
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
    totWs     = c(799.9970703125,799.996948242188,754.776611328125,799.996459960938,799.995849609375),
    Trsp      = c(0.0292789563536644,0.0221749134361744,2.29914855957031,0.0282737705856562,0.0520914532244205),
    Evap      = c(0.0411604531109333,0.0415968708693981,0.532630681991577,0.0503083132207394,0.0731317102909088),
    Runoff    = c(1.36480987071991,1.9401947259903,0,1.97796106338501,1.70130407810211),
    ws1       = c(19.9981384277344,19.9980201721191,19.9853935241699,19.9977245330811,19.997371673584),
    ws2       = c(179.999176025391,179.999145507812,157.093017578125,179.998977661133,179.998794555664),
    ws3       = c(599.999755859375,599.999755859375,577.698181152344,599.999755859375,599.999694824219),
    LAI       = c(2.71642017364502,2.71631741523743,2.5534508228302,2.83028817176819,2.82902693748474),
    GPP       = c(0.000462660245830193,0.000195858359802514,0.00715218530967832,0.000202659124624915,0.000482367642689496),
    Rauto     = c(0.000105033133877441,0.000737429596483707,0.00279316166415811,0.000530223769601434,0.000573102035559714),
    Rh        = c(0.000727456819731742,0.000724036071915179,0.00292010395787656,0.000717765477020293,0.000716762384399772),
    NSC       = c(0.144965410232544,0.143055111169815,0.27723091840744,0.179523959755898,0.178501188755035),
    seedC     = c(0.013337817043066,0.0133533850312233,0.0164945479482412,0.0229069758206606,0.0229256767779589),
    leafC     = c(0.461791336536407,0.461773931980133,0.434086710214615,0.481148988008499,0.48093456029892),
    rootC     = c(0.273726612329483,0.273656159639359,0.25735729932785,0.285051465034485,0.284763097763062),
    SW_C      = c(3.64496064186096,3.64468097686768,3.70324563980103,3.81750178337097,3.81761574745178),
    HW_C      = c(2.59264850616455,2.59320855140686,2.64516043663025,2.72638130187988,2.72647452354431),
    NSN       = c(0.00304208789020777,0.00302907708100975,0.00281427963636816,0.00313978921622038,0.00313498498871922),
    seedN     = c(0.000666890700813383,0.000667669111862779,0.000824727118015289,0.00114534795284271,0.00114628300070763),
    leafN     = c(0.00794553384184837,0.00794523395597935,0.00746885221451521,0.00827859714627266,0.00827490817755461),
    rootN     = c(0.00684316549450159,0.00684140529483557,0.00643393443897367,0.00712628616020083,0.00711907725781202),
    SW_N      = c(0.0104141738265753,0.0104133738204837,0.0105807008221745,0.0109071461483836,0.0109074730426073),
    HW_N      = c(0.00740752369165421,0.00740912463515997,0.00755755696445704,0.00778961926698685,0.00778988702222705),
    McrbC     = c(0.0912828966975212,0.0912832543253899,0.091776430606842,0.0913321822881699,0.0913312807679176),
    fastSOM   = c(1.23357093334198,1.23411810398102,1.26667034626007,1.20393180847168,1.20452332496643),
    slowSOM   = c(32.3874778747559,32.3873443603516,32.3175201416016,32.1459503173828,32.145824432373),
    McrbN     = c(0.00912828929722309,0.0091283256188035,0.00917764287441969,0.00913321785628796,0.00913312844932079),
    fastSoilN = c(0.0611010268330574,0.0611149147152901,0.0620325915515423,0.0601994059979916,0.0602141916751862),
    slowSoilN = c(0.320407330989838,0.320409864187241,0.320768505334854,0.320915341377258,0.320918023586273),
    mineralN  = c(0.000197294939425774,0.000197714121895842,0.000170465704286471,0.000196237597265281,0.000196585577214137),
    N_uptk    = c(1.35970831252052e-05,1.34878282551654e-05,4.73367508675437e-05,1.39498224598356e-05,1.39247413244448e-05))
  ref_BiomeE_gsLeun_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00400206865742803,0.00448572542518377,0.00750343827530742,0.00824577640742064,0.015996016561985,1.10443830490112),
    LAI             = c(0.00597201427444816,0.00855128560215235,0.0236289892345667,0.0259668566286564,0.0479755438864231,2.82776665687561),
    Density         = c(500,494.733184814453,463.607696533203,458.471374511719,874.311767578125,9592.0966796875),
    DBH             = c(0.65788322687149,0.714902698993683,1.05199158191681,1.12863123416901,1.0255788564682,3.30056095123291),
    Density12       = c(0,0,0,0,0,134.253936767578),
    DBH12           = c(0,0,0,0,0,16.8585395812988),
    QMD12           = c(0,0,0,0,0,16.9640731811523),
    NPP             = c(0.000455248868092895,0.00205271318554878,0.00669513875618577,0.00733748869970441,0.0132791725918651,0.789834439754486),
    GPP             = c(0.00266986410133541,0.00399639410898089,0.0115679241716862,0.0127315586432815,0.0236653909087181,1.39895606040955),
    Rauto           = c(0.00221461523324251,0.00194368103984743,0.0048727854155004,0.00539406994357705,0.010386218316853,0.60912162065506),
    Rh              = c(0.00236317655071616,0.00205450737848878,0.00226395367644727,0.00247024674899876,0.00461494782939553,0.672988712787628),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.997619628906,799.997619628906,799.997619628906,799.997619628906,799.997619628906,799.995849609375),
    Transp          = c(0.704922735691071,1.0637092590332,3.14139652252197,3.45765209197998,6.422682762146,375.697387695312),
    Evap            = c(176.998413085938,176.84342956543,175.920715332031,175.784408569336,174.523239135742,105.35230255127),
    Runoff          = c(409.847991943359,409.645812988281,408.499145507812,408.317230224609,406.602905273438,106.478538513184),
    plantC          = c(0.00805916078388691,0.00890088453888893,0.0226384159177542,0.0258787497878075,0.0573301501572132,7.51000690460205),
    soilC           = c(0.00935985427349806,0.00851631630212069,0.0132406055927277,0.0148677974939346,0.0315966829657555,33.4428863525391),
    plantN          = c(0.000283414934528992,0.000254236190812662,0.000544839189387858,0.00060827744891867,0.00117187574505806,0.0383445397019386),
    soilN           = c(0.000807353353593498,0.000737329595722258,0.000728220154996961,0.000770614948123693,0.00125773821491748,0.390489995479584),
    totN            = c(0.00109076825901866,0.00099156575743109,0.00127305928617716,0.00137889245525002,0.00242961384356022,0.428834527730942),
    NSC             = c(0.00267619988881052,0.00207317643798888,0.0055290344171226,0.00627270294353366,0.0132943727076054,0.177580147981644),
    SeedC           = c(0,0,0.000427327468059957,0.000620634760707617,0.000398915872210637,0.0229441840201616),
    leafC           = c(0.00101524242199957,0.00145371851976961,0.00401692790910602,0.00441436562687159,0.00815584324300289,0.480720341205597),
    rootC           = c(0.00060615933034569,0.000867955386638641,0.00239834189414978,0.00263563543558121,0.00486951740458608,0.284466713666916),
    SapwoodC        = c(0.00108643516432494,0.00165165890939534,0.0060768686234951,0.00706456881016493,0.0178665891289711,3.8177285194397),
    WoodC           = c(0.00267512421123683,0.00285437563434243,0.00418991595506668,0.00487084407359362,0.0127449110150337,2.72656750679016),
    NSN             = c(0.000240045483224094,0.000194650419871323,0.000365065439837053,0.000401300232624635,0.000802401336841285,0.00311649404466152),
    SeedN           = c(0,0,2.13663643080508e-05,3.10317336698063e-05,1.99457972485106e-05,0.00114720838610083),
    leafN           = c(1.74681463249726e-05,2.50125358434161e-05,6.91151726641692e-05,7.59534523240291e-05,0.000140329400892369,0.00827122293412685),
    rootN           = c(1.5153977983573e-05,2.16988610191038e-05,5.99584927840624e-05,6.58908247714862e-05,0.000121737823064905,0.00711166858673096),
    SapwoodN        = c(3.10409996018279e-06,4.71902512799716e-06,1.736248304951e-05,2.01844832190545e-05,5.10473946633283e-05,0.0109077943488955),
    WoodN           = c(7.64321612223284e-06,8.1553607742535e-06,1.19711958177504e-05,1.39167123052175e-05,3.64140396413859e-05,0.00779015105217695),
    McrbC           = c(0.000234114078921266,0.000331696297507733,0.000457518734037876,0.000494118605274707,0.000907054985873401,0.0913312807679176),
    fastSOM         = c(0.00799247156828642,0.00679179467260838,0.00791259575635195,0.00864304229617119,0.0160311684012413,1.20549154281616),
    SlowSOM         = c(0.00113326858263463,0.00139282585587353,0.00487049063667655,0.00573063641786575,0.0146584585309029,32.1460647583008),
    McrbN           = c(2.34114086197224e-05,3.31696282955818e-05,4.57518726761919e-05,4.94118612550665e-05,9.07055000425316e-05,0.00913312844932079),
    fastSoilN       = c(0.000531428260728717,0.000448410748504102,0.000382947531761602,0.000410433014621958,0.000741282419767231,0.0602388754487038),
    slowSoilN       = c(2.69610682153143e-05,3.02897315123118e-05,7.64911965234205e-05,8.79272192833014e-05,0.000204215742996894,0.320921421051025),
    mineralN        = c(0.000225552576011978,0.000225459472858347,0.000223029579501599,0.000222842820221558,0.000221534515731037,0.000196585577214137),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000144652876770124,0.000158941998961382,0.000267393159447238,0.0115823363885283),
    N_yrMin         = c(-0.0147743243724108,-9.26537495615776e-08,0.000144462479511276,0.000158756403834559,0.000267198047367856,0.0115820858627558),
    N_P2S           = c(1.98027591977734e-05,2.88893734250451e-05,8.7338441517204e-05,9.62792619247921e-05,0.000180414324859157,0.0113513059914112),
    N_loss          = c(0.0249017998576164,0.010099139995873,0.00990787427872419,0.00989393331110477,0.00981921050697565,0.00834961421787739),
    totseedC        = c(0,0,0,0,0,0.00305297644808888),
    totseedN        = c(0,0,0,0,0,0.000152648848597892),
    Seedling_C      = c(0,0,0,0,0,0.00305297644808888),
    Seedling_N      = c(0,0,0,0,0,0.000152648848597892),
    MaxAge          = c(0.999922752380371,2.00033664703369,7.9944634437561,8.9969425201416,16.0144596099854,138.591293334961),
    MaxVolume       = c(0.00020332750864327,0.000246162409894168,0.000598524697124958,0.000703596277162433,0.00190511276014149,0.492488771677017),
    MaxDBH          = c(0.00657883239910007,0.00714902672916651,0.0105199161916971,0.0112863117828965,0.0174035467207432,0.19477815926075),
    NPPL            = c(0.001176628167741,0.000700533681083471,0.00118906644638628,0.00130050932057202,0.00258257193490863,0.11245359480381),
    NPPW            = c(0.00126155931502581,0.000784097821451724,0.00155806133989245,0.00178237247746438,0.00397400138899684,0.306686073541641),
    n_deadtrees     = c(2.98538179777097e-06,2.69603833658039e-06,6.03626858719508e-06,6.81211349728983e-06,1.41874961627764e-05,0.00180775963235646),
    c_deadtrees     = c(8.48920462885872e-05,9.43891063798219e-05,0.000250810815487057,0.000289816758595407,0.000700234435498714,0.329331576824188),
    m_turnover      = c(8.48920462885872e-05,9.43891063798219e-05,0.000250810815487057,0.000289816758595407,0.000700234435498714,0.329331576824188),
    c_turnover_time = c(2.12049031257629,3.64033102989197,2.68918538093567,2.7327868938446,3.20707249641418,8.89041805267334),
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
    cohort      = c(2,3,4,1,5,6,7,8,9,10),
    year        = c(251,251,251,251,251,251,251,251,251,251),
    cID         = c(98,271,248,405,403,347,370,384,394,404),
    PFT         = c(2,2,2,2,2,2,2,2,2,2),
    layer       = c(1,1,1,1,2,2,2,2,2,2),
    density     = c(45.9453926086426,88.3085479736328,3860.36962890625,932.582092285156,393.40966796875,10.8802881240845,121.763404846191,540.707702636719,3086.3037109375,511.826538085938),
    flayer      = c(0.0592438951134682,0.080800473690033,0.776563763618469,0.119202755391598,0.044277586042881,0.000252540572546422,0.00189638487063348,0.0049459645524621,0.015576021745801,0.00167896889615804),
    DBH         = c(19.4778156280518,15.495774269104,5.64466285705566,4.17194604873657,3.83264350891113,1.33782613277435,1.02536618709564,0.719114601612091,0.483746409416199,0.362982541322708),
    dDBH        = c(0.0969082117080688,0.168265402317047,0.140364840626717,0.123925507068634,0.0727303326129913,0.0461408868432045,0.0576198101043701,0.0501702055335045,0.0482346415519714,0.0893887355923653),
    height      = c(15.8881244659424,14.1712818145752,8.55305957794189,7.3531231880188,7.04776954650879,4.1639199256897,3.64537310600281,3.05282235145569,2.50386762619019,2.16892910003662),
    age         = c(138.591293334961,120.957389831543,61.2937622070312,42.2392158508301,42.2305335998535,21.5884895324707,13.7037878036499,8.16503047943115,3.28068089485168,0.999922752380371),
    BA          = c(0.0297968555241823,0.0188589040189981,0.00250245281495154,0.00136699597351253,0.00115368363913149,0.000140568910865113,8.25748575152829e-05,4.06149665650446e-05,1.83791489689611e-05,1.03481188489241e-05),
    dBA         = c(0.000295761972665787,0.000407345592975616,0.000122908735647798,8.00056150183082e-05,4.33704117313027e-05,9.52908885665238e-06,9.01973544387147e-06,5.4694501159247e-06,3.48246248904616e-06,4.46913236373803e-06),
    Acrown      = c(12.8944149017334,9.14979076385498,2.01163053512573,1.27820122241974,1.12548291683197,0.232108369469643,0.155743420124054,0.0914720520377159,0.0504682064056396,0.032803475856781),
    Aleaf       = c(40.6163673400879,28.8198127746582,5.20897102355957,2.99999856948853,1.7724951505661,0.250649809837341,0.168163597583771,0.0987611189484596,0.0544841289520264,0.0354012288153172),
    nsc         = c(8.69034671783447,8.61025333404541,0.0604438371956348,0.0354319773614407,0.575447380542755,0.0922481343150139,0.0817672535777092,0.0449579730629921,0.0245664119720459,0.0240462925285101),
    seedC       = c(0.0670315995812416,0.049742478877306,0.00295112188905478,0.00167514057829976,0.00878775492310524,0.000907446723431349,0.000372642709407955,0.000859255436807871,0.00182746548671275,0.00218956894241273),
    leafC       = c(0.223763048648834,0.25884672999382,0.0410976782441139,0.0308676771819592,0.0225305054336786,1.07380110137001e-07,0,0,0,0),
    rootC       = c(6.90478277206421,4.89936828613281,0.885525107383728,0.509999752044678,0.301324188709259,0.0426104664802551,0.0285878125578165,0.0167893897742033,0.00926230195909739,0.00601820880547166),
    sapwC       = c(4.12256097793579,2.9248960018158,0.525796949863434,0.302982866764069,0.179908245801926,0.0254409518092871,0.00877813901752234,0.00515532959252596,0.00284407124854624,0.00184794387314469),
    woodC       = c(107.882415771484,63.7514839172363,6.24858713150024,3.11740040779114,1.31882703304291,0.080956369638443,0.0439035557210445,0.019412849098444,0.00779884541407228,0.00402712449431419),
    nsn         = c(74.3384246826172,43.931224822998,4.30555295944214,2.14803624153137,3.01330709457397,0.303967475891113,0.164870828390121,0.0729061663150787,0.0292928256094456,0.0151327569037676),
    treeG       = c(8.72370147705078,7.53424739837646,1.44603288173676,0.84990930557251,0.475799292325974,0.0715661197900772,0.044805996119976,0.0255186334252357,0.014680340886116,0.0165860280394554),
    fseed       = c(0.0256500113755465,0.0343560166656971,0.0284209847450256,0.0363187901675701,0.0473529621958733,1.50043217672646e-06,0,0,0,0),
    fleaf       = c(0.168099790811539,0.143239006400108,0.14520089328289,0.141369387507439,0.11870726197958,0.149757951498032,0.178606674075127,0.195692792534828,0.212796613574028,0.425208300352097),
    froot       = c(0.567995131015778,0.467971682548523,0.415671706199646,0.407196164131165,0.441437244415283,0.433115601539612,0.241038024425507,0.249938756227493,0.244935810565948,0.0225267615169287),
    fwood       = c(0.238255083560944,0.35443326830864,0.410706400871277,0.415115714073181,0.39250248670578,0.417124897241592,0.580355286598206,0.554368436336517,0.542267560958862,0.552264928817749),
    GPP         = c(20.9848556518555,14.8277940750122,2.54994654655457,1.48335671424866,0.902374625205994,0.133908092975616,0.0883583128452301,0.0513165555894375,0.0276933051645756,0.0165275614708662),
    NPP         = c(13.0554456710815,8.54744911193848,1.42188680171967,0.828814446926117,0.52199912071228,0.0767327323555946,0.0523517355322838,0.0306746046990156,0.0161071941256523,0.00563223566859961),
    Rauto       = c(7.92940998077393,6.28034543991089,1.1280597448349,0.65454226732254,0.380375474691391,0.0571753606200218,0.0360065773129463,0.0206419508904219,0.0115861101076007,0.0108953258022666),
    Nupt        = c(0.180093288421631,0.127281174063683,0.0211675390601158,0.0121992873027921,0.00778928166255355,0.00108841969631612,0.000369834044249728,0.000178067100932822,0,0),
    Nfix        = c(0,0,0,0,0,0,0,0,0,0),
    deathrate   = c(0.0959627628326416,0.0709986090660095,0.0234108716249466,0.0185213424265385,0.139694377779961,0.270669490098953,0.295521676540375,0.321265399456024,0.341765940189362,0.352460086345673),
    n_deadtrees = c(0.000361855083610862,0.000330899987602606,0.000574257283005863,5.97844555159099e-05,0.000175720226252452,9.94341462501325e-07,6.04697515882435e-06,2.67650684691034e-05,0.000228247808991,4.31884436693508e-05),
    c_deadtrees = c(0.0891342982649803,0.0779811143875122,0.109055086970329,0.0106135718524456,0.0297391898930073,0.000160566254635341,0.00117993366438895,0.00276585109531879,0.00778062734752893,0.000921336701139808))

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
  # mod_BiomeE_PLULUC$aggregated$output_annual_cell|>filter(                           year %in% c(1, 2, 8, 9, 16, 251)) |> dput()
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
    CAI             = c(0.00416006147861481,0.0051026726141572,0.0137958535924554,0.0158615447580814,0.0471739545464516,1.34490203857422),
    LAI             = c(0.00620649103075266,0.00972458533942699,0.043438233435154,0.0499434173107147,0.144030213356018,3.74448370933533),
    Density         = c(500,494.722717285156,462.53564453125,456.990539550781,1980.69897460938,46017.578125),
    DBH             = c(0.67508590221405,0.779046058654785,1.58127760887146,1.74943590164185,1.17045247554779,1.0599946975708),
    Density12       = c(0,0,0,0,0,214.249237060547),
    DBH12           = c(0,0,0,0,0,21.6128730773926),
    QMD12           = c(0,0,0,0,0,21.649543762207),
    NPP             = c(0.00230822991579771,0.00474031921476126,0.0180877912789583,0.0207991749048233,0.060594454407692,1.43212628364563),
    GPP             = c(0.00443602073937654,0.0068761482834816,0.0263272896409035,0.0303885489702225,0.0890411734580994,2.20210719108582),
    Rauto           = c(0.00212779105640948,0.00213582906872034,0.00823949836194515,0.00958937499672174,0.0284467153251171,0.769980907440186),
    Rh              = c(0.0028953910805285,0.00248818146064878,0.00352250225841999,0.00409194361418486,0.0119022633880377,1.33055460453033),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903930664062,799.904052734375,799.904663085938,799.90478515625,799.906616210938,799.930053710938),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.692077636719,159.533355712891,157.896453857422,157.586547851562,153.370544433594,94.3526916503906),
    Runoff          = c(427.933654785156,427.995849609375,429.632873535156,429.942749023438,434.158508300781,493.176879882812),
    plantC          = c(0.00990187004208565,0.0133129935711622,0.0604372695088387,0.0732633024454117,0.244614824652672,11.8816633224487),
    soilC           = c(0.0111686438322067,0.0100096687674522,0.0205106195062399,0.0243918374180794,0.0789286494255066,74.3610610961914),
    plantN          = c(0.000283176312223077,0.000251700810622424,0.00107264402322471,0.00126686436124146,0.0036161239258945,0.112371981143951),
    soilN           = c(0.00390609027817845,0.00347655825316906,0.00350195541977882,0.00360360974445939,0.00510023115202785,0.801159143447876),
    totN            = c(0.00418926635757089,0.00372825912199914,0.00457459967583418,0.00487047433853149,0.00871635507792234,0.913531124591827),
    NSC             = c(0.0042251693084836,0.00518226157873869,0.0209579579532146,0.0248370133340359,0.0757391154766083,2.22338676452637),
    SeedC           = c(0,0,0.00153295951895416,0.00226585380733013,0.00386109668761492,0.0703786164522171),
    leafC           = c(0.0010551034938544,0.00165317952632904,0.00738450000062585,0.00849038083106279,0.0244851373136044,0.636562287807465),
    rootC           = c(0.000629958754871041,0.00098704535048455,0.00440898025408387,0.00506925676018,0.0146190663799644,0.380062222480774),
    SapwoodC        = c(0.00115265452768654,0.00201194896362722,0.0154777336865664,0.0192941166460514,0.0739046931266785,4.90330505371094),
    WoodC           = c(0.00283898459747434,0.00347855873405933,0.0106751415878534,0.0133066792041063,0.0520057007670403,3.66796946525574),
    NSN             = c(0.000237868691328913,0.000182893098099157,0.000683991820551455,0.000787610246334225,0.00227655866183341,0.0639094635844231),
    SeedN           = c(0,0,7.66480516176671e-05,0.00011329265544191,0.0001930549624376,0.00351893017068505),
    leafN           = c(1.81540090125054e-05,2.84444759017788e-05,0.000127057413919829,0.000146085221786052,0.000421290460508317,0.0109526989981532),
    rootN           = c(1.57489594130311e-05,2.46760791924316e-05,0.000110224282252602,0.000126731276395731,0.000365475891157985,0.00950153917074203),
    SapwoodN        = c(3.29329850501381e-06,5.74842579226242e-06,4.4222098949831e-05,5.51260527572595e-05,0.000211156293516979,0.0140094440430403),
    WoodN           = c(8.11138215794927e-06,9.93872890830971e-06,3.05004286929034e-05,3.80190867872443e-05,0.000148587685544044,0.0104799084365368),
    McrbC           = c(0.000287109753116965,0.000403912068577483,0.000671922985929996,0.00077282334677875,0.00220573716796935,0.162999212741852),
    fastSOM         = c(0.00974625535309315,0.00818874873220921,0.0127468416467309,0.0147854499518871,0.0427582710981369,2.0337233543396),
    SlowSOM         = c(0.00113527732901275,0.00141700753010809,0.00709185376763344,0.00883356481790543,0.033964641392231,72.1643371582031),
    McrbN           = c(2.87109760392923e-05,4.03912054025568e-05,6.71923044137657e-05,7.72823404986411e-05,0.000220573710976169,0.0162999201565981),
    fastSoilN       = c(0.000636344775557518,0.000536485691554844,0.000581176253035665,0.000663300626911223,0.00187161192297935,0.10714727640152),
    slowSoilN       = c(2.69899937848095e-05,3.06047149933875e-05,0.000103922066045925,0.000125899488921277,0.000426500046160072,0.675477802753448),
    mineralN        = c(0.00321404449641705,0.00286907656118274,0.00274966470897198,0.0027371272444725,0.00258154538460076,0.00223414390347898),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000328889494994655,0.000375684059690684,0.0010224279249087,0.0222047530114651),
    N_yrMin         = c(-0.0117859477177262,-0.000344969506841153,0.00031738291727379,0.000363147351890802,0.00100460310932249,0.0221877209842205),
    N_P2S           = c(2.00448012037668e-05,3.11766561935656e-05,0.000158920214744285,0.000184204152901657,0.00053854682482779,0.0222463458776474),
    N_loss          = c(0.0219198539853096,0.0104610007256269,0.00974611192941666,0.00970412977039814,0.00916288606822491,0.00751804327592254),
    totseedC        = c(0,0,0,0,0.00356994988396764,0.0649071559309959),
    totseedN        = c(0,0,0,0,0.000178497604792938,0.00324535765685141),
    Seedling_C      = c(0,0,0,0,0.00356994988396764,0.0649071559309959),
    Seedling_N      = c(0,0,0,0,0.000178497604792938,0.00324535765685141),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627487182617),
    MaxVolume       = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627487182617),
    MaxDBH          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627487182617),
    NPPL            = c(0.00121877959463745,0.0008825832628645,0.00248827831819654,0.00281241675838828,0.00808483175933361,0.169744476675987),
    NPPW            = c(0.00149163906462491,0.00154099566861987,0.00566299259662628,0.00676144519820809,0.0210112184286118,0.651887178421021),
    n_deadtrees     = c(2.9888342396589e-06,2.69008114628377e-06,1.28593292174628e-05,1.56000605784357e-05,5.2299743401818e-05,0.00905296579003334),
    c_deadtrees     = c(0.000104511025710963,0.000142284145113081,0.000724548532161862,0.000902157858945429,0.00362703669816256,0.904662847518921),
    m_turnover      = c(0.000104511025710963,0.000142284145113081,0.000724548532161862,0.000902157858945429,0.00362703669816256,0.904662847518921),
    c_turnover_time = c(1.90326523780823,2.25734519958496,1.88507056236267,1.96802306175232,2.4751398563385,5.6266942024231),
    lu_fraction     = c(1,1,1,1,1,1),
    prod_pool_1_C   = c(0.000375000003259629,0.000227448996156454,1.13240193968522e-05,6.86836483509978e-06,2.07406671393073e-07,1.40129846432482e-45),
    prod_pool_1_N   = c(1.07142852812103e-06,6.49854257517291e-07,3.23543396518744e-08,1.96238989502717e-08,5.92590532200177e-10,1.40129846432482e-45),
    prod_pool_2_C   = c(0.000375000003259629,0.000356711039785296,0.000264258094830438,0.000251370074693114,0.000177137553691864,1.39750544470019e-09),
    prod_pool_2_N   = c(1.07142852812103e-06,1.01917441952537e-06,7.55023165766033e-07,7.18200283245096e-07,5.06107255660027e-07,3.99287460733921e-12),
    Rprod_0_C       = c(0.000250000011874363,0,0,0,0,0),
    Rprod_0_N       = c(7.14285704361828e-07,0,0,0,0,0),
    Rprod_1_C       = c(0.000147551007103175,8.949420589488e-05,4.45565456175245e-06,2.70249097411579e-06,8.16081637822208e-08,0),
    Rprod_1_N       = c(4.21574270603742e-07,2.55697727880033e-07,1.27304407016027e-08,7.72140218430195e-09,2.33166208563063e-10,0),
    Rprod_2_C       = c(1.82889543793863e-05,1.73969929164741e-05,1.28880119518726e-05,1.22594556160038e-05,8.63909554027487e-06,6.81571049598872e-11),
    Rprod_2_N       = c(5.22541547809396e-08,4.9705693783153e-08,3.68228931790782e-08,3.50270212834403e-08,2.46831284300697e-08,1.94734677059875e-13))
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
    LAI             = c(3.4922297000885,3.49872899055481,3.61742806434631,3.74305200576782,3.74376797676086),
    GPP             = c(0.00227762758731842,0.00224546459503472,0.00936589390039444,0.00246698618866503,0.00246065971441567),
    Rauto           = c(6.12458534305915e-05,0.00236861989833415,0.0024318634532392,0.00197463692165911,0.00198430172167718),
    Rh              = c(0.00146035756915808,0.00145140045788139,0.00574139412492514,0.00141523755155504,0.001413878868334),
    NSC             = c(2.14304780960083,2.13828492164612,2.1303768157959,2.2305862903595,2.22722339630127),
    seedC           = c(0.0019968063570559,0.0021789469756186,0.0329009033739567,0.0699842870235443,0.070181593298912),
    leafC           = c(0.593679130077362,0.594783842563629,0.614962816238403,0.636318802833557,0.636440634727478),
    rootC           = c(0.360928446054459,0.36085844039917,0.365907251834869,0.379917114973068,0.379989683628082),
    SW_C            = c(4.5676908493042,4.55334377288818,4.69937181472778,4.90116739273071,4.90223789215088),
    HW_C            = c(3.35169792175293,3.36794519424438,3.51899361610413,3.66643118858337,3.66720104217529),
    NSN             = c(0.0614702217280865,0.0614091902971268,0.0626659020781517,0.0639448463916779,0.0638884156942368),
    seedN           = c(9.98403047560714e-05,0.000108947337139398,0.00164504500571638,0.00349921477027237,0.00350908027030528),
    leafN           = c(0.0102148456498981,0.0102338567376137,0.0105810575187206,0.0109485015273094,0.0109505970031023),
    rootN           = c(0.00902319885790348,0.00902144890278578,0.00914766546338797,0.00949791260063648,0.00949972867965698),
    SW_N            = c(0.0130505440756679,0.0130095547065139,0.0134267760440707,0.0140033354982734,0.0140063930302858),
    HW_N            = c(0.00957628153264523,0.00962270423769951,0.0100542698055506,0.0104755172505975,0.0104777161031961),
    McrbC           = c(0.163020133972168,0.163022756576538,0.164115786552429,0.163002550601959,0.162999197840691),
    fastSOM         = c(2.19501852989197,2.19556164741516,2.2025363445282,2.03177905082703,2.03243160247803),
    slowSOM         = c(72.7654800415039,72.765007019043,72.5841827392578,72.1644821166992,72.1640319824219),
    McrbN           = c(0.0163020137697458,0.0163022764027119,0.0164115782827139,0.016300255432725,0.0162999201565981),
    fastSoilN       = c(0.113785140216351,0.113796405494213,0.113353185355663,0.107105150818825,0.107118561863899),
    slowSoilN       = c(0.675333976745605,0.675336599349976,0.675630569458008,0.675470352172852,0.675473272800446),
    mineralN        = c(0.00221035978756845,0.00223766081035137,0.00152802339289337,0.00226603355258703,0.00229924730956554),
    N_uptk          = c(0.000141792901558802,6.19682805336197e-06,0,0,0))
  ref_BiomeE_PLULUC_primary_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00416006147861481,0.0051026726141572,0.013795854523778,0.0158615466207266,0.0471739545464516,1.34490180015564),
    LAI             = c(0.00620649103075266,0.00972458533942699,0.043438233435154,0.0499434173107147,0.144030258059502,3.74448347091675),
    Density         = c(500,494.722686767578,462.535614013672,456.990539550781,1980.70007324219,46017.58984375),
    DBH             = c(0.67508590221405,0.77904599905014,1.58127772808075,1.74943602085114,1.17045211791992,1.05999422073364),
    Density12       = c(0,0,0,0,0,214.249237060547),
    DBH12           = c(0,0,0,0,0,21.6128730773926),
    QMD12           = c(0,0,0,0,0,21.6495418548584),
    NPP             = c(0.00230822968296707,0.00474032014608383,0.0180877950042486,0.0207991693168879,0.0605944767594337,1.43212604522705),
    GPP             = c(0.00443602073937654,0.00687614921480417,0.0263272896409035,0.0303885489702225,0.0890411883592606,2.20210719108582),
    Rauto           = c(0.00212779105640948,0.00213582930155098,0.00823949463665485,0.00958937965333462,0.028446713462472,0.769981145858765),
    Rh              = c(0.00236286479048431,0.0020676152780652,0.00343759660609066,0.00402783416211605,0.0118935955688357,1.33055472373962),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903991699219,799.904052734375,799.904724121094,799.904907226562,799.90673828125,799.930053710938),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.599700927734,159.441146850586,157.805053710938,157.495330810547,153.281402587891,94.2941131591797),
    Runoff          = c(428.025726318359,428.088317871094,429.724395751953,430.034057617188,434.247924804688,493.235260009766),
    plantC          = c(0.00990187004208565,0.0133129954338074,0.0604372769594193,0.0732633098959923,0.244614720344543,11.8816652297974),
    soilC           = c(0.00937039777636528,0.00863198284059763,0.0202523395419121,0.0241976678371429,0.0789027363061905,74.3610763549805),
    plantN          = c(0.000283176312223077,0.000251700810622424,0.00107308442238718,0.00126732815988362,0.00361604942008853,0.112294517457485),
    soilN           = c(0.00379347242414951,0.00337960757315159,0.00347751541994512,0.00358472322113812,0.00509759178385139,0.801228404045105),
    totN            = c(0.00407664850354195,0.00363130844198167,0.00455059995874763,0.00485205138102174,0.00871364120393991,0.913522899150848),
    NSC             = c(0.00422516884282231,0.00518226251006126,0.0209579616785049,0.0248370189219713,0.0757390111684799,2.22338652610779),
    SeedC           = c(0,0,0.00153295940253884,0.00226585427299142,0.00386109319515526,0.0703786239027977),
    leafC           = c(0.0010551034938544,0.00165317952632904,0.00738449953496456,0.00849038176238537,0.0244851429015398,0.63656222820282),
    rootC           = c(0.000629958754871041,0.00098704535048455,0.00440898025408387,0.00506925676018,0.0146190682426095,0.380062222480774),
    SapwoodC        = c(0.00115265452768654,0.00201194896362722,0.0154777374118567,0.0192941222339869,0.0739047154784203,4.90330600738525),
    WoodC           = c(0.00283898459747434,0.00347855873405933,0.0106751378625631,0.0133066764101386,0.05200569704175,3.6679699420929),
    NSN             = c(0.000237868676776998,0.000182893098099157,0.000684432161506265,0.000788073870353401,0.00227648415602744,0.0638319998979568),
    SeedN           = c(0,0,7.66480443417095e-05,0.00011329265544191,0.000193054700503126,0.00351893133483827),
    leafN           = c(1.8154007193516e-05,2.84444759017788e-05,0.000127057341160253,0.000146085236337967,0.000421290373196825,0.01095269061625),
    rootN           = c(1.57489575940417e-05,2.46760791924316e-05,0.000110224362288136,0.000126731349155307,0.000365475978469476,0.00950154103338718),
    SapwoodN        = c(3.29329850501381e-06,5.74842533751507e-06,4.42221098637674e-05,5.51260636711959e-05,0.000211156337172724,0.0140094440430403),
    WoodN           = c(8.11138215794927e-06,9.93873254628852e-06,3.05004105030093e-05,3.80190831492655e-05,0.00014858775830362,0.0104799130931497),
    McrbC           = c(0.000234328705118969,0.000333371630404145,0.000644201005343348,0.000751312880311161,0.00220260047353804,0.162999197840691),
    fastSOM         = c(0.00800079107284546,0.0068816039711237,0.0125162834301591,0.014612790197134,0.0427354797720909,2.03372311592102),
    SlowSOM         = c(0.00113527732901275,0.00141700753010809,0.00709185423329473,0.008833565749228,0.0339646562933922,72.1643524169922),
    McrbN           = c(2.34328708756948e-05,3.3337164495606e-05,6.44200990791433e-05,7.51312909414992e-05,0.000220260044443421,0.0162999201565981),
    fastSoilN       = c(0.000531830999534577,0.000451580359367654,0.000561729830224067,0.000648404587991536,0.00186946208123118,0.107151478528976),
    slowSoilN       = c(2.69899937848095e-05,3.06047149933875e-05,0.00010392205149401,0.000125899474369362,0.000426499871537089,0.675477743148804),
    mineralN        = c(0.00321121863089502,0.00286408537067473,0.00274744350463152,0.00273528788238764,0.00258136983029544,0.00229924730956554),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000328867841744795,0.000375712901586667,0.0010222572600469,0.0221947133541107),
    N_yrMin         = c(-0.0117887761443853,-0.000347134715411812,0.000317854312015697,0.000363559287507087,0.00100462301634252,0.0221746861934662),
    N_P2S           = c(2.00447993847774e-05,3.11766561935656e-05,0.000158925526193343,0.000184209915460087,0.000538545835297555,0.0222442373633385),
    N_loss          = c(0.021915266290307,0.010445331223309,0.00973904691636562,0.00969855580478907,0.00916206929832697,0.00753329368308187),
    totseedC        = c(0,0,0,0,0.00356994615867734,0.0649071782827377),
    totseedN        = c(0,0,0,0,0.000178497357410379,0.00324535882100463),
    Seedling_C      = c(0,0,0,0,0.00356994615867734,0.0649071782827377),
    Seedling_N      = c(0,0,0,0,0.000178497342858464,0.00324535858817399),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627563476562),
    MaxVolume       = c(0.000215764259337448,0.000299950072076172,0.00152817298658192,0.0019280546111986,0.00737557932734489,0.652383387088776),
    MaxDBH          = c(0.00675085838884115,0.00779045978561044,0.0158127769827843,0.0174943599849939,0.0313498750329018,0.220105081796646),
    NPPL            = c(0.00121877959463745,0.0008825832628645,0.00248827692121267,0.002812419552356,0.00808483455330133,0.169744402170181),
    NPPW            = c(0.00149163894820958,0.00154099601786584,0.00566299306228757,0.00676144752651453,0.02101119607687,0.651887357234955),
    n_deadtrees     = c(2.9888342396589e-06,2.69008114628377e-06,1.28646079247119e-05,1.56057722051628e-05,5.22986738360487e-05,0.00905085541307926),
    c_deadtrees     = c(0.000104511025710963,0.000142284159664996,0.000724548590369523,0.000902158091776073,0.00362703530117869,0.904662847518921),
    m_turnover      = c(0.000104511025710963,0.000142284159664996,0.000724548590369523,0.000902158091776073,0.00362703530117869,0.904662847518921),
    c_turnover_time = c(1.90326523780823,2.25734448432922,1.88506996631622,1.96802186965942,2.47514224052429,5.62669277191162),
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
    Evap            = c(0.0399133078753948,0.0409998670220375,0.483673214912415,0.0485905706882477,0.0699304714798927),
    Runoff          = c(1.36660242080688,1.96390557289124,1.74325275421143,1.99354791641235,1.77749955654144),
    ws1             = c(19.9600887298584,19.9590015411377,19.5163269042969,19.9514083862305,19.9300708770752),
    ws2             = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3             = c(600,600,600,600,600),
    LAI             = c(3.49222993850708,3.49872970581055,3.61742806434631,3.74305152893066,3.74376773834229),
    GPP             = c(0.00227762712165713,0.00224546436220407,0.00936589110642672,0.0024669854901731,0.00246065971441567),
    Rauto           = c(6.12458461546339e-05,0.00236870953813195,0.00243187905289233,0.00197461084462702,0.00198432477191091),
    Rh              = c(0.00146035687066615,0.00145139975938946,0.00574139226227999,0.00141523708589375,0.00141387851908803),
    NSC             = c(2.1430447101593,2.13828134536743,2.13037395477295,2.23058462142944,2.22722125053406),
    seedC           = c(0.00199680984951556,0.00217895023524761,0.0329008512198925,0.0699842199683189,0.0701815262436867),
    leafC           = c(0.593679070472717,0.594784021377563,0.614962756633759,0.636318683624268,0.636440575122833),
    rootC           = c(0.360928416252136,0.360858500003815,0.365907222032547,0.379917025566101,0.379989624023438),
    SW_C            = c(4.56769037246704,4.55334424972534,4.69937038421631,4.9011664390564,4.90223550796509),
    HW_C            = c(3.35169625282288,3.36794352531433,3.51899266242981,3.66643023490906,3.66720032691956),
    NSN             = c(0.0614123828709126,0.0613451525568962,0.0628624483942986,0.0639507323503494,0.0640421807765961),
    seedN           = c(9.98404866550118e-05,0.000108947497210465,0.00164504267740995,0.00349921081215143,0.00350907607935369),
    leafN           = c(0.0102148428559303,0.0102338558062911,0.0105810631066561,0.0109485071152449,0.0109506025910378),
    rootN           = c(0.00902319420129061,0.00902144517749548,0.00914766080677509,0.00949790608137846,0.00949972216039896),
    SW_N            = c(0.0130505440756679,0.0130095537751913,0.0134267713874578,0.0140033317729831,0.0140063865110278),
    HW_N            = c(0.00957627687603235,0.00962269864976406,0.0100542642176151,0.0104755153879523,0.0104777151718736),
    McrbC           = c(0.163020089268684,0.163022711873055,0.164115756750107,0.163002520799637,0.162999168038368),
    fastSOM         = c(2.19501686096191,2.19555997848511,2.2025351524353,2.03177809715271,2.03243064880371),
    slowSOM         = c(72.7654647827148,72.7649917602539,72.5841674804688,72.1644668579102,72.1640167236328),
    McrbN           = c(0.0163020081818104,0.0163022708147764,0.0164115764200687,0.0163002517074347,0.0162999164313078),
    fastSoilN       = c(0.113782078027725,0.113793343305588,0.11335052549839,0.107103116810322,0.107116527855396),
    slowSoilN       = c(0.675333976745605,0.675336599349976,0.675630569458008,0.675470352172852,0.675473272800446),
    mineralN        = c(0.0022484662476927,0.00228170235641301,0.00132014625705779,0.0022728736512363,0.00215815543197095),
    N_uptk          = c(0,0,0.00045189680531621,7.82818387961015e-05,0.000147884173202328))
  ref_BiomeE_PLULUC_secondary_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00416006147861481,0.0051026726141572,0.013795854523778,0.0158615466207266,0.0471739545464516,1.34490156173706),
    LAI             = c(0.00620649103075266,0.00972458533942699,0.043438233435154,0.0499434173107147,0.144030258059502,3.74448323249817),
    Density         = c(500,494.722686767578,462.535614013672,456.990539550781,1980.70007324219,46017.56640625),
    DBH             = c(0.67508590221405,0.77904599905014,1.58127772808075,1.74943602085114,1.17045211791992,1.0599946975708),
    Density12       = c(0,0,0,0,0,214.249359130859),
    DBH12           = c(0,0,0,0,0,21.6128692626953),
    QMD12           = c(0,0,0,0,0,21.6495399475098),
    NPP             = c(0.00230822968296707,0.00474032014608383,0.0180877950042486,0.0207991693168879,0.0605944767594337,1.43212652206421),
    GPP             = c(0.00443602073937654,0.00687614921480417,0.0263272896409035,0.0303885489702225,0.0890411883592606,2.20210695266724),
    Rauto           = c(0.00212779105640948,0.00213582930155098,0.00823949463665485,0.00958937965333462,0.028446713462472,0.769980430603027),
    Rh              = c(0.00369418039917946,0.00311903026886284,0.00364986062049866,0.00418810732662678,0.0119152693077922,1.33055436611176),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903991699219,799.904052734375,799.904724121094,799.904907226562,799.90673828125,799.930053710938),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.599700927734,159.441146850586,157.805053710938,157.495330810547,153.281402587891,94.2941131591797),
    Runoff          = c(428.025787353516,428.088317871094,429.724395751953,430.034057617188,434.247924804688,493.235260009766),
    plantC          = c(0.00990187004208565,0.0133129954338074,0.0604372769594193,0.0732633098959923,0.244614720344543,11.8816614151001),
    soilC           = c(0.0138660119846463,0.0120761953294277,0.020898038521409,0.0246830955147743,0.0789675638079643,74.3610610961914),
    plantN          = c(0.000283176312223077,0.000251700810622424,0.00107198359910399,0.0012661685468629,0.00361624336801469,0.112448267638683),
    soilN           = c(0.00407501682639122,0.00362198404036462,0.00353861507028341,0.00363193918019533,0.00510419066995382,0.801085293292999),
    totN            = c(0.00435819290578365,0.00387368490919471,0.00461059855297208,0.00489810761064291,0.00872043427079916,0.913533568382263),
    NSC             = c(0.00422516884282231,0.00518226251006126,0.0209579616785049,0.0248370189219713,0.0757390111684799,2.22338533401489),
    SeedC           = c(0,0,0.00153295940253884,0.00226585427299142,0.00386109319515526,0.0703785419464111),
    leafC           = c(0.0010551034938544,0.00165317952632904,0.00738449953496456,0.00849038176238537,0.0244851429015398,0.636562168598175),
    rootC           = c(0.000629958754871041,0.00098704535048455,0.00440898025408387,0.00506925676018,0.0146190682426095,0.380062162876129),
    SapwoodC        = c(0.00115265452768654,0.00201194896362722,0.0154777374118567,0.0192941222339869,0.0739047154784203,4.90330457687378),
    WoodC           = c(0.00283898459747434,0.00347855873405933,0.0106751378625631,0.0133066764101386,0.05200569704175,3.66796898841858),
    NSN             = c(0.000237868676776998,0.000182893098099157,0.000683331280015409,0.000786914257332683,0.0022766781039536,0.0639857649803162),
    SeedN           = c(0,0,7.66480443417095e-05,0.00011329265544191,0.000193054700503126,0.00351892737671733),
    leafN           = c(1.8154007193516e-05,2.84444759017788e-05,0.000127057341160253,0.000146085236337967,0.000421290373196825,0.0109526971355081),
    rootN           = c(1.57489575940417e-05,2.46760791924316e-05,0.000110224362288136,0.000126731349155307,0.000365475978469476,0.00950153544545174),
    SapwoodN        = c(3.29329850501381e-06,5.74842533751507e-06,4.42221098637674e-05,5.51260636711959e-05,0.000211156337172724,0.01400944031775),
    WoodN           = c(8.11138215794927e-06,9.93873254628852e-06,3.05004105030093e-05,3.80190831492655e-05,0.00014858775830362,0.0104799102991819),
    McrbC           = c(0.000366281310562044,0.000509722682181746,0.000713505956809968,0.000805088900960982,0.00221044081263244,0.162999168038368),
    fastSOM         = c(0.0123644527047873,0.0101494649425149,0.013092678040266,0.0150444395840168,0.0427924655377865,2.0337221622467),
    SlowSOM         = c(0.00113527732901275,0.00141700753010809,0.00709185423329473,0.008833565749228,0.0339646562933922,72.1643371582031),
    McrbN           = c(3.66281310562044e-05,5.09722667629831e-05,7.13505942258053e-05,8.05088930064812e-05,0.00022104408708401,0.0162999164313078),
    fastSoilN       = c(0.000793115410488099,0.000663843646179885,0.000610345916356891,0.000685644685290754,0.00187483767513186,0.107149444520473),
    slowSoilN       = c(2.69899937848095e-05,3.06047149933875e-05,0.00010392205149401,0.000125899474369362,0.000426499871537089,0.675477743148804),
    mineralN        = c(0.00321828341111541,0.00287656346336007,0.00275299651548266,0.00273988605476916,0.00258180871605873,0.00215815543197095),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000328921974869445,0.000375640869606286,0.00102268438786268,0.0222645122557878),
    N_yrMin         = c(-0.0117817036807537,-0.000341721664881334,0.000316675839712843,0.000362529419362545,0.00100457400549203,0.0222078040242195),
    N_P2S           = c(2.00447993847774e-05,3.11766561935656e-05,0.000158912327606231,0.000184195640031248,0.000538548629265279,0.0222488809376955),
    N_loss          = c(0.021926736459136,0.0104845045134425,0.00975670851767063,0.00971248932182789,0.0091641116887331,0.00749913975596428),
    totseedC        = c(0,0,0,0,0.00356994615867734,0.0649071037769318),
    totseedN        = c(0,0,0,0,0.000178497357410379,0.00324535509571433),
    Seedling_C      = c(0,0,0,0,0.00356994615867734,0.0649071037769318),
    Seedling_N      = c(0,0,0,0,0.000178497342858464,0.00324535509571433),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,77.7627334594727),
    MaxVolume       = c(0.000215764259337448,0.000299950072076172,0.00152817298658192,0.0019280546111986,0.00737557932734489,0.652382910251617),
    MaxDBH          = c(0.00675085838884115,0.00779045978561044,0.0158127769827843,0.0174943599849939,0.0313498750329018,0.220105022192001),
    NPPL            = c(0.00121877959463745,0.0008825832628645,0.00248827692121267,0.002812419552356,0.00808483455330133,0.16974450647831),
    NPPW            = c(0.00149163894820958,0.00154099601786584,0.00566299306228757,0.00676144752651453,0.02101119607687,0.651886641979218),
    n_deadtrees     = c(2.9888342396589e-06,2.69008114628377e-06,1.28514093375998e-05,1.5591493138345e-05,5.23014678037725e-05,0.00905550550669432),
    c_deadtrees     = c(0.000104511025710963,0.000142284159664996,0.000724548590369523,0.000902158091776073,0.00362703530117869,0.904662489891052),
    m_turnover      = c(0.000104511025710963,0.000142284159664996,0.000724548590369523,0.000902158091776073,0.00362703530117869,0.904662489891052),
    c_turnover_time = c(1.90326523780823,2.25734448432922,1.88506996631622,1.96802186965942,2.47514224052429,5.6266975402832),
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
  expect_equal(tolerance = 5e-2, tibble(mod_BiomeE_PLULUC$aggregated$output_annual_cell |>filter(       year %in% c(1, 2, 8, 9, 16, 251))),   ref_BiomeE_PLULUC_aggregated)
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


