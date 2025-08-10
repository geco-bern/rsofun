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

  expect_equal(out, rsofun::biomee_p_model_output, tolerance = 0.08) # NOTE: since Biomee is sensitive, feel free to deactivate this line if it does not pass
  
  # If these tests fail it means that the output of the model is out of sync with the data in the data directory.
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

  expect_equal(out, rsofun::biomee_p_model_luluc_output, tolerance = 0.08) # NOTE: since Biomee is sensitive, feel free to deactivate this line if it does not pass
  
  # If these tests fail it means that the output of the model is out of sync with the data in the data directory.
  # It could either mean that:
  # - the model was accidentally altered and should be fixed to deliver the expected output
  # - the model, drivers, or parameters was changed and the output data needs to be re-generetaed using the scripts in
  #   raw-data directory.
  #
  # Note: Biomee is quite sensitive with regards to which order cohorts are processed (ex: reduce stage when cohorts are merged together).
  # As a consequence, a slight change in the code may cause the final number of cohorts to be differents and therefore a large difference in the mean of the colunms of output_annual_aggregated.
  # These changes do not imply any meaningful alteration of the code functions and are simply artefacts.
})

test_that("biomee output check (gs leuning)", {
  skip_on_cran()

  out <- runread_biomee_f(
    biomee_gs_leuning_drivers,
    makecheck = TRUE,
    parallel = FALSE)

  expect_true(all.equal(colMeans(out$data[[1]]$output_daily_tile), colMeans(biomee_gs_leuning_output$data[[1]]$output_daily_tile), tolerance = 1e-4))
  expect_true(all.equal(colMeans(out$data[[1]]$output_annual_tile), colMeans(biomee_gs_leuning_output$data[[1]]$output_annual_tile), tolerance = 1e-4))
  expect_true(all.equal(colMeans(out$data[[1]]$output_annual_cohorts), colMeans(biomee_gs_leuning_output$data[[1]]$output_annual_cohorts), tolerance = 1e-4))

  expect_equal(out, rsofun::biomee_gs_leuning_output, tolerance = 0.08) # NOTE: since Biomee is sensitive, feel free to deactivate this line if it does not pass
  
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

  # test for correctly returned types
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

  # test if the returned types
  # are in a list (don't error / warning)
  expect_type(mod, "list")

  # test runread_pmodel_f
  df_output <- runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = FALSE,
    parallel = FALSE
  )

  # test for correctly returned types
  expect_type(df_output, "list")

  # test runread_pmodel_f
  df_output_p <- runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = TRUE, 
    ncores = 2
  )

  # test for correctly returned types
  expect_type(df_output_p, "list")
  
  # also check for correctly returned _values_, not only types
  mod_pmodel_bysite           <- head_tail(tibble(mod), n=30)
  mod_pmodel_runread_serial   <- head_tail(tibble(df_output$data[[1]]), n=30)
  mod_pmodel_runread_parallel <- head_tail(tibble(df_output_p$data[[1]]), n=30)
  
  expect_equal(mod_pmodel_bysite, mod_pmodel_runread_serial)
  expect_equal(mod_pmodel_bysite, mod_pmodel_runread_parallel)
  expect_snapshot_value_fmt(mod_pmodel_bysite, tolerance = 0.01, cran = TRUE)
  
  # also check that reference data sets are up-to-date
  expect_equal(df_output, rsofun::p_model_output, tolerance = 0.01)
  
  
  # Rerun again (inverse order) to test memory leakage:
  df_output_p_rerun <- runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = TRUE, 
    ncores = 2
  )
  df_output_rerun <- runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = FALSE,
    parallel = FALSE
  )
  mod_rerun <- run_pmodel_f_bysite(
    df_drivers$sitename[1],
    df_drivers$params_siml[[1]],
    df_drivers$site_info[[1]],
    df_drivers$forcing[[1]],
    params_modl = params_modl,
    makecheck = FALSE
  )
  expect_equal(mod,
               mod_rerun)
  expect_equal(df_output,
               df_output_rerun)
  expect_equal(df_output_p,
               df_output_p_rerun)
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

  # test if the returned types
  # are in a list (don't error / warning)
  expect_type(mod, "list")

  # test runread_pmodel_f
  df_output <- runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = FALSE,
    parallel = FALSE
  )

  # test for correctly returned types
  expect_type(df_output, "list")

  # test runread_pmodel_f
  df_output_p <- runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = TRUE,
    ncores = 2
  )

  # test for correctly returned types
  expect_type(df_output_p, "list")
  
  # also check for correctly returned _values_, not only types
  mod_pmodel_vcmax_bysite           <- head_tail(tibble(mod), n=30)
  mod_pmodel_vcmax_runread_serial   <- head_tail(tibble(df_output$data[[1]]), n=30)
  mod_pmodel_vcmax_runread_parallel <- head_tail(tibble(df_output_p$data[[1]]), n=30)
  
  expect_equal(mod_pmodel_vcmax_bysite, mod_pmodel_vcmax_runread_serial)
  expect_equal(mod_pmodel_vcmax_bysite, mod_pmodel_vcmax_runread_parallel)
  expect_snapshot_value_fmt(mod_pmodel_vcmax_bysite, tolerance = 0.01, cran = TRUE)
  
  # also check that reference data sets are up-to-date
  expect_equal(df_output, rsofun::p_model_output_vcmax25, tolerance = 0.01)
  
  # Rerun again (inverse order) to test memory leakage:
  df_output_p_rerun <- runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = TRUE,
    ncores = 2
  )
  df_output_rerun <- runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = FALSE,
    parallel = FALSE
  )
  mod_rerun <- run_pmodel_f_bysite(
    df_drivers$sitename[1],
    df_drivers$params_siml[[1]],
    df_drivers$site_info[[1]],
    df_drivers$forcing[[1]],
    params_modl = params_modl,
    makecheck = FALSE
  )
  expect_equal(mod, 
               mod_rerun)
  expect_equal(df_output, 
               df_output_rerun)
  expect_equal(df_output_p |> arrange(sitename), 
               df_output_p_rerun |> arrange(sitename))
})

test_that("p-model onestep output check (run_pmodel_onestep_f_bysite())", {
  skip_on_cran()
  
  # Define simulation inputs
  inputs <- list(
    # for forcing:
    temp  = 20,           # temperature, deg C
    vpd   = 1000,         # Pa,
    ppfd  = 300/10^6,     # mol/m2/s
    co2   = 400,          # ppm,
    patm  = 101325,       # Pa
    fapar = 1,            # -
    # for params_modl
    kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.0,        # disable temperature-dependence of kphio
    kphio_par_b        = 1.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
    kc_jmax            = 0.41
    )
  
  # compute reference value:
  library(rpmodel)
  resR <- rpmodel(
    tc=inputs$temp, vpd=inputs$vpd, co2=inputs$co2, 
    patm=inputs$patm, kphio=inputs$kphio, beta=inputs$beta_unitcostratio, 
    ppfd=inputs$ppfd, # rpmodel docs state that units of ppfd define output units of: lue,gpp,vcmax,rd
                      # this also affects: vcmax25,gs
    # NOTE: unused inputs: kphio_par_a, kphio_par_b, rd_to_vcmax, kc_jmax
    fapar          = inputs$fapar,            # fraction  ,
    c4             = FALSE,
    method_jmaxlim = "wang17",
    do_ftemp_kphio = FALSE,        # corresponding to setup ORG
    do_soilmstress = FALSE,        # corresponding to setup ORG
    verbose        = TRUE
  ) |> 
    tidyr::as_tibble()
  
  # compute value with run_pmodel_onestep_f_bysite:
  resF <- run_pmodel_onestep_f_bysite(
    lc4 = FALSE,
    forcing = data.frame(temp = inputs$temp, vpd = inputs$vpd, ppfd = inputs$ppfd, 
                         co2 = inputs$co2, patm = inputs$patm),
    params_modl = list(
      kphio              = inputs$kphio,
      kphio_par_a        = inputs$kphio_par_a,
      kphio_par_b        = inputs$kphio_par_b,
      beta_unitcostratio = inputs$beta_unitcostratio,
      rd_to_vcmax        = inputs$rd_to_vcmax,
      kc_jmax            = inputs$kc_jmax
    ),
    makecheck = TRUE
  )
  # testthat::expect_equal(resF, resR) 
  # NOTE: this fails because of different units.
  
  # Fix units for comparison
  resR_units_fixed <- resR |> 
    dplyr::mutate(
      gs   = gs,                 # NOTE: keep units as-is: (mol C m-2 Pa-1)      (computed as A/(ca-ci))
      iwue = iwue / inputs$patm, # NOTE: was initially: (Pa)                     (computed as (ca-ci)/1.6)
                                 # NOTE: is now: (unitless)
      rd   = rd * 12.0107) |>    # NOTE: was initially: 2.82e-7 mol C m-2 s-1    (computed as 0.015*Vcmax*(fr/fv))
                                 # NOTE: is now: (3.39e-6 g C m-2 s-1) # still slightly different from 3.16e-6, but remains within tolerance
    dplyr::select(-ns_star, -xi, -mj, -mc, -ci,
                  -gpp, -ca, -gammastar, -kmm) |>
    dplyr::select(vcmax, jmax, vcmax25, jmax25, chi, gs, iwue, rd) |>
    dplyr::select(-vcmax25, -jmax25) # NOTE: as long as rpmodel is not updated with Kumarathunge T-dependency, we can only compare vcmax and jmax

  resF_units_fixed <- resF |> 
    dplyr::mutate(
      gs   = gs_accl * inputs$ppfd*inputs$fapar,    
                                 # NOTE: was initially: mol C / mol photons Pa-1 (computed as lue/molmass / (ca-ci+0.1))
                                 # NOTE: is now: mol C m-2 Pa-1 s-1              (computed as lue/molmass*iabs / (ca-ci+0.1))
                                 # NOTE: is now:gs with 4.75e-7 still slightly different from 4.79e-7, but remains within tolerance
      iwue = iwue,               # NOTE: keep units as-is: (-)                   (computed as (ca-ci)/1.6/patm)
      rd   = rd) |>              # NOTE: keep units as-is: 3.16e-6 g C m-2 s-1   (computed as rd_to_vcmax*vcmax25*calc_ftemp_inst_rd(Temp)*c_molmass
    dplyr::select(-gs_accl) |>
    dplyr::select(vcmax, jmax, vcmax25, jmax25, chi, gs, iwue, rd) |>
    dplyr::select(-vcmax25, -jmax25) # NOTE: as long as rpmodel is not updated with Kumarathunge T-dependency, we can only compare vcmax and jmax
  

  # Now comparison must pass
  testthat::expect_equal(resR_units_fixed, resF_units_fixed, tolerance = 1e-5)
  # If this test fails it means that the output of the model is incompatible with
  # the package {rpmodel}.
  # It could either mean that:
  # - the model was accidentally altered and should be fixed to deliver the expected output
  # - the package {rpmodel} has been altered
 
  
  # Rerun again (inverse order) to test memory leakage:
  resF_rerun <- run_pmodel_onestep_f_bysite(
    lc4 = FALSE,
    forcing = data.frame(temp = inputs$temp, vpd = inputs$vpd, ppfd = inputs$ppfd, 
                         co2 = inputs$co2, patm = inputs$patm),
    params_modl = list(
      kphio              = inputs$kphio,
      kphio_par_a        = inputs$kphio_par_a,
      kphio_par_b        = inputs$kphio_par_b,
      beta_unitcostratio = inputs$beta_unitcostratio,
      rd_to_vcmax        = inputs$rd_to_vcmax,
      kc_jmax            = inputs$kc_jmax
    ),
    makecheck = TRUE
  )
  expect_equal(resF, 
               resF_rerun)
  
  expect_snapshot_value_fmt(resF, tolerance = 0.01, cran = TRUE)
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
  
  # test for correctly returned types
  expect_type(df_output, "list")
  
})

test_that("Check net C (and N) balances without/with land-use-change", {
  skip_on_cran()
  
  # read in demo data
  df_drivers_BiomeE_PLULUC <- rsofun::biomee_p_model_luluc_drivers
  
  # modify 
  df_drivers_BiomeE_PLULUC$params_siml[[1]]$do_daily_diagnostics <- TRUE
  df_drivers_BiomeE_PLULUC$params_siml[[1]]$spinupyears <- 1000
  df_drivers_BiomeE_PLULUC$params_siml[[1]]$nyeartrend <- 251
  df_drivers_BiomeE_PLULUC$forcing[[1]] <- df_drivers_BiomeE_PLULUC$forcing[[1]] |>
    # repeat forcing and update dates
    list() |> rep(251) |> dplyr::bind_rows(.id = "repeatedyear") |> 
    # # While we could change the date of each row with below code, 
    # # it is actually not needed since it is not read by run_biomee_f_bysite()
    # # mutate(date = date + lubridate::years(as.numeric(repeatedyear) - 1)) |> 
    dplyr::select(-repeatedyear)
  
  # A) First simulation without LUC:
  # df_drivers_BiomeE_PLULUC$params_siml[[1]]$do_closedN_run <- FALSE # TODO: keep this FALSE?
  df_drivers_BiomeE_PLULUC$luc_forcing[[1]] <- structure(c(0, 0, 0, 0), dim = c(2, 2, 1))
  
  # B) Second simulation with LUC:
  df_drivers_BiomeE_PLULUC_2 <- df_drivers_BiomeE_PLULUC
  df_drivers_BiomeE_PLULUC_2$luc_forcing[[1]] <- structure(c(0, 0, 0.5, 0), dim = c(2, 2, 1))
  
  # A/B) run simulations
  mod_BiomeE_PLULUC_1 <- run_biomee_f_bysite(
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
  mod_BiomeE_PLULUC_2 <- run_biomee_f_bysite(
    sitename       = df_drivers_BiomeE_PLULUC_2$sitename[1],
    params_siml    = df_drivers_BiomeE_PLULUC_2$params_siml[[1]],
    site_info      = df_drivers_BiomeE_PLULUC_2$site_info[[1]],
    forcing        = df_drivers_BiomeE_PLULUC_2$forcing[[1]],
    params_tile    = df_drivers_BiomeE_PLULUC_2$params_tile[[1]],
    params_species = df_drivers_BiomeE_PLULUC_2$params_species[[1]],
    init_cohort    = df_drivers_BiomeE_PLULUC_2$init_cohort[[1]],
    init_soil      = df_drivers_BiomeE_PLULUC_2$init_soil[[1]],
    init_lu        = df_drivers_BiomeE_PLULUC_2$init_lu[[1]],
    luc_forcing    = df_drivers_BiomeE_PLULUC_2$luc_forcing[[1]],
    makecheck      = TRUE
  )
  
  # A/B) postprocess (per LU) for test
  df_balance1 <- dplyr::bind_rows(.id = "LU",
                           aggregated= mod_BiomeE_PLULUC_1$aggregated$output_annual_cell,
                           primary   = mod_BiomeE_PLULUC_1$primary$output_annual_tile,
                           secondary = mod_BiomeE_PLULUC_1$secondary$output_annual_tile) |> 
    dplyr::tibble() |>
    tidyr::replace_na(replace = list(
      Rprod_0_C = 0, Rprod_0_N = 0,
      Rprod_1_C = 0, Rprod_1_N = 0,
      Rprod_2_C = 0, Rprod_2_N = 0)) |>
    # postprocess:
    dplyr::group_by(LU) |>
    dplyr::mutate(                                  # * lu_fraction: scales values from per m2 tile to per m2 landscape
           `C_balance_+Gpp-Rauto-Rh` = (GPP-Rauto-Rh) * lu_fraction, # kg C m-2 yr-1
           `C_balance_+Npp-Rh`       = (NPP      -Rh) * lu_fraction, # kg C m-2 yr-1
           `C_pool`                  = (plantC+soilC) * lu_fraction, # kg C m-2
           `N_pool`                  = (plantN+soilN) * lu_fraction, # kg N m-2
           `C_balance_PlantSoilPools`= c(0,diff(C_pool)),            # kg C m-2 yr-1
           `N_balance_PlantSoilPools`= c(0,diff(N_pool)),            # kg C m-2 yr-1
           `C_balance_+Gpp-Rauto-Rh-Prods` = `C_balance_+Gpp-Rauto-Rh` - Rprod_0_C - Rprod_1_C - Rprod_2_C # kg C yr-1 (m-2 grid cell)
    )
  df_balance2 <- dplyr::bind_rows(.id = "LU",
                           aggregated= mod_BiomeE_PLULUC_2$aggregated$output_annual_cell,
                           primary   = mod_BiomeE_PLULUC_2$primary$output_annual_tile,
                           secondary = mod_BiomeE_PLULUC_2$secondary$output_annual_tile) |> 
    dplyr::tibble() |>
    tidyr::replace_na(replace = list(
      Rprod_0_C = 0, Rprod_0_N = 0,
      Rprod_1_C = 0, Rprod_1_N = 0,
      Rprod_2_C = 0, Rprod_2_N = 0)) |>
    # postprocess:
    dplyr::group_by(LU) |>
    dplyr::mutate(                                 # * lu_fraction: scales values from per m2 tile to per m2 landscape
           `C_balance_+Gpp-Rauto-Rh` = (GPP-Rauto-Rh) * lu_fraction, # kg C m-2 yr-1
           `C_balance_+Npp-Rh`       = (NPP      -Rh) * lu_fraction, # kg C m-2 yr-1
           `C_pool`                  = (plantC+soilC) * lu_fraction, # kg C m-2
           `N_pool`                  = (plantN+soilN) * lu_fraction, # kg N m-2
           `C_balance_PlantSoilPools`= c(0,diff(C_pool)),            # kg C m-2 yr-1
           `N_balance_PlantSoilPools`= c(0,diff(N_pool)),            # kg C m-2 yr-1
           `C_balance_+Gpp-Rauto-Rh-Prods` = `C_balance_+Gpp-Rauto-Rh` - Rprod_0_C - Rprod_1_C - Rprod_2_C # kg C yr-1 (m-2 grid cell)
    )
  
  # A/B) prepare balances for tests
  df_balance1_totest <- df_balance1 |> 
    dplyr::select(LU, year, C_pool, N_pool, `C_balance_+Gpp-Rauto-Rh-Prods`) |>
    dplyr::mutate(C__pooldiff = c(0, diff(C_pool)),
                  N__pooldiff = c(0, diff(N_pool))) |>
    dplyr::filter(LU != "secondary")
  
  year_of_LUC <- 1 + df_drivers_BiomeE_PLULUC$params_siml[[1]]$spinupyears
  df_balance2_totest <- df_balance2 |> 
    dplyr::select(LU, year, C_pool, N_pool, `C_balance_+Gpp-Rauto-Rh-Prods`) |>
    dplyr::mutate(C__pooldiff = c(0, diff(C_pool)),
                  N__pooldiff = c(0, diff(N_pool))) |>
    dplyr::filter(year != year_of_LUC)
  
  # A/B) Plot the tests
  # # A/B) test equivalence of net C fluxes and changes of C pools
  # pl1 <- ggplot(df_balance1_totest, aes(x=year)) + facet_grid(~LU) +
  #   geom_line(aes(y=C__pooldiff,                     color = "diff(C_pool)")) +
  #   geom_line(aes(y=`C_balance_+Gpp-Rauto-Rh-Prods`, color = "+Gpp-Rauto-Rh-Prods"), linetype="dashed") +
  #   scale_color_manual(values = c("diff(C_pool)" = "black", "+Gpp-Rauto-Rh-Prods" = "darkred")) +
  #   theme_classic() + labs(x = "Year", y = "kg C m-2 year-1", linetype = NULL, color = NULL) +
  #   # coord_cartesian(xlim = c(1000, NA)) +
  #   theme(legend.position = c(0,1), legend.justification = c(-0.2,1))
  # pl2 <- ggplot(df_balance2_totest, aes(x=year)) + facet_grid(~LU) +
  #   geom_line(aes(y=C__pooldiff,                     color = "diff(C_pool)")) +
  #   geom_line(aes(y=`C_balance_+Gpp-Rauto-Rh-Prods`, color = "+Gpp-Rauto-Rh-Prods"), linetype="dashed") +
  #   scale_color_manual(values = c("diff(C_pool)" = "black", "+Gpp-Rauto-Rh-Prods" = "darkred")) +
  #   theme_classic() + labs(x = "Year", y = "kg C m-2 year-1", linetype = NULL, color = NULL) +
  #   theme(legend.position = c(0,1), legend.justification = c(-0.2,1))
  # # A) test steady states in C and N pools
  # pl3 <- ggplot(df_balance1_totest, aes(x=year)) + facet_grid(~LU) +
  #   geom_line(aes(y=N__pooldiff), color = "black") +
  #   theme_classic() + labs(x = "Year", y = "kg N m-2 year-1", linetype = NULL, color = NULL)
  # pl4 <- ggplot(df_balance2_totest, aes(x=year)) + facet_grid(~LU) +
  #   geom_line(aes(y=N__pooldiff), color = "black") +
  #   theme_classic() + labs(x = "Year", y = "kg N m-2 year-1", linetype = NULL, color = NULL)
  # (pl1 + pl2) / (pl3 + pl4) + plot_annotation(tag_levels = 'a')# using patchwork

  # A/B) test equivalence of net C fluxes and changes of C pools
  testthat::expect_equal(tolerance = 0.03,
                         df_balance1_totest$`C_balance_+Gpp-Rauto-Rh-Prods`,
                         df_balance1_totest$C__pooldiff)
  testthat::expect_equal(tolerance = 0.03,
                         df_balance2_totest$`C_balance_+Gpp-Rauto-Rh-Prods`,
                         df_balance2_totest$C__pooldiff)
  
  # A) test steady states in C and N pools 
  #    in simulation A) (without LUC) 
  #    across the spinup and transient years, i.e. from 990 to 1250
  testthat::expect_true(
    all(abs(dplyr::filter(df_balance1_totest, year > 990)$N__pooldiff) < 1e-2)
  )

  testthat::expect_true(
    all(abs(dplyr::filter(df_balance1_totest, year > 990)$C__pooldiff) < 3e-2)
  )
  
})
