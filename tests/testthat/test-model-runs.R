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

test_that("p-model onestep output check (run_pmodel_onestep_f_bysite())", {
  skip_on_cran()
  
  # Define simulation inputs
  inputs <- list(
    # for forcing:
    temp  = 15,           # temperature, deg C
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
    dplyr::select(vcmax, jmax, vcmax25, jmax25, chi, gs, iwue, rd)

  resF_units_fixed <- resF |> 
    dplyr::mutate(
      gs   = gs_accl * inputs$ppfd*inputs$fapar,    
                                 # NOTE: was initially: mol C / mol photons Pa-1 (computed as lue/molmass / (ca-ci+0.1))
                                 # NOTE: is now: mol C m-2 Pa-1 s-1              (computed as lue/molmass*iabs / (ca-ci+0.1))
                                 # NOTE: is now:gs with 4.75e-7 still slightly different from 4.79e-7, but remains within tolerance
      iwue = iwue,               # NOTE: keep units as-is: (-)                   (computed as (ca-ci)/1.6/patm)
      rd   = rd) |>              # NOTE: keep units as-is: 3.16e-6 g C m-2 s-1   (computed as rd_to_vcmax*vcmax25*calc_ftemp_inst_rd(Temp)*c_molmass
    dplyr::select(-wscal, -gs_accl) |>
    dplyr::select(vcmax, jmax, vcmax25, jmax25, chi, gs, iwue, rd)
  

  # Now comparison must pass
  testthat::expect_equal(resR_units_fixed, resF_units_fixed, tolerance = 1e-5)
  # If this test fails it means that the output of the model is incompatible with
  # the package {rpmodel}.
  # It could either mean that:
  # - the model was accidentally altered and should be fixed to deliver the expected output
  # - the package {rpmodel} has been altered
 
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
    list() |> rep(251) |> dplyr::bind_rows(.id = "repeatedyear") |> 
    # While we could change the date of each row with below code, 
    # it is actually not needed since it is not read by run_biomee_f_bysite()
    # mutate(date = date + lubridate::years(as.numeric(repeatedyear) - 1)) |> 
    select(-repeatedyear)
  df_drivers_BiomeE_PLULUC$forcing[[1]] <- df_drivers_BiomeE_PLULUC$forcing[[1]] |>
    # repeat forcing and update dates
    list() |> rep(251) |> dplyr::bind_rows(.id = "repeatedyear") |> 
    # While we could change the date of each row with below code, 
    # it is actually not needed since it is not read by run_biomee_f_bysite()
    # mutate(date = date + lubridate::years(as.numeric(repeatedyear) - 1)) |> 
    select(-repeatedyear)
  df_drivers_BiomeE_gsLeun$forcing[[1]] <- df_drivers_BiomeE_gsLeun$forcing[[1]] |>
    # repeat forcing and update dates
    list() |> rep(251) |> dplyr::bind_rows(.id = "repeatedyear") |> 
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
    SoilWater = c(799.944641113281,799.947204589844,799.131042480469,799.930847167969,799.903991699219),
    Transp    = c(0,0,0,0,0),
    Evap      = c(0.0553628616034985,0.0527696460485458,0.868921220302582,0.0691292807459831,0.0960086733102798),
    Runoff    = c(1.43654549121857,1.94845604896545,1.08043730258942,1.96995043754578,1.75696134567261),
    ws1       = c(19.944637298584,19.9472312927246,19.1310787200928,19.930871963501,19.9039936065674),
    ws2       = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3       = c(600,600,600,600,600),
    LAI       = c(0,0.000364852574421093,0.00466133235022426,0.00618919776752591,0.00619784602895379),
    NPP       = c(-2.97977797991678e-09, -5.82730172027368e-05, 1.41528635140276e-05,1.6193821466004e-06, 1.58737884703442e-06),
    GPP       = c(0,0,1.90892133105081e-05,6.17546766079613e-06,6.16563283983851e-06),
    Rauto     = c(2.97977797991678e-09,5.82730172027368e-05,4.93635025122785e-06,4.55608324045897e-06,4.57825399280409e-06),
    Rh        = c(2.77344884125341e-06,2.75237016467145e-06,1.03804013633635e-05,2.3680029244133e-06,2.36252321883512e-06),
    NSC       = c(0.00582691049203277,0.00565209938213229,0.00398708041757345,0.00424133753404021,0.00423404527828097),
    seedC     = c(0,0,0,0,0),
    leafC     = c(0,6.20249411440454e-05,0.000792426522821188,0.00105216365773231,0.00105363386683166),
    rootC     = c(0,3.70325360563584e-05,0.000473125197459012,0.000628203561063856,0.000629081332590431),
    sapwoodC  = c(0.00250000017695129,0.00163697078824043,0.000808653887361288,0.00114870828110725,0.00115068175364286),
    heartwoodC= c(0,0.000880510022398084,0.00244240881875157,0.002835190622136,0.00283709052018821),
    NSN       = c(0.000293089426122606,0.000291046482743695,0.00025848179939203,0.000238109059864655,0.000237988890148699),
    seedN     = c(0,0,0,0,0),
    leafN     = c(0,1.06719380710274e-06,1.36344051497872e-05,1.81034265551716e-05,1.81287232408067e-05),
    rootN     = c(0,9.25813424146327e-07,1.18281122922781e-05,1.57050762936706e-05,1.57270224008244e-05),
    sapwoodN  = c(7.14285715730512e-06,4.67705922346795e-06,2.31044009524339e-06,3.28202372656961e-06,3.28766213897325e-06),
    heartwoodN= c(0,2.5157430627587e-06,6.97831228535506e-06,8.10054189059883e-06,8.10597066447372e-06),
    mcrbC     = c(3.69218014384387e-07,7.35389221517835e-07,9.97748793452047e-05,0.000234167644521222,0.000234328705118969),
    fastSOM   = c(0.009996865876019,0.00999375618994236,0.00931816641241312,0.00799902994185686,0.00799865275621414),
    slowSOM   = c(0.000999990850687027,0.000999981770291924,0.00105635263025761,0.00113423995207995,0.00113475287798792),
    mcrbN     = c(3.6921800727896e-08,7.3538920730698e-08,9.97748793452047e-06,2.34167637245264e-05,2.34328708756948e-05),
    fastSoilN = c(0.000666457752231508,0.000666250416543335,0.000621211074758321,0.000531849800609052,0.000531776517163962),
    slowSoilN = c(2.49997719947714e-05,2.4999544621096e-05,2.58212767221266e-05,2.69752108579269e-05,2.69825650320854e-05),
    mineralN  = c(0.0149249145761132,0.0148505428805947,0.00587099697440863,0.00320566212758422,0.00321121863089502),
    N_uptk    = c(0,0,0,0,0))
  ref_BiomeE_Pmodel_odt_yr251 <- tibble(
    year      = c(251,251,251,251,251),
    doy       = c(1,2,180,364,365),
    Tk        = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp      = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    SoilWater = c(799.960083007812,799.958984375,799.516296386719,799.951416015625,799.930053710938),
    Transp    = c(0,0,0,0,0),
    Evap      = c(0.0399133116006851,0.0409998670220375,0.483673214912415,0.0485905706882477,0.0699304714798927),
    Runoff    = c(1.36660242080688,1.96390557289124,1.74325275421143,1.99354791641235,1.77749955654144),
    ws1       = c(19.9600887298584,19.9590015411377,19.5163269042969,19.9514083862305,19.9300708770752),
    ws2       = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3       = c(600,600,600,600,600),
    LAI       = c(3.4922297000885,3.49872899055481,3.61742806434631,3.74305200576782,3.74376797676086),
    NPP       = c(0.00221638171933591, -0.000123182544484735, 0.00693395920097828, 0.000492333900183439, 0.000476401066407561),
    GPP       = c(0.00227762758731842,0.00224546459503472,0.00936589390039444,0.00246698618866503,0.00246065971441567),
    Rauto     = c(6.12458534305915e-05,0.00236861989833415,0.0024318634532392,0.00197463692165911,0.00198430172167718),
    Rh        = c(0.00146035756915808,0.00145140045788139,0.00574139412492514,0.00141523755155504,0.001413878868334),
    NSC       = c(2.14304780960083,2.13828492164612,2.1303768157959,2.2305862903595,2.22722339630127),
    seedC     = c(0.0019968063570559,0.0021789469756186,0.0329009033739567,0.0699842870235443,0.070181593298912),
    leafC     = c(0.593679130077362,0.594783842563629,0.614962816238403,0.636318802833557,0.636440634727478),
    rootC     = c(0.360928446054459,0.36085844039917,0.365907251834869,0.379917114973068,0.379989683628082),
    sapwoodC  = c(4.5676908493042,4.55334377288818,4.69937181472778,4.90116739273071,4.90223789215088),
    heartwoodC= c(3.35169792175293,3.36794519424438,3.51899361610413,3.66643118858337,3.66720104217529),
    NSN       = c(0.0614702217280865,0.0614091902971268,0.0626659020781517,0.0639448463916779,0.0638884156942368),
    seedN     = c(9.98403047560714e-05,0.000108947337139398,0.00164504500571638,0.00349921477027237,0.00350908027030528),
    leafN     = c(0.0102148456498981,0.0102338567376137,0.0105810575187206,0.0109485015273094,0.0109505970031023),
    rootN     = c(0.00902319885790348,0.00902144890278578,0.00914766546338797,0.00949791260063648,0.00949972867965698),
    sapwoodN  = c(0.0130505440756679,0.0130095547065139,0.0134267760440707,0.0140033354982734,0.0140063930302858),
    heartwoodN= c(0.00957628153264523,0.00962270423769951,0.0100542698055506,0.0104755172505975,0.0104777161031961),
    mcrbC     = c(0.163020133972168,0.163022756576538,0.164115786552429,0.163002550601959,0.162999197840691),
    fastSOM   = c(2.19501852989197,2.19556164741516,2.2025363445282,2.03177905082703,2.03243160247803),
    slowSOM   = c(72.7654800415039,72.765007019043,72.5841827392578,72.1644821166992,72.1640319824219),
    mcrbN     = c(0.0163020137697458,0.0163022764027119,0.0164115782827139,0.016300255432725,0.0162999201565981),
    fastSoilN = c(0.113785140216351,0.113796405494213,0.113353185355663,0.107105150818825,0.107118561863899),
    slowSoilN = c(0.675333976745605,0.675336599349976,0.675630569458008,0.675470352172852,0.675473272800446),
    mineralN  = c(0.00221035978756845,0.00223766081035137,0.00152802339289337,0.00226603355258703,0.00229924730956554),
    N_uptk    = c(0.000141792901558802,6.19682805336197e-06,0,0,0))
  ref_BiomeE_Pmodel_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00416251085698605,0.00511125242337584,0.013907034881413,0.0160003751516342,0.0483339205384254,1.37583410739899),
    LAI             = c(0.00621013063937426,0.0097409076988697,0.0437882207334042,0.0503804795444012,0.147045791149139,3.79896926879883),
    Density         = c(500,494.722503662109,462.518585205078,456.966278076172,2009.96313476562,46633.4140625),
    DBH             = c(0.675350725650787,0.7799192070961,1.58980083465576,1.75969135761261,1.17825710773468,1.076789021492),
    Density12       = c(0,0,0,0,0,202.66227722168),
    DBH12           = c(0,0,0,0,0,21.0354156494141),
    QMD12           = c(0,0,0,0,0,21.3040790557861),
    NPP             = c(0.00233111949637532,0.00477784592658281,0.0183418728411198,0.0211064927279949,0.0621540136635303,1.44544994831085),
    GPP             = c(0.00443709455430508,0.00688399979844689,0.0265300050377846,0.0306448806077242,0.0909370854496956,2.21131491661072),
    Rauto           = c(0.00210597505792975,0.00210615363903344,0.00818813219666481,0.00953838787972927,0.0287830717861652,0.765864968299866),
    Rh              = c(0.00236287247389555,0.00206780130974948,0.00345703633502126,0.00405465206131339,0.0121752610430121,1.34333610534668),
    Prcp            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903930664062,799.903991699219,799.904724121094,799.904846191406,799.90673828125,799.930114746094),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.691955566406,159.532897949219,157.880935668945,157.567352294922,153.254913330078,94.1740112304688),
    Runoff          = c(427.933654785156,427.996276855469,429.648162841797,429.962066650391,434.274169921875,493.355407714844),
    plantC          = c(0.00992458313703537,0.0133715905249119,0.0612290874123573,0.0742909759283066,0.251720786094666,11.774600982666),
    soilC           = c(0.00937056448310614,0.0086335800588131,0.0203695576637983,0.0243595018982887,0.080634132027626,75.1439895629883),
    totC            = c(0.0192951485514641,0.022005170583725,0.0815986469388008,0.0986504778265953,0.33235490322113,86.9185943603516),
    plantN          = c(0.000283172208582982,0.000251664518145844,0.00108302047010511,0.00127931591123343,0.00370623869821429,0.113182485103607),
    soilN           = c(0.00379347801208496,0.00337964971549809,0.00347966281697154,0.00358892441727221,0.00514161959290504,0.807599067687988),
    totN            = c(0.0040766503661871,0.00363131426274776,0.00456268340349197,0.00486824009567499,0.00884785875678062,0.920781552791595),
    NSC             = c(0.00424329005181789,0.00522226467728615,0.0213056188076735,0.0252688508480787,0.0783128142356873,2.24216532707214),
    seedC           = c(0,0,0.00155770848505199,0.00230354908853769,0.00411435309797525,0.0713211074471474),
    leafC           = c(0.00105572224128991,0.00165595440194011,0.00744399800896645,0.00856468174606562,0.0249977856874466,0.645824790000916),
    rootC           = c(0.000630328198894858,0.000988702056929469,0.00444450415670872,0.00511361798271537,0.0149251474067569,0.385595291852951),
    sapwoodC        = c(0.00115369341801852,0.00201713317073882,0.0156696904450655,0.0195541884750128,0.0758503675460815,4.79197645187378),
    heartwoodC      = c(0.0028415487613529,0.00348753668367863,0.0108075644820929,0.0134860882535577,0.0535203255712986,3.63771796226501),
    NSN             = c(0.00023783439246472,0.000182727162609808,0.000690292159561068,0.000794533698353916,0.0023276514839381,0.064779669046402),
    seedN           = c(0,0,7.78854373493232e-05,0.000115177543193568,0.000205717631615698,0.00356605648994446),
    leafN           = c(1.81646610144526e-05,2.84922152786748e-05,0.000128081010188907,0.000147363491123542,0.000430111074820161,0.0111120594665408),
    rootN           = c(1.57581798703177e-05,2.47175175900338e-05,0.000111112429294735,0.000127840336062945,0.000373128161299974,0.00963986292481422),
    sapwoodN        = c(3.2962670957204e-06,5.7632378229755e-06,4.47705460828729e-05,5.58691062906291e-05,0.000216715328861028,0.0136913610622287),
    heartwoodN      = c(8.11871268524555e-06,9.96438848233083e-06,3.0878814868629e-05,3.85317507607397e-05,0.000152915032231249,0.0103934742510319),
    mcrbC           = c(0.000234329476370476,0.000333390984451398,0.000647217035293579,0.00075562996789813,0.00225507956929505,0.164414137601852),
    fastSOM         = c(0.00800092332065105,0.00688283005729318,0.0125935832038522,0.0147168226540089,0.0437205471098423,2.05150580406189),
    slowSOM         = c(0.00113531190436333,0.00141735922079533,0.00712875789031386,0.00888704787939787,0.0346585102379322,72.9280700683594),
    mcrbN           = c(2.34329472732497e-05,3.33390999003313e-05,6.47217020741664e-05,7.55629953346215e-05,0.000225507959839888,0.0164414141327143),
    fastSoilN       = c(0.000531837635207921,0.000451616855571046,0.000564671528991312,0.000652465911116451,0.00191263970918953,0.108197376132011),
    slowSoilN       = c(2.69904885499272e-05,3.06092515529599e-05,0.000104368504253216,0.000126537634059787,0.000434708286775276,0.680824875831604),
    mineralN        = c(0.00321121700108051,0.00286408443935215,0.00274590100161731,0.00273435795679688,0.00256876368075609,0.00213536969386041),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.00033195482683368,0.000379318167688325,0.00106669578235596,0.0225101206451654),
    N_yrMin         = c(-0.0117887761443853,-0.00034713459899649,0.000321342697134241,0.000367776519851759,0.00102654949296266,0.0225571487098932),
    N_P2S           = c(2.0049003069289e-05,3.12088232021779e-05,0.000160182637046091,0.000185801400220953,0.000548946671187878,0.0224458295851946),
    N_loss          = c(0.0219152607023716,0.0104453265666962,0.00973560195416212,0.00969444587826729,0.00914391409605742,0.00739127490669489),
    totseedC        = c(0,0,0,0.00227513629943132,0.00364530761726201,0.0654599517583847),
    totseedN        = c(0,0,0,0.000113756905193441,0.000182265343028121,0.00327299861237407),
    Seedling_C      = c(0,0,0,0.00227513629943132,0.00364530761726201,0.0654599517583847),
    Seedling_N      = c(0,0,0,0.000113756905193441,0.000182265343028121,0.00327299861237407),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,79.7137832641602),
    MaxVolume       = c(0.000215959051274695,0.000300723884720355,0.00154718419071287,0.00195414945483208,0.00751346396282315,0.725371241569519),
    MaxDBH          = c(0.00675350753590465,0.00779919186607003,0.0158980078995228,0.0175969135016203,0.0316033586859703,0.230491578578949),
    NPPL            = c(0.00121943862177432,0.000885054818354547,0.00251281773671508,0.00284165726043284,0.00835549365729094,0.171732649207115),
    NPPW            = c(0.00149524281732738,0.00155159796122462,0.00575843034312129,0.00688086031004786,0.0218216553330421,0.658564686775208),
    n_deadtrees     = c(2.98888289762544e-06,2.68998428509803e-06,1.30011576402467e-05,1.57794547703816e-05,5.37226296728477e-05,0.009063552133739),
    c_deadtrees     = c(0.000104753984487616,0.000142925855470821,0.0007350267842412,0.00091632641851902,0.0037391115911305,0.889440536499023),
    m_turnover      = c(0.000104753984487616,0.000142925855470821,0.0007350267842412,0.00091632641851902,0.0037391115911305,0.889440536499023),
    c_turnover_time = c(1.9003928899765,2.24770641326904,1.87682473659515,1.95994222164154,2.45262455940247,5.52370643615723),
    lu_fraction     = c(1,1,1,1,1,1))
  ref_BiomeE_Pmodel_oac_yr1 <- tibble(
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
    NSC         = 0.0845033749938011,
    seedC       = 0.00475737359374762,
    leafC       = 0,
    rootC       = 0.0211020689457655,
    sapwoodC    = 0.0125991748645902,
    heartwoodC  = 0.0230530891567469,
    NSN         = 0.0567796900868416,
    treeG       = 0.0781993493437767,
    fseed       = 0,
    fleaf       = 0.311710923910141,
    froot       = 0.306792557239532,
    fwood       = 0.381496518850327,
    NPP         = 0.0461645908653736,
    GPP         = 0.0887204110622406,
    Rauto       = 0.042555820196867,
    N_uptk      = 0,
    N_fxed      = 0,
    deathrate   = 0.0105546750128269,
    n_deadtrees = 4.3513900891412e-06,
    c_deadtrees = 0.00010314847168047)
  ref_BiomeE_Pmodel_oac_yr2 <- tibble(
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
    NSC         = 0.104750856757164,
    seedC       = 0.00369688123464584,
    leafC       = 0,
    rootC       = 0.033416286110878,
    sapwoodC    = 0.0199514869600534,
    heartwoodC  = 0.0406682156026363,
    NSN         = 0.0703133046627045,
    treeG       = 0.0755703151226044,
    fseed       = 0,
    fleaf       = 0.236070990562439,
    froot       = 0.35174748301506,
    fwood       = 0.412181466817856,
    NPP         = 0.0958177149295807,
    GPP         = 0.138989970088005,
    Rauto       = 0.0431722514331341,
    N_uptk      = 0,
    N_fxed      = 0,
    deathrate   = 0.0106876138597727,
    n_deadtrees = 4.85187956655864e-06,
    c_deadtrees = 0.0001401223562425)
  ref_BiomeE_Pmodel_oac_yr251 <- tibble(
    year        = c(251,251,251,251,251),
    cID         = c(397,405,453,451,25),
    PFT         = c(2,2,2,2,2),
    layer       = c(1,1,1,2,2),
    density     = c(149.401016235352,53.2612571716309,6204.306640625,1829.57446289062,38396.87109375),
    flayer      = c(0.247986137866974,0.0482195615768433,0.781054198741913,0.169435322284698,0.129138872027397),
    DBH         = c(23.0491580963135,15.3867559432983,4.12981939315796,3.36543917655945,0.369073837995529),
    dDBH        = c(0.42443722486496,0.380046665668488,0.245740637183189,0.0657945871353149,0.0290652271360159),
    height      = c(17.2834339141846,14.1213445663452,7.31590461730957,6.604248046875,2.18705224990845),
    age         = c(79.7137832641602,79.7137832641602,25.7797470092773,25.7797470092773,2.81041598320007),
    BA          = c(0.0417253524065018,0.018594479188323,0.00133952882606536,0.000889556191395968,1.06983416117146e-05),
    dBA         = c(0.00152254849672318,0.000907208770513535,0.000154671724885702,3.44417640008032e-05,1.61867865244858e-06),
    Acrown      = c(16.5986919403076,9.0534029006958,1.25889039039612,0.926091492176056,0.033632654696703),
    Aleaf       = c(52.2818565368652,28.5152816772461,3.96451163291931,1.45851707458496,0.0363169871270657),
    NSC         = c(38.3595199584961,20.333080291748,2.39638876914978,0.283469200134277,0.00576031673699617),
    seedC       = c(0.822567701339722,0.448743045330048,0.0623313896358013,0.022894911468029,0.0018853775691241),
    leafC       = c(1.24791276454926,0.658048391342163,0.0752148330211639,0.0137011418119073,0),
    rootC       = c(8.88791561126709,4.8475980758667,0.673967003822327,0.24794790148735,0.00617388775572181),
    sapwoodC    = c(5.30660772323608,2.89430069923401,0.402397900819778,0.14803946018219,0.00368617381900549),
    heartwoodC  = c(158.889007568359,62.7212791442871,3.04479670524597,0.978022515773773,0.00418663211166859),
    NSN         = c(109.498344421387,43.2269439697266,2.09915590286255,2.23453903198242,0.0157208405435085),
    treeG       = c(20.9510726928711,11.251781463623,1.43992221355438,0.33076623082161,0.0105965211987495),
    fseed       = c(0.0595631934702396,0.0584839284420013,0.0522353462874889,0.0414224341511726,0),
    fleaf       = c(0.0977673605084419,0.102879054844379,0.133100807666779,0.0548723526299,0.31301686167717),
    froot       = c(0.306600600481033,0.31228169798851,0.344545662403107,0.472529411315918,0.363980025053024),
    fwood       = c(0.536068797111511,0.52635532617569,0.470118194818497,0.431175768375397,0.323003113269806),
    NPP         = c(22.0481491088867,12.040563583374,1.67459845542908,0.0610319674015045,0.000463664066046476),
    GPP         = c(33.8128623962402,18.3549690246582,2.48450422286987,0.236606135964394,0.00615543918684125),
    Rauto       = c(11.7647142410278,6.31440544128418,0.809905707836151,0.175574168562889,0.00569177512079477),
    N_uptk      = c(0.313951283693314,0.175001218914986,0.0268299877643585,0.00131960189901292,0),
    N_fxed      = c(0,0,0,0,0),
    deathrate   = c(0.120657943189144,0.0703560188412666,0.0183926019817591,0.155317112803459,0.351918429136276),
    n_deadtrees = c(0.00349239259958267,0.000352286559063941,0.00116903800517321,0.00115728343371302,0.00289255077950656),
    c_deadtrees = c(0.580791890621185,0.0504684299230576,0.0991864204406738,0.110986568033695,0.0480072423815727))
  ref_BiomeE_gsLeun_odt_yr1 <- tibble(
    year      = c(1,1,1,1,1),
    doy       = c(1,2,180,364,365),
    Tk        = c(273.533660888672,271.508483886719,290.34521484375,271.626373291016,273.387603759766),
    Prcp      = c(1.43654537200928,2.00381803512573,2.4158182144165,2.05754518508911,1.8260909318924),
    SoilWater = c(799.998291015625,799.998168945312,799.987243652344,799.997924804688,799.997619628906),
    Transp    = c(0,1.49713048358535e-06,0.00434475671499968,6.02663967583794e-05,0.000113291425805073),
    Evap      = c(0.0556573569774628,0.0521354898810387,0.931808114051819,0.070465937256813,0.100166037678719),
    Runoff    = c(1.38256895542145,1.95178186893463,0.626788854598999,1.98649656772614,1.72622036933899),
    ws1       = c(19.9983215332031,19.9982204437256,19.9872646331787,19.9979648590088,19.997652053833),
    ws2       = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3       = c(600,600,600,600,600),
    LAI       = c(0,0.000364852749044076,0.00462460890412331,0.00595784047618508,0.00596493016928434),
    NPP       = c(-2.98780267193877e-09,-5.82156390009914e-05,8.06788011686876e-06,-3.13597115564335e-06,-2.59934495261405e-06), 
    GPP       = c(0,6.26106597678699e-08,1.37183378683403e-05,4.5652640778826e-07,1.09515019630635e-06),
    Rauto     = c(2.98780267193877e-09,5.82782486162614e-05,5.65045684197685e-06,3.59244927494728e-06,3.69437634617498e-06),
    Rh        = c(2.7720184334612e-06,2.75420779871638e-06,1.03837501228554e-05,2.36792357100057e-06,2.36069990933174e-06),
    NSC       = c(0.00582691328600049,0.00565216084942222,0.00336788222193718,0.00269267871044576,0.00268313591368496),
    seedC     = c(0,0,0,0,0),
    leafC     = c(0,6.20249702478759e-05,0.000786183518357575,0.0010128328576684,0.00101403810549527),
    rootC     = c(0,3.70325542462524e-05,0.000469397753477097,0.000604720728006214,0.000605440291110426),
    sapwoodC  = c(0.00250000017695129,0.00163697078824043,0.000798859749920666,0.00108340277802199,0.00108491943683475),
    heartwoodC= c(0,0.000880510022398084,0.00241249590180814,0.0026732471305877,0.00267418939620256),
    NSN       = c(0.000293089426122606,0.000291046482743695,0.000258813262917101,0.000240256078541279,0.000240150795434602),
    seedN     = c(0,0,0,0,0),
    leafN     = c(0,1.06719437553693e-06,1.35269801830873e-05,1.7426687918487e-05,1.74474243976874e-05),
    rootN     = c(0,9.2581376520684e-07,1.17349400170497e-05,1.51180129250861e-05,1.51360018207924e-05),
    sapwoodN  = c(7.14285715730512e-06,4.67705922346795e-06,2.28245676225924e-06,3.0954361136537e-06,3.09976962853398e-06),
    heartwoodN= c(0,2.5157430627587e-06,6.89284070176654e-06,7.63785374147119e-06,7.64054584578844e-06),
    mcrbC     = c(3.68914641057927e-07,7.35217042802105e-07,9.97279857983813e-05,0.00023395313473884,0.000234114078921266),
    fastSOM   = c(0.00999687053263187,0.00999375898391008,0.00931745953857899,0.00799086131155491,0.00799041334539652),
    slowSOM   = c(0.00099999166559428,0.000999983283691108,0.00105622306000441,0.00113227101974189,0.00113276392221451),
    mcrbN     = c(3.68914641057927e-08,7.35217042802105e-08,9.97279857983813e-06,2.33953142014798e-05,2.34114086197224e-05),
    fastSoilN = c(0.000666458043269813,0.000666250591166317,0.000621163984760642,0.000531450554262847,0.000531375815626234),
    slowSoilN = c(2.49997920036549e-05,2.49995828198735e-05,2.58189174928702e-05,2.69468200713163e-05,2.6953917767969e-05),
    mineralN  = c(0.013301195576787,0.0117978919297457,0.0002205292112194,0.000225525363930501,0.000225552576011978),
    N_uptk    = c(0,0,0,0,0))
  ref_BiomeE_gsLeun_odt_yr251 <- tibble(
    year      = c(251,251,251,251,251),
    doy       = c(1,2,180,364,365),
    Tk        = c(273.533660888672,271.508483886719,290.34521484375,271.626373291016,273.387603759766),
    Prcp      = c(1.43654537200928,2.00381803512573,2.4158182144165,2.05754518508911,1.8260909318924),
    SoilWater = c(799.9970703125,799.996948242188,754.776611328125,799.996459960938,799.995849609375),
    Transp    = c(0.0292789563536644,0.0221749134361744,2.29914855957031,0.0282737705856562,0.0520914532244205),
    Evap      = c(0.0411604531109333,0.0415968708693981,0.532630681991577,0.0503083132207394,0.0731317102909088),
    Runoff    = c(1.36480987071991,1.9401947259903,0,1.97796106338501,1.70130407810211),
    ws1       = c(19.9981384277344,19.9980201721191,19.9853935241699,19.9977245330811,19.997371673584),
    ws2       = c(179.999176025391,179.999145507812,157.093017578125,179.998977661133,179.998794555664),
    ws3       = c(599.999755859375,599.999755859375,577.698181152344,599.999755859375,599.999694824219),
    LAI       = c(2.71642017364502,2.71631741523743,2.5534508228302,2.83028817176819,2.82902693748474),
    NPP       = c(0.000357679848093539,-0.000545259565114975,0.0043612034060061,-0.000327472342178226,-9.05703927855939e-05),
    GPP       = c(0.000462727446574718,0.000195887172594666,0.00715355342254043,0.000202683484531008,0.000482457893667743),
    Rauto     = c(0.000105033133877441,0.000737429596483707,0.00279316166415811,0.000530223769601434,0.000573102035559714),
    Rh        = c(0.000727456819731742,0.000724036071915179,0.00292010395787656,0.000717765477020293,0.000716762384399772),
    NSC       = c(0.144965410232544,0.143055111169815,0.27723091840744,0.179523959755898,0.178501188755035),
    seedC     = c(0.013337817043066,0.0133533850312233,0.0164945479482412,0.0229069758206606,0.0229256767779589),
    leafC     = c(0.461791336536407,0.461773931980133,0.434086710214615,0.481148988008499,0.48093456029892),
    rootC     = c(0.273726612329483,0.273656159639359,0.25735729932785,0.285051465034485,0.284763097763062),
    sapwoodC  = c(3.64496064186096,3.64468097686768,3.70324563980103,3.81750178337097,3.81761574745178),
    heartwoodC= c(2.59264850616455,2.59320855140686,2.64516043663025,2.72638130187988,2.72647452354431),
    NSN       = c(0.00304208789020777,0.00302907708100975,0.00281427963636816,0.00313978921622038,0.00313498498871922),
    seedN     = c(0.000666890700813383,0.000667669111862779,0.000824727118015289,0.00114534795284271,0.00114628300070763),
    leafN     = c(0.00794553384184837,0.00794523395597935,0.00746885221451521,0.00827859714627266,0.00827490817755461),
    rootN     = c(0.00684316549450159,0.00684140529483557,0.00643393443897367,0.00712628616020083,0.00711907725781202),
    sapwoodN  = c(0.0104141738265753,0.0104133738204837,0.0105807008221745,0.0109071461483836,0.0109074730426073),
    heartwoodN= c(0.00740752369165421,0.00740912463515997,0.00755755696445704,0.00778961926698685,0.00778988702222705),
    mcrbC     = c(0.0912828966975212,0.0912832543253899,0.091776430606842,0.0913321822881699,0.0913312807679176),
    fastSOM   = c(1.23357093334198,1.23411810398102,1.26667034626007,1.20393180847168,1.20452332496643),
    slowSOM   = c(32.3874778747559,32.3873443603516,32.3175201416016,32.1459503173828,32.145824432373),
    mcrbN     = c(0.00912828929722309,0.0091283256188035,0.00917764287441969,0.00913321785628796,0.00913312844932079),
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
    Prcp            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.997619628906,799.997619628906,799.997619628906,799.997619628906,799.997619628906,799.995849609375),
    Transp          = c(0.704922735691071,1.0637092590332,3.14139652252197,3.45765209197998,6.422682762146,375.697387695312),
    Evap            = c(176.998413085938,176.84342956543,175.920715332031,175.784408569336,174.523239135742,105.35230255127),
    Runoff          = c(409.847991943359,409.645812988281,408.499145507812,408.317230224609,406.602905273438,106.478538513184),
    plantC          = c(0.00805916078388691,0.00890088453888893,0.0226384159177542,0.0258787497878075,0.0573301501572132,7.51000690460205),
    soilC           = c(0.00935985427349806,0.00851631723344326,0.0132406465709209,0.0148678496479988,0.0315968729555607,33.4446449279785), 
    totC            = c(0.017419021576643,0.0174172185361385,0.0358792245388031,0.0407467931509018,0.0889277309179306,40.947639465332),
    plantN          = c(0.000283414934528992,0.000254236190812662,0.000544839189387858,0.00060827744891867,0.00117187574505806,0.0383445397019386),
    soilN           = c(0.000807353353593498,0.000737329595722258,0.000728220154996961,0.000770614948123693,0.00125773821491748,0.390489995479584),
    totN            = c(0.00109076825901866,0.00099156575743109,0.00127305928617716,0.00137889245525002,0.00242961384356022,0.428834527730942),
    NSC             = c(0.00267619988881052,0.00207317643798888,0.0055290344171226,0.00627270294353366,0.0132943727076054,0.177580147981644),
    seedC           = c(0,0,0.000427327468059957,0.000620634760707617,0.000398915872210637,0.0229441840201616),
    leafC           = c(0.00101524242199957,0.00145371851976961,0.00401692790910602,0.00441436562687159,0.00815584324300289,0.480720341205597),
    rootC           = c(0.00060615933034569,0.000867955386638641,0.00239834189414978,0.00263563543558121,0.00486951740458608,0.284466713666916),
    sapwoodC        = c(0.00108643516432494,0.00165165890939534,0.0060768686234951,0.00706456881016493,0.0178665891289711,3.8177285194397),
    heartwoodC      = c(0.00267512421123683,0.00285437563434243,0.00418991595506668,0.00487084407359362,0.0127449110150337,2.72656750679016),
    NSN             = c(0.000240045483224094,0.000194650419871323,0.000365065439837053,0.000401300232624635,0.000802401336841285,0.00311649404466152),
    seedN           = c(0,0,2.13663643080508e-05,3.10317336698063e-05,1.99457972485106e-05,0.00114720838610083),
    leafN           = c(1.74681463249726e-05,2.50125358434161e-05,6.91151726641692e-05,7.59534523240291e-05,0.000140329400892369,0.00827122293412685),
    rootN           = c(1.5153977983573e-05,2.16988610191038e-05,5.99584927840624e-05,6.58908247714862e-05,0.000121737823064905,0.00711166858673096),
    sapwoodN        = c(3.10409996018279e-06,4.71902512799716e-06,1.736248304951e-05,2.01844832190545e-05,5.10473946633283e-05,0.0109077943488955),
    heartwoodN      = c(7.64321612223284e-06,8.1553607742535e-06,1.19711958177504e-05,1.39167123052175e-05,3.64140396413859e-05,0.00779015105217695),
    mcrbC           = c(0.000234114078921266,0.000331696297507733,0.000457518734037876,0.000494118605274707,0.000907054985873401,0.0913312807679176),
    fastSOM         = c(0.00799247156828642,0.00679179467260838,0.00791259575635195,0.00864304229617119,0.0160311684012413,1.20549154281616),
    slowSOM         = c(0.00113326858263463,0.00139282585587353,0.00487049063667655,0.00573063641786575,0.0146584585309029,32.1460647583008),
    mcrbN           = c(2.34114086197224e-05,3.31696282955818e-05,4.57518726761919e-05,4.94118612550665e-05,9.07055000425316e-05,0.00913312844932079),
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
    NSC         = 0.0535239949822426,
    seedC       = 0.00480090966448188,
    leafC       = 0,
    rootC       = 0.0203048475086689,
    sapwoodC    = 0.0121231861412525,
    heartwoodC  = 0.0217287018895149,
    NSN         = 0.053502481430769,
    treeG       = 0.0721193701028824,
    fseed       = 0,
    fleaf       = 0.326300173997879,
    froot       = 0.323846757411957,
    fwood       = 0.349853128194809,
    NPP         = 0.00910497829318047,
    GPP         = 0.0533972829580307,
    Rauto       = 0.0442923046648502,
    N_uptk      = 0,
    N_fxed      = 0,
    deathrate   = 0.0105336084961891,
    n_deadtrees = 4.29384454037063e-06,
    c_deadtrees = 8.35835744510405e-05)
  ref_BiomeE_gsLeun_oac_yr2 <- tibble(
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
    NSC         = 0.04190494120121,
    seedC       = 0.00393445231020451,
    leafC       = 0,
    rootC       = 0.0293838884681463,
    sapwoodC    = 0.0175439082086086,
    heartwoodC  = 0.0333848409354687,
    NSN         = 0.0576952509582043,
    treeG       = 0.0531104505062103,
    fseed       = 0,
    fleaf       = 0.266610950231552,
    froot       = 0.434975028038025,
    fwood       = 0.298414021730423,
    NPP         = 0.0414913110435009,
    GPP         = 0.0807787701487541,
    Rauto       = 0.0392874591052532,
    N_uptk      = 0,
    N_fxed      = 0,
    deathrate   = 0.0106044635176659,
    n_deadtrees = 4.58221984445117e-06,
    c_deadtrees = 9.25029235077091e-05)
  ref_BiomeE_gsLeun_oac_yr251 <- tibble(
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
    NSC         = c(8.69034671783447,8.61025333404541,0.0604438371956348,0.0354319773614407,0.575447380542755,0.0922481343150139,0.0817672535777092,0.0449579730629921,0.0245664119720459,0.0240462925285101),
    seedC       = c(0.0670315995812416,0.049742478877306,0.00295112188905478,0.00167514057829976,0.00878775492310524,0.000907446723431349,0.000372642709407955,0.000859255436807871,0.00182746548671275,0.00218956894241273),
    leafC       = c(0.223763048648834,0.25884672999382,0.0410976782441139,0.0308676771819592,0.0225305054336786,1.07380110137001e-07,0,0,0,0),
    rootC       = c(6.90478277206421,4.89936828613281,0.885525107383728,0.509999752044678,0.301324188709259,0.0426104664802551,0.0285878125578165,0.0167893897742033,0.00926230195909739,0.00601820880547166),
    sapwoodC    = c(4.12256097793579,2.9248960018158,0.525796949863434,0.302982866764069,0.179908245801926,0.0254409518092871,0.00877813901752234,0.00515532959252596,0.00284407124854624,0.00184794387314469),
    heartwoodC  = c(107.882415771484,63.7514839172363,6.24858713150024,3.11740040779114,1.31882703304291,0.080956369638443,0.0439035557210445,0.019412849098444,0.00779884541407228,0.00402712449431419),
    NSN         = c(74.3384246826172,43.931224822998,4.30555295944214,2.14803624153137,3.01330709457397,0.303967475891113,0.164870828390121,0.0729061663150787,0.0292928256094456,0.0151327569037676),
    treeG       = c(8.72370147705078,7.53424739837646,1.44603288173676,0.84990930557251,0.475799292325974,0.0715661197900772,0.044805996119976,0.0255186334252357,0.014680340886116,0.0165860280394554),
    fseed       = c(0.0256500113755465,0.0343560166656971,0.0284209847450256,0.0363187901675701,0.0473529621958733,1.50043217672646e-06,0,0,0,0),
    fleaf       = c(0.168099790811539,0.143239006400108,0.14520089328289,0.141369387507439,0.11870726197958,0.149757951498032,0.178606674075127,0.195692792534828,0.212796613574028,0.425208300352097),
    froot       = c(0.567995131015778,0.467971682548523,0.415671706199646,0.407196164131165,0.441437244415283,0.433115601539612,0.241038024425507,0.249938756227493,0.244935810565948,0.0225267615169287),
    fwood       = c(0.238255083560944,0.35443326830864,0.410706400871277,0.415115714073181,0.39250248670578,0.417124897241592,0.580355286598206,0.554368436336517,0.542267560958862,0.552264928817749),
    NPP         = c(13.0554456710815,8.54744911193848,1.42188680171967,0.828814446926117,0.52199912071228,0.0767327323555946,0.0523517355322838,0.0306746046990156,0.0161071941256523,0.00563223566859961),
    GPP         = c(20.9848556518555,14.8277940750122,2.54994654655457,1.48335671424866,0.902374625205994,0.133908092975616,0.0883583128452301,0.0513165555894375,0.0276933051645756,0.0165275614708662),
    Rauto       = c(7.92940998077393,6.28034543991089,1.1280597448349,0.65454226732254,0.380375474691391,0.0571753606200218,0.0360065773129463,0.0206419508904219,0.0115861101076007,0.0108953258022666),
    N_uptk      = c(0.180093288421631,0.127281174063683,0.0211675390601158,0.0121992873027921,0.00778928166255355,0.00108841969631612,0.000369834044249728,0.000178067100932822,0,0),
    N_fxed      = c(0,0,0,0,0,0,0,0,0,0),
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
    CAI             = c(0.00416251085698605,0.00511125242337584,0.013907034881413,0.0160003751516342,0.0483339205384254,1.3758339881897),
    LAI             = c(0.00621013063937426,0.0097409076988697,0.0437882207334042,0.0503804795444012,0.147045791149139,3.79896903038025),
    Density         = c(500,494.722534179688,462.518615722656,456.966278076172,2009.96313476562,46633.41015625),
    DBH             = c(0.675350785255432,0.7799192070961,1.58980083465576,1.75969135761261,1.17825710773468,1.076789021492),
    Density12       = c(0,0,0,0,0,202.662551879883),
    DBH12           = c(0,0,0,0,0,21.0354099273682),
    QMD12           = c(0,0,0,0,0,21.3040714263916),
    NPP             = c(0.00233111949637532,0.00477784592658281,0.0183418728411198,0.0211064927279949,0.0621540136635303,1.44544982910156),
    GPP             = c(0.00443709455430508,0.00688400026410818,0.0265300050377846,0.0306448806077242,0.0909370854496956,2.21131491661072),
    Rauto           = c(0.00210597505792975,0.00210615363903344,0.00818813219666481,0.00953838787972927,0.0287830736488104,0.765864908695221),
    Rh              = c(0.00289539876393974,0.0024883677251637,0.00354194291867316,0.00411876430734992,0.0121839363127947,1.34333622455597),
    Prcp            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903930664062,799.903991699219,799.904724121094,799.904846191406,799.90673828125,799.93017578125),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.691955566406,159.532897949219,157.880935668945,157.567352294922,153.254913330078,94.1740112304688),
    Runoff          = c(427.933685302734,427.996307373047,429.648162841797,429.962097167969,434.274169921875,493.355407714844),
    plantC          = c(0.00992458406835794,0.0133715905249119,0.0612290874123573,0.0742909759283066,0.251720786094666,11.7746019363403),
    soilC           = c(0.0111688086763024,0.0100112631917,0.0206278469413519,0.0245536770671606,0.0806600749492645,75.1439971923828),
    totC            = c(NaN,NaN,NaN,NaN,NaN,NaN),
    plantN          = c(0.000283172208582982,0.000251664518145844,0.00108211406040937,0.00127839797642082,0.0037057469598949,0.113139607012272),
    soilN           = c(0.00390609540045261,0.00347659992985427,0.00350461690686643,0.00360831106081605,0.00514484057202935,0.807641208171844),
    totN            = c(0.00418926775455475,0.00372826447710395,0.00458673108369112,0.0048867086879909,0.00885058753192425,0.920780837535858),
    NSC             = c(0.00424329005181789,0.00522226467728615,0.0213056188076735,0.0252688508480787,0.0783128142356873,2.24216532707214),
    seedC           = c(0,0,0.00155770848505199,0.00230354908853769,0.00411435309797525,0.0713211074471474),
    leafC           = c(0.00105572224128991,0.00165595440194011,0.00744399847462773,0.00856468174606562,0.0249977856874466,0.645824730396271),
    rootC           = c(0.000630328198894858,0.000988702056929469,0.00444450415670872,0.00511361798271537,0.0149251474067569,0.385595291852951),
    sapwoodC        = c(0.00115369341801852,0.00201713317073882,0.0156696904450655,0.0195541903376579,0.0758503675460815,4.79197692871094),
    heartwoodC      = c(0.0028415487613529,0.00348753668367863,0.0108075644820929,0.0134860891848803,0.0535203255712986,3.63771796226501),
    NSN             = c(0.00023783439246472,0.000182727177161723,0.000689385808072984,0.00079361570533365,0.0023271597456187,0.0647367835044861),
    seedN           = c(0,0,7.78854373493232e-05,0.000115177543193568,0.000205717631615698,0.0035660567227751),
    leafN           = c(1.81646610144526e-05,2.84922152786748e-05,0.000128081010188907,0.000147363491123542,0.000430111103923991,0.0111120585352182),
    rootN           = c(1.57581798703177e-05,2.47175175900338e-05,0.000111112429294735,0.000127840336062945,0.000373128161299974,0.00963986292481422),
    sapwoodN        = c(3.2962670957204e-06,5.7632378229755e-06,4.47705460828729e-05,5.58691062906291e-05,0.000216715343412943,0.0136913638561964),
    heartwoodN      = c(8.11871268524555e-06,9.96438848233083e-06,3.0878814868629e-05,3.85317507607397e-05,0.000152915032231249,0.010393476113677),
    mcrbC           = c(0.000287110568024218,0.000403931946493685,0.000674939888995141,0.000777141249272972,0.002258216496557,0.164414122700691),
    fastSOM         = c(0.00974638666957617,0.00818997155874968,0.0128241498023272,0.0148894870653749,0.0437433496117592,2.05150580406189),
    slowSOM         = c(0.00113531190436333,0.00141735922079533,0.00712875789031386,0.00888704787939787,0.0346585102379322,72.9280776977539),
    mcrbN           = c(2.87110542558366e-05,4.03931953769643e-05,6.74939874443226e-05,7.77141249272972e-05,0.000225821640924551,0.0164414122700691),
    fastSoilN       = c(0.000636350305285305,0.00053652172209695,0.000584115798119456,0.0006673569441773,0.00191482773516327,0.108197599649429),
    slowSoilN       = c(2.69904885499272e-05,3.06092515529599e-05,0.000104368504253216,0.000126537634059787,0.000434708286775276,0.680824756622314),
    mineralN        = c(0.00321404356509447,0.00286907562986016,0.00274863862432539,0.00273670232854784,0.00256948289461434,0.00217742240056396),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000331897346768528,0.000379295815946534,0.00106235279235989,0.0224656797945499),
    N_yrMin         = c(-0.0117859477177262,-0.000344969768775627,0.000320882681990042,0.000367360014934093,0.001026670797728,0.0225536432117224),
    N_P2S           = c(2.0049003069289e-05,3.12088232021779e-05,0.0001601717522135,0.00018579009338282,0.000548938405700028,0.0224444735795259),
    N_loss          = c(0.0219198521226645,0.0104609970003366,0.00974265113472939,0.00970002729445696,0.0091446116566658,0.007394899148494),
    totseedC        = c(0,0,0,0.00227513629943132,0.00364530785009265,0.0654599517583847),
    totseedN        = c(0,0,0,0.000113756905193441,0.000182265343028121,0.00327299884520471),
    Seedling_C      = c(0,0,0,0.00227513629943132,0.00364530785009265,0.0654599517583847),
    Seedling_N      = c(0,0,0,0.000113756905193441,0.000182265343028121,0.00327299884520471),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,79.7137908935547),
    MaxVolume       = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,79.7137908935547),
    MaxDBH          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,79.7137908935547),
    NPPL            = c(0.00121943862177432,0.000885054818354547,0.00251281796954572,0.00284165749326348,0.00835549365729094,0.171732634305954),
    NPPW            = c(0.00149524281732738,0.00155159807763994,0.00575843080878258,0.00688086031004786,0.0218216553330421,0.658564746379852),
    n_deadtrees     = c(2.98888289762544e-06,2.68998428509803e-06,1.29902773551294e-05,1.57681315613445e-05,5.37143241672311e-05,0.0090621979907155),
    c_deadtrees     = c(0.000104753984487616,0.000142925855470821,0.0007350267842412,0.00091632641851902,0.00373911182396114,0.889440596103668),
    m_turnover      = c(0.000104753984487616,0.000142925855470821,0.0007350267842412,0.00091632641851902,0.00373911182396114,0.889440596103668),
    c_turnover_time = c(1.9003928899765,2.24770641326904,1.87682485580444,1.95994234085083,2.45262455940247,5.52370643615723),
    lu_fraction     = c(1,1,1,1,1,1),
    prod_pool_1_C   = c(0.000375000003259629,0.000227448996156454,1.13240193968522e-05,6.86836483509978e-06,2.07406671393073e-07,1.40129846432482e-45),
    prod_pool_1_N   = c(1.07142852812103e-06,6.4985425751729e-07,3.23543396518744e-08,1.96238989502717e-08,5.92590532200177e-10,1.40129846432482e-45),
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
    SoilWater       = c(799.944641113281,799.947204589844,799.131042480469,799.930847167969,799.903991699219),
    Transp          = c(0,0,0,0,0),
    Evap            = c(0.0553628616034985,0.0527696460485458,0.868921220302582,0.0691292807459831,0.0960086733102798),
    Runoff          = c(1.43654549121857,1.94845604896545,1.08043730258942,1.96995043754578,1.75696134567261),
    ws1             = c(19.944637298584,19.9472312927246,19.1310787200928,19.930871963501,19.9039936065674),
    ws2             = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3             = c(600,600,600,600,600),
    LAI             = c(0,0.000364852574421093,0.00466133235022426,0.00618919776752591,0.00619784602895379),
    NPP             = c(0,0,0,0,0),
    GPP             = c(0,0,1.90892133105081e-05,6.17546766079613e-06,6.16563283983851e-06),
    Rauto           = c(2.97977797991678e-09,5.82730172027368e-05,4.93635025122785e-06,4.55608324045897e-06,4.57825399280409e-06),
    Rh              = c(2.77344884125341e-06,2.75237016467145e-06,1.03804013633635e-05,2.3680029244133e-06,2.36252321883512e-06),
    NSC             = c(0.00582691049203277,0.00565209938213229,0.00398708041757345,0.00424133753404021,0.00423404527828097),
    seedC           = c(0,0,0,0,0),
    leafC           = c(0,6.20249411440454e-05,0.000792426522821188,0.00105216365773231,0.00105363386683166),
    rootC           = c(0,3.70325360563584e-05,0.000473125197459012,0.000628203561063856,0.000629081332590431),
    sapwoodC        = c(0.00250000017695129,0.00163697078824043,0.000808653887361288,0.00114870828110725,0.00115068175364286),
    heartwoodC      = c(0,0.000880510022398084,0.00244240881875157,0.002835190622136,0.00283709052018821),
    NSN             = c(0.000293089426122606,0.000291046482743695,0.00025848179939203,0.000238109059864655,0.000237988890148699),
    seedN           = c(0,0,0,0,0),
    leafN           = c(0,1.06719380710274e-06,1.36344051497872e-05,1.81034265551716e-05,1.81287232408067e-05),
    rootN           = c(0,9.25813424146327e-07,1.18281122922781e-05,1.57050762936706e-05,1.57270224008244e-05),
    sapwoodN        = c(7.14285715730512e-06,4.67705922346795e-06,2.31044009524339e-06,3.28202372656961e-06,3.28766213897325e-06),
    heartwoodN      = c(0,2.5157430627587e-06,6.97831228535506e-06,8.10054189059883e-06,8.10597066447372e-06),
    mcrbC           = c(3.69218014384387e-07,7.35389221517835e-07,9.97748793452047e-05,0.000234167644521222,0.000234328705118969),
    fastSOM         = c(0.009996865876019,0.00999375618994236,0.00931816641241312,0.00799902994185686,0.00799865275621414),
    slowSOM         = c(0.000999990850687027,0.000999981770291924,0.00105635263025761,0.00113423995207995,0.00113475287798792),
    mcrbN           = c(3.6921800727896e-08,7.3538920730698e-08,9.97748793452047e-06,2.34167637245264e-05,2.34328708756948e-05),
    fastSoilN       = c(0.000666457752231508,0.000666250416543335,0.000621211074758321,0.000531849800609052,0.000531776517163962),
    slowSoilN       = c(2.49997719947714e-05,2.4999544621096e-05,2.58212767221266e-05,2.69752108579269e-05,2.69825650320854e-05),
    mineralN        = c(0.0149249155074358,0.0148505438119173,0.00587099697440863,0.00320566212758422,0.00321121863089502),
    N_uptk          = c(0,0,0,0,0))
  ref_BiomeE_PLULUC_primary_odt_yr251 <- tibble(
    year            = c(251,251,251,251,251),
    doy             = c(1,2,180,364,365),
    Tk              = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp            = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    SoilWater       = c(799.960083007812,799.958984375,799.516296386719,799.951416015625,799.930053710938),
    Transp          = c(0,0,0,0,0),
    Evap            = c(0.0399133116006851,0.0409998670220375,0.483673214912415,0.0485905706882477,0.0699304714798927),
    Runoff          = c(1.36660242080688,1.96390557289124,1.74325275421143,1.99354791641235,1.77749955654144),
    ws1             = c(19.9600887298584,19.9590015411377,19.5163269042969,19.9514083862305,19.9300708770752),
    ws2             = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3             = c(600,600,600,600,600),
    LAI             = c(3.4922297000885,3.49872899055481,3.61742806434631,3.74305200576782,3.74376797676086),
    NPP             = c(0,0,0,0,0),
    GPP             = c(0.00227762758731842,0.00224546459503472,0.00936589390039444,0.00246698618866503,0.00246065971441567),
    Rauto           = c(6.12458534305915e-05,0.00236861989833415,0.0024318634532392,0.00197463692165911,0.00198430172167718),
    Rh              = c(0.00146035756915808,0.00145140045788139,0.00574139412492514,0.00141523755155504,0.001413878868334),
    NSC             = c(2.14304780960083,2.13828492164612,2.1303768157959,2.2305862903595,2.22722339630127),
    seedC           = c(0.0019968063570559,0.0021789469756186,0.0329009033739567,0.0699842870235443,0.070181593298912),
    leafC           = c(0.593679130077362,0.594783842563629,0.614962816238403,0.636318802833557,0.636440634727478),
    rootC           = c(0.360928446054459,0.36085844039917,0.365907251834869,0.379917114973068,0.379989683628082),
    sapwoodC        = c(4.5676908493042,4.55334377288818,4.69937181472778,4.90116739273071,4.90223789215088),
    heartwoodC      = c(3.35169792175293,3.36794519424438,3.51899361610413,3.66643118858337,3.66720104217529),
    NSN             = c(0.0614702217280865,0.0614091902971268,0.0626659020781517,0.0639448463916779,0.0638884156942368),
    seedN           = c(9.98403047560714e-05,0.000108947337139398,0.00164504500571638,0.00349921477027237,0.00350908027030528),
    leafN           = c(0.0102148456498981,0.0102338567376137,0.0105810575187206,0.0109485015273094,0.0109505970031023),
    rootN           = c(0.00902319885790348,0.00902144890278578,0.00914766546338797,0.00949791260063648,0.00949972867965698),
    sapwoodN        = c(0.0130505440756679,0.0130095547065139,0.0134267760440707,0.0140033354982734,0.0140063930302858),
    heartwoodN      = c(0.00957628153264523,0.00962270423769951,0.0100542698055506,0.0104755172505975,0.0104777161031961),
    mcrbC           = c(0.163020133972168,0.163022756576538,0.164115786552429,0.163002550601959,0.162999197840691),
    fastSOM         = c(2.19501852989197,2.19556164741516,2.2025363445282,2.03177905082703,2.03243160247803),
    slowSOM         = c(72.7654800415039,72.765007019043,72.5841827392578,72.1644821166992,72.1640319824219),
    mcrbN           = c(0.0163020137697458,0.0163022764027119,0.0164115782827139,0.016300255432725,0.0162999201565981),
    fastSoilN       = c(0.113785140216351,0.113796405494213,0.113353185355663,0.107105150818825,0.107118561863899),
    slowSoilN       = c(0.675333976745605,0.675336599349976,0.675630569458008,0.675470352172852,0.675473272800446),
    mineralN        = c(0.00221035978756845,0.00223766081035137,0.00152802339289337,0.00226603355258703,0.00229924730956554),
    N_uptk          = c(0.000141792901558802,6.19682805336197e-06,0,0,0))
  ref_BiomeE_PLULUC_primary_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00416251085698605,0.00511125242337584,0.013907034881413,0.0160003751516342,0.0483339205384254,1.37583410739899),
    LAI             = c(0.00621013063937426,0.0097409076988697,0.0437882207334042,0.0503804795444012,0.147045791149139,3.79896926879883),
    Density         = c(500,494.722503662109,462.518585205078,456.966278076172,2009.96313476562,46633.4140625),
    DBH             = c(0.675350725650787,0.7799192070961,1.58980083465576,1.75969135761261,1.17825710773468,1.076789021492),
    Density12       = c(0,0,0,0,0,202.66227722168),
    DBH12           = c(0,0,0,0,0,21.0354156494141),
    QMD12           = c(0,0,0,0,0,21.3040790557861),
    NPP             = c(0.00233111949637532,0.00477784592658281,0.0183418728411198,0.0211064927279949,0.0621540136635303,1.44544994831085),
    GPP             = c(0.00443709455430508,0.00688399979844689,0.0265300050377846,0.0306448806077242,0.0909370854496956,2.21131491661072),
    Rauto           = c(0.00210597505792975,0.00210615363903344,0.00818813219666481,0.00953838787972927,0.0287830717861652,0.765864968299866),
    Rh              = c(0.00236287247389555,0.00206780130974948,0.00345703633502126,0.00405465206131339,0.0121752610430121,1.34333610534668),
    Prcp            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903930664062,799.903991699219,799.904724121094,799.904846191406,799.90673828125,799.930114746094),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.691955566406,159.532897949219,157.880935668945,157.567352294922,153.254913330078,94.1740112304688),
    Runoff          = c(427.933654785156,427.996276855469,429.648162841797,429.962066650391,434.274169921875,493.355407714844),
    plantC          = c(0.00992458313703537,0.0133715905249119,0.0612290874123573,0.0742909759283066,0.251720786094666,11.774600982666),
    soilC           = c(0.00937056448310614,0.0086335800588131,0.0203695576637983,0.0243595018982887,0.080634132027626,75.1439895629883),
    totC            = c(0.0192951485514641,0.022005170583725,0.0815986469388008,0.0986504778265953,0.33235490322113,86.9185943603516),
    plantN          = c(0.000283172208582982,0.000251664518145844,0.00108302047010511,0.00127931591123343,0.00370623869821429,0.113182485103607),
    soilN           = c(0.00379347801208496,0.00337964971549809,0.00347966281697154,0.00358892441727221,0.00514161959290504,0.807599067687988),
    totN            = c(0.0040766503661871,0.00363131426274776,0.00456268340349197,0.00486824009567499,0.00884785875678062,0.920781552791595),
    NSC             = c(0.00424329005181789,0.00522226467728615,0.0213056188076735,0.0252688508480787,0.0783128142356873,2.24216532707214),
    seedC           = c(0,0,0.00155770848505199,0.00230354908853769,0.00411435309797525,0.0713211074471474),
    leafC           = c(0.00105572224128991,0.00165595440194011,0.00744399800896645,0.00856468174606562,0.0249977856874466,0.645824790000916),
    rootC           = c(0.000630328198894858,0.000988702056929469,0.00444450415670872,0.00511361798271537,0.0149251474067569,0.385595291852951),
    sapwoodC        = c(0.00115369341801852,0.00201713317073882,0.0156696904450655,0.0195541884750128,0.0758503675460815,4.79197645187378),
    heartwoodC      = c(0.0028415487613529,0.00348753668367863,0.0108075644820929,0.0134860882535577,0.0535203255712986,3.63771796226501),
    NSN             = c(0.00023783439246472,0.000182727162609808,0.000690292159561068,0.000794533698353916,0.0023276514839381,0.064779669046402),
    seedN           = c(0,0,7.78854373493232e-05,0.000115177543193568,0.000205717631615698,0.00356605648994446),
    leafN           = c(1.81646610144526e-05,2.84922152786748e-05,0.000128081010188907,0.000147363491123542,0.000430111074820161,0.0111120594665408),
    rootN           = c(1.57581798703177e-05,2.47175175900338e-05,0.000111112429294735,0.000127840336062945,0.000373128161299974,0.00963986292481422),
    sapwoodN        = c(3.2962670957204e-06,5.7632378229755e-06,4.47705460828729e-05,5.58691062906291e-05,0.000216715328861028,0.0136913610622287),
    heartwoodN      = c(8.11871268524555e-06,9.96438848233083e-06,3.0878814868629e-05,3.85317507607397e-05,0.000152915032231249,0.0103934742510319),
    mcrbC           = c(0.000234329476370476,0.000333390984451398,0.000647217035293579,0.00075562996789813,0.00225507956929505,0.164414137601852),
    fastSOM         = c(0.00800092332065105,0.00688283005729318,0.0125935832038522,0.0147168226540089,0.0437205471098423,2.05150580406189),
    slowSOM         = c(0.00113531190436333,0.00141735922079533,0.00712875789031386,0.00888704787939787,0.0346585102379322,72.9280700683594),
    mcrbN           = c(2.34329472732497e-05,3.33390999003313e-05,6.47217020741664e-05,7.55629953346215e-05,0.000225507959839888,0.0164414141327143),
    fastSoilN       = c(0.000531837635207921,0.000451616855571046,0.000564671528991312,0.000652465911116451,0.00191263970918953,0.108197376132011),
    slowSoilN       = c(2.69904885499272e-05,3.06092515529599e-05,0.000104368504253216,0.000126537634059787,0.000434708286775276,0.680824875831604),
    mineralN        = c(0.00321121700108051,0.00286408443935215,0.00274590100161731,0.00273435795679688,0.00256876368075609,0.00213536969386041),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.00033195482683368,0.000379318167688325,0.00106669578235596,0.0225101206451654),
    N_yrMin         = c(-0.0117887780070305,-0.00034713459899649,0.000321342697134241,0.000367776519851759,0.00102654949296266,0.0225571487098932),
    N_P2S           = c(2.0049003069289e-05,3.12088232021779e-05,0.000160182637046091,0.000185801400220953,0.000548946671187878,0.0224458295851946),
    N_loss          = c(0.0219152607023716,0.0104453265666962,0.00973560195416212,0.00969444587826729,0.00914391409605742,0.00739127490669489),
    totseedC        = c(0,0,0,0.00227513629943132,0.00364530761726201,0.0654599517583847),
    totseedN        = c(0,0,0,0.000113756905193441,0.000182265343028121,0.00327299861237407),
    Seedling_C      = c(0,0,0,0.00227513629943132,0.00364530761726201,0.0654599517583847),
    Seedling_N      = c(0,0,0,0.000113756905193441,0.000182265343028121,0.00327299861237407),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,79.7137832641602),
    MaxVolume       = c(0.000215959051274695,0.000300723884720355,0.00154718419071287,0.00195414945483208,0.00751346396282315,0.725371241569519),
    MaxDBH          = c(0.00675350753590465,0.00779919186607003,0.0158980078995228,0.0175969135016203,0.0316033586859703,0.230491578578949),
    NPPL            = c(0.00121943862177432,0.000885054818354547,0.00251281773671508,0.00284165726043284,0.00835549365729094,0.171732649207115),
    NPPW            = c(0.00149524281732738,0.00155159796122462,0.00575843034312129,0.00688086031004786,0.0218216553330421,0.658564686775208),
    n_deadtrees     = c(2.98888289762544e-06,2.68998428509803e-06,1.30011576402467e-05,1.57794547703816e-05,5.37226296728477e-05,0.009063552133739),
    c_deadtrees     = c(0.000104753984487616,0.000142925855470821,0.0007350267842412,0.00091632641851902,0.0037391115911305,0.889440536499023),
    m_turnover      = c(0.000104753984487616,0.000142925855470821,0.0007350267842412,0.00091632641851902,0.0037391115911305,0.889440536499023),
    c_turnover_time = c(1.9003928899765,2.24770641326904,1.87682473659515,1.95994222164154,2.45262455940247,5.52370643615723),
    lu_fraction     = c(0.600000023841858,0.600000023841858,0.600000023841858,0.600000023841858,0.600000023841858,0.600000023841858))
  ref_BiomeE_PLULUC_primary_oac_yr1 <- tibble(
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
    NSC             = 0.0845033749938011,
    seedC           = 0.00475737359374762,
    leafC           = 0,
    rootC           = 0.0211020689457655,
    sapwoodC        = 0.0125991748645902,
    heartwoodC      = 0.0230530891567469,
    NSN             = 0.0567796900868416,
    treeG           = 0.0781993493437767,
    fseed           = 0,
    fleaf           = 0.311710923910141,
    froot           = 0.306792557239532,
    fwood           = 0.381496518850327,
    NPP             = 0.0461645908653736,
    GPP             = 0.0887204110622406,
    Rauto           = 0.042555820196867,
    N_uptk          = 0,
    N_fxed          = 0,
    deathrate       = 0.0105546750128269,
    n_deadtrees     = 4.3513900891412e-06,
    c_deadtrees     = 0.00010314847168047)
  ref_BiomeE_PLULUC_primary_oac_yr2 <- tibble(
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
    NSC             = 0.104750856757164,
    seedC           = 0.00369688123464584,
    leafC           = 0,
    rootC           = 0.033416286110878,
    sapwoodC        = 0.0199514869600534,
    heartwoodC      = 0.0406682156026363,
    NSN             = 0.0703133046627045,
    treeG           = 0.0755703151226044,
    fseed           = 0,
    fleaf           = 0.236070990562439,
    froot           = 0.35174748301506,
    fwood           = 0.412181466817856,
    NPP             = 0.0958177149295807,
    GPP             = 0.138989970088005,
    Rauto           = 0.0431722514331341,
    N_uptk          = 0,
    N_fxed          = 0,
    deathrate       = 0.0106876138597727,
    n_deadtrees     = 4.85187956655864e-06,
    c_deadtrees     = 0.0001401223562425)
  ref_BiomeE_PLULUC_primary_oac_yr251 <- tibble(
    year        = c(251,251,251,251,251),
    cID         = c(453,397,405,451,25),
    PFT         = c(2,2,2,2,2),
    layer       = c(1,1,1,2,2),
    density     = c(6204.306640625,149.401016235352,53.2612571716309,1829.57446289062,38396.87109375),
    flayer      = c(0.781054198741913,0.247986137866974,0.0482195615768433,0.169435322284698,0.129138872027397),
    DBH         = c(4.12981939315796,23.0491580963135,15.3867559432983,3.36543917655945,0.369073837995529),
    dDBH        = c(0.245740637183189,0.42443722486496,0.380046665668488,0.0657945871353149,0.0290652271360159),
    height      = c(7.31590461730957,17.2834339141846,14.1213445663452,6.604248046875,2.18705224990845),
    age         = c(25.7797470092773,79.7137832641602,79.7137832641602,25.7797470092773,2.81041598320007),
    BA          = c(0.00133952882606536,0.0417253524065018,0.018594479188323,0.000889556191395968,1.06983416117146e-05),
    dBA         = c(0.000154671724885702,0.00152254849672318,0.000907208770513535,3.44417640008032e-05,1.61867865244858e-06),
    Acrown      = c(1.25889039039612,16.5986919403076,9.0534029006958,0.926091492176056,0.033632654696703),
    Aleaf       = c(3.96451163291931,52.2818565368652,28.5152816772461,1.45851707458496,0.0363169871270657),
    NSC         = c(2.39638876914978,38.3595199584961,20.333080291748,0.283469200134277,0.00576031673699617),
    seedC       = c(0.0623313896358013,0.822567701339722,0.448743045330048,0.022894911468029,0.0018853775691241),
    leafC       = c(0.0752148330211639,1.24791276454926,0.658048391342163,0.0137011418119073,0),
    rootC       = c(0.673967003822327,8.88791561126709,4.8475980758667,0.24794790148735,0.00617388775572181),
    sapwoodC    = c(0.402397900819778,5.30660772323608,2.89430069923401,0.14803946018219,0.00368617381900549),
    heartwoodC  = c(3.04479670524597,158.889007568359,62.7212791442871,0.978022515773773,0.00418663211166859),
    NSN         = c(2.09915590286255,109.498344421387,43.2269439697266,2.23453903198242,0.0157208405435085),
    treeG       = c(1.43992221355438,20.9510726928711,11.251781463623,0.33076623082161,0.0105965211987495),
    fseed       = c(0.0522353462874889,0.0595631934702396,0.0584839284420013,0.0414224341511726,0),
    fleaf       = c(0.133100807666779,0.0977673605084419,0.102879054844379,0.0548723526299,0.31301686167717),
    froot       = c(0.344545662403107,0.306600600481033,0.31228169798851,0.472529411315918,0.363980025053024),
    fwood       = c(0.470118194818497,0.536068797111511,0.52635532617569,0.431175768375397,0.323003113269806),
    NPP         = c(1.67459845542908,22.0481491088867,12.040563583374,0.0610319674015045,0.000463664066046476),
    GPP         = c(2.48450422286987,33.8128623962402,18.3549690246582,0.236606135964394,0.00615543918684125),
    Rauto       = c(0.809905707836151,11.7647142410278,6.31440544128418,0.175574168562889,0.00569177512079477),
    N_uptk      = c(0.0268299877643585,0.313951283693314,0.175001218914986,0.00131960189901292,0),
    N_fxed      = c(0,0,0,0,0),
    deathrate   = c(0.0183926019817591,0.120657943189144,0.0703560188412666,0.155317112803459,0.351918429136276),
    n_deadtrees = c(0.00116903800517321,0.00349239259958267,0.000352286559063941,0.00115728343371302,0.00289255077950656),
    c_deadtrees = c(0.0991864204406738,0.580791890621185,0.0504684299230576,0.110986568033695,0.0480072423815727))
  ref_BiomeE_PLULUC_secondary_odt_yr1 <- tibble(
    year            = c(1,1,1,1,1),
    doy             = c(1,2,180,364,365),
    Tk              = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp            = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    SoilWater       = c(799.944641113281,799.947204589844,799.131042480469,799.930847167969,799.903991699219),
    Transp          = c(0,0,0,0,0),
    Evap            = c(0.0553628727793694,0.0527696460485458,0.868921220302582,0.0691292807459831,0.0960086733102798),
    Runoff          = c(1.43660509586334,1.94845604896545,1.08043730258942,1.96995043754578,1.75696134567261),
    ws1             = c(19.944637298584,19.9472312927246,19.1310787200928,19.930871963501,19.9039936065674),
    ws2             = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3             = c(600,600,600,600,600),
    LAI             = c(0,0.000364852574421093,0.00466133235022426,0.00618919776752591,0.00619784602895379),
    NPP             = c(0,0,0,0,0),
    GPP             = c(0,0,1.90892133105081e-05,6.17546766079613e-06,6.16563283983851e-06),
    Rauto           = c(2.97977797991678e-09,5.82730172027368e-05,4.93635025122785e-06,4.55608324045897e-06,4.57825399280409e-06),
    Rh              = c(4.38448614659137e-06,4.35115134678199e-06,1.62614360306179e-05,3.65712071470625e-06,3.64831771548779e-06),
    NSC             = c(0.00582691049203277,0.00565209938213229,0.00398708041757345,0.00424133753404021,0.00423404527828097),
    seedC           = c(0,0,0,0,0),
    leafC           = c(0,6.20249411440454e-05,0.000792426522821188,0.00105216365773231,0.00105363386683166),
    rootC           = c(0,3.70325360563584e-05,0.000473125197459012,0.000628203561063856,0.000629081332590431),
    sapwoodC        = c(0.00250000017695129,0.00163697078824043,0.000808653887361288,0.00114870828110725,0.00115068175364286),
    heartwoodC      = c(0,0.000880510022398084,0.00244240881875157,0.002835190622136,0.00283709052018821),
    NSN             = c(0.000293089426122606,0.000291046482743695,0.00025848179939203,0.000238109059864655,0.000237988890148699),
    seedN           = c(0,0,0,0,0),
    leafN           = c(0,1.06719380710274e-06,1.36344051497872e-05,1.81034265551716e-05,1.81287232408067e-05),
    rootN           = c(0,9.25813424146327e-07,1.18281122922781e-05,1.57050762936706e-05,1.57270224008244e-05),
    sapwoodN        = c(7.14285715730512e-06,4.67705922346795e-06,2.31044009524339e-06,3.28202372656961e-06,3.28766213897325e-06),
    heartwoodN      = c(0,2.5157430627587e-06,6.97831228535506e-06,8.10054189059883e-06,8.10597066447372e-06),
    mcrbC           = c(5.84022927796468e-07,1.16322576104722e-06,0.000156941576278768,0.00036603509215638,0.000366281310562044),
    fastSOM         = c(0.0158219542354345,0.0158170331269503,0.0146148949861526,0.0123640624806285,0.0123623143881559),
    slowSOM         = c(0.000999990850687027,0.000999981770291924,0.00105635263025761,0.00113423995207995,0.00113475287798792),
    mcrbN           = c(5.84022927796468e-08,1.16322574683636e-07,1.56941569002811e-05,3.6603509215638e-05,3.66281310562044e-05),
    fastSoilN       = c(0.000959546654485166,0.000959338853135705,0.000903414911590517,0.000793190556578338,0.000793060928117484),
    slowSoilN       = c(2.49997719947714e-05,2.4999544621096e-05,2.58212767221266e-05,2.69752108579269e-05,2.69825650320854e-05),
    mineralN        = c(0.0149248940870166,0.0148505019024014,0.00587352132424712,0.00321272760629654,0.00321828341111541),
    N_uptk          = c(0,0,0,0,0))
  ref_BiomeE_PLULUC_secondary_odt_yr251 <- tibble(
    year            = c(251,251,251,251,251),
    doy             = c(1,2,180,364,365),
    Tk              = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp            = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    SoilWater       = c(799.960083007812,799.958984375,799.516296386719,799.951416015625,799.930053710938),
    Transp          = c(0,0,0,0,0),
    Evap            = c(0.0399133078753948,0.0409998670220375,0.483673214912415,0.0485905706882477,0.0699304714798927),
    Runoff          = c(1.36660242080688,1.96390557289124,1.74325275421143,1.99354791641235,1.77749955654144),
    ws1             = c(19.9600887298584,19.9590015411377,19.5163269042969,19.9514083862305,19.9300708770752),
    ws2             = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3             = c(600,600,600,600,600),
    LAI             = c(3.49222993850708,3.49872970581055,3.61742806434631,3.74305152893066,3.74376773834229),
    NPP             = c(0,0,0,0,0),
    GPP             = c(0.00227762712165713,0.00224546436220407,0.00936589110642672,0.0024669854901731,0.00246065971441567),
    Rauto           = c(6.12458461546339e-05,0.00236870953813195,0.00243187905289233,0.00197461084462702,0.00198432477191091),
    Rh              = c(0.00146035687066615,0.00145139975938946,0.00574139226227999,0.00141523708589375,0.00141387851908803),
    NSC             = c(2.1430447101593,2.13828134536743,2.13037395477295,2.23058462142944,2.22722125053406),
    seedC           = c(0.00199680984951556,0.00217895023524761,0.0329008512198925,0.0699842199683189,0.0701815262436867),
    leafC           = c(0.593679070472717,0.594784021377563,0.614962756633759,0.636318683624268,0.636440575122833),
    rootC           = c(0.360928416252136,0.360858500003815,0.365907222032547,0.379917025566101,0.379989624023438),
    sapwoodC        = c(4.56769037246704,4.55334424972534,4.69937038421631,4.9011664390564,4.90223550796509),
    heartwoodC      = c(3.35169625282288,3.36794352531433,3.51899266242981,3.66643023490906,3.66720032691956),
    NSN             = c(0.0614123828709126,0.0613451525568962,0.0628624483942986,0.0639507323503494,0.0640421807765961),
    seedN           = c(9.98404866550118e-05,0.000108947497210465,0.00164504267740995,0.00349921081215143,0.00350907607935369),
    leafN           = c(0.0102148428559303,0.0102338558062911,0.0105810631066561,0.0109485071152449,0.0109506025910378),
    rootN           = c(0.00902319420129061,0.00902144517749548,0.00914766080677509,0.00949790608137846,0.00949972216039896),
    sapwoodN        = c(0.0130505440756679,0.0130095537751913,0.0134267713874578,0.0140033317729831,0.0140063865110278),
    heartwoodN      = c(0.00957627687603235,0.00962269864976406,0.0100542642176151,0.0104755153879523,0.0104777151718736),
    mcrbC           = c(0.163020089268684,0.163022711873055,0.164115756750107,0.163002520799637,0.162999168038368),
    fastSOM         = c(2.19501686096191,2.19555997848511,2.2025351524353,2.03177809715271,2.03243064880371),
    slowSOM         = c(72.7654647827148,72.7649917602539,72.5841674804688,72.1644668579102,72.1640167236328),
    mcrbN           = c(0.0163020081818104,0.0163022708147764,0.0164115764200687,0.0163002517074347,0.0162999164313078),
    fastSoilN       = c(0.113782078027725,0.113793343305588,0.11335052549839,0.107103116810322,0.107116527855396),
    slowSoilN       = c(0.675333976745605,0.675336599349976,0.675630569458008,0.675470352172852,0.675473272800446),
    mineralN        = c(0.0022484662476927,0.00228170235641301,0.00132014625705779,0.0022728736512363,0.00215815543197095),
    N_uptk          = c(0,0,0.00045189680531621,7.82818387961015e-05,0.000147884173202328))
  ref_BiomeE_PLULUC_secondary_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00416251085698605,0.00511125242337584,0.013907034881413,0.0160003751516342,0.0483339205384254,1.37583374977112),
    LAI             = c(0.00621013063937426,0.0097409076988697,0.0437882207334042,0.0503804795444012,0.147045791149139,3.79896831512451),
    Density         = c(500,494.722503662109,462.518585205078,456.966278076172,2009.96313476562,46633.40625),
    DBH             = c(0.675350725650787,0.7799192070961,1.58980083465576,1.75969135761261,1.17825710773468,1.07678890228271),
    Density12       = c(0,0,0,0,0,202.662948608398),
    DBH12           = c(0,0,0,0,0,21.035400390625),
    QMD12           = c(0,0,0,0,0,21.3040580749512),
    NPP             = c(0.00233111949637532,0.00477784592658281,0.0183418728411198,0.0211064927279949,0.0621540136635303,1.44544959068298),
    GPP             = c(0.00443709455430508,0.00688399979844689,0.0265300050377846,0.0306448806077242,0.0909370854496956,2.21131443977356),
    Rauto           = c(0.00210597505792975,0.00210615363903344,0.00818813219666481,0.00953838787972927,0.0287830717861652,0.765864789485931),
    Rh              = c(0.0036941880825907,0.00311921746470034,0.00366930267773569,0.00421493267640471,0.0121969478204846,1.34333634376526),
    Prcp            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.903930664062,799.903991699219,799.904724121094,799.904846191406,799.90673828125,799.930114746094),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.691955566406,159.532897949219,157.880935668945,157.567352294922,153.254913330078,94.1740112304688),
    Runoff          = c(427.933715820312,427.996276855469,429.648162841797,429.962066650391,434.274169921875,493.355407714844),
    plantC          = c(0.00992458313703537,0.0133715905249119,0.0612290874123573,0.0742909759283066,0.251720786094666,11.7746019363403),
    soilC           = c(0.0138661740347743,0.0120777860283852,0.0210152808576822,0.024844940751791,0.0806989744305611,75.1440124511719),
    totC            = c(0.0237907581031322,0.025449376553297,0.0822443664073944,0.0991359204053879,0.332419753074646,86.9186172485352),
    plantN          = c(0.000283172208582982,0.000251664518145844,0.00108075456228107,0.00127702089957893,0.00370500888675451,0.113075278699398),
    soilN           = c(0.00407502101734281,0.00362202501855791,0.00354204792529345,0.00363739067688584,0.00514967180788517,0.807704389095306),
    totN            = c(0.00435819337144494,0.00387368956580758,0.00462280260398984,0.00491441134363413,0.00885468069463968,0.920779645442963),
    NSC             = c(0.00424329005181789,0.00522226467728615,0.0213056188076735,0.0252688508480787,0.0783128142356873,2.24216532707214),
    seedC           = c(0,0,0.00155770848505199,0.00230354908853769,0.00411435309797525,0.0713211074471474),
    leafC           = c(0.00105572224128991,0.00165595440194011,0.00744399800896645,0.00856468174606562,0.0249977856874466,0.645824611186981),
    rootC           = c(0.000630328198894858,0.000988702056929469,0.00444450415670872,0.00511361798271537,0.0149251474067569,0.385595262050629),
    sapwoodC        = c(0.00115369341801852,0.00201713317073882,0.0156696904450655,0.0195541884750128,0.0758503675460815,4.7919774055481),
    heartwoodC      = c(0.0028415487613529,0.00348753668367863,0.0108075644820929,0.0134860882535577,0.0535203255712986,3.63771820068359),
    NSN             = c(0.00023783439246472,0.000182727162609808,0.000688026251737028,0.00079223868669942,0.00232642167247832,0.0646724626421928),
    seedN           = c(0,0,7.78854373493232e-05,0.000115177543193568,0.000205717631615698,0.0035660567227751),
    leafN           = c(1.81646610144526e-05,2.84922152786748e-05,0.000128081010188907,0.000147363491123542,0.000430111074820161,0.0111120566725731),
    rootN           = c(1.57581798703177e-05,2.47175175900338e-05,0.000111112429294735,0.000127840336062945,0.000373128161299974,0.00963986199349165),
    sapwoodN        = c(3.2962670957204e-06,5.7632378229755e-06,4.47705460828729e-05,5.58691062906291e-05,0.000216715328861028,0.0136913657188416),
    heartwoodN      = c(8.11871268524555e-06,9.96438848233083e-06,3.0878814868629e-05,3.85317507607397e-05,0.000152915032231249,0.0103934779763222),
    mcrbC           = c(0.000366282154573128,0.00050974334590137,0.000716524082235992,0.000809408084023744,0.00226292153820395,0.164414092898369),
    fastSOM         = c(0.01236458029598,0.0101506831124425,0.0131699983030558,0.0151484841480851,0.0437775440514088,2.05150580406189),
    slowSOM         = c(0.00113531190436333,0.00141735922079533,0.00712875789031386,0.00888704787939787,0.0346585102379322,72.928092956543),
    mcrbN           = c(3.6628214729717e-05,5.09743331349455e-05,7.16524082235992e-05,8.09408084023744e-05,0.000226292147999629,0.0164414085447788),
    fastSoilN       = c(0.000793119193986058,0.000663878920022398,0.000613282201811671,0.000689693435560912,0.00191810971591622,0.108197934925556),
    slowSoilN       = c(2.69904885499272e-05,3.06092515529599e-05,0.000104368504253216,0.000126537634059787,0.000434708286775276,0.68082457780838),
    mineralN        = c(0.00321828317828476,0.0028765625320375,0.00275274482555687,0.00274021876975894,0.00257056136615574,0.00224050111137331),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(0,0,0.000331811083015054,0.000379262259230018,0.00105583830736578,0.0223990194499493),
    N_yrMin         = c(-0.0117817027494311,-0.000341722508892417,0.000320192659273744,0.000366735243005678,0.0010268526384607,0.0225483849644661),
    N_P2S           = c(2.0049003069289e-05,3.12088232021779e-05,0.000160155424964614,0.000185773096745834,0.000548925891052932,0.022442439571023),
    N_loss          = c(0.021926736459136,0.0104845026507974,0.00975322537124157,0.00970839895308018,0.00914565660059452,0.00740033481270075),
    totseedC        = c(0,0,0,0.00227513629943132,0.00364530761726201,0.0654599517583847),
    totseedN        = c(0,0,0,0.000113756905193441,0.000182265343028121,0.00327299861237407),
    Seedling_C      = c(0,0,0,0.00227513629943132,0.00364530761726201,0.0654599517583847),
    Seedling_N      = c(0,0,0,0.000113756905193441,0.000182265343028121,0.00327299861237407),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,79.7137908935547),
    MaxVolume       = c(0.000215959051274695,0.000300723884720355,0.00154718419071287,0.00195414945483208,0.00751346396282315,0.725371241569519),
    MaxDBH          = c(0.00675350753590465,0.00779919186607003,0.0158980078995228,0.0175969135016203,0.0316033586859703,0.230491578578949),
    NPPL            = c(0.00121943862177432,0.000885054818354547,0.00251281773671508,0.00284165726043284,0.00835549365729094,0.17173258960247),
    NPPW            = c(0.00149524281732738,0.00155159796122462,0.00575843034312129,0.00688086031004786,0.0218216553330421,0.658564746379852),
    n_deadtrees     = c(2.98888289762544e-06,2.68998428509803e-06,1.29739555632113e-05,1.57511458382942e-05,5.37018568138592e-05,0.00906016677618027),
    c_deadtrees     = c(0.000104753984487616,0.000142925855470821,0.0007350267842412,0.00091632641851902,0.0037391115911305,0.889440655708313),
    m_turnover      = c(0.000104753984487616,0.000142925855470821,0.0007350267842412,0.00091632641851902,0.0037391115911305,0.889440655708313),
    c_turnover_time = c(1.9003928899765,2.24770641326904,1.87682473659515,1.95994222164154,2.45262455940247,5.52370643615723),
    lu_fraction     = c(0.400000005960464,0.400000005960464,0.400000005960464,0.400000005960464,0.400000005960464,0.400000005960464))
  ref_BiomeE_PLULUC_secondary_oac_yr1 <- tibble(
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
    NSC             = 0.0845033749938011,
    seedC           = 0.00475737359374762,
    leafC           = 0,
    rootC           = 0.0211020689457655,
    sapwoodC        = 0.0125991748645902,
    heartwoodC      = 0.0230530891567469,
    NSN             = 0.0567796900868416,
    treeG           = 0.0781993493437767,
    fseed           = 0,
    fleaf           = 0.311710923910141,
    froot           = 0.306792557239532,
    fwood           = 0.381496518850327,
    NPP             = 0.0461645908653736,
    GPP             = 0.0887204110622406,
    Rauto           = 0.042555820196867,
    N_uptk          = 0,
    N_fxed          = 0,
    deathrate       = 0.0105546750128269,
    n_deadtrees     = 4.3513900891412e-06,
    c_deadtrees     = 0.00010314847168047)
  ref_BiomeE_PLULUC_secondary_oac_yr2 <- tibble(
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
    NSC             = 0.104750856757164,
    seedC           = 0.00369688123464584,
    leafC           = 0,
    rootC           = 0.033416286110878,
    sapwoodC        = 0.0199514869600534,
    heartwoodC      = 0.0406682156026363,
    NSN             = 0.0703133046627045,
    treeG           = 0.0755703151226044,
    fseed           = 0,
    fleaf           = 0.236070990562439,
    froot           = 0.35174748301506,
    fwood           = 0.412181466817856,
    NPP             = 0.0958177149295807,
    GPP             = 0.138989970088005,
    Rauto           = 0.0431722514331341,
    N_uptk          = 0,
    N_fxed          = 0,
    deathrate       = 0.0106876138597727,
    n_deadtrees     = 4.85187956655864e-06,
    c_deadtrees     = 0.0001401223562425)
  ref_BiomeE_PLULUC_secondary_oac_yr251 <- tibble(
    year        = c(251,251,251,251,251),
    cID         = c(453,397,405,451,25),
    PFT         = c(2,2,2,2,2),
    layer       = c(1,1,1,2,2),
    density     = c(6204.30126953125,149.400802612305,53.2621536254883,1829.57348632812,38396.8671875),
    flayer      = c(0.781053483486176,0.247985795140266,0.0482205040752888,0.169435098767281,0.129138857126236),
    DBH         = c(4.12981939315796,23.0491580963135,15.3867835998535,3.36543726921082,0.369073837995529),
    dDBH        = c(0.245740637183189,0.42443722486496,0.380046665668488,0.0657934695482254,0.0290652271360159),
    height      = c(7.31590461730957,17.2834339141846,14.1213569641113,6.60424613952637,2.18705224990845),
    age         = c(25.7797470092773,79.7137908935547,79.7137908935547,25.7797470092773,2.81041669845581),
    BA          = c(0.00133952882606536,0.0417253524065018,0.0185945481061935,0.000889555201865733,1.06983416117146e-05),
    dBA         = c(0.000154671724885702,0.00152254849672318,0.000907210633158684,3.44411819241941e-05,1.61867865244858e-06),
    Acrown      = c(1.25889039039612,16.5986919403076,9.05342769622803,0.926090717315674,0.033632654696703),
    Aleaf       = c(3.96451163291931,52.2818565368652,28.5153579711914,1.45851588249207,0.0363169871270657),
    NSC         = c(2.39638876914978,38.3595199584961,20.333137512207,0.2834692299366,0.00576031720265746),
    seedC       = c(0.0621957555413246,0.81993979215622,0.447905600070953,0.0230079535394907,0.00188537826761603),
    leafC       = c(0.0752148330211639,1.24791276454926,0.658050119876862,0.0137011585757136,0),
    rootC       = c(0.673967003822327,8.88791561126709,4.84761095046997,0.247947707772255,0.00617388775572181),
    sapwoodC    = c(0.402397900819778,5.30660772323608,2.89430856704712,0.148039355874062,0.00368617381900549),
    heartwoodC  = c(3.04479670524597,158.889007568359,62.7215423583984,0.978021562099457,0.00418663211166859),
    NSN         = c(2.09915590286255,109.498344421387,43.227123260498,2.23453593254089,0.0157208405435085),
    treeG       = c(1.43992221355438,20.9510726928711,11.2518119812012,0.330765783786774,0.010596520267427),
    fseed       = c(0.0522353462874889,0.0595631934702396,0.058483924716711,0.041422538459301,0),
    fleaf       = c(0.133100807666779,0.0977673605084419,0.102878838777542,0.0548719614744186,0.313016831874847),
    froot       = c(0.344545662403107,0.306600600481033,0.3122818171978,0.472529530525208,0.363980054855347),
    fwood       = c(0.470118194818497,0.536068797111511,0.526355385780334,0.431176036596298,0.323003143072128),
    NPP         = c(1.67459845542908,22.0481491088867,12.0405979156494,0.0610321760177612,0.000463665463030338),
    GPP         = c(2.48450422286987,33.8128623962402,18.3550243377686,0.23660609126091,0.00615544011816382),
    Rauto       = c(0.809905707836151,11.7647142410278,6.31442594528198,0.175573915243149,0.00569177465513349),
    N_uptk      = c(0.0267345085740089,0.309610784053802,0.17282697558403,0.00145390117540956,0),
    N_fxed      = c(0,0,0,0,0),
    deathrate   = c(0.0183926019817591,0.120657943189144,0.0703561827540398,0.155317172408104,0.351918429136276),
    n_deadtrees = c(0.00116748921573162,0.00348765077069402,0.00035197997931391,0.00116049533244222,0.00289255194365978),
    c_deadtrees = c(0.0991863310337067,0.580791115760803,0.0504695922136307,0.110986426472664,0.0480072423815727))
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


