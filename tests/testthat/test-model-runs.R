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
  df_drivers_BiomeE_gsLeun <- rsofun::biomee_gs_leuning_drivers
  # remove spinup that we can check initial conditions and transient phases
  df_drivers_BiomeE_Pmodel$params_siml[[1]]$spinupyears = 0
  df_drivers_BiomeE_gsLeun$params_siml[[1]]$spinupyears = 0
  df_drivers_BiomeE_Pmodel$params_siml[[1]]$nyeartrend = 251
  df_drivers_BiomeE_gsLeun$params_siml[[1]]$nyeartrend = 251
  df_drivers_BiomeE_Pmodel$forcing[[1]] <- df_drivers_BiomeE_Pmodel$forcing[[1]] |>
    # repeat forcing and update dates
    list() |> rep(251) |> bind_rows(.id = "repeatedyear") |> 
    mutate(date = date + lubridate::years(as.numeric(repeatedyear) - 1)) |> 
    select(-repeatedyear)
  df_drivers_BiomeE_gsLeun$forcing[[1]] <- df_drivers_BiomeE_gsLeun$forcing[[1]] |>
    # repeat forcing and update dates
    list() |> rep(251) |> bind_rows(.id = "repeatedyear") |> 
    mutate(date = date + lubridate::years(as.numeric(repeatedyear) - 1)) |> 
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
  expect_type(mod_BiomeE_Pmodel, "list")
  expect_s3_class(mod_BiomeE_Pmodel$data$output_daily_tile, "data.frame")
  expect_s3_class(mod_BiomeE_Pmodel$data$output_annual_tile, "data.frame")
  expect_s3_class(mod_BiomeE_Pmodel$data$output_annual_cohorts, "data.frame")
  
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel$data$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel$data$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun$data$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun$data$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel_2ndTry$data$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel_2ndTry$data$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel_2ndTry$data$output_annual_cohorts))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun_2ndTry$data$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun_2ndTry$data$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun_2ndTry$data$output_annual_cohorts))))

  # Testing memory leakage, i.e. repeatability
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_daily_tile    ), tibble(mod_BiomeE_Pmodel_2ndTry$data$output_daily_tile    ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_annual_tile   ), tibble(mod_BiomeE_Pmodel_2ndTry$data$output_annual_tile   ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts), tibble(mod_BiomeE_Pmodel_2ndTry$data$output_annual_cohorts), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_daily_tile    ), tibble(mod_BiomeE_gsLeun_2ndTry$data$output_daily_tile    ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_annual_tile   ), tibble(mod_BiomeE_gsLeun_2ndTry$data$output_annual_tile   ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts), tibble(mod_BiomeE_gsLeun_2ndTry$data$output_annual_cohorts), tolerance = 1e-6)
  
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
    Tc        = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp      = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    totWs     = c(799.944641113281,799.947204589844,799.132995605469,799.930969238281,799.904113769531),
    Trsp      = c(0,0,0,0,0),
    Evap      = c(0.0553628616034985,0.0527696460485458,0.866991937160492,0.0690219923853874,0.0958724543452263),
    Runoff    = c(1.43654549121857,1.94845604896545,1.08583152294159,1.97007417678833,1.7570686340332),
    ws1       = c(19.944637298584,19.9472312927246,19.1330089569092,19.9309787750244,19.9041271209717),
    ws2       = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3       = c(600,600,600,600,600),
    LAI       = c(0,0.000364852574421093,0.0114295054227114,0.0127560198307037,0.0127624524757266),
    GPP       = c(0,2.63141316736437e-07,3.22172854794189e-05,7.47269314160803e-06,8.22019865154289e-06),
    Rauto     = c(2.97977797991678e-09,5.82770990149584e-05,6.47079059490352e-06,5.27829615748487e-06,5.31174691786873e-06),
    Rh        = c(2.77344884125341e-06,2.75237016467145e-06,1.07433834273252e-05,2.56007547250192e-06,2.55474083132867e-06),
    NSC       = c(0.00582691049203277,0.00565235828980803,0.00280602532438934,0.00323596899397671,0.00322869536466897),
    seedC     = c(0,0,0,0,0),
    leafC     = c(0,6.20249411440454e-05,0.00194301584269851,0.00216852338053286,0.00216961721889675),
    rootC     = c(0,3.70325360563584e-05,0.0011600946309045,0.00129473593551666,0.0012953890254721),
    SW_C      = c(0.00250000017695129,0.00163698103278875,0.00191696360707283,0.00226848176680505,0.00227023381739855),
    HW_C      = c(0,0.000880499777849764,0.00132189702708274,0.00156422972213477,0.00156543299090117),
    NSN       = c(0.000293089426122606,0.000291046482743695,0.000211540478630923,0.000204758209292777,0.000204587166081183),
    seedN     = c(0,0,0,0,0),
    leafN     = c(0,1.06719380710274e-06,3.34313081111759e-05,3.73113834939431e-05,3.73302027583122e-05),
    rootN     = c(0,9.25813424146327e-07,2.90023490379099e-05,3.23683452734258e-05,3.2384672522312e-05),
    SW_N      = c(7.14285715730512e-06,4.67708878204576e-06,5.47703893971629e-06,6.48137665848481e-06,6.48638251732336e-06),
    HW_N      = c(0,2.51571350418089e-06,3.77684705199499e-06,4.46922467745026e-06,4.47266256742296e-06),
    McrbC     = c(3.69218014384387e-07,7.35389221517835e-07,0.000101825069577899,0.000244498573010787,0.000244678347371519),
    fastSOM   = c(0.009996865876019,0.00999375618994236,0.00964514445513487,0.00866211205720901,0.00866379030048847),
    slowSOM   = c(0.000999990850687027,0.000999981770291924,0.00114092021249235,0.00132202042732388,0.0013230872573331),
    McrbN     = c(3.6921800727896e-08,7.3538920730698e-08,1.01825071396888e-05,2.44498569372809e-05,2.44678340095561e-05),
    fastSoilN = c(0.000666457752231508,0.000666250416543335,0.000634161347988993,0.000566041155252606,0.000566019618418068),
    slowSoilN = c(2.49997719947714e-05,2.4999544621096e-05,2.70209675363731e-05,2.96457219519652e-05,2.96609432552941e-05),
    mineralN  = c(0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239),
    N_uptk    = c(0,0,0,0,0))
  ref_BiomeE_Pmodel_odt_yr251 <- tibble(
    year      = c(251,251,251,251,251),
    doy       = c(1,2,180,364,365),
    Tc        = c(273.533660888672,271.508483886719,290.345184326172,271.626403808594,273.387603759766),
    Prcp      = c(1.43654549121857,2.00381827354431,2.4158182144165,2.05754542350769,1.82609081268311),
    totWs     = c(799.960327148438,799.958984375,799.517028808594,799.951416015625,799.930053710938),
    Trsp      = c(0,0,0,0,0),
    Evap      = c(0.039690725505352,0.0409707687795162,0.482973247766495,0.0485606715083122,0.0698926448822021),
    Runoff    = c(1.36664271354675,1.96412754058838,1.7442467212677,1.99358224868774,1.77752935886383),
    ws1       = c(19.9603099822998,19.9590301513672,19.5170269012451,19.9514389038086,19.9301071166992),
    ws2       = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3       = c(600,600,600,600,600),
    LAI       = c(3.53568625450134,3.54208874702454,3.65039849281311,3.77342104911804,3.7740535736084),
    GPP       = c(0.00136154366191477,0.0011921344557777,0.00945074297487736,0.00202571228146553,0.00222756084986031),
    Rauto     = c(5.15544525114819e-05,0.00231798319146037,0.00255353678949177,0.00192175526171923,0.0019302882719785),
    Rh        = c(0.00148199405521154,0.00147300760727376,0.00583265163004398,0.00144057429861277,0.00143929268233478),
    NSC       = c(2.0715479850769,2.06586766242981,2.29211640357971,2.14540910720825,2.14195442199707),
    seedC     = c(0.000946450105402619,0.00112251657992601,0.0323686376214027,0.0705148205161095,0.07070492208004),
    leafC     = c(0.601066648960114,0.602155089378357,0.620567798614502,0.641481637954712,0.641589105129242),
    rootC     = c(0.365518838167191,0.36542609333992,0.369782626628876,0.383002161979675,0.383066415786743),
    SW_C      = c(4.51379108428955,4.50026321411133,4.64217710494995,4.84963989257812,4.850670337677),
    HW_C      = c(3.40076804161072,3.4161376953125,3.57569098472595,3.72681593894958,3.7275550365448),
    NSN       = c(0.0628255233168602,0.062759168446064,0.0659686923027039,0.0649487376213074,0.064893014729023),
    seedN     = c(4.73225154564716e-05,5.61258420930244e-05,0.00161843176465482,0.00352574023418128,0.00353524554520845),
    leafN     = c(0.0103419665247202,0.0103606954216957,0.0106774978339672,0.0110373459756374,0.0110391955822706),
    rootN     = c(0.00913795363157988,0.00913563556969166,0.00924454815685749,0.00957503542304039,0.00957664009183645),
    SW_N      = c(0.0128965461626649,0.0128578953444958,0.013263363391161,0.0138561138883233,0.0138590587303042),
    HW_N      = c(0.00971648003906012,0.00976039376109838,0.0102162575349212,0.0106480438262224,0.0106501551344991),
    McrbC     = c(0.125145077705383,0.125146940350533,0.126072600483894,0.124845653772354,0.124842308461666),
    fastSOM   = c(2.1484706401825,2.14904379844666,2.16251230239868,2.00087761878967,2.0015504360199),
    slowSOM   = c(68.6171569824219,68.6166381835938,68.4240798950195,67.9826431274414,67.9821548461914),
    McrbN     = c(0.0125145073980093,0.0125146936625242,0.0126072596758604,0.0124845653772354,0.0124842310324311),
    fastSoilN = c(0.112604945898056,0.112616457045078,0.112205840647221,0.106014303863049,0.106028035283089),
    slowSoilN = c(-0.229112923145294,-0.229108169674873,-0.228179439902306,-0.227036446332932,-0.227031409740448),
    mineralN  = c(0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239),
    N_uptk    = c(0,0,0,0,0))
  ref_BiomeE_Pmodel_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00405564159154892,0.0052763232961297,0.0149482442066073,0.0171369854360819,0.0513847544789314,1.36627888679504),
    LAI             = c(0.0127688692882657,0.0102937677875161,0.0470685660839081,0.0539613887667656,0.156217932701111,3.7746844291687),
    Density         = c(500,494.729614257812,462.272399902344,456.653045654297,2135.7353515625,44049.171875),
    DBH             = c(0.663765788078308,0.796652972698212,1.66887664794922,1.84300994873047,1.17401266098022,1.07525610923767),
    Density12       = c(0,0,0,0,0,135.089202880859),
    DBH12           = c(0,0,0,0,0,23.0577220916748),
    QMD12           = c(0,0,0,0,0,23.1451721191406),
    NPP             = c(0.00389408622868359,0.00674637360498309,0.019524285569787,0.0223992075771093,0.0653136745095253,1.43470561504364),
    GPP             = c(0.00745289633050561,0.00848872400820255,0.0285363532602787,0.0328553542494774,0.096586786210537,2.21575403213501),
    Rauto           = c(0.00355881196446717,0.0017423452809453,0.00901205651462078,0.0104561448097229,0.0312730967998505,0.781048774719238),
    Rh              = c(0.00246006599627435,0.00226382468827069,0.00380709138698876,0.00446669291704893,0.0133570050820708,1.35242986679077),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.904113769531,799.904052734375,799.90478515625,799.904968261719,799.906921386719,799.930053710938),
    Transp          = c(0,0,0,0,0,0),
    Evap            = c(159.256103515625,159.255996704102,157.634735107422,157.307174682617,152.771575927734,94.1745910644531),
    Runoff          = c(428.369079589844,428.273376464844,429.894348144531,430.222198486328,434.757415771484,493.354797363281),
    plantC          = c(0.0105238854885101,0.0156379565596581,0.0656016170978546,0.0793626084923744,0.264621615409851,11.8139190673828),
    soilC           = c(0.0102370381355286,0.00960550550371408,0.0221070796251297,0.0262785404920578,0.0844079181551933,70.1101684570312),
    plantN          = c(0.000285133544821292,0.000247212592512369,0.00118658249266446,0.00138858472928405,0.00399564485996962,0.113515600562096),
    soilN           = c(0.015620275400579,0.0155605813488364,0.0150987496599555,0.0149353016167879,0.0129517503082752,-0.0934814363718033),
    totN            = c(0.0159054081887007,0.0158077944070101,0.0162853319197893,0.0163238868117332,0.0169473960995674,0.0200341641902924),
    NSC             = c(0.0032185222953558,0.00708878319710493,0.0215249620378017,0.0254684872925282,0.0778438746929169,2.13820552825928),
    SeedC           = c(0,0,0.00171516090631485,0.00252696103416383,0.0043891416862607,0.0708947405219078),
    leafC           = c(0.00217070779763162,0.00174994068220258,0.00800165627151728,0.00917343702167273,0.0265570506453514,0.641696333885193),
    rootC           = c(0.00129604001995176,0.00102046376559883,0.00477745896205306,0.00547708105295897,0.0158561207354069,0.383130431175232),
    SapwoodC        = c(0.00227198260836303,0.00211766944266856,0.0175113435834646,0.0217347349971533,0.0820751264691353,4.85169839859009),
    WoodC           = c(0.0015666326507926,0.00366109935566783,0.0120710395276546,0.0149819040670991,0.0579003021121025,3.72829294204712),
    NSN             = c(0.000204416181077249,0.000175080931512639,0.000759190879762173,0.000862567045260221,0.00252291513606906,0.0648373141884804),
    SeedN           = c(0,0,8.57580380397849e-05,0.000126348124467768,0.000219456953345798,0.00354473665356636),
    leafN           = c(3.73489674529992e-05,3.01092750305543e-05,0.000137676295707934,0.000157837974256836,0.000456940557342023,0.0110410405322909),
    rootN           = c(3.2400948839495e-05,2.55115955951624e-05,0.00011943624849664,0.000136926828417927,0.000396402348997071,0.00957824196666479),
    SapwoodN        = c(6.49137973596225e-06,6.05048398938379e-06,5.00324131280649e-05,6.20992504991591e-05,0.000234500388614833,0.0138619970530272),
    WoodN           = c(4.47609045295394e-06,1.04602877399884e-05,3.44886793754995e-05,4.28054190706462e-05,0.000165429475600831,0.010652263648808),
    McrbC           = c(0.000244678347371519,0.000358484103344381,0.000684031459968537,0.000790257065091282,0.00224615167826414,0.124842308461666),
    fastSOM         = c(0.00866819266229868,0.00752276880666614,0.0136073790490627,0.0158520061522722,0.0464400723576546,2.00285220146179),
    SlowSOM         = c(0.00132416712585837,0.00172425212804228,0.00781566929072142,0.00963627826422453,0.035721693187952,67.9824752807617),
    McrbN           = c(2.44678340095561e-05,3.58484103344381e-05,6.84031474520452e-05,7.90257035987452e-05,0.000224615170736797,0.0124842310324311),
    fastSoilN       = c(0.000566131842788309,0.000485190801555291,0.000611094175837934,0.000704500183928758,0.00203651352785528,0.106061227619648),
    slowSoilN       = c(2.96762409561779e-05,3.95422539440915e-05,-0.00058074714615941,-0.000848224095534533,-0.00430937763303518,-0.227026894688606),
    mineralN        = c(0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(2.43759695877088e-05,0,0.000354791147401556,0.000398804142605513,0.00113197544123977,0.0218793321400881),
    N_yrMin         = c(-0.0519865341484547,-0.0519134625792503,-0.0520325116813183,-0.0520266219973564,-0.0519105717539787,-0.0340939238667488),
    N_P2S           = c(4.2480733100092e-05,3.75631389033515e-05,0.000172886459040456,0.000199736459762789,0.000584769120905548,0.0220205187797546),
    N_loss          = c(0.0528971552848816,0.0528973713517189,0.0528936013579369,0.0528931617736816,0.0528868436813354,0.052728496491909),
    totseedC        = c(0,0,0,0,0,0.0664675086736679),
    totseedN        = c(0,0,0,0,0,0.00332337571308017),
    Seedling_C      = c(0,0,0,0,0,0.0664675086736679),
    Seedling_N      = c(0,0,0,0,0,0.00332337548024952),
    MaxAge          = c(1.00000047683716,1.99997925758362,8.00019931793213,9.00026512145996,16.0007247924805,80.8927459716797),
    MaxVolume       = c(0.000207492703339085,0.000315693556331098,0.00172955146990716,0.00217307568527758,0.00812549889087677,0.774184048175812),
    MaxDBH          = c(0.00663765799254179,0.0079665295779705,0.0166887659579515,0.0184300988912582,0.0327002443373203,0.237114235758781),
    NPPL            = c(0.00254952511750162,2.28866315410414e-06,0.00267835124395788,0.00301921833306551,0.00887043215334415,0.168731868267059),
    NPPW            = c(0.00133861263748258,0.00198061787523329,0.00630293367430568,0.00749385636299849,0.0233189761638641,0.66543048620224),
    n_deadtrees     = c(3.00553074339405e-06,2.64790764958889e-06,1.44240239023929e-05,1.73601183632854e-05,5.86944879614748e-05,0.00922025367617607),
    c_deadtrees     = c(0.000110929977381602,0.000167499034432694,0.000797449378296733,0.000992193119600415,0.00399454403668642,0.869915187358856),
    m_turnover      = c(0.000110929977381602,0.000167499034432694,0.000797449378296733,0.000992193119600415,0.00399454403668642,0.869915187358856),
    c_turnover_time = c(1.1703405380249,1.84846329689026,1.91514623165131,1.99922490119934,2.48296928405762,5.60282850265503))
  ref_BiomeE_Pmodel_oac_yr1 <- tibble(
    cohort      = "1", 
    year        = 1, 
    cID         = 1, 
    PFT         = 2, 
    layer       = 1, 
    density     = 500, 
    flayer      = 0.00306598958559334, 
    DBH         = 0.663765788078308, 
    dDBH        = 0.112950146198273, 
    height      = 2.93304538726807, 
    age         = 1.00000047683716,
    BA          = 3.46034685207997e-05,
    dBA         = 3.46034685207997e-05,
    Acrown      = 0.0811128318309784,
    Aleaf       = 0.255377382040024,
    nsc         = 0.0643704459071159,
    seedC       = 0.00408832356333733,
    leafC       = 0,
    rootC       = 0.0434141531586647,
    sapwC       = 0.0259208008646965,
    woodC       = 0.0454396530985832,
    nsn         = 0.031332653015852,
    treeG       = 0.130049347877502,
    fseed       = 0,
    fleaf       = 0.392085790634155,
    froot       = 0.402051895856857,
    fwood       = 0.205862268805504,
    GPP         = 0.149058043956757,
    NPP         = 0.0778816714882851,
    Rauto       = 0.0711762681603432,
    Nupt        = 0.000487519399030134,
    Nfix        = 0,
    n_deadtrees = 3.00553074339405e-06,
    c_deadtrees = 0.000110929977381602,
    deathrate   = 0.0105407815426588)
  ref_BiomeE_Pmodel_oac_yr2 <- tibble(
    cohort      = "2", 
    year        = 2, 
    cID         = 1, 
    PFT         = 2, 
    layer       = 1, 
    density     = 494.729614257812, 
    flayer      = 0.00401289202272892, 
    DBH         = 0.796652972698212, 
    dDBH        = 0.132887154817581, 
    height      = 3.21329045295715,
    age         = 1.99997925758362,
    BA          = 4.98457629873883e-05,
    dBA         = 1.52422944665886e-05,
    Acrown      = 0.106650643050671,
    Aleaf       = 0.208068564534187,
    nsc         = 0.143286004662514,
    seedC       = 0.00353892170824111,
    leafC       = 0,
    rootC       = 0.035371657460928,
    sapwC       = 0.0206266958266497,
    woodC       = 0.0428045801818371,
    nsn         = 0.0740020275115967,
    treeG       = 0.0574493445456028,
    fseed       = 0,
    fleaf       = 0.000805246585514396,
    froot       = 0.302331268787384,
    fwood       = 0.6968634724617,
    GPP         = 0.171582981944084,
    NPP         = 0.136364877223969,
    Rauto       = 0.0352181680500507,
    Nupt        = 0,
    Nfix        = 0,
    n_deadtrees = 2.64790764958889e-06,
    c_deadtrees = 0.000167499034432694,
    deathrate   = 0.0107110552489758)
  ref_BiomeE_Pmodel_oac_yr251 <- tibble(
    cohort      = c("251","502","753","1004","1255"),
    year        = c(251,251,251,251,251),
    cID         = c(25,25,25,25,25),
    PFT         = c(2,2,2,2,2),
    layer       = c(1,1,1,2,2),
    density     = c(122.168533325195,12.9206714630127,4783.298828125,1298.49401855469,37832.2890625),
    flayer      = c(0.205872714519501,0.0129728000611067,0.78115451335907,0.0251633077859879,0.083132416009903),
    DBH         = c(23.7114238739014,16.8767833709717,5.18056917190552,4.19205045700073,0.370734304189682),
    dDBH        = c(0.428679585456848,0.391022861003876,0.268162786960602,0.0598590821027756,0.029956828802824),
    height      = c(17.5300922393799,14.7894248962402,8.19429206848145,7.37081670761108,2.19197487831116),
    age         = c(80.8927459716797,80.8927307128906,29.7683296203613,29.76833152771,2.85343670845032),
    BA          = c(0.0441575683653355,0.0223701689392328,0.00210787472315133,0.00138020294252783,1.07948208096786e-05),
    dBA         = c(0.00158222019672394,0.00102459080517292,0.000212572631426156,-0.00051509914919734,-7.81801645644009e-09),
    Acrown      = c(17.3190803527832,10.3996725082397,1.76862013339996,1.28745126724243,0.0338595807552338),
    Aleaf       = c(54.551197052002,32.7559623718262,5.57001352310181,2.027663230896,0.0365621112287045),
    nsc         = c(38.2109451293945,22.394100189209,3.30161309242249,0.319530755281448,0.00573643902316689),
    seedC       = c(0.892193675041199,0.542978882789612,0.0873714685440063,0.0334938392043114,0.0018752267351374),
    leafC       = c(1.30792379379272,0.764245450496674,0.11076482385397,0.0072888289578259,0),
    rootC       = c(9.27370357513428,5.5685133934021,0.946902275085449,0.344702750444412,0.0062155588530004),
    sapwC       = c(5.53694534301758,3.32472991943359,0.565356314182281,0.20580780506134,0.0037110538687557),
    woodC       = c(169.586791992188,77.579231262207,5.12862682342529,1.62084150314331,0.00423006899654865),
    nsn         = c(116.861305236816,53.4605026245117,3.53438377380371,3.7031307220459,0.015883332118392),
    treeG       = c(21.9133453369141,12.9940357208252,2.06198930740356,0.431162476539612,0.0108073614537716),
    fseed       = c(0.0596861764788628,0.0588150955736637,0.0537174567580223,0.0169050637632608,0),
    fleaf       = c(0.0973390489816666,0.101460285484791,0.12595771253109,0.0694635435938835,0.314212173223495),
    froot       = c(0.305799186229706,0.310388624668121,0.336867988109589,0.511873364448547,0.357975721359253),
    fwood       = c(0.537175536155701,0.529336035251617,0.483456879854202,0.40175798535347,0.327812075614929),
    GPP         = c(35.5235786437988,21.2393074035645,3.52411842346191,0.342023283243179,0.00640262337401509),
    NPP         = c(22.9877700805664,13.8089609146118,2.34248995780945,0.103463336825371,0.000557028746698052),
    Rauto       = c(12.5358057022095,7.43035745620728,1.18162775039673,0.238559931516647,0.00584559282287955),
    Nupt        = c(0.331428468227386,0.212375938892365,0.0359044335782528,0.00293993973173201,0),
    Nfix        = c(0,0,0,0,0),
    n_deadtrees = c(0.00317890685983002,0.000116291135782376,0.00154358800500631,0.00154358800500631,0.0028378798160702),
    c_deadtrees = c(0.522324204444885,0.0167172569781542,0.141630679368973,0.141630679368973,0.0476123578846455),
    deathrate   = c(0.125461295247078,0.0793321281671524,0.0217914208769798,0.0217914208769798,0.35177081823349))
  ref_BiomeE_gsLeun_odt_yr1 <- tibble(
    year      = c(1,1,1,1,1),
    doy       = c(1,2,180,364,365),
    Tc        = c(273.533660888672,271.508483886719,290.34521484375,271.626373291016,273.387603759766),
    Prcp      = c(1.43654537200928,2.00381803512573,2.4158182144165,2.05754518508911,1.8260909318924),
    totWs     = c(799.998291015625,799.998168945312,799.987243652344,799.997924804688,799.997619628906),
    Trsp      = c(0,1.49713571317989e-06,0.00989423505961895,0.000119651507702656,0.000218600151129067),
    Evap      = c(0.0556573569774628,0.0521354898810387,0.929720222949982,0.07036142796278,0.100025936961174),
    Runoff    = c(1.38256895542145,1.95178186893463,0.611501753330231,1.98658740520477,1.72619795799255),
    ws1       = c(19.9983215332031,19.9982204437256,19.9872608184814,19.9979648590088,19.9976501464844),
    ws2       = c(179.999984741211,179.999984741211,179.999984741211,179.999984741211,179.999984741211),
    ws3       = c(600,600,600,600,600),
    LAI       = c(0,0.000364852749044076,0.011269086971879,0.0120545439422131,0.012058237567544),
    GPP       = c(0,6.26108729306907e-08,3.06351903418545e-05,8.34546085570764e-07,1.98450516109006e-06),
    Rauto     = c(2.98780267193877e-09,5.82782595301978e-05,8.66287246026332e-06,4.15019985666731e-06,4.35614811067353e-06),
    Rh        = c(2.7720184334612e-06,2.75420779871638e-06,1.07334790300229e-05,2.55169356933038e-06,2.54445421887795e-06),
    NSC       = c(0.00582691328600049,0.00565216084942222,0.00134072790388018,0.00179522286634892,0.00178503000643104),
    seedC     = c(0,0,0,0,0),
    leafC     = c(0,6.20249702478759e-05,0.0019157447386533,0.00204927264712751,0.0020499003585428),
    rootC     = c(0,3.70325542462524e-05,0.00114381220191717,0.00122353609185666,0.00122391094919294),
    SW_C      = c(0.00250000017695129,0.00163698103278875,0.00187556317541748,0.0020797720644623,0.00208074669353664),
    HW_C      = c(0,0.000880499777849764,0.00129258120432496,0.00143350858706981,0.00143417331855744),
    NSN       = c(0.000293089426122606,0.000291046482743695,0.000213006234844215,0.000189515107194893,0.000189369806321338),
    seedN     = c(0,0,0,0,0),
    leafN     = c(0,1.06719437553693e-06,3.2962067052722e-05,3.52595379808918e-05,3.52703427779488e-05),
    rootN     = c(0,9.2581376520684e-07,2.85953265120042e-05,3.05883622786496e-05,3.05977337120567e-05),
    SW_N      = c(7.14285715730512e-06,4.67708878204576e-06,5.35875187779311e-06,5.94220591665362e-06,5.94499078943045e-06),
    HW_N      = c(0,2.51571350418089e-06,3.69308986591932e-06,4.0957388591778e-06,4.09763833886245e-06),
    McrbC     = c(3.68914641057927e-07,7.35217042802105e-07,0.000101686237030663,0.000243882124777883,0.000244060705881566),
    fastSOM   = c(0.00999687053263187,0.00999375898391008,0.00963238812983036,0.00862457603216171,0.00862602982670069),
    slowSOM   = c(0.00099999166559428,0.000999983283691108,0.00113767047878355,0.00131190498359501,0.00131291372235864),
    McrbN     = c(3.68914641057927e-08,7.35217042802105e-08,1.01686237030663e-05,2.43882132053841e-05,2.44060702243587e-05),
    fastSoilN = c(0.000666458043269813,0.000666250591166317,0.00063381198560819,0.000564941379707307,0.000564913265407085),
    slowSoilN = c(2.49997920036549e-05,2.49995828198735e-05,2.69737010967219e-05,2.95015670417342e-05,2.95159279630752e-05),
    mineralN  = c(0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239),
    N_uptk    = c(0,0,0,3.26594630450927e-07,0))
  ref_BiomeE_gsLeun_odt_yr251 <- tibble(
    year      = c(251,251,251,251,251),
    doy       = c(1,2,180,364,365),
    Tc        = c(273.533660888672,271.508483886719,290.34521484375,271.626373291016,273.387603759766),
    Prcp      = c(1.43654537200928,2.00381803512573,2.4158182144165,2.05754518508911,1.8260909318924),
    totWs     = c(731.164733886719,733.100830078125,716.131103515625,729.361206054688,731.053649902344),
    Trsp      = c(0.0344678051769733,0.0267226919531822,2.92578029632568,0.0337419398128986,0.0616292916238308),
    Evap      = c(0.0402824692428112,0.0410229787230492,0.483833283185959,0.0493568405508995,0.0718512162566185),
    Runoff    = c(0,0,0,0,0),
    ws1       = c(19.9980888366699,19.9979629516602,19.6839447021484,19.9976615905762,19.9972991943359),
    ws2       = c(179.998962402344,179.998901367188,133.943588256836,179.998718261719,179.998504638672),
    ws3       = c(531.167663574219,533.103942871094,562.503601074219,529.364807128906,531.057861328125),
    LAI       = c(3.25403642654419,3.31764793395996,3.32860231399536,3.39499855041504,3.39532446861267),
    GPP       = c(0.000524534785654396,0.000223315844777972,0.00905360467731953,0.000235706131206825,0.000560263695660979),
    Rauto     = c(0.000125647362438031,0.0108530195429921,0.00284489570185542,0.00137514597736299,0.00143388856668025),
    Rh        = c(0.00099808000959456,0.000994771136902273,0.00401309318840504,0.000972399662714452,0.000972421607002616),
    NSC       = c(1.15120446681976,1.11899769306183,1.16031241416931,1.18785095214844,1.18436563014984),
    seedC     = c(0.000137575043481775,0.000451966043328866,0.0157529786229134,0.0365422554314137,0.0366444736719131),
    leafC     = c(0.553186297416687,0.564000070095062,0.565862357616425,0.577149748802185,0.577205121517181),
    rootC     = c(0.330284982919693,0.336364269256592,0.33338075876236,0.33924001455307,0.339270621538162),
    SW_C      = c(4.18843507766724,4.18769931793213,4.27440547943115,4.38803911209106,4.38859939575195),
    HW_C      = c(2.99533867835999,2.99904584884644,3.06052398681641,3.14654517173767,3.14696836471558),
    NSN       = c(0.0552602037787437,0.0548694208264351,0.0549194179475307,0.055515144020319,0.0555202923715115),
    seedN     = c(6.87875262883608e-06,2.25983039854327e-05,0.00078764877980575,0.00182711286470294,0.00183222361374646),
    leafN     = c(0.00951812695711851,0.00970419123768806,0.00973623525351286,0.00993044394999743,0.00993139762431383),
    rootN     = c(0.00825711991637945,0.00840909965336323,0.00833450816571712,0.00848098285496235,0.00848174653947353),
    SW_N      = c(0.0119669558480382,0.0119648557156324,0.0122125865891576,0.0125372549518943,0.0125388512387872),
    HW_N      = c(0.00855811219662428,0.00856870505958796,0.00874435529112816,0.00899013131856918,0.00899134017527103),
    McrbC     = c(0.103956334292889,0.103956826031208,0.104550376534462,0.103543065488338,0.103540726006031),
    fastSOM   = c(1.75759303569794,1.75816917419434,1.78778755664825,1.67230677604675,1.67293691635132),
    slowSOM   = c(37.8931121826172,37.8929290771484,37.8068389892578,37.5839691162109,37.583797454834),
    McrbN     = c(0.0103956330567598,0.0103956824168563,0.0104550374671817,0.0103543065488338,0.0103540727868676),
    fastSoilN = c(0.0914474055171013,0.0914599299430847,0.0917267054319382,0.0873922258615494,0.0874061733484268),
    slowSoilN = c(-0.193332701921463,-0.193328455090523,-0.192495435476303,-0.191494047641754,-0.191489636898041),
    mineralN  = c(0.0149541581049562,0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239),
    N_uptk    = c(0.00110154075082392,3.98107522414648e-06,0.000193415718968026,4.75575870950706e-05,4.82684954477008e-05))
  ref_BiomeE_gsLeun_oat <- tibble(
    year            = c(1,2,8,9,16,251),
    CAI             = c(0.00383033487014472,0.00450962223112583,0.00819942075759172,0.0089819049462676,0.0171406716108322,1.15652751922607),
    LAI             = c(0.012061906978488,0.00972219835966825,0.0258209034800529,0.0282852370291948,0.0513404086232185,3.39564800262451),
    Density         = c(500,494.74462890625,463.442840576172,458.261749267578,918.901733398438,23760.080078125),
    DBH             = c(0.638946235179901,0.717453956604004,1.11637961864471,1.19524621963501,1.02929449081421,1.45303297042847),
    Density12       = c(0,0,0,0,0,24.9917449951172),
    DBH12           = c(0,0,0,0,0,21.3747997283936),
    QMD12           = c(0,0,0,0,0,21.3747978210449),
    NPP             = c(0.00188672903459519,0.00392864737659693,0.00732014887034893,0.00800003856420517,0.0142090143635869,0.971164345741272),
    GPP             = c(0.00582598941400647,0.00570838898420334,0.0126636316999793,0.0138930305838585,0.0253501199185848,1.73267769813538),
    Rauto           = c(0.00393926212564111,0.00177974288817495,0.00534348795190454,0.00589299434795976,0.0111410990357399,0.76151317358017),
    Rh              = c(0.00245671276934445,0.00224067294038832,0.00248544989153743,0.00272041140124202,0.00508538819849491,0.913783609867096),
    rain            = c(587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938,587.529663085938),
    SoilWater       = c(799.997619628906,799.997619628906,799.997619628906,799.997619628906,799.997619628906,731.053649902344),
    Transp          = c(1.58682346343994,1.54753375053406,3.43921232223511,3.77336263656616,6.87999677658081,471.390258789062),
    Evap            = c(176.599166870117,176.623168945312,175.792205810547,175.648391723633,174.330139160156,99.848747253418),
    Runoff          = c(409.372680664062,409.386566162109,408.328338623047,408.135467529297,406.338348388672,15.0407629013062),
    plantC          = c(0.00856858212500811,0.0110101932659745,0.0254160854965448,0.0289349593222141,0.0626543983817101,9.67161273956299),
    soilC           = c(0.0101881846785545,0.00943439546972513,0.0146548906341195,0.0164152216166258,0.0339097157120705,39.3617134094238),
    plantN          = c(0.000265159993432462,0.00023071575560607,0.000603965658228844,0.000672495923936367,0.00126933900173753,0.0972623601555824),
    soilN           = c(0.0156189557164907,0.0155555000528693,0.0154885100200772,0.0154348835349083,0.0149154616519809,-0.0786959081888199),
    totN            = c(0.0158841162919998,0.0157862156629562,0.0160924755036831,0.0161073803901672,0.016184801235795,0.0185664519667625),
    NSC             = c(0.00177722354419529,0.00394269358366728,0.00615795096382499,0.00694745220243931,0.0143724847584963,1.18175745010376),
    SeedC           = c(0,0,0.000483855401398614,0.000698169227689505,0.000431179039878771,0.0367463864386082),
    leafC           = c(0.00205052434466779,0.00165277381893247,0.00438955379649997,0.00480849016457796,0.00872786995023489,0.577260196208954),
    rootC           = c(0.00122428347822279,0.000872367119882256,0.00262082135304809,0.00287095131352544,0.00521105108782649,0.33930104970932),
    SapwoodC        = c(0.0020817150361836,0.00166484387591481,0.00696399761363864,0.00805681198835373,0.0197972003370523,4.38915681838989),
    WoodC           = c(0.00143483537249267,0.00287751504220068,0.00479990663006902,0.00555308582261205,0.0141146080568433,3.14739060401917),
    NSN             = c(0.000189224607311189,0.000167490958119743,0.000405114697059616,0.00044419351615943,0.000870441494043916,0.0554771982133389),
    SeedN           = c(0,0,2.41927609749837e-05,3.49084657500498e-05,2.15589316212572e-05,0.00183731934521347),
    leafN           = c(3.52810748154297e-05,2.84374400507659e-05,7.55266009946354e-05,8.27347903396003e-05,0.000150171588757075,0.00993234384804964),
    rootN           = c(3.0607043299824e-05,2.18091845454182e-05,6.55204858048819e-05,7.17737420927733e-05,0.000130276122945361,0.00848250743001699),
    SapwoodN        = c(5.94775701756589e-06,4.75669639854459e-06,1.9897135643987e-05,2.30194636969827e-05,5.65634290978778e-05,0.0125404475256801),
    WoodN           = c(4.09952963309479e-06,8.22147558210418e-06,1.3714029591938e-05,1.58659677254036e-05,4.03274570999201e-05,0.00899254623800516),
    McrbC           = c(0.000244060705881566,0.000355546886567026,0.000498957117088139,0.000536960025783628,0.00094179785810411,0.103540726006031),
    fastSOM         = c(0.00863019004464149,0.00738893868401647,0.00865897629410028,0.00945140421390533,0.0172651894390583,1.67409074306488),
    SlowSOM         = c(0.00131393398623914,0.00168990984093398,0.00549695733934641,0.00642685731872916,0.0157027263194323,37.5840835571289),
    McrbN           = c(2.44060702243587e-05,3.55546872015111e-05,4.98957124364097e-05,5.36960033059586e-05,9.41797843552195e-05,0.0103540727868676),
    fastSoilN       = c(0.00056501931976527,0.000481053197290748,0.000417977687902749,0.000448278355179355,0.000800038746092469,0.0874356031417847),
    slowSoilN       = c(2.95303816528758e-05,3.88929038308561e-05,2.0636787667172e-05,-6.7090368247591e-05,-0.000978756812401116,-0.191485583782196),
    mineralN        = c(0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239,0.0149999996647239),
    N_fxed          = c(0,0,0,0,0,0),
    N_uptk          = c(3.19233095069649e-06,3.21314303164399e-07,0.000157412388944067,0.000172907326486893,0.00028531753923744,0.0168303567916155),
    N_yrMin         = c(-0.68156760931015,-0.681483685970306,-0.681664407253265,-0.681664645671844,-0.68166196346283,-0.667197227478027),
    N_P2S           = c(4.10517786804121e-05,3.44256659445819e-05,9.57487791310996e-05,0.000105227780295536,0.000193606290849857,0.0169456228613853),
    N_loss          = c(0.682522118091583,0.682522118091583,0.682521104812622,0.682521224021912,0.682520568370819,0.682251811027527),
    totseedC        = c(0,0,0,0,0.00248533301055431,0.0353716835379601),
    totseedN        = c(0,0,0,0,0.000124266938655637,0.00176858354825526),
    Seedling_C      = c(0,0,0,0,0.00248533301055431,0.0353716835379601),
    Seedling_N      = c(0,0,0,0,0.000124266953207552,0.00176858354825526),
    MaxAge          = c(0.999922752380371,2.00033664703369,7.9944634437561,8.9969425201416,16.0144596099854,139.018890380859),
    MaxVolume       = c(0.000190083810593933,0.0002481410629116,0.0006860465509817,0.000802674621809274,0.00211384077556431,0.609839200973511),
    MaxDBH          = c(0.00638946238905191,0.00717453984543681,0.0111637962982059,0.0119524616748095,0.0182092916220427,0.213747978210449),
    NPPL            = c(0.0024177273735404,1.75831144133554e-06,0.00128914858214557,0.00140485574956983,0.00275850435718894,0.140794068574905),
    NPPW            = c(0.00101654999889433,0.00106277200393379,0.00173990649636835,0.00197750912047923,0.00430274149402976,0.352771282196045),
    n_deadtrees     = c(2.78702714240353e-06,2.44736429522163e-06,6.75206683808938e-06,7.60372995500802e-06,1.55495472426992e-05,0.00491177663207054),
    c_deadtrees     = c(9.00621089385822e-05,0.00011679285671562,0.000284140463918447,0.000327159796142951,0.000774936750531197,0.415144175291061),
    m_turnover      = c(9.00621089385822e-05,0.00011679285671562,0.000284140463918447,0.000327159796142951,0.000774936750531197,0.415144175291061),
    c_turnover_time = c(1.41147541999817,2.70755624771118,2.75871539115906,2.8081214427948,3.28037548065186,8.92190170288086))
  ref_BiomeE_gsLeun_oac_yr1 <- tibble(
    cohort      = "1", 
    year        = 1, 
    cID         = 1, 
    PFT         = 2, 
    layer       = 1, 
    density     = 500, 
    flayer      = 0.00306598958559334, 
    DBH         = 0.638946235179901, 
    dDBH        = 0.0881305932998657, 
    height      = 2.87767958641052, 
    age         = 0.999922752380371,
    BA          = 3.20640610880218e-05,
    dBA         = 3.20640610880218e-05,
    Acrown      = 0.0766066983342171,
    Aleaf       = 0.241238132119179,
    nsc         = 0.0355444699525833,
    seedC       = 0.00378449214622378,
    leafC       = 0,
    rootC       = 0.0410104840993881,
    sapwC       = 0.0244856681674719,
    woodC       = 0.0416342988610268,
    nsn         = 0.0286967065185308,
    treeG       = 0.118728518486023,
    fseed       = 0,
    fleaf       = 0.407269865274429,
    froot       = 0.421490788459778,
    fwood       = 0.171239390969276,
    GPP         = 0.116519793868065,
    NPP         = 0.0377345941960812,
    Rauto       = 0.0787852257490158,
    Nupt        = 6.38466153759509e-05,
    Nfix        = 0,
    n_deadtrees = 2.78702714240353e-06,
    c_deadtrees = 9.00621089385822e-05,
    deathrate   = 0.0105107361450791)
  ref_BiomeE_gsLeun_oac_yr2 <- tibble(
    cohort      = "2", 
    year        = 2, 
    cID         = 1, 
    PFT         = 2, 
    layer       = 1, 
    density     = 494.74462890625, 
    flayer      = 0.00379007519222796, 
    DBH         = 0.717453956604004, 
    dDBH        = 0.0785077437758446, 
    height      = 3.04936408996582,
    age         = 2.00033664703369,
    BA          = 4.04276033805218e-05,
    dBA         = 8.36354229249991e-06,
    Acrown      = 0.0911504998803139,
    Aleaf       = 0.196509435772896,
    nsc         = 0.0796914920210838,
    seedC       = 0.00338540226221085,
    leafC       = 0,
    rootC       = 0.0334066040813923,
    sapwC       = 0.0176326744258404,
    woodC       = 0.0336505696177483,
    nsn         = 0.058161623775959,
    treeG       = 0.035260483622551,
    fseed       = 0,
    fleaf       = 0.00100792094599456,
    froot       = 0.389776825904846,
    fwood       = 0.609215199947357,
    GPP         = 0.115380510687828,
    NPP         = 0.0794076025485992,
    Rauto       = 0.0359729565680027,
    Nupt        = 6.49454887025058e-06,
    Nfix        = 0,
    n_deadtrees = 2.44736429522163e-06,
    c_deadtrees = 0.00011679285671562,
    deathrate   = 0.0106077026575804)
  ref_BiomeE_gsLeun_oac_yr251 <- tibble(
    cohort      = c("251","502","753","1004","1255","1506","1757","2008","2259","2510"),
    year        = c(251,251,251,251,251,251,251,251,251,251),
    cID         = c(40,117,117,185,196,205,209,213,217,221),
    PFT         = c(2,2,2,2,2,2,2,2,2,2),
    layer       = c(1,1,2,2,2,2,2,2,2,2),
    density     = c(24.9917449951172,3051.89453125,93.0557708740234,2.37667727470398,28.3467636108398,73.972900390625,305.655364990234,1402.40344238281,7120.03759765625,11657.345703125),
    flayer      = c(0.0364958606660366,0.960054218769073,0.0241257902234793,0.000103919395769481,0.000701286015100777,0.00126169121358544,0.00367818330414593,0.0112265115603805,0.0344583094120026,0.015027473680675),
    DBH         = c(21.3747978210449,7.7843017578125,6.76903343200684,2.11387372016907,1.46058881282806,1.14893805980682,0.918075323104858,0.7073073387146,0.516905009746552,0.38764163851738),
    dDBH        = c(0.211912393569946,0.16106441617012,0.0840798020362854,0.0732675194740295,0.0646425411105156,0.0594982877373695,0.0546658411622047,0.0493528321385384,0.0464871972799301,0.0749638974666595),
    height      = c(16.64381980896,10.0441303253174,9.36624526977539,5.23412847518921,4.35077476501465,3.85886263847351,3.44944357872009,3.02769613265991,2.58827567100525,2.24142980575562),
    age         = c(139.018890380859,67.9092330932617,67.3973770141602,30.2183113098145,20.86643409729,16.0075016021729,11.9977960586548,7.9880256652832,3.99460077285767,1.39321005344391),
    BA          = c(0.0358834266662598,0.00475914822891355,0.00359868002124131,0.000350952206645161,0.000167550548212603,0.000103677179140504,6.61982558085583e-05,3.92921938328072e-05,2.09851168619934e-05,1.18018679131637e-05),
    dBA         = c(0.000707976520061493,0.000190359074622393,8.88451468199492e-05,2.39066139329225e-05,1.45026715472341e-05,1.04599093901925e-05,7.64870492275804e-06,5.29197859577835e-06,3.60480953531805e-06,1.45841841003858e-06),
    Acrown      = c(14.8230457305908,3.2577691078186,2.64167976379395,0.461005508899689,0.264776736497879,0.18472295999527,0.131945297122002,0.0892255082726479,0.0557441040873528,0.0362006835639477),
    Aleaf       = c(46.6906890869141,10.2610969543457,4.1604323387146,0.497814446687698,0.285906285047531,0.199457854032516,0.142465621232986,0.0963355377316475,0.0601816363632679,0.0390721708536148),
    nsc         = c(17.2000980377197,3.5075843334198,1.37352025508881,0.265533119440079,0.145201027393341,0.0978555753827095,0.0672483444213867,0.0432897321879864,0.0264431331306696,0.0234699416905642),
    seedC       = c(0.732490420341492,0.161034971475601,0.0653149336576462,0.00474500702694058,0.00272517721168697,0.00190307002048939,0.00135923561174423,0.000918502453714609,0.00169598811771721,0.00213751173578203),
    leafC       = c(0.567994654178619,0.115359500050545,0.0129345478489995,0,0,0,0,0,0,0),
    rootC       = c(7.93741703033447,1.74438655376434,0.707273542881012,0.0846284553408623,0.0486040711402893,0.0339078344404697,0.0242191553115845,0.0163770411163568,0.0102308783680201,0.00664226897060871),
    sapwC       = c(4.73910427093506,1.04150128364563,0.422283858060837,0.025985911488533,0.014924306422472,0.010411698371172,0.00743670435622334,0.00502871442586184,0.00314148119650781,0.00203956686891615),
    woodC       = c(133.588729858398,13.0854501724243,4.87936592102051,0.231844514608383,0.0990628600120544,0.0570376440882683,0.034047469496727,0.0186871960759163,0.00908366497606039,0.00468475604429841),
    nsn         = c(92.0517730712891,9.01799964904785,11.1481018066406,0.870507121086121,0.371978223323822,0.214164033532143,0.127845630049706,0.0701730251312256,0.034114908427,0.0175981596112251),
    treeG       = c(13.1313953399658,2.82696580886841,1.13397109508514,0.138895481824875,0.0776932537555695,0.05330491065979,0.0373654589056969,0.0247106701135635,0.0156471896916628,0.0152894575148821),
    fseed       = c(0.0432547070086002,0.0408068262040615,0.0114064179360867,0,0,0,0,0,0,0),
    fleaf       = c(0.132434323430061,0.145182713866234,0.138724908232689,0.153452053666115,0.165410995483398,0.174429222941399,0.184251099824905,0.197132244706154,0.211453557014465,0.357199609279633),
    froot       = c(0.435018658638,0.446928530931473,0.44936004281044,0.228073492646217,0.23514387011528,0.239866659045219,0.245203584432602,0.251769304275513,0.250615477561951,0.0745696872472763),
    fwood       = c(0.389292359352112,0.367081969976425,0.400508642196655,0.618474423885345,0.599445164203644,0.58570408821106,0.570545256137848,0.551098465919495,0.537930965423584,0.568230748176575),
    GPP         = c(24.0496978759766,5.24036979675293,2.07000660896301,0.264297813177109,0.150635629892349,0.104455828666687,0.0741392374038696,0.0497130304574966,0.0305801462382078,0.0186414308845997),
    NPP         = c(13.39381980896,2.94410991668701,1.14456343650818,0.153608128428459,0.0883700549602509,0.0615964792668819,0.0439721159636974,0.0296694505959749,0.0180348288267851,0.00809822138398886),
    Rauto       = c(10.6558752059937,2.29626059532166,0.925443947315216,0.110689707100391,0.0622656010091305,0.042859323322773,0.0301671270281076,0.0200435854494572,0.0125453155487776,0.0105432132259011),
    Nupt        = c(0.226463928818703,0.0525825135409832,0.0179873835295439,0.00164998578839004,0.000990381464362144,0.0007167948060669,0.000529507698956877,0.000176085217390209,0,0),
    Nfix        = c(0,0,0,0,0,0,0,0,0,0),
    n_deadtrees = c(0.000451631844043732,0.00276859058067203,0.000119085001642816,5.15107160481421e-07,3.91209960071137e-06,7.43757527743583e-06,2.25572257477324e-05,7.14058987796307e-05,0.000500370748341084,0.000966270570643246),
    c_deadtrees = c(0.0696461498737335,0.276003122329712,0.0164205525070429,7.61558767408133e-05,0.000503623043186963,0.000873036391567439,0.0024265053216368,0.00694022234529257,0.020028056576848,0.0222267899662256),
    deathrate   = c(0.108821861445904,0.031718485057354,0.0951597318053246,0.216726556420326,0.261360436677933,0.285504907369614,0.304401010274887,0.32228085398674,0.338847905397415,0.350268691778183))
  
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_daily_tile )|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365)),  ref_BiomeE_Pmodel_odt_yr1)
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_daily_tile )|>filter(year==251, doy %in% c(1, 2, 180, 364, 365)),  ref_BiomeE_Pmodel_odt_yr251)
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_annual_tile)|>filter(           year %in% c(1, 2, 8, 9, 16, 251)), ref_BiomeE_Pmodel_oat)
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts)|>filter(year==  1),                                ref_BiomeE_Pmodel_oac_yr1)
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts)|>filter(year==  2),                                ref_BiomeE_Pmodel_oac_yr2)
  expect_equal(tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts)|>filter(year==251),                                ref_BiomeE_Pmodel_oac_yr251)

  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_daily_tile )|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365)),  ref_BiomeE_gsLeun_odt_yr1)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_daily_tile )|>filter(year==251, doy %in% c(1, 2, 180, 364, 365)),  ref_BiomeE_gsLeun_odt_yr251)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_annual_tile)|>filter(           year %in% c(1, 2, 8, 9, 16, 251)), ref_BiomeE_gsLeun_oat)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts)|>filter(year==  1),                                ref_BiomeE_gsLeun_oac_yr1)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts)|>filter(year==  2),                                ref_BiomeE_gsLeun_oac_yr2)
  expect_equal(tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts)|>filter(year==251),                                ref_BiomeE_gsLeun_oac_yr251)
  
})


