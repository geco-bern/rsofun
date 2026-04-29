# BiomeE: test model outputs, repeatability and agreement with reference values from subfolder ./_snaps/
set.seed(10)

test_that("BiomeEP Tksoil initial days (rolling mean, smoothed)", {
  out <- runread_biomee_f(
    biomee_p_model_drivers,
    makecheck = TRUE,
    parallel = FALSE)
  testthat::expect_equal(tolerance = 1e-6,
    object = out$data[[1]]$output_daily_tile$Tksoil[1:100], # |> dput()
    expected = c(274.131225585938, 274.048858642578, 273.926605224609, 273.828460693359, 
                 273.749633789062, 273.685852050781, 273.649108886719, 273.640594482422, 
                 273.629180908203, 273.614959716797, 273.617065429688, 273.612640380859, 
                 273.610260009766, 273.594940185547, 273.549255371094, 273.518737792969, 
                 273.537414550781, 273.561279296875, 273.596313476562, 273.595153808594, 
                 273.577362060547, 273.511474609375, 273.436340332031, 273.346618652344, 
                 273.287780761719, 273.245819091797, 273.211639404297, 273.187286376953, 
                 273.146240234375, 273.085357666016, 273.017272949219, 272.994445800781, 
                 272.974212646484, 272.945190429688, 272.927551269531, 272.899078369141, 
                 272.884002685547, 272.870178222656, 272.845550537109, 272.824340820312, 
                 272.797088623047, 272.765411376953, 272.720794677734, 272.682922363281, 
                 272.681671142578, 272.687591552734, 272.684417724609, 272.677215576172, 
                 272.686370849609, 272.702239990234, 272.70263671875,  272.713531494141, 
                 272.748168945312, 272.802947998047, 272.880401611328, 272.964019775391, 
                 273.035675048828, 273.078796386719, 273.143005371094, 273.194885253906, 
                 273.249633789062, 273.320373535156, 273.384155273438, 273.424102783203, 
                 273.451446533203, 273.467651367188, 273.490570068359, 273.537017822266, 
                 273.592193603516, 273.650756835938, 273.727630615234, 273.828094482422, 
                 273.946411132812, 274.096221923828, 274.250335693359, 274.402404785156, 
                 274.532379150391, 274.634552001953, 274.724029541016, 274.813507080078, 
                 274.903900146484, 274.981170654297, 275.064575195312, 275.181274414062, 
                 275.298217773438, 275.421630859375, 275.558258056641, 275.715179443359, 
                 275.874176025391, 276.011444091797, 276.159576416016, 276.333129882812, 
                 276.5234375,      276.67919921875,  276.811828613281, 276.972717285156, 
                 277.122802734375, 277.219665527344, 277.342437744141, 277.502380371094
    ))
})

test_that("Snapshot tests run_biomee_f_bysite()", {
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
  
  # d) Testing numeric values against reference values
  #    By hardcoding the outputs below:
  #       - any code changes to the numeric outputs must be reflected in below values
  #       - and thus such breaking changes are legibly tracked in the git history
  #
  # Hardcoded reference outputs: snapshot testing
  # NOTE: this is expected to change reasonably frequently whenever something is
  #       changed in the model.
  #       If this is expected, please update the hardcoded reference snapshots in
  #       the subfolder tests/testthat/_snap/
  #       To do so, simply follow the instuctions, e.g. snapshot_accept(). Thanks!
  mod_BiomeE_Pmodel_odt_yr1   <- tibble(mod_BiomeE_Pmodel$data$output_daily_tile )|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365))
  mod_BiomeE_Pmodel_odt_yr251 <- tibble(mod_BiomeE_Pmodel$data$output_daily_tile )|>filter(year==251, doy %in% c(1, 2, 180, 364, 365))
  mod_BiomeE_Pmodel_oat       <- tibble(mod_BiomeE_Pmodel$data$output_annual_tile)|>filter(           year %in% c(1, 2, 8, 9, 16, 251))
  mod_BiomeE_Pmodel_oac_yr1   <- tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts)|>filter(year==  1)
  mod_BiomeE_Pmodel_oac_yr2   <- tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts)|>filter(year==  2)
  mod_BiomeE_Pmodel_oac_yr251 <- tibble(mod_BiomeE_Pmodel$data$output_annual_cohorts)|>filter(year==251)
  
  
  mod_BiomeE_gsLeun_odt_yr1   <- tibble(mod_BiomeE_gsLeun$data$output_daily_tile)|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365))
  mod_BiomeE_gsLeun_odt_yr251 <- tibble(mod_BiomeE_gsLeun$data$output_daily_tile)|>filter(year==251, doy %in% c(1, 2, 180, 364, 365))
  mod_BiomeE_gsLeun_oat       <- tibble(mod_BiomeE_gsLeun$data$output_annual_tile)|>filter(           year %in% c(1, 2, 8, 9, 16, 251))
  mod_BiomeE_gsLeun_oac_yr1   <- tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts)|>filter(year==  1)
  mod_BiomeE_gsLeun_oac_yr2   <- tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts)|>filter(year==  2)
  mod_BiomeE_gsLeun_oac_yr251 <- tibble(mod_BiomeE_gsLeun$data$output_annual_cohorts)|>filter(year==251)

  
  mod_BiomeE_PLULUC_aggregated          <- tibble(mod_BiomeE_PLULUC$aggregated$output_annual_cell|>filter(        year %in% c(1, 2, 8, 9, 16, 251)))
  
  mod_BiomeE_PLULUC_primary_odt_yr1     <- tibble(mod_BiomeE_PLULUC$primary$output_daily_tile|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365)))
  mod_BiomeE_PLULUC_primary_odt_yr251   <- tibble(mod_BiomeE_PLULUC$primary$output_daily_tile|>filter(year==251, doy %in% c(1, 2, 180, 364, 365)))
  mod_BiomeE_PLULUC_primary_oat         <- tibble(mod_BiomeE_PLULUC$primary$output_annual_tile|>filter(           year %in% c(1, 2, 8, 9, 16, 251)))
  mod_BiomeE_PLULUC_primary_oac_yr1     <- tibble(mod_BiomeE_PLULUC$primary$output_annual_cohorts|>filter(year==  1))
  mod_BiomeE_PLULUC_primary_oac_yr2     <- tibble(mod_BiomeE_PLULUC$primary$output_annual_cohorts|>filter(year==  2))
  mod_BiomeE_PLULUC_primary_oac_yr251   <- tibble(mod_BiomeE_PLULUC$primary$output_annual_cohorts|>filter(year==251))
  
  mod_BiomeE_PLULUC_secondary_odt_yr1   <- tibble(mod_BiomeE_PLULUC$secondary$output_daily_tile|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365)))
  mod_BiomeE_PLULUC_secondary_odt_yr251 <- tibble(mod_BiomeE_PLULUC$secondary$output_daily_tile|>filter(year==251, doy %in% c(1, 2, 180, 364, 365)))
  mod_BiomeE_PLULUC_secondary_oat       <- tibble(mod_BiomeE_PLULUC$secondary$output_annual_tile|>filter(           year %in% c(1, 2, 8, 9, 16, 251)))
  mod_BiomeE_PLULUC_secondary_oac_yr1   <- tibble(mod_BiomeE_PLULUC$secondary$output_annual_cohorts|>filter(year==  1))
  mod_BiomeE_PLULUC_secondary_oac_yr2   <- tibble(mod_BiomeE_PLULUC$secondary$output_annual_cohorts|>filter(year==  2))
  mod_BiomeE_PLULUC_secondary_oac_yr251 <- tibble(mod_BiomeE_PLULUC$secondary$output_annual_cohorts|>filter(year==251))
  

  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_odt_yr1,   tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_odt_yr251, tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_oat,       tolerance = 0.04, cran = TRUE)  # Higher than 0.01 for N_uptk on Windows
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_oac_yr1,   tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_oac_yr2,   tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_oac_yr251, tolerance = 0.085, cran = TRUE) # Higher than 0.01 for Nupt on Windows
  
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_odt_yr1,   tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_odt_yr251, tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_oat,       tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_oac_yr1,   tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_oac_yr2,   tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_oac_yr251, tolerance = 0.01, cran = TRUE)
  
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_aggregated,          tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_primary_odt_yr1,     tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_primary_odt_yr251,   tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_primary_oat,         tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_primary_oac_yr1,     tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_primary_oac_yr2,     tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_primary_oac_yr251,   tolerance = 0.085, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_secondary_odt_yr1,   tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_secondary_odt_yr251, tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_secondary_oat,       tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_secondary_oac_yr1,   tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_secondary_oac_yr2,   tolerance = 0.01, cran = TRUE)
  expect_snapshot_value_fmt(mod_BiomeE_PLULUC_secondary_oac_yr251, tolerance = 0.085, cran = TRUE)
})
