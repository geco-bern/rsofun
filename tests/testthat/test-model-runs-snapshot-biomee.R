# BiomeE: test model outputs, repeatability and agreement with reference values from subfolder ./_snaps/
set.seed(10)

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
