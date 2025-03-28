context("BiomeE: test model outputs, repeatability and agreement with reference values")
set.seed(10)

test_that("Snapshot tests run_biomee_f_bysite()", {
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
  expect_s3_class(mod_BiomeE_Pmodel$output_daily_tile, "data.frame")
  expect_s3_class(mod_BiomeE_Pmodel$output_annual_tile, "data.frame")
  expect_s3_class(mod_BiomeE_Pmodel$output_annual_cohorts, "data.frame")
  
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel$output_annual_cohorts))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun$output_annual_cohorts))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel_2ndTry$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel_2ndTry$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_Pmodel_2ndTry$output_annual_cohorts))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun_2ndTry$output_daily_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun_2ndTry$output_annual_tile))))
  expect_true(all(!is.na(tibble(mod_BiomeE_gsLeun_2ndTry$output_annual_cohorts))))

  # Testing memory leakage, i.e. repeatability
  expect_equal(tibble(mod_BiomeE_Pmodel$output_daily_tile    ), tibble(mod_BiomeE_Pmodel_2ndTry$output_daily_tile    ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_Pmodel$output_annual_tile   ), tibble(mod_BiomeE_Pmodel_2ndTry$output_annual_tile   ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_Pmodel$output_annual_cohorts), tibble(mod_BiomeE_Pmodel_2ndTry$output_annual_cohorts), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_gsLeun$output_daily_tile    ), tibble(mod_BiomeE_gsLeun_2ndTry$output_daily_tile    ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_gsLeun$output_annual_tile   ), tibble(mod_BiomeE_gsLeun_2ndTry$output_annual_tile   ), tolerance = 1e-6)
  expect_equal(tibble(mod_BiomeE_gsLeun$output_annual_cohorts), tibble(mod_BiomeE_gsLeun_2ndTry$output_annual_cohorts), tolerance = 1e-6)
  
  # Hardcoded reference outputs: snapshot testing
  # NOTE: this is expected to change reasonably frequently whenever something is
  #       changed in the model.
  #       If this is expected, please update the hardcoded reference snapshots in 
  #       the subfolder tests/testthat/_snap/
  #       To do so, simply follow the instructions, e.g. snapshot_accept(). Thanks!
  mod_BiomeE_Pmodel_odt_yr1   <- tibble(mod_BiomeE_Pmodel$output_daily_tile )|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365))  
  mod_BiomeE_Pmodel_odt_yr251 <- tibble(mod_BiomeE_Pmodel$output_daily_tile )|>filter(year==251, doy %in% c(1, 2, 180, 364, 365))  
  mod_BiomeE_Pmodel_oat       <- tibble(mod_BiomeE_Pmodel$output_annual_tile)|>filter(           year %in% c(1, 2, 8, 9, 16, 251)) 
  mod_BiomeE_Pmodel_oac_yr1   <- tibble(mod_BiomeE_Pmodel$output_annual_cohorts)|>filter(year==  1)                                
  mod_BiomeE_Pmodel_oac_yr2   <- tibble(mod_BiomeE_Pmodel$output_annual_cohorts)|>filter(year==  2)                                
  mod_BiomeE_Pmodel_oac_yr251 <- tibble(mod_BiomeE_Pmodel$output_annual_cohorts)|>filter(year==251)                                
  
  mod_BiomeE_gsLeun_odt_yr1   <- tibble(mod_BiomeE_gsLeun$output_daily_tile )|>filter(year==  1, doy %in% c(1, 2, 180, 364, 365))
  mod_BiomeE_gsLeun_odt_yr251 <- tibble(mod_BiomeE_gsLeun$output_daily_tile )|>filter(year==251, doy %in% c(1, 2, 180, 364, 365))
  mod_BiomeE_gsLeun_oat       <- tibble(mod_BiomeE_gsLeun$output_annual_tile)|>filter(           year %in% c(1, 2, 8, 9, 16, 251))
  mod_BiomeE_gsLeun_oac_yr1   <- tibble(mod_BiomeE_gsLeun$output_annual_cohorts)|>filter(year==  1)
  mod_BiomeE_gsLeun_oac_yr2   <- tibble(mod_BiomeE_gsLeun$output_annual_cohorts)|>filter(year==  2)
  mod_BiomeE_gsLeun_oac_yr251 <- tibble(mod_BiomeE_gsLeun$output_annual_cohorts)|>filter(year==251)
  
  testthat::local_edition(3)
  # define a better formatted version of testthat::expect_snapshot_value 
  expect_snapshot_value_fmt <- function(
    x, 
    style = c("json", "json2", "deparse", "serialize"),
    cran = FALSE, tolerance = testthat_tolerance(), ..., variant = NULL) {
    # copy of expect_snapshot_value() but using constructive::construct()
    # for parsing of the values
    testthat:::edition_require(3, "expect_snapshot_value()")
    variant <- testthat:::check_variant(variant)
    lab <- rlang::quo_label(enquo(x))
    save <- function(x) paste0(as.character((constructive::construct(x))$code), collapse = "\n")
    load <- function(x) eval(parse(text = x))  # load <- function(x) testthat:::reparse(x)
    testthat:::with_is_snapshotting(force(x))
    testthat:::check_roundtrip(x, load(save(x)), label = lab, style = "constructive::construct", 
                               ..., tolerance = tolerance)
    testthat:::expect_snapshot_helper(lab, x, save = save, load = load, 
                                      cran = cran, ..., tolerance = tolerance, variant = variant, 
                                      trace_env = rlang::caller_env())
  }
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_odt_yr1,   tolerance = 0.05)
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_odt_yr251, tolerance = 0.05)
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_oat,       tolerance = 0.05)
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_oac_yr1,   tolerance = 0.05)
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_oac_yr2,   tolerance = 0.05)
  expect_snapshot_value_fmt(mod_BiomeE_Pmodel_oac_yr251, tolerance = 0.085) # For Nupt on Windows
  
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_odt_yr1,   tolerance = 0.05)
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_odt_yr251, tolerance = 0.05)
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_oat,       tolerance = 0.05)
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_oac_yr1,   tolerance = 0.05)
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_oac_yr2,   tolerance = 0.05)
  expect_snapshot_value_fmt(mod_BiomeE_gsLeun_oac_yr251, tolerance = 0.05)
})
