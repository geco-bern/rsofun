context("test models and their parameters")

test_that("p-model run check", {
  skip_on_cran()
  
  # load parameters (valid ones)
  params_modl <- list(
    kphio           = 0.05,
    soilm_par_a     = 1.0,
    soilm_par_b     = 0.0,
    vpdstress_par_a = 0.2,
    vpdstress_par_b = 0.2,
    vpdstress_par_m = 5
  )
  
  # read in demo data
  df_drivers <- p_model_drivers
  
  # run the SOFUN Fortran P-model
  mod <- run_pmodel_f_bysite( 
    df_drivers$sitename[1],
    df_drivers$params_siml[[1]],
    df_drivers$siteinfo[[1]],
    df_drivers$forcing[[1]], 
    df_drivers$df_soiltexture[[1]],
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

# test_that("lm3ppa p-model run check", {
#   skip_on_cran()
#   
#   df_drivers <- lm3ppa_p_model_drivers
#   
#   df_output <- run_lm3ppa_f_bysite(
#     df_drivers$sitename[1],
#     df_drivers$params_siml[[1]],
#     df_drivers$siteinfo[[1]],
#     df_drivers$forcing[[1]],
#     df_drivers$params_tile[[1]],
#     df_drivers$params_species[[1]],
#     df_drivers$params_soil[[1]],
#     df_drivers$init_cohort[[1]],
#     df_drivers$init_soil[[1]],
#     makecheck = TRUE)
#   
#   # test for correctly returned values
#   expect_type(df_output, "list")
# })

test_that("lm3ppa leuning run check", {
  skip_on_cran()
  
  df_drivers <- lm3ppa_gs_leuning_drivers
  df_drivers$params_siml[[1]]$spinup <- TRUE
  df_drivers$params_siml[[1]]$spinupyears <- 1
  df_drivers$params_tile[[1]]$par_mort_under <- 1
  
  df_output <- run_lm3ppa_f_bysite(
    df_drivers$sitename[1],
    df_drivers$params_siml[[1]],
    df_drivers$siteinfo[[1]],
    df_drivers$forcing[[1]],
    df_drivers$params_tile[[1]],
    df_drivers$params_species[[1]],
    df_drivers$params_soil[[1]],
    df_drivers$init_cohort[[1]],
    df_drivers$init_soil[[1]],
    makecheck = TRUE)
  
  # test for correctly returned values
  expect_type(df_output, "list")
  
})