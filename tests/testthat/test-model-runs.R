context("test models and their parameters")
set.seed(10)

test_that("run_pmodel_f_bysite()", {
  skip_on_cran()
  
  # load parameters (valid ones)
  params_modl <- list(
    kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41,
    whc                = 2000 # site info, water holding capacity in mm
  )

  # read in demo data
  #df_drivers <- p_model_drivers # TODO: NOT YET UPDATED FOR PHYDRO (a newformat, b add phydro_ parameters)
  df_drivers <- readRDS(file = here::here("data/p_model_drivers_newformat.rds"))
  
  # check run_pmodel_f_bysite() ##########################
  # run the SOFUN Fortran P-model using the internal function `run_pmodel_f_bysite`
  mod1 <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = FALSE, use_pml = FALSE, use_gs = FALSE),
    site_info      = df_drivers$site_info[[1]],
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl,
    makecheck      = TRUE
  )
  mod2 <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = FALSE, use_pml = FALSE, use_gs = TRUE),
    site_info      = df_drivers$site_info[[1]],
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl,
    makecheck      = TRUE
  )
  mod3 <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = FALSE, use_pml = TRUE, use_gs = TRUE),
    site_info      = df_drivers$site_info[[1]],
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl,
    makecheck      = TRUE
  )
  # mod4 <- run_pmodel_f_bysite(
  #   sitename       = df_drivers$sitename[1],
  #   params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = TRUE, use_pml = TRUE, use_gs = TRUE),
  #   site_info      = df_drivers$site_info[[1]],
  #   forcing        = df_drivers$forcing[[1]],
  #   forcing_acclim = df_drivers$forcing[[1]],
  #   params_modl    = params_modl,             # TODO: phydro requires params: bsoil, phydro_alpha, phydro_b_plant, phydro_gamma, phydro_K_plant, phydro_p50_plant, Ssoil 
  #   makecheck      = TRUE
  # )

  # test if the returned values
  # are in a list (don't error / warning)
  expect_type(mod1, "list")
  expect_s3_class(mod1, "data.frame")
  
  expect_true(all(!is.na(tibble(mod1))))
  expect_true(all(!is.na(tibble(mod2))))
  expect_true(all(!is.na(tibble(mod3))))
  # expect_true(all(!is.na(tibble(mod4))))
  
  # tibble(mod1) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref1 <- tibble(
    date      = structure(c(13514, 13583, 14714, 14914, 15515, 15695), class = "Date"), 
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97), 
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846,0.689359784126282), 
    gpp       = c(1.65618813037872, 6.02679443359375, 6.72385692596436, 1.84405922889709, 9.40890026092529, 0.896598398685455), 
    aet       = c(0.977257549762726, 1.5287424325943, 3.31175374984741, 1.61871206760406, 3.53805994987488, 1.13068926334381), 
    le        = c(2417903.75, 3775541, 8173714, 3988548.25, 8600976, 2803547.25), 
    pet       = c(0.103817254304886, 1.45484209060669, 3.08336925506592, 0.335356384515762, 6.51526165008545, -0.377504140138626), 
    vcmax     = c(9.54577535594581e-06, 1.18804200610612e-05, 1.91590133908903e-05, 1.37124443426728e-05, 5.95575438637752e-05, 5.93948425375856e-06), 
    jmax      = c(3.08439557556994e-05, 3.66509229934309e-05, 5.80160958634224e-05, 3.691642996273e-05, 0.000109297579911072, 2.01222374016652e-05), 
    vcmax25   = c(3.51696653524414e-05, 3.77493124688044e-05, 5.76805359742139e-05, 3.59945297532249e-05, 5.17318439960945e-05, 2.60039796557976e-05), 
    jmax25    = c(7.8623415902257e-05, 8.34658203530125e-05,0.000126882849144749, 7.2937982622534e-05, 9.94235306279734e-05,5.87244903726969e-05), 
    gs_accl   = c(0.0016942253569141, 0.00173744710627943,0.00175592955201864, 0.00170137174427509, 0.00156046624761075,0.00193470600061119), 
    wscal     = c(0.157053604722023, 0.15498948097229, 0.310147076845169, 0.205126166343689, 0.264321148395538, 0.318660259246826), 
    chi       = c(0.629108071327209, 0.642833471298218, 0.639411568641663,0.66897189617157, 0.674327433109283, 0.673606336116791), 
    iwue      = c(9.066015627468e-05,8.73051467351615e-05, 8.65905749378726e-05, 7.94415027485229e-05,8.00566194811836e-05, 8.02328504505567e-05), 
    rd        = c(0.0936944633722305, 0.121099025011063, 0.188513651490211, 0.146433308720589, 0.569870233535767,0.0640662834048271), 
    tsoil     = c(8.88349437713623, 11.0428524017334,11.4038162231445, 15.0282745361328, 20.0777721405029, 9.41864013671875), 
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083), 
    wcont     = c(314.107208251953, 309.978973388672, 620.294128417969, 410.252319335938, 528.642272949219, 637.320495605469), 
    snow      = c(0, 0, 0, 0, 0, 0), 
    cond      = c(0, 0, 0, 0, 0, 0), 
    le_canopy = c(0, 0, 0, 0, 0, 0), 
    le_soil   = c(0, 0, 0, 0, 0, 0), 
    dpsi      = c(0, 0, 0, 0, 0, 0), 
    psi_leaf  = c(0, 0, 0, 0, 0, 0))
  # tibble(mod2) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> select(wscal, wcont) |> dput()
  ref2 <- tibble(
    date      = structure(c(13514, 13583, 14714, 14914, 15515, 15695), class = "Date"), 
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97), 
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282), 
    gpp       = c(1.65618813037872, 6.02679443359375, 6.72385692596436, 1.84405922889709, 9.40890026092529, 0.896598398685455), 
    aet       = c(0.0831360220909119, 1.46265971660614, 1.86834669113159, 0.240892946720123, 5.62481117248535, -0.0911358147859573), 
    le        = c(205692.84375, 3612336.25, 4611252, 593566.4375, 13673840, -225971.515625), 
    pet       = c(0.103817254304886, 1.45484209060669, 3.08336925506592, 0.335356384515762, 6.51526165008545, -0.377504140138626), 
    vcmax     = c(9.54577535594581e-06, 1.18804200610612e-05, 1.91590133908903e-05, 1.37124443426728e-05, 5.95575438637752e-05, 5.93948425375856e-06), 
    jmax      = c(3.08439557556994e-05, 3.66509229934309e-05, 5.80160958634224e-05, 3.691642996273e-05, 0.000109297579911072, 2.01222374016652e-05), 
    vcmax25   = c(3.51696653524414e-05, 3.77493124688044e-05, 5.76805359742139e-05, 3.59945297532249e-05, 5.17318439960945e-05, 2.60039796557976e-05), 
    jmax25    = c(7.8623415902257e-05, 8.34658203530125e-05, 0.000126882849144749, 7.2937982622534e-05, 9.94235306279734e-05, 5.87244903726969e-05), 
    gs_accl   = c(0.0016942253569141, 0.00173744710627943, 0.00175592955201864, 0.00170137174427509, 0.00156046624761075, 0.00193470600061119), 
    # wscal    = c(0.10409427434206, 0.129727497696877, 0.587451696395874, 0.497713387012482, 0.780628979206085, 0.86269211769104), 
    wscal     = c(0.104094229638577, 0.129727452993393, 0.587451696395874, 0.497713387012482, 0.780628979206085, 0.86269211769104), 
    chi       = c(0.629108071327209, 0.642833471298218, 0.639411568641663, 0.66897189617157, 0.674327433109283, 0.673606336116791), 
    iwue      = c(9.066015627468e-05, 8.73051467351615e-05, 8.65905749378726e-05, 7.94415027485229e-05, 8.00566194811836e-05, 8.02328504505567e-05), 
    rd        = c(0.0936944633722305, 0.121099025011063, 0.188513651490211, 0.146433308720589, 0.569870233535767, 0.0640662834048271), 
    tsoil     = c(9.0220832824707, 11.0288057327271, 11.3866157531738, 15.0849714279175, 19.9336929321289, 9.59065818786621), 
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083), 
    wcont     = c(208.188461303711, 259.454895019531, 1174.90344238281, 995.4267578125, 1561.25793457031, 1725.38427734375), 
    snow      = c(0, 0, 0, 0, 0, 0), 
    cond      = c(0, 0, 0, 0, 0, 0), 
    le_canopy = c(127639.390625, 2577880.75, 2284844.5, 376195.9375, 9554412, 4795.3779296875), 
    le_soil   = c(78053.453125, 1034455.5625, 2326407.75, 217370.515625, 4119428, -230766.890625), 
    dpsi      = c(0, 0, 0, 0, 0, 0), 
    psi_leaf  = c(0, 0, 0, 0, 0, 0))

  # tibble(mod3) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref3 <- tibble(
    date      = structure(c(13514, 13583, 14714, 14914, 15515, 15695), class = "Date"), 
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97), 
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282), 
    gpp       = c(1.65618813037872, 6.02679443359375, 6.72385692596436, 1.84405922889709, 9.40890026092529, 0.896598398685455), 
    aet       = c(0.100568220019341, 1.24525260925293, 2.51481199264526, 0.302865445613861, 5.28670930862427, -0.297944843769073), 
    le        = c(248823.109375, 3075405.25, 6206788, 746268.3125, 12851919, -738755.125), 
    pet       = c(0.103817254304886, 1.45484209060669, 3.08336925506592, 0.335356384515762, 6.51526165008545, -0.377504140138626), 
    vcmax     = c(9.54577535594581e-06, 1.18804200610612e-05, 1.91590133908903e-05, 1.37124443426728e-05, 5.95575438637752e-05, 5.93948425375856e-06), 
    jmax      = c(3.08439557556994e-05, 3.66509229934309e-05, 5.80160958634224e-05, 3.691642996273e-05, 0.000109297579911072, 2.01222374016652e-05), 
    vcmax25   = c(3.51696653524414e-05, 3.77493124688044e-05, 5.76805359742139e-05, 3.59945297532249e-05, 5.17318439960945e-05, 2.60039796557976e-05), 
    jmax25    = c(7.8623415902257e-05, 8.34658203530125e-05, 0.000126882849144749, 7.2937982622534e-05, 9.94235306279734e-05, 5.87244903726969e-05), 
    gs_accl   = c(0.0016942253569141, 0.00173744710627943, 0.00175592955201864, 0.00170137174427509, 0.00156046624761075, 0.00193470600061119), 
    wscal     = c(0.0814640149474144, 0.108003601431847, 0.461629748344421, 0.326346158981323, 0.524367153644562, 0.587543845176697), 
    chi       = c(0.629108071327209, 0.642833471298218, 0.639411568641663, 0.66897189617157, 0.674327433109283, 0.673606336116791), 
    iwue      = c(9.066015627468e-05, 8.73051467351615e-05, 8.65905749378726e-05, 7.94415027485229e-05, 8.00566194811836e-05, 8.02328504505567e-05), 
    rd        = c(0.0936944633722305, 0.121099025011063, 0.188513651490211, 0.146433308720589, 0.569870233535767, 0.0640662834048271), 
    tsoil     = c(9.16450881958008, 11.0153522491455, 11.3974561691284, 15.0535011291504, 20.0137233734131, 9.49603080749512), 
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083), 
    wcont     = c(162.928024291992, 216.007202148438, 923.259521484375, 652.692321777344, 1048.73425292969, 1175.08764648438), 
    snow      = c(0, 0, 0, 0, 0, 0), 
    cond      = c(0, 0, 0, 0, 0, 0), 
    le_canopy = c(170769.65625, 2040949.625, 3880380.25, 528897.8125, 8732491, -507988.21875), 
    le_soil   = c(78053.453125, 1034455.5625, 2326407.75, 217370.515625, 4119428, -230766.890625), 
    dpsi      = c(0, 0, 0, 0, 0, 0), 
    psi_leaf  = c(0, 0, 0, 0, 0, 0)
  )
  expect_equal(slice(tibble(mod1), c(1, 70, 1200, 1400, 2000, 2180)), ref1, tolerance = 1e-7)
  expect_equal(slice(tibble(mod2), c(1, 70, 1200, 1400, 2000, 2180)), ref2, tolerance = 1e-7)
  expect_equal(slice(tibble(mod3), c(1, 70, 1200, 1400, 2000, 2180)), ref3, tolerance = 1e-7)
  
})
test_that("runread_pmodel_f()", {
  skip_on_cran()
  
  # load parameters (valid ones)
  params_modl <- list(
    kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41,
    whc                = 2000 # site info, water holding capacity in mm
  )
  
  # read in demo data
  #df_drivers <- p_model_drivers # TODO: NOT YET UPDATED FOR PHYDRO (a newformat, b add phydro_ parameters)
  df_drivers <- readRDS(file = here::here("data/p_model_drivers_newformat.rds"))
  
  df_output_singlecore <- rsofun::runread_pmodel_f(
    df_drivers,
    par = params_modl, 
    makecheck = TRUE,
    parallel = FALSE, ncores = 1
  )
  df_output_singlecore$data[[1]]

  # test for correctly returned values
  expect_type(df_output_singlecore, "list")
  
  # test runread_pmodel_f
  df_output_parallel <- rsofun::runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = TRUE, ncores = 1
  )
  
  # test for correctly returned values
  expect_type(df_output_parallel, "list")
  
  # test singlecore is equal to multicore
  expect_identical(df_output_singlecore, df_output_parallel)
})

test_that("p-model run check Vcmax25", {
  skip_on_cran()
  
  # load parameters (valid ones)
  params_modl <- list(
    kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )
  
  # read in demo data
  df_drivers <- p_model_drivers_vcmax25 |>
    # TODO: NOT YET UPDATED FOR PHYDRO
    # # specify additionally needed params_siml flags:
    mutate(params_siml = purrr::map(params_siml, \(x)
                                  mutate(x,
                                         use_pml    = TRUE,
                                         use_gs     = TRUE,
                                         use_phydro = FALSE))) |>
    # specify additionally needed site info:
    mutate(site_info = purrr::map(site_info, \(x)
                                  mutate(x,
                                         canopy_height = 5,
                                         reference_height = 10)))
  
  # run the SOFUN Fortran P-model
  mod <- run_pmodel_f_bysite( 
    df_drivers$sitename[1],
    df_drivers$params_siml[[1]],
    df_drivers$site_info[[1]],
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl = params_modl,
    makecheck = FALSE
  )
  
  # test if the returned values
  # are in a list (don't error / warning)
  expect_type(mod, "list")
  
  # test runread_pmodel_f
  df_output <- rsofun::runread_pmodel_f(
    df_drivers,
    par = params_modl, 
    makecheck = FALSE,
    parallel = FALSE
  )
  
  # test for correctly returned values
  expect_type(df_output, "list")
  
  # test runread_pmodel_f
  df_output_p <- rsofun::runread_pmodel_f(
    df_drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = TRUE,
    ncores = 1
  )
  
  # test for correctly returned values
  expect_type(df_output_p, "list")
})

test_that("phydro-model run check LE and AET", {
  # skip_on_cran()

  # load parameters (valid ones)
  params_modl <- list(
    kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )

  # read in demo data
  #df_drivers <- p_model_drivers # TODO: NOT YET UPDATED FOR PHYDRO
  df_drivers <- readRDS(file = here::here("data/p_model_drivers_newformat.rds"))
  df_drivers$params_siml[[1]]$use_gs     <- TRUE

  # run the SOFUN Fortran PHYDRO-model
  # Run 3 simulations with different WHC:
  df_output <- bind_rows(
    rsofun::runread_pmodel_f(
      drivers = df_drivers |>
        tidyr::unnest(site_info) |> mutate(whc = 432) |>
        tidyr::nest(site_info = !c(sitename, params_siml, starts_with("forcing"))),
      par = purrr::assign_in(params_modl, "whc", 432)
    ) |> mutate(sitename = paste0(sitename, "_432mm")),
    rsofun::runread_pmodel_f(
      drivers = df_drivers |>
        tidyr::unnest(site_info) |> mutate(whc = 5) |>
        tidyr::nest(site_info = !c(sitename, params_siml, starts_with("forcing"))),
      par = purrr::assign_in(params_modl, "whc", 5)
    ) |> mutate(sitename = paste0(sitename, "_5mm")),
    rsofun::runread_pmodel_f(
      drivers = df_drivers |>
        tidyr::unnest(site_info) |> mutate(whc = 5000) |>
        tidyr::nest(site_info = !c(sitename, params_siml, starts_with("forcing"))),
      par = purrr::assign_in(params_modl, "whc", 5000)
    ) |> mutate(sitename = paste0(sitename, "_5000mm"))
  )

  # # Plot:
  # df_output |>
  #   tidyr::unnest(data) |> select(-site_info) |>
  #   filter(date < "2012-01-01") |>
  #   select(sitename, date, gpp, aet, le, pet, le_canopy, le_soil) |>
  #   tidyr::pivot_longer(!c(sitename,date)) %>%
  #   ggplot(data = ., mapping=aes(x=date, y=value, color=sitename, linetype=sitename)) +
  #   geom_line() +
  #   facet_grid(name~., scales = "free_y") +
  #   theme_bw()

  # 1) Check that le is sum of le_canopy and le_soil
  df_output |>
    tidyr::unnest(data) |>
    # select(sitename, date, aet, le, le_canopy, le_soil) |>
    group_by(sitename) |>
    mutate(le_sum = le_canopy + le_soil) |>
    mutate(test_equality_lesum = expect_equal(le_sum, le, tolerance = 0.1))
    # filter(abs(le_sum - le) > 5)

  # 2) Check that aet and le give the same
  library(cwd)
  le_to_et <- function(le_Wm2, tc, patm){
    # Convert latent heat flux (W/m2) to evapotranspiration in mass units (mm/d).
    1000 * 60 * 60 * 24 * le_Wm2 / (cwd::calc_enthalpy_vap(tc) * cwd::calc_density_h2o(tc, patm))
    # mm/m * s/day      * W/m2   * (kg / J) * (m3 / kg)
    #     = mm/day * s/m3 * m3/J * W  * kg/kg
    #     = mm/day
  }

  df_output |>
    tidyr::unnest(data) |>
    select(sitename, date, aet_mmday = aet, le_Jm2d = le) |>
    # append temperature and pressure form forcing to convert LE into AET:
    left_join(select(df_drivers$forcing[[1]],
                     date, ta_degC = temp, pa_Pa = patm)) |>
    group_by(sitename) |>
    # mutate(le_mmd = le_to_et(le_Jm2d / 86400, ta_degC, pa_Pa))
    mutate(test_equality_aet_le  =
             expect_equal(aet_mmday, le_to_et(le_Jm2d / 86400, ta_degC, pa_Pa),
                          tolerance = 0.01)) # tolerance in mm/d


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
