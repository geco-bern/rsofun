context("test models and their parameters")
set.seed(10)
test_that("run_pmodel_f_bysite()", {
  skip_on_cran()
  
  # load parameters (valid ones)
  params_modl <- list(
    kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.01,    # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    rd_to_vcmax        = 0.014,   # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41,
    gw_calib           = 2.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    beta_unitcostratio = 146.0
  )
  params_modl_phydro <- list(
    kphio              = 0.04998,
    kphio_par_a        = 0.01,       # set to zero to disable temperature-dependence of kphio
    kphio_par_b        = 1.0,
    rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41,
    gw_calib           = 2.0,
    phydro_K_plant     = 5e-17,   # TODO: add documentaiton: Phydro: Plant conductivity                
    phydro_p50_plant   = -0.46,   # TODO: add documentaiton: Phydro: Plant P50               
    phydro_b_plant     = 1,       # TODO: add documentaiton: Phydro: shape parameter of vulnerability curve           
    phydro_alpha       = 0.08,    # TODO: add documentaiton: Phydro: Cost of Jmax              
    phydro_gamma       = 0.065,   # TODO: add documentaiton: Phydro: Cost of hydraulics               
    bsoil              = 3,       # TODO: add documentaiton: Phydro: parameter converting RZWSC to predawn water potential (depends on rooting system hence PFT specific)           
    Ssoil              = 113      # TODO: add documentaiton: Phydro: parameter converting RZWSC to predawn water potential (depends on rooting system hence PFT specific)            
  )

  # read in demo data
  df_drivers <- rsofun::p_model_drivers_formatPhydro

  # P-hydro's analytical gs = E / (1.6 * VPD / patm) needs positive VPD.
  # Use an explicit 1 Pa lower bound for this regression fixture rather than
  # recording platform-dependent NaNs from exact-zero deficit days.
  forcing_phydro <- df_drivers$forcing[[1]] |>
    mutate(vpd = pmax(vpd, 1))
  
  # check run_pmodel_f_bysite() ##########################
  # run the SOFUN Fortran P-model using the internal function `run_pmodel_f_bysite`
  mod1 <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = FALSE, use_pml = FALSE, use_gs = FALSE),
    site_info      = mutate(df_drivers$site_info[[1]], whc = 2000),
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl,
    makecheck      = TRUE
  )
  mod2 <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = FALSE, use_pml = FALSE, use_gs = TRUE),
    site_info      = mutate(df_drivers$site_info[[1]], whc = 2000),
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl,
    makecheck      = TRUE
  )
  mod3 <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = FALSE, use_pml = TRUE, use_gs = TRUE),
    site_info      = mutate(df_drivers$site_info[[1]], whc = 2000),
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl,
    makecheck      = TRUE
  )
  mod4 <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = TRUE, use_pml = TRUE, use_gs = TRUE),
    site_info      = mutate(df_drivers$site_info[[1]], whc = 253),
    forcing        = forcing_phydro,
    forcing_acclim = forcing_phydro,
    params_modl    = params_modl_phydro,
    makecheck      = TRUE
  )
  
  # Rerun again (inverse order) to test memory leakage:
  mod4_2ndTry <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = TRUE, use_pml = TRUE, use_gs = TRUE),
    site_info      = mutate(df_drivers$site_info[[1]], whc = 253),
    forcing        = forcing_phydro,
    forcing_acclim = forcing_phydro,
    params_modl    = params_modl_phydro,
    makecheck      = TRUE
  )
  mod3_2ndTry <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = FALSE, use_pml = TRUE, use_gs = TRUE),
    site_info      = mutate(df_drivers$site_info[[1]], whc = 2000),
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl,
    makecheck      = TRUE
  )
  mod2_2ndTry <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = FALSE, use_pml = FALSE, use_gs = TRUE),
    site_info      = mutate(df_drivers$site_info[[1]], whc = 2000),
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl,
    makecheck      = TRUE
  )
  mod1_2ndTry <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = FALSE, use_pml = FALSE, use_gs = FALSE),
    site_info      = mutate(df_drivers$site_info[[1]], whc = 2000),
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl,
    makecheck      = TRUE
  )
  


  # Testing if the returned values are in a list (don't error / warning)
  expect_type(mod1, "list")
  expect_s3_class(mod1, "data.frame")
  
  expect_true(all(!is.na(tibble(mod1))))
  expect_true(all(!is.na(tibble(mod2))))
  expect_true(all(!is.na(tibble(mod3))))
  expect_true(all(!is.na(tibble(mod4))))
  expect_true(all(!is.na(tibble(mod1_2ndTry))))
  expect_true(all(!is.na(tibble(mod2_2ndTry))))
  expect_true(all(!is.na(tibble(mod3_2ndTry))))
  expect_true(all(!is.na(tibble(mod4_2ndTry))))
  
  # Testing memory leakage, i.e. repeatability
  expect_equal(tibble(mod1), tibble(mod1_2ndTry), tolerance = 1e-6)
  expect_equal(tibble(mod2), tibble(mod2_2ndTry), tolerance = 1e-6)
  expect_equal(tibble(mod3), tibble(mod3_2ndTry), tolerance = 1e-6)
  expect_equal(tibble(mod4), tibble(mod4_2ndTry), tolerance = 1e-6)
  
  
  # Hardcoded reference outputs.
  # NOTE: this is expected to change reasonably frequently whenever something is
  #       changed in the model.
  #       If this is expected, please update the hardcoded reference values below.
  #       To do so, simply use the commented code, making use of dput(). Thanks!
  # tibble(mod1) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref1 <- tibble(
    date      = as.Date(c("2007-01-01", "2007-03-11", "2010-04-15", "2010-11-01", "2012-06-24", "2012-12-21")),
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97),
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282),
    gpp       = c(1.65618813037872, 6.02679443359375, 6.72385692596436, 1.84405922889709, 9.40890026092529, 0.896598398685455),
    aet       = c(0.977257549762726, 1.5287424325943, 3.31175374984741, 1.61871206760406, 3.53805994987488, 1.13068926334381),
    le        = c(2417903.75, 3775541, 8173714, 3988548.25, 8600976, 2803547.25),
    pet       = c(0.103817254304886, 1.45484209060669, 3.08336925506592, 0.335356384515762, 6.51526165008545, -0.377504140138626),
    vcmax     = c(9.54577535594581e-06, 1.18804200610612e-05, 1.91590133908903e-05, 1.37124443426728e-05, 5.95575438637752e-05, 5.93948425375856e-06),
    jmax      = c(3.08439557556994e-05, 3.66509229934309e-05, 5.80160958634224e-05, 3.691642996273e-05, 0.000109297579911072, 2.01222374016652e-05),
    vcmax25   = c(3.51696653524414e-05, 3.77493124688044e-05, 5.76805359742139e-05, 3.59945297532249e-05, 5.17318439960945e-05, 2.60039796557976e-05),
    jmax25    = c(7.8623415902257e-05, 8.34658203530125e-05, 0.000126882849144749, 7.2937982622534e-05, 9.94235306279734e-05, 5.87244903726969e-05),
    gs_accl   = c(1.80036209940226e-07, 6.6131934772784e-07, 7.74569343775511e-07, 2.13159879081104e-07, 1.07288553863327e-06, 9.92605464489316e-08),
    wscal     = c(0.157053604722023, 0.15498948097229, 0.310147076845169, 0.205126166343689, 0.264321148395538, 0.318660259246826),
    chi       = c(0.629108071327209, 0.642833471298218, 0.639411568641663, 0.66897189617157, 0.674327433109283, 0.673606336116791),
    iwue      = c(9.066015627468e-05, 8.73051467351615e-05, 8.65905749378726e-05, 7.94415027485229e-05, 8.00566194811836e-05, 8.02328504505567e-05),
    rd        = c(0.0936944633722305, 0.121099025011063, 0.188513651490211, 0.146433308720589, 0.569870233535767, 0.0640662834048271),
    tsoil     = c(8.88349437713623, 11.0428524017334, 11.4038162231445, 15.0282745361328, 20.0777721405029, 9.41864013671875),
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083),
    wcont     = c(314.107208251953, 309.978973388672, 620.294128417969, 410.252319335938, 528.642272949219, 637.320495605469),
    snow      = c(0, 0, 0, 0, 0, 0),
    cond      = c(0, 0, 0, 0, 0, 0),
    le_canopy = c(0, 0, 0, 0, 0, 0),
    le_soil   = c(0, 0, 0, 0, 0, 0),
    dpsi      = c(0, 0, 0, 0, 0, 0),
    psi_leaf  = c(0, 0, 0, 0, 0, 0),
    runoff    = c(0, 0, 0, 0, 0, 0)
  )
  
  # tibble(mod2) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref2 <- tibble(
    date      = as.Date(c("2007-01-01", "2007-03-11", "2010-04-15", "2010-11-01", "2012-06-24", "2012-12-21")),
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97),
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282),
    gpp       = c(1.65618813037872, 6.02679443359375, 6.72385692596436, 1.84405922889709, 9.40890026092529, 0.896598398685455),
    aet       = c(0.0913383290171623, 1.57156276702881, 2.11342096328735, 0.263829350471497, 6.06539297103882, 0.00193400727584958),
    le        = c(225986.78125, 3881294.5, 5216118.5, 650082.375, 14744888, 4795.37646484375),
    pet       = c(0.103817254304886, 1.45484209060669, 3.08336925506592, 0.335356384515762, 6.51526165008545, -0.377504140138626),
    vcmax     = c(9.54577535594581e-06, 1.18804200610612e-05, 1.91590133908903e-05, 1.37124443426728e-05, 5.95575438637752e-05, 5.93948425375856e-06),
    jmax      = c(3.08439557556994e-05, 3.66509229934309e-05, 5.80160958634224e-05, 3.691642996273e-05, 0.000109297579911072, 2.01222374016652e-05),
    vcmax25   = c(3.51696653524414e-05, 3.77493124688044e-05, 5.76805359742139e-05, 3.59945297532249e-05, 5.17318439960945e-05, 2.60039796557976e-05),
    jmax25    = c(7.8623415902257e-05, 8.34658203530125e-05, 0.000126882849144749, 7.2937982622534e-05, 9.94235306279734e-05, 5.87244903726969e-05),
    gs_accl   = c(1.80036209940226e-07, 6.6131934772784e-07, 7.74569343775511e-07, 2.13159879081104e-07, 1.07288553863327e-06, 9.92605464489316e-08),
    wscal     = c(0.0870173871517181, 0.1104985922575, 0.522542655467987, 0.419027715921402, 0.660358786582947, 0.729638695716858),
    chi       = c(0.629108071327209, 0.642833471298218, 0.639411568641663, 0.66897189617157, 0.674327433109283, 0.673606336116791),
    iwue      = c(9.066015627468e-05, 8.73051467351615e-05, 8.65905749378726e-05, 7.94415027485229e-05, 8.00566194811836e-05, 8.02328504505567e-05),
    rd        = c(0.0936944633722305, 0.121099025011063, 0.188513651490211, 0.146433308720589, 0.569870233535767, 0.0640662834048271),
    tsoil     = c(9.10265350341797, 11.021068572998, 11.3922777175903, 15.0690364837646, 19.9729499816895, 9.54344177246094),
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083),
    wcont     = c(174.034774780273, 220.997177124023, 1045.08532714844, 838.055419921875, 1320.71752929688, 1459.27734375),
    snow      = c(0, 0, 0, 0, 0, 0),
    cond      = c(0, 0, 0, 0, 0, 0),
    le_canopy = c(127639.3984375, 2577880.75, 2284844.5, 376195.875, 9554412, 4795.3779296875),
    le_soil   = c(98347.3515625, 1303413.875, 2931274, 273886.84375, 5190479, 0),
    dpsi      = c(0, 0, 0, 0, 0, 0),
    psi_leaf  = c(0, 0, 0, 0, 0, 0),
    runoff    = c(0, 0, 0, 0, 0, 0)
  )

  # tibble(mod3) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref3 <- tibble(
    date      = as.Date(c("2007-01-01", "2007-03-11", "2010-04-15", "2010-11-01", "2012-06-24", "2012-12-21")),
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97),
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282),
    gpp       = c(1.65618813037872, 6.02679443359375, 6.72385692596436, 1.84405922889709, 9.40890026092529, 0.896598398685455),
    aet       = c(0.0496958047151566, 0.866879522800446, 1.8788982629776, 0.156055092811584, 4.60135173797607, -0.0194761194288731),
    le        = c(122955.9921875, 2140935.5, 4637294.5, 384523.75, 11185824, -48291.09375),
    pet       = c(0.108938276767731, 1.35599589347839, 2.76327228546143, 0.326252281665802, 5.732346534729, -0.322612106800079),
    vcmax     = c(9.54577535594581e-06, 1.18804200610612e-05, 1.91590133908903e-05, 1.37124443426728e-05, 5.95575438637752e-05, 5.93948425375856e-06),
    jmax      = c(3.08439557556994e-05, 3.66509229934309e-05, 5.80160958634224e-05, 3.691642996273e-05, 0.000109297579911072, 2.01222374016652e-05),
    vcmax25   = c(3.51696653524414e-05, 3.77493124688044e-05, 5.76805359742139e-05, 3.59945297532249e-05, 5.17318439960945e-05, 2.60039796557976e-05),
    jmax25    = c(7.8623415902257e-05, 8.34658203530125e-05, 0.000126882849144749, 7.2937982622534e-05, 9.94235306279734e-05, 5.87244903726969e-05),
    gs_accl   = c(1.80036209940226e-07, 6.6131934772784e-07, 7.74569343775511e-07, 2.13159879081104e-07, 1.07288553863327e-06, 9.92605464489316e-08),
    wscal     = c(0.268708527088165, 0.299058884382248, 0.853358745574951, 0.794888317584991, 0.962663650512695, 1),
    chi       = c(0.629108071327209, 0.642833471298218, 0.639411568641663, 0.66897189617157, 0.674327433109283, 0.673606336116791),
    iwue      = c(9.066015627468e-05, 8.73051467351615e-05, 8.65905749378726e-05, 7.94415027485229e-05, 8.00566194811836e-05, 8.02328504505567e-05),
    rd        = c(0.0936944633722305, 0.121099025011063, 0.188513651490211, 0.146433308720589, 0.569870233535767, 0.0640662834048271),
    tsoil     = c(8.90407085418701, 11.0406312942505, 11.3600034713745, 15.1555614471436, 19.8312454223633, 9.67749691009521),
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083),
    wcont     = c(537.417053222656, 598.117797851562, 1706.71752929688, 1589.77661132812, 1925.32727050781, 2000),
    snow      = c(0, 0, 0, 0, 0, 0),
    cond      = c(0, 0, 0, 0, 0, 0),
    le_canopy = c(24608.6328125, 837521.75, 1706020.75, 110637.0078125, 5995345.5, -48291.109375),
    le_soil   = c(98347.3515625, 1303413.875, 2931274, 273886.84375, 5190479, 0),
    dpsi      = c(0, 0, 0, 0, 0, 0),
    psi_leaf  = c(0, 0, 0, 0, 0, 0),
    runoff    = c(0, 0, 0, 0, 0, 1.6195068359375)
  )
  
  # tibble(mod4) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref4 <- tibble(
    date      = as.Date(c("2007-01-01", "2007-03-11", "2010-04-15", "2010-11-01", "2012-06-24", "2012-12-21")),
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97),
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282),
    gpp       = c(2.47801828384399, 4.28905963897705, 6.3787431716919, 1.1185222864151, 4.02259016036987, 1.46236944198608),
    aet       = c(0.108937665820122, 1.35598397254944, 2.7632565498352, 0.326243430376053, 5.73223686218262, -0.205343887209892),
    le        = c(269530.5625, 3348878.75, 6819973, 803872.25, 13934991, -509150.75),
    pet       = c(0.108938276767731, 1.35599589347839, 2.76327228546143, 0.326252281665802, 5.732346534729, -0.322612106800079),
    vcmax     = c(6.11318637311342e-06, 7.75735861680005e-06, 1.1996246939816e-05, 3.171238631694e-06, 4.024429290439e-05, 4.28104931415874e-06),
    jmax      = c(2.28261833399301e-05, 2.86256417894037e-05, 4.33794921264052e-05, 5.82312213737168e-06, 7.37965820007958e-05, 1.72473573911702e-05),
    vcmax25   = c(1.77601305040298e-05, 2.0241144738975e-05, 2.96207872452214e-05, 7.73034571466269e-06, 3.53723116859328e-05, 1.51182721310761e-05),
    jmax25    = c(5.0449903937988e-05, 5.72783465031534e-05, 8.36151448311284e-05, 1.03888187368284e-05, 6.85645500198007e-05, 4.33691857324447e-05),
    gs_accl   = c(0.120841890573502, 0.0664659515023232, 0.0994757488369942, 0.0195570159703493, 0.0161445159465075, 0.811914145946503),
    wscal     = c(0.614042401313782, 0.806674122810364, 0.963730275630951, 0.413668930530548, 0.435908675193787, 1),
    chi       = c(0.949473917484283, 0.841001987457275, 0.839071214199066, 0.856464982032776, 0.389523804187775, 0.99558699131012),
    iwue      = c(1.97608169401065e-05, 6.21843064436689e-05, 6.17925106780604e-05, 5.51137491129339e-05, 0.000240103618125431, 1.73566104422207e-06),
    rd        = c(0.0766694843769073, 0.101897828280926, 0.157458364963531, 0.0470401868224144, 0.579597413539886, 0.0540313646197319),
    tsoil     = c(8.94792938232422, 11.0362043380737, 11.3827972412109, 15.0883674621582, 19.9829540252686, 9.52828884124756),
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083),
    wcont     = c(155.352722167969, 204.08854675293, 243.823760986328, 104.658241271973, 110.284896850586, 253),
    snow      = c(0, 0, 0, 0, 0, 0),
    cond      = c(0, 0, 0, 0, 0, 0),
    le_canopy = c(171183.203125, 2045464.875, 3888698.75, 529985.375, 8744512, -509150.75),
    le_soil   = c(98347.3515625, 1303413.875, 2931274, 273886.84375, 5190479, 0),
    dpsi      = c(0.343142241239548, 1.05541467666626, 1.22262001037598, 0.456934839487076, 1.89833831787109, 0.0780339315533638),
    psi_leaf  = c(-0.654849529266357, -1.13271486759186, -1.22975397109985, -1.61481726169586, -2.73323273658752, -0.0780339315533638),
    runoff    = c(0, 0, 0, 0, 0, 1.80534362792969)
  )
  expect_equal(dplyr::slice(tibble(mod1), c(1, 70, 1200, 1400, 2000, 2180)), ref1, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod2), c(1, 70, 1200, 1400, 2000, 2180)), ref2, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod3), c(1, 70, 1200, 1400, 2000, 2180)), ref3, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod4), c(1, 70, 1200, 1400, 2000, 2180)), ref4, tolerance = 2e-6) # gs_accl requires 2e-6 tolerance
  
  expect_equal(dplyr::slice(tibble(mod1_2ndTry), c(1, 70, 1200, 1400, 2000, 2180)), ref1, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod2_2ndTry), c(1, 70, 1200, 1400, 2000, 2180)), ref2, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod3_2ndTry), c(1, 70, 1200, 1400, 2000, 2180)), ref3, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod4_2ndTry), c(1, 70, 1200, 1400, 2000, 2180)), ref4, tolerance = 2e-6) # gs_accl requires 2e-6 tolerance
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
    gw_calib           = 2.0
  )
  
  # read in demo data
  df_drivers <- rsofun::p_model_drivers_formatPhydro # TODO: NOT YET UPDATED FOR PHYDRO (still add default phydro_* parameters)

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
    kc_jmax            = 0.41,
    gw_calib           = 2.0
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
  mod <- rsofun::run_pmodel_f_bysite( 
    df_drivers$sitename[1],
    df_drivers$params_siml[[1]],
    df_drivers$site_info[[1]],
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl = params_modl,
    makecheck = TRUE
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
    kc_jmax            = 0.41,
    gw_calib           = 2.0
  )

  # read in demo data
  df_drivers <- rsofun::p_model_drivers_formatPhydro # TODO: NOT YET UPDATED FOR PHYDRO (still add default phydro_* parameters)
  df_drivers$params_siml[[1]]$use_gs     <- TRUE

  # run the SOFUN Fortran PHYDRO-model
  # Run 3 simulations with different WHC:
  df_output <- bind_rows(
    rsofun::runread_pmodel_f(
      drivers = df_drivers |>
        tidyr::unnest(site_info) |> mutate(whc = 432) |>
        tidyr::nest(site_info = !c(sitename, params_siml, starts_with("forcing"))),
      par = params_modl
    ) |> mutate(sitename = paste0(sitename, "_432mm")),
    rsofun::runread_pmodel_f(
      drivers = df_drivers |>
        tidyr::unnest(site_info) |> mutate(whc = 5) |>
        tidyr::nest(site_info = !c(sitename, params_siml, starts_with("forcing"))),
      par = params_modl
    ) |> mutate(sitename = paste0(sitename, "_5mm")),
    rsofun::runread_pmodel_f(
      drivers = df_drivers |>
        tidyr::unnest(site_info) |> mutate(whc = 5000) |>
        tidyr::nest(site_info = !c(sitename, params_siml, starts_with("forcing"))),
      par = params_modl
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
