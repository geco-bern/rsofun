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
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
    params_modl    = params_modl_phydro,
    makecheck      = TRUE
  )
  
  # Rerun again (inverse order) to test memory leakage:
  mod4_2ndTry <- run_pmodel_f_bysite(
    sitename       = df_drivers$sitename[1],
    params_siml    = dplyr::mutate(df_drivers$params_siml[[1]], use_phydro = TRUE, use_pml = TRUE, use_gs = TRUE),
    site_info      = mutate(df_drivers$site_info[[1]], whc = 253),
    forcing        = df_drivers$forcing[[1]],
    forcing_acclim = df_drivers$forcing[[1]],
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
  # expect_true(all(!is.na(tibble(mod4)))) # TODO: some gpp,gs_accl, chi, iwue  are NaN. Is this expected?
  expect_true(all(!is.na(tibble(mod1_2ndTry))))
  expect_true(all(!is.na(tibble(mod2_2ndTry))))
  expect_true(all(!is.na(tibble(mod3_2ndTry))))
  # expect_true(all(!is.na(tibble(mod4_2ndTry)))) # TODO: some gpp,gs_accl, chi, iwue  are NaN. Is this expected?
  
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
    date      = as.Date(c("2007-01-01","2007-03-11","2010-04-15","2010-11-01","2012-06-24","2012-12-21")), 
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
    psi_leaf  = c(0, 0, 0, 0, 0, 0))
  
  # tibble(mod2) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref2 <- tibble(
    date      = as.Date(c("2007-01-01","2007-03-11","2010-04-15","2010-11-01","2012-06-24","2012-12-21")), 
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97),
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282),
    gpp       = c(1.65618813037872, 6.02679443359375, 6.72385692596436, 1.84405922889709, 9.40890026092529, 0.896598398685455),
    aet       = c(0.0913383215665817, 1.57156276702881, 2.11342096328735, 0.263829529285431, 6.06539392471313, -0.115333966910839),
    le        = c(225986.75, 3881294.5, 5216118.5, 650082.75, 14744891, -285970.90625), 
    pet       = c(0.103817254304886, 1.45484209060669, 3.08336925506592, 0.335356384515762, 6.51526165008545, -0.377504140138626),
    vcmax     = c(9.54577535594581e-06, 1.18804200610612e-05, 1.91590133908903e-05, 1.37124443426728e-05, 5.95575438637752e-05, 5.93948425375856e-06),
    jmax      = c(3.08439557556994e-05, 3.66509229934309e-05, 5.80160958634224e-05, 3.691642996273e-05, 0.000109297579911072, 2.01222374016652e-05),
    vcmax25   = c(3.51696653524414e-05, 3.77493124688044e-05, 5.76805359742139e-05, 3.59945297532249e-05, 5.17318439960945e-05, 2.60039796557976e-05),
    jmax25    = c(7.8623415902257e-05, 8.34658203530125e-05, 0.000126882849144749, 7.2937982622534e-05, 9.94235306279734e-05, 5.87244903726969e-05),
    gs_accl   = c(1.80036209940226e-07, 6.6131934772784e-07, 7.74569343775511e-07, 2.13159879081104e-07, 1.07288553863327e-06, 9.92605464489316e-08),
    wscal     = c(0.0881856083869934, 0.112692959606647, 0.529396653175354, 0.425926119089127, 0.673057317733765, 0.744019031524658),
    chi       = c(0.629108071327209, 0.642833471298218, 0.639411568641663, 0.66897189617157, 0.674327433109283, 0.673606336116791),
    iwue      = c(9.066015627468e-05, 8.73051467351615e-05, 8.65905749378726e-05, 7.94415027485229e-05, 8.00566194811836e-05, 8.02328504505567e-05),
    rd        = c(0.0936944633722305, 0.121099025011063, 0.188513651490211, 0.146433308720589, 0.569870233535767, 0.0640662834048271),
    tsoil     = c(9.0977668762207, 11.0215282440186, 11.3917474746704, 15.0705471038818, 19.9689388275146, 9.54828262329102),
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083),
    wcont     = c(176.371215820312, 225.385925292969, 1058.79333496094, 851.852233886719, 1346.11462402344, 1488.0380859375),
    snow      = c(0, 0, 0, 0, 0, 0), 
    cond      = c(0, 0, 0, 0, 0, 0), 
    le_canopy = c(127639.3984375, 2577880.75, 2284844.5, 376195.875, 9554412, 4795.3779296875), 
    le_soil   = c(98347.3515625, 1303413.875, 2931274, 273886.84375, 5190479, -290766.28125),     
    dpsi      = c(0, 0, 0, 0, 0, 0),     
    psi_leaf  = c(0, 0, 0, 0, 0, 0))

  # tibble(mod3) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref3 <- tibble(
    date      = as.Date(c("2007-01-01","2007-03-11","2010-04-15","2010-11-01","2012-06-24","2012-12-21")), 
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97),
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282),
    gpp       = c(1.65618813037872, 6.02679443359375, 6.72385692596436, 1.84405922889709, 9.40890026092529, 0.896598398685455),
    aet       = c(0.0496958047151566, 0.866879522800446, 1.87889838218689, 0.156055122613907, 4.60135173797607, -0.136744096875191),
    le        = c(122955.984375, 2140935.5, 4637295, 384523.84375, 11185824, -339057.375), 
    pet       = c(0.108938276767731, 1.35599589347839, 2.76327228546143, 0.326252281665802, 5.732346534729, -0.322612106800079),
    vcmax     = c(9.54577535594581e-06, 1.18804200610612e-05, 1.91590133908903e-05, 1.37124443426728e-05, 5.95575438637752e-05, 5.93948425375856e-06),
    jmax      = c(3.08439557556994e-05, 3.66509229934309e-05, 5.80160958634224e-05, 3.691642996273e-05, 0.000109297579911072, 2.01222374016652e-05),
    vcmax25   = c(3.51696653524414e-05, 3.77493124688044e-05, 5.76805359742139e-05, 3.59945297532249e-05, 5.17318439960945e-05, 2.60039796557976e-05),
    jmax25    = c(7.8623415902257e-05, 8.34658203530125e-05, 0.000126882849144749, 7.2937982622534e-05, 9.94235306279734e-05, 5.87244903726969e-05),
    gs_accl   = c(1.80036209940226e-07, 6.6131934772784e-07, 7.74569343775511e-07, 2.13159879081104e-07, 1.07288553863327e-06, 9.92605464489316e-08),
    wscal     = c(0.294413775205612, 0.325790286064148, 0.887494623661041, 0.82906848192215, 0.962663650512695, 1), 
    chi       = c(0.629108071327209, 0.642833471298218, 0.639411568641663, 0.66897189617157, 0.674327433109283, 0.673606336116791),
    iwue      = c(9.066015627468e-05, 8.73051467351615e-05, 8.65905749378726e-05, 7.94415027485229e-05, 8.00566194811836e-05, 8.02328504505567e-05),
    rd        = c(0.0936944633722305, 0.121099025011063, 0.188513651490211, 0.146433308720589, 0.569870233535767, 0.0640662834048271),
    tsoil     = c(8.90927314758301, 11.0400791168213, 11.3561420440674, 15.1653509140015, 19.8312225341797, 9.67751979827881),
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083),
    wcont     = c(588.827575683594, 651.58056640625, 1774.9892578125, 1658.13696289062, 1925.32727050781, 2000), 
    snow      = c(0, 0, 0, 0, 0, 0),
    cond      = c(0, 0, 0, 0, 0, 0), 
    le_canopy = c(24608.6328125, 837521.75, 1706020.75, 110637.0078125, 5995345.5, -48291.109375),
    le_soil   = c(98347.3515625, 1303413.875, 2931274, 273886.84375, 5190479, -290766.28125), 
    dpsi      = c(0, 0, 0, 0, 0, 0),
    psi_leaf  = c(0, 0, 0, 0, 0, 0))
  
  # tibble(mod4) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref4 <- tibble(
    date      = as.Date(c("2007-01-01","2007-03-11","2010-04-15","2010-11-01","2012-06-24","2012-12-21")), 
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97),
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282),
    gpp       = c(2.47916746139526, 4.29396057128906, 6.37855243682861, 1.12339150905609, 4.18545007705688, 1.46236681938171),
    aet       = c(0.108937680721283, 1.35598421096802, 2.76325654983521, 0.32624351978302, 5.73224258422852, -0.3226118683815),
    le        = c(269530.59375, 3348879.5, 6819973, 803872.375, 13935005, -799917),
    pet       = c(0.108938276767731, 1.35599589347839, 2.76327228546143, 0.326252281665802, 5.732346534729, -0.322612106800079),
    vcmax     = c(6.111431048339e-06, 7.75509124650853e-06, 1.19958594950731e-05, 3.18529146170476e-06, 4.026747046737e-05, 4.28107478001039e-06),
    jmax      = c(2.28475473704748e-05, 2.86447157122893e-05, 4.33829118264839e-05, 5.85015914111864e-06, 7.40396353648975e-05, 1.724714456941e-05),
    vcmax25   = c(1.77550300577423e-05, 2.02352257474558e-05, 2.96198322757846e-05, 7.76460092311027e-06, 3.53926807292737e-05, 1.51183612615569e-05),
    jmax25    = c(5.0497124902904e-05, 5.73165089008398e-05, 8.36217368487269e-05, 1.04370537883369e-05, 6.87903666403145e-05, 4.33686545875389e-05),
    gs_accl   = c(0.124305546283722, 0.0675740614533424, 0.0994751155376434, 0.0197544004768133, 0.0170975066721439, 0.811915874481201),
    wscal     = c(0.627422451972961, 0.828165829181671, 0.963730275630951, 0.414474546909332, 0.442287534475327, 1),
    chi       = c(0.950858950614929, 0.843430578708649, 0.839075028896332, 0.857280552387238, 0.400212585926056, 0.99558699131012),
    iwue      = c(1.92191091628047e-05, 6.12344738328829e-05, 6.17910482105799e-05, 5.48005882592406e-05, 0.000235899657127447, 1.73565433669864e-06),
    rd        = c(0.0766474679112434, 0.101868033409119, 0.157453283667564, 0.0472486354410648, 0.579931199550629, 0.0540316849946976),
    tsoil     = c(8.95078659057617, 11.0359144210815, 11.3826942443848, 15.0886602401733, 19.9825744628906, 9.5286808013916),
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083),
    wcont     = c(158.737884521484, 209.525955200195, 243.823760986328, 104.862060546875, 111.898742675781, 253),
    snow      = c(0, 0, 0, 0, 0, 0),
    cond      = c(0, 0, 0, 0, 0, 0),
    le_canopy = c(171183.25, 2045465.5, 3888698.75, 529985.5625, 8744526, -509150.75),
    le_soil   = c(98347.3515625, 1303413.875, 2931274, 273886.84375, 5190479, -290766.28125),
    dpsi      = c(0.338661968708038, 1.0493232011795, 1.22259771823883, 0.456368923187256, 1.90744316577911, 0.0780341103672981),
    psi_leaf  = c(-0.624942302703857, -1.11408090591431, -1.22973167896271, -1.60701656341553, -2.70482659339905, -0.0780341103672981))
  expect_equal(dplyr::slice(tibble(mod1), c(1, 70, 1200, 1400, 2000, 2180)), ref1, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod2), c(1, 70, 1200, 1400, 2000, 2180)), ref2, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod3), c(1, 70, 1200, 1400, 2000, 2180)), ref3, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod4), c(1, 70, 1200, 1400, 2000, 2180)), ref4, tolerance = 1e-6)
  
  expect_equal(dplyr::slice(tibble(mod1_2ndTry), c(1, 70, 1200, 1400, 2000, 2180)), ref1, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod2_2ndTry), c(1, 70, 1200, 1400, 2000, 2180)), ref2, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod3_2ndTry), c(1, 70, 1200, 1400, 2000, 2180)), ref3, tolerance = 1e-6)
  expect_equal(dplyr::slice(tibble(mod4_2ndTry), c(1, 70, 1200, 1400, 2000, 2180)), ref4, tolerance = 1e-6)
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
