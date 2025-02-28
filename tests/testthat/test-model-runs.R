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
    
    phydro_K_plant     = 5e-17,   # TODO: add documentaiton: Phydro: Plant conductivity                
    phydro_p50_plant   = -0.46,   # TODO: add documentaiton: Phydro: Plant P50               
    phydro_b_plant     = 1,       # TODO: add documentaiton: Phydro: shape parameter of vulnerability curve           
    phydro_alpha       = 0.08,    # TODO: add documentaiton: Phydro: Cost of Jmax              
    phydro_gamma       = 0.065,   # TODO: add documentaiton: Phydro: Cost of hydraulics               
    bsoil              = 3,       # TODO: add documentaiton: Phydro: parameter converting RZWSC to predawn water potential (depends on rooting system hence PFT specific)           
    Ssoil              = 113      # TODO: add documentaiton: Phydro: parameter converting RZWSC to predawn water potential (depends on rooting system hence PFT specific)            
  )

  # read in demo data
  df_drivers <- rsofun::p_model_drivers_format2024_08 |> rowwise() |> mutate(forcing = list(mutate(forcing, vwind=2.0))) |> ungroup()
  
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
  
  # tibble(mod2) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref2 <- tibble(
    date      = structure(c(13514, 13583, 14714, 14914, 15515, 15695), class = "Date"), 
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97), 
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282), 
    gpp       = c(1.65618813037872, 6.02679443359375, 6.72385692596436, 1.84405922889709, 9.40890026092529, 0.896598398685455), 
    aet       = c(0.0831360220909119, 1.46265971660614, 1.86834669113159, 0.240892946720123, 5.62481117248535, -0.0911358147859573), 
    le        = c(205692.84375, 3612336.25, 4611252, 593566.4375, 13673840, -225971.515625),     
    pet       = c(0.103817254304886, 1.45484209060669, 3.08336925506592,     0.335356384515762, 6.51526165008545, -0.377504140138626),     
    vcmax     = c(9.54577535594581e-06, 1.18804200610612e-05, 1.91590133908903e-05,     1.37124443426728e-05, 5.95575438637752e-05, 5.93948425375856e-06    ), 
    jmax      = c(3.08439557556994e-05, 3.66509229934309e-05, 5.80160958634224e-05,     3.691642996273e-05, 0.000109297579911072, 2.01222374016652e-05    ), 
    vcmax25   = c(3.51696653524414e-05, 3.77493124688044e-05,     5.76805359742139e-05, 3.59945297532249e-05, 5.17318439960945e-05,     2.60039796557976e-05), 
    jmax25    = c(7.8623415902257e-05, 8.34658203530125e-05,     0.000126882849144749, 7.2937982622534e-05, 9.94235306279734e-05,     5.87244903726969e-05), 
    gs_accl   = c(0.0016942253569141, 0.00173744710627943,     0.00175592955201864, 0.00170137174427509, 0.00156046624761075,     0.00193470600061119), 
    wscal     = c(0.104094244539738, 0.129727467894554,     0.587451696395874, 0.497713387012482, 0.780628979206085,     0.86269211769104), 
    chi       = c(0.629108071327209, 0.642833471298218,     0.639411568641663, 0.66897189617157, 0.674327433109283, 0.673606336116791    ), 
    iwue      = c(9.066015627468e-05, 8.73051467351615e-05, 8.65905749378726e-05,     7.94415027485229e-05, 8.00566194811836e-05, 8.02328504505567e-05    ), 
    rd        = c(0.0936944633722305, 0.121099025011063, 0.188513651490211,     0.146433308720589, 0.569870233535767, 0.0640662834048271),     
    tsoil     = c(9.02208423614502, 11.0288057327271, 11.3866157531738,     15.0849714279175, 19.9336929321289, 9.59065818786621), 
    netrad    = c(4.16539621353149,     55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359,     -16.011812210083), 
    wcont     = c(208.188491821289, 259.454925537109,     1174.90344238281, 995.4267578125, 1561.25793457031, 1725.38427734375    ), 
    snow      = c(0, 0, 0, 0, 0, 0), 
    cond      = c(0, 0, 0, 0, 0, 0),     
    le_canopy = c(127639.390625, 2577880.75, 2284844.5, 376195.9375,     9554412, 4795.3779296875), 
    le_soil   = c(78053.453125, 1034455.5625,     2326407.75, 217370.515625, 4119428, -230766.890625), 
    dpsi      = c(0,     0, 0, 0, 0, 0), 
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
    wscal     = c(0.0814638137817383, 0.108003400266171, 0.461629569530487, 0.326345980167389, 0.524366974830627, 0.587543725967407), 
    chi       = c(0.629108071327209, 0.642833471298218, 0.639411568641663, 0.66897189617157, 0.674327433109283, 0.673606336116791), 
    iwue      = c(9.066015627468e-05, 8.73051467351615e-05, 8.65905749378726e-05, 7.94415027485229e-05, 8.00566194811836e-05, 8.02328504505567e-05), 
    rd        = c(0.0936944633722305, 0.121099025011063, 0.188513651490211, 0.146433308720589, 0.569870233535767, 0.0640662834048271), 
    tsoil     = c(9.16451072692871, 11.0153512954712, 11.3974561691284, 15.0535011291504, 20.0137233734131, 9.49603080749512), 
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083), 
    wcont     = c(162.927627563477, 216.006805419922, 923.259155273438, 652.691955566406, 1048.73400878906, 1175.08740234375), 
    snow      = c(0, 0, 0, 0, 0, 0), 
    cond      = c(0, 0, 0, 0, 0, 0), 
    le_canopy = c(170769.65625, 2040949.625, 3880380.25, 528897.8125, 8732491, -507988.21875), 
    le_soil   = c(78053.453125, 1034455.5625, 2326407.75, 217370.515625, 4119428, -230766.890625), 
    dpsi      = c(0, 0, 0, 0, 0, 0), 
    psi_leaf  = c(0, 0, 0, 0, 0, 0)
  )
  
  # tibble(mod4) |> slice(c(1, 70, 1200, 1400, 2000, 2180)) |> dput()
  ref4 <- tibble(
    date      = structure(c(13514, 13583, 14714, 14914, 15515, 15695), class = "Date"), 
    year_dec  = c(2007, 2007.189, 2010.285, 2010.833, 2012.478, 2012.97), 
    fapar     = c(0.617119550704956, 0.637238144874573, 0.614814937114716, 0.668549001216888, 0.672287464141846, 0.689359784126282), 
    gpp       = c(2.4825496673584, 4.29858160018921, 6.37876176834106, 1.16307175159454, 6.1460018157959, 1.46240973472595), 
    aet       = c(0.100733757019043, 1.24704611301422, 2.51813817024231, 0.303279280662537, 5.29152059555054, -0.29841285943985), 
    le        = c(249232.671875, 3079834.5, 6214997, 747287.9375, 12863615, -739915.5), 
    pet       = c(0.103817254304886, 1.45484209060669, 3.08336925506592, 0.335356384515762, 6.51526165008545, -0.377504140138626), 
    vcmax     = c(6.10240022069775e-06, 7.75274838815676e-06, 1.19952146633295e-05, 3.3023632113327e-06, 4.0464994526701e-05, 4.28065595770022e-06), 
    jmax      = c(2.29531615332235e-05, 2.86642443825258e-05, 4.33885979873594e-05, 6.07609081271221e-06, 7.67164674471132e-05, 1.72506606759271e-05), 
    vcmax25   = c(1.77287929545855e-05, 2.02291139430599e-05, 2.96182388410671e-05, 8.04998035164317e-06, 3.55662923539057e-05, 1.51168824231718e-05), 
    jmax25    = c(5.07305485371035e-05, 5.73555880691856e-05, 8.36326944408938e-05, 1.08401309262263e-05, 7.12774199200794e-05, 4.33774948760401e-05), 
    gs_accl   = c(0.127987444400787, 0.0686747059226036, 0.0995644107460976, 0.0210667010396719, 0.0324819944798946, 0.811927676200867), 
    wscal     = c(0.642368197441101, 0.852013349533081, 0.966963946819305, 0.419518351554871, 0.556477844715118, 1), 
    chi       = c(0.952207505702972, 0.845774114131927, 0.839214026927948, 0.861443877220154, 0.536405801773071, 0.995586931705475), 
    iwue      = c(1.86916877282783e-05, 6.03179141762666e-05, 6.17376645095646e-05, 5.3201994887786e-05, 0.000182334129931405, 1.73568002992397e-06), 
    rd        = c(0.076534204185009, 0.101837269961834, 0.157444804906845, 0.0489852055907249, 0.582775950431824, 0.0540263988077641), 
    tsoil     = c(8.96044540405273, 11.0349359512329, 11.3815355300903, 15.0929355621338, 19.9768238067627, 9.54089260101318), 
    netrad    = c(4.16539621353149, 55.9189796447754, 116.783561706543, 12.2599382400513, 192.525726318359, -16.011812210083), 
    wcont     = c(162.519149780273, 215.559371948242, 244.641876220703, 106.138145446777, 140.78889465332, 253), 
    snow      = c(0, 0, 0, 0, 0, 0), 
    cond      = c(0, 0, 0, 0, 0, 0), 
    le_canopy = c(171179.21875, 2045379, 3888589.5, 529917.4375, 8744187, -509148.625), 
    le_soil   = c(78053.453125, 1034455.5625, 2326407.75, 217370.515625, 4119428, -230766.890625), 
    dpsi      = c(0.334371000528336, 1.04337632656097, 1.22221386432648, 0.45578670501709, 1.94676566123962, 0.0780353099107742), 
    psi_leaf  = c(-0.594782650470734, -1.09586262702942, -1.22868001461029, -1.56316161155701, -2.32053685188293, -0.0780353099107742)
  )

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
    kc_jmax            = 0.41
  )
  
  # read in demo data
  df_drivers <- rsofun::p_model_drivers_format2024_08 # TODO: NOT YET UPDATED FOR PHYDRO (still add default phydro_* parameters)
  for (it in seq_along((df_drivers$forcing))){df_drivers$forcing[[it]]$vwind = 2.0} # TODO: add vwind, TODO: this would need to be changed in rsofun example data
  
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
  for (it in seq_along((df_drivers$forcing))){df_drivers$forcing[[it]]$vwind = 2.0} # TODO: add vwind, TODO: this would need to be changed in rsofun example data

  # run the SOFUN Fortran P-model
  mod <- rsofun::run_pmodel_f_bysite( 
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
  df_drivers <- rsofun::p_model_drivers_format2024_08 # TODO: NOT YET UPDATED FOR PHYDRO (still add default phydro_* parameters)
  df_drivers$params_siml[[1]]$use_gs     <- TRUE
  for (it in seq_along((df_drivers$forcing))){df_drivers$forcing[[it]]$vwind = 2.0} # TODO: add vwind, TODO: this would need to be changed in rsofun example data
  
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
