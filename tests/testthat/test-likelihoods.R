set.seed(10)

test_that("test likelihood/RMSE calculations with pmodel", {
  
  test_params_pmodel <- data.frame( # test_params_pmodel was generated with dput(test_params_pmodel)
    kphio              = c(0.04998, 0.101645649550483, 0.142234791386873,0.136563287638128, 0.0529878485854715), 
    kphio_par_a        = c(0.01   , -0.00398199869785458, -0.00144068546174094, -0.00302413401333615, -0.00134308318863623), 
    kphio_par_b        = c(1.0    , 15.59719867073, 14.5120448060334, 20.2669072570279, 18.2723819604144), 
    soilm_thetastar    = c(0.6*240, 21.4685604907572, 129.50339589268, 36.7124842107296, 209.40362457186), 
    soilm_betao        = c(0.01   , 0.508695727679878, 0.69720912235789, 0.306312796194106, 0.546225873986259), 
    beta_unitcostratio = c(146.0  , 109.196580422577, 113.911265798379, 146.538958174642, 114.256114442833), 
    rd_to_vcmax        = c(0.014  , 0.0468210919597186, 0.0725045789754949, 0.0594035412720405, 0.0491376937157475), 
    tau_acclim         = c(30.0   , 53.9236626827624, 26.6468327937182, 34.3328710102942, 52.5165054232348), 
    kc_jmax            = c(0.41   , 0.624640251602978, 0.492042974522337, 0.23459618492052, 0.502756303641945), 
    err_gpp            = c(0.5    , 3.13616614692146, 2.82713630301412, 0.282233132501133, 3.00473784686066))
  # test_params_pmodel was created with: par_cal_best <- c(
  # test_params_pmodel was created with:   kphio              = 0.09423773,
  # test_params_pmodel was created with:   kphio_par_a        = -0.0025,
  # test_params_pmodel was created with:   kphio_par_b        = 20,
  # test_params_pmodel was created with:   soilm_thetastar    = 0.6*240,
  # test_params_pmodel was created with:   soilm_betao        = 0.2,
  # test_params_pmodel was created with:   beta_unitcostratio = 146.0,
  # test_params_pmodel was created with:   rd_to_vcmax        = 0.014,
  # test_params_pmodel was created with:   tau_acclim         = 30.0,
  # test_params_pmodel was created with:   kc_jmax            = 0.41,
  # test_params_pmodel was created with:   err_gpp          = 1
  # test_params_pmodel was created with: )
  # test_params_pmodel was created with: par_cal_min <- c(
  # test_params_pmodel was created with:   kphio              = 0.03,
  # test_params_pmodel was created with:   kphio_par_a        = -0.004,
  # test_params_pmodel was created with:   kphio_par_b        = 10,
  # test_params_pmodel was created with:   soilm_thetastar    = 0,
  # test_params_pmodel was created with:   soilm_betao        = 0,
  # test_params_pmodel was created with:   beta_unitcostratio = 50.0,
  # test_params_pmodel was created with:   rd_to_vcmax        = 0.01,
  # test_params_pmodel was created with:   tau_acclim         = 7.0,
  # test_params_pmodel was created with:   kc_jmax            = 0.2,
  # test_params_pmodel was created with:   err_gpp          = 0.01
  # test_params_pmodel was created with: )
  # test_params_pmodel was created with: par_cal_max <- c(
  # test_params_pmodel was created with:   kphio              = 0.15,
  # test_params_pmodel was created with:   kphio_par_a        = -0.001,
  # test_params_pmodel was created with:   kphio_par_b        = 30,
  # test_params_pmodel was created with:   soilm_thetastar    = 240,
  # test_params_pmodel was created with:   soilm_betao        = 1,
  # test_params_pmodel was created with:   beta_unitcostratio = 200.0,
  # test_params_pmodel was created with:   rd_to_vcmax        = 0.1,
  # test_params_pmodel was created with:   tau_acclim         = 60.0,
  # test_params_pmodel was created with:   kc_jmax            = 0.8,
  # test_params_pmodel was created with:   err_gpp          = 4
  # test_params_pmodel was created with: )
  # test_params_pmodel was created with: library(BayesianTools) # for prior sampling
  # test_params_pmodel was created with: prior <- createUniformPrior(lower = par_cal_min, upper = par_cal_max, best = par_cal_best)
  # test_params_pmodel was created with: test_params_pmodel <- prior$sampler(4) |> as.data.frame() |>
  # test_params_pmodel was created with:   stats::setNames(nm = names(par_cal_min))
  # test_params_pmodel was created with: test_params_pmodel <- bind_rows(par_cal_best, test_params_pmodel)
  
  # also add error model for bigDelta13C and le
  test_params_pmodel <- dplyr::mutate(test_params_pmodel, err_bigD13C = 5, err_le = 100000) 
  
  
  # Test rsofun::cost_likelihood_pmodel()
  ll_values <- apply(test_params_pmodel, 1, function(par_v) { # par_v is a named vector
    rsofun::cost_likelihood_pmodel(     # likelihood cost function from package
      par     = as.list(par_v),                      # par: should be a named list
      obs     = rsofun::pmodel_validation |> filter(sitename == "FR-Pue"), # obs: example data from package
      drivers = rsofun::pmodel_drivers |> filter(sitename == "FR-Pue"),# drivers: example data from package
      par_fixed = NULL)
  })
  testthat::expect_equal(
    tolerance = 1e-4,
    object = ll_values, 
    # expected was generated with dput(ll_values)
    expected = c(
      -10469.4336971745,
      -4471.57101486946,
      -12060.6843920937,
      -2692922.28725325,
      -4064.88443627102)
  )
  
  # NOTE: this can be removed when legacy cost_likelihood_pmodel_legacy is removed/upated
  ll_values_legacy <- apply(test_params_pmodel |> dplyr::select(-err_bigD13C, -err_le), 1, function(par_v) { # par_v is a named vector
    rsofun::cost_likelihood_pmodel_legacy(     # likelihood cost function from package
      par     = par_v,                  # par: should be a named vector (for legacy)
      obs     = rsofun::pmodel_validation |> filter(sitename == "FR-Pue") |> dplyr::select(-targets), # obs: example data from package
      drivers = rsofun::pmodel_drivers |> filter(sitename == "FR-Pue"),# drivers: example data from package
      targets = c('gpp'), # this uses legacy arguments
      par_fixed = NULL)
  })
  testthat::expect_equal(ll_values, ll_values_legacy)
  
  
  # Test rsofun::cost_rmse_pmodel()
  rmse_values <- apply(dplyr::select(test_params_pmodel,-err_gpp, -err_bigD13C, -err_le), 1, function(par_v) { # par_v is a named vector
    rsofun::cost_rmse_pmodel(
      par = par_v,                      # par: should be a named vector
      obs = rsofun::pmodel_validation |> filter(sitename == "FR-Pue"), # obs: example data from package
      drivers = rsofun::pmodel_drivers |> filter(sitename == "FR-Pue"),# drivers: example data from package
      par_fixed = NULL
    )
  })
  testthat::expect_equal(
    tolerance = 1e-4,
    object = rmse_values, 
    # expected was generated with dput(rmse_values)
    expected = c(
      1.61488424553516,
      2.27090370750813,
      8.30075735547608,
      14.9343407605068,
      1.30002711468356
    )
  )
  
  
  
  # Also test D13C and LE targets and multi-target loglikelihoods:
  obs     <- pmodel_validation |> dplyr::filter(sitename %in% c("CH-Dav","lon_+010.52_lat_+051.08"))
  drivers <- pmodel_drivers    |> dplyr::filter(sitename %in% c("CH-Dav","lon_+010.52_lat_+051.08"))
  
  obs_D13C <- obs |> mutate(targets = lapply(targets, \(x) setdiff(x, c("gpp","le"))))      # remove gpp and le
  obs_GPP  <- obs |> mutate(targets = lapply(targets, \(x) setdiff(x, c("le","bigD13C"))))  # remove le and D13C
  obs_LE   <- obs |> mutate(targets = lapply(targets, \(x) setdiff(x, c("gpp","bigD13C")))) # remove gpp and D13C
  
  # D13C only
  ll_values2 <- apply(dplyr::select(test_params_pmodel, -err_gpp, -err_le), 1, function(par_v) { # par_v is a named vector
    rsofun::cost_likelihood_pmodel(         # likelihood cost function from package
      par     = as.list(par_v),
      obs     = obs_D13C,
      drivers = drivers,
    )
  })
  # LE only
  ll_values3 <- apply(dplyr::select(test_params_pmodel, -err_bigD13C, -err_gpp), 1, function(par_v) {
    rsofun::cost_likelihood_pmodel(
      par     = as.list(par_v),
      obs     = obs_LE,
      drivers = drivers,
    )
  })
  # GPP only
  ll_values4 <- apply(dplyr::select(test_params_pmodel, -err_bigD13C, -err_le), 1, function(par_v) {
    rsofun::cost_likelihood_pmodel(
      par     = as.list(par_v),
      obs     = obs_GPP,
      drivers = drivers,
    )
  })
  # All together
  ll_values_all <- apply(test_params_pmodel, 1, function(par_v) {
    rsofun::cost_likelihood_pmodel(
      par     = as.list(par_v),
      obs     = obs,
      drivers = drivers,
    )
  })
  
  testthat::expect_equal(ll_values_all, ll_values2 + ll_values3 + ll_values4) # loglikelihood of multiple targets is additive
  testthat::expect_equal(
    tolerance = 0.5, #tolerance = 1e-4,
    object = ll_values_all, 
    # expected was generated with dput(ll_values_all)
    expected = c(
      -7604094.26694711, 
      -7597509.03269559, 
      -7599477.68138934, 
      -8019161.82496339, 
      -7597376.3915261
    )
  )
  testthat::expect_equal(
    tolerance = 0.5, #tolerance = 1e-4,
    object = ll_values4, 
    # expected was generated with dput(ll_values4)
    expected = c(
      -8400.06603907821, 
      -1814.84759984799, 
      -3783.50251326301, 
      -423467.623009604, 
      -1682.21297953174
    )
  )
  testthat::expect_equal(
    tolerance = 0.5, #tolerance = 1e-4,
    object = ll_values3, 
    # expected was generated with dput(ll_values3)
    expected = c(
      -7595676.40909368, 
      -7595676.40909368, 
      -7595676.40909368, 
      -7595676.40909368, 
      -7595676.40909368
    )
  )
  testthat::expect_equal(
    tolerance = 1e-4,
    object = ll_values2, 
    # expected was generated with dput(ll_values2)
    expected = c(
      -17.7918143528227, 
      -17.7760020628653, 
      -17.7697824014869, 
      -17.7928601079711, 
      -17.7694528869911
    )
  )
  
  # test p-model likelihood with only fixed parameters (for GPP, and ET and bigD13C)
  par <- list() # no estimatable parameters
  obs     <- pmodel_validation |> dplyr::filter(sitename %in% c("CH-Dav","lon_+010.52_lat_+051.08"))
  drivers <- pmodel_drivers    |> dplyr::filter(sitename %in% c("CH-Dav","lon_+010.52_lat_+051.08"))
  par_fixed<- list(
    kphio       = 0.05,
    kphio_par_a = -0.01,
    kphio_par_b = 1,
    soilm_thetastar    = 0.6 * 240,  # old setup with soil moisture stress
    soilm_betao        = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41,
    # error model parameters
    err_gpp     = 2,
    err_le      = 2e8,
    err_bigD13C = 4
  )
  par_D13C <- par[-c(4,5)]; obs_D13C <- obs |> mutate(targets = lapply(targets, \(x) setdiff(x, c("gpp","le"))))      # remove gpp and le
  par_GPP  <- par[-c(5,6)]; obs_GPP  <- obs |> mutate(targets = lapply(targets, \(x) setdiff(x, c("le","bigD13C"))))  # remove le and D13C
  par_LE   <- par[-c(4,6)]; obs_LE   <- obs |> mutate(targets = lapply(targets, \(x) setdiff(x, c("gpp","bigD13C")))) # remove gpp and D13C
  
  ll_all  <- cost_likelihood_pmodel(par,obs,drivers,par_fixed)
  ll_D13C <- cost_likelihood_pmodel(par_D13C, obs_D13C, drivers,par_fixed)
  ll_GPP  <- cost_likelihood_pmodel(par_GPP,  obs_GPP,  drivers,par_fixed)
  ll_LE   <- cost_likelihood_pmodel(par_LE,   obs_LE,   drivers,par_fixed)
  
  testthat::expect_equal(ll_all, ll_D13C + ll_GPP + ll_LE)
})

test_that("test likelihood/RMSE calculations with BiomeE", {
  test_params_BiomeE <- data.frame( # test_params_BiomeE was generated with dput(test_params_BiomeE)
    phiRL     = c(6.59158648136072, 2.41828079945408, 4.51794087081216, 0.323927985038608), 
    LAI_light = c(4.83413460890297, 4.89137732107192, 6.25084221335128, 1.65691818702035), 
    tf_base   = c(0.986252965405583, 1.52580757206306, 0.278885046485811, 0.125027264398523), 
    par_mort  = c(1.64211843877565, 0.579043845250271, 1.28934027748182, 1.11228716920596), 
    err_GPP   = c(2.9679689736967, 3.70911861001514, 1.16307689385489, 0.195016647893935)
  ) # TODO: in BiomeE output is uppercase GPP, but in p-model it is lowercase
  
  # test_params_BiomeE was created with:  Test cost_likelihood_biomee()
  # test_params_BiomeE was created with:  parBiomeE_cal_best <- c(
  # test_params_BiomeE was created with:    phiRL              = 3.5,
  # test_params_BiomeE was created with:    LAI_light          = 3.5,
  # test_params_BiomeE was created with:    tf_base            = 1,
  # test_params_BiomeE was created with:    par_mort           = 1,
  # test_params_BiomeE was created with:    err_GPP          = 1
  # test_params_BiomeE was created with:  )
  # test_params_BiomeE was created with:  parBiomeE_cal_min <- c(
  # test_params_BiomeE was created with:    phiRL              = 0.1,
  # test_params_BiomeE was created with:    LAI_light          = 0.1,
  # test_params_BiomeE was created with:    tf_base            = 0.1,
  # test_params_BiomeE was created with:    par_mort           = 0.1,
  # test_params_BiomeE was created with:    err_GPP          = 0.01
  # test_params_BiomeE was created with:  )
  # test_params_BiomeE was created with:  parBiomeE_cal_max <- c(
  # test_params_BiomeE was created with:    phiRL              = 7.0,
  # test_params_BiomeE was created with:    LAI_light          = 7.0,
  # test_params_BiomeE was created with:    tf_base            = 2.0,
  # test_params_BiomeE was created with:    par_mort           = 2.0,
  # test_params_BiomeE was created with:    err_GPP          = 4
  # test_params_BiomeE was created with:  )
  # test_params_BiomeE was created with:  prior_BiomeE <- createUniformPrior(lower = parBiomeE_cal_min, upper = parBiomeE_cal_max, best = parBiomeE_cal_best)
  # test_params_BiomeE was created with:  test_params_BiomeE <- prior_BiomeE$sampler(4) |> as.data.frame() |>
  # test_params_BiomeE was created with:    stats::setNames(nm = names(par_cal_min))
  
  # Test rsofun::cost_likelihood_biomee()
  ll_values_BiomeE <- apply(test_params_BiomeE, 1, function(par_v) { # par_v is a named vector
    rsofun::cost_likelihood_biomee(    # likelihood cost function from package
      par = par_v,                     # par: should be a named vector
      obs = rsofun::biomee_validation, # obs: example data from package
      drivers = rsofun::biomee_p_model_drivers,
      targets = c('GPP')) # TODO: in BiomeE output is uppercase GPP, but in p-model it is lowercase
  })
  testthat::expect_equal(
    tolerance = 1e-4, 
    object = ll_values_BiomeE, 
    # expected was generated with dput(ll_values_BiomeE)
    expected = c(
      -2.0202778978475,
      -2.23976808695674,
      -1.24193195159227,
      -0.369097633033684
    )
  )
  
  
  # Test rsofun::cost_rmse_biomee()
  relError_values_BiomeE <- apply(dplyr::select(test_params_BiomeE, -err_GPP), 1, function(par_v) { # par_v is a named vector
    rsofun::cost_rmse_biomee(          # cost function for RMSE (actually relative error) from package
      par = par_v,                     # par: should be a named vector
      obs = rsofun::biomee_validation, # obs: example data from package
      drivers = rsofun::biomee_p_model_drivers)
  })
  testthat::expect_equal(
    tolerance = 1e-4,
    object = relError_values_BiomeE,
    # expected was generated with dput(relError_values_BiomeE)
    # NOTE: these errors are relative Errors, not RMSE:
    expected = c(
      0.42976158393698,
      0.168135096963972,
      0.152359273844623,
      0.316205527799935
    )
  )
})


