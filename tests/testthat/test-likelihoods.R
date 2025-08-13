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
  
  # also add error model for vcmax25
  test_params_pmodel <- dplyr::mutate(test_params_pmodel, err_vcmax25 = 0.5) 
  
  
  # Test rsofun::cost_likelihood_pmodel()
  ll_values <- apply(test_params_pmodel |> dplyr::select(-err_vcmax25), 1, function(par_v) { # par_v is a named vector
    # TODO: when rewriting cost_likelihood_pmodel: activate more difficult check to ignore unneded error parameter 'err_vcmax25'
    #       ll_values <- apply(test_params_pmodel, 1, function(par_v) {...})
    rsofun::cost_likelihood_pmodel(     # likelihood cost function from package
      par = par_v,                      # par: should be a named vector
      obs = rsofun::p_model_validation, # obs: example data from package
      drivers = rsofun::p_model_drivers,# drivers: example data from package
      targets = c('gpp'),
      par_fixed = NULL)
  })
  testthat::expect_equal(
    tolerance = 1e-4,
    object = ll_values, 
    # expected was generated with dput(ll_values)
    expected = c(
      -13706.5058738304,
      -4109.97422397591,
      -11148.2269071033,
      -2255167.68663634,
      -3846.02937529864)
  )
  
  # Test rsofun::cost_rmse_pmodel()
  rmse_values <- apply(dplyr::select(test_params_pmodel,-err_gpp, -err_vcmax25), 1, function(par_v) { # par_v is a named vector
    rsofun::cost_rmse_pmodel(
      par = par_v,                      # par: should be a named vector
      obs = rsofun::p_model_validation, # obs: example data from package
      drivers = rsofun::p_model_drivers,
      targets = c('gpp'),
      par_fixed = NULL
    )
  })
  testthat::expect_equal(
    tolerance = 1e-4,
    object = rmse_values, 
    # expected was generated with dput(rmse_values)
    expected = c(
      1.91661972744907, 
      2.02647969696678,
      8.19483248539096,
      14.0907280919599,
      1.38184787783589
    )
  )
  
  # Also test vcmax25 target and multi-target loglikelihoods:
  ll_values2 <- apply(dplyr::select(test_params_pmodel, -err_gpp), 1, function(par_v) { # par_v is a named vector
    # TODO: when rewriting cost_likelihood_pmodel: activate more difficult check to ignore unneded error parameter 'err_gpp'
    #       ll_values2 <- apply(test_params_pmodel, 1, function(par_v) {...})
    rsofun::cost_likelihood_pmodel(         # likelihood cost function from package
      par     = par_v,                      # par: should be a named vector
      obs     = p_model_validation_vcmax25, # obs: example data from package
      drivers = p_model_drivers_vcmax25,    # drivers: example data from package
      targets = c('vcmax25'))
  })
  ll_values3 <- apply(test_params_pmodel, 1, function(par_v) { # par_v is a named vector
    rsofun::cost_likelihood_pmodel(                                    # likelihood cost function from package
      par     = par_v,                                                 # par: should be a named vector
      obs     = rbind(p_model_validation, p_model_validation_vcmax25), # obs: example data from package 
      drivers = rbind(p_model_drivers, p_model_drivers_vcmax25),       # drivers: example data from package
      targets = c('gpp', 'vcmax25'))
  })
  
  testthat::expect_equal(ll_values3, ll_values + ll_values2) # loglikelihood of multiple targets is additive
  testthat::expect_equal(
    tolerance = 0.5, #tolerance = 1e-4,
    object = ll_values3, 
    # expected was generated with dput(ll_values3)
    expected = c(
      -13707.4063840446,
      -4110.8773900703,
      -11149.1301175005,
      -2255168.59027969,
      -3846.93254413504
    )
  )
  testthat::expect_equal(
    tolerance = 1e-4,
    object = ll_values2, 
    # expected was generated with dput(ll_values2)
    expected = c(
      -0.903165435731823,
      -0.903165445893757,
      -0.903165590656233,
      -0.903165645611412, 
      -0.90316542572855
    )
  )
  
  # test p-model likelihood with only fixed parameters
  ll_pmodel_fixed <- rsofun::cost_likelihood_pmodel(
    par     = c(),                                                   # par: should be a named vector
    obs     = rbind(p_model_validation, p_model_validation_vcmax25), # obs: example data from package 
    drivers = rbind(p_model_drivers, p_model_drivers_vcmax25),       # drivers: example data from package
    
    # additional arguments for the cost function
    par_fixed = c(         # fix parameter value from previous calibration
      kc_jmax            = 0.8,
      kphio              = 0.041,
      kphio_par_a        = 0.0,
      kphio_par_b        = 16,
      soilm_thetastar    = 0.6 * 240,  # to recover paper setup with soil moisture stress
      soilm_betao        = 0.0,
      beta_unitcostratio = 146.0,
      rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
      tau_acclim         = 30.0,
      err_gpp = 0.2, err_vcmax25 = 3.0
    ), 
    targets = c('gpp', 'vcmax25')
  )
  testthat::expect_equal(tolerance = 0.5, #tolerance = 1e-4,
                         object = ll_pmodel_fixed,
                         # expected was generated with dput(ll_pmodel_fixed)
                         expected = -336583.32327482)
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


