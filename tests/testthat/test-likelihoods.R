context("test P-model and BiomeE likelihood frameworks")
set.seed(10)

test_that("test likelihood calculations", {
  library(BayesianTools)
  # par_cal_best <- c(
  #   kphio              = 0.09423773,
  #   kphio_par_a        = -0.0025,
  #   kphio_par_b        = 20,
  #   soilm_thetastar    = 0.6*240,
  #   soilm_betao        = 0.2,
  #   beta_unitcostratio = 146.0,
  #   rd_to_vcmax        = 0.014,
  #   tau_acclim         = 30.0,
  #   kc_jmax            = 0.41,
  #   err_gpp          = 1
  # )
  # par_cal_min <- c(
  #   kphio              = 0.03,
  #   kphio_par_a        = -0.004,
  #   kphio_par_b        = 10,
  #   soilm_thetastar    = 0,
  #   soilm_betao        = 0,
  #   beta_unitcostratio = 50.0,
  #   rd_to_vcmax        = 0.01,
  #   tau_acclim         = 7.0,
  #   kc_jmax            = 0.2,
  #   err_gpp          = 0.01
  # )
  # par_cal_max <- c(
  #   kphio              = 0.15,
  #   kphio_par_a        = -0.001,
  #   kphio_par_b        = 30,
  #   soilm_thetastar    = 240,
  #   soilm_betao        = 1,
  #   beta_unitcostratio = 200.0,
  #   rd_to_vcmax        = 0.1,
  #   tau_acclim         = 60.0,
  #   kc_jmax            = 0.8,
  #   err_gpp          = 4
  # )
  # prior <- createUniformPrior(lower = par_cal_min, upper = par_cal_max, best = par_cal_best)
  # test_params_pmodel <- prior$sampler(4) |> as.data.frame() |>
  #   stats::setNames(nm = names(par_cal_min))
  test_params_pmodel <- data.frame( # test_params_pmodel were generated with dput(test_params_pmodel)
    kphio              = c(0.101645649550483, 0.142234791386873,0.136563287638128, 0.0529878485854715), 
    kphio_par_a        = c(-0.00398199869785458, -0.00144068546174094, -0.00302413401333615, -0.00134308318863623), 
    kphio_par_b        = c(15.59719867073, 14.5120448060334, 20.2669072570279, 18.2723819604144), 
    soilm_thetastar    = c(21.4685604907572, 129.50339589268, 36.7124842107296, 209.40362457186), 
    soilm_betao        = c(0.508695727679878, 0.69720912235789, 0.306312796194106, 0.546225873986259), 
    beta_unitcostratio = c(109.196580422577, 113.911265798379, 146.538958174642, 114.256114442833), 
    rd_to_vcmax        = c(0.0468210919597186, 0.0725045789754949, 0.0594035412720405, 0.0491376937157475), 
    tau_acclim         = c(53.9236626827624, 26.6468327937182, 34.3328710102942, 52.5165054232348), 
    kc_jmax            = c(0.624640251602978, 0.492042974522337, 0.23459618492052, 0.502756303641945), 
    err_gpp          = c(3.13616614692146, 2.82713630301412, 0.282233132501133, 3.00473784686066))
  test_params_pmodel <- mutate(test_params_pmodel, err_vcmax25 = 0.5) # also add error model for vcmax25
  
  # Test rsofun::cost_likelihood_pmodel
  ll_values <- apply(test_params_pmodel, 1, function(par_v) { # par_v is a vector
    rsofun::cost_likelihood_pmodel(     # likelihood cost function from package
      par = as.list(par_v),                      # must be named
      obs = rsofun::p_model_validation, # example data from package
      drivers = rsofun::p_model_drivers,
      targets = c('gpp'))
  })
  testthat::expect_equal(object = ll_values, 
                         # expected was generated with dput(ll_values)
                         expected = c(-4109.97422462441,
                                      -11148.2269519099,
                                      -2255167.68711405,
                                      -3846.02937870932))
  # Test rsofun::cost_rmse_pmodel()
  rmse_values <- apply(dplyr::select(test_params_pmodel,-err_gpp, -err_vcmax25), 1, function(par_v) { # par_v is a vector
    rsofun::cost_rmse_pmodel(
      par = as.list(par_v),  # kphio related parameters
      obs = rsofun::p_model_validation,
      drivers = rsofun::p_model_drivers,
      targets = c('gpp'),
      par_fixed = list()
    )
  })
  testthat::expect_equal(object = rmse_values, 
                         # expected was generated with dput(rmse_values)
                         expected = c(2.02647969696678,
                                      8.19483248539096,
                                      14.0907280919599,
                                      1.38184787783589))
  
  # Also test vcmax25 target and multi-target loglikelihoods:
  ll_values2 <- apply(test_params_pmodel, 1, function(par_v) { # par_v is a vector
    rsofun::cost_likelihood_pmodel(     # likelihood cost function from package
      par     = as.list(par_v),
      obs     = p_model_validation_vcmax25, # example data from package 
      drivers = p_model_drivers_vcmax25,       # example data from package
      targets = c('vcmax25'))
  })
  ll_values3 <- apply(test_params_pmodel, 1, function(par_v) { # par_v is a vector
    rsofun::cost_likelihood_pmodel(     # likelihood cost function from package
      par     = as.list(par_v),
      obs     = rbind(p_model_validation, p_model_validation_vcmax25), # example data from package 
      drivers = rbind(p_model_drivers, p_model_drivers_vcmax25),       # example data from package
      targets = c('gpp', 'vcmax25'))
  })
  testthat::expect_equal(ll_values3, ll_values + ll_values2) # loglikelihood of multiple targets is additive
  testthat::expect_equal(object = ll_values3, 
                         # expected was generated with dput(ll_values3)
                         expected = c(-4110.8773900703,
                                      -11149.1301175005,
                                      -2255168.59027969,
                                      -3846.93254413504))
  testthat::expect_equal(object = ll_values2, 
                         # expected was generated with dput(ll_values2)
                         expected = c(-0.903165445893757,
                                      -0.903165590656233,
                                      -0.903165645611412, 
                                      -0.90316542572855))
  
  # test p-model likelihood with only fixed parameters
  ll_pmodel_fixed <- rsofun::cost_likelihood_pmodel(
    obs     = rbind(p_model_validation, p_model_validation_vcmax25), # example data from package 
    drivers = rbind(p_model_drivers, p_model_drivers_vcmax25),       # example data from package
    par = c(),
    # arguments for the cost function
    par_fixed = list(         # fix parameter value from previous calibration
      kc_jmax = 0.8,
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
  testthat::expect_equal(object = ll_pmodel_fixed,
                         # expected was generated with dput(ll_pmodel_fixed)
                         expected = -336583.32327482)
  
  
  # Test rsofun::cost_likelihood_biomee()
  # parBiomeE_cal_best <- c(
  #   phiRL              = 3.5,
  #   LAI_light          = 3.5,
  #   tf_base            = 1,
  #   par_mort           = 1,
  #   err_GPP          = 1
  # )
  # parBiomeE_cal_min <- c(
  #   phiRL              = 0.1,
  #   LAI_light          = 0.1,
  #   tf_base            = 0.1,
  #   par_mort           = 0.1,
  #   err_GPP          = 0.01
  # )
  # parBiomeE_cal_max <- c(
  #   phiRL              = 7.0,
  #   LAI_light          = 7.0,
  #   tf_base            = 2.0,
  #   par_mort           = 2.0,
  #   err_GPP          = 4
  # )
  # prior_BiomeE <- createUniformPrior(lower = parBiomeE_cal_min, upper = parBiomeE_cal_max, best = parBiomeE_cal_best)
  # test_params_BiomeE <- prior_BiomeE$sampler(4) |> as.data.frame() |>
  #   stats::setNames(nm = names(par_cal_min))
  test_params_BiomeE <- data.frame( # test_params_BiomeE were generated with dput(test_params_BiomeE)
    phiRL = c(6.59158648136072, 2.41828079945408, 4.51794087081216, 0.323927985038608), 
    LAI_light = c(4.83413460890297, 4.89137732107192, 6.25084221335128, 1.65691818702035), 
    tf_base = c(0.986252965405583, 1.52580757206306, 0.278885046485811, 0.125027264398523), 
    par_mort = c(1.64211843877565, 0.579043845250271, 1.28934027748182, 1.11228716920596), 
    err_GPP = c(2.9679689736967, 3.70911861001514, 1.16307689385489, 0.195016647893935)) # TODO: in BiomeE output is uppercase GPP, but in p-model it is lowercase
  ll_values_BiomeE <- apply(test_params_BiomeE, 1, function(par_v) { # par_v is a vector
    rsofun::cost_likelihood_biomee(    # likelihood cost function from package
      par = par_v,                     # must be named
      obs = rsofun::biomee_validation, # example data from package
      drivers = rsofun::biomee_gs_leuning_drivers,
      targets = c('GPP')) # TODO: in BiomeE output is uppercase GPP, but in p-model it is lowercase
  })
  testthat::expect_equal(object = ll_values_BiomeE, 
                         # expected was generated with dput(ll_values_BiomeE)
                         expected = c(-2.20049712370222,
                                      -2.23945078756242,
                                      -1.07594190198439,
                                      -13.548057740458))
  # Test rsofun::cost_rmse_biomee()
  rmse_values_BiomeE <- apply(dplyr::select(test_params_BiomeE, -err_GPP), 1, function(par_v) { # par_v is a vector
    rsofun::cost_rmse_biomee(     # likelihood cost function from package
      par = par_v,                      # must be named
      obs = rsofun::biomee_validation, # example data from package
      drivers = rsofun::biomee_gs_leuning_drivers)
  })
  testthat::expect_equal(object = rmse_values_BiomeE, 
                         # expected was generated with dput(rmse_values_BiomeE)
                         expected = c(0.991929068682269,
                                      0.371260267221828,
                                      0.220816131051185,
                                      0.356524485640199))
  
  # testthat::expect_equal(order(rmse_values_BiomeE), # TODO: shouldn't the ll-order with 
  #                                                   # a normal error model correspond 
  #                                                   # to the RMSE order? 
  #                        order(ll_values_BiomeE))
  
  # Test multi-site BiomeE loglikelihood (NOTE: pseudo-multi-site for lack of input data)
  # undebug(rsofun::cost_likelihood_biomee)
  ll_values_BiomeE_multisite <- apply(test_params_BiomeE, 1, function(par_v) { # par_v is a vector
    rsofun::cost_likelihood_biomee(    # likelihood cost function from package
      par = par_v,                     # must be named
      obs     = dplyr::bind_rows(rsofun::biomee_validation, mutate(rsofun::biomee_validation, sitename = 'CH-Lae_copy')), # example data from package
      drivers = dplyr::bind_rows(rsofun::biomee_gs_leuning_drivers, mutate(rsofun::biomee_gs_leuning_drivers, sitename = 'CH-Lae_copy')), # example data from package
      targets = c('GPP'))
  })
  testthat::expect_equal(object = ll_values_BiomeE_multisite, 2 * ll_values_BiomeE)
  testthat::expect_equal(object = ll_values_BiomeE_multisite, 
                         # expected was generated with dput(ll_values_BiomeE_multisite) (TODO: or actually dput(ll_values_BiomeE * 2))
                         expected = c(-4.40099424740445,
                                      -4.47890157512484,
                                      -2.15188380396878,
                                      -27.096115480916))
})

# example from the Trotsiuk, Hartig, Forrester paper:
###   #' r3pg_sim <- function( par_df, ...){
###   #'   #' @description function to simulate the model for a given parameter
###   #'   #' @param par_df data frame of the parameters with two columns: parameter, piab
###   #'   
###   #'   sim.df <- run_3PG(
###   #'     site = site_solling, 
###   #'     species = species_solling,
###   #'     climate = climate_solling,
###   #'     thinning = thinn_solling,
###   #'     parameters = par_df, 
###   #'     size_dist = NULL,
###   #'     settings =  list(light_model = 1, transp_model = 1, phys_model = 1),
###   #'     check_input = TRUE, ...)
###   #'   
###   #'   return( sim.df )
###   #' }
###   #' r3pg_ll <- function( par_v ){
###   #'   #' @param par_v a vector of parameters for the calibration, including errors 
###   #'   #' par_v = par_cal_best
###   #'   
###   #'   # replace the default values for the selected parameters
###   #'   err_id = grep('err', par_cal_names)
###   #'   
###   #'   par_df = dplyr::bind_rows(
###   #'     dplyr::filter(par_def.df, !parameter %in% par_cal_names),
###   #'     data.frame(parameter = par_cal_names[-err_id], piab = par_v[-err_id]))
###   #'   
###   #'   err_v = par_v[err_id]
###   #'   
###   #'   # simulate the model
###   #'   sim.df <- r3pg_sim(par_df, df_out = FALSE)
###   #'   sim.df <- cbind(sim.df[,,2,c(3,5,6)], sim.df[,,4,c(1,2,3)])
###   #'   
###   #'   # calculate the log likelihood
###   #'   logpost <- sapply(1:6, function(i) {
###   #'     likelihoodIidNormal( sim.df[,i], observ_solling_mat[,i], err_v[i] ) 
###   #'   }) %>% sum(.)
###   #'   
###   #'   if(is.nan(logpost) | is.na(logpost) | logpost == 0 ){logpost <- -Inf}
###   #'   
###   #'   return( logpost )
###   #' }
