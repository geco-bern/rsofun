context("Test model output (values)")

test_that("p-model quantitative check", {
  skip_on_cran()
  
  # grab gpp data from the validation set
  # for FR-Pue
  gpp <- p_model_validation$data[[1]]$gpp
  
  # set model drivers to the NPHT paper
  # ones
  params_modl <- list(
    kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
    kphio_par_a        = 0.0,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
    kphio_par_b        = 1.0,
    soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
    soilm_betao        = 0.01,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
    tau_acclim         = 30.0,
    kc_jmax            = 0.41
  )
  
  # run the model for these parameters
  output <- rsofun::runread_pmodel_f(
    rsofun::p_model_drivers,
    par = params_modl
  )$data[[1]]$gpp
  
  # normal tolerance ~ 0.305
  tolerance <- mean(abs(output - gpp), na.rm = TRUE)/
     mean(abs(gpp), na.rm = TRUE)
  
  # test for correctly returned values
  expect_equal(tolerance, 0.3050983, tolerance = 0.03)
})

# test_that("p-model consistency R vs Fortran (rpmodel vs rsofun)", {
#   skip_on_cran()
# 
#   df <- rsofun::p_model_drivers
# 
#   # set model drivers to the NPHT paper
#   # ones
#   params_modl <- list(
#     kphio           = 0.09423773,
#     soilm_par_a     = 0.33349283,
#     soilm_par_b     = 1.45602286,
#     tau_acclim_tempstress = 10,
#     par_shape_tempstress  = 0.0
#   )
# 
#   df$forcing[[1]] <- df$forcing[[1]] %>%
#     dplyr::mutate(dplyr::across(-c(date,doy), mean))
# 
#   # run the model for these parameters
#   output <- rsofun::runread_pmodel_f(
#     df,
#     par = params_modl
#   )$data[[1]]$gpp
# 
#   df <- df$forcing[[1]]
# 
#   output_rp <- apply(df, 1, function(x){
#     out <- rpmodel::rpmodel(
#       tc             = as.numeric(x['temp']),
#       patm           = as.numeric(x['patm']),
#       co2            = as.numeric(x['co2']),
#       fapar          = as.numeric(x['fapar']),
#       ppfd           = as.numeric(x['ppfd']),
#       vpd            = as.numeric(x['vpd']),
#       elv            = 270,
#       kphio          = 0.09423773,
#       beta           = 146,
#       c4             = FALSE,
#       method_optci   = "prentice14",
#       method_jmaxlim = "wang17",
#       do_ftemp_kphio = FALSE,
#       do_soilmstress = FALSE,
#       verbose        = TRUE
#     )
#   })
# 
#   output_rp <- data.frame(do.call("rbind", output_rp))
#   output_rp <- unlist(output_rp$gpp)
# 
#   plot(output_rp)
#   plot(output)
# 
#   # normal tolerance ~ 0.67
#   tolerance <- mean(abs(output - gpp), na.rm = TRUE)/
#     mean(abs(gpp), na.rm = TRUE)
# 
#   # test for correctly returned values
#   expect_equal(tolerance, 0.6768124, tolerance = 0.03)
# })
