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
  expect_equal(tolerance, 0.4201191, tolerance = 0.04)
})
