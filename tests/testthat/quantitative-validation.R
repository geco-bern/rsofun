context("Test model output (values)")

test_that("p-model quantitative check", {
  skip_on_cran()
  
  # grab gpp data from the validation set
  # for FR-Pue
  gpp <- p_model_validation$data[[1]]$gpp
  
  # set model drivers to the NPHT paper
  # ones
  params_modl <- list(
    kphio           = 0.09423773,
    soilm_par_a     = 0.33349283,
    soilm_par_b     = 1.45602286,
    tau_acclim_tempstress = 10,
    par_shape_tempstress  = 0.0
  )
  
  # run the model for these parameters
  output <- rsofun::runread_pmodel_f(
    rsofun::p_model_drivers,
    par = params_modl
  )$data[[1]]$gpp
  
  # normal tolerance ~ 0.67
  tolerance <- mean(abs(output - gpp), na.rm = TRUE)/
     mean(abs(gpp), na.rm = TRUE)
  
  # test for correctly returned values
  expect_equal(tolerance, 0.6768124, tolerance = 0.03)
})
