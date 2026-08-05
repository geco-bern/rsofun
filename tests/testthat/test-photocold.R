test_that("cold acclimation is opt-in and affects daily P-model output", {
  params_modl <- list(
    kphio = 0.04998,
    kphio_par_a = 0.0,
    kphio_par_b = 1.0,
    soilm_thetastar = 144.0,
    soilm_betao = 0.0,
    beta_unitcostratio = 146.0,
    rd_to_vcmax = 0.014,
    tau_acclim = 30.0,
    kc_jmax = 0.41
  )
  drivers <- rsofun::pmodel_drivers[5, ]

  output_legacy <- rsofun::runread_pmodel_f(
    drivers,
    par = params_modl,
    makecheck = TRUE
  )
  output_disabled <- rsofun::runread_pmodel_f(
    drivers,
    par = c(params_modl, list(
      coldacclim_par_a = 0.0,
      coldacclim_par_b = 0.0,
      coldacclim_par_c = 0.0,
      coldacclim_par_d = 0.0
    )),
    makecheck = TRUE
  )
  output_enabled <- rsofun::runread_pmodel_f(
    drivers,
    par = c(params_modl, list(
      coldacclim_par_a = -2.0,
      coldacclim_par_b = 0.5,
      coldacclim_par_c = 50.0,
      coldacclim_par_d = 0.1
    )),
    makecheck = TRUE
  )

  expect_identical(output_legacy$data[[1]], output_disabled$data[[1]])
  expect_true("snow" %in% names(output_enabled$data[[1]]))
  expect_true(any(
    abs(output_legacy$data[[1]]$gpp - output_enabled$data[[1]]$gpp) > 1e-8,
    na.rm = TRUE
  ))
})
