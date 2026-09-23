set.seed(10)

test_that("p-model quantitative check", {
  skip_on_cran()

  # set model drivers to the NPHT paper ones
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
    rsofun::pmodel_drivers |> dplyr::filter(sitename == "FR-Pue"),
    par = params_modl)
  pred <- output |> tidyr::unnest(data)

  # grab observational gpp data from the validation set for FR-Pue
  obs <- pmodel_validation |> dplyr::filter(sitename == "FR-Pue") |> tidyr::unnest(data)

  obs_pred <- dplyr::left_join(
    select(obs, sitename, date, gpp_obs = gpp),
    select(pred, sitename, date, gpp_pred = gpp),
    by = join_by(sitename, date))

  # relative MAE ~ 0.305
  rMAE <- mean(abs(obs_pred$gpp_pred - obs_pred$gpp_obs), na.rm = TRUE) /
    mean(abs(obs_pred$gpp_obs), na.rm = TRUE)

  # test for correctly returned values
  expect_equal(rMAE, 0.3432234, tolerance = 0.04)
})
