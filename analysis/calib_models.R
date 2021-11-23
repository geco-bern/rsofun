library(rsofun)

df_drivers <- lm3ppa_gs_leuning_drivers
ddf_obs <- lm3ppa_validation_2

df_drivers$params_siml[[1]]$spinup <- FALSE

# Mortality as DBH
settings <- list(
  method              = "bayesiantools",
  targetvars          = c("gpp"),
  timescale           = list(targets_obs = "y"),
  sitenames           = "CH-Lae",
  metric              = cost_rmse_lm3ppa_gsleuning,
  dir_results         = "./",
  name                = "ORG",
  control = list(
    sampler = "DEzs",
    settings = list(
      burnin = 1,
      iterations = 5
    )
  ),
  par = list(
    phiRL = list(lower=0.5, upper=5, init=3.5),
    LAI_light = list(lower=2, upper=5, init=3.5),
    tf_base = list(lower=0.5, upper=1.5, init=1),
    par_mort = list(lower=0.1, upper=2, init=1))
)

pars <- calib_sofun(
  drivers = df_drivers,
  obs = ddf_obs,
  settings = settings
)
