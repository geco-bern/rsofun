library(rsofun)
library(rpmodel)
library(tidyverse)
# source("R/cost_functions.R")
# source("R/calib_sofun_2.R")

drivers <- p_model_drivers
obs <- p_model_validation

settings <- list(
  method              = "bayesiantools",
  targets             = c("gpp"),
  timescale           = list(targets_obs = "y"),
  sitenames           = "FR-Pue",
  metric              = likelihood_pmodel,
  control = list(
    sampler = "DEzs",
    settings = list(
      burnin = 10,
      iterations = 400,
      nrChains = 3
    )
  ),
  par = list(
    kphio = list(lower=0.04, upper=0.09, init=0.05),
    soilm_par_a = list(lower=0.04, upper=0.09, init=0.05),
    soilm_par_b = list(lower=0.04, upper=0.09, init=0.05),
    err_gpp = list(lower = 0, upper = 30, init = 15),
    err_gpp_unc = list(lower = 0, upper = 1, init = 1)
  )
)

pars <- calib_sofun(
  drivers = drivers,
  obs = obs,
  settings = settings
)