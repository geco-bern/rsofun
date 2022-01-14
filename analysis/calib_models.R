library(rsofun)
library(rpmodel)
library(tidyverse)
source("R/cost_functions.R")
source("R/calib_sofun_2.R")

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
    phiRL = list(lower=0.5, upper=5, init=3.5),
    LAI_light = list(lower=2, upper=5, init=3.5),
    tf_base = list(lower=0.5, upper=1.5, init=1),
    par_mort = list(lower=0.1, upper=2, init=1),
    err_gpp = list(lower = 0, upper = 30, init = 15),
    err_gpp_unc = list(lower = 0, upper = 1, init = 1)
  )
)

pars <- calib_sofun_2(
  drivers = drivers,
  obs = obs,
  settings = settings
)