library(dplyr)
library(tibble)
#if(!require(devtools)){install.packages(devtools)}
#devtools::install_github("stineb/rsofun")
library(devtools)
library(rsofun)
library(ggplot2)
library(multidplyr)

#build()
install()

load("input_data/df_drivers_GR_gs.RData")
load("input_data/ddf_obs.RData")

settings_calib_GR_gs <- list(
  method              = "gensa",
  targetvars          = c("targets_obs"),
  timescale           = list(targets_obs = "y"),
  maxit               = 2000, 
  sitenames           = "CH-Lae",
  metric              = "rmse",
  dir_results         = "./",
  name                = "ORG",
  par                 = list(phiRL = list(lower=0.5, upper=5, init=3.5),
                             LAI_light = list(lower=2, upper=5, init=3.5),
                             tf_base = list(lower=0.5, upper=1.5, init=1),
                             par_mort = list(lower=0.1, upper=2, init=1),
                             par_mort_under = list(lower=0.1, upper=2, init=1))
)

set.seed(1152)
settings_calib_GR_gs <- calib_sofun(
  df_drivers = df_drivers,  
  ddf_obs = ddf_obs,
  settings = settings_calib_GR_gs
)

save(settings_calib_GR_gs, file = "input_data/settings_calib_GR_gs_1_euler.RData")
