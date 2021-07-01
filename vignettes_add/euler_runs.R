library(dplyr)
library(tibble)
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("stineb/rsofun")
library(rsofun)
install()
library(ggplot2)
library(ingestr)
library(multidplyr)

load("/cluster/home/lmarques/rsofun/input_data/df_drivers_DBH_gs.RData")

start <- Sys.time()
df_output <- runread_lm3ppa_f(
  df_drivers,
  makecheck = TRUE,
  parallel = FALSE
)
print(Sys.time() - start)

write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile, "/cluster/home/lmarques/rsofun/output_euler/ea1sa1DBHgl_out_annual_tileEULER.csv")

load("/cluster/home/lmarques/rsofun/input_data/ddf_obs.RData")

settings_calib_DBH_gs <- list(
  method              = "gensa",
  targetvars          = c("targets_obs"),
  timescale           = list(targets_obs = "y"),
  maxit               = 2, 
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
settings_calib_DBH_gs <- calib_sofun(
  df_drivers = df_drivers,  
  ddf_obs = ddf_obs,
  settings = settings_calib_DBH_gs
)

save(settings_calib_DBH_gs, file = "/cluster/home/lmarques/rsofun/input_data/settings_calib_DBH_gsEULER.RData")
