library(rsofun)
library(tidyverse)

df_drivers <- lm3ppa_p_model_drivers

df_drivers$params_siml[[1]] <- tibble( #list
  spinup                = TRUE,
  spinupyears           = 1800, 
  recycle               = 1,    # 9 or 11 changed to 1 when aggregating forcing into 1 year
  firstyeartrend        = 2009, 
  nyeartrend            = 11,    # 9 or 11 (longer transient years)
  outputhourly          = TRUE,
  outputdaily           = TRUE,
  do_U_shaped_mortality = TRUE,
  update_annualLAImax   = TRUE,
  do_closedN_run        = TRUE,
  method_photosynth     = "gs_leuning", # gs_leuning or pmodel
  method_mortality      = "dbh" # dbh or cstarvation or growthrate or const_selfthing
)

bla <- runread_lm3ppa_f(
  lm3ppa_gs_leuning_drivers,
  ncores = 1,
  makecheck = FALSE
)
 
# df_output <- run_lm3ppa_f_bysite(
#   df_drivers$sitename[1],
#   df_drivers$params_siml[[1]],
#   df_drivers$siteinfo[[1]],
#   df_drivers$forcing[[1]],
#   df_drivers$params_tile[[1]],
#   df_drivers$params_species[[1]],
#   df_drivers$params_soil[[1]],
#   df_drivers$init_cohort[[1]],
#   df_drivers$init_soil[[1]],
#   makecheck = TRUE)