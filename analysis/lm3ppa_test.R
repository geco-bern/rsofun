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
  method_photosynth     = "pmodel", # gs_leuning or pmodel
  method_mortality      = "dbh" # dbh or cstarvation or growthrate or const_selfthing
)

bla <- runread_lm3ppa_f(
  df_drivers,
  ncores = 1,
  makecheck = FALSE
)