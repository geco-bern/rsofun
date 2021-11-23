# Pre-calculate results for vignettes to speed up rendering
# 
# The current routine is very slow, and hangs the session at
# times.

library(rsofun)

# run the model
lm3ppa_p_model_output <- runread_lm3ppa_f(
  lm3ppa_p_model_drivers,
  makecheck = TRUE,
  parallel = FALSE
)$data[[1]]$output_annual_tile[c('year','GPP','plantC')]

save(lm3ppa_p_model_output,
     file ="data/lm3ppa_p_model_output.rda",
     compress = "xz")

lm3ppa_gs_leuning_output <- runread_lm3ppa_f(
  lm3ppa_gs_leuning_drivers,
  makecheck = TRUE,
  parallel = FALSE
)$data[[1]]$output_annual_tile[c('year','GPP','plantC')]

save(lm3ppa_gs_leuning_output,
     file ="data/lm3ppa_gs_leuning_output.rda",
     compress = "xz")