#!/usr/bin/env Rscript

library(rsofun)
library(tibble)

generate_output <- function(drivers) {
  out <- runread_biomee_f(
    drivers,
    makecheck = TRUE,
    parallel = FALSE)
  
  return(out)
}

# run the model gs-leuning
biomee_gs_leuning_output <- generate_output(biomee_gs_leuning_drivers)

save(biomee_gs_leuning_output,
     file ="data/biomee_gs_leuning_output.rda",
     compress = "xz")

# run the model p-model
biomee_p_model_output <- generate_output(biomee_p_model_drivers)

save(biomee_p_model_output,
     file = "data/biomee_p_model_output.rda",
     compress = "xz")

# run the model p-model LULUC
biomee_p_model_luluc_output <- generate_output(biomee_p_model_luluc_drivers)

save(biomee_p_model_luluc_output,
     file = "data/biomee_p_model_luluc_output.rda",
     compress = "xz")
