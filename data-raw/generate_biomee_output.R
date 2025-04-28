#!/usr/bin/env Rscript

library(rsofun)

# load data currently in data/ directory (to prevent using potentially outdated 
# data, from the currently loaded rsofun package):
from_data_dir <- rlang::env()
load("data/biomee_gs_leuning_drivers.rda", envir = from_data_dir)
load("data/biomee_p_model_drivers.rda",    envir = from_data_dir)

generate_output <- function(drivers) {
  out <- runread_biomee_f(
    drivers,
    makecheck = TRUE,
    parallel = FALSE)
  
  return(out)
}

# run the model gs-leuning
biomee_gs_leuning_output <- generate_output(from_data_dir[['biomee_gs_leuning_drivers']])

save(biomee_gs_leuning_output,
     file ="data/biomee_gs_leuning_output.rda",
     compress = "xz")

# run the model p-model
biomee_p_model_output <- generate_output(from_data_dir[['biomee_p_model_drivers']])

save(biomee_p_model_output,
     file = "data/biomee_p_model_output.rda",
     compress = "xz")
