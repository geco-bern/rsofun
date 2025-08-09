#!/usr/bin/env Rscript

library(rsofun)

# Define model parameter values from previous work
params_modl <- list(
  kphio              = 0.04998, # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.01,  # set to zero to disable temperature-dependence of kphio, setup ORG in Stocker et al. 2020 GMD
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
  soilm_betao        = 0.01,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014, # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41
)

# Run the model for these parameters and the example drivers
p_model_output <- rsofun::runread_pmodel_f(
  drivers = rsofun::p_model_drivers,
  par     = params_modl)

p_model_output_vcmax25 <- rsofun::runread_pmodel_f(
  drivers = rsofun::p_model_drivers_vcmax25,
  par = params_modl)

save(p_model_output,
     file ="data/p_model_output.rda",
     compress = "xz")

save(p_model_output_vcmax25,
     file ="data/p_model_output_vcmax25.rda",
     compress = "xz")
