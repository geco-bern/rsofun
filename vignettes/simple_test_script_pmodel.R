library(tidyverse)

params_modl <- list(
  kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
  soilm_betao        = 0.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41,
  whc = 432
)

p_model_drivers_test = p_model_drivers
p_model_drivers_test$params_siml[[1]]$use_phydro = F
p_model_drivers_test$params_siml[[1]]$use_pml = F
p_model_drivers_test$site_info[[1]]$canopy_height = 20
p_model_drivers_test$site_info[[1]]$reference_height = 22
p_model_drivers_test$forcing[[1]]$netrad = NA
p_model_drivers_test$forcing_acclim = p_model_drivers_test$forcing

# run the model for these parameters
output <- rsofun::runread_pmodel_f(
  p_model_drivers_test,
  par = params_modl
)

print(
output$data[[1]] %>% 
  select(date, gpp, aet, pet, wscal, netrad, wcont) %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(x=date, y=value)) +
  theme_classic()+
  geom_line(col="seagreen") +
  facet_wrap(~name, scales="free")
)