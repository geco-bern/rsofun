library(rsofun)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(khroma)

## ----------------------------------------------------------------------------------------------------------------------
p_model_drivers = readRDS(file = here::here("data/p_model_drivers_newformat.rds"))
p_model_validation = readRDS(file = here::here("data/p_model_validation_newformat.rds"))

## ----------------------------------------------------------------------------------------------------------------------
# define model parameter values from previous work
# ------------------------------------------------------
# Note that in the phydro branch of rsofun,
#    whc must be included in params_modl, rather than in site_info
# ------------------------------------------------------
params_modl <- list(
  kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41,
  whc                = p_model_drivers$site_info[[1]]$whc
  )

## ----------------------------------------------------------------------------------------------------------------------
p_model_drivers$params_siml[[1]]$use_gs <- TRUE
p_model_drivers$params_siml[[1]]$use_pml <- TRUE
p_model_drivers$params_siml[[1]]$use_phydro <- FALSE

## ----------------------------------------------------------------------------------------------------------------------
# run the model for these parameters
output_whc1 <- rsofun::runread_pmodel_f(
  p_model_drivers,
  par = params_modl
)

## ----------------------------------------------------------------------------------------------------------------------
# run the model for WHC divided by 10
params_modl2 <- params_modl
params_modl2$whc <- p_model_drivers$site_info[[1]]$whc * 0.5

output_whc2 <- rsofun::runread_pmodel_f(
  p_model_drivers,
  par = params_modl2
)

## ----------------------------------------------------------------------------------------------------------------------
# Plot
tmp <- output_whc1 |> 
  select(data) |> 
  unnest(data) |> 
  select(date, le, le_soil, le_canopy, aet, pet, wcont) |> 
  mutate(whc = "orig") |> 
  bind_rows(
    output_whc2 |> 
      select(data) |> 
      unnest(data) |> 
      select(date, le, le_soil, le_canopy, aet, pet, wcont) |> 
      mutate(whc = "small")
  ) |> 
  mutate(doy = lubridate::yday(date)) |> 
  group_by(doy, whc) |> 
  summarise(across(where(is.numeric), mean))

tmp |> 
  ggplot(aes(doy, aet, color = whc)) + 
  geom_line() + 
  scale_color_okabeito() +
  theme_classic()

tmp |> 
  ggplot(aes(doy, le / (24*60*60), color = whc)) + 
  geom_line() + 
  scale_color_okabeito() +
  theme_classic()
  
tmp |> 
  ggplot(aes(doy, le_soil / (24*60*60), color = whc)) + 
  geom_line() + 
  scale_color_okabeito() +
  theme_classic()

tmp |> 
  ggplot(aes(doy, le_canopy / (24*60*60), color = whc)) + 
  geom_line() + 
  scale_color_okabeito() +
  theme_classic()

tmp |> 
  ggplot(aes(doy, wcont, color = whc)) + 
  geom_hline(yintercept = c(0, p_model_drivers$site_info[[1]]$whc, p_model_drivers$site_info[[1]]$whc * 0.5), color = "grey") +
  geom_line() + 
  scale_color_okabeito() +
  theme_classic()

tmp |> 
  ggplot(aes(doy, aet/pet, color = whc)) + 
  geom_line() + 
  scale_color_okabeito() +
  theme_classic() +
  ylim(0, 1)
