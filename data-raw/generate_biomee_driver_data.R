#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(rsofun)

# load script arguments
args <- commandArgs(trailingOnly = TRUE)

# load data
if (is.na(args[1])){
  load("data-raw/CH-LAE_forcing.rda")
} else {
  load(args[1])
}

# sitename
sitename <- "CH-Lae"

# Take only year 2004 to 2014, corresponding to subset of data for site CH-Lae
siteinfo <- tibble(
  sitename = "CH-Lae",
  lon = 8.365,
  lat = 47.47808,
  elv = 700,
  year_start = 2004,
  year_end = 2014,
  classid = NA,
  c4 = FALSE,
  whc = NA,
  koeppen_code = NA,
  igbp_land_use = "Mixed Forests",
  plant_functional_type = "Broadleaf trees")

siteinfo <- siteinfo %>% 
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

# load model parameters (valid ones)
params_siml <- tibble(
  spinup = TRUE,
  spinupyears = 250,
  recycle = 1,
  firstyeartrend = 2009,
  nyeartrend = 1,
  steps_per_day = 24,
  outputhourly = TRUE,
  outputdaily = TRUE,
  do_U_shaped_mortality = TRUE,
  update_annualLAImax = TRUE,
  do_closedN_run = TRUE,
  do_reset_veg = FALSE, # TRUE
  dist_frequency = 0, # 100, 75, 50, 25, 15, 10
  method_photosynth = "gs_leuning",
  method_mortality = "dbh"
)

params_siml_pmodel <- params_siml
params_siml_pmodel$method_photosynth <- "pmodel"

params_tile <- tibble(
  soiltype = 3,
  FLDCAP = 0.4,
  WILTPT = 0.05,
  K1 = 2.0,
  K2 = 0.05,
  K_nitrogen = 8.0,
  MLmixRatio = 0.8,
  etaN = 0.025,
  LMAmin = 0.02,
  fsc_fine = 1.0,
  fsc_wood = 0.0,
  GR_factor = 0.33,
  l_fract = 0.0,
  retransN = 0.0,
  f_initialBSW = 0.2,
  f_N_add = 0.02,
  tf_base = 1,
  par_mort = 1,
  par_mort_under = 1
)

params_species <- tibble(
  lifeform      = rep(1,16),
  phenotype     = c(0,1,1,rep(1,13)),
  pt            = rep(0,16),
  # Root parameters
  alpha_FR      = rep(1.2,16),
  rho_FR        = rep(200,16),
  root_r        = rep(2.9E-4,16), 
  root_zeta     = rep(0.29,16), 
  Kw_root       = rep(3.5e-09,16),
  leaf_size     = rep(0.04,16), 
  # Photosynthesis parameters
  Vmax          = rep(35.0E-6,16),
  Vannual       = rep(1.2,16),
  wet_leaf_dreg = rep(0.3,16),
  m_cond        = rep(7.0,16), 
  alpha_phot    = rep(0.06,16), 
  gamma_L       = rep(0.02,16), 
  gamma_LN      = rep(70.5 ,16),
  gamma_SW      = rep(0.08,16),
  gamma_FR      = rep(12.0,16),
  tc_crit       = rep(283.16,16),
  tc_crit_on    = rep(280.16,16),
  gdd_crit      = rep(280.0,16),
  betaON        = rep(0,2,16),     
  betaOFF       = rep(0,1,16), 
  # Allometry parameters
  alphaHT       = rep(36,16),                   
  thetaHT       = rep(0.5,16),                   
  alphaCA       = rep(150,16),                   
  thetaCA       = rep(1.5,16),                   
  alphaBM       = rep(5200,16),                   
  thetaBM       = c(2.36,2.30,2.54,rep(2.30,13)), 
  # Reproduction parameters
  seedlingsize  = rep(0.05,16),
  maturalage    = rep(5,16),     
  v_seed        = rep(0.1,16), 
  # Mortality parameters
  mortrate_d_c  = rep(0.01,16),                   
  mortrate_d_u  = rep(0.075,16), 
  # Leaf parameters
  LMA           = c(0.05,0.17,0.11,rep(0.1,13)),  
  leafLS        = rep(1,16), 
  LNbase        = rep(0.8E-3,16), 
  CNleafsupport = rep(80,16),
  rho_wood      = c(590,370,350,rep(300,13)),   
  taperfactor   = rep(0.75,16),
  lAImax        = rep(3.5,16), 
  tauNSC        = rep(3,16), 
  fNSNmax       = rep(5,16),                      
  phiCSA        = rep(0.25E-4,16),    
  # C/N ratios for plant pools
  CNleaf0      = rep(25,16),  
  CNsw0        = rep(350,16),  
  CNwood0      = rep(350,16),  
  CNroot0      = rep(40,16),  
  CNseed0      = rep(20,16),  
  Nfixrate0     = rep(0,16),   
  NfixCost0     = rep(12,16),
  internal_gap_frac      = rep(0.1,16),
  # calibratable params
  kphio         = rep(0.05,16),
  phiRL         = rep(3.5,16),
  LAI_light     = rep(3.5,16)
) 

params_soil <- tibble(
  type              = c("Coarse","Medium","Fine","CM","CF","MF","CMF","Peat","MCM"),
  GMD               = c(0.7, 0.4, 0.3, 0.1, 0.1, 0.07, 0.007, 0.3, 0.3),
  GSD               = c(5.0, 5.3, 7.4, 6.1, 6.1, 14.0, 15.0, 7.4, 7.4),
  vwc_sat           = c(0.380, 0.445, 0.448, 0.412, 0.414, 0.446, 0.424, 0.445, 0.445),
  chb               = c(3.5,6.4,11.0,4.8,6.3,8.4,6.3,6.4,6.4),
  psi_sat_ref       = c(-600, -790, -910, -1580, -1680, -1880, -5980, -790, -790),
  k_sat_ref         = c(130.8, 75.1, 53.2, 12.1, 11.1, 12.7, 1.69, 53.2, 53.2),
  alphaSoil         = rep(1, 9),
  heat_capacity_dry = c(1.2e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.4e6, 1.0)
)

init_cohort <- tibble(
  init_n_cohorts = 1,   # number of PFTs
  init_cohort_species = rep(1, 10),    # indicates sps # 1 - Fagus sylvatica
  init_cohort_nindivs = rep(0.05,10),  # initial individual density, individual/m2 ! 1 indiv/m2 = 10.000 indiv/ha
  init_cohort_bl      = rep(0.0,10),   # initial biomass of leaves, kg C/individual
  init_cohort_br      = rep(0.0, 10),  # initial biomass of fine roots, kg C/individual
  init_cohort_bsw     = rep(0.05,10),  # initial biomass of sapwood, kg C/individual
  init_cohort_bHW     = rep(0.0, 10),  # initial biomass of heartwood, kg C/tree
  init_cohort_seedC   = rep(0.0, 10),  # initial biomass of seeds, kg C/individual
  init_cohort_nsc     = rep(0.05,10)   # initial non-structural biomass
)

init_soil <- tibble( #list
  init_fast_soil_C    = 0.0,
  init_slow_soil_C    = 0.0,
  init_Nmineral       = 0.015,
  N_input             = 0.0008
)

rh_to_vpd <- function(temp, rh) {
  esat <- 611.0 * exp( (17.27 * temp)/(temp + 237.3) )

  return(esat * (1.0 - rh))
}

#---- gs leuning formatting -----
forcing <- forcingLAE %>% 
  dplyr::group_by(
    lubridate::month(datehour),
    lubridate::day(datehour),
    lubridate::hour(datehour)) %>% 
  summarise_at(vars(1:13), list(~mean(., na.rm = TRUE))) %>%
  rename(month=`lubridate::month(datehour)`,day=`lubridate::day(datehour)`) %>%
  ungroup()
forcing <- forcing %>%
  rename(year=YEAR, hod=HOUR, ppfd=Swdown, temp=TEMP, rh=RH,
         rain=RAIN, wind=WIND, patm=PRESSURE, co2=aCO2_AW) %>%
  mutate(date = make_date(year,month,day),
           vpd = rh_to_vpd(temp, rh)) %>%
  select(date,hod,temp,rain,vpd,ppfd,patm,wind,co2,)

biomee_gs_leuning_drivers <- tibble(
  sitename,
  site_info = list(tibble(siteinfo)),
  params_siml = list(tibble(params_siml)),
  params_tile = list(tibble(params_tile)),
  params_species = list(tibble(params_species)),
  params_soil = list(tibble(params_soil)),
  init_cohort = list(tibble(init_cohort)),
  init_soil = list(tibble(init_soil)),
  forcing = list(tibble(forcing))
)

save(biomee_gs_leuning_drivers,
     file ="data/biomee_gs_leuning_drivers.rda",
     compress = "xz")

#---- p-model formatting -----
forcing <- forcingLAE %>% 
  dplyr::group_by(
    lubridate::month(datehour),
    lubridate::day(datehour)) %>% 
  summarise_at(vars(1:13), 
               list(~mean(., na.rm = TRUE))) %>%
  rename(month=`lubridate::month(datehour)`,day=`lubridate::day(datehour)`) %>%
  ungroup()
forcing <- forcing %>%
  rename(year=YEAR, hod=HOUR, ppfd=Swdown, temp=TEMP, rh=RH,
         rain=RAIN, wind=WIND, patm=PRESSURE, co2=aCO2_AW) %>%
  mutate(date = make_date(year,month,day),
           vpd = rh_to_vpd(temp, rh)) %>%
  select(date,hod,temp,rain,vpd,ppfd,patm,wind,co2)

biomee_p_model_drivers <- tibble(
  sitename,
  site_info = list(tibble(siteinfo)),
  params_siml = list(tibble(params_siml_pmodel)),
  params_tile = list(tibble(params_tile)),
  params_species = list(tibble(params_species)),
  params_soil = list(tibble(params_soil)),
  init_cohort = list(tibble(init_cohort)),
  init_soil = list(tibble(init_soil)),
  forcing  =list(tibble(forcing))
)

save(biomee_p_model_drivers,
     file ="data/biomee_p_model_drivers.rda",
     compress = "xz")

# run the model gs-leuning
out <- runread_biomee_f(
  biomee_gs_leuning_drivers,
  makecheck = TRUE,
  parallel = FALSE)

biomee_gs_leuning_output_annual_tile <- out$data[[1]]$output_annual_tile
biomee_gs_leuning_output_annual_cohorts <- out$data[[1]]$output_annual_cohorts

cowplot::plot_grid(
  biomee_gs_leuning_output %>% 
    ggplot() +
    geom_line(aes(x = year, y = GPP)) +
    theme_classic()+labs(x = "Year", y = "GPP"),
  biomee_gs_leuning_output %>% 
    ggplot() +
    geom_line(aes(x = year, y = plantC)) +
    theme_classic()+labs(x = "Year", y = "plantC")
)

biomee_gs_leuning_output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = npp,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "NPP")

save(biomee_gs_leuning_output,
     file ="data/biomee_gs_leuning_output.rda",
     compress = "xz")

# run the model p-model
out <- runread_biomee_f(
  biomee_p_model_drivers,
  makecheck = TRUE,
  parallel = FALSE)

biomee_p_model_output_annual_tile <- out$data[[1]]$output_annual_tile
biomee_p_model_output_annual_cohorts <- out$data[[1]]$output_annual_cohorts

cowplot::plot_grid(
  biomee_p_model_output %>% 
    ggplot() +
    geom_line(aes(x = year, y = GPP)) +
    theme_classic()+labs(x = "Year", y = "GPP"),
  biomee_p_model_output %>% 
    ggplot() +
    geom_line(aes(x = year, y = plantC)) +
    theme_classic()+labs(x = "Year", y = "plantC")
)

biomee_p_model_output_annual_cohorts %>% group_by(PFT,year) %>%
  summarise(npp=sum(NPP*density/10000)) %>% mutate(PFT=as.factor(PFT)) %>%
  ggplot() +
  geom_line(aes(x = year, y = npp,col=PFT)) +
  theme_classic()+labs(x = "Year", y = "NPP")

save(biomee_p_model_output,
     file = "data/biomee_p_model_output.rda",
     compress = "xz")

