#!/usr/bin/env Rscript

library(tidyverse)
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
  spinupyears = 700,
  recycle = 800,
  firstyeartrend = 2009,
  nyeartrend = 800,
  outputhourly = TRUE,
  outputdaily = TRUE,
  do_U_shaped_mortality = TRUE,
  update_annualLAImax = TRUE,
  do_closedN_run = TRUE,
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
  alpha_FR      = rep(1.2,16),
  rho_FR        = rep(200,16),
  root_r        = rep(2.9E-4,16), 
  root_zeta     = rep(0.29,16), 
  Kw_root       = rep(3.5e-09,16),
  leaf_size     = rep(0.04,16), 
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
  seedlingsize  = rep(0.05,16),
  LNbase        = rep(0.8E-3,16),
  lAImax        = rep(3.5,16),
  Nfixrate0     = rep(0,16),
  NfixCost0     = rep(12,16),
  phiCSA        = rep(0.25E-4,16),
  mortrate_d_c  = rep(0.01,16),
  mortrate_d_u  = rep(0.075,16),
  maturalage    = rep(5,16),
  fNSNmax       = rep(5,16),
  LMA           = c(0.05,0.17,0.11,rep(0.1,13)),
  rho_wood      = c(590,370,350,rep(300,13)), 
  alphaBM       = rep(5200,16),
  thetaBM       = c(2.36,2.30,2.54,rep(2.30,13)),
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
  init_cohort_species = rep(1, 10),
  init_cohort_nindivs = rep(0.05,10),
  init_cohort_bsw     = rep(0.05,10),
  init_cohort_bHW     = rep(0.0, 10),
  init_cohort_nsc     = rep(0.05,10)
)

init_soil <- tibble( #list
  init_fast_soil_C    = 0.0,
  init_slow_soil_C    = 0.0,
  init_Nmineral       = 0.015,
  N_input             = 0.0008
)

#---- gs leuning formatting -----
forcing <- forcingLAE %>% 
  dplyr::group_by(
    lubridate::month(datehour),
    lubridate::day(datehour),
    lubridate::hour(datehour)) %>% 
  summarise_at(vars(1:13), list(~mean(., na.rm = TRUE)))
forcing <- forcing[,-c(1:3)]
forcing <- bind_rows(replicate(800, forcing, simplify = FALSE))

lm3ppa_gs_leuning_drivers <- tibble(
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

save(lm3ppa_gs_leuning_drivers,
     file ="data/lm3ppa_gs_leuning_drivers.rda",
     compress = "xz")

#---- p-model formatting -----
forcingLAE <- forcingLAE %>% 
  dplyr::group_by(
    lubridate::month(datehour),
    lubridate::day(datehour)) %>% 
  summarise_at(vars(1:13), 
               list(~mean(., na.rm = TRUE)))
forcing <- forcingLAE[,-c(1:2)]
forcing <- bind_rows(replicate(800, forcing, simplify = FALSE))

lm3ppa_p_model_drivers <- tibble(
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

save(lm3ppa_p_model_drivers,
     file ="data/lm3ppa_p_model_drivers.rda",
     compress = "xz")

# run the model
lm3ppa_p_model_output <- runread_lm3ppa_f(
  lm3ppa_p_model_drivers,
  makecheck = TRUE,
  parallel = FALSE
)$data[[1]]$output_annual_tile[c('year','GPP','plantC')]

save(lm3ppa_p_model_output,
     file = "data/lm3ppa_p_model_output.rda",
     compress = "xz")

lm3ppa_gs_leuning_output <- runread_lm3ppa_f(
  lm3ppa_gs_leuning_drivers,
  makecheck = TRUE,
  parallel = FALSE
)$data[[1]]$output_annual_tile[c('year','GPP','plantC')]

save(lm3ppa_gs_leuning_output,
     file ="data/lm3ppa_gs_leuning_output.rda",
     compress = "xz")


