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
  lon = 8.365,
  lat = 47.47808,
  elv = 700,
  year_start = 2004,
  year_end = 2014,
  c4 = FALSE,
  igbp_land_use = "Mixed Forests",
  plant_functional_type = "Broadleaf trees")

siteinfo <- siteinfo %>% 
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

# load model parameters (valid ones)
params_siml_gs_leuning <- tibble(
  spinupyears = 250,
  recycle = 1,
  firstyeartrend = 2009,
  nyeartrend = 1,
  steps_per_day = 24,
  do_U_shaped_mortality = TRUE,
  do_closedN_run = FALSE,
  method_photosynth = "gs_leuning",
  method_mortality = "dbh",
  do_daily_diagnostics = TRUE # Default True
)

params_siml_pmodel <- params_siml_gs_leuning
params_siml_pmodel$method_photosynth <- "pmodel"
params_siml_pmodel$steps_per_day <- 1

params_tile <- tibble(
  soiltype          = 3,
  FLDCAP            = 0.4,
  WILTPT            = 0.05,
  K1                = 2.0,
  K2                = 0.05,
  K_nitrogen        = 8.0,
  MLmixRatio        = 0.8,
  etaN              = 0.025,
  LMAmin            = 0.02,
  fsc_fine          = 1.0,
  fsc_wood          = 0.0,
  GR_factor         = 0.33,
  l_fract           = 0.0,
  retransN          = 0.0,
  f_initialBSW      = 0.2,
  f_N_add           = 0.02,
  tf_base           = 1,
  par_mort          = 1,
  par_mort_under    = 1
)

params_species <- tibble(
  lifeform          = c(0, 1, 1, 1, 1),    # 0: grass; 1 Woody
  phenotype         = c(0, 0, 1, 1, 1),    # 0: Deciduous; 1 Evergreen
  pt                = c(1, 0, 0, 0, 0),    # 0: C3; 1: C4
  # Root parameters
  alpha_FR          = rep(1.2, 5),
  rho_FR            = rep(200, 5),
  root_r            = rep(2.9E-4, 5),
  root_zeta         = rep(0.29, 5),
  Kw_root           = rep(3.5e-09, 5),
  leaf_size         = rep(0.04, 5),  ######### Unused
  # Photosynthesis parameters
  Vmax              = rep(35.0E-6, 5),
  Vannual           = rep(1.2, 5),
  wet_leaf_dreg     = rep(0.3, 5),
  m_cond            = c(7.0, 7.0, 7.0, 7.0, 7.0), #
  alpha_phot        = rep(0.06, 5),
  gamma_L           = rep(0.02, 5),
  gamma_LN          = rep(70.5, 5),
  gamma_SW          = c(0.02, 0.08, 0.08, 0.08, 0.08), # Wood Acambium respiration rate (kgC/m2/yr)
  gamma_FR          = rep(12.0, 5),
  tk_crit           = rep(283.16, 5),
  tk_crit_on        = rep(280.16, 5),
  gdd_crit          = rep(280.0, 5),
  betaON            = rep(0, 5), ######### Unused
  betaOFF           = rep(0, 5), ######### Unused
  # Allometry parameters
  alphaHT           = rep(36, 5),
  thetaHT           = rep(0.5, 5),
  alphaCA           = rep(150, 5),
  thetaCA           = rep(1.5, 5),
  alphaBM           = rep(5200, 5),
  thetaBM           = c(2.3, 2.36, 2.30, 2.54, 2.30),
  # Reproduction parameters
  seedlingsize      = rep(0.05, 5),                   # initial size of seedlings (s0_plant)
  maturalage        = c(0, 5, 5, 5, 5),               # (AgeRepro)
  v_seed            = rep(0.1, 5),
  # Mortality parameters
  mortrate_d_c      = c(0.02, 0.01, 0.01, 0.01, 0.01),# canopy tree mortality rate, year-1 (r0mort_c)
  mortrate_d_u      = c(4.0, 0.075, 0.075, 0.075, 0.075), # understory tree mortality rate, year-1 (A_sd)
  # Leaf parameters
  LMA               = c(0.02, 0.05, 0.17, 0.11, 0.1), # Leaf mass per unit area
  leafLS            = rep(1, 5),
  LNbase            = c(1.2E-3, 0.8E-3, 0.8E-3, 0.8E-3, 0.8E-3), # kgN m-2 leaf, Vmax = 0.03125*LNbase
  CNleafsupport     = rep(80, 5),
  rho_wood          = c(150, 590, 370, 350, 300),     #
  taperfactor       = rep(0.75, 5),
  lAImax            = rep(3.5, 5),                    # maximum crown LAI
  tauNSC            = rep(3, 5),
  fNSNmax           = rep(5, 5),
  phiCSA            = rep(0.25E-4, 5),                # ratio of Asap/Acrown
  # C/N ratios for plant pools
  CNleaf0           = rep(25, 5),
  CNsw0             = rep(350, 5),
  CNwood0           = rep(350, 5),
  CNroot0           = rep(40, 5),
  CNseed0           = rep(20, 5),
  Nfixrate0         = rep(0.0, 5),                    # 0.03 kgN kgRootC-1 yr-1
  NfixCost0         = c(0, 12, 12, 12, 12),           # N fixation carbon cost: 12 gC/gN
  internal_gap_frac = rep(0.1,5),
  # calibratable params
  kphio             = rep(0.05, 5),
  phiRL             = c(0.7, 3.5, 3.5, 3.5, 3.5),     # Root/Leaf area ratio
  LAI_light         = rep(3.5, 5)                      # Light-limited crown LAI
)
params_species <- params_species[2:5, ] # NOTE: current drivers and outputs had not been updated
                                        #       with the first species (the grass species), leading
                                        #       to test fail if run with all 5 species. Therefore,
                                        #       the grass species is removed again here. 
                                        # TODO: Later one can decide to include the grass species in the 
                                        #       example driver, but this requires updating outputs and
                                        #       tests as well.

init_cohort <- tibble(
  init_cohort_species = rep(2, 1),    # indicates sps # 2 - Fagus sylvatica
  init_cohort_nindivs = rep(0.05, 1), # initial individual density, individual/m2 ! 1 indiv/m2 = 10.000 indiv/ha
  init_cohort_bl      = rep(0.0, 1),  # initial biomass of leaves, kg C/individual
  init_cohort_br      = rep(0.0, 1),  # initial biomass of fine roots, kg C/individual
  init_cohort_bsw     = rep(0.05, 1), # initial biomass of sapwood, kg C/individual
  init_cohort_bHW     = rep(0.0, 1),  # initial biomass of heartwood, kg C/tree
  init_cohort_seedC   = rep(0.0, 1),  # initial biomass of seeds, kg C/individual
  init_cohort_nsc     = rep(0.05, 1), # initial non-structural biomass
  lu_index            = rep(0, 1)     # index land use (LU) containing this cohort. 0 (default) means any vegetated tile will contain a copy.
)

init_soil <- tibble( #list
  init_fast_soil_C    = 0.01,
  init_slow_soil_C    = 0.001,
  init_Nmineral       = 0.015,
  N_input             = 0.01
)

rh_to_vpd <- function(temp, # Air temperature (deg C)
                      rh    # Relative humidity (< 1)
) {
  esat <- 611.0 * exp( (17.27 * temp)/(temp + 237.3) )
  
  return(esat * (1.0 - rh)) # VPD (Pa)
}

rad_to_ppfd <- function(rad   # Downwelling radiation (W m-2)
) {
  kcFCE <- 2.04 # from flux to energy conversion, umol/J (Meek et al., 1984)
  
  return(rad * kcFCE * 1.0e-6)  # PPFD (mol m-2 s-1)
}

build_forcing <- function(forcing_data, hourly) {
  if (hourly)
    groups <- forcing_data %>% dplyr::group_by(
      lubridate::month(datehour),
      lubridate::day(datehour),
      lubridate::hour(datehour))
  else
    groups <- forcing_data %>% dplyr::group_by(
      lubridate::month(datehour),
      lubridate::day(datehour))
  forcing <- groups %>%
    summarise_at(vars(1:13), list(~mean(., na.rm = TRUE))) %>%
    rename(month=`lubridate::month(datehour)`,day=`lubridate::day(datehour)`) %>%
    ungroup() %>%
    rename(year=YEAR, hod=HOUR, rad=Swdown, temp=TEMP, rh=RH,
           rain=RAIN, wind=WIND, patm=PRESSURE, co2=aCO2_AW) %>%
    mutate(date = make_date(year,month,day),
           vpd = rh_to_vpd(temp, rh / 100.0),
           ppfd = rad_to_ppfd(rad)) %>%
    select(date,hod,temp,rain,vpd,ppfd,patm,wind,co2,)
  return(forcing)
}

build_driver <- function(params_siml, forcing) {
  drivers <- tibble(
    sitename,
    site_info = list(tibble(siteinfo)),
    params_siml = list(tibble(params_siml)),
    params_tile = list(tibble(params_tile)),
    params_species = list(tibble(params_species)),
    init_cohort = list(tibble(init_cohort)),
    init_soil = list(tibble(init_soil)),
    forcing = list(tibble(forcing))
  )
  return(drivers)
}

#---- gs leuning driver -----
forcing_gs_leuning <- build_forcing(forcingLAE, TRUE)

biomee_gs_leuning_drivers <- build_driver(params_siml_gs_leuning, forcing_gs_leuning)

save(biomee_gs_leuning_drivers,
     file ="data/biomee_gs_leuning_drivers.rda",
     compress = "xz")

#---- p-model driver -----
forcing_pmodel <- build_forcing(forcingLAE, FALSE)

biomee_p_model_drivers <- build_driver(params_siml_pmodel, forcing_pmodel)

save(biomee_p_model_drivers,
     file ="data/biomee_p_model_drivers.rda",
     compress = "xz")

#---- p-model with LULUC driver -----
biomee_p_model_luluc_drivers <- biomee_p_model_drivers
lu_defs <- tibble(
  name      = c('primary', 'secondary'),
  fraction  = c(1.0, 0.0)
)
transitions <- c(0, 0, 0.4, 0)
biomee_p_model_luluc_drivers$params_siml[[1]]$do_daily_diagnostics <- FALSE

n_lu <- length(lu_defs$name)
n_trans <- length(transitions) / n_lu ^ 2
biomee_p_model_luluc_drivers$init_lu[[1]] <- lu_defs
biomee_p_model_luluc_drivers$luc_forcing[[1]]  <- array(transitions, c(n_lu, n_lu, n_trans))

save(biomee_p_model_luluc_drivers,
     file ="data/biomee_p_model_luluc_drivers.rda",
     compress = "xz")





