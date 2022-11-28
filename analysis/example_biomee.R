library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(patchwork)
library(multidplyr)

sitename <- "CH-Lae"

# Take only year 2004 to 2014, corresponding to subset of data for site CH-Lae
site_info <- tibble(
  sitename="CH-Lae",
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
  plant_functional_type = "Broadleaf trees"
  )

site_info <- site_info %>% 
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

params_siml <- tibble(
  spinup                = TRUE,
  spinupyears           = 700, 
  recycle               = 800,    # 9 or 11 changed to 1 when aggregating forcing into 1 year
  firstyeartrend        = 2009, 
  nyeartrend            = 800,    # 9 or 11 (longer transient years)
  outputhourly          = TRUE,
  outputdaily           = TRUE,
  do_U_shaped_mortality = TRUE,
  update_annualLAImax   = TRUE,
  do_closedN_run        = TRUE,
  method_photosynth     = "pmodel", # gs_leuning or pmodel
  method_mortality      = "dbh" # dbh or cstarvation or growthrate or const_selfthing
  )

params_tile <- tibble(
  soiltype     = 3,     # Sand = 1, LoamySand = 2, SandyLoam = 3, SiltLoam = 4, FrittedClay = 5, Loam = 6, Clay = 7
  FLDCAP       = 0.4,   # soil property: field capacity 
  WILTPT       = 0.05,  # soil property: wilting point
  K1           = 2.0,   # turnover rate of fast SOM per year
  K2           = 0.05,  # turnover rate of slow SOM per year
  K_nitrogen   = 8.0,   # mineral Nitrogen turnover rate
  MLmixRatio   = 0.8,   # the ratio of C and N returned to litters from microbes
  etaN         = 0.025, # loss rate with runoff
  LMAmin       = 0.02,  # minimum LMA, boundary condition
  fsc_fine     = 1.0,   # fraction of fast turnover carbon in fine biomass
  fsc_wood     = 0.0,   # fraction of fast turnover carbon in wood biomass
  GR_factor    = 0.33,  # growth respiration factor
  l_fract      = 0.0,   # fraction of the carbon retained after leaf drop
  retransN     = 0.0,   # retranslocation coefficient of Nitrogen
  f_initialBSW = 0.2,
  f_N_add      = 0.02,   # re-fill of N for sapwood
  
  # add calibratable params
  tf_base        = 1,
  par_mort       = 1,    # param_dbh=1 param_csv=1 param_gr=1 CAI_MAX=2
  par_mort_under = 1
  )

params_species <- tibble(
  
  lifeform      = rep(1,16),                      # 0 for grasses; 1 for trees
  phenotype     = c(0,1,1,rep(1,13)),             # 0 for Deciduous; 1 for Evergreen
  pt            = rep(0,16),                      # 0 for C3; 1 for C4
  # Root parameters
  alpha_FR      = rep(1.2,16),                    # Fine root turnover rate yr-1
  rho_FR        = rep(200,16),                    # material density of fine roots (kgC m-3)
  root_r        = rep(2.9E-4,16), 
  root_zeta     = rep(0.29,16), 
  Kw_root       = rep(3.5e-09,16),               # mol /(s m2 Mpa)
  leaf_size     = rep(0.04,16), 
  
  # Photosynthesis parameters
  Vmax          = rep(35.0E-6,16),               # mol m-2 s-1
  Vannual       = rep(1.2,16),                   # kgC m-2 yr-1
  wet_leaf_dreg = rep(0.3,16),                   # wet leaf photosynthesis down-regulation: wet leaf is 30% less than dry leaf
  m_cond        = rep(7.0,16), 
  alpha_phot    = rep(0.06,16), 
  gamma_L       = rep(0.02,16), 
  gamma_LN      = rep(70.5 ,16),  # kgC kgN-1 yr-1
  gamma_SW      = rep(0.08,16),   # kgC m-2 Acambium yr-1
  gamma_FR      = rep(12.0,16),   # kgC kgN-1 yr-1
  tc_crit       = rep(283.16,16),   # OFF
  tc_crit_on    = rep(280.16,16),   # ON
  gdd_crit      = rep(280.0,16),   # Simulations 280, 240, 200
  
  seedlingsize  = rep(0.05,16),                   # initial size of seedlings #In Ensheng BiomeE: 0.05
  LNbase        = rep(0.8E-3,16),                 # kgN m-2 leaf, Vmax = 0.03125*LNbase
  lAImax        = rep(3.5,16),                    # maximum crown LAI
  Nfixrate0     = rep(0,16),                      # 0.03 kgN kgRootC-1 yr-1
  NfixCost0     = rep(12,16),                     # 12, 24 gC/gN
  phiCSA        = rep(0.25E-4,16),                # ratio of sapwood area to leaf area
  mortrate_d_c  = rep(0.01,16),                   # canopy tree mortality rate, year-1
  mortrate_d_u  = rep(0.075,16),                  # understory tree mortality rate, year-1
  maturalage    = rep(5,16),                      # the age that can reproduce
  fNSNmax       = rep(5,16),                      # multiplier for NSNmax as sum of potential bl and br
  LMA           = c(0.05,0.17,0.11,rep(0.1,13)),  # Leaf mass per unit area. For sps: Beech-Spruce-Fir # In Ensheng rep(0.035,16)
  rho_wood      = c(590,370,350,rep(300,13)),     # In Ensheng rep(300,16),   # c(590,370,350,rep(300,13)),
  alphaBM       = rep(5200,16),                   #c(0.19,0.15,0.09,rep(0.15,13)), # In Ensheng BiomeE: 5200.0 
  thetaBM       = c(2.36,2.30,2.54,rep(2.30,13)), # In Ensheng BiomeE: 2.5 rep(2.5,16),
  
  # add calibratable params
  kphio         = rep(0.05,16),
  phiRL         = rep(3.5,16),
  LAI_light     = rep(3.5,16)               # Light-limited crown LAI
  
  ) 

params_soil <- tibble(
  type              = c("Coarse","Medium","Fine","CM","CF","MF","CMF","Peat","MCM"),
  GMD               = c(0.7, 0.4, 0.3, 0.1, 0.1, 0.07, 0.007, 0.3, 0.3),
  GSD               = c(5.0, 5.3, 7.4, 6.1, 6.1, 14.0, 15.0, 7.4, 7.4),
  vwc_sat           = c(0.380, 0.445, 0.448, 0.412, 0.414, 0.446, 0.424, 0.445, 0.445),
  chb               = c(3.5,6.4,11.0,4.8,6.3,8.4,6.3,6.4,6.4),
  psi_sat_ref       = c(-600, -790, -910, -1580, -1680, -1880, -5980, -790, -790), # Pa
  k_sat_ref         = c(130.8, 75.1, 53.2, 12.1, 11.1, 12.7, 1.69, 53.2, 53.2), # mol/(s MPa m)
  alphaSoil         = rep(1, 9),
  heat_capacity_dry = c(1.2e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.1e6, 1.4e6, 1.0)
  )

init_cohort <- tibble(
 init_cohort_species = rep(1, 10),   # indicates sps # 1 - Fagus sylvatica
 init_cohort_nindivs = rep(0.05,10),  # initial individual density, individual/m2 ! 1 indiv/m2 = 10.000 indiv/ha
 init_cohort_bsw     = rep(0.05,10), # initial biomass of sapwood, kg C/individual
 init_cohort_bHW     = rep(0.0, 10), # initial biomass of heartwood, kg C/tree
 init_cohort_nsc     = rep(0.05,10)  # initial non-structural biomass
)

init_soil <- tibble( #list
 init_fast_soil_C    = 0.0,    # initial fast soil C, kg C/m2
 init_slow_soil_C    = 0.0,    # initial slow soil C, kg C/m2
 init_Nmineral       = 0.015,  # Mineral nitrogen pool, (kg N/m2)
 N_input             = 0.0008  # annual N input to soil N pool, kgN m-2 yr-1
)


df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)


load("data-raw/CH-LAE_forcing.rda")

if (params_siml$method_photosynth == "gs_leuning"){
  forcingLAE <- forcingLAE %>% 
    dplyr::group_by(lubridate::month(datehour),lubridate::day(datehour),lubridate::hour(datehour)) %>% 
    summarise_at(vars(1:13), list(~mean(., na.rm = TRUE)))
  forcing <- forcingLAE[,-c(1:3)]
  forcing <- bind_rows(replicate(800, forcing, simplify = FALSE)) # Duplicate for the # of transient years
  
} else if (params_siml$method_photosynth == "pmodel"){ #&& dt_secs != (60*60*24)){
  forcingLAE <- forcingLAE %>% 
    dplyr::group_by(lubridate::month(datehour),lubridate::day(datehour)) %>% 
    summarise_at(vars(1:13), list(~mean(., na.rm = TRUE)))
  forcing <- forcingLAE[,-c(1:2)]
  forcing <- bind_rows(replicate(800, forcing, simplify = FALSE)) # Duplicate for the # of transient years
}


if (params_siml$method_photosynth == "gs_leuning"){
  forcing <- forcing %>% mutate(Swdown = Swdown*1) # levels = *1, *1.15 and *1.30
  #forcing <- forcing %>% mutate(aCO2_AW = aCO2_AW*1.30) # levels = *1, *1.15 and *1.30
} else if (params_siml$method_photosynth == "pmodel"){ 
  forcing <- forcing %>% mutate(PAR = PAR*1) # levels = *1, *1.15 and *1.30
}

print(packageVersion("rsofun"))

## for versions above 4.0
df_drivers <- tibble(sitename,
                    site_info = list(tibble(site_info)),
                    params_siml = list(tibble(params_siml)),
                    params_tile = list(tibble(params_tile)),
                    params_species=list(tibble(params_species)),
                    params_soil=list(tibble(params_soil)),
                    init_cohort=list(tibble(init_cohort)),
                    init_soil=list(tibble(init_soil)),
                    forcing=list(tibble(forcing)),
                    .name_repair = "unique")

out <- run_biomee_f_bysite( sitename,
                            params_siml,
                            site_info,
                            forcing, # ddf_input
                            params_tile,
                            params_species,
                            params_soil,
                            init_cohort,
                            init_soil,
                            makecheck = TRUE
                            )

gg1 <- out$output_annual_tile %>%
  ggplot() +
  geom_line(aes(x = year, y = GPP)) +
  theme_classic()+labs(x = "Year", y = "GPP")

gg2 <- out$output_annual_tile %>%
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC")

print("Writing luxembourg.pdf")
print(gg1/gg2)
ggsave("luxembourg.pdf")
