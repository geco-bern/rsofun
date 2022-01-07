# This script calibrates the model for the target variables selected

# load packages
library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(multidplyr)

# Get ddf_obs

# Target 1. GPP data from FluxNet 
Fluxnet <- read_csv("~/data/FLUXNET-2015_Tier1/20191024/YY/FLX_CH-Lae_FLUXNET2015_FULLSET_YY_2004-2014_1-3.csv")
GPP <- Fluxnet[,c("TIMESTAMP","GPP_NT_VUT_REF")] # g C/m2/yr
GPP <- GPP %>% mutate(GPP_KgC = GPP_NT_VUT_REF/1000) # Convert in Kg C/m2/yr as in the LM3-PPA model
mean_annual_gpp <- mean(GPP$GPP_KgC) #Kg C/m2/yr

# Target 2. LAI 
lai_LAE <- read_csv("~/data/modis_subsets/MODIS_LAI_MCD15A3H_daily_CH-Lae.csv")
lai_LAE <- lai_LAE %>% mutate(month=str_sub(date, 6, 7))
lai_LAE_summer <- lai_LAE %>% dplyr::filter(month=="06"|month=="07"|month=="08")
mean_annual_lai <- mean(lai_LAE_summer$linear, na.rm=T)
max_annual_lai <- quantile(lai_LAE_summer$linear, probs = 0.95, na.rm=T) # Use max/q95 and mean LAI across june july august

# Target 3. Total Stand Biomass 
# ln(Biomass) = ln(b0) + b1 ln(DBH); Biomass = b0*DBH^b1
LAE_data <- read_csv("~/data/LWF/Laegeren/20201216_Laegeren_data.csv") # Data from Vova
# Calculate DBH from UMFANG is the circumference (mm) C=2*pi*r. See there are many NA in the variable recorded BHD (dbh).
LAE_data <- LAE_data %>% mutate(DBH_cm = UMFANG/pi)
sort(unique(LAE_data$SPECIES))
load("~/data/biomass/d_param.rda")
d_param_lnb0_AG <- d_param %>% dplyr::filter(equation_n==3,parameter_id==1,component_n=="Aboveground",parameter=="lnb0") %>% rename(SPECIES=species, lnb0_AG=value)
d_param_b1_AG <- d_param %>% dplyr::filter(equation_n==3,parameter_id==1,component_n=="Aboveground",parameter=="b1") %>% rename(SPECIES=species, b1_AG=value)
d_param_lnb0_BG <- d_param %>% dplyr::filter(equation_n==3,parameter_id==1,component_n=="Root mass",parameter=="lnb0") %>% rename(SPECIES=species, lnb0_BG=value)
d_param_b1_BG <- d_param %>% dplyr::filter(equation_n==3,parameter_id==1,component_n=="Root mass",parameter=="b1") %>% rename(SPECIES=species, b1_BG=value)
LAE_data_bio <- LAE_data %>% left_join(d_param_lnb0_AG[,c(1,8)]) %>% left_join(d_param_b1_AG[,c(1,8)]) %>% left_join(d_param_lnb0_BG[,c(1,8)])  %>% left_join(d_param_b1_BG[,c(1,8)]) %>% mutate(lnb0_AG=ifelse(is.na(lnb0_AG),mean(d_param_lnb0_AG$lnb0_AG,na.rm=T),lnb0_AG)) %>% mutate(b1_AG=ifelse(is.na(b1_AG),mean(d_param_b1_AG$b1_AG,na.rm=T),b1_AG)) %>% mutate(lnb0_BG=ifelse(is.na(lnb0_BG),mean(d_param_lnb0_BG$lnb0_BG,na.rm=T),lnb0_BG)) %>% mutate(b1_BG=ifelse(is.na(b1_BG),mean(d_param_b1_BG$b1_BG,na.rm=T),b1_BG)) 
# Filter trees with DBH >= 12 cm
LAE_data_bio <- LAE_data_bio %>% dplyr::filter(DBH_cm >= 12)
# Calcualte Biomass
LAE_data_bio <- LAE_data_bio %>% mutate(AGB_Kg=exp(lnb0_AG)*DBH_cm^b1_AG) %>% mutate(BGB_Kg=exp(lnb0_BG)*DBH_cm^b1_BG) %>% 
  mutate(TreeBiomass_Kg=AGB_Kg+BGB_Kg)
LAE_data_bio_allsps <- LAE_data_bio %>% group_by(INVYEAR) %>% summarise(TotalBiomass_Kg=sum(TreeBiomass_Kg)) # Summarize by INVYEAR
LAE_m2 <- 13400 # Area of Laegeren plot in m2
BiomassLAE <- mean(LAE_data_bio_allsps$TotalBiomass_Kg)/LAE_m2

# Target 4. Density and size distribution
LAE_data <- read_csv("~/data/LWF/Laegeren/20201216_Laegeren_data.csv") # Data from Vova
LAE_data <- LAE_data %>% mutate(DBH_cm = UMFANG/pi)
LAE_data %>% group_by(INVYEAR) %>% summarise(nTrees=n())
LAE_nTrees <- LAE_data %>% dplyr::filter(DBH_cm >= 12.0)
# For all tree species
LAE_allsps <- LAE_nTrees %>% group_by(INVYEAR) %>% summarise(nTrees=n())
LAE_ha <- 1.34 # Area of Laegeren plot in ha
densityLAE <- mean(LAE_allsps$nTrees)/LAE_ha
# Size distribution
LAE_nTrees <- LAE_nTrees %>% mutate(size_bins = cut(DBH_cm, breaks = quantile(DBH_cm, probs = seq(0, 1, 0.2)),include.lowest=TRUE))
LAE_size_dist <- LAE_nTrees %>% group_by(size_bins,INVYEAR) %>% 
  summarise(nTrees=n()) %>% ungroup() %>% group_by(size_bins) %>% summarise(nTrees=mean(nTrees)/LAE_ha)
ggplot(LAE_size_dist, aes(size_bins, nTrees)) + geom_col()

# Prepare the observed target variables: ddf_obs
ddf_obs <- data.frame(
  variables = c("GPP","LAI","Biomass","dbh_c1","dbh_c2","dbh_c3","dbh_c4","dbh_c5"),
  targets_obs = c(mean_annual_gpp, max_annual_lai, BiomassLAE,
                  LAE_size_dist$nTrees[1],LAE_size_dist$nTrees[2],LAE_size_dist$nTrees[3],LAE_size_dist$nTrees[4],LAE_size_dist$nTrees[5])
) 

save(ddf_obs, file = "~/rsofun/data/inputs/ddf_obs.RData")
load("~/rsofun/data/inputs/ddf_obs.RData")

# Calibration for DBH mortality (see euler_dbh_gs.R)
load("~/rsofun/data/inputs/df_drivers_DBH_gs.RData")
load("~/rsofun/data/inputs/ddf_obs.RData")

settings_calib_DBH_gs <- list(
  method              = "gensa",
  targetvars          = c("targets_obs"),
  timescale           = list(targets_obs = "y"),
  maxit               = 2000, 
  sitenames           = "CH-Lae",
  metric              = "rmse",
  dir_results         = "./",
  name                = "ORG",
  par                 = list(phiRL = list(lower=0.5, upper=5, init=3.5),
                             LAI_light = list(lower=2, upper=7, init=3.5),
                             tf_base = list(lower=0.2, upper=1.5, init=1),
                             par_mort = list(lower=0.1, upper=2, init=1),
                             par_mort_under = list(lower=0.1, upper=2, init=1))
)

set.seed(1152)
settings_calib_DBH_gs <- calib_sofun(
  df_drivers = df_drivers,  
  ddf_obs = ddf_obs,
  settings = settings_calib_DBH_gs
)

save(settings_calib_DBH_gs, file = "~/rsofun/data/inputs/settings_calib_DBH_gs_uniq_euler.RData")

# Calibration for GR mortality (see euler_gr_gs.R)
load("~/rsofun/data/inputs/df_drivers_GR_gs.RData")
load("~/rsofun/data/inputs/ddf_obs.RData")

settings_calib_GR_gs <- list(
  method              = "gensa",
  targetvars          = c("targets_obs"),
  timescale           = list(targets_obs = "y"),
  maxit               = 2000, 
  sitenames           = "CH-Lae",
  metric              = "rmse",
  dir_results         = "./",
  name                = "ORG",
  par                 = list(phiRL = list(lower=0.5, upper=5, init=3.5),
                             LAI_light = list(lower=2, upper=7, init=3.5),
                             tf_base = list(lower=0.2, upper=1.5, init=1),
                             par_mort = list(lower=0.1, upper=2, init=1),
                             par_mort_under = list(lower=0.1, upper=2, init=1))
)

set.seed(1152)
settings_calib_GR_gs <- calib_sofun(
  df_drivers = df_drivers,  
  ddf_obs = ddf_obs,
  settings = settings_calib_GR_gs
)

save(settings_calib_GR_gs, file = "~/rsofun/data/inputs/settings_calib_GR_gs_uniq_euler.RData")

