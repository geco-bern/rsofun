# This script evaluates the model and compares the target variables selected

# load packages
library(dplyr)
library(tibble)
library(rsofun)
library(ggplot2)
library(multidplyr)

# Evaluation for DBH mortality 
# DBH param 1,2,3 - Calibration for param 2
load("~/rsofun/data/inputs/df_drivers_DBH_gs.RData")
load("~/rsofun/data/inputs/settings_calib_DBH_gs_uniq_euler.RData")

df_drivers$params_species[[1]]$phiRL      <-  settings_calib_DBH_gs$par_opt["phiRL"]  
df_drivers$params_species[[1]]$LAI_light  <-  settings_calib_DBH_gs$par_opt["LAI_light"]
df_drivers$params_tile[[1]]$tf_base       <-  settings_calib_DBH_gs$par_opt["tf_base"]
df_drivers$params_tile[[1]]$par_mort      <-  settings_calib_DBH_gs$par_opt["par_mort"]
df_drivers$params_tile[[1]]$par_mort_under<-  settings_calib_DBH_gs$par_opt["par_mort_under"]

df_calib_DBH_gs <- runread_lm3ppa_f(
  df_drivers,
  makecheck = TRUE,
  parallel = FALSE
)

df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC")

# DBH p1
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea1sa1DBHp1gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea1sa1DBHp1gl_out_annual_cohorts.csv")

write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea2sa1DBHp1gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea2sa1DBHp1gl_out_annual_cohorts.csv")

write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea3sa1DBHp1gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea3sa1DBHp1gl_out_annual_cohorts.csv")

# DBH p2
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea1sa1DBHp2gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea1sa1DBHp2gl_out_annual_cohorts.csv")

write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea2sa1DBHp2gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea2sa1DBHp2gl_out_annual_cohorts.csv")

write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea3sa1DBHp2gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea3sa1DBHp2gl_out_annual_cohorts.csv")

# DBH p3
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea1sa1DBHp3gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea1sa1DBHp3gl_out_annual_cohorts.csv")

write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea2sa1DBHp3gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea2sa1DBHp3gl_out_annual_cohorts.csv")

write.csv(df_calib_DBH_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea3sa1DBHp3gl_out_annual_tile.csv")
write.csv(df_calib_DBH_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea3sa1DBHp3gl_out_annual_cohorts.csv")

# Compare targets before and after calibration - DBH

# Post
df_mod <- df_calib_DBH_gs$data[[1]]$output_annual_tile %>% 
  tail(df_drivers$params_siml[[1]]$nyeartrend) %>% 
  dplyr::summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Biomass=mean(plantC))

df_mod_sizedist <- df_calib_DBH_gs$data[[1]]$output_annual_cohorts %>%
  dplyr::filter(year>df_drivers$params_siml[[1]]$spinupyears) %>% 
  dplyr::filter(dbh>12.1) %>% mutate(size_bins = cut(dbh, breaks = sizedist)) %>%
  group_by(size_bins,year) %>% summarise(nTrees=sum(density)) %>% ungroup() %>% 
  group_by(size_bins) %>% summarise(nTrees=mean(nTrees))

dff <- data.frame(
  variables = c("GPP","LAI","Biomass","dbh_c1","dbh_c2","dbh_c3","dbh_c4","dbh_c5"),
  targets_mod = c(df_mod$GPP, df_mod$LAI, df_mod$Biomass,
                  df_mod_sizedist$nTrees[1],df_mod_sizedist$nTrees[2],df_mod_sizedist$nTrees[3],df_mod_sizedist$nTrees[4],df_mod_sizedist$nTrees[5])
) %>% dplyr::left_join(ddf_obs, by = "variables")

postcalibDBH <- dff %>% 
  ggplot() +
  geom_point(aes(x = targets_mod, y = targets_obs, col=variables)) +
  theme_classic()+labs(x = "Predicted", y = "Observed")+
  geom_abline(col="grey") + ggtitle("After calibration - DBH gs-Leuning") +
  facet_wrap(~variables,nrow = 4) + theme(legend.position = "none")

#Pre
preDBHp1gl_out_annual_tile <- read.csv("~/rsofun/data/outputs/preDBHp2gs_output_annual_tile.csv")
preDBHp1gl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/preDBHp2gs_output_annual_cohorts.csv")

df_mod <- preDBHp1gl_out_annual_tile %>% 
  tail(df_drivers$params_siml[[1]]$nyeartrend) %>% 
  dplyr::summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Biomass=mean(plantC))

df_mod_sizedist <- preDBHp1gl_out_annual_cohorts %>%
  dplyr::filter(year>df_drivers$params_siml[[1]]$spinupyears) %>% 
  dplyr::filter(dbh>12.1) %>% mutate(size_bins = cut(dbh, breaks = sizedist)) %>%
  group_by(size_bins,year) %>% summarise(nTrees=sum(density)) %>% ungroup() %>% 
  group_by(size_bins) %>% summarise(nTrees=mean(nTrees))

dff <- data.frame(
  variables = c("GPP","LAI","Biomass","dbh_c1","dbh_c2","dbh_c3","dbh_c4","dbh_c5"),
  targets_mod = c(df_mod$GPP, df_mod$LAI, df_mod$Biomass,df_mod_sizedist$nTrees[1],df_mod_sizedist$nTrees[2],df_mod_sizedist$nTrees[3],df_mod_sizedist$nTrees[4],df_mod_sizedist$nTrees[5])
) %>% dplyr::left_join(ddf_obs, by = "variables")

precalibDBH <- dff %>% 
  ggplot() +
  geom_point(aes(x = targets_mod, y = targets_obs, col=variables)) +
  theme_classic()+labs(x = "Predicted", y = "Observed") +
  geom_abline(col="grey") + ggtitle("Before calibration - DBH gs-Leuning") +
  facet_wrap(~variables,nrow = 4) + theme(legend.position = "none")

ff <- precalibDBH + postcalibDBH + plot_annotation(tag_levels = 'A')
ff
ggsave("~/rsofun/manuscript/figures/fig_ff.png", width = 8, height = 6, dpi=300)

# Evaluation for GR mortality 
# GR param 1,2,3 - Calibration for param 2
load("~/rsofun/data/inputs/df_drivers_GR_gs.RData")
load("~/rsofun/data/inputs/settings_calib_GR_gs_uniq_euler.RData")

df_drivers$params_species[[1]]$phiRL      <-  settings_calib_GR_gs$par_opt["phiRL"]
df_drivers$params_species[[1]]$LAI_light  <-  settings_calib_GR_gs$par_opt["LAI_light"]
df_drivers$params_tile[[1]]$tf_base       <-  settings_calib_GR_gs$par_opt["tf_base"]
df_drivers$params_tile[[1]]$par_mort      <-  settings_calib_GR_gs$par_opt["par_mort"]
df_drivers$params_tile[[1]]$par_mort_under<-  settings_calib_GR_gs$par_opt["par_mort_under"]

df_calib_GR_gs <- runread_lm3ppa_f(
  df_drivers,
  makecheck = TRUE,
  parallel = FALSE
)

df_calib_GR_gs$data[[1]]$output_annual_tile %>% 
  ggplot() +
  geom_line(aes(x = year, y = plantC)) +
  theme_classic()+labs(x = "Year", y = "plantC")

# GR p1
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea1sa1GRp1gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea1sa1GRp1gl_out_annual_cohorts.csv")

write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea2sa1GRp1gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea2sa1GRp1gl_out_annual_cohorts.csv")

write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea3sa1GRp1gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea3sa1GRp1gl_out_annual_cohorts.csv")

# GR p2
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea1sa1GRp2gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea1sa1GRp2gl_out_annual_cohorts.csv")

write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea2sa1GRp2gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea2sa1GRp2gl_out_annual_cohorts.csv")

write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea3sa1GRp2gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea3sa1GRp2gl_out_annual_cohorts.csv")

# GR p3
write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea1sa1GRp3gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea1sa1GRp3gl_out_annual_cohorts.csv")

write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea2sa1GRp3gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea2sa1GRp3gl_out_annual_cohorts.csv")

write.csv(df_calib_GR_gs$data[[1]]$output_annual_tile,   "~/rsofun/data/outputs/ea3sa1GRp3gl_out_annual_tile.csv")
write.csv(df_calib_GR_gs$data[[1]]$output_annual_cohorts,"~/rsofun/data/outputs/ea3sa1GRp3gl_out_annual_cohorts.csv")

# Compare targets before and after calibration - GR

# Post
df_mod <- df_calib_GR_gs$data[[1]]$output_annual_tile %>% 
  tail(df_drivers$params_siml[[1]]$nyeartrend) %>% 
  dplyr::summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Biomass=mean(plantC))

df_mod_sizedist <- df_calib_GR_gs$data[[1]]$output_annual_cohorts %>%
  dplyr::filter(year>df_drivers$params_siml[[1]]$spinupyears) %>% 
  dplyr::filter(dbh>12.1) %>% mutate(size_bins = cut(dbh, breaks = sizedist)) %>%
  group_by(size_bins,year) %>% summarise(nTrees=sum(density)) %>% ungroup() %>% 
  group_by(size_bins) %>% summarise(nTrees=mean(nTrees))

dff <- data.frame(
  variables = c("GPP","LAI","Biomass","dbh_c1","dbh_c2","dbh_c3","dbh_c4","dbh_c5"),
  targets_mod = c(df_mod$GPP, df_mod$LAI, df_mod$Biomass,df_mod_sizedist$nTrees[1],df_mod_sizedist$nTrees[2],df_mod_sizedist$nTrees[3],df_mod_sizedist$nTrees[4],df_mod_sizedist$nTrees[5])
) %>% dplyr::left_join(ddf_obs, by = "variables")

postcalibGR <- dff %>% 
  ggplot() +
  geom_point(aes(x = targets_mod, y = targets_obs, col=variables)) +
  theme_classic()+labs(x = "Predicted", y = "Observed")+
  geom_abline(col="grey") + ggtitle("After calibration - GR gs-Leuning") +
  facet_wrap(~variables,nrow = 4) + theme(legend.position = "none")

#Pre
preGRgl_out_annual_tile <- read.csv("~/rsofun/data/outputs/preGRp2gs_output_annual_tile.csv")
preGRgl_out_annual_cohorts <- read.csv("~/rsofun/data/outputs/preGRp2gs_output_annual_cohorts.csv")

df_mod <- preGRgl_out_annual_tile %>% 
  tail(df_drivers$params_siml[[1]]$nyeartrend) %>% 
  dplyr::summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Biomass=mean(plantC))

df_mod_sizedist <- preGRgl_out_annual_cohorts %>%
  dplyr::filter(year>df_drivers$params_siml[[1]]$spinupyears) %>% 
  dplyr::filter(dbh>12.1) %>% mutate(size_bins = cut(dbh, breaks = sizedist)) %>%
  group_by(size_bins,year) %>% summarise(nTrees=sum(density)) %>% ungroup() %>% 
  group_by(size_bins) %>% summarise(nTrees=mean(nTrees))

dff <- data.frame(
  variables = c("GPP","LAI","Biomass","dbh_c1","dbh_c2","dbh_c3","dbh_c4","dbh_c5"),
  targets_mod = c(df_mod$GPP, df_mod$LAI, df_mod$Biomass,
                  df_mod_sizedist$nTrees[1],df_mod_sizedist$nTrees[2],df_mod_sizedist$nTrees[3],df_mod_sizedist$nTrees[4],df_mod_sizedist$nTrees[5])
) %>% dplyr::left_join(ddf_obs, by = "variables")

precalibGR <- dff %>% 
  ggplot() +
  geom_point(aes(x = targets_mod, y = targets_obs, col=variables)) +
  theme_classic()+labs(x = "Predicted", y = "Observed")+
  geom_abline(col="grey") + ggtitle("Before calibration - GR gs-Leuning") +
  facet_wrap(~variables,nrow = 4) + theme(legend.position = "none")

gg <- precalibGR + postcalibGR + plot_annotation(tag_levels = 'A')
gg
ggsave("~/rsofun/manuscript/figures/fig_gg.png", width = 8, height = 6, dpi=300)

