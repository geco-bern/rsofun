rm(list=ls())
library(tidyverse)
library(reshape2)
library(rsofun)
library(BayesianTools)
library(tictoc)
library(ncdf4)
library(scatterPlotMatrix)
library(ggpointdensity)

plot_only = F

source("vignettes/read_meta_fdk.R")

root_data_dir = "~/Downloads/fluxdatakit_oct3"
lsm_path = paste0(root_data_dir, "/FLUXDATAKIT_LSM/")
out_dir = "~/Desktop/phydro_output_6_p50priors/"
data_dir = paste0(root_data_dir, "/phydro_drivers/")
figures_dir = paste0(out_dir, "/figures/")

sites = read.delim("vignettes/site_list.txt", header=F)

# sites_meta = 
# sites %>% # slice(1:5) %>% 
#   pull(V1) %>% 
#   map_df(
#     ~suppressWarnings(
#       try(
#         read_meta_fdk(
#           site = .,
#           path = lsm_path,
#           meta_data = T
#         )
#       )
#     )
#   )
# 
# sites_meta %>% write.csv(file="vignettes/ancillary_data/sites_meta.csv")

sites_meta = read.csv(file="vignettes/ancillary_data/sites_meta.csv")

sites_meta = sites_meta %>% 
  mutate(IGBP_veg_short = case_match(IGBP_veg_short,
                                 "Woody Savannas" ~ "WSA",
                                 "Savannas" ~ "SAV",
                                 "Permanent Wetlands" ~ "WET",
                                 "Open Shrublands" ~ "OSH",
                                 "Closed Shrublands" ~ "CSH",
                                 "Grasslands" ~ "GRA",
                                 "Evergreen Needleleaf Forest" ~ "ENF",
                                 "Evergreen Broadleaf Forest" ~ "EBF",
                                 "Deciduous Broadleaf Forest" ~ "DBF",
                                 "Mixed Forest" ~ "MF",
                                 "Croplands" ~ "CRO",
                                 "Cropland/Natural Vegetation Mosaic" ~ "MF",
                                 "Urban and Built-Up" ~ NA,
                                 .default = IGBP_veg_short))

sites_meta %>% ggplot(aes(x=IGBP_veg_short)) + geom_bar() + coord_flip()


map_all_sites = 
tibble(filename=list.files(out_dir, full.names = T)) %>% 
  filter(str_detect(filename, "MAP.csv")) %>% 
  pull(filename) %>% 
  map_df(~read.csv(.))


all_data = map_all_sites %>% 
  rename(sitename = site) %>%
  left_join(sites_meta) 


## FANCY PAIRS PLOT ##
all_data %>% 
  select(phydro_K_plant:whc, IGBP_veg_short) %>% 
  mutate(phydro_K_plant = log10(phydro_K_plant),
         whc = log10(whc),
         Ssoil = log10(Ssoil),
         # phydro_p50_plant = log10(-phydro_p50_plant),
         # phydro_alpha = log10(phydro_alpha)
         ) %>% 
  filter(IGBP_veg_short %in% c("SAV", "OSH", "GRA", "ENF", "EBF", "DBF", "CRO")) %>% 
  mutate(IGBP_veg_short = factor(IGBP_veg_short)) %>% 
  scatterPlotMatrix(distribType = 1, regressionType = 1, rotateTitle = F, zAxisDim = "IGBP_veg_short",
                    plotProperties = list(regression = list( strokeWidth = 2 )),
                    columnLabels = gsub("\\_", "<br>", colnames(.)) )


all_data %>% ggplot(aes(x=IGBP_veg_short)) + geom_bar() + coord_flip()

all_data %>% group_by(IGBP_veg_short) %>% summarize(count=length(IGBP_veg_short)) %>% 
right_join(all_data) %>% 
  filter(count > 2) %>% 
  select(kphio:whc, IGBP_veg_short, sitename) %>% 
  mutate(phydro_K_plant = log10(phydro_K_plant),
         whc = log10(whc),
         Ssoil = log10(Ssoil),
         # kphio = log10(kphio),
         # phydro_gamma = log10(phydro_gamma)
         ) %>%
  pivot_longer(cols = !c(sitename, IGBP_veg_short)) %>% 
  ggplot(aes(x=IGBP_veg_short, y=value, fill=IGBP_veg_short)) + 
  geom_violin() + 
  scale_fill_viridis_d()+
  facet_wrap(facets = "name", scales = "free") +
  coord_flip() 

all_data %>% 
  filter(IGBP_veg_short %in% c("SAV", "OSH", "GRA", "ENF", "EBF", "DBF", "CRO")) %>% 
  ggplot(aes(group=IGBP_veg_short, x=Ssoil, y=whc, col=IGBP_veg_short)) + 
  geom_point()+
  geom_smooth(method="lm", se=F)+
  scale_x_log10()+scale_y_log10()+
  geom_abline(slope = 1)


r2_all_sites = tibble(filename=list.files(out_dir, full.names = T)) %>% 
  filter(str_detect(filename, "r2_nrmse.csv")) %>% 
  pull(filename) %>% 
  map_df(~read.csv(.)) 

world <- map_data("world")
r2_all_sites %>% 
  left_join(sites_meta, by=c("site"="sitename")) %>% 
  ggplot() +
  theme_classic()+
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill= "grey90", size=0.05
  ) +
  geom_point(aes(x=longitude, y=latitude, color=r2))+
  scale_color_gradient2(mid = "cyan", midpoint = 0.5, low = "red", high="blue", limits=c(0,1))+
  facet_wrap(~variable, nrow = 2)

r2_all_sites %>% 
  left_join(sites_meta, by=c("site"="sitename")) %>% 
  select(variable, r2, IGBP_veg_short) %>% 
  ggplot(aes(x=IGBP_veg_short, y=r2, fill=IGBP_veg_short)) + 
  geom_violin(scale = "width", width=0.4) + 
  facet_wrap(~variable, nrow=2)
  

#### Water potentials analysis ####

vod_day = readxl::read_excel("~/Downloads/AMSR_fluxnet_data.xlsx", sheet = "VOD_ASC_filtered_qa(1_30_PM)")
vod_day = vod_day %>% 
  pivot_longer(-c(1,2), names_to="date", values_to = "vod_day") %>% 
  mutate(date = as.Date(as.numeric(date), origin="1899-12-30")) %>% 
  mutate(vod_day = case_match(vod_day, -999~NA, .default=vod_day))

vod_night = readxl::read_excel("~/Downloads/AMSR_fluxnet_data.xlsx", sheet = "VOD_DSC_filtered_qa(1_30_AM)")
vod_night = vod_night %>% 
  pivot_longer(-c(1,2), names_to="date", values_to = "vod_night") %>% 
  mutate(date = as.Date(as.numeric(date), origin="1899-12-30")) %>% 
  mutate(vod_night = case_match(vod_night, -999~NA, .default=vod_night))

vod = vod_day %>% left_join(vod_night)

read_phydro_output = function(site){
  file = tibble(files=list.files(out_dir, full.names = T)) %>% 
    filter(stringr::str_detect(files, site)) %>% 
    filter(stringr::str_detect(files, "phydro_output.rda")) %>% 
    pull(files)

  load(file)
  
  output_p_opt$data[[1]] %>% 
    select(date, gpp, le, jmax25, dpsi, psi_leaf, gpp_obs, le_obs) %>% 
    mutate(SITE_ID = site)
}

read_obs = function(site){
  file_obs = tibble(files=list.files(data_dir, full.names = T)) %>% 
    filter(stringr::str_detect(files, site)) %>% 
    filter(stringr::str_detect(files, "p_hydro_validation.rda")) %>% 
    pull(files)
  
  load(file_obs)
  
  p_hydro_validation$data[[1]] 
}


sites_phydro = tibble(files=list.files(out_dir)) %>% 
  filter(stringr::str_detect(files, "phydro_output.rda")) %>% 
  mutate(site = substr(files, 1,6)) %>% 
  # slice(1:5) %>%
  pull(site)

phydro_out = sites_phydro %>% 
  map_df(~read_phydro_output(.))

vod_phydro = vod %>% left_join(phydro_out) %>% 
  drop_na

sites_vod_phydro = vod_phydro %>% group_by(SITE_ID) %>% count() %>% arrange(desc(n)) %>% pull(SITE_ID)

for (i in seq(1,length(sites_vod_phydro), by=20)){
  print(
  vod_phydro %>% 
    filter(SITE_ID %in% sites_vod_phydro[i:(i+20-1)]) %>% 
    mutate(vod_ratio = log(vod_night/vod_day)) %>% 
    ggplot(aes(y=vod_ratio, x=le_obs)) +
    geom_pointdensity() + 
    geom_smooth(method="lm", formula=y~0+x, col="orange", linewidth=0.5)+
    scale_colour_viridis_c()+
    facet_wrap("SITE_ID", scales="free")+
    guides(color="none")
  )
}


#### Isohydricity analysis ####

sites_phydro = phydro_out %>% group_by(SITE_ID) %>% count() %>% arrange(desc(n)) %>% pull(SITE_ID)

site_min_psisoil = 
  phydro_out %>% 
  group_by(SITE_ID) %>% 
  summarize(psi_min = min(psi_leaf+dpsi))

for (i in seq(1,length(sites_phydro), by=20)){
  cairo_pdf(filename = paste0("vignettes/figures/psi_leaf_vs_soil_",i,".pdf"))
  print(
  phydro_out %>% 
    filter(SITE_ID %in% sites_phydro[i:(i+20-1)]) %>% 
    left_join(site_min_psisoil) %>% 
    mutate(psi_soil = psi_leaf+dpsi) %>%  
    filter(psi_soil > psi_min*0.8) %>% 
    group_by(SITE_ID) %>% 
    ggplot(aes(y=psi_leaf, x=psi_soil)) +
    geom_pointdensity() + 
    geom_smooth(method="lm", col="orange", linewidth=0.5)+
    geom_abline(slope=1, intercept=0, col="white")+
    scale_colour_viridis_c()+
    facet_wrap("SITE_ID", scales="free")+
    guides(color="none")
  )
  dev.off()
}



phydro_out %>% 
  left_join(site_min_psisoil) %>% 
  mutate(psi_soil = psi_leaf+dpsi) %>%  
  filter(psi_soil > psi_min*0.8) %>% 
  group_by(SITE_ID) %>% 
  group_modify(~broom::tidy(lm(psi_leaf~psi_soil, data=.x))) %>% 
  filter(estimate < 10 & estimate > -10) %>% 
  ggplot()+
  geom_histogram(aes(x=estimate), bins=100) +
  geom_vline(xintercept = 1, col="red")+
  facet_wrap("term", scales="free")


phydro_out %>% 
  left_join(site_min_psisoil) %>% 
  mutate(psi_soil = psi_leaf+dpsi) %>%  
  filter(psi_soil > psi_min*0.8) %>% 
  group_by(SITE_ID) %>% 
  group_modify(~broom::tidy(lm(psi_leaf~psi_soil, data=.x))) %>% 
  filter(estimate < 10 & estimate > -10) %>% 
  pivot_wider(id_cols = SITE_ID, names_from = term, values_from = estimate) %>% 
  ggplot(aes(x=`(Intercept)`, y=psi_soil))+
  geom_point() +
  geom_smooth(method="lm")
