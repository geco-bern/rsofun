rm(list=ls())
library(tidyverse)
library(reshape2)
library(rsofun)
library(BayesianTools)
library(tictoc)
library(ncdf4)
library(scatterPlotMatrix)

plot_only = F
source("vignettes/read_meta_fdk.R")

root_data_dir = "~/Downloads/fluxdatakit_oct3"
lsm_path = paste0(root_data_dir, "/FLUXDATAKIT_LSM/")
out_dir = "~/Desktop/phydro_output"
data_dir = paste0(root_data_dir, "/phydro_drivers/")
figures_dir = paste0(out_dir, "/figures/")

files_lsm = list.files(lsm_path)
sites = files_lsm %>% substr(1,6) %>% unique()


sites_meta =
  sites %>%
  map_df(
    ~suppressWarnings(
      try(
        read_meta_fdk(
          site = .,
          path = lsm_path,
          meta_data = T
        )
      )
    )
  )

sites_meta %>% write.csv(file="vignettes/ancillary_data/sites_meta.csv")

sites_meta = read.csv(file="vignettes/ancillary_data/sites_meta.csv")
valid_years = read.csv("vignettes/ancillary_data/valid_years_final.csv", header = T, fileEncoding = "UTF-16")

sites_meta %>% ggplot(aes(x=IGBP_veg_short)) + geom_bar() + coord_flip()

site_list = 
  valid_years %>% 
  rename(sitename = Site,
         valid_start_year = start_year,
         valid_end_year = end_year) %>% 
  left_join(sites_meta) %>% 
  select(-X, -comment) %>% 
  filter(!is.na(valid_start_year) & !is.na(valid_end_year)) %>% # Remove sites that dont have any valid years
  filter((IGBP_veg_short %in% c("WSA", "SAV", "OSH", "GRA", "ENF", "EBF", "DBF", "CSH")))

site_list %>% pull(sitename) %>% write.table(file = "vignettes/site_list_2.txt", row.names = F, quote = F, col.names = F)
