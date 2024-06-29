library(FluxDataKit)
library(tidyverse)
library(reshape2)

dat = data.frame(site=NULL, meta=NULL)

sites = c(
  "CH-Dav",
  "DE-Hai",
  "DE-Tha",
  "FR-LBr",
  "FR-Pue",
  "GF-Guy",
  "NL-Loo",
  "US-Me2",
  "US-NR1",
  "US-Ton"
)

site = sites[1]

lsm_path = "~/Downloads/fluxdatakit_oct3/FLUXDATAKIT_LSM/"
csv_path = "~/Downloads/fluxdatakit_oct3/FLUXDATAKIT_FLUXNET/"
out_path = "~/Downloads/fluxdatakit_oct3/Phydro_drivers/"

files_csv = list.files(csv_path)
files_lsm = list.files(lsm_path)

for (site in sites){

  # Get filename for HH data for matching site
  file_csv = files_csv[intersect(grep(site, files_csv), 
                                 grep("HH", files_csv))]
  
  # get metadata
  # --------------------------------------------------------
  message("- reading Metadata for site")
  meta <- suppressWarnings(
    try(
      fdk_convert_lsm(
        site = site,
        fluxnet_format = TRUE,
        path = lsm_path,
        meta_data = T
      )
    )
  )
  
  dat = rbind(dat, data.frame(site=site, meta=meta))
}

write.csv(dat, file = paste0(out_path, "/sites_meta.csv"))


dat = read.csv(file = paste0(out_path, "/sites_meta.csv"), header=T)
params = read.csv("~/codes/rsofun/vignettes/estimated_params_fluxnet.csv", header=T)


dat %>% 
  select(site, meta.IGBP_veg_short) %>% 
  left_join(params) %>% 
  select(-site) %>% 
  melt("meta.IGBP_veg_short") %>% 
  ggplot(aes(y=value, x=meta.IGBP_veg_short)) + 
  geom_point() + 
  facet_wrap("variable", scales = "free")

dat %>% 
  select(site, meta.canopy_height) %>% 
  left_join(params) %>% 
  select(-site) %>% 
  melt("meta.canopy_height") %>% 
  drop_na %>% 
  ggplot(aes(y=value, x=meta.canopy_height)) + 
  geom_point() + 
  facet_wrap("variable", scales = "free")


