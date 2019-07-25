
# Environment

# Load the package. This contains all the necessary wrapper functions to set up and run SOFUN and read its output. 
library(rsofun)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)

# Get site names of full FLUXNET2015 Tier 1 set
sitelist <- rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015$sitename

# Specify the data read from FLUXNET2015 files as a vector of variable names corresponding to their naming in the FLUXNET2015 datasets. 
getvars <- c( 
  "VPD_F",     # Mean daily vapour pressure deficit  hPa
  "VPD_F_DAY"  # Mean daytime vapour pressure deficit  hPa
  )

ddf <- purrr::map( 
  as.list(sitelist),
  ~get_obs_bysite_fluxnet2015( 
    sitename = ., 
    path_fluxnet2015 = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked/", 
    path_fluxnet2015_hh = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_0.5h/original/unpacked/",
    timescale = "d", 
    getvars = getvars, 
    getswc = FALSE,                              
    verbose = TRUE
    ) ) %>%
  bind_rows( .id = "sitename" )

