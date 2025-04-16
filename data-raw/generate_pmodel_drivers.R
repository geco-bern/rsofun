#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(zen4R)

# load script arguments
args <- commandArgs(trailingOnly = TRUE)

# load data
#   Download FluxDataKit data from Zenodo:
#   sudo apt install librdf0-dev
#   install.packages("zen4R")
download_path <- tempdir()
dir.create(download_path, recursive = TRUE)
zen4R::download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "rsofun_driver_data_v3.4.2.rds") # v3.4.2
FDK_published_rsofun_driver_data <- readRDS(file.path(download_path, "rsofun_driver_data_v3.4.2.rds"))

#---- p_model_drivers (forcing data) -----
p_model_drivers <- FDK_published_rsofun_driver_data |> 
  
  # subset to single site
  dplyr::filter(sitename == "FR-Pue") |> 
  
  # subset dates
  dplyr::mutate(forcing = purrr::map(forcing, ~dplyr::filter(., lubridate::year(date) %in% 2007:2012))) |> 
  
  # select variables for P-model simulation  
  dplyr::mutate(forcing = purrr::map(forcing, ~dplyr::select(.,
    date, 
    temp, 
    vpd, 
    ppfd, 
    netrad, 
    patm,
    snow, 
    rain, 
    tmin, 
    tmax, 
    fapar, 
    co2,
    ccov
    )))

save(p_model_drivers,
     file ="data/p_model_drivers.rda",
     compress = "xz")
  
#---- p_model_validation (evaluation data) -----
p_model_validation <- FDK_published_rsofun_driver_data |> 
  
  # subset to single site
  dplyr::filter(sitename == "FR-Pue") |> 
  
  # subset dates
  dplyr::mutate(forcing = purrr::map(forcing, ~dplyr::filter(., lubridate::year(date) %in% 2007:2012))) |> 
  
  # rename and select variables
  dplyr::select(sitename, data = forcing) |> 
  dplyr::mutate(data = purrr::map(data, ~dplyr::select(., date, gpp, gpp_qc)))

save(p_model_validation,
     file ="data/p_model_validation.rda",
     compress = "xz")


