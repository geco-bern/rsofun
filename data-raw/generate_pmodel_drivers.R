#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(rsofun)

# load data
#   Download FluxDataKit data from Zenodo:
#   sudo apt install librdf0-dev
#   install.packages("zen4R")
library(zen4R)
download_path <- tempdir()   #download_path <- "~/data_2/FluxDataKit/v3.4.2/"
dir.create(download_path, recursive = T)


# download_zenodo(path = download_path, "10.5281/zenodo.10885934", files = "rsofun_driver_data_v3.rds") # v3
# download_zenodo(path = download_path, "10.5281/zenodo.11370417", files = "rsofun_driver_data_v3.1.rds") # v3.1
# download_zenodo(path = download_path, "10.5281/zenodo.12092555", files = "rsofun_driver_data_v3.2.rds") # v3.2
# download_zenodo(path = download_path, "10.5281/zenodo.12818273", files = "rsofun_driver_data_v3.3.rds") # v3.3
# download_zenodo(path = download_path, "10.5281/zenodo.13748398", files = "rsofun_driver_data_v3.4.rds") # v3.4
download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "rsofun_driver_data_v3.4.2.rds") # v3.4.2

# UNUSED: download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "FLUXDATAKIT_FLUXNET.tar.gz", timeout=1800)
# UNUSED: download_zenodo(path = download_path, "10.5281/zenodo.14808331", files = "fdk_site_info.csv")

# FDK_published_rsofun_driver_data <- readRDS(file.path(download_path, "rsofun_driver_data_v3.rds"))
# FDK_published_rsofun_driver_data <- readRDS(file.path(download_path, "rsofun_driver_data_v3.1.rds"))
# FDK_published_rsofun_driver_data <- readRDS(file.path(download_path, "rsofun_driver_data_v3.2.rds"))
# FDK_published_rsofun_driver_data <- readRDS(file.path(download_path, "rsofun_driver_data_v3.3.rds"))
# FDK_published_rsofun_driver_data <- readRDS(file.path(download_path, "rsofun_driver_data_v3.4.rds"))
FDK_published_rsofun_driver_data <- readRDS(file.path(download_path, "rsofun_driver_data_v3.4.2.rds"))

#---- p_model_drivers -----
p_model_drivers <- FDK_published_rsofun_driver_data %>%
  filter(sitename == "FR-Pue")
p_model_validation <- FDK_published_rsofun_driver_data %>%
  filter(sitename == "FR-Pue") %>% select(sitename, forcing) %>% rename(data = forcing)

# subset dates and variables
p_model_drivers$forcing <- lapply(p_model_drivers$forcing, 
       \(df) df %>%
         # subset dates
         filter(date >= "2007-01-01", date <= "2012-12-31") %>%
         # remove unused variables
         # select(-vwind) %>% # since FDK v3.4.2
         # select(-gpp, -gpp_qc, -nee, -nee_qc, -le, -le_qc) %>% 
         # keep only these variables
         select(date, temp, vpd, ppfd, netrad, patm, 
                snow, rain, tmin, tmax, fapar, co2, ccov) %>% 
         mutate(ccov = 0.0)) 

p_model_validation$data <- lapply(p_model_validation$data, 
                                    \(df) df %>%
                                      # subset dates
                                      filter(date >= "2007-01-01", date <= "2012-12-31") %>%
                                      # keep only these variables
                                      select(date, gpp) %>%
                                      mutate(gpp_unc = 0.0)
                                    )

save(p_model_drivers,
     file ="data/p_model_drivers.rda",
     compress = "xz")

#---- p_model_validation -----
save(p_model_validation,
     file ="data/p_model_validation.rda",
     compress = "xz")






#---- p_model2_drivers -----
# Download
library(zen4R)
dir <- file.path(tempdir(), "rsofun_doc")
dir.create(dir)
download_zenodo(path = dir, 
                doi = "10.5281/zenodo.17495564", 
                files = "geco-bern/rsofun_doc-v1.0.2.zip", 
                timeout = 600)
# files_in_zip <- unzip(
#   zipfile = file.path(tempdir(), "rsofun_doc-v1.0.2.zip"), 
#   list = TRUE)
# print(files_in_zip)
# tidyr::tibble(dplyr::filter(files_in_zip, grepl("data/.*rds",Name)))
unzip(zipfile = file.path(dir, "rsofun_doc-v1.0.2.zip"), 
      exdir   = file.path(dir, "extracted"), 
      junkpaths = TRUE, 
      files = c(#"geco-bern-rsofun_doc-93c8d4d/data/00_bigD13C_forcing.rds", 
                #"geco-bern-rsofun_doc-93c8d4d/data/00_bigD13C_target.rds", 
                #"geco-bern-rsofun_doc-93c8d4d/data/00_gpp_forcingtarget.rds", 
                #"geco-bern-rsofun_doc-93c8d4d/data/00_vj_forcing.rds", 
                #"geco-bern-rsofun_doc-93c8d4d/data/00_vj_target.rds", 
                "geco-bern-rsofun_doc-93c8d4d/data/01_bigD13C-vj-gpp_calibsofun_drivers.rds", 
                "geco-bern-rsofun_doc-93c8d4d/data/01_bigD13C-vj-gpp_calibsofun_obs.rds")
)
file.remove(file.path(dir, "rsofun_doc-v1.0.2.zip"))
list.files(dir)
list.files(file.path(dir, "extracted"))

# readr::read_rds(file.path(dir, "extracted", "00_bigD13C_forcing.rds"))
# readr::read_rds(file.path(dir, "extracted", "00_bigD13C_target.rds"))
# readr::read_rds(file.path(dir, "extracted", "00_gpp_forcingtarget.rds"))
# readr::read_rds(file.path(dir, "extracted", "00_vj_forcing.rds"))
# readr::read_rds(file.path(dir, "extracted", "00_vj_target.rds"))

p_model2_drivers    <- readr::read_rds(file.path(dir, "extracted", "01_bigD13C-vj-gpp_calibsofun_drivers.rds"))

# subset dates and variables
# TODO

# store as rda into the package
save(p_model2_drivers,
     file ="data/p_model_drivers2.rda",
     compress = "xz")

#---- p_model2_validation -----
p_model2_validation <- readr::read_rds(file.path(dir, "extracted", "01_bigD13C-vj-gpp_calibsofun_obs.rds"))

# subset dates and variables
# TODO

# store as rda into the package
save(p_model2_validation,
     file ="data/p_model2_validation.rda",
     compress = "xz")




