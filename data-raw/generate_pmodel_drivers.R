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

#---- p_model_oldformat_drivers -----
p_model_oldformat_drivers <- FDK_published_rsofun_driver_data %>%
  filter(sitename == "FR-Pue")
p_model_oldformat_validation <- FDK_published_rsofun_driver_data %>%
  filter(sitename == "FR-Pue") %>% select(sitename, forcing) %>% rename(data = forcing)

# subset dates and variables
p_model_oldformat_drivers$forcing <- lapply(p_model_oldformat_drivers$forcing, 
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

p_model_oldformat_validation$data <- lapply(p_model_oldformat_validation$data, 
                                    \(df) df %>%
                                      # subset dates
                                      filter(date >= "2007-01-01", date <= "2012-12-31") %>%
                                      # keep only these variables
                                      select(date, gpp) %>%
                                      mutate(gpp_unc = 0.0)
                                    )

save(p_model_oldformat_drivers,
     file ="data/p_model_oldformat_drivers.rda",
     compress = "xz")

#---- p_model_oldformat_validation -----
save(p_model_oldformat_validation,
     file ="data/p_model_oldformat_validation.rda",
     compress = "xz")


#---- pmodel_drivers -----
# Download
library(zen4R)
dir <- file.path(tempdir(), "rsofun_doc")
dir.create(dir)
download_zenodo(path = dir, 
                doi = "10.5281/zenodo.17495564", 
                files = "geco-bern/rsofun_doc-v1.0.2.zip", 
                timeout = 600)
# unzip(zipfile = file.path(tempdir(), "rsofun_doc-v1.0.2.zip"), 
#       list = TRUE) # print files in zip
unzip(zipfile = file.path(dir, "rsofun_doc-v1.0.2.zip"), 
      exdir   = file.path(dir, "extracted"), 
      junkpaths = TRUE, 
      files = c("geco-bern-rsofun_doc-93c8d4d/data/01_bigD13C-vj-gpp_calibsofun_drivers.rds", 
                "geco-bern-rsofun_doc-93c8d4d/data/01_bigD13C-vj-gpp_calibsofun_obs.rds")
)
file.remove(file.path(dir, "rsofun_doc-v1.0.2.zip"))

pmodel_drivers_allsites    <- readr::read_rds(
  file.path(dir, "extracted", "01_bigD13C-vj-gpp_calibsofun_drivers.rds"))
#---- pmodel_validation -----
pmodel_validation_allsites <- readr::read_rds(
  file.path(dir, "extracted", "01_bigD13C-vj-gpp_calibsofun_obs.rds"))

# subset dates and variables
pmodel_validation_allsites_2 <- pmodel_validation_allsites |> 
  tidyr::unnest_wider(targets) |> 
  # subset variables
  dplyr::filter(gpp | bigD13C) |>
  select(-vj)

# subset sites
N_sites <- 6
set.seed(42)
pmodel_validation_3 <- pmodel_validation_allsites_2 |> 
  group_by(gpp) |> 
  # sample some sites
  slice_sample(n=N_sites) |>
  ungroup() |>
  # and ensure FR-Pue is in it
  bind_rows(filter(pmodel_validation_allsites_2, sitename == "FR-Pue")) |> distinct() |>
  arrange(run_model) |>
  # nest again:
  # nest(targets = c('bigD13C','gpp'))
  mutate(targets = purrr::pmap(list(bigD13C,gpp), \(x,y) setNames(list(x,y), c("bigD13C", "gpp")))) |> select(-bigD13C, -gpp) |>
  select(sitename, run_model, targets, data)

pmodel_drivers <- pmodel_drivers_allsites |> 
  dplyr::filter(sitename %in% pmodel_validation_3$sitename)

pmodel_drivers
pmodel_validation_3

# reduce file size and remove unused columns:
pmodel_drivers <- pmodel_drivers |>
  # remove columns: canopy_height, reference_height, nyears_gpp, FDK_koeppen_code, FDK_igbp_land_use
  dplyr::mutate(site_info = purrr::map(site_info, ~select(.x, lon, lat, elv, whc))) %>%
  # remove unneeded dates (only from daily model runs, i.e. GPP, not onestep, i.e. bigDelta13C)
  {bind_rows(
    dplyr::filter(., run_model == "daily") |>
      dplyr::mutate(forcing = purrr::map(
        forcing, ~dplyr::filter(.x, date >= "2007-01-01"))), #  & date < "2013-01-01"
    dplyr::filter(., run_model == "onestep")
  )}

# remove unneded dates from observations (i.e. those outside of the driver dates) :
drivers_available <- pmodel_drivers |> 
  filter(run_model == "daily") |> group_by(sitename) |> 
  mutate(forcing_years_available = purrr::map(forcing, ~(.x$date |> lubridate::year() |> unique()))) |>
  select(sitename, forcing_years_available)
pmodel_validation_3
pmodel_validation_3_gppYears <- pmodel_validation_3 |> 
  left_join(drivers_available, by = join_by(sitename)) |>
  filter(run_model == "daily") |>
  unnest(data) |>
  mutate(obs_year = lubridate::year(date)) |>
  rowwise() |> filter(obs_year %in% forcing_years_available) |> ungroup() |>
  select(-obs_year, -forcing_years_available) |>
  nest(data = -c(sitename, run_model, targets))
pmodel_validation <- bind_rows(
  pmodel_validation_3_gppYears,
  pmodel_validation_3 |> filter(run_model == "onestep") |> unnest(data) |> select(-vj) |> nest(data = c('bigD13C'))
)

# store as rda into the package
usethis::use_data(pmodel_drivers,    overwrite = TRUE, compress = "xz")
usethis::use_data(pmodel_validation, overwrite = TRUE, compress = "xz")

# Alternative format:
p_model3_drivers <- rsofun::pmodel_drivers |> 
  dplyr::select(-run_model)



rsofun::p_model_oldformat_validation         |> unnest(data) # date, gpp, gpp_unc
