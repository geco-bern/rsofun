#!/usr/bin/env Rscript

library(tidyverse)
library(lubridate)
library(rsofun)
library(zen4R)

# load data
#   Download FluxDataKit data from Zenodo:
#   sudo apt install librdf0-dev
#   install.packages("zen4R")
download_path <- tempdir()
dir.create(download_path, recursive = TRUE)
zen4R::download_zenodo(
  path = download_path, "10.5281/zenodo.14808331",
  files = "rsofun_driver_data_v3.4.2.rds"
)
FDK_published_rsofun_driver_data <- readRDS(
  file.path(download_path, "rsofun_driver_data_v3.4.2.rds")
)

#---- p_model_oldformat_drivers -----
p_model_oldformat_drivers <- FDK_published_rsofun_driver_data |>
  dplyr::filter(sitename == "FR-Pue") |>

  # subset dates
  dplyr::mutate(forcing = purrr::map(forcing, ~ dplyr::filter(., lubridate::year(date) %in% 2007:2012))) |>

  # select variables for P-model simulation
  # # removes unused variables (-vwind since FDK v3.4.2)
  # # removes unused variables (-gpp, -gpp_qc, -nee, -nee_qc, -le, -le_qc)
  dplyr::mutate(forcing = purrr::map(
    forcing,
    ~ dplyr::select(.,
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
      ccov) |>
      dplyr::mutate(ccov = 0.0))
  ) |>
  # select variables for P-model simulation
  dplyr::mutate(site_info = purrr::map(
    site_info,
    ~ dplyr::select(.,
      -canopy_height,
      -reference_height) |>
      dplyr::mutate(whc = 432.375)))


usethis::use_data(p_model_oldformat_drivers, overwrite = TRUE, compress = "xz")
# testthat::expect_equal(object = rsofun::p_model_oldformat_drivers,
#                        expected = p_model_oldformat_drivers)


#---- p_model_oldformat_validation -----
p_model_oldformat_validation <- FDK_published_rsofun_driver_data |>

  # subset to single site
  dplyr::filter(sitename == "FR-Pue") |>

  # subset dates
  dplyr::mutate(forcing = purrr::map(forcing, ~ dplyr::filter(., lubridate::year(date) %in% 2007:2012))) |>

  # rename and select variables
  dplyr::select(sitename, data = forcing) |>
  dplyr::mutate(data = purrr::map(
    data,
    ~ dplyr::select(.,
      date,
      gpp # , gpp_qc
    ) |>
      dplyr::mutate(gpp_unc = 0.0)))

usethis::use_data(p_model_oldformat_validation, overwrite = TRUE, compress = "xz")
# testthat::expect_equal(object   = rsofun::p_model_oldformat_validation,
#                        expected = p_model_oldformat_validation)





#---- pmodel_drivers -----
# Download
download_path <- file.path(tempdir(), "rsofun_doc")
dir.create(download_path, recursive = TRUE)

dir.create(download_path)
zen4R::download_zenodo(path = download_path,
  doi = "10.5281/zenodo.17495564",
  files = "geco-bern/rsofun_doc-v1.0.2.zip",
  timeout = 600)
# unzip(zipfile = file.path(tempdir(), "rsofun_doc-v1.0.2.zip"),
#       list = TRUE) # print files in zip
utils::unzip(
  zipfile = file.path(download_path, "rsofun_doc-v1.0.2.zip"),
  exdir   = file.path(download_path, "extracted"),
  junkpaths = TRUE,
  files = c("geco-bern-rsofun_doc-93c8d4d/data/01_bigD13C-vj-gpp_calibsofun_drivers.rds",
    "geco-bern-rsofun_doc-93c8d4d/data/01_bigD13C-vj-gpp_calibsofun_obs.rds")
)
file.remove(file.path(download_path, "rsofun_doc-v1.0.2.zip"))

pmodel_drivers_allsites <- readr::read_rds(
  file.path(download_path, "extracted", "01_bigD13C-vj-gpp_calibsofun_drivers.rds"))

#---- pmodel_validation -----
pmodel_validation_allsites <- readr::read_rds(
  file.path(download_path, "extracted", "01_bigD13C-vj-gpp_calibsofun_obs.rds"))

#---- pmodel_drivers and pmodel_validation: processing -----
# subset sites
sites_to_keep <- c( # these are some manual samples for different vegetation
  # types from Table S2 (https://doi.org/10.5194/gmd-18-9855-2025)
  "FR-Pue", "DK-Sor", "US-Ha1", "CH-Dav",
  "FI-Hyy", "GF-Guy", "CZ-BK1", "US-PFa",

  "lon_+010.52_lat_+051.08",
  "lon_+112.58_lat_+023.13",
  "lon_+011.10_lat_+048.30",
  "lon_-079.10_lat_+035.97", # this site would have bigD13C AND VJ observations
  "lon_-119.82_lat_+034.50",
  "lon_+146.13_lat_-032.97",
  "lon_+148.30_lat_-036.10",
  "lon_+145.13_lat_-005.83",
  "lon_+153.00_lat_-026.85",
  "lon_-116.45_lat_+047.16",
  "lon_-122.98_lat_+038.40",
  "lon_-149.61_lat_+063.97")

# pmodel_drivers processing:
# A) subset sites
pmodel_drivers_subset <- pmodel_drivers_allsites |>
  # subset sites
  dplyr::filter(sitename %in% sites_to_keep) |>
  # remove unneeded columns of site_info:
  # canopy_height, reference_height, nyears_gpp, FDK_koeppen_code, FDK_igbp_land_use
  dplyr::mutate(site_info = purrr::map(
    site_info,
    ~ dplyr::select(.x, lon, lat, elv, whc)))

# B) remove column 'run_model':
pmodel_drivers_subset_daily <- pmodel_drivers_subset |> dplyr::filter(run_model == "daily") |>
  # move 'run_model' into 'params_siml'
  dplyr::mutate(params_siml = purrr::map(
    params_siml,
    ~ dplyr::mutate(.x, onestep = FALSE))) |>
  dplyr::select(-run_model)

pmodel_drivers_subset_onestep <- pmodel_drivers_subset |> dplyr::filter(run_model == "onestep") |>
  # move 'run_model' into 'params_siml'
  dplyr::mutate(params_siml = purrr::map(
    params_siml,
    ~ dplyr::mutate(.x, onestep = TRUE))) |>
  dplyr::select(-run_model)

# C) subset dates to reduce file size
pmodel_drivers_subset_daily <- pmodel_drivers_subset_daily |>
  # remove unneeded dates (only from daily model runs, i.e. GPP, not onestep, i.e. bigDelta13C)
  tidyr::unnest(forcing) |>
  rename(wind = "vwind") |>
  dplyr::mutate(year = lubridate::year(date)) |>
  dplyr::filter(year >= 2007 & year <= 2016) |>
  dplyr::group_by(sitename) |> dplyr::filter(
    (sitename == "FR-Pue" & year < min(year) + 6) |          # only keep 6 years for FR-Pue
      (sitename == "GF-Guy" & year >= 2015 & year <= 2016) | # or 2 specific years for GF-Guy (see validation)
      (sitename != "GF-Guy" & year < min(year) + 2)          # only keep at most 2 years for other sites
  ) |>
  dplyr::select(-year) |>
  tidyr::nest(forcing = "date":"ccov") |>
  dplyr::ungroup()

# D) finalize drivers
pmodel_drivers <- bind_rows(pmodel_drivers_subset_daily, 
                            pmodel_drivers_subset_onestep)

# pmodel_validation processing:
# A) subset sites, variables
pmodel_validation_subset <- pmodel_validation_allsites |>
  # remove 'run_model' from validation data set (we implicitly use targets)
  dplyr::select(-run_model) |>
  # subset sites
  dplyr::filter(sitename %in% sites_to_keep) |>
  # subset variables
  tidyr::unnest_wider(targets) |>
  dplyr::select(-vj) |>
  # only keep sites with targets bigD13C or gpp
  dplyr::filter(gpp | bigD13C) |>
  # nest targets again into a one-row tibble:
  nest(targets = c("bigD13C", "gpp")) |>
  dplyr::select(sitename, targets, data)

# B) transform targets from a named list into a
named_boolean_list_to_vector <- function(lst) {
  # transform a named list,
  # e.g. list(bigD13C = FALSE, gpp = TRUE, et = TRUE) into list("gpp")
  names(lst)[unlist(lst) == TRUE]
}
pmodel_validation_subset <- pmodel_validation_subset |>
  dplyr::mutate(targets = purrr::map(
    targets,
    named_boolean_list_to_vector))

# C) subset dates to reduce file size
# remove unneded dates from observations (i.e. those outside of the driver dates) :
# C1) find which years are available in driver
drivers_available <- pmodel_drivers |>
  # filter rows with run_model == "daily":
  dplyr::rowwise() |> dplyr::filter(all(params_siml$onestep == FALSE)) |> dplyr::ungroup() |>
  # summarise which years have driver data:
  group_by(sitename) |>
  dplyr::mutate(forcing_years_available =
    purrr::map(
      forcing,
      ~ (.x$date |> lubridate::year() |> unique()))
  ) |>
  dplyr::select(sitename, forcing_years_available)

# C2) subset gpp runs
pmodel_validation_subset_gppYears <- pmodel_validation_subset |>
  # dplyr::rowwise() |> filter("gpp" %in% targets) |> dplyr::ungroup() |>
  inner_join(drivers_available, by = join_by(sitename)) |>
  unnest(data) |>
  dplyr::mutate(obs_year = lubridate::year(date)) |>
  dplyr::rowwise() |> filter(obs_year %in% forcing_years_available) |> dplyr::ungroup() |>
  dplyr::select(-obs_year, -forcing_years_available) |>
  nest(data = -c(sitename, targets))

# C3) define two targets for Davos CH-Dav:
pmodel_validation_subset_gppYears$targets[[
  which(pmodel_validation_subset_gppYears$sitename == "CH-Dav")]] <- c("gpp", "le")

# D) remove unneeded leaf-trait observations (vj) and format bigD13C
pmodel_validation_subset_bigD13C <- pmodel_validation_subset |>
  dplyr::rowwise() |> filter("bigD13C" %in% targets) |> dplyr::ungroup() |>
  # filter(run_model == "onestep") |>
  unnest(data) |> dplyr::select(-vj) |> unnest(bigD13C) |> tidyr::unite(col = "id", c("year", "species")) |>
  dplyr::select(sitename, targets, id, bigD13C = bigD13C_obs_permil) |>
  nest(data = c("id", "bigD13C"))

# E) finalize validation observations
pmodel_validation <- bind_rows(pmodel_validation_subset_gppYears,
                               pmodel_validation_subset_bigD13C)

# store as rda into the package
usethis::use_data(pmodel_drivers,    overwrite = TRUE, compress = "xz")
usethis::use_data(pmodel_validation, overwrite = TRUE, compress = "xz")
