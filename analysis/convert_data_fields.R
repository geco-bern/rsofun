# convert BiomeE data fields forcing data fields
# to be in line with the p-model data
library(tidyverse)
library(rsofun)

#---- gs leuning drivers ----
biomee_gs_leuning_drivers <- rsofun::biomee_gs_leuning_drivers
biomee_gs_leuning_drivers$forcing[[1]] <- biomee_gs_leuning_drivers$forcing[[1]] %>%
  mutate(
    date = as.Date(paste(YEAR, DOY),"%Y %j"),
    snow = NA,
    vpd = NA,
    ccov_int = NA,
    ccov = NA
  ) %>%
  rename(
    year = YEAR,
    doy = DOY,
    hour = HOUR,
    temp = TEMP,
    temp_soil = SoilT,
    prec = RAIN,
    rh = RH,
    ppfd = Swdown,
    par = PAR,
    patm = PRESSURE,
    wind = WIND,
    co2 = aCO2_AW,
    swc = SWC
  ) %>%
  select(
    date, year, doy, hour, temp, temp_soil,
    prec, snow, vpd, rh, ppfd, par, patm, wind, ccov_int,
    ccov, co2, swc
  )

save(biomee_gs_leuning_drivers, file = "data/biomee_gs_leuning_drivers.rda", compress = "xz")
rm(list=ls())
#---- p-model drivers --- 

biomee_p_model_drivers <- rsofun::biomee_p_model_drivers
biomee_p_model_drivers$forcing[[1]] <- biomee_p_model_drivers$forcing[[1]] %>%
  mutate(
    date = as.Date(paste(YEAR, DOY),"%Y %j"),
    snow = NA,
    vpd = NA,
    ccov_int = NA,
    ccov = NA
  ) %>%
  rename(
    year = YEAR,
    doy = DOY,
    hour = HOUR,
    temp = TEMP,
    temp_soil = SoilT,
    prec = RAIN,
    rh = RH,
    ppfd = Swdown,
    par = PAR,
    patm = PRESSURE,
    wind = WIND,
    co2 = aCO2_AW,
    swc = SWC
  ) %>%
  select(
    date, year, doy, hour, temp, temp_soil,
    prec, snow, vpd, rh, ppfd, par, patm, wind, ccov_int,
    ccov, co2, swc
  )

save(biomee_p_model_drivers, file = "data/biomee_p_model_drivers.rda", compress = "xz")
