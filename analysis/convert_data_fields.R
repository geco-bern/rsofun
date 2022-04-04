# convert LM3PPA data fields forcing data fields
# to be in line with the p-model data
library(tidyverse)
library(rsofun)

#---- gs leuning drivers ----
lm3ppa_gs_leuning_drivers <- rsofun::lm3ppa_gs_leuning_drivers
lm3ppa_gs_leuning_drivers$forcing[[1]] <- lm3ppa_gs_leuning_drivers$forcing[[1]] %>%
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

save(lm3ppa_gs_leuning_drivers, file = "data/lm3ppa_gs_leuning_drivers.rda", compress = "xz")
rm(list=ls())
#---- p-model drivers --- 

lm3ppa_p_model_drivers <- rsofun::lm3ppa_p_model_drivers
lm3ppa_p_model_drivers$forcing[[1]] <- lm3ppa_p_model_drivers$forcing[[1]] %>%
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

save(lm3ppa_p_model_drivers, file = "data/lm3ppa_p_model_drivers.rda", compress = "xz")
