# convert LM3PPA data fields forcing data fields
# to be in line with the p-model data
library(tidyverse)
library(rsofun)

df <- rsofun::lm3ppa_gs_leuning_drivers

forcing <- df$forcing[[1]]
forcing <- forcing %>%
  mutate(
    date = as.Date(paste(YEAR, DOY),"%Y %j"),
    year = YEAR,
    doy = DOY,
    hour = HOUR,
    temp = TEMP,
    temp_soil = SoilT,
    prec = RAIN,
    snow = NA,
    vpd = NA,
    rh = RH,
    ppfd = Swdown,
    par = PAR,
    patm = PRESSURE,
    wind = WIND,
    ccov_int = NA,
    ccov = NA,
    co2 = aCO2_AW,
    swc = SWC
  ) %>%
  select(
    date, year, doy, hour, temp, temp_soil,
    prec, snow, vpd, rh, ppfd, par, patm, wind, ccov_int,
    ccov, co2, swc
  )

print(str(forcing))

