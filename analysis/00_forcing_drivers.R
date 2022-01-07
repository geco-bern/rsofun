# This script prepares the forcing data for CH-Lae simulations

# load packages
library(rsofun)
library(dplyr)
library(readr)
library(ingestr)
library(tidyverse)

# Meteo data
FLX_HH_LAE <- read_csv("~/data/FLUXNET-2015_Tier1/20191024/HH/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2014_1-3.csv") %>%
  dplyr::select(TIMESTAMP_START,PPFD_IN,SW_IN_F,TA_F,TS_F_MDS_1,RH,P_F,WS_F,PA_F,SWC_F_MDS_1) %>% 
  dplyr::mutate_all(funs(ifelse(.==-9999, NA, .))) %>% 
  mutate(YEAR=str_sub(TIMESTAMP_START, 1, 4)) %>% 
  mutate(MONTH=str_sub(TIMESTAMP_START, 5, 6)) %>% 
  mutate(DOY=str_sub(TIMESTAMP_START, 7, 8)) %>% 
  mutate(HOUR=str_sub(TIMESTAMP_START, 9, 10)) %>% 
  relocate(YEAR, MONTH, DOY, HOUR) %>%
  mutate(YEAR=as.integer(YEAR),MONTH=as.numeric(MONTH),DOY=as.numeric(DOY),HOUR=as.numeric(HOUR))

# Half-hourly data
FluxForcing <- FluxForcing %>% 
  dplyr::rename(PAR=PPFD_IN,
                Swdown=SW_IN_F,
                TEMP=TA_F,
                SoilT=TS_F_MDS_1,
                RH=RH,
                RAIN=P_F,
                WIND=WS_F,
                PRESSURE=PA_F,
                SWC=SWC_F_MDS_1
  ) %>% 
  dplyr::select(-TIMESTAMP_START) %>% 
  mutate(datehour = make_datetime(YEAR, MONTH, DOY, HOUR))  %>% 
  mutate(date = make_datetime(YEAR, MONTH, DOY)) %>%  
  mutate(PRESSURE = PRESSURE*1000) %>%  
  mutate(PAR = PAR*2.02) %>%
  dplyr::filter(!(mday(datehour)==29 & month(datehour)==2)) %>% 
  mutate(DOY=lubridate::yday(datehour)) %>% 
  dplyr::select(-MONTH) #%>% drop_na()

# Hourly data
FluxForcing <- FluxForcing %>%
  dplyr::group_by(lubridate::year(datehour),
                  lubridate::month(datehour),
                  lubridate::day(datehour),
                  lubridate::hour(datehour)) %>% 
  summarise_at(vars(1:14), list(~mean(., na.rm = TRUE)))  %>% 
  mutate(sitename = "CH-Lae") 

## convert rain to units per seconds
dt_secs <- lubridate::interval(FluxForcing$datehour[1], FluxForcing$datehour[2]) %>% 
  time_length("seconds")
FluxForcing <- FluxForcing %>% 
  mutate(RAIN = RAIN / dt_secs)

FluxForcing <- FluxForcing[,-c(1:4)]

# CO2 for one specific site
df_co2 <- ingestr::ingest_bysite(
  sitename  = "CH-Lae",
  source  = "co2_mlo",
  year_start= 2007,
  year_end  = 2014,
  verbose = FALSE
  )

# Combine the two FLUXNET and CO2 to create the complete forcing data.

FluxForcing <- FluxForcing %>% 
  #tidyr::unnest(data) %>% 
  left_join(
    df_co2,
    by = c("sitename", "date")
  ) %>% rename(aCO2_AW=co2) %>% relocate(aCO2_AW, .after = PRESSURE) 
  #group_by(sitename) %>% 
  #tidyr::nest()

FluxForcing <- FluxForcing %>% relocate(aCO2_AW, .after = PRESSURE) 
forcingLAE <- FluxForcing
save(forcingLAE, file = "~/rsofun/data/inputs/forcingLAE.RData")
