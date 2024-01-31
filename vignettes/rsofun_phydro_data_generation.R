params <-
list(output_dir = ".")

#' ---
#' title: "Generate input data for rsofun Phydro"
#' author: "Jaideep Joshi"
#' date: "2023-11-12"
#' output: html_document
#' params: 
#'   output_dir: "."
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
## --------------------------------------------------------------------
library(tidyverse)
library(reshape2)
# library(FluxDataKit)
library(lubridate)
library(ncdf4)

source("read_meta_fdk.R")

#' 
#' 
## --------------------------------------------------------------------
args = commandArgs(trailingOnly=TRUE)

#site <- "GF-Guy"
if (length(args)==0) {
  #stop("At least one argument must be supplied: site name", call.=FALSE)
  site = "GF-Guy"
}else{
  site = args[1]
}

if (length(args)<2) {
  root_data_dir = "~/Downloads/fluxdatakit_oct3"
}else{
  root_data_dir = args[2]
}

print(args)


print(getwd())

#' 
#' 
## --------------------------------------------------------------------

lsm_path = paste0(root_data_dir, "/FLUXDATAKIT_LSM/")
csv_path = paste0(root_data_dir, "/FLUXDATAKIT_FLUXNET/")
out_path = paste0(root_data_dir, "/phydro_drivers/")

dir.create(out_path, showWarnings = F)
dir.create(paste0(out_path, "data_gen_figures/"), showWarnings = F)

figures_prefix = paste0(out_path,"/data_gen_figures/", site)

files_csv = list.files(csv_path)
files_lsm = list.files(lsm_path)

# Get filename for HH data for matching site
file_csv = files_csv[intersect(grep(site, files_csv), 
                               grep("HH", files_csv))]

# get metadata
# --------------------------------------------------------
message("- reading Metadata for site")
meta <- suppressWarnings(
  try(
    read_meta_fdk(
      site = site,
      path = lsm_path,
      meta_data = T
      )
    )
  )

print(meta)

# get half-hourly data  --------------------------------------------------------
# message("- convert to FLUXNET standard CSV file")
# hhdf <- suppressWarnings(
#   try(
#     fdk_convert_lsm(
#       site = site,
#       fluxnet_format = TRUE,
#       path = "~/Downloads/flux_data_kit_beta/fluxes/"
#       )
#     )
#   )
#
# if(inherits(hhdf, "try-error")){
#   message("!!! conversion to FLUXNET failed  !!!")
#   return(NULL)
# }

message("- reading FLUXNET format halfhourly data")
hhdf <- readr::read_csv(paste0(csv_path,"/",file_csv))

# Add date and time columns to hhdf for easier further processing.
# ---------------------------------------------------------
hhdf =
  hhdf |>
    mutate(time = lubridate::as_datetime(as.character(TIMESTAMP_START), tz = "GMT", format="%Y%m%d%H%M")) |>
    mutate(date = lubridate::as_date(time))

message("- Add SW_OUT=NA if not present")
if (!("SW_OUT" %in% colnames(hhdf))) {
  hhdf$SW_OUT = NA
}


# Aggregate to daily 24-hr means  ----------------------------------------------------------
message("- downsampling FLUXNET format - 24 hr means")
ddf_24hr_mean <-
  try(
    hhdf |>
    group_by(date) |>
    select(-TIMESTAMP_START, -TIMESTAMP_END) |>
    summarize_all(.funs = mean)
  )

# Check aggregation
cairo_pdf(filename = paste0(figures_prefix, "_fig1_ddf_24hr_mean.pdf"))
# png(filename = paste0(figures_prefix, "_fig1_ddf_24hr_mean.png"), height=700*3, width=700*3, res = 300)
p1 = ddf_24hr_mean %>%
  mutate(albedo = SW_OUT/SW_IN_F_MDS) %>%
  select(SW_IN_F_MDS, NETRAD, LW_IN_F_MDS, LAI, FPAR, GPP_DT_VUT_REF, LE_F_MDS, TA_F_MDS, P_F, date) %>%
  melt("date") %>%
  ggplot(aes(y=value, x=date)) +
  geom_line(col="aquamarine4") +
  theme_classic() +
  theme(strip.background = element_rect(color = "white", size = 1))+
  facet_wrap(~variable, scales = "free")
p1 %>% print()
dev.off()

ddf_24hr_mean %>% filter(!is.na(NETRAD)) %>% pull(date) %>% summary()

#'
## --------------------------------------------------------------------
# valid_years = read.csv(text = "
#     Site      , start_year ,  end_year \n
#     CH-Dav    , 2000       ,  2010     \n
#     DE-Hai    , 2000       ,  2020     \n
#     DE-Tha    , 2000       ,  2020     \n
#     FR-LBr    , 2000       ,  2008     \n
#     FR-Pue    , 2000       ,  2013     \n
#     GF-Guy    , 2004       ,  2015     \n
#     NL-Loo    , 2000       ,  2014     \n
#     US-Me2    , 2002       ,  2020     \n
#     US-NR1    , 2000       ,  2016     \n
#     US-Ton    , 2001       ,  2014     \n
#     ",
#     header=T, strip.white=T)

valid_years = read.csv("ancillary_data/valid_years_final.csv", header = T, fileEncoding = "UTF-16")

# Get valid data years
ystart = valid_years %>% filter(Site==site) %>% pull(start_year)
yend = valid_years %>% filter(Site==site) %>% pull(end_year)

# ystart = max(meta[[1]]$year_start, 2000) %>% as.numeric()
# yend = meta[[1]]$year_end %>% as.numeric()

# Aggregate around daily maximum ppfd for acclimating model
# ---------------------------------------------------------
test.3day = hhdf %>% filter(date >= as_date(paste0(floor((ystart+yend)/2),"-06-01")) &
                            date <= as_date(paste0(floor((ystart+yend)/2),"-06-03")) )

aggregate_daily_3hr_maxima = function(df){
  # Get the time at which SW_IN is maximum
  maxppfd <- df %>% filter(SW_IN_F_MDS == max(SW_IN_F_MDS))
  max_t <- maxppfd$time[1]

  # Select times that lie in 3-hr interval around max_t
  df_aroundmax <- df %>% filter(time < (max_t + 1.5*3600) &
                                time > (max_t - 1.5*3600) )

  # take mean of selected entries
  df_mean <- df_aroundmax |>
    select(-TIMESTAMP_START, -TIMESTAMP_END) |>
    summarize_all(.funs = mean)

  df_mean
}

# Test aggregation
# ----------------
test.3day.3hr = test.3day %>% group_by(date) %>% do(aggregate_daily_3hr_maxima(.)) %>% ungroup()

cairo_pdf(filename = paste0(figures_prefix, "_fig2_3hr_maxima_sample.pdf"))
p2 = test.3day %>% select(time, SW_IN_F_MDS, NETRAD, TA_F_MDS, VPD_F_MDS, GPP_NT_VUT_REF, GPP_DT_VUT_REF, LE_F_MDS, LAI, FPAR) %>%
  melt("time") %>%
  mutate(type="hourly") %>%
  rbind(test.3day.3hr %>%
          select(time, SW_IN_F_MDS, NETRAD, TA_F_MDS, VPD_F_MDS, GPP_NT_VUT_REF, GPP_DT_VUT_REF, LE_F_MDS, LAI, FPAR) %>%
          melt("time") %>%
          mutate(type="daily")
  ) %>%
  ggplot(aes(y=value, x=as.POSIXct(time))) +
  geom_line(data = . %>% filter(type == "hourly"), col="aquamarine4") +
  geom_point(data = . %>% filter(type == "daily")) +
  theme_classic() +
  theme(strip.background = element_rect(color = "white", size = 1))+
  facet_wrap(~variable, scales = "free")
p2 %>% print()
dev.off()

#'
## --------------------------------------------------------------------
# Apply 3hr maxima aggregation to all data
# ----------------------------------------
message("- downsampling FLUXNET format - daily 3-hr means around max ppfd")
ddf_3hr_maxima <- hhdf |>
  group_by(date) |>
  do(aggregate_daily_3hr_maxima(.)) |>
  ungroup()

#'
## --------------------------------------------------------------------
aggregate_daily_daylength = function(df){
  # Get the time at which SW_IN > 0
  pos_ppfd <- df %>% filter(SW_IN_F_MDS > 10)
  # if SW_IN is unavailable in that year calc daylength based on NETRAD
  if (nrow(pos_ppfd) < 2){
    pos_ppfd <- df %>% filter(NETRAD > 25)
  }

  tmax <- max(pos_ppfd$time)
  tmin <- min(pos_ppfd$time)

  # Select times that lie in 3-hr interval around max_t
  df_aroundmax <- df %>% filter(time <= tmax &
                                time >= tmin )

  # take mean of selected entries
  df_mean <- df_aroundmax |>
    select(-TIMESTAMP_START, -TIMESTAMP_END) |>
    summarize_all(.funs = mean) |>
    mutate(daylength = difftime(tmax, tmin, units="hours") |> as.numeric())

  df_mean
}

# Test aggregation
# ----------------
test.3day.daylen = test.3day %>% group_by(date) %>% do(aggregate_daily_daylength(.)) %>% ungroup()

cairo_pdf(filename = paste0(figures_prefix, "_fig3_daytime_sample.pdf"))
p3 = test.3day %>% select(time, SW_IN_F_MDS, NETRAD, TA_F_MDS, VPD_F_MDS, GPP_NT_VUT_REF, GPP_DT_VUT_REF, LE_F_MDS, LAI) %>%
  mutate(daylength = NA) %>%
  melt("time") %>%
  mutate(type="hourly") %>%
  rbind(test.3day.daylen %>%
          select(time, SW_IN_F_MDS, NETRAD, TA_F_MDS, VPD_F_MDS, GPP_NT_VUT_REF, GPP_DT_VUT_REF, LE_F_MDS, LAI, daylength) %>%
          melt("time") %>%
          mutate(type="daily")
  ) %>%
  ggplot(aes(y=value, x=as.POSIXct(time))) +
  geom_line(data = . %>% filter(type == "hourly"), col="aquamarine4") +
  geom_point(data = . %>% filter(type == "daily")) +
  theme_classic() +
  theme(strip.background = element_rect(color = "white", size = 1))+
  facet_wrap(~variable, scales = "free")
p3 %>% print()
dev.off()

#'
## --------------------------------------------------------------------
# Apply daytime mean aggregation to all data
# ------------------------------------------
message("- downsampling FLUXNET format - daytime means")
ddf_daytime_mean <- hhdf |>
  group_by(date) |>
  do(aggregate_daily_daylength(.)) |>
  ungroup()

# Check daylength seasonality
cairo_pdf(filename = paste0(figures_prefix, "_fig4_daylenth_seasonality.pdf"))
p4 = ddf_daytime_mean %>%
  ggplot(aes(y=daylength, x=date)) +
  geom_line()+
  theme_classic()
p4 %>% print()
dev.off()

#'
## --------------------------------------------------------------------
# Calculate daily tmax and tmin from hh data
# ------------------------------------------
tmaxmin <-
  hhdf |>
  group_by(date) |>
  summarize(
    tmax = max(TA_F_MDS),
    tmin = min(TA_F_MDS)
  )

#'
## --------------------------------------------------------------------
# Creating driver object  ------------------------------------------------------
message("- compiling drivers")
load("../data/p_model_drivers.rda")


nc = nc_open("ancillary_data/cwdx80.nc")
lons = ncvar_get(nc, "lon")
lats = ncvar_get(nc, "lat")
S80 = ncvar_get(nc, "cwdx80")

site_lon = meta[[1]]$longitude
site_lat = meta[[1]]$latitude

lonid = which(lons > site_lon)[1]-1
latid = which(lats > site_lat)[1]-1
n = 1
S80_slice = S80[(lonid-n):(lonid+n), (latid-n):(latid+n)]
whc_site = mean(as.numeric(S80_slice, na.rm=T))
whc_site_sd = sd(as.numeric(S80_slice, na.rm=T))


p_hydro_drivers <- p_model_drivers
p_hydro_drivers$sitename[[1]] = site
p_hydro_drivers$site_info[[1]] =
    tibble(
       lon=meta[[1]]$longitude,
       lat=meta[[1]]$latitude,
       elv = meta[[1]]$elevation,
       canopy_height = ifelse(is.na(meta[[1]]$canopy_height), yes = 20, meta[[1]]$canopy_height),
       reference_height = ifelse(is.na(meta[[1]]$reference_height), yes = 22, meta[[1]]$reference_height),
       whc = whc_site,
       whc_sd = whc_site_sd,
       IGBP_veg_short = meta[[1]]$IGBP_veg_short
    )
kfFEC = 2.04

start_year = ystart
end_year = yend

# for demo, use just a subset of years
p_hydro_drivers$forcing <-
  ddf_24hr_mean |>
  dplyr::filter(lubridate::year(date) %in% start_year:end_year) |>
  dplyr::filter(!(lubridate::mday(date) == 29 & lubridate::month(date) == 2)) |>
  left_join(tmaxmin) |>
  group_by(date) |>
  summarize(
    date = date,
    temp = TA_F_MDS,
    vpd = VPD_F_MDS * 100,
    ppfd = SW_IN_F_MDS * kfFEC * 1e-06,
    netrad = NETRAD,
    patm = PA_F * 1000,
    snow = 0,
    rain = P_F * 48 /(60 * 60 * 24), # P_F [mm timestep-1] * 48 [timesteps day-1] / 86400 [secs day-1 ]
    tmin = tmin, # TMIN_F_MDS,
    tmax = tmax, # TMAX_F_MDS,
    fapar = FPAR,
    co2 = CO2_F_MDS,
    ccov = 0
  ) |>
  list()

p_hydro_drivers$forcing_acclim <-
  ddf_3hr_maxima |>
  dplyr::filter(lubridate::year(date) %in% start_year:end_year) |>
  dplyr::filter(!(lubridate::mday(date) == 29 & lubridate::month(date) == 2)) |>
  left_join(tmaxmin) |>
  group_by(date) |>
  summarize(
    date = date,
    time = time,
    temp = TA_F_MDS,
    vpd = VPD_F_MDS * 100,
    ppfd = SW_IN_F_MDS * kfFEC * 1e-06,
    netrad = NETRAD,
    patm = PA_F * 1000,
    snow = 0,
    rain = NA, # P_F * 48 / (60 * 60 * 24),
    tmin = tmin, # TMIN_F_MDS,
    tmax = tmax, # TMAX_F_MDS,
    fapar = FPAR,
    co2 = CO2_F_MDS,
    ccov = 0
  ) |>
  list()

p_hydro_drivers$forcing_daytime_mean <-
  ddf_daytime_mean |>
  dplyr::filter(lubridate::year(date) %in% start_year:end_year) |>
  dplyr::filter(!(lubridate::mday(date) == 29 & lubridate::month(date) == 2)) |>
  left_join(tmaxmin) |>
  group_by(date) |>
  summarize(
    date = date,
    time = time,
    temp = TA_F_MDS,
    vpd = VPD_F_MDS * 100,
    ppfd = SW_IN_F_MDS * kfFEC * 1e-06,
    netrad = NETRAD,
    patm = PA_F * 1000,
    snow = 0,
    rain = NA,  # P_F * 48 / (60 * 60 * 24),
    tmin = tmin, # TMIN_F_MDS,
    tmax = tmax, # TMAX_F_MDS,
    fapar = FPAR,
    co2 = CO2_F_MDS,
    ccov = 0,
    daylength = daylength
  ) |>
  list()


p_hydro_drivers$forcing_halfhourly <-
  hhdf |>
  dplyr::filter(lubridate::year(date) %in% start_year:end_year) |>
  dplyr::filter(!(lubridate::mday(date) == 29 & lubridate::month(date) == 2)) |>
  group_by(time) |>
  summarize(
    date = date,
    time = time,
    temp = TA_F_MDS,
    vpd = VPD_F_MDS * 100,
    ppfd = SW_IN_F_MDS * kfFEC * 1e-06,
    netrad = NETRAD,
    patm = PA_F * 1000,
    snow = 0,
    rain = P_F * 48 / (60 * 60 * 24),
    fapar = FPAR,
    co2 = CO2_F_MDS,
    ccov = 0
  ) |>
  list()



# write all drivers to file
# apply compression to minimize space
filn <- paste0(out_path, "/",site,"_p_hydro_drivers.rda")
message(paste0("- writing to file: ", filn))
save(p_hydro_drivers,
     file = filn
     )


# JJ Note: The gpp and latenth units here are different from the demo dataset supplied with rsofun. Here the units are matched to the output units from rsofun (see conversion below)
# Write validation data
load("../data/p_model_validation.rda")

p_hydro_validation <- p_model_validation
p_hydro_validation$sitename[[1]] = site

p_hydro_validation$data <-
  ddf_24hr_mean |>
  dplyr::filter(lubridate::year(date) %in% start_year:end_year) |>
  dplyr::filter(!(lubridate::mday(date) == 29 & lubridate::month(date) == 2)) |>
  group_by(date) |>
    summarise(
      date = date,
      time = time,
      gpp = GPP_DT_VUT_REF,
      latenth = LE_F_MDS
    ) |>
  mutate(gpp = gpp*86400/1e6*12)  |>    # convert [umol m-2 s-1] to [gC m-2 day-1]
  mutate(latenth = latenth*86400) |>    # convert [W m-2] to [J m-2 day-1]
  list()

p_hydro_validation$data_hh <-
  hhdf |>
    dplyr::filter(lubridate::year(date) %in% start_year:end_year) |>
    dplyr::filter(!(lubridate::mday(date) == 29 & lubridate::month(date) == 2)) |>
    group_by(time) |>
    summarise(
      date = date,
      time = time,
      gpp = GPP_DT_VUT_REF,
      latenth = LE_F_MDS
    ) |>
  mutate(gpp = gpp*86400/1e6*12)  |>    # convert [umol m-2 s-1] to [gC m-2 day-1]
  mutate(latenth = latenth*86400) |>    # convert [W m-2] to [J m-2 day-1]
  list()

p_hydro_validation$data_3hr_mean <-
  ddf_3hr_maxima |>
    dplyr::filter(lubridate::year(date) %in% start_year:end_year) |>
    dplyr::filter(!(lubridate::mday(date) == 29 & lubridate::month(date) == 2)) |>
    group_by(time) |>
    summarise(
      date = date,
      time = time,
      gpp = GPP_DT_VUT_REF,
      latenth = LE_F_MDS
    ) |>
  mutate(gpp = gpp*86400/1e6*12)  |>    # convert [umol m-2 s-1] to [gC m-2 day-1]
  mutate(latenth = latenth*86400) |>    # convert [W m-2] to [J m-2 day-1]
  list()

p_hydro_validation$data_daytime_mean <-
  ddf_daytime_mean |>
    dplyr::filter(lubridate::year(date) %in% start_year:end_year) |>
    dplyr::filter(!(lubridate::mday(date) == 29 & lubridate::month(date) == 2)) |>
    group_by(time) |>
    summarise(
      date = date,
      time = time,
      gpp = GPP_DT_VUT_REF,
      latenth = LE_F_MDS
    ) |>
  mutate(gpp = gpp*86400/1e6*12)  |>    # convert [umol m-2 s-1] to [gC m-2 day-1]
  mutate(latenth = latenth*86400) |>    # convert [W m-2] to [J m-2 day-1]
  list()


filn <- paste0(out_path,"/",site,"_p_hydro_validation.rda")
message(paste0("- writing to file: ", filn))
save(p_hydro_validation,
     file = filn
     )


#'
#'

cairo_pdf(filename = paste0(figures_prefix, "_fig5_phydro_drivers.pdf"), height=5, width=10)
p5 = p_hydro_drivers$forcing[[1]] %>%
  select(date, ppfd, netrad, temp, vpd, fapar, rain) %>%
  melt("date") %>%
  mutate(type="24-hr mean") %>%
  rbind(p_hydro_drivers$forcing_acclim[[1]] %>%
          select(date, ppfd, netrad, temp, vpd, fapar, rain) %>%
          melt("date") %>%
          mutate(type="3-hr maxima")
  ) %>%
  rbind(p_hydro_drivers$forcing_daytime_mean[[1]] %>%
          select(date, ppfd, netrad, temp, vpd, fapar, rain) %>%
          melt("date") %>%
          mutate(type="daytime means")
  ) %>%
  ggplot(aes(y=value, x=date)) +
  geom_line(aes(group=type, col=type), alpha=0.5) +
  theme_classic() +
  theme(strip.background = element_rect(color = "white", size = 1))+
  facet_wrap(~variable, scales = "free")
p5 %>% print()
dev.off()

