## This is described in vignette prepare_inputs_rsofun.Rmd

library(rsofun)
library(dplyr)
library(readr)
library(ingestr)

mysites <- "FR-Pue"

siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
  dplyr::filter(sitename %in% mysites)

## take only year 2007 to 2014, corresponding to subset of data for site FR-Pue provided in this package as demo
siteinfo <- siteinfo %>% 
  dplyr::mutate(year_start = 2007, year_end = 2014)

siteinfo <- siteinfo %>% 
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

params_siml <- list(
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = FALSE,
  tempstress         = FALSE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE
	)

siteinfo <- prepare_setup_sofun(siteinfo = siteinfo, params_siml = params_siml)

params_modl <- list(
	kphio           = 0.05,
	soilm_par_a     = 1.0,
	soilm_par_b     = 0.0,
	vpdstress_par_a = 0.2,
	vpdstress_par_b = 0.2,
	vpdstress_par_m = 5
	)

df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)

ddf_fluxnet <- ingestr::ingest(
  siteinfo  = siteinfo,
  source    = "fluxnet",
  getvars   = list(temp = "TA_F_DAY", prec = "P_F", vpd  = "VPD_F_DAY", ppfd =  "SW_IN_F", patm = "PA_F"),
  dir       = "~/data/FLUXNET-2015_Tier1/20191024/DD/",
  settings  = list(dir_hh = "~/data/FLUXNET-2015_Tier1/20191024/HH/", getswc = FALSE),
  timescale = "d"
  )

ddf_cru <- ingestr::ingest(
  siteinfo  = siteinfo %>% dplyr::filter(sitename %in% mysites),
  source    = "cru",
  getvars   = list(ccov = "cld"),
  dir       = "~/data/cru/ts_4.01/"
  )

ddf_meteo <- ddf_fluxnet %>% 
  tidyr::unnest(data) %>% 
  left_join(
    ddf_cru %>% 
      tidyr::unnest(data),
    by = c("sitename", "date")
  ) %>% 
  group_by(sitename) %>% 
  tidyr::nest()

settings_gee <- ingestr::get_settings_gee( 
  bundle = "modis_fpar", 
  python_path = system("which python", intern = TRUE),
  gee_path = "~/google_earth_engine_subsets/gee_subset/",
  data_path = "~/data/gee_subsets/",
  method_interpol = "linear",
  keep = FALSE,
  overwrite_raw = FALSE,
  overwrite_interpol = TRUE
  )

df_gee_modis_fpar <- ingestr::ingest(
  siteinfo  = siteinfo %>% dplyr::filter(sitename %in% mysites),
  source = "gee",
  settings = settings_gee,
  verbose = FALSE
  )

df_co2 <- ingestr::ingest(
  siteinfo  = siteinfo %>% dplyr::filter(sitename %in% mysites),
  source = "co2",
  verbose = FALSE,
  settings = list(path = "~/data/co2/cCO2_rcp85_const850-1765.csv")
  )

df_drivers <- rsofun::collect_drivers_sofun( 
  siteinfo       = siteinfo %>% dplyr::filter(sitename %in% mysites),
  meteo          = ddf_meteo, 
  fapar          = df_gee_modis_fpar,
  co2            = df_co2,
  df_soiltexture = df_soiltexture
  )

save(df_drivers, file = "~/rsofun/data/df_drivers.RData")
