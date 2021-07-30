library(rsofun)
library(dplyr)
library(readr)
library(ingestr)
library(ggplot2)

mysites <- "FR-Pue"

siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
  dplyr::filter(sitename %in% mysites) %>% 

  ## take only year 2007 to 2014, corresponding to subset of data for site FR-Pue provided in this package as demo
  dplyr::mutate(year_start = 2007, year_end = 2014) %>% 

  ## add info
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

rsofun::df_drivers

## run for a single site
mod <- run_pmodel_f_bysite( 
  df_drivers$sitename[1], 
  df_drivers$params_siml[[1]], 
  df_drivers$siteinfo[[1]], 
  df_drivers$forcing[[1]], 
  df_drivers$df_soiltexture[[1]], 
  params_modl = params_modl, 
  makecheck = TRUE 
  )

# df_output <- runread_pmodel_f(
#      df_drivers, 
#      params_modl = params_modl, 
#      makecheck = TRUE,
#      parallel = FALSE
#      )

ggplot() +
  geom_line(data = df_output$data[[1]], aes(x=date, y=gpp), color = 'black') + 
  labs(title = df_output$sitename[[1]], subtitle = "SOFUN output")

