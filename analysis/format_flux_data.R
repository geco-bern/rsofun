library(ingestr)
library(tidyverse)
library(lubridate)

mysites <- "FR-Pue"

siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
  dplyr::filter(sitename %in% mysites) %>% 
  ## take only year 2007 to 2014, corresponding to subset of data for site FR-Pue provided in this package as demo
  dplyr::mutate(year_start = 2007, year_end = 2014) %>% 
  ## add info
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

settings_ingestr_fluxnet <- list(
  dir_hh = "~/Downloads/FLX_FR-Pue_FLUXNET2015_FULLSET_2000-2014_2-4/", 
  getswc = FALSE,
  filter_ntdt = TRUE,
  threshold_GPP = 0.8,
  remove_neg = FALSE
)

p_model_validation <- ingestr::ingest(
  siteinfo = siteinfo,
  source    = "fluxnet",
  getvars = list(gpp = "GPP_NT_VUT_REF",
                 gpp_unc = "GPP_NT_VUT_SE"),
  dir = "~/Downloads/FLX_FR-Pue_FLUXNET2015_FULLSET_2000-2014_2-4/",
  settings = settings_ingestr_fluxnet,
  timescale = "d"
)

save(p_model_validation, file = "data/p_model_validation.rda", compress = "xz")

mysites <- "CH-Lae"

siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
  dplyr::filter(sitename %in% mysites) %>% 
  dplyr::mutate(year_start = 2007, year_end = 2014) %>% 
  ## add info
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

settings_ingestr_fluxnet <- list(
  dir_hh = "~/Downloads/FLX_CH-Lae_FLUXNET2015_FULLSET_2004-2014_1-4/", 
  getswc = FALSE,
  filter_ntdt = TRUE,
  threshold_GPP = 0.8,
  remove_neg = FALSE
)

lm3ppa_validation <- ingestr::ingest(
  siteinfo = siteinfo,
  source    = "fluxnet",
  getvars = list(gpp = "GPP_NT_VUT_REF",
                 gpp_unc = "GPP_NT_VUT_SE"),
  dir = "~/Downloads/FLX_CH-Lae_FLUXNET2015_FULLSET_2004-2014_1-4/",
  settings = settings_ingestr_fluxnet,
  timescale = "d"
)

save(lm3ppa_validation, file = "data/lm3ppa_validation.rda", compress = "xz")