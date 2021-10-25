# convert data fields

library(rsofun)

print(lm3ppa_gs_leuning_drivers$forcing[[1]]$DOY)
print(lm3ppa_p_model_drivers$forcing[[1]]$DOY)

# p_model_drivers$params_soil <- p_model_drivers$soil_texture
# p_model_drivers <- p_model_drivers %>%
#   select(
#     -soil_texture
#   )
# 
# save(p_model_drivers,
#      file ="data/p_model_drivers.rda",
#      compress = "xz")
# 
# convert_p_model_drivers <- function(df) {
#   df$site_info <- df$siteinfo
#   df$params_soil <- df$df_soiltexture
#   df <- df %>%
#     select(
#       -siteinfo, -df_soiltexture
#     )
#   return(df)
# }

rm(list = ls())

siteinfo <- data.frame(
  sitename="CH-Lae",
  lon = 8.365, lat = 47.47808,
  elv = 700, year_start = 2004, year_end = 2014,
  classid = NA, c4 = FALSE, whc = NA, koeppen_code = NA,
  igbp_land_use = "Mixed Forests", plant_functional_type = "Broadleaf trees"
  )

siteinfo <- as_tibble(siteinfo)
siteinfo <- siteinfo %>% 
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

params_tile <- tibble( #list
  
  soiltype     = 3,     # Sand = 1, LoamySand = 2, SandyLoam = 3, SiltLoam = 4, FrittedClay = 5, Loam = 6, Clay = 7
  FLDCAP       = 0.4,   # soil property: field capacity 
  WILTPT       = 0.05,  # soil property: wilting point
  K1           = 2.0,   # turnover rate of fast SOM per year
  K2           = 0.05,  # turnover rate of slow SOM per year
  K_nitrogen   = 8.0,   # mineral Nitrogen turnover rate
  MLmixRatio   = 0.8,   # the ratio of C and N returned to litters from microbes
  etaN         = 0.025, # loss rate with runoff
  LMAmin       = 0.02,  # minimum LMA, boundary condition
  fsc_fine     = 1.0,   # fraction of fast turnover carbon in fine biomass
  fsc_wood     = 0.0,   # fraction of fast turnover carbon in wood biomass
  GR_factor    = 0.33,  # growth respiration factor
  l_fract      = 0.0,   # fraction of the carbon retained after leaf drop
  retransN     = 0.0,   # retranslocation coefficient of Nitrogen
  f_initialBSW = 0.2,
  f_N_add      = 0.02,   # re-fill of N for sapwood
  
  # add calibratable params
  tf_base        = 1,
  par_mort       = 1,    # param_dbh=1 param_csv=1 param_gr=1 CAI_MAX=2
  par_mort_under = 1
)

lm3ppa_gs_leuning_drivers <- tibble(
  lm3ppa_gs_leuning_drivers,
  site_info = list(siteinfo))
lm3ppa_gs_leuning_drivers$params_tile[[1]] <- params_tile
 
lm3ppa_p_model_drivers <- tibble(
  lm3ppa_p_model_drivers,
  site_info = list(siteinfo))
lm3ppa_p_model_drivers$params_tile[[1]] <- params_tile

# save(lm3ppa_gs_leuning_drivers,
#      file ="data/lm3ppa_gs_leuning_drivers.rda",
#      compress = "xz")
# 
# save(lm3ppa_p_model_drivers,
#      file ="data/lm3ppa_p_model_drivers.rda",
#      compress = "xz")

df_drivers <- lm3ppa_p_model_drivers
df_drivers$params_siml[[1]] <- tibble( #list
    spinup                = TRUE,
    spinupyears           = 1800, 
    recycle               = 9,    # 9 or 11 changed to 1 when aggregating forcing into 1 year
    firstyeartrend        = 2000, 
    nyeartrend            = 11,    # 9 or 11 (longer transient years)
    outputhourly          = TRUE,
    outputdaily           = TRUE,
    do_U_shaped_mortality = TRUE,
    update_annualLAImax   = TRUE,
    do_closedN_run        = TRUE,
    method_photosynth     = "pmodel", # gs_leuning or pmodel
    method_mortality      = "dbh" # dbh or cstarvation or growthrate or const_selfthing
  )

out <- runread_lm3ppa_f(
  df_drivers,
  ncores = 1,
  makecheck = FALSE
)$data[[1]]

plot(out$output_daily_tile$GPP)



