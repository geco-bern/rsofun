rm(list=ls())
library(tidyverse)
library(reshape2)
library(rsofun)
library(BayesianTools)
library(tictoc)
library(ncdf4)
library(scatterPlotMatrix)
library(ggpointdensity)

plot_only = F
root_data_dir = "~/Downloads/fluxdatakit_oct3"
lsm_path = paste0(root_data_dir, "/FLUXDATAKIT_LSM/")
data_dir = paste0(root_data_dir, "/phydro_drivers/")

# out_dir = "~/codes/rsofun/vignett/"
# figures_dir = paste0(out_dir, "/figures/")


read_obs = function(site){
  file_obs = tibble(files=list.files(data_dir, full.names = T)) %>% 
    filter(stringr::str_detect(files, site)) %>% 
    filter(stringr::str_detect(files, "p_hydro_validation.rda")) %>% 
    pull(files)
  
  load(file_obs)
  
  p_hydro_validation$data[[1]] = p_hydro_validation$data[[1]] %>%
    rename(le=latenth) 
  
  p_hydro_validation
}

read_drv = function(site){
  file_obs = tibble(files=list.files(data_dir, full.names = T)) %>% 
    filter(stringr::str_detect(files, site)) %>% 
    filter(stringr::str_detect(files, "p_hydro_drivers.rda")) %>% 
    pull(files)
  
  load(file_obs)
  
  p_hydro_drivers$forcing[[1]]$netrad[is.na(p_hydro_drivers$forcing[[1]]$netrad)] = 0
  p_hydro_drivers$forcing[[1]]$patm[p_hydro_drivers$forcing[[1]]$patm < 0] = 1.0135e5
  
  p_hydro_drivers$params_siml[[1]]$use_phydro = T
  p_hydro_drivers$params_siml[[1]]$use_pml = F
  
  p_hydro_drivers
}

sites_phydro = c("FR-LBr", "FI-Hyy", "CH-Dav")

phydro_validation = sites_phydro %>% 
  map_df(~read_obs(.))

phydro_drivers = sites_phydro %>% 
  map_df(~read_drv(.))


params_modl <- list(
  kphio              = 0.045, # 0.089, # 0.11, #0.04998,    
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  # soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
  # soilm_betao        = 0.0,
  # beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41,
  phydro_K_plant     = 1.421218e-16 , #0.3e-16,
  phydro_p50_plant   = -2.506519e+00, #-1,
  phydro_b_plant     = 1,
  phydro_alpha       = 1.334564e-01, #0.1,
  phydro_gamma       = 1.889438e+00, #1,
  bsoil              = 3, #3,
  Ssoil              = 3.193891e+02, #3,
  whc                = 7.819509e+02 #200
)

output_p <- rsofun::runread_pmodel_f(
  phydro_drivers,
  par = params_modl
)

get_density <- function(x, y, ...) {
  df = tibble(x=x, y=y) %>% drop_na
  dens <- MASS::kde2d(df$x, df$y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

plot_p = function(output_p){
  for(i in 1:nrow(output_p)){
  print(
  output_p$data[[i]] %>% 
    select(date, gpp, le) %>% 
    pivot_longer(-date) %>% 
    mutate(type="pred") %>% 
    rbind(
      phydro_validation$data[[i]] %>% 
        select(date, gpp, le) %>% 
        pivot_longer(-date) %>% 
        mutate(type="obs")
    ) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    group_by(name) %>% 
    reframe(obs = obs, pred = pred, density=scale(get_density(pred, obs,n=100))) %>%  
    ggplot(aes(y=obs, x=pred, colour=density)) +
    scale_color_viridis_c() +
    geom_point(alpha=0.7) +
    geom_abline(slope=1, intercept=0, col="red")+
    theme_classic() +
    theme(strip.background = element_rect(color = "white", size = 1))+
    facet_wrap(~name, scales = "free", nrow = 1)+
    labs(colour="Density")
  )
  }
}

###

uniform_range = function(lower, upper){
  list(lower= lower, upper=upper, mean = (upper+lower)/2, sd = (upper-lower)*10)
}

gaussian_range = function(mean, sd){
  if (mean > 0) list(lower= max(mean-5*sd, 0), upper=mean+5*sd, mean = mean, sd = sd)
  else          list(lower= mean-5*sd, upper=min(mean+5*sd, 0), mean = mean, sd = sd)
}

pars_calib = list(
  kphio = uniform_range(lower=0.005, upper=0.09),
  phydro_K_plant = uniform_range(lower=0.1e-16, 1e-16),
  phydro_p50_plant = gaussian_range(mean = -1, sd = 0.2),  
  # phydro_alpha = gaussian_range(mean = 0.1, sd = 0.005),
  phydro_gamma = gaussian_range(mean = 0.5, sd = 0.3),
  #bsoil = uniform_range(lower=0.1, upper=10),
  # Ssoil = uniform_range(lower = 0, upper = whc_site+whc_site_sd),
  whc = gaussian_range(mean = 300, sd = 50),
  err_gpp = uniform_range(lower = 0.01, upper = 4),
  err_le = uniform_range(lower = 0.1e6, upper = 10e6)
)

pars_fixed = list(         # fix all other parameters
  # kphio              = 0.045,
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41,
  # phydro_K_plant     = 0.3e-16,
  # phydro_P50_plant     = -1,
  phydro_b_plant     = 1,
  phydro_alpha       = 0.08,
  # phydro_gamma       = 1
  bsoil              = 3,
  Ssoil              = 113
  # whc                = 90  
)

debug = T

# Define calibration settings and parameter ranges from previous work
settings_bayes <- list(
  method = "BayesianTools",
  par = pars_calib,
  metric = rsofun::cost_likelihood_pmodel,
  control = list(
    sampler = "DEzs",
    settings = list(
      nrChains =   ifelse(debug, yes = 1,    no = 3    ),
      burnin =     ifelse(debug, yes = 30,  no = 10000),        
      iterations = ifelse(debug, yes = 100, no = 50000)     # kept artificially low
    )
  )
)


message("Begin calibration...")
pars_calib_bayes <- calib_sofun(
  # calib_sofun arguments:
  drivers = phydro_drivers,  
  obs = phydro_validation,
  settings = settings_bayes,
  # extra arguments passed to the cost function:
  par_fixed = pars_fixed,
  targets = c("gpp", "le")           # define target variable GPP
)


params_modl_opt = c(pars_calib_bayes$par, pars_fixed)

output_p_opt <- rsofun::runread_pmodel_f(
  phydro_drivers,
  par = params_modl_opt
)

plot_p(output_p_opt)
