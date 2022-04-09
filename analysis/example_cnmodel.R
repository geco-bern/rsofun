library(dplyr)
library(rsofun)
library(ggplot2)
library(patchwork)


pars <- list(

  # P-model
  kphio                 = 0.04607080,
  soilm_par_a           = 2.75687824,
  soilm_par_b           = 1.68140444,
  tau_acclim_tempstress = 7.35259044,
  par_shape_tempstress  = 0.09863961,

  # Plant
  f_nretain             = 0.500000,
  fpc_tree_max          = 0.950000,
  growtheff             = 0.600000,
  r_root                = 0.913000,
  r_sapw                = 0.044000,
  exurate               = 0.003000,
  
  k_decay_leaf_base     = 1.00000,
  k_decay_leaf_width    = 0.00000,
  k_decay_root          = 1.00000,
  k_decay_labl          = 0.00000,
  k_decay_sapw          = 1.00000,
  
  r_cton_root           = 37.0000,
  r_cton_wood           = 100.000,
  ncw_min               = 0.056,
  r_n_cw_v              = 0.4,
  r_ctostructn_leaf     = 35.0000,
  kbeer                 = 0.500000,

  # Phenology (should be PFT-specific)
  gddbase               = 5.0,
  ramp                  = 0.0,
  phentype              = 2.0,

  # Soil physics (should be derived from params_soil, fsand, fclay, forg, fgravel)
  perc_k1               = 5.0,        
  thdiff_wp             = 0.2,          
  thdiff_whc15          = 0.8,
  thdiff_fc             = 0.4,          
  forg                  = 0.01,
  wbwp                  = 0.029,  
  por                   = 0.421,    
  fsand                 = 0.82,      
  fclay                 = 0.06,      
  fsilt                 = 0.12,  

  # Water and energy balance
  kA                    = 107,     
  kalb_sw               = 0.17,    
  kalb_vis              = 0.03,    
  kb                    = 0.20,    
  kc                    = 0.25,    
  kCw                   = 1.05,    
  kd                    = 0.50,    
  ke                    = 0.0167,  
  keps                  = 23.44,   
  kWm                   = 220.0,   
  kw                    = 0.26,    
  komega                = 283.0,
  maxmeltrate           = 3.0,

  # Soil BGC
  klitt_af10            = 1.2,
  klitt_as10            = 0.35,
  klitt_bg10            = 0.35,
  kexu10                = 50.0,
  ksoil_fs10            = 0.021,
  ksoil_sl10            = 7.0e-04,
  ntoc_crit1            = 0.45,
  ntoc_crit2            = 0.76,
  cton_microb           = 10.0,
  cton_soil             = 9.77,
  fastfrac              = 0.985,

  # N uptake
  eff_nup               = 0.600000,
  minimumcostfix        = 1.000000,
  fixoptimum            = 25.15000,
  a_param_fix           = -3.62000,
  b_param_fix           = 0.270000,

  # Inorganic N transformations
  maxnitr               = 0.1,
  non                   = 0.01,
  n2on                  = 0.0005,
  kn                    = 83.0,
  kdoc                  = 17.0,
  docmax                = 1.0,
  dnitr2n2o             = 0.01,
  
  # Additional parameters - previously forgotten
  beta                  = 146.000000,
  rd_to_vcmax           = 0.01400000,
  tau_acclim            = 10,

  # for development
  tmppar                = 9999

  )

## add new required columns to forcing 
tmp <- rsofun::p_model_drivers %>% 
  mutate(forcing = purrr::map(forcing, ~mutate(., 
                                               fharv = 0.0,
                                               dno3 = 0.1,
                                               dnh4 = 0.1)))

## no spinup, 1 year transient run
tmp$params_siml[[1]]$spinupyears <- 10
tmp$params_siml[[1]]$recycle <- 5
# tmp$params_siml[[1]]$nyeartrend <- 1
# tmp$forcing[[1]] <- tmp$forcing[[1]] %>% filter(lubridate::year(date) == 2007)

## run the model
output <- runread_pmodel_f(
  tmp,
  par = pars
  )

## Test plot

output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(date, gpp)) + 
  geom_line()

output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(date, npp)) + 
  geom_line()

output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(date, cleaf)) + 
  geom_line()

output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(date, nleaf)) + 
  geom_line()

output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(cleaf/nleaf, ..density..)) + 
  geom_histogram()

output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(date, fapar)) + 
  geom_line()

output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(date, lai)) + 
  geom_line()

output$data[[1]] %>% 
  as_tibble() %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year) %>% 
  summarise(npp = sum(npp, na.rm = TRUE), gpp = sum(gpp, na.rm = TRUE)) %>% 
  mutate(cue = npp/gpp) %>% 
  ggplot(aes(year, cue)) + 
  geom_line()



source("analysis/get_fill_seeds.R")

df_fill_seeds <- get_fill_seeds(
  output$data[[1]]$gpp,
  output$data[[1]]$drd,
  output$data[[1]]$lai,
  lubridate::yday(output$data[[1]]$date)
  )

df <- bind_cols(
  output$data[[1]],
  df_fill_seeds
)

gg1 <- df %>% 
  slice(1:365) %>% 
  ggplot(aes(date, gpp)) +
  geom_line()

gg2 <- df %>% 
  slice(1:365) %>% 
  ggplot() +
  geom_line(aes(date, (gpp-drd)/lai)) +
  geom_line(aes(date, an_max), color = "red")

gg1/gg2
