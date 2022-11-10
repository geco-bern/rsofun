library(dplyr)
library(rsofun)
library(ggplot2)
library(patchwork)
library(readr)
library(lubridate)

## Parameters ------------------------
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
  r_root                = 2*0.913000,
  r_sapw                = 2*0.044000,
  exurate               = 0.003000,
  
  k_decay_leaf_base     = 1.00000,
  k_decay_leaf_width    = 2.00000,
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
  # eff_nup               = 0.600000,  # original value
  eff_nup               = 0.005000,
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

## Forcing ------------------------
## add new required columns to forcing 
tmp <- rsofun::p_model_drivers %>% 
  mutate(forcing = purrr::map(forcing, ~mutate(., 
                                               fharv = 0.0,
                                               dno3 = 0.1,
                                               dnh4 = 0.1
                                               )))

### Harvesting and seed input ----------
use_cseed <- 0 # 100
cn_seed <- 20
use_nseed <- use_cseed / cn_seed

tmp$forcing[[1]] <- tmp$forcing[[1]] %>%
  mutate(fharv = ifelse(month(date) == 7 & mday(date) == 15, 0.0, 0.0),
         cseed = ifelse(month(date) == 3 & mday(date) == 15, use_cseed, 0.0),
         nseed = ifelse(month(date) == 3 & mday(date) == 15, use_nseed, 0.0)) 
  
## check visually
tmp$forcing[[1]] %>%
  ggplot(aes(date, fharv)) +
  geom_line()

## no spinup, 1 year transient run
tmp$params_siml[[1]]$spinupyears <- 2000
tmp$params_siml[[1]]$recycle <- 5
# tmp$params_siml[[1]]$nyeartrend <- 1
# tmp$forcing[[1]] <- tmp$forcing[[1]] %>% filter(lubridate::year(date) == 2007)

### Synthetic forcing: Mean seasonal cycle -----------------------
# tmp$forcing[[1]] <- tmp$forcing[[1]] %>%
#   filter(!(lubridate::month(date) == 2 & lubridate::mday(date) == 29))
# df_meanann <- tmp$forcing[[1]] %>%
#   mutate(doy = lubridate::yday(date)) %>%
#   group_by(doy) %>%
#   summarise(across(where(is.double), .fns = mean)) %>%
#   filter(!(doy == 366))
# nyears <- tmp$forcing[[1]] %>%
#   mutate(year = lubridate::year(date)) %>%
#   pull(year) %>%
#   unique() %>%
#   length()
# tmp2 <- purrr::map_dfr(
#   as.list(seq(nyears)),
#   ~{df_meanann}) %>%
#   mutate(date = tmp$forcing[[1]]$date)

### Synthetic forcing: Constant climate in all days -----------------------
df_growingseason_mean <- tmp$forcing[[1]] %>%
  filter(temp > 5) %>%
  summarise(across(where(is.double), .fns = mean))
df_mean <- tmp$forcing[[1]] %>%
  summarise(across(where(is.double), .fns = mean))

tmp$forcing[[1]] <- tmp$forcing[[1]] %>%
  mutate(temp = df_growingseason_mean$temp,
         prec = df_mean$prec,
         vpd = df_growingseason_mean$vpd,
         ppfd = df_mean$ppfd,
         patm = df_growingseason_mean$patm,
         ccov_int = df_growingseason_mean$ccov_int,
         ccov = df_growingseason_mean$ccov,
         snow = df_mean$snow,
         rain = df_mean$rain,
         fapar = df_mean$fapar,
         co2 = df_growingseason_mean$co2,
         tmin = df_growingseason_mean$tmin,
         tmax = df_growingseason_mean$tmax,
  )

# increase CO2 from 2010
tmp$forcing[[1]] <- tmp$forcing[[1]] %>%
  mutate(co2 = ifelse(year(date) > 2010, co2 * 2, co2))


## Model run ------------------------
output <- runread_pmodel_f(
  tmp,
  par = pars
  )

## Visualisations  ------------------------
# LAI
gg1 <- output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(date, lai)) + 
  geom_line()
gg2 <- output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(date, cseed)) + 
  geom_line()
gg3 <- output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(date, cleaf)) + 
  geom_line()
gg4 <- output$data[[1]] %>% 
  as_tibble() %>% 
  # ggplot(aes(date, calc_f_seed(x2))) + 
  ggplot(aes(date, x3)) + 
  geom_line()

gg1 / gg2 / gg3 / gg4

output$data[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(date, x1)) + 
  geom_line()

calc_f_seed <- function(an_unitlai_diff_damped){
  yy <- 1 / (1 + exp(1000*(an_unitlai_diff_damped)))
  return(yy)
}

ggplot() +
  geom_function(fun = calc_f_seed) +
  xlim(-0.02,0.02) +
  # geom_vline(xintercept = 1, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted")


# df <- tibble(
#   x = 1:15,
#   y = c(2.75753609E-06, 1.42544932E-05, 2.04479511E-05, 1.85851095E-05, 2.37835593E-05, 2.47478038E-05, 9.18351452E-06, 2.34256640E-05, 1.13146407E-05, 2.28059034E-05, 6.91168907E-06, 2.24883406E-05, 2.22943618E-05, 2.46384789E-05, 2.53433227E-05)
# )
# lm(y ~ x, data = df)

# N2O
ggplot() + 
  geom_line(data = output$data[[1]], aes(date, en2o))

## read (experimental) files
aout <- read_fwf(file = "out/out_rsofun.a.csoil.txt", col_types = "in") %>%
  setNames(c("year", "csoil")) %>%
  left_join(
    read_fwf(file = "out/out_rsofun.a.nsoil.txt", col_types = "in") %>%
      setNames(c("year", "nsoil")),
    by = "year"
  )

aout %>%
  slice(1000:2008) |> 
  ggplot(aes(year, csoil)) +
  geom_line()

aout %>%
  ggplot(aes(year, nsoil)) +
  geom_line()


# ## Test plots
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, gpp)) + 
#   geom_line()
# 
output$data[[1]] %>%
  as_tibble() %>%
  ggplot() +
  geom_line(aes(date, gpp))
  # geom_line(aes(date, gpp-drd), color = 'red')

# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, cex)) + 
#   geom_line()

# output$data[[1]] %>%
#   as_tibble() %>%
#   ggplot(aes(date, cleaf)) +
#   geom_line()
# 
# output$data[[1]] %>%
#   as_tibble() %>%
#   ggplot(aes(date, croot)) +
#   geom_line()

# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, clabl)) + 
#   geom_line()
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, nlabl)) + 
#   geom_line()
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, nleaf)) + 
#   geom_line()
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, pnh4 + pno3)) + 
#   geom_line()
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, drd/gpp)) + 
#   geom_line()
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, cleaf/nleaf)) + 
#   geom_line()
# 
# r_cton_leaf <- mean(output$data[[1]]$cleaf / output$data[[1]]$nleaf, na.rm = TRUE)
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(cleaf, nleaf)) + 
#   geom_point() +
#   geom_abline(slope = 1/r_cton_leaf, intercept = 0, color = "red", linetype = "dotted")
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(cleaf, cleaf/nleaf)) + 
#   geom_point()
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(cleaf/nleaf, ..density..)) + 
#   geom_histogram()
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, fapar)) + 
#   geom_line()
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, csoil)) + 
#   geom_line()
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   ggplot(aes(date, en2o)) + 
#   geom_line()
# 
# gg1 <- output$data[[1]] %>% 
#   as_tibble() %>% 
#   slice(1:500) %>% 
#   ggplot(aes(date, drd)) + 
#   geom_line()
# 
# gg2 <- output$data[[1]] %>% 
#   as_tibble() %>% 
#   slice(1:500) %>% 
#   ggplot(aes(date, drd/gpp)) + 
#   geom_line()
# 
gg3 <- output$data[[1]] %>%
  as_tibble() %>%
  mutate(cue = npp/gpp) %>%
  slice(1:500) %>%
  ggplot(aes(date, cue)) +
  geom_line()
# 
# gg1/
#   gg2/
#   gg3
# 
# output$data[[1]] %>% 
#   as_tibble() %>% 
#   mutate(year = lubridate::year(date)) %>% 
#   group_by(year) %>% 
#   summarise(npp = sum(npp, na.rm = TRUE), gpp = sum(gpp, na.rm = TRUE)) %>% 
#   mutate(cue = npp/gpp) %>% 
#   ggplot(aes(year, cue)) + 
#   geom_line()
# 
# print(gg)
# 
# print(paste("Maximum leaf C: ", max(output$data[[1]]$cleaf)))
# 



# 
# cue_stress <- function(cue){
#   yy <- 1 / (1 + exp(10*(cue+0.25)))
#   return(yy)  
# }
# 
# ggplot() +
#   geom_function(fun = cue_stress) +
#   xlim(-5,1) +
#   geom_vline(xintercept = 1, linetype = "dotted") +
#   geom_vline(xintercept = 0, linetype = "dotted")
# 
# tmp <- read_fwf(
#     "~/rsofun/test.txt",
#     fwf_widths(c(19,18,18), c("cue", "cue_damped", "f_deactivate")),
#     skip = 1,
#     col_types = "nn") %>% 
#   tail(6000)
# 
# gg1 <- tmp %>%
#   mutate(id = 1:n()) %>% 
#   ggplot() +
#   geom_line(aes(id, cue)) +
#   geom_line(aes(id, cue_damped), color = "red") +
#   ylim(-1,1)
# gg2 <- tmp %>%
#   mutate(id = 1:n()) %>% 
#   ggplot() +
#   geom_line(aes(id, f_deactivate))
# gg1/
#   gg2
# 
# 
# # source("analysis/get_fill_seeds.R")
# # 
# # df_fill_seeds <- get_fill_seeds(
# #   output$data[[1]]$gpp,
# #   output$data[[1]]$drd,
# #   output$data[[1]]$lai,
# #   lubridate::yday(output$data[[1]]$date)
# #   )
# # 
# # df <- bind_cols(
# #   output$data[[1]],
# #   df_fill_seeds
# # )
# # 
# # gg1 <- df %>% 
# #   slice(1:365) %>% 
# #   ggplot(aes(date, gpp)) +
# #   geom_line()
# # 
# # gg2 <- df %>% 
# #   slice(1:365) %>% 
# #   ggplot() +
# #   geom_line(aes(date, (gpp-drd)/lai)) +
# #   geom_line(aes(date, an_max), color = "red") +
# #   geom_line(aes(date, an_unitlai_damped), color = "royalblue")
# # 
# # gg1/gg2
