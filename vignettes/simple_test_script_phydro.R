library(tidyverse)
library(ggpointdensity)

setwd("~/codes/rsofun")

load("data/AU-ASM_p_hydro_drivers.rda")
load("data/AU-ASM_p_hydro_validation.rda")

p_hydro_validation$data[[1]] = p_hydro_validation$data[[1]] %>%
  rename(le=latenth) 

p_hydro_drivers$forcing[[1]]$netrad[is.na(p_hydro_drivers$forcing[[1]]$netrad)] = 0
p_hydro_drivers$forcing[[1]]$patm[p_hydro_drivers$forcing[[1]]$patm < 0] = 1.0135e5

p_hydro_drivers$params_siml[[1]]$use_phydro = T
p_hydro_drivers$params_siml[[1]]$use_pml = F

# Calibrated parameters for AU-ASM site
params_modl <- list(
  kphio              = 0.08857451,    # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41,
  phydro_K_plant     = 3.035552e-17,
  phydro_p50_plant   = -0.8639834,
  phydro_gamma       = 0.2859356,
  phydro_b_plant     = 1,
  phydro_alpha       = 0.08,
  bsoil              = 3,
  Ssoil              = 113,
  whc                = 676.9775
)

# run the model for these parameters
output <- rsofun::runread_pmodel_f(
  p_hydro_drivers,
  par = params_modl
)

print(
output$data[[1]] %>% 
  select(date, gpp, le, aet, pet, netrad, wcont) %>% 
  pivot_longer(-date) %>% 
  mutate(type="pred") %>% 
  rbind(
    p_hydro_validation$data[[1]] %>% 
      select(date, gpp, le) %>% 
      pivot_longer(-date) %>% 
      mutate(type="obs")
  ) %>% 
  ggplot(aes(x=date, y=value, group=type, col=type)) +
  theme_classic()+
  geom_line() +
  facet_wrap(~name, scales="free")
)

get_density <- function(x, y, ...) {
  df = tibble(x=x, y=y) %>% drop_na
  dens <- MASS::kde2d(df$x, df$y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}


print(
  output$data[[1]] %>% 
    select(date, gpp, le) %>% 
    pivot_longer(-date) %>% 
    mutate(type="pred") %>% 
    rbind(
      p_hydro_validation$data[[1]] %>% 
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

