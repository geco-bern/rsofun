rm(list=ls())
library(tidyverse)
library(reshape2)
library(rsofun)
library(BayesianTools)
library(tictoc)
library(ncdf4)

plot_only = F
debug = F

tic("phydro")

## Generate data if not already available

args = commandArgs(trailingOnly=TRUE)

#site <- "GF-Guy"
if (length(args)==0) {
  #stop("At least one argument must be supplied: site name", call.=FALSE)
  site = "FR-Pue"
}else{
  site = args[1]
}

if (length(args)<2) {
  root_data_dir = "~/Downloads/fluxdatakit_oct3"
}else{
  root_data_dir = args[2]
}

if (length(args)<3) {
  out_dir = "~/Downloads/fluxdatakit_oct3/phydro_output_fixedkphio/"
}else{
  out_dir = paste0(args[3],"/")
}


data_dir = paste0(root_data_dir, "/phydro_drivers/")
figures_dir = paste0(out_dir, "/figures/")

dir.create(out_dir, showWarnings = F)
dir.create(figures_dir, showWarnings = F)

load(paste0(data_dir,"/",site,"_p_hydro_drivers.rda"))
load(paste0(data_dir,"/",site,"_p_hydro_validation.rda"))

p_hydro_validation$data[[1]] = p_hydro_validation$data[[1]] %>%
  rename(le=latenth) 
#   mutate(gpp = gpp*86400/1e6*12) %>% # convert to [gC m-2 day-1]
#   mutate(latenth = latenth*86400)    # convert [W m-2] to [J m-2 day-1]

p_hydro_drivers$forcing[[1]]$netrad[is.na(p_hydro_drivers$forcing[[1]]$netrad)] = 0
p_hydro_drivers$forcing[[1]]$patm[p_hydro_drivers$forcing[[1]]$patm < 0] = 1.0135e5

p_hydro_drivers$params_siml[[1]]$use_phydro = T
p_hydro_drivers$params_siml[[1]]$use_pml = F


get_density <- function(x, y, ...) {
  df = tibble(x=x, y=y) %>% drop_na
  dens <- MASS::kde2d(df$x, df$y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

plot_pmodel = function(output_p, out_filename_prefix=""){
  p1 = output_p$data[[1]] %>% select(date, gpp, le) %>% 
    melt("date") %>% 
    mutate(group="model") %>% 
    rbind(p_hydro_validation$data[[1]] %>% 
            select(date, gpp, le) %>%
            # mutate(gpp = gpp*86400/1e6*12) %>% # convert to gC m-2 day-1
            # mutate(latenth = latenth*86400) %>%  # convert W m-2 to J m-2 day-1 
            melt("date") %>% 
            mutate(group="obs")) %>% 
    ggplot(aes(y=value, x=as.Date(date))) +
    geom_line(aes(group=group, col=group), alpha=0.7) +
    theme_classic() +
    theme(strip.background = element_rect(color = "white", size = 1))+
    facet_wrap(~variable, scales = "free", nrow = 2)
  
  p2 = output_p$data[[1]] %>% select(date, gpp, le) %>% 
    drop_na %>% 
    melt("date", value.name = "pred") %>% 
    left_join(p_hydro_validation$data[[1]] %>% 
                select(date, gpp, le) %>%
                drop_na %>% 
                # mutate(gpp = gpp*86400/1e6*12) %>% # convert to gC m-2 day-1
                # mutate(le = le*86400) %>%  # convert W m-2 to J m-2 day-1 
                melt("date", value.name = "obs")) %>% 
    group_by(variable) %>% 
    reframe(obs = obs, pred = pred, density=scale(get_density(pred, obs,n=100))) %>%  
    ggplot(aes(y=obs, x=pred, colour=density)) +
    scale_color_viridis_c() +
    geom_point(alpha=0.7) +
    geom_abline(slope=1, intercept=0, col="red")+
    theme_classic() +
    theme(strip.background = element_rect(color = "white", size = 1))+
    facet_wrap(~variable, scales = "free", nrow = 1)+
    labs(colour="Density")
  
  p3 = output_p$data[[1]] %>% 
    mutate(vcmax25 = vcmax25*1e6,  # mol m-2 s-1 --> umol m-2 s-1
           le = le / 86400,         # J m-2 day-1 --> W m-2
           le_soil = le_soil / 86400,         # J m-2 day-1 --> W m-2
           psi_soil = psi_leaf+dpsi
           ) %>% 
    select(date, gpp, vcmax25, le, le_soil, dpsi, psi_soil, psi_leaf, gs_accl) %>% 
    drop_na %>% 
    melt("date") %>% 
    group_by(variable) %>% 
    ggplot(aes(y=value, x=as.Date(date))) +
    geom_line(alpha=0.5, col="cyan4") +
    theme_classic() +
    theme(strip.background = element_rect(color = "white", size = 1))+
    facet_wrap(~variable, scales = "free", nrow = 2)
  
  if (out_filename_prefix == ""){
    print(p1)
    print(p2)
    print(p3)
  }
  else{
    cairo_pdf(filename = paste0(out_filename_prefix, "_timeseries.pdf"), height=5, width=7)
    print(p1)
    dev.off()
    cairo_pdf(filename = paste0(out_filename_prefix, "_pred_vs_obs.pdf"), height=3.5, width=7)
    print(p2)
    dev.off()
    cairo_pdf(filename = paste0(out_filename_prefix, "_all_predictions.pdf"), height=3.5, width=7)
    print(p3)
    dev.off()
    
  }
    
}

params_modl <- list(
  kphio              = 7.013438e-02, # 0.089, # 0.11, #0.04998,    
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
  p_hydro_drivers,
  par = params_modl
)
plot_pmodel(output_p)


## P-hydro full calibration (BayesianTools)
# nc = nc_open("~/Downloads/cwdx80.nc")
# lons = ncvar_get(nc, "lon")
# lats = ncvar_get(nc, "lat")
# S80 = ncvar_get(nc, "cwdx80")
# 
# site_lon = p_hydro_drivers$site_info[[1]]$lon
# site_lat = p_hydro_drivers$site_info[[1]]$lat
# 
# lonid = which(lons > site_lon)[1]-1
# latid = which(lats > site_lat)[1]-1 
# n = 1
# S80_slice = S80[(lonid-n):(lonid+n), (latid-n):(latid+n)]
# whc_site = mean(as.numeric(S80_slice, na.rm=T))
# whc_site_sd = sd(as.numeric(S80_slice, na.rm=T))

whc_site = p_hydro_drivers$site_info[[1]]$whc
whc_site_sd = p_hydro_drivers$site_info[[1]]$whc_sd

# image(x=lons, y=lats, z=log(1+S80), col = scales::viridis_pal()(100))
# points(x=site_lon, y=site_lat, pch=20, col="red")

p_hydro_drivers$params_siml[[1]]$use_phydro = T
p_hydro_drivers$params_siml[[1]]$use_pml = F

parjj = read.csv("ancillary_data/fitted_params_Joshi_et_al_2022.csv")
parjj = parjj %>% mutate(K.scalar = K.scalar*1e-16)
pars_joshi2022 = parjj %>% 
  filter(Species != "Helianthus annuus") %>% 
  select(K.scalar, P50, alpha, gamma, A.G, Species) %>% 
  filter(A.G != "") %>% 
  # mutate(A.G = case_match(A.G,
  #                              "M. Angiosperm" ~ "Angiosperm",
  #                              .default = A.G)) %>% 
  pivot_longer(cols=c("K.scalar", "P50", "alpha", "gamma")) %>% 
  group_by(A.G, name) %>% 
  summarize(mean=mean(value), sd=sd(value), n=length(value))


# igbp = "WSA"
type = p_hydro_drivers$site_info[[1]]$IGBP_veg_short %>% 
  case_match(c("WSA", "EBF", "DBF") ~ "Angiosperm",
             c("ENF") ~ "Gymnosperm",
             c("OSH", "CSH") ~ "Shrub",
             c("GRA", "CRO", "SAV") ~ "Shrub",
             c("MF", "WET") ~ "Angiosperm")

message("type = ", p_hydro_drivers$site_info[[1]]$IGBP_veg_short, " --> " ,type, "\n")

gamma_mean = pars_joshi2022 %>% 
  filter(A.G == type & name == "gamma") %>% 
  pull(mean)

gamma_sd = pars_joshi2022 %>% 
  filter(A.G == type & name == "gamma") %>% 
  pull(sd) %>% c(0.1) %>% max()


print(c(gamma_mean, gamma_sd))

uniform_range = function(lower, upper){
  list(lower= lower, upper=upper, mean = (upper+lower)/2, sd = (upper-lower)*10)
}

gaussian_range = function(mean, sd){
  if (mean > 0) list(lower= max(mean-5*sd, 0), upper=mean+5*sd, mean = mean, sd = sd)
  else          list(lower= mean-5*sd, upper=min(mean+5*sd, 0), mean = mean, sd = sd)
}

# par = list(
#   kphio = list(lower=0.04, upper=0.09, init=0.05),
#   phydro_K_plant = list(lower=0.05e-16, upper=0.3e-16, init=0.15e-16),
#   phydro_p50_plant = list(lower=-3, upper=-0.5, init=-1),
#   phydro_alpha = list(lower=0.06, upper=0.15, init=0.1),
#   phydro_gamma = list(lower=0.5, upper=1.5, init=1),
#   bsoil = list(lower=0.1, upper=10, init=3),
#   Ssoil = list(lower=0.1, upper=1000, init=10),
#   whc = list(lower=10, upper=5000, init=1000),
#   err_gpp = list(lower = 0.01, upper = 4, init = 2),
#   err_le = list(lower = 0.1e6, upper = 10e6, init = 2e6)
# ),

pars_calib = list(
  # kphio = uniform_range(lower=0.005, upper=0.09),
  # phydro_K_plant = gaussian_range(mean = pars_joshi2022 %>% filter(A.G=="Gymnosperm", name=="K.scalar") %>% pull(mean),
  #                                 sd = pars_joshi2022 %>% filter(A.G=="Gymnosperm", name=="K.scalar") %>% pull(sd)),
  # phydro_p50_plant = gaussian_range(mean = pars_joshi2022 %>% filter(A.G=="Gymnosperm", name=="P50") %>% pull(mean),
  #                                   sd = pars_joshi2022 %>% filter(A.G=="Gymnosperm", name=="P50") %>% pull(sd)),
  phydro_K_plant = uniform_range(lower=0.1e-16, 1e-16),
  phydro_p50_plant = uniform_range(lower=-4, -0.3),
  # phydro_alpha = gaussian_range(mean = pars_joshi2022 %>% filter(A.G=="Gymnosperm", name=="alpha") %>% pull(mean),
  #                               sd = pars_joshi2022 %>% filter(A.G=="Gymnosperm", name=="alpha") %>% pull(sd)),
  # phydro_gamma = gaussian_range(mean = pars_joshi2022 %>% filter(A.G=="Gymnosperm", name=="gamma") %>% pull(mean),
  #                               sd = pars_joshi2022 %>% filter(A.G=="Gymnosperm", name=="gamma") %>% pull(sd)),
  phydro_alpha = gaussian_range(mean = 0.11, sd = 0.02),
  # phydro_gamma = uniform_range(lower = 0.1, upper = 2),
  phydro_gamma = gaussian_range(mean = gamma_mean, sd = gamma_sd),
  #bsoil = uniform_range(lower=0.1, upper=10),
  Ssoil = uniform_range(lower = 0, upper = whc_site+whc_site_sd),
  whc = gaussian_range(mean = whc_site, sd = whc_site_sd),
  err_gpp = uniform_range(lower = 0.01, upper = 4),
  err_le = uniform_range(lower = 0.1e6, upper = 10e6)
)

pars_fixed = list(         # fix all other parameters
  kphio              = 0.045,
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41,
  # phydro_K_plant     = 0.3e-16,
  # phydro_P50_plant     = -1,
  phydro_b_plant     = 1,
  # phydro_alpha       = 0.1,
  # phydro_gamma       = 1
  bsoil              = 3
  # Ssoil              = 40,
  # whc                = 90  
)

# Define calibration settings and parameter ranges from previous work
settings_bayes <- list(
  method = "BayesianTools",
  par = pars_calib,
  metric = rsofun::cost_likelihood_pmodel,
  control = list(
    sampler = "DEzs",
    settings = list(
      nrChains =   ifelse(debug, yes = 1,    no = 3    ),
      burnin =     ifelse(debug, yes = 300,  no = 10000),        
      iterations = ifelse(debug, yes = 1200, no = 50000)     # kept artificially low
    )
  )
)

file_prefix = paste0(out_dir, site, "_nchains_",
                     settings_bayes$control$settings$nrChains,
                     "_nsteps_",
                     settings_bayes$control$settings$iterations,
                     "_burnin_",
                     settings_bayes$control$settings$burnin)

calib_file =paste0(file_prefix, "_mcmc_output.rda")

if (!plot_only){
  # Calibrate the model and optimize the free parameters using
  # demo datasets
  message("Begin calibration...")
  pars_calib_bayes <- calib_sofun(
    # calib_sofun arguments:
    drivers = p_hydro_drivers,  
    obs = p_hydro_validation,
    settings = settings_bayes,
    # extra arguments passed to the cost function:
    par_fixed = pars_fixed,
    targets = c("gpp", "le")           # define target variable GPP
  )
  
  save(pars_calib_bayes, file = calib_file)
  
} else{
  load(calib_file)
}

fig_file_prefix = paste0(figures_dir, site, "_nchains_",
                         settings_bayes$control$settings$nrChains,
                         "_nsteps_",
                         settings_bayes$control$settings$iterations,
                         "_burnin_",
                         settings_bayes$control$settings$burnin)
  
cairo_pdf(filename = paste0(fig_file_prefix, "_trace_plot.pdf"), width = 7, height = 9)
# plot(pars_calib_bayes$mod)
codachain = getSample(pars_calib_bayes$mod, coda = T, thin = "auto")
par(mfrow = c(4,3), mar=c(2,3,2,1), oma=c(1,1,1,1))
plot(codachain, density = F, auto.layout = F, smooth = T, xlab="")
dev.off()

cairo_pdf(filename = paste0(fig_file_prefix, "_correlation_plot.pdf"))
correlationPlot(pars_calib_bayes$mod)
dev.off()

cairo_pdf(filename = paste0(fig_file_prefix, "_marginal_plot.pdf"))
marginalPlot(pars_calib_bayes$mod)
dev.off()

pars_calib_bayes$par %>% 
  t() %>% as.data.frame() %>% 
  mutate(site = site) %>% 
  write.csv(file = paste0(file_prefix, "_MAP.csv"), row.names = F)

params_modl_opt = c(pars_calib_bayes$par, pars_fixed)

if(params_modl_opt %>% names() %>% duplicated %>% any()){
  message("Duplicated entries in optimal params")
  stop()
}

output_p_opt <- rsofun::runread_pmodel_f(
  p_hydro_drivers,
  par = params_modl_opt
)
plot_pmodel(output_p_opt)

output_p_opt$data[[1]] =
  p_hydro_validation$data[[1]] %>% 
  select(date, gpp, le) %>% 
  rename(gpp_obs = gpp, le_obs = le) %>% 
  right_join(output_p_opt$data[[1]])

save(output_p_opt, file=paste0(file_prefix, "_phydro_output.rda"))

plot_pmodel(output_p_opt, out_filename_prefix = fig_file_prefix)

# Calculate R2 and RMSE
output_p_opt$data[[1]] %>% select(date, gpp, le) %>% 
  drop_na %>% 
  melt("date", value.name = "pred") %>% 
  left_join(p_hydro_validation$data[[1]] %>% 
              select(date, gpp, le) %>%
              drop_na %>% 
              # mutate(gpp = gpp*86400/1e6*12) %>% # convert to gC m-2 day-1
              # mutate(le = le*86400) %>%  # convert W m-2 to J m-2 day-1 
              melt("date", value.name = "obs")) %>% 
  group_by(variable) %>% 
  summarize(r2 = cor(pred,obs)^2, 
            nrmse = sqrt(sum(pred-obs)^2/length(pred-obs))/mean(obs),
            site = site) %>% 
  write.csv(file=paste0(file_prefix, "_r2_nrmse.csv"), row.names = F)

toc()


