rm(list=ls())
library(tidyverse)
library(reshape2)
library(rsofun)
library(BayesianTools)
library(tictoc)

plot_only = F

tic("phydro")

## Generate data if not already available

args = commandArgs(trailingOnly=TRUE)

#site <- "GF-Guy"
if (length(args)==0) {
  #stop("At least one argument must be supplied: site name", call.=FALSE)
  site = "GF-Guy"
}else{
  site = args[1]
}

data_dir = "~/Downloads/fluxdatakit_oct3/Phydro_drivers_3/"
out_dir = "~/Downloads/fluxdatakit_oct3/phydro_output/"
figures_dir = "~/Downloads/fluxdatakit_oct3/phydro_output/figures/"

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
  dens <- MASS::kde2d(x, y, ...)
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
  
  if (out_filename_prefix == ""){
    print(p1)
    print(p2)
  }
  else{
    cairo_pdf(filename = paste0(out_filename_prefix, "_timeseries.pdf"), height=5, width=7)
    print(p1)
    dev.off()
    cairo_pdf(filename = paste0(out_filename_prefix, "_pred_vs_obs.pdf"), height=3.5, width=7)
    print(p2)
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
  bsoil              = 3.193891e+00, #3,
  Ssoil              = 3.193891e+02, #3,
  whc                = 7.819509e+02 #200
)

output_p <- rsofun::runread_pmodel_f(
  p_hydro_drivers,
  par = params_modl
)
plot_pmodel(output_p)


## P-hydro full calibration (BayesianTools)

p_hydro_drivers$params_siml[[1]]$use_phydro = T
p_hydro_drivers$params_siml[[1]]$use_pml = F

# Define calibration settings and parameter ranges from previous work
settings_bayes <- list(
  method = "BayesianTools",
  par = list(
    kphio = list(lower=0.04, upper=0.09, init=0.05),
    phydro_K_plant = list(lower=0.05e-16, upper=5e-16, init=0.3e-16),
    phydro_p50_plant = list(lower=-3, upper=-0.5, init=-1),
    phydro_alpha = list(lower=0.06, upper=0.15, init=0.1),
    phydro_gamma = list(lower=0.5, upper=1.5, init=1),
    bsoil = list(lower=0.1, upper=10, init=3),
    Ssoil = list(lower=0.1, upper=1000, init=10),
    whc = list(lower=10, upper=5000, init=1000),
    err_gpp = list(lower = 0.01, upper = 4, init = 2),
    err_le = list(lower = 0.1e6, upper = 10e6, init = 2e6)
  ),
  metric = rsofun::cost_likelihood_pmodel,
  control = list(
    sampler = "DEzs",
    settings = list(
      nrChains = 3,
      burnin = 10000,        
      iterations = 20000     # kept artificially low
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
  pars_calib_bayes <- calib_sofun(
    # calib_sofun arguments:
    drivers = p_hydro_drivers,  
    obs = p_hydro_validation,
    settings = settings_bayes,
    # extra arguments passed to the cost function:
    par_fixed = list(         # fix all other parameters
      kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
      kphio_par_b        = 1.0,
      rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
      tau_acclim         = 30.0,
      kc_jmax            = 0.41,
      # phydro_K_plant     = 0.3e-16,
      # phydro_P50_plant     = -1,
      phydro_b_plant     = 1
      # phydro_alpha       = 0.1,
      # phydro_gamma       = 1
      # bsoil              = 3,
      # Ssoil              = 40,
      # whc                = 90  
    ),
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

params_modl_opt = params_modl

params_modl_opt$kphio            = pars_calib_bayes$par[["kphio"]]
params_modl_opt$phydro_K_plant   = pars_calib_bayes$par[["phydro_K_plant"]]
params_modl_opt$phydro_p50_plant = pars_calib_bayes$par[["phydro_p50_plant"]]
params_modl_opt$phydro_alpha     = pars_calib_bayes$par[["phydro_alpha"]]
params_modl_opt$phydro_gamma     = pars_calib_bayes$par[["phydro_gamma"]]
params_modl_opt$bsoil            = pars_calib_bayes$par[["bsoil"]]
params_modl_opt$Ssoil            = pars_calib_bayes$par[["Ssoil"]]
params_modl_opt$whc              = pars_calib_bayes$par[["whc"]]

output_p_opt <- rsofun::runread_pmodel_f(
  p_hydro_drivers,
  par = params_modl_opt
)
plot_pmodel(output_p_opt)
save(output_p_opt, file=paste0(file_prefix, "_phydro_output.rda"))

plot_pmodel(output_p_opt, out_filename_prefix = fig_file_prefix)


toc()


