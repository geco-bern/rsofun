# Script running Bayesian calibration

# Load libraries
library(rsofun)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sensitivity)
library(BayesianTools)
library(tictoc)

# Define functions for plotting

getSetup <- function(x) {
  classes <- class(x)
  if (any(c('mcmcSampler', 'smcSampler') %in% classes)) x$setup
  else if (any(c('mcmcSamplerList', 'smcSamplerList') %in% classes)) x[[1]]$setup
  else stop('Can not get setup from x')
}

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

plot_prior_posterior_density <- function(
    x               # Bayesian calibration output
){
  # Get matrices of prior and posterior samples
  posteriorMat <- getSample(x, parametersOnly = TRUE)
  priorMat <-  getSetup(x)$prior$sampler(10000) # nPriorDraws = 10000
  
  # Parameter names
  nPar <- ncol(posteriorMat)
  parNames <- colnames(posteriorMat)
  # rename columns priorMat
  colnames(priorMat) <- parNames  
  
  # Create data frame for plotting
  df_plot <- rbind(data.frame(posteriorMat, distrib = "posterior"),
                   data.frame(priorMat, distrib = "prior")
  )
  df_plot$distrib <- as.factor(df_plot$distrib)
  
  # Plot with facet wrap
  df_plot |>
    tidyr::gather(variable, value, kphio:err_gpp) |>
    ggplot(
      aes(x = value, fill = distrib)
    ) +
    geom_density() +
    theme_classic() +
    facet_wrap( ~ variable , nrow = 2, scales = "free") +
    theme(legend.position = "bottom",
          axis.title.x = element_text("")) +
    ggtitle("Marginal parameter uncertainty") +
    scale_fill_manual(values = c("#29a274ff", t_col("#777055ff"))) # GECO colors
  
}

# Set random seed for reproducibility
set.seed(432)

# Define calibration settings
settings_calib <- list(
  method = "BayesianTools",
  metric = rsofun::cost_likelihood_pmodel,
  control = list(
    sampler = "DEzs",
    settings = list(
      burnin = 1500,
      iterations = 12000,
      nrChains = 3,       # number of independent chains
      startValue = 3      # number of internal chains to be sampled
    )),
  par = list(
    kphio = list(lower = 0.03, upper = 0.15, init = 0.05),
    soilm_betao = list(lower = 0, upper = 1, init = 0.2),
    kc_jmax = list(lower = 0.2, upper = 0.8, init = 0.41),
    err_gpp = list(lower = 0.1, upper = 3, init = 0.8)
  )
)

# Run calibration and measure time
tic()

par_calib <- calib_sofun(
  drivers = rsofun::p_model_drivers,
  obs = rsofun::p_model_validation,
  settings = settings_calib,
  par_fixed = list(
    kphio_par_a = -0.0025,            # define model parameter values from
    kphio_par_b = 20,                 # Stocker et al. 2020
    soilm_thetastar    = 0.6*240,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,
    tau_acclim         = 30.0),
  targets = "gpp"
)

toc() # Stop measuring time

# Plot prior and posterior distributions
plot_prior_posterior_density(par_calib$mod)
