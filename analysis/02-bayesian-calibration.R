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
  parNames <- colnames(posteriorMat)
  # rename columns priorMat
  colnames(priorMat) <- parNames  
  
  # Create data frame for plotting
  df_plot <- rbind(data.frame(posteriorMat, distrib = "posterior"),
                   data.frame(priorMat, distrib = "prior")
  )
  df_plot$distrib <- as.factor(df_plot$distrib)
  
  # Plot with facet wrap
  gg <- df_plot |>
    tidyr::gather(variable, value, kphio:err_gpp) |>
    ggplot(
      aes(x = value, fill = distrib)
    ) +
    geom_density() +
    theme_classic() +
    facet_wrap( ~ variable , nrow = 2, scales = "free") +
    theme(legend.position = "bottom",
          axis.title.x = element_text("")) +
    scale_fill_manual(NULL, values = c("#29a274ff", t_col("#777055ff"))) # GECO colors
  
  return(gg)
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
      burnin = 12000,
      iterations = 24000,
      nrChains = 3,       # number of independent chains
      startValue = 3      # number of internal chains to be sampled
    )),
  par = list(
    kphio = list(lower = 0.02, upper = 0.15, init = 0.05),
    kphio_par_a =list(lower = -0.004, upper = -0.001, init = -0.0025),
    kphio_par_b = list(lower = 10, upper = 30, init = 20),
    soilm_thetastar = list(
      lower = 0.01 * rsofun::p_model_drivers$site_info[[1]]$whc,
      upper = 1.0  * rsofun::p_model_drivers$site_info[[1]]$whc,
      init  = 0.6  * rsofun::p_model_drivers$site_info[[1]]$whc
    ),
    soilm_betao = list(lower = 0.0, upper = 1.0, init = 0.0),
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
    beta_unitcostratio = 146.0,
    kc_jmax            = 0.41,
    rd_to_vcmax        = 0.014,
    tau_acclim         = 20.0
    ),
  targets = "gpp"
)

toc() # Stop measuring time

# Plot prior and posterior distributions
gg <- plot_prior_posterior_density(par_calib$mod)
gg

get_runtime <- function(par_calib) {# function(settings_calib){
  total_time_secs <- sum(unlist(lapply(
    par_calib$mod,
    function(curr_chain){curr_chain$settings$runtime[["elapsed"]]})))
  return(sprintf("Total runtime: %.0f secs", total_time_secs))
}

# Plot MCMC diagnostics
plot(par_calib$mod)
summary(par_calib$mod) # Gives Gelman Rubin multivariate of 1.019
summary(par_calib$par)
print(get_runtime(par_calib))

# Output
## Define MCMC postprocessing
get_settings_str <- function(par_calib) {# function(settings_calib){
  stopifnot(is(par_calib$mod, "mcmcSamplerList"))
  
  # explore what's in a mcmcSamplerList:
  # summary(par_calib$mod)
  # plot(par_calib$mod)
  individual_chains <- par_calib$mod
  nrChains <- length(individual_chains) # number of chains
  # plot(individual_chains[[1]]) # chain 1
  # plot(individual_chains[[2]]) # chain 2
  # plot(individual_chains[[3]]) # chain 3
  # class(individual_chains[[1]]$setup); individual_chains[[1]]$setup # Bayesian Setup
  # individual_chains[[1]]$chain
  # individual_chains[[1]]$X
  # individual_chains[[1]]$Z
  nrInternalChains <- lapply(individual_chains, function(curr_chain){curr_chain$settings$nrChains})  |> unlist() |> unique() |> paste0(collapse = "-")
  nrIterations     <- lapply(individual_chains, function(curr_chain){curr_chain$settings$iterations})|> unlist() |> unique() |> paste0(collapse = "-")
  nrBurnin         <- lapply(individual_chains, function(curr_chain){curr_chain$settings$burnin})    |> unlist() |> unique() |> paste0(collapse = "-")
  sampler_name     <- lapply(individual_chains, function(curr_chain){curr_chain$settings$sampler})   |> unlist() |> unique() |> paste0(collapse = "-")
  
  # create descriptive string of settings for filename
  return(sprintf(
    "Sampler-%s-%siterations_ofwhich%sburnin_chains_%sx%s_",
    sampler_name, nrIterations, nrBurnin, nrChains,  nrInternalChains))
}

settings_string <- get_settings_str(par_calib)

saveRDS(par_calib, file = paste0("./analysis/paper_results_files/par_calib.rds"))

ggsave("./analysis/paper_results_files/prior_posterior.pdf", plot = gg, width = 6, height = 5)
ggsave("./analysis/paper_results_files/prior_posterior.png", plot = gg, width = 6, height = 5)

