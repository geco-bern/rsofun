# Produce figures and results for paper

library(rsofun)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sensitivity)
library(BayesianTools)
library(tictoc)

# Figure 1 #####

# define model parameter values from Stocker et al 2020
params_modl <- list(
  kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
  soilm_betao        = 0.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41
)

# run the model for these parameters
output <- rsofun::runread_pmodel_f(
  p_model_drivers,
  par = params_modl
)

# Create data.frame for plotting
df_gpp_plot <- rbind(
  output |>
    filter(sitename == "FR-Pue") |>
    unnest(data) |>
    select(date, gpp) |>
    mutate(type = "P-model output"),
  p_model_validation |>
    filter(sitename == "FR-Pue") |>
    unnest(data) |>
    select(date, gpp) |>
    mutate(type = "Observed")
)
df_gpp_plot$type <- factor(df_gpp_plot$type,
                           levels = c('P-model output',
                                      'Observed'))

# Plot GPP
ggplot(data = df_gpp_plot) +
  geom_line(
    aes(x = date,
        y = gpp,
        color = type),
    alpha = 0.7
  ) +
  scale_color_manual(values = c(
    'P-model output'='grey70',
    'Observed'='black')) +
  theme_classic() +
  theme(panel.grid.major.y = element_line()) +
  labs(
    x = 'Date',
    y = expression(paste("GPP (g C m"^-2, "s"^-1, ")")),
    colour = ""
  )

# Figure 2 ####

# Some code lines are commented out, because they create the object loaded below
# and take a long time to run.
# For checking reproducibility, run the commented code.

# Load Morris sensitivity output from sensitivity_analysis.Rmd
load("vignettes/files/morrisOut.rda")

# Define log-likelihood function
ll_pmodel <- function(
    par_v                 # a vector of all calibratable parameters including errors
){
  rsofun::cost_likelihood_pmodel(        # reuse likelihood cost function
    par_v,
    obs = rsofun::p_model_validation,
    drivers = rsofun::p_model_drivers,
    targets = "gpp"
  )
}

# best parameter values (from previous literature)
par_cal_best <- c(
  kphio              = 0.09423773,
  kphio_par_a        = -0.0025,
  kphio_par_b        = 20,
  soilm_thetastar    = 0.6*240,
  soilm_betao        = 0.2,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,
  tau_acclim         = 30.0,
  kc_jmax            = 0.41,
  error_gpp          = 1
)

# lower bound
par_cal_min <- c(
  kphio              = 0.03,
  kphio_par_a        = -0.004,
  kphio_par_b        = 10,
  soilm_thetastar    = 0,
  soilm_betao        = 0,
  beta_unitcostratio = 50.0,
  rd_to_vcmax        = 0.01,
  tau_acclim         = 7.0,
  kc_jmax            = 0.2,
  error_gpp          = 0.01
)

# upper bound
par_cal_max <- c(
  kphio              = 0.15,
  kphio_par_a        = -0.001,
  kphio_par_b        = 30,
  soilm_thetastar    = 240,
  soilm_betao        = 1,
  beta_unitcostratio = 200.0,
  rd_to_vcmax        = 0.1,
  tau_acclim         = 60.0,
  kc_jmax            = 0.8,
  error_gpp          = 4
)

# Create BayesinaTools setup object
morris_setup <- BayesianTools::createBayesianSetup(
  likelihood = ll_pmodel,
  prior = BayesianTools::createUniformPrior(par_cal_min, par_cal_max, par_cal_best),
  names = names(par_cal_best)
)

# # Run Morris sensitivity analysis
# set.seed(432)
# morrisOut <- sensitivity::morris(
#   model = morris_setup$posterior$density,
#   factors = names(par_cal_best),
#   r = 1000,
#   design = list(type = "oat", levels = 20, grid.jump = 3),
#   binf = par_cal_min,
#   bsup = par_cal_max,
#   scale = TRUE)

# Summarise the morris output
morrisOut.df <- data.frame(
  parameter = names(par_cal_best),
  mu.star = apply(abs(morrisOut$ee), 2, mean, na.rm = T),
  sigma = apply(morrisOut$ee, 2, sd, na.rm = T)
) %>%
  arrange( mu.star )

# Create barplot to show sensitivity analysis output
morrisOut.df |>
  tidyr::pivot_longer( -parameter, names_to = "variable", values_to = "value") |>
  ggplot(aes(
    reorder(parameter, value),
    value, 
    fill = variable),
    color = NA) +
  geom_bar(position = position_dodge(), stat = 'identity') +
  scale_fill_manual("", 
                    labels = c('mu.star' = expression(mu * "*"),
                               'sigma' = expression(sigma)),
                    values = c('mu.star' = "#29a274ff", 
                               'sigma' = "#777055ff")) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_blank(),
    legend.position = c(0.9, 0.1), legend.justification = c(0.95, 0.05)
  ) +
  coord_flip()    # make horizontal

# Figure 3 ####

# Some code lines are commented out, because they create the object loaded below
# and take a long time to run.
# For checking reproducibility, run the commented code.

# Load calibration output from sensitivity_analysis.Rmd
load("vignettes/files/par_calib.rda")

# Code to produce file
# Calibrates kphio, betao, kc_jmax - top 3 model params
set.seed(2023)

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
  drivers = p_model_drivers,
  obs = p_model_validation,
  settings = settings_calib,
  par_fixed = list(
    kphio_par_a = -0.0025,
    kphio_par_b = 20,
    soilm_thetastar    = 0.6*240,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,
    tau_acclim         = 30.0),
  targets = "gpp"
)

toc() # took 1555.601 sec to run

# Save result
saveRDS(par_calib, file = "analysis/calibration_exploration_files/par_calib_12000iterations.rsd")

# Define functions for plotting (re-use BayesianTools hidden code)
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

plot_prior_posterior_density(par_calib$mod)

# Figure 4 ####

# Some code lines are commented out, because they create the object loaded below
# and take a long time to run.
# For checking reproducibility, run the commented code.

# Evaluation of the uncertainty coming from the model parameters' uncertainty

# Sample parameter values from the posterior distribution
samples_par <- getSample(par_calib$mod,
                         thin = 46,              # get 600 samples in total
                         whichParameters = 1:4) |>
  as.data.frame() |>
  dplyr::mutate(mcmc_id = 1:n()) |>
  tidyr::nest(.by = mcmc_id, .key = "pars")

# Define function to run model for a set of sampled parameters
run_pmodel <- function(sample_par){
  # Function that runs the P-model for a sample of parameters
  # and also adds the new observation error
  
  out <- runread_pmodel_f(
    drivers = p_model_drivers,
    par =  list(                      # copied from par_fixed above
      kphio = sample_par$kphio,
      kphio_par_a = -0.0025,
      kphio_par_b = 20,
      soilm_thetastar    = 0.6*240,
      soilm_betao        = sample_par$soilm_betao,
      beta_unitcostratio = 146.0,
      rd_to_vcmax        = 0.014,
      tau_acclim         = 30.0,
      kc_jmax            = sample_par$kc_jmax)       # value from posterior
  )
  
  # return modelled GPP and prediction for a new GPP observation
  gpp <- out$data[[1]][, "gpp"]
  data.frame(gpp = gpp, 
             gpp_pred = gpp + rnorm(n = length(gpp), mean = 0, 
                                   sd = sample_par$err_gpp),
             date = out$data[[1]][, "date"])
}

set.seed(2023)
# Run the P-model for each set of parameters
pmodel_runs <- samples_par |>
  dplyr::mutate(sim = purrr::map(pars, ~run_pmodel(.x))) |>
  # format to obtain 90% credible intervals
  dplyr::select(mcmc_id, sim) |>
  tidyr::unnest(sim) |>
  dplyr::group_by(date) |>
  # compute quantiles for each day
  dplyr::summarise(
    gpp_q05 = quantile(gpp, 0.05, na.rm = TRUE),
    gpp = quantile(gpp, 0.5, na.rm = TRUE),          # get median
    gpp_q95 = quantile(gpp, 0.95, na.rm = TRUE),
    gpp_pred_q05 = quantile(gpp_pred, 0.05, na.rm = TRUE),
    gpp_pred_q95 = quantile(gpp_pred, 0.95, na.rm = TRUE)
  )

# Plot the credible intervals computed above
# for the first year only
plot_gpp_error <- ggplot(data = pmodel_runs |>
                           dplyr::slice(1:365) |>
                           dplyr::left_join(
                             # Merge GPP validation data (first year)
                             p_model_validation$data[[1]][1:365, ] |>
                               dplyr::rename(gpp_obs = gpp),
                             by = "date") |>
                           dplyr::left_join(
                             # Merge GPP before calibration
                             output$data[[1]][1:365, ] |>
                               dplyr::select(date, gpp) |>
                               dplyr::rename(gpp_no_calib = gpp),
                             by = "date")
                         ) +             # Plot only first year
  geom_ribbon(
    aes(ymin = gpp_pred_q05, 
        ymax = gpp_pred_q95,
        x = date,
        fill = "Model uncertainty")) +
  geom_ribbon(
    aes(ymin = gpp_q05, 
        ymax = gpp_q95,
        x = date,
        fill = "Parameter uncertainty")) +
  
  # geom_line(
  #   aes(
  #     date,
  #     gpp_no_calib,
  #     color = "Non-calibrated predictions"
  #   ), 
  #   lty = 2
  # ) +
  geom_line(
    aes(
      date,
      gpp,
      color = "Predictions"
    )
  ) +
  geom_line(
    aes(
      date,
      gpp_obs,
      color = "Observations"
    )
  ) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom") +
  labs(
    x = 'Date',
    y = expression(paste("GPP (g C m"^-2, "s"^-1, ")"))
  )

# Include observations in the plot
plot_gpp_error +  
  scale_color_manual(name = "",
                     breaks = c("Observations",
                                "Predictions",
                                "Non-calibrated predictions"),
                     values = c(t_col("black", 10),
                                t_col("#E69F00", 10),
                                t_col("#56B4E9", 10))) +
  scale_fill_manual(name = "",
                     breaks = c("Model uncertainty",
                                "Parameter uncertainty"),
                     values = c(t_col("#E69F00", 60),
                                t_col("#009E73", 40)))


# Result RMSE ####

# Compute RMSE for the non-calibrated P-model run
(output$data[[1]]$gpp - p_model_validation$data[[1]]$gpp)^2 |>
  mean(na.rm = TRUE) |>
  sqrt()

# Compute RMSE for P-model run after calibration

## update parameter values
params_modl_calib <- params_modl
params_modl_calib$kphio <- par_calib$par[1]
params_modl_calib$soilm_betao <- par_calib$par[2]
params_modl_calib$kc_jmax <- par_calib$par[3]
params_modl_calib$err_gpp <- par_calib$par[4]

# run the model for these parameters
output_calib <- rsofun::runread_pmodel_f(
  p_model_drivers,
  par = params_modl_calib
)

# Compute RMSE
(output_calib$data[[1]]$gpp - p_model_validation$data[[1]]$gpp)^2 |>
  mean(na.rm = TRUE) |>
  sqrt()
