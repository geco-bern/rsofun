# Script getting the uncertainty estimation, using the
# output from 02-bayesian-calibration.R
getwd()

# Load libraries
library(rsofun)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sensitivity)
library(BayesianTools)

# Run 02-bayesian-calibration.R if missing calibration output object
if(!exists('par_calib')){
  source(here::here(
    'analysis/02-bayesian-calibration.R')) # path relative to project location
}

# Set random seed for reproducibility
set.seed(2023)

# Evaluation of the uncertainty coming from the model parameters' uncertainty

# Sample parameter values from the posterior distribution
samples_par <- getSample(par_calib$mod,
                         thin = 60,              # get 600 samples in total
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
      kphio_par_b = sample_par$kphio_par_b,
      soilm_thetastar    = 0.6*240,
      soilm_betao        = 0.2,
      beta_unitcostratio = 146.0,
      rd_to_vcmax        = 0.014,
      tau_acclim         = 30.0,
      kc_jmax            = sample_par$kc_jmax)       # value from posterior
  )
  
  # return modelled GPP and prediction for a new GPP observation
  gpp <- out$data[[1]][, "gpp"]
  data.frame(gpp = gpp, 
             gpp_pred = rnorm(n = length(gpp), mean = gpp,
                                    sd = sample_par$err_gpp),
             date = out$data[[1]][, "date"])
}

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
    gpp_q50 = quantile(gpp, 0.5, na.rm = TRUE),          # get median
    gpp_q95 = quantile(gpp, 0.95, na.rm = TRUE),
    gpp_pred_q05 = quantile(gpp_pred, 0.05, na.rm = TRUE),
    gpp_pred_q95 = quantile(gpp_pred, 0.95, na.rm = TRUE)
  )

# Plot the credible intervals computed above
# for the first year only
data_to_plot <- pmodel_runs |>
  # Plot only first year
  dplyr::slice(1:365) |>
  dplyr::left_join(
    # Merge GPP validation data (first year)
    p_model_validation$data[[1]][1:365, ] |>
      dplyr::rename(gpp_obs = gpp),
    by = "date")

plot_gpp_error <- ggplot(data = data_to_plot) +
  geom_ribbon(
    aes(ymin = gpp_pred_q05,
        ymax = gpp_pred_q95,
        x = date,
        fill = "Model uncertainty"
    )) +
  geom_ribbon(
    aes(ymin = gpp_q05, 
        ymax = gpp_q95,
        x = date,
        fill = "Parameter uncertainty"
    )) +
  # Include observations in the plot
  geom_point(
    aes(x = date,
        y = gpp_obs,
        color = "Observations"
    ),
  ) +
  geom_line(
    aes(x = date,
        y = gpp_q50,
        color = "Predictions"
    )
  ) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom") +
  labs(
    x = 'Date',
    y = expression(paste("GPP (g C m"^-2, "s"^-1, ")"))
  )

plot_gpp_error <- plot_gpp_error +
  scale_color_manual(NULL,
                     breaks = c("Observations",
                                "Predictions"),
                     values = c(t_col("black", 0),
                                t_col("tomato", 0))) +
  scale_fill_manual(NULL,
                    breaks = c("Model uncertainty",
                               "Parameter uncertainty"),
                    values = c(t_col("tomato", 50),
                               t_col("#1b9e77", 0)))
plot_gpp_error

settings_string <- get_settings_str(par_calib)
ggsave(paste0("./analysis/paper_results_files/",settings_string,"_gpp_predictions_observations.pdf"), plot = plot_gpp_error, width = 6, height = 5)
ggsave(paste0("./analysis/paper_results_files/",settings_string,"_gpp_predictions_observations.png"), plot = plot_gpp_error, width = 6, height = 5)
