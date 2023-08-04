# Produce figures and results for paper

library(rsofun)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sensitivity)
library(BayesianTools)

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

# Load Morris sensitivity output from sensitivity_analysis.Rmd
load("vignettes/files/morrisOut.rda")

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

# Summarise the morris output
morrisOut.df <- data.frame(
  parameter = names(par_cal_best),
  mu.star = apply(abs(morrisOut$ee), 2, mean, na.rm = T),
  sigma = apply(morrisOut$ee, 2, sd, na.rm = T)
) %>%
  arrange( mu.star )


morrisOut.df |>
  tidyr::pivot_longer( -parameter, names_to = "variable", values_to = "value") |>
  ggplot(aes(
    reorder(parameter, value),
    value, 
    fill = variable),
    color = NA) +
  geom_bar(position = position_dodge(), stat = 'identity') +
  scale_fill_brewer("", labels = c('mu.star' = expression(mu * "*"),
                                   'sigma' = expression(sigma)),
                    palette = "Greys") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_blank(),
    legend.position = c(0.05, 0.95), legend.justification = c(0.05, 0.95)
  )

# Figure 3 ####

# Load calibration output from sensitivity_analysis.Rmd
load("vignettes/files/par_calib.rda")

# Load code to produce credible intervals from sensitivity_analysis.Rmd
load("vignettes/files/pmodel_runs.rda")

# Plot the credible intervals computed above
# for the first year only
plot_gpp_error <- ggplot(data = pmodel_runs |>
                           dplyr::slice(1:365)) +             # Plot only first year
  geom_ribbon(
    aes(ymin = gpp_q05, 
        ymax = gpp_q95,
        x = date),
    fill = 'blue', alpha = 0.5) +
  geom_ribbon(
    aes(ymin = gpp_obs_q05, 
        ymax = gpp_obs_q95,
        x = date),
    fill = 'grey40', alpha = 0.2) +
  geom_line(
    aes(
      date,
      gpp
    ),
    colour = "grey40",
    alpha = 0.8
  ) +
  theme_classic() +
  theme(panel.grid.major.y = element_line()) +
  labs(
    x = 'Date',
    y = expression(paste("GPP (g C m"^-2, "s"^-1, ")"))
  )

# Define GPP validation data (first year)
validation_data <- p_model_validation$data[[1]][1:365, ]

# Include observations in the plot
plot_gpp_error +  
  geom_line(
    data = validation_data,
    aes(
      date,
      gpp
    ),
    alpha = 0.8
  )


# Result RMSE ####

# Compute RMSE for the non-calibrated P-model run
(output$data[[1]]$gpp - p_model_validation$data[[1]]$gpp)^2 |>
  mean(na.rm = TRUE) |>
  sqrt()

# Compute RMSE for P-model run after calibration

## update parameter values
params_modl_calib <- params_modl
params_modl_calib$kphio <- par_calib$par[1]
params_modl_calib$kphio_par_a <- par_calib$par[2]
params_modl_calib$kphio_par_b <- par_calib$par[3]
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
