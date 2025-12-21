## Script running sensitivity analysis

# Load libraries
library(rsofun)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sensitivity)
library(BayesianTools)

# Set random seed for reproducibility
set.seed(432)

# Define log-likelihood function
ll_pmodel <- function(
    par_v                 # a named vector of all calibratable parameters including errors
){
  rsofun::cost_likelihood_pmodel(        # likelihood cost function from package
    par_v,
    obs = rsofun::p_model_validation,    # example data from package
    drivers = rsofun::p_model_drivers,
    targets = "gpp"
  )
}

# Define parameter bounds (from previous literature)

# best parameter values (initial values)
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
  prior = BayesianTools::createUniformPrior(par_cal_min, 
                                            par_cal_max, 
                                            par_cal_best),
  names = names(par_cal_best)
)

# Run Morris sensitivity analysis
set.seed(432)
morrisOut <- sensitivity::morris(
  model = morris_setup$posterior$density,
  factors = names(par_cal_best),
  r = 1000,
  design = list(type = "oat", levels = 20, grid.jump = 3),
  binf = par_cal_min,
  bsup = par_cal_max,
  scale = TRUE
  )

# Summarise the morris output into statistics
morrisOut.df <- data.frame(
  parameter = names(par_cal_best),
  mu.star = apply(abs(morrisOut$ee), 2, mean, na.rm = T),
  sigma = apply(morrisOut$ee, 2, sd, na.rm = T)
) %>%
  arrange( mu.star )

# Create barplot to show sensitivity analysis output
gg <- morrisOut.df |>
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

ggsave("./analysis/paper_results_files/morris.pdf", plot = gg, width = 5, height = 3)
ggsave("./analysis/paper_results_files/morris.png", plot = gg, width = 5, height = 3)
