# Script to test all the possible cost functions
# and see if the calibrated parameters lead to better results

set.seed(10)

# Load libraries
library(rsofun)
library(dplyr)
library(ggplot2)

# Set initial parameter values
pars <- list(
  kphio           = 0.04607080,
  soilm_par_a     = 2.75687824,
  soilm_par_b     = 1.68140444,
  tau_acclim_tempstress = 7.35259044,
  par_shape_tempstress  = 0.09863961
)

# Create the cost functions and calibration settings
arg <- purrr::cross(list(
  setup = c('BRC', 'FULL'), 
  method = c('GenSA', 'BayesianTools')))

control <- list(
  GenSA = list(
    maxit = 100),
  BayesianTools = list(
    sampler = 'DEzs',
    settings = list(
      burnin = 500,
      iterations = 1500)
    ))

settings <- lapply(arg, function(p){
  setting <- list(
    method = p$method,
    targetvars = c('gpp'),
    metric = create_cost_rmse(params_modl = pars,
                              setup = p$setup,
                              method = p$method),
    dir_results = "./",
    control = control[[p$method]]
    )
  
  if(p$setup == 'FULL'){
    setting$par <- list(
      kphio = list(lower=0.04, upper=0.2, init=0.05),
      soilm_par_a = list(lower=0.1, upper=5, init=2.4),
      soilm_par_b = list(lower=1, upper=2, init=1.5)
    )
  }else if(p$setup == 'BRC'){
    setting$par <- list(
      kphio = list(lower=0.04, upper=0.2, init=0.05)
    )
  }
  setting
})

# Run calibration routines
outputs <- lapply(settings, function(f){
  calib_sofun(p_model_drivers,
              p_model_validation,
              f)
})

# Run P-model with original parameters
model_data <- runread_pmodel_f(
  p_model_drivers,
  par = pars
) %>%
  filter(sitename == "FR-Pue") %>%
  tidyr::unnest(data)

validation_data <- p_model_validation %>%
  filter(sitename == "FR-Pue") %>%
  tidyr::unnest(data)

# Function that updates parameter values
update_pars <- function(params_modl, new_params){
  for(n in names(new_params)){
    params_modl[n] <- new_params[n]
  }
  return(params_modl)
}

# Function that computes the RMSE between observed GPP and predicted GPP
rmse <- function(obs, pred){
  obs.na <- !is.na(obs)
  sqrt(sum((obs[obs.na] - pred[obs.na])^2))
}

# Now plot the results
par(mfrow = c(2,2))
lapply(outputs, function(out){
  
  model_data_new <- runread_pmodel_f(
    p_model_drivers,
    par = update_pars(pars, out$par)
  ) %>%
    filter(sitename == "FR-Pue") %>%
    tidyr::unnest(data)
  
  ggplot() +
    geom_line(
      data = model_data,
      aes(
        date,
        gpp
      ),
      colour = "red",
      alpha = 0.5
    ) +
    geom_line(
      data = validation_data,
      aes(
        date,
        gpp
      )
    ) +
    geom_line(
      data = model_data_new,
      aes(
        date,
        gpp
      ),
      colour = "blue",
      alpha = 0.7
    ) +
    labs(
      x = "Date",
      y = "GPP",
      title = paste("RMSE change after calibration:", 
                         rmse(validation_data$gpp, model_data$gpp) - rmse(validation_data$gpp, model_data_new$gpp))
    )
})
# Since we use RMSE as a metric, it must be reduced and even more reduced when
# there are more calibratable parameters. This is simple math.