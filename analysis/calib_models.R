library(rsofun)
library(dplyr)
#library(tidyverse)
source("R/calib_sofun.R")

likelihood_biomee_test <- function(
    par,
    par_names,
    obs,
    targets,
    drivers
){
  
  # predefine variables for CRAN check compliance
  GPP <- LAI <- Density12 <- plantC <- error <- NULL
  
  # Add changed model parameters to drivers, overwriting where necessary.
  drivers$params_species[[1]]$phiRL[]      <- par[1]
  drivers$params_species[[1]]$LAI_light[]  <- par[2]
  drivers$params_tile[[1]]$tf_base         <- par[3]
  drivers$params_tile[[1]]$par_mort        <- par[4]
  
  # run model
  df <- runread_biomee_f(
    drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
  
  # did we spin up
  spin_up <- drivers$params_siml[[1]]$spinup
  
  # drop spinup years if activated
  # see below
  if (spin_up){
    spin_up_years <- drivers$params_siml[[1]]$spinupyears + 1
  } else {
    spin_up_years <- 0
  }
  
  # Aggregate variables from the model df taking the last 500 yrs
  # if spun up
  df <- df$data[[1]]$output_annual_tile %>%
    utils::tail(500 - spin_up_years) %>%
    dplyr::summarise(
      GPP = mean(GPP),
      LAI = stats::quantile(LAI, probs = 0.95, na.rm=T),
      Density = mean(Density12),
      Biomass = mean(plantC)
    )
  
  # reshuffle observed data
  col_names <- obs$data[[1]]$variables
  obs <- data.frame(t(obs$data[[1]]$targets_obs))
  colnames(obs) <- col_names
  
  # calculate the log likelihood
  logpost <- sapply(targets, function(i) {
    
    # select correct target variable
    # based on targets list
    predicted <- df %>%
      select(
        !!!i
      )
    
    observed <- obs %>%
      select(
        !!!i
      )
    
    # calculate likelihood
    # for all targets and their
    # error ranges
    ll <- BayesianTools::likelihoodIidNormal(
      predicted,
      observed,
      par[grep(paste0('^err_', i,'$'), par_names)]
    )
    
  })
  
  # sum log likelihoods
  logpost <- sum(unlist(logpost))
  
  # trap boundary conditions
  if(is.nan(logpost) | is.na(logpost) | logpost == 0 ){logpost <- -Inf}
  
  return(logpost)
}


df_drivers <- biomee_gs_leuning_drivers
ddf_obs <- biomee_validation_2
df_drivers$params_siml[[1]]$spinup <- FALSE

# Mortality as DBH
settings <- list(
  method              = "bayesiantools",
  targets             = c("GPP"),
    metric              = likelihood_biomee_test,
  control = list(
    sampler = "DEzs",
    settings = list(
      burnin = 1,
      iterations = 10000,
      nrChains = 1
    )
  ),
  par = list(
    phiRL = list(lower=0.5, upper=5, init=3.5),
    LAI_light = list(lower=2, upper=5, init=3.5),
    tf_base = list(lower=0, upper=1, init=0.5),
    par_mort = list(lower=0.1, upper=1, init=0.5),
    
    # uncertainties
    err_GPP = list(lower = 0, upper = 1, init = 0.5),
    err_LAI = list(lower = 0, upper = 1, init = 0.5),
    err_Density = list(lower = 0, upper = 1, init = 0.5),
    err_Biomass = list(lower = 0, upper = 1, init = 0.5)
  )
)

pars <- calib_sofun(
  drivers = df_drivers,
  obs = ddf_obs,
  settings = settings
)
