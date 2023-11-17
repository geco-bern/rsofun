# This function implements the likelihood cost function used for calibration,
# but considers a model error that scales with GPP magnitude, thus accounting
# for linear heterokedasticity of the error term.

cost_likelihood_heteroskedastic_pmodel <- function(
    par,   # model parameters & error terms for each target
    obs,
    drivers,
    targets,
    par_fixed = NULL,   # non-calibrated model parameters
    parallel = FALSE,
    ncores = 2
){
  # predefine variables for CRAN check compliance
  sitename <- data <- gpp_mod <- NULL
  
  ## check input parameters
  if( (length(par) + length(par_fixed)) != (9 + 2*length(targets)) ){
    stop('Error: Input calibratable and fixed parameters (par and par_fixed)
    do not match length of the required P-model parameters and target error terms.')
  }
  
  ## define parameter set based on calibrated parameters
  calib_param_names <- c('kphio', 'kphio_par_a', 'kphio_par_b',
                         'soilm_thetastar', 'soilm_betao',
                         'beta_unitcostratio', 'rd_to_vcmax', 
                         'tau_acclim', 'kc_jmax')
  
  if(!is.null(par_fixed)){
    params_modl <- list()
    # complete with calibrated values
    i <- 1 # start counter
    for(par_name in calib_param_names){
      if(is.null(par_fixed[[par_name]])){
        params_modl[[par_name]] <- par[i]   # use calibrated par value
        i <- i + 1                          # counter of calibrated params
      }else{
        params_modl[[par_name]] <- par_fixed[[par_name]]  # use fixed par value
      }
    }
  }else{
    params_modl <- as.list(par[1:9])       # all parameters calibrated
    names(params_modl) <- calib_param_names
  }
  
  ## run the model
  df <- runread_pmodel_f(
    drivers,
    par = params_modl,
    makecheck = TRUE,
    parallel = parallel,
    ncores = ncores
  )
  
  ## clean model output and unnest
  df <- df |>
    dplyr::rowwise() |>
    dplyr::reframe(
      cbind(sitename, data[, c('date', unique(c('gpp', targets)))]) |>
        stats::setNames(c('sitename', 'date', paste0(unique(c('gpp', targets)), '_mod')))
    ) # gpp is used to get average trait prediction
  
  # separate validation data into fluxes and traits, site by site
  is_flux <- apply(obs, 1, function(x){ 'date' %in% colnames(x$data)})
  
  if(sum(is_flux) > 0){
    flux_sites <- obs$sitename[is_flux]
    
    # Unnest flux observations for our targets
    obs_flux <- obs[is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', 'date', targets)))
    
    if(ncol(obs_flux) < 3){
      warning("Dated observations (fluxes) are missing for the chosen targets.")
      df_flux <- data.frame()
    }else{
      # Join P-model output and flux observations
      df_flux <- df |>
        dplyr::filter(sitename %in% flux_sites) |>
        dplyr::left_join(
          obs_flux, 
          by = c('sitename', 'date'))    # observations with missing date are ignored
    }
  }else{
    df_flux <- data.frame()
  }
  
  if(sum(!is_flux) > 0){
    trait_sites <- obs$sitename[!is_flux]
    
    # Unnest trait observations for our targets
    obs_trait <- obs[!is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', targets)))
    
    if(ncol(obs_trait) < 2){
      warning("Non-dated observations (traits) are missing for the chosen targets.")
      df_trait <- data.frame()
    }else{
      # Join output and trait observations
      df_trait <- df |>
        dplyr::filter(sitename %in% trait_sites) |>
        dplyr::group_by(sitename) |>
        # get growing season average traits
        dplyr::summarise(across(ends_with("_mod") & !starts_with('gpp'),
                                ~ sum(.x * gpp_mod/sum(gpp_mod)),
                                .names = "{.col}")) |>
        dplyr::left_join(
          obs_trait,
          by = c('sitename')        # compare yearly averages rather than daily obs
        )
    }
  }else{
    df_trait <- data.frame()
  }
  
  # loop over targets
  ll <- lapply(seq(length(targets)), function(i){
    target <- targets[i]
    # get observations and predicted target values, without NA 
    if(target %in% colnames(df_flux)){
      df_target <- df_flux[, c(paste0(target, '_mod'), target)] |>
        tidyr::drop_na()
    }else{
      df_target <- data.frame()
    }
    if(target %in% colnames(df_trait)){
      df_target <- rbind(df_target,
                         df_trait[, c(paste0(target, '_mod'), target)] |>
                           tidyr::drop_na())
    }
    
    # calculate normal log-likelihood with varying standard deviation
    ll <- sum(stats::dnorm(
      df_target[[paste0(target, '_mod')]],
      mean = df_target[[target]],
      sd = par[length(par)-2*length(targets) + i] + 
        par[length(par) - 2*length(targets) + i+1] * df_target[[paste0(target, '_mod')]],
      log = TRUE
    ))
  }) |>
    unlist() |>
    sum()
  
  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}
  
  return(ll)
}

# Now we try to run the calibration
set.seed(2023)

# Define calibration settings
settings_calib <- list(
  method = "BayesianTools",
  metric = cost_likelihood_heteroskedastic_pmodel,
  control = list(
    sampler = "DEzs",
    settings = list(
      burnin = 3000,
      iterations = 9000,
      nrChains = 3,        # number of independent chains
      startValue = 3       # number of internal chains to be sampled
    )),
  par = list(
    kphio = list(lower = 0.03, upper = 0.15, init = 0.05),
    kphio_par_a = list(lower = -0.004, upper = -0.001, init = -0.0025),
    kphio_par_b = list(lower = 10, upper = 30, init =25),
    err_gpp_a = list(lower = 0.1, upper = 3, init = 0.8),
    err_gpp_b = list(lower = -1, upper = 1, init = 0)
  )
)

# Calibrate kphio-related parameters and err_gpp 
par_calib <- calib_sofun(
  drivers = p_model_drivers,
  obs = p_model_validation,
  settings = settings_calib,
  par_fixed = list(
    soilm_thetastar    = 0.6*240,
    soilm_betao        = 0.2,
    beta_unitcostratio = 146.0,
    rd_to_vcmax        = 0.014,
    tau_acclim         = 30.0,
    kc_jmax            = 0.41),
  targets = "gpp"
)

