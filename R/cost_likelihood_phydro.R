cost_likelihood_phydro <- function(
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
  
  using_phydro = drivers$params_siml[[1]]$use_phydro
  
  # FIXME Jaideep: Instead of checking the number of params, 
  #    it might be better to check for presence of each param in par and par_fixed
  ## check input parameters
  expected_params = ifelse(using_phydro, yes=14, no=10)
  if( (length(par) + length(par_fixed)) != (expected_params + length(targets)) ){
    stop(paste0('Error: Input calibratable and fixed parameters (par = ',length(par),' and par_fixed = ',length(par_fixed),')
    do not match length of the required P-model parameters (',expected_params + length(targets),').'))
  }
  
  
  ## define parameter set based on calibrated parameters
  if (using_phydro){
    calib_param_names <- c('kphio', 'kphio_par_a', 'kphio_par_b',
                           'rd_to_vcmax', 'tau_acclim', 'kc_jmax',
                           'phydro_K_plant', 'phydro_p50_plant', 'phydro_b_plant',
                           'phydro_alpha', 'phydro_gamma',
                           'bsoil', 'Ssoil', 'whc')
  } else {
    calib_param_names <- c('kphio', 'kphio_par_a', 'kphio_par_b',
                           'soilm_thetastar', 'soilm_betao',
                           'beta_unitcostratio', 'rd_to_vcmax', 
                           'tau_acclim', 'kc_jmax', 'whc')
  }
  
  # FIXME Jaideep: Here it is assumed that the params in par will appear in exactly the same order in settings as in the list above. Better to do this in an order-independent way.
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
    params_modl <- as.list(par[1:expected_params])       # all parameters calibrated
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
  
  ## Calculate dpsi intercept from outputs
  df <- df |> 
    mutate(dpsi_int = purrr::map(.x=data, .f=function(d){
      # Remove extremeties (potentially spurious values)
      d_filt = d |> 
        mutate(psi_soil = psi_leaf + dpsi) |> 
        filter(psi_soil > min(psi_soil)+0.01) |>
        filter(psi_soil < max(psi_soil)-0.01)
      
      # calculate psi_soil threshold for "wet" regime
      psi_soil_max <- d_filt |>
        with(quantile(psi_soil, probs = 0.95))
      
      # calculate dpsi intercept as mean of dpsi in wet regime
      int_q = d_filt |>
        filter(psi_soil >= psi_soil_max) |>
        pull(dpsi) |>
        mean()
      
      # calculate actual dpsi intercept by fitting lm (might not work)
      mod = d_filt |> with(lm(psi_leaf~psi_soil))
      mods = summary(mod)
      int_reg = -mod$coefficients[1]
      p_slope = mods$coefficients[2,4]
      
      # if lm gives good fit, return actual intercept, else return wet-regime mean
      dpsi_int = ifelse(p_slope < 0.05, 
                        yes = int_reg, 
                        no = int_q)
      dpsi_int
    })) 
  
  ## clean model output and unnest
  df <- df |>
    dplyr::rowwise() |>
    dplyr::reframe(
      cbind(sitename, data[, c('date', unique(c('gpp', targets)))], dpsi_int) |>
        stats::setNames(c('sitename', 'date', paste0(unique(c('gpp', targets)), '_mod'), 'dpsi_int_mod'))
    ) # gpp is used to get average trait prediction
  
  # separate validation data into fluxes and traits, site by site
  is_flux <- apply(obs, 1, function(x){ 'date' %in% colnames(x$data)})
  
  if(sum(is_flux) > 0){
    flux_sites <- obs$sitename[is_flux]
    
    # Unnest flux observations for our targets
    obs_flux <- obs[is_flux, ] |>
      dplyr::select(sitename, data) |>
      tidyr::unnest(data) |>
      dplyr::select(any_of(c('sitename', 'date', targets))) |>
      mutate(dpsi_int = 1)
    
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
    
    # calculate normal log-likelihood
    ll <- sum(stats::dnorm(
      df_target[[paste0(target, '_mod')]],
      mean = df_target[[target]],
      sd = par[length(par)-length(targets) + i],
      log = TRUE
    ))
  }) |>
    unlist() |>
    sum()
  
  # compute ll for dpsi using a Gaussian prior with mean 1 and sd 0.33
  ll_dpsi = sum(stats::dnorm(
    df_flux[['dpsi_int_mod']],
    mean = df_flux[['dpsi_int']],
    sd = 0.33,
    log = TRUE
  ))
  
  ll <- ll + ll_dpsi 
  
  # trap boundary conditions
  if(is.nan(ll) | is.na(ll) | ll == 0){ll <- -Inf}
  
  return(ll)
}
