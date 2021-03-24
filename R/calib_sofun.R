#' Calibrates SOFUN model parameters
#'
#' This is the main function that handles the calibration of SOFUN model parameters. 
#' 
#' @param df_drivers asdf
#' @param ddf_obs A data frame containing observational data used for model calibration. Created by function \code{get_obs_calib2()}
#' @param settings A list containing model calibration settings. See vignette_rsofun.pdf for more information and examples.
#'
#' @return A complemented named list containing the calibration settings and optimised parameter values.
#' @export
#'
#' @examples \dontrun{ calib_sofun( df_drivers = filter(df_drivers, sitename %in% settings$sitenames),  ddf_obs = ddf_obs_calib, settings = settings)}
#' 
calib_sofun <- function( df_drivers, ddf_obs, settings ){

  # targetvars <- paste0( settings$targetvars, "_obs")
  
  ## Use only calibsites
  df_drivers <- df_drivers %>% 
    dplyr::filter(sitename %in% settings$sitenames)

  ## make global #commented out for calibrating lm3ppa
  # targetvars_with_unc <- c(targetvars, paste0(settings$targetvars, "_unc")) 
  # ddf_obs <- ddf_obs %>% 
    # tidyr::unnest(data) %>% 
    # rename(gpp_obs = gpp) %>%  
    # dplyr::select( date, sitename, one_of( targetvars_with_unc ) ) %>% 
    # dplyr::filter( sitename %in% settings$sitenames )

  if (nrow(ddf_obs)>0){

    ## Example run for getting structure of output file
    if (identical(names(settings$par), "kphio")){
      ## For calibrating quantum yield efficiency only
      # cost_rmse <- cost_chisquared_kphio
      cost_rmse <- cost_rmse_kphio
    
    } else if ( "kphio" %in% names(settings$par) && "soilm_par_a" %in% names(settings$par) && "soilm_par_b" %in% names(settings$par) ){  
      ## Full stack calibration
      cost_rmse <- cost_rmse_fullstack   

    } else if ( "vpdstress_par_a" %in% names(settings$par) && "vpdstress_par_b" %in% names(settings$par) && "vpdstress_par_m" %in% names(settings$par) ){  
      ## Calibration of VPD stress function (P-model runs with soilmstress and tempstress on)
      # cost_rmse <- cost_chisquared_vpdstress
      cost_rmse <- cost_rmse_vpdstress
      
    }  else if ( "kphio" %in% names(settings$par) && "phiRL" %in% names(settings$par) && "LAI_light" %in% names(settings$par) &&
     "tf_base" %in% names(settings$par) && "par_mort" %in% names(settings$par) ){  
      cost_rmse <- cost_rmse_lm3ppa

    }  else if ( "phiRL" %in% names(settings$par) && "LAI_light" %in% names(settings$par) &&
     "tf_base" %in% names(settings$par) && "par_mort" %in% names(settings$par) ){  
      cost_rmse <- cost_rmse_lm3ppa_gsleuning

    } 

    ##----------------------------------------------------------------
    ## Do the calibration
    ##----------------------------------------------------------------
    ## check directory where calibration results are stored
    if (!dir.exists(settings$dir_results)) system( paste0( "mkdir -p ", settings$dir_results) )

    if (settings$method=="gensa"){
      ##----------------------------------------------------------------
      ## calibrate the model parameters using GenSA (simulated annealing)
      ##----------------------------------------------------------------
      ptm <- proc.time()
      out_optim <- GenSA(
                          par   = lapply( settings$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
                          fn    = cost_rmse,
                          lower = lapply( settings$par, function(x) x$lower ) %>% unlist(),
                          upper = lapply( settings$par, function(x) x$upper ) %>% unlist(),
                          control=list( 
                                        #temperature=4000, 
                                        max.call=settings$maxit,
                                        trace.mat=TRUE,
                                        threshold.stop=1e-2, #1e-4,
                                        # nb.stop.improvement=5,
                                        max.time=300
                                        ),
                          ddf_obs = ddf_obs,
                          df_drivers = df_drivers
                        )
      out_optim$time_optim <- proc.time() - ptm
      print(out_optim$par)

      filn <- paste0( settings$dir_results, "/out_gensa_", settings$name, ".Rdata")
      print( paste0( "writing output from GenSA function to ", filn ) )
      save( out_optim, file = filn )
      
      # ## Test plot with the same SOFUN call
      # plot_test_kphio( out_optim$par, subtitle = "", label = paste("CALIB test", settings$name), makepdf = FALSE )    


    } else if (settings$method=="optimr"){
      ##----------------------------------------------------------------
      ## Calibrate the model parameters using R optimr(). Optimr is a 
      ## wrapper that implements different optimization methods.
      ## Here, we're using unconstrained parameters. The optimization 
      ## method is the default for unconstrained parameters: 
      ## Rvmminu, Rcgminu, lbfgsb3, newuoa, nmkb (this is a list).
      ##----------------------------------------------------------------
      ptm <- proc.time()
      out_optim <- optimr(
                          par     = lapply( settings$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
                          fn      = cost_rmse,
                          control = list( maxit = settings$maxit )
                          )
      proc.time() - ptm
      print(out_optim$par)

      filn <- paste0( settings$dir_results, "/out_optimr_", settings$name, ".Rdata")
      print( paste0( "writing output from optimr function to ", filn ) )
      save( out_optim, file = filn )

      # ## Test plot with the same SOFUN call
      # plot_test_kphio( out_optim$par, subtitle = "", label = paste("CALIB test", settings$name), makepdf = FALSE )


    } else if (settings$method=="BayesianTools"){
      ##----------------------------------------------------------------
      ## Calibrate the model parameters using Bayesian calibration 
      ## implemented in 'BayesianTools'. I.e., the posterior density is
      ## maximised (explicitly: the sum of log-likelihoods is minimized) 
      ##----------------------------------------------------------------
      ptm <- proc.time()
      bt_setup    <- createBayesianSetup( cost_rmse( inverse=TRUE ), 
                                          lower = apply( settings$par, function(x) x$lower ) %>% unlist(), 
                                          upper = lapply( settings$par, function(x) x$upper ) %>% unlist() 
                                          )
      bt_settings <- list( iterations = settings$maxit,  message = TRUE )
      out_optim <- runMCMC( bayesianSetup = bt_setup, sampler = "DEzs", settings = bt_settings )
      proc.time() - ptm
      

    } else if (settings$method=="linscale"){
      ##----------------------------------------------------------------
      ## Calibrate the quantum yield efficiency parameter ('kphio') 
      ## applied as a linear scalar to simulated GPP.
      ##----------------------------------------------------------------
      ## Combine obs and mod by columns and make global
      modobs <<- bind_cols( obs, mod ) %>% 
                 mutate( gpp_mod = gpp_mod / param_init )

      ptm <- proc.time()
      out_optim <- optimr(
                          par     = lapply( settings$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
                          fn      = cost_linscale_rmse,
                          control = list( maxit = settings$maxit )
                          )
      proc.time() - ptm
      print(out_optim$par)

      filn <- paste0( settings$dir_results, "/out_linscale_", settings$name, ".Rdata")
      print( paste0( "writing output from linscale function to ", filn ) )
      save( out_optim, file = filn )

    }

    # ## xxx Test---------------------------
    # parvals <- seq( from = lapply( settings$par, function(x) x$lower ) %>% unlist(),
    #                 to   = lapply( settings$par, function(x) x$upper ) %>% unlist(),
    #                 length.out = 20 )
    # cost <- purrr::map( as.list( parvals ), ~cost_rmse( par = list(.) ) ) %>% unlist()
    # plot( parvals, cost, pch=16 )
    # abline(v=out_optim$par[1], col="red")
    # ## -----------------------------------
    
    ## save optimised parameters
    names(out_optim$par) <- names(settings$par)
    for (parnam in names(settings$par)){
      settings$par[[ parnam ]]$opt <- out_optim$par[ parnam ]
    }
    settings$par_opt <- out_optim$par
    
  } else {

    print("WARNING: No observational target data left after filtering.")

    settings$par$kphio$opt          <- NA
    settings$par$temp_ramp_edge$opt <- NA
    settings$par$soilm_par_a$opt    <- NA
    settings$par$soilm_par_b$opt    <- NA

  }

  ## Write calibrated parameters into a CSV file
  vec <- unlist( unname( lapply( settings$par, function(x) x$opt  )) )
  df <- as_tibble(t(vec)) %>% setNames( names(settings$par) )
  filn <- paste0( settings$dir_results, "/params_opt_", settings$name, ".csv")
  print( paste0( "writing calibrated parameters to ", filn ) )
  write_csv( df, path = filn )

  return( settings )

}

##------------------------------------------------------------
## Generic cost function of model-observation (mis-)match using
## root mean square error.
##------------------------------------------------------------
cost_rmse_kphio <- function( par, ddf_obs, df_drivers, inverse = FALSE ){

  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = par[1],
    soilm_par_a     = 1.0,
    soilm_par_b     = 0.0,
    vpdstress_par_a = 0.2,
    vpdstress_par_b = 0.2,
    vpdstress_par_m = 5
    )
  
  df <- runread_pmodel_f(
    df_drivers, 
    params_modl = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
    ) %>%   
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data) %>% 
    dplyr::rename(gpp_mod = gpp) %>% 
    dplyr::left_join(ddf_obs %>% 
                       unnest(data), 
                     by = c("sitename", "date"))
  
  ## Calculate cost (RMSE)
  cost <- sqrt( mean( (df$gpp_mod - df$gpp_obs )^2, na.rm = TRUE ) )
  
  # print(paste("par =", paste(par, collapse = ", " ), "cost =", cost))
  
  if (inverse) cost <- 1.0 / cost

  return(cost)
}

##------------------------------------------------------------
## Generic cost function of model-observation (mis-) match using
## the chi-squared statistic ( ).
##------------------------------------------------------------
cost_chisquared_kphio <- function( par, inverse = FALSE ){
  
  ## Full stack calibration
  out <- system( paste0("echo ", simsuite, " ", sprintf( "%f %f %f %f %f %f", par[1], 1.0, 0.0, -9999.0, -9999.0, -9999.0 ), " | ./run", model ), intern = TRUE )  ## single calibration parameter
  
  ## read output from calibration run
  out <- read_fwf( outfilnam, col_positions, col_types = cols( col_double() ) )
  
  ## Combine obs and mod by columns
  out <- bind_cols( obs, out )
  
  ## Calculate cost (chi-squared)
  cost <- ((out$gpp_mod - out$gpp_obs )/(out$gpp_unc))^2
  cost <- sum(cost, na.rm = TRUE)/sum(!is.na(cost))
  
  if (inverse) cost <- 1.0 / cost
  
  return(cost)
}

##------------------------------------------------------------
## Generic cost function of model-observation (mis-)match using
## root mean square error.
##------------------------------------------------------------
cost_rmse_fullstack <- function( par, ddf_obs, df_drivers, inverse = FALSE ){

  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = par[1],
    soilm_par_a     = par[2],
    soilm_par_b     = par[3],
    vpdstress_par_a = 0.0,
    vpdstress_par_b = 0.0,
    vpdstress_par_m = 0
  )
  
  df <- runread_pmodel_f(
    df_drivers, 
    params_modl = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
    ) %>%   
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data) %>% 
    dplyr::rename(gpp_mod = gpp) %>% 
    dplyr::left_join(ddf_obs, by = c("sitename", "date"))
  
  ## Calculate cost (RMSE)
  cost <- sqrt( mean( (df$gpp_mod - df$gpp_obs )^2, na.rm = TRUE ) )
  
  # print(paste("par =", paste(par, collapse = ", " ), "cost =", cost))
  
  if (inverse) cost <- 1.0 / cost

  return(cost)
}

##------------------------------------------------------------
## Generic cost function of model-observation (mis-) match using
## root mean square error.
##------------------------------------------------------------
cost_rmse_vpdstress <- function( par, ddf_obs, df_drivers, inverse = FALSE ){
  
  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = 0.04971,
    soilm_par_a     = 1.0,
    soilm_par_b     = 0.0,
    vpdstress_par_a = par[1],
    vpdstress_par_b = par[2],
    vpdstress_par_m = par[3]
  )
  
  df <- runread_pmodel_f(
    df_drivers, 
    params_modl = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
    ) %>%   
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data) %>% 
    dplyr::rename(latenth_mod = latenth) %>% 
    dplyr::left_join(ddf_obs, by = c("sitename", "date"))
  
  ## Calculate cost (RMSE)
  cost <- sqrt( mean( (df$latenth_mod - df$latenth_obs )^2, na.rm = TRUE ) )
  
  # print(paste("par =", paste(par, collapse = ", " ), "cost =", cost))
  
  if (inverse) cost <- 1.0 / cost  
  
  return(cost)
}

##------------------------------------------------------------
## Generic cost function of model-observation (mis-) match using
## the chi-squared statistic (Keenan et al., 2012 GCB).
##------------------------------------------------------------
cost_chisquared_vpdstress <- function( par, ddf_obs, df_drivers, inverse = FALSE ){
  
  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  params_modl <- list(
    kphio           = 0.04971,
    soilm_par_a     = 1.0,
    soilm_par_b     = 0.0,
    vpdstress_par_a = par[1],
    vpdstress_par_b = par[2],
    vpdstress_par_m = par[3]
  )
  
  # df <- df_drivers %>% 
  #   mutate(data = purrr::pmap(
  #     .,
  #     run_sofun_f_bysite,
  #     params_modl = params_modl,
  #     makecheck = FALSE
  #   )) %>% 
  df <- runread_pmodel_f(
    df_drivers, 
    params_modl = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
    ) %>%   
    dplyr::select(sitename, data) %>% 
    tidyr::unnest(data) %>% 
    dplyr::rename(latenth_mod = latenth) %>% 
    dplyr::left_join(ddf_obs, by = c("sitename", "date"))
  
  ## Calculate cost (RMSE)
  cost <- ((df$latenth_mod - df$latenth_obs )/(2 * df$latenth_unc))^2
  cost <- sum(cost, na.rm = TRUE)/sum(!is.na(cost))
  
  # print(paste("par =", paste(par, collapse = ", " ), "cost =", cost))

  if (inverse) cost <- 1.0 / cost
  
  return(cost)
}

##------------------------------------------------------------
## LM3-PPA calibration p-model
##------------------------------------------------------------
cost_rmse_lm3ppa <- function( par, ddf_obs, df_drivers, inverse = FALSE ){
  
  # Add changed model parameters to df_drivers, overwriting where necessary.
  df_drivers$params_species[[1]]$kphio[]      <- par[1]  # the same for all values
  df_drivers$params_species[[1]]$phiRL[]      <- par[2]  # the same for all values
  df_drivers$params_species[[1]]$LAI_light[]  <- par[3]  # the same for all values
  df_drivers$params_tile[[1]]$tf_base         <- par[4]
  df_drivers$params_tile[[1]]$par_mort        <- par[5]

  df <- runread_lm3ppa_f(
    df_drivers, 
    makecheck = TRUE,
    parallel = FALSE
  ) 
  
  # Aggregate variables from the model df taking the last 500 yrs
  df_mod <- df$data[[1]]$output_annual_tile %>% 
    tail(500) %>% 
    dplyr::summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC))
  
  dff <- data.frame(
    variables = c("GPP","LAI","Density","Biomass"),
    targets_mod = c(df_mod$GPP, df_mod$LAI, df_mod$Density, df_mod$Biomass)
    ) %>% 
    dplyr::left_join(ddf_obs, by = "variables") %>% 
    mutate(error = targets_mod - targets_obs) %>% 
    mutate(error_rel = error / targets_obs)
  
  ## Calculate cost (RMSE) across the N targets
  cost <- sqrt(mean(dff$error_rel^2, na.rm = TRUE))
  
  print(paste("par =", paste(par, collapse = ", " ), "cost =", cost))
  
  if (inverse) cost <- 1.0 / cost  
  
  return(cost)
}

##------------------------------------------------------------
## LM3-PPA calibration gs-Leuning
##------------------------------------------------------------
cost_rmse_lm3ppa_gsleuning <- function( par, ddf_obs, df_drivers, inverse = FALSE ){
  
  # Add changed model parameters to df_drivers, overwriting where necessary.
  df_drivers$params_species[[1]]$phiRL[]      <- par[1]  # the same for all values
  df_drivers$params_species[[1]]$LAI_light[]  <- par[2]  # the same for all values
  df_drivers$params_tile[[1]]$tf_base         <- par[3]
  df_drivers$params_tile[[1]]$par_mort        <- par[4]

  df <- runread_lm3ppa_f(
    df_drivers, 
    makecheck = TRUE,
    parallel = FALSE
  ) 
  
  # Aggregate variables from the model df taking the last 500 yrs
  df_mod <- df$data[[1]]$output_annual_tile %>% 
    tail(500) %>% 
    dplyr::summarise(GPP = mean(GPP), LAI= quantile(LAI, probs = 0.95, na.rm=T), Density=mean(Density12), Biomass=mean(plantC))
  
  dff <- data.frame(
    variables = c("GPP","LAI","Density","Biomass"),
    targets_mod = c(df_mod$GPP, df_mod$LAI, df_mod$Density, df_mod$Biomass)
    ) %>% 
    dplyr::left_join(ddf_obs, by = "variables") %>% 
    mutate(error = targets_mod - targets_obs) %>% 
    mutate(error_rel = error / targets_obs)
  
  ## Calculate cost (RMSE) across the N targets
  cost <- sqrt(mean(dff$error_rel^2, na.rm = TRUE))
  
  print(paste("par =", paste(par, collapse = ", " ), "cost =", cost))
  
  if (inverse) cost <- 1.0 / cost  
  
  return(cost)
}

##------------------------------------------------------------
## Cost function of linearly scaled output
##------------------------------------------------------------
cost_linscale_rmse <- function( par ){
  
  ## Calculate cost (RMSE). 'modobs' is a global variable.
  cost <- sqrt( mean( ( par * modobs$gpp_mod - modobs$gpp_obs )^2, na.rm = TRUE ) )

  return(cost)
}

##------------------------------------------------------------
## Generic cost function of model-observation (mis-)match using
## mean absolute error.
##------------------------------------------------------------
cost_mae <- function( par ){

  ## execute model for this parameter set
  out <- system( paste0("echo ", simsuite, " ", sprintf( "%f", par[1] ), " | ./run", model ), intern = TRUE )

  ## read output from calibration run
  out <- read_fwf( outfilnam, col_positions )
  
  ## Combine obs and mod by columns
  out <- bind_cols( obs, out )
  
  ## Calculate cost (RMSE)
  cost <- mean( abs( out$gpp_mod - out$gpp_obs ), na.rm = TRUE )

  return(cost)
}


