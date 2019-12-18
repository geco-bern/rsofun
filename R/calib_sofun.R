#' Calibrates SOFUN model parameters
#'
#' This is the main function that handles the calibration of SOFUN model parameters. 
#' 
#' @param settings_calib A list containing model calibration settings. See vignette_rsofun.pdf for more information and examples.
#' @param df_drivers asdf
#' @param settings_input A list containing model input settings. See vignette_rsofun.pdf for more information and examples.
#' @param ddf_obs (Optional) A data frame containing observational data used for model calibration. Created by function \code{get_obs()}
#'
#' @return A complemented named list containing the calibration settings and optimised parameter values.
#' @export
#'
#' @examples settings_calib <- calib_sofun( setup, settings_calib, settings_sims, settings_input )
#' 
calib_sofun <- function( settings_calib, df_drivers, settings_input = NA, ddf_obs = NA ){

  targetvars <- paste0( settings_calib$targetvars, "_obs")
  
  ## Use only calibsites
  df_drivers <- df_drivers %>% 
    dplyr::filter(sitename %in% settings_calib$sitenames)
  
  ##----------------------------------------------------------------
  ## Collect observational data used as calibration target
  ##----------------------------------------------------------------
  if (identical(NA,ddf_obs)){
    print("Collecting observational target data ...")
    ddf_obs <- get_obs_calib( 
      settings_calib, 
      dplyr::select(df_drivers, sitename, siteinfo) %>% tidyr::unnest(siteinfo), 
      settings_input 
      )
  }

  ##----------------------------------------------------------------
  ## Apply filters
  ##----------------------------------------------------------------
  ## "filtering" pre-MODIS data
  idxs <- which(ddf_obs$date < lubridate::ymd("2000-02-18"))
  make_na_premodis <- function(vec, idxs){
    vec[idxs] <- NA
    return(vec)
  }
  ddf_obs <- ddf_obs %>% 
    dplyr::mutate_at( vars(one_of(targetvars)), ~make_na_premodis(., idxs) )
    

  ## make global
  targetvars_with_unc <- c(targetvars, paste0(settings_calib$targetvars, "_unc"))
  ddf_obs <- ddf_obs %>% 
    dplyr::select( date, sitename, one_of( targetvars_with_unc ) ) %>% 
    dplyr::filter( sitename %in% settings_calib$sitenames )

  if (nrow(ddf_obs)>0){

    ## Example run for getting structure of output file
    if (identical(names(settings_calib$par),"kphio")){
      ## For calibrating quantum yield efficiency only
      # cost_rmse <- cost_chisquared_kphio
      cost_rmse <- cost_rmse_kphio
    
    } else if ( "kphio" %in% names(settings_calib$par) && "soilm_par_a" %in% names(settings_calib$par) && "soilm_par_b" %in% names(settings_calib$par) ){  
      ## Full stack calibration
      cost_rmse <- cost_rmse_fullstack   

    } else if ( "vpdstress_par_a" %in% names(settings_calib$par) && "vpdstress_par_b" %in% names(settings_calib$par) && "vpdstress_par_m" %in% names(settings_calib$par) ){  
      ## Calibration of VPD stress function (P-model runs with soilmstress and tempstress on)
      # cost_rmse <- cost_chisquared_vpdstress
      cost_rmse <- cost_rmse_vpdstress
      
    }

    ##----------------------------------------------------------------
    ## Do the calibration
    ##----------------------------------------------------------------
    ## check directory where calibration results are stored
    if (!dir.exists(settings_calib$dir_results)) system( paste0( "mkdir -p ", settings_calib$dir_results) )

    if (settings_calib$method=="gensa"){
      ##----------------------------------------------------------------
      ## calibrate the model parameters using GenSA (simulated annealing)
      ##----------------------------------------------------------------
      ptm <- proc.time()
      out_optim <- GenSA(
                          par   = lapply( settings_calib$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
                          fn    = cost_rmse,
                          lower = lapply( settings_calib$par, function(x) x$lower ) %>% unlist(),
                          upper = lapply( settings_calib$par, function(x) x$upper ) %>% unlist(),
                          control=list( 
                                        #temperature=4000, 
                                        max.call=settings_calib$maxit,
                                        trace.mat=TRUE,
                                        threshold.stop=1e-4,
                                        max.time=300
                                        ),
                          ddf_obs = ddf_obs,
                          df_drivers = df_drivers
                        )
      out_optim$time_optim <- proc.time() - ptm
      print(out_optim$par)

      filn <- paste0( settings_calib$dir_results, "/out_gensa_", settings_calib$name, ".Rdata")
      print( paste0( "writing output from GenSA function to ", filn ) )
      save( out_optim, file = filn )
      
      # ## Test plot with the same SOFUN call
      # plot_test_kphio( out_optim$par, subtitle = "", label = paste("CALIB test", settings_calib$name), makepdf = FALSE )    


    } else if (settings_calib$method=="optimr"){
      ##----------------------------------------------------------------
      ## Calibrate the model parameters using R optimr(). Optimr is a 
      ## wrapper that implements different optimization methods.
      ## Here, we're using unconstrained parameters. The optimization 
      ## method is the default for unconstrained parameters: 
      ## Rvmminu, Rcgminu, lbfgsb3, newuoa, nmkb (this is a list).
      ##----------------------------------------------------------------
      ptm <- proc.time()
      out_optim <- optimr(
                          par     = lapply( settings_calib$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
                          fn      = cost_rmse,
                          control = list( maxit = settings_calib$maxit )
                          )
      proc.time() - ptm
      print(out_optim$par)

      filn <- paste0( settings_calib$dir_results, "/out_optimr_", settings_calib$name, ".Rdata")
      print( paste0( "writing output from optimr function to ", filn ) )
      save( out_optim, file = filn )

      # ## Test plot with the same SOFUN call
      # plot_test_kphio( out_optim$par, subtitle = "", label = paste("CALIB test", settings_calib$name), makepdf = FALSE )


    } else if (settings_calib$method=="BayesianTools"){
      ##----------------------------------------------------------------
      ## Calibrate the model parameters using Bayesian calibration 
      ## implemented in 'BayesianTools'. I.e., the posterior density is
      ## maximised (explicitly: the sum of log-likelihoods is minimized) 
      ##----------------------------------------------------------------
      ptm <- proc.time()
      bt_setup    <- createBayesianSetup( cost_rmse( inverse=TRUE ), 
                                          lower = apply( settings_calib$par, function(x) x$lower ) %>% unlist(), 
                                          upper = lapply( settings_calib$par, function(x) x$upper ) %>% unlist() 
                                          )
      bt_settings <- list( iterations = settings_calib$maxit,  message = TRUE )
      out_optim <- runMCMC( bayesianSetup = bt_setup, sampler = "DEzs", settings = bt_settings )
      proc.time() - ptm
      

    } else if (settings_calib$method=="linscale"){
      ##----------------------------------------------------------------
      ## Calibrate the quantum yield efficiency parameter ('kphio') 
      ## applied as a linear scalar to simulated GPP.
      ##----------------------------------------------------------------
      ## Combine obs and mod by columns and make global
      modobs <<- bind_cols( obs, mod ) %>% 
                 mutate( gpp_mod = gpp_mod / param_init )

      ptm <- proc.time()
      out_optim <- optimr(
                          par     = lapply( settings_calib$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
                          fn      = cost_linscale_rmse,
                          control = list( maxit = settings_calib$maxit )
                          )
      proc.time() - ptm
      print(out_optim$par)

      filn <- paste0( settings_calib$dir_results, "/out_linscale_", settings_calib$name, ".Rdata")
      print( paste0( "writing output from linscale function to ", filn ) )
      save( out_optim, file = filn )

    }

    # ## xxx Test---------------------------
    # parvals <- seq( from = lapply( settings_calib$par, function(x) x$lower ) %>% unlist(),
    #                 to   = lapply( settings_calib$par, function(x) x$upper ) %>% unlist(),
    #                 length.out = 20 )
    # cost <- purrr::map( as.list( parvals ), ~cost_rmse( par = list(.) ) ) %>% unlist()
    # plot( parvals, cost, pch=16 )
    # abline(v=out_optim$par[1], col="red")
    # ## -----------------------------------
    
    ## save optimised parameters
    names(out_optim$par) <- names(settings_calib$par)
    for (parnam in names(settings_calib$par)){
      settings_calib$par[[ parnam ]]$opt <- out_optim$par[ parnam ]
    }
    settings_calib$par_opt <- out_optim$par
    
  } else {

    print("WARNING: No observational target data left after filtering.")

    settings_calib$par$kphio$opt          <- NA
    settings_calib$par$temp_ramp_edge$opt <- NA
    settings_calib$par$soilm_par_a$opt    <- NA
    settings_calib$par$soilm_par_b$opt    <- NA

  }

  ## Write calibrated parameters into a CSV file
  vec <- unlist( unname( lapply( settings_calib$par, function(x) x$opt  )) )
  df <- as_tibble(t(vec)) %>% setNames( names(settings_calib$par) )
  filn <- paste0( settings_calib$dir_results, "/params_opt_", settings_calib$name, ".csv")
  print( paste0( "writing calibrated parameters to ", filn ) )
  write_csv( df, path = filn )

  return( settings_calib )

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

  df <- runread_sofun_f(
    df_drivers, 
    params_modl = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
    ) %>%   
  # df <- df_drivers %>% 
  #   mutate(out_sofun = purrr::pmap(
  #     .,
  #     run_sofun_f_bysite,
  #     params_modl = params_modl,
  #     makecheck = FALSE
  #   )) %>% 
    dplyr::select(sitename, out_sofun) %>% 
    dplyr::rename(id = sitename) %>% 
    tidyr::unnest(out_sofun) %>% 
    dplyr::rename(gpp_mod = gpp) %>% 
    dplyr::left_join(ddf_obs, by = c("sitename", "date"))
  
  ## Calculate cost (RMSE)
  cost <- sqrt( mean( (df$gpp_mod - df$gpp_obs )^2, na.rm = TRUE ) )
  
  print(paste("par =", paste(par, collapse = ", " ), "cost =", cost))
  
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
cost_rmse_fullstack <- function( par, inverse = FALSE ){

  ## Full stack calibration
  out <- system( paste0("echo ", simsuite, " ", sprintf( "%f %f %f %f %f %f", par[1], par[2], par[3], -9999.0, -9999.0, -9999.0 ), " | ./run", model ), intern = TRUE )  ## holding kphio fixed at previously optimised value for splined FPAR
  
  ## read output from calibration run
  out <- read_fwf( outfilnam, col_positions, col_types = cols( col_double() ) )
  
  ## Combine obs and mod by columns
  out <- bind_cols( obs, out )
  
  ## Calculate cost (RMSE)
  cost <- sqrt( mean( (out$gpp_mod - out$gpp_obs )^2, na.rm = TRUE ) )
  # print(paste("cost =", cost, "par =", paste(par, collapse = ", " )))
  
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
  
  df <- runread_sofun_f(
    df_drivers, 
    params_modl = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
    ) %>%   
  # df <- df_drivers %>% 
  #   mutate(out_sofun = purrr::pmap(
  #     .,
  #     run_sofun_f_bysite,
  #     params_modl = params_modl,
  #     makecheck = FALSE
  #   )) %>% 
    dplyr::select(sitename, out_sofun) %>% 
    dplyr::rename(id = sitename) %>% 
    tidyr::unnest(out_sofun) %>% 
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
  #   mutate(out_sofun = purrr::pmap(
  #     .,
  #     run_sofun_f_bysite,
  #     params_modl = params_modl,
  #     makecheck = FALSE
  #   )) %>% 
  df <- runread_sofun_f(
    df_drivers, 
    params_modl = params_modl, 
    makecheck = TRUE,
    parallel = FALSE
    ) %>%   
    dplyr::select(sitename, out_sofun) %>% 
    dplyr::rename(id = sitename) %>% 
    tidyr::unnest(out_sofun) %>% 
    dplyr::rename(latenth_mod = latenth) %>% 
    dplyr::left_join(ddf_obs, by = c("sitename", "date"))
  
  ## Calculate cost (RMSE)
  cost <- ((df$latenth_mod - df$latenth_obs )/(2 * df$latenth_unc))^2
  cost <- sum(cost, na.rm = TRUE)/sum(!is.na(cost))
  
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

#' Get observational data for calibration
#'
#' Gets observational data for model calibration by looping over sites
#' 
#' @param settings_calib A list containing model calibration settings. See vignette_rsofun.pdf for more information and examples.
#' @param settings_sims A list containing model simulation settings from \code{\link{prepare_setup_sofun}}.  See vignette_rsofun.pdf for more information and examples.
#' @param settings_input A list containing model input settings. See vignette_rsofun.pdf for more information and examples.
#'
#' @return A data frame (tibble) containing observational data used for model calibration
#' @export
#'
#' @examples ddf_obs <- get_obs_calib( settings_calib, settings_sims, settings_input )
#' 
##------------------------------------------------------------
## Gets data by looping over sites
##------------------------------------------------------------
get_obs_calib <- function( settings_calib, settings_sims, settings_input ){


  ##------------------------------------------------------------
  ## Read raw observational data from files.
  ## This creates a data frame (tibble) that contains
  ## a column 'date' and columns for each target variable. The number of rows
  ## corresponds to each simulation's length (number of days).
  ##------------------------------------------------------------
  ## loop over sites to get data frame with all variables
  list_bysite <- lapply( settings_calib$sitenames, 
                         function(x) get_obs_bysite(x,
                                                    settings_calib = settings_calib, 
                                                    settings_sims  = dplyr::filter(settings_sims, sitename == x),
                                                    settings_input = settings_input) %>% 
                           mutate( sitename=x ) 
                         )
  
  ## combine dataframes from multiple sites along rows
  ddf_obs <- dplyr::bind_rows( list_bysite )
  rm("list_bysite")

  return( ddf_obs )

}


##------------------------------------------------------------
## Function returns a data frame (tibble) containing all the observational data
## used as target variables for calibration for a given site, covering specified dates.
##------------------------------------------------------------
get_obs_bysite <- function( mysitename, settings_calib, settings_sims, settings_input ){

  print(paste("getting obs for ", mysitename))

  ## Initialise daily dataframe (WITHOUT LEAP YEARS, SOFUN USES FIXED 365-DAYS YEARS!)
  ddf <- init_dates_dataframe( year(settings_sims$date_start), year(settings_sims$date_end), noleap = TRUE )

  if ("gpp" %in% settings_calib$targetvars){

    ## Interpret benchmarking data specification
    datasource <- stringr::str_split( settings_calib$datasource, "_" ) %>% unlist()

    if ("fluxnet2015" %in% datasource){
      ##------------------------------------------------------------
      ## Get FLUXNET 2015 data
      ##------------------------------------------------------------
      ## Make sure data is available for this site
      getvars <- c("GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "NEE_VUT_REF_NIGHT_QC", "NEE_VUT_REF_DAY_QC", "GPP_NT_VUT_SE", "GPP_DT_VUT_SE")
      
      error <- check_download_fluxnet2015( settings_calib$path_fluxnet2015, mysitename )
      
      ddf <- get_obs_bysite_fluxnet2015(
        mysitename, 
        path_fluxnet2015 = settings_input$path_fluxnet2015, 
        timescale = settings_calib$timescale$gpp,
        getvars = getvars, 
        getswc = FALSE,
        threshold_GPP = settings_calib$threshold_GPP, 
        remove_neg = FALSE,
        verbose = TRUE
        ) %>% 
        dplyr::mutate( gpp_obs = case_when("NT" %in% datasource & !("DT" %in% datasource) ~ GPP_NT_VUT_REF,
                                           "DT" %in% datasource & !("NT" %in% datasource) ~ GPP_DT_VUT_REF
                                           # "DT" %in% datasource &   "NT" %in% datasource  ~ (GPP_NT_VUT_REF+GPP_DT_VUT_REF)/2
                                           ),
                       gpp_unc = case_when("NT" %in% datasource & !("DT" %in% datasource) ~ GPP_NT_VUT_SE,
                                           "DT" %in% datasource & !("NT" %in% datasource) ~ GPP_DT_VUT_SE )) %>% 
        dplyr::right_join( ddf, by = "date" ) %>% 
        dplyr::select(date, gpp_obs, gpp_unc)
      
      # ## replace observations with NA if uncertainty is very small -- only necessary when using chi-squared calibration
      # targetvars_unc <- paste0(settings_calib$targetvars, "_unc")
      # replace_zero_with_na <- function(obs, unc){
      #   idxs <- which( unc < 0.0001 )
      #   obs[idxs] <- NA
      #   return(obs)
      # }
      # ddf <- ddf %>% 
      #   dplyr::mutate( gpp_obs = replace_zero_with_na(gpp_obs, gpp_unc) )
      # 
      
      # test <- sum(!is.na(ddf$gpp_obs))
      # if (test<3) rlang::abort(paste0("Too hard filtering for site ", mysitename))

    } else {
      
      ddf <- ddf %>% dplyr::mutate( gpp_obs = NA, gpp_unc = NA )
      
    }

    if ("Ty" %in% datasource){
      ##------------------------------------------------------------
      ## Get GePiSaT data
      ##------------------------------------------------------------
      ## Make sure data is available for this site
      error <- check_download_gepisat( settings_calib$path_gepisat, mysitename )

      ddf_gepisat <- get_obs_bysite_gpp_gepisat( mysitename, settings_calib$path_gepisat, settings_calib$timescale )

      ## add to other data frame and take take weighted average for updated 'gpp_obs'
      if (!is.null(ddf_gepisat)){
        
        ddf <- ddf_gepisat %>%
          ## Some GPP data looks weird when its error in resp. day is zero. Exclude this data.
          mutate( gpp_obs = ifelse( gpp_err_obs == 0.0, NA, gpp_obs ) ) %>% 
          dplyr::rename( gpp_obs_gepisat = gpp_obs ) %>%
          right_join( ddf, by = "date" )
        totlen <- length(datasource[ -which( datasource=="fluxnet2015" ) ])
        if (totlen>1){
          ddf$gpp_obs <- sum( c(ddf$gpp_obs * (totlen-1), ddf$gpp_obs_gepisat), na.rm = TRUE ) / totlen
        } else {
          ddf <- ddf %>% mutate( gpp_obs = gpp_obs_gepisat )
        }
        
      } else {
        ## No GePiSaT data available for this site. Consider all GPP data missing (NA).
        ddf <- ddf %>% mutate( gpp_obs = NA )
      }

    }

  }

  if ("latenth" %in% settings_calib$targetvars){
    
    ## Interpret benchmarking data specification
    datasource <- stringr::str_split( settings_calib$datasource, "_" ) %>% unlist()
    
    if ("fluxnet2015" %in% datasource){
      ##------------------------------------------------------------
      ## Get FLUXNET 2015 data
      ##------------------------------------------------------------
      ## Make sure data is available for this site
      error <- check_download_fluxnet2015( settings_calib$path_fluxnet2015, mysitename )
      
      ddf <- get_obs_bysite_fluxnet2015(
        mysitename, 
        path_fluxnet2015 = settings_input$path_fluxnet2015, 
        timescale = settings_calib$timescale$latenth,
        getvars = c("LE_F_MDS", "LE_RANDUNC", "LE_F_MDS_QC"), 
        getswc = FALSE,
        threshold_LE = 0.6, 
        remove_neg = FALSE,
        verbose = TRUE
        ) %>% 
        dplyr::right_join( ddf, by = "date" ) %>% 
        dplyr::rename(latenth_obs = latenth)
      
    } else {
      
      ddf <- ddf %>% dplyr::mutate( latenth_obs = NA, latenth_unc = NA )
      
    }
    
  }
  
  if ("wcont" %in% settings_calib$targetvars){

    if ("fluxnet2015" %in% settings_calib$datasource$wcont){

      ## Make sure data is available for this site
      error <- check_download_fluxnet2015( settings_calib$path_fluxnet2015, mysitename )

      ddf <- get_obs_bysite_wcont_fluxnet2015( mysitename, settings_calib$path_fluxnet2015, settings_calib$timescale ) %>%
             dplyr::right_join( ddf, by = "date" )

    }

  }

  return(ddf)

}

