calib_sofun <- function( setup, settings_calib, settings_sims, overwrite=FALSE ){

  require(readr)
  require(rlang)
  
  ##----------------------------------------------------------------
  ## Collect observational data used as calibration target
  ##----------------------------------------------------------------
  if (file.exists("ddf_obs.Rdata")&!overwrite){
    load("ddf_obs.Rdata")
  } else {
    print("Collecting observational target data ...")
    ddf_obs <- get_obs( settings_calib, settings_sims )
    save( ddf_obs, file = "ddf_obs.Rdata" )
  }

  ##----------------------------------------------------------------
  ## Apply filters
  ##----------------------------------------------------------------
  ## "filtering" pre-MODIS data
  ddf_obs <- ddf_obs %>% mutate( gpp_obs = ifelse( date < "2000-02-18", NA, gpp_obs ) )
  
  ## "filtering" by minimum temperature
  if (!is.na(settings_calib$filter_temp_min)) ddf_obs <- ddf_obs %>% mutate( gpp_obs = ifelse( temp < settings_calib$filter_temp_min, NA, gpp_obs ) )

  ## "filtering" by maximum temperature
  if (!is.na(settings_calib$filter_temp_max)) ddf_obs <- ddf_obs %>% mutate( gpp_obs = ifelse( temp > settings_calib$filter_temp_max, NA, gpp_obs ) )
          
  ## "filtering" by low soil moisture
  if (!is.na(settings_calib$filter_soilm_min)) ddf_obs <- ddf_obs %>% mutate( gpp_obs = ifelse( soilm_obs_mean < settings_calib$filter_soilm_min, NA, gpp_obs ) )
  
  ## "filtering" by whether it's a drought based on Stocker et al. (2018) analysis
  if (settings_calib$filter_drought){
    ## For calibration, use only non-drought data from sites where the flux/RS-based drought detection 
    ## worked well (see Stocker et al., 2018 New Phytologist). This is based on the fLUE data available
    ## through Zenodo (https://zenodo.org/record/1158524#.W2fz2dgzbUI).
    ddf_obs <-  read_csv( "~/data/flue/flue_stocker18nphyt.csv" ) %>%
      dplyr::filter( !is.na(cluster) ) %>%
      dplyr::select( site, date, is_flue_drought ) %>%
      dplyr::rename( sitename = site ) %>%
      right_join( ddf_obs, by = c( "sitename", "date" ) ) %>%
      mutate( gpp_obs = ifelse( is_flue_drought, NA, gpp_obs ) )
    
  }

  ## make global 
  obs <<- ddf_obs %>% dplyr::select( date, sitename, one_of( paste0( settings_calib$targetvars, "_obs") ) )

  ##----------------------------------------------------------------
  ## Set up for calibration ensemble
  ##----------------------------------------------------------------
  ## Write total number of simulation years in this ensemble to file (needed in calibration setup)
  filn_totrunyears <- paste0( settings_sims$path_input, "run/totrunyears_calib.txt" )
  zz <- file( filn_totrunyears, "w")
  tmp <- purrr::map( unlist(settings_sims$nyears)[names(unlist(settings_sims$nyears)) %in% settings_calib$sitenames] %>% sum(), ~cat( ., "\n", file=zz ) )
  close(zz)

  ## Write total number of calibration targets to file
  filn_nvars_calib <- paste0( settings_sims$path_input, "run/nvars_calib.txt" )
  zz <- file( filn_nvars_calib, "w")
  tmp <- cat( length( settings_calib$targetvars ), "\n", file=zz )
  close(zz)      

  ## Write run names (site names) for calibration
  settings_calib$path_runnames <- paste0( settings_sims$path_input, "run/runnames_calib_", settings_sims$name, ".txt" )
  zz <- file( settings_calib$path_runnames, "w")
  tmp <- purrr::map( as.list(settings_calib$sitenames), ~cat( ., "\n", file=zz ) )
  close(zz)

  ## global variables (used inside cost function)
  model    <<- paste0(setup$model, "_simsuite")
  simsuite <<- settings_sims$name

  ## save current working directory
  here <- getwd()
  setwd( settings_sims$dir_sofun )

  ## example run to get output file structure
  param_init <- unlist( lapply( settings_calib$par, function(x) x$init ) ) %>% unname()
  outfilnam <<- paste0( settings_sims$dir_sofun, "output_calib/calibtargets_tmp_fluxnet2015.txt" )
  system( paste0("rm ", outfilnam))
  system( paste0("make ", model) )

  ## For calibrating quantum yield efficiency only
  out <- system( paste0("echo ", simsuite, " ", sprintf( "%f %f %f %f", param_init[1], -9999, 1.0, 0.0 ), " | ./run", model ), intern = TRUE )  ## single calibration parameter

  # ## For calibration temperature ramp parameters
  # out <- system( paste0("echo ", simsuite, " ", sprintf( "%f %f %f %f", param_init[1], param_init[2], 1.0, 0.0 ), " | ./run", model ), intern = TRUE )  ## holding kphio fixed at previously optimised value for splined FPAR
  
  # ## For calibrating soil moisture stress parameters
  # out <- system( paste0("echo ", simsuite, " ", sprintf( "%f %f %f %f", param_init[1], -9999, param_init[2], param_init[3] ), " | ./run", model ), intern = TRUE )  ## holding kphio fixed at previously optimised value for splined FPAR
  
  # col_positions <<- fwf_empty( outfilnam, skip = 0, col_names = paste0( settings_calib$targetvars, "_mod" ), comment = "" ) ## this caused a mean bug, 
  col_positions <<- list( begin = 4, end = 15, skip = 0, col_names = paste0( settings_calib$targetvars, "_mod" ) ) ## this is how bug is fixed
  mod <- read_fwf( outfilnam, col_positions, col_types = cols( col_double() ) )

  if (nrow(mod)!=nrow(obs)) abort("Set overwrite to TRUE to re-read observattional data for calibration.")

  ##----------------------------------------------------------------
  ## Do the calibration
  ##---------------------------------------------------------------- 
  if (settings_calib$method=="gensa"){
    ##----------------------------------------------------------------
    ## calibrate the model parameters using GenSA (simulated annealing)
    ##----------------------------------------------------------------
    require(GenSA)
    ptm <- proc.time()
    out_optim <- GenSA(
                        par   = lapply( settings_calib$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
                        fn    = cost_rmse,
                        lower = lapply( settings_calib$par, function(x) x$lower ) %>% unlist(),
                        upper = lapply( settings_calib$par, function(x) x$upper ) %>% unlist(),
                        control=list( temperature=4000, max.call=settings_calib$maxit )
                      )
    proc.time() - ptm
    print(out_optim$par)

    filn <- paste0( here, "/out_gensa_", settings_calib$name, ".Rdat")
    print( paste0( "writing output from GenSA function to ", filn ) )
    save( out_optim, file = filn )
    
  } else if (settings_calib$method=="optimr"){
    ##----------------------------------------------------------------
    ## calibrate the model parameters using R optimr()
    ##----------------------------------------------------------------
    require(optimr)
    ptm <- proc.time()
    out_optim <- optimr(
                        par     = lapply( settings_calib$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
                        fn      = cost_rmse,
                        control = list( maxit = settings_calib$maxit )
                        )
    proc.time() - ptm
    print(out_optim$par)

    filn <- paste0( here, "/out_optimr_", settings_calib$name, ".Rdat")
    print( paste0( "writing output from optimr function to ", filn ) )
    save( out_optim, file = filn )

  } else if (settings_calib$method=="BayesianTools"){
    ##----------------------------------------------------------------
    ## calibrate the model parameters using BayesianTools and 
    ##----------------------------------------------------------------
    require(BayesianTools)

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
    ## calibrate the quantum yield efficiency parameter ('kphio') 
    ## applied as a linear scalar to simulated GPP.
    ##----------------------------------------------------------------
    ## Combine obs and mod by columns and make global
    modobs <<- bind_cols( obs, mod ) %>% 
               mutate( gpp_mod = gpp_mod / param_init )

    require(optimr)
    ptm <- proc.time()
    out_optim <- optimr(
                        par     = lapply( settings_calib$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
                        fn      = cost_linscale_rmse,
                        control = list( maxit = settings_calib$maxit )
                        )
    proc.time() - ptm
    print(out_optim$par)

    filn <- paste0( here, "/out_linscale_", settings_calib$name, ".Rdat")
    print( paste0( "writing output from linscale function to ", filn ) )
    save( out_optim, file = filn )

  }

  ## save optimised parameters
  settings_calib$par$kphio$opt          <- out_optim$par[1]
  # settings_calib$par$temp_ramp_edge$opt <- out_optim$par[2] 
  settings_calib$par$soilm_par_a$opt    <- out_optim$par[2] 
  settings_calib$par$soilm_par_b$opt    <- out_optim$par[3] 
  
  setwd( here )

  ## Write calibrated parameters into a CSV file
  vec <- unlist( unname( lapply( settings_calib$par, function(x) x$opt  )) )
  # df <- as_tibble(vec) %>% setNames( names(vec) )
  df <- as_tibble(t(vec))
  filn <- paste0("params_opt_", settings_calib$name,".csv")
  print( paste0( "writing calibrated parameters to ", filn ) )
  write_csv( df, path = filn )
  
  return( settings_calib )

}

##------------------------------------------------------------
## Generic cost function of model-observation (mis-)match using
## root mean square error.
##------------------------------------------------------------
cost_rmse <- function( par, inverse = FALSE ){

  ## execute model for this parameter set
  ## For calibrating quantum yield efficiency only
  out <- system( paste0("echo ", simsuite, " ", sprintf( "%f %f %f %f", par[1], -9999, 1.0, 0.0 ), " | ./run", model ), intern = TRUE )  ## single calibration parameter

  # ## For calibration temperature ramp parameters
  # out <- system( paste0("echo ", simsuite, " ", sprintf( "%f %f %f %f", par[1], par[2], 1.0, 0.0 ), " | ./run", model ), intern = TRUE )  ## holding kphio fixed at previously optimised value for splined FPAR
  
  # ## For calibrating soil moisture stress parameters
  # out <- system( paste0("echo ", simsuite, " ", sprintf( "%f %f %f %f", par[1], -9999, par[2], par[3] ), " | ./run", model ), intern = TRUE )  ## holding kphio fixed at previously optimised value for splined FPAR
  
  ## read output from calibration run
  out <- read_fwf( outfilnam, col_positions, col_types = cols( col_double() ) )
  
  ## Combine obs and mod by columns
  out <- bind_cols( obs, out )
  
  ## Calculate cost (RMSE)
  cost <- sqrt( mean( (out$gpp_mod - out$gpp_obs )^2, na.rm = TRUE ) )
  
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

##------------------------------------------------------------
## Gets data by looping over sites
##------------------------------------------------------------
get_obs <- function( settings_calib, settings_sims ){

  require(readr)
  require(dplyr)

  ##------------------------------------------------------------
  ## Read raw observational data from files.
  ## This creates a data frame (tibble) that contains
  ## a column 'date' and columns for each target variable. The number of rows
  ## corresponds to each simulation's length (number of days).
  ##------------------------------------------------------------
  ## loop over sites to get data frame with all variables
  list_bysite <- lapply( settings_calib$sitenames, function(x) get_obs_bysite(x,
                                                      settings_calib = settings_calib, 
                                                      settings_sims  = settings_sims
                                                      ) %>% mutate( sitename=x ) )
                
  ## combine dataframes from multiple sites along rows
  ddf_obs <- bind_rows( list_bysite )
  rm("list_bysite")

  return( ddf_obs )

}


##------------------------------------------------------------
## Function returns a data frame (tibble) containing all the observational data
## used as target variables for calibration for a given site, covering specified dates.
##------------------------------------------------------------
get_obs_bysite <- function( sitename, settings_calib, settings_sims ){

  require(dplyr)

  source("init_dates_dataframe.R")
  source("get_obs_bysite_gpp_fluxnet2015.R")
  source("get_obs_bysite_gpp_gepisat.R")
  source("check_download_fluxnet2015.R")
  source("check_download_gepisat.R")

  ## Initialise daily dataframe (WITHOUT LEAP YEARS, SOFUN USES FIXED 365-DAYS YEARS!)
  ddf <- init_dates_dataframe( year(settings_sims$date_start[[sitename]]), year(settings_sims$date_end[[sitename]]), noleap = TRUE )

  if ("gpp" %in% settings_calib$targetvars){

    if ("fluxnet2015" %in% settings_calib$datasource$gpp){

      ## Make sure data is available for this site
      error <- check_download_fluxnet2015( settings_calib$path_fluxnet2015, sitename )

      ## This gets gpp_obs as mean of GPP_NT_VUT_REF and GPP_DT_VUT_REF
      ddf <-  get_obs_bysite_gpp_fluxnet2015( sitename, settings_calib$path_fluxnet2015, settings_calib$timescale ) %>%
              rename( gpp_obs_fluxnet2015 = gpp_obs ) %>%
              right_join( ddf, by = "date" )

    }

    if ("gepisat" %in% settings_calib$datasource$gpp){

      ## Make sure data is available for this site
      error <- check_download_gepisat( settings_calib$path_gepisat, sitename )

      tmp <-  get_obs_bysite_gpp_gepisat( sitename, settings_calib$path_gepisat, settings_calib$timescale )
      
      if (!is.null(tmp)){
        ddf <- tmp %>%
          rename( gpp_obs_gepisat = gpp_obs ) %>%
          right_join( ddf, by = "date" )
      } else {
        ddf <- ddf %>% mutate( gpp_obs_gepisat = NA )
      }

    }

    if ("fluxnet2015" %in% settings_calib$datasource$gpp && "gepisat" %in% settings_calib$datasource$gpp){
      
      ## For now, take mean across GPP_NT_VUT_REF, GPP_DT_VUT_REF, and GePiSaT-GPP
      ddf$gpp_obs <-  apply( dplyr::select( ddf, gpp_obs_fluxnet2015, gpp_obs_gepisat ), 1, FUN = weighted.mean, c(2,1), na.rm=TRUE ) 
      ddf <- ddf %>% mutate( gpp_obs = ifelse( is.nan(gpp_obs), NA, gpp_obs ) )
    
    } else if ("fluxnet2015" %in% settings_calib$datasource$gpp){
    
      ## For now, take mean across GPP_NT_VUT_REF, and GPP_DT_VUT_REF
      ddf <- ddf %>% mutate( gpp_obs = gpp_obs_fluxnet2015 )

    }

  }

  if ("wcont" %in% settings_calib$targetvars){

    if ("fluxnet2015" %in% settings_calib$datasource$wcont){

      ## Make sure data is available for this site
      error <- check_download_fluxnet2015( settings_calib$path_fluxnet2015, sitename )

      ddf <- get_obs_bysite_wcont_fluxnet2015( sitename, settings_calib$path_fluxnet2015, settings_calib$timescale ) %>%
             right_join( ddf, by = "date" )

    }

  }

  return(ddf)

}

