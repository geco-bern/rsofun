calib_sofun <- function( setup, settings_calib, settings_sims ){

  ##----------------------------------------------------------------
  ## Collect observational data used as calibration target
  ##----------------------------------------------------------------
  ddf_obs <- get_obs( settings_calib, settings_sims )

  ## subset and make global 
  obs <<- ddf_obs %>%
    ## "filter" data, i.e. replace by NA if above/below a certaint temperature or soil moisture threshold
    mutate( gpp_obs = ifelse( temp < settings_calib$filter_temp_min, NA, gpp_obs ) ) %>% # "filtering" by minimum temperature
    mutate( gpp_obs = ifelse( temp > settings_calib$filter_temp_max, NA, gpp_obs ) ) %>% # "filtering" by maximum temperature
    mutate( gpp_obs = ifelse( soilm_obs_mean < settings_calib$filter_soilm_min, NA, gpp_obs ) ) %>% # "filtering" by minimum soil moisture
    select( date, sitename, one_of( paste0( settings_calib$targetvars, "_obs") ) )

  ##----------------------------------------------------------------
  ## Set up for calibration ensemble
  ##----------------------------------------------------------------
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
  param_init <- unlist( lapply( settings_calib$par, function(x) x$init ) )
  system( paste0("make ", model) )
  out <- system( paste0("echo ", simsuite, " ", sprintf( "%f", param_init ), " | ./run", model ), intern = TRUE )  
  outfilnam <<- paste0( settings_sims$dir_sofun, "output_calib/calibtargets_tmp_fluxnet2015.txt" )
  col_positions <<- fwf_empty( outfilnam, skip = 0, col_names = paste0( settings_calib$targetvars, "_mod" ), comment = "" )
  mod <- read_fwf( outfilnam, col_positions )
  
  ## xxx try one run with initial parameters using the cost function
  # cost <- cost_rmse( lapply( settings_calib$par, function(x) x$init ) %>% unlist() )

  ##----------------------------------------------------------------
  ## Do the calibration
  ##---------------------------------------------------------------- 
  if (settings_calib$method=="gensa"){
    ##----------------------------------------------------------------
    ## calibrate the model parameters using GenSA (simulated annealing)
    ##----------------------------------------------------------------
    require(GenSA)
    ptm <- proc.time()
    optim_par_gensa = GenSA(
        par   = lapply( settings_calib$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
        fn    = cost_rmse,
        lower = lapply( settings_calib$par, function(x) x$lower ) %>% unlist(),
        upper = lapply( settings_calib$par, function(x) x$upper ) %>% unlist(),
        control=list( temperature=4000, max.call=settings_calib$maxit )
      )
    proc.time() - ptm
    print(optim_par_gensa$par)

    filn <- paste0("out_gensa_", settings_calib$name, ".Rdat")
    print( paste0( "writing output from GenSA function to ", filn ) )
    save( optim_par_gensa, file = filn )

    ## save optimised parameters
    opt <- optim_par_gensa$par[1]
    settings_calib$par$kphio$opt <- opt

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
    optim_par_bayesiantools <- runMCMC( bayesianSetup = bt_setup, sampler = "DEzs", settings = bt_settings )
    proc.time() - ptm

  }

  setwd( here )

  ## Write calibrated parameters into a CSV file
  vec <- unlist( lapply( settings_calib$par, function(x) x$opt ) )
  df <- as_tibble(vec) %>% setNames( names(vec) )
  filn <- paste0("params_opt_", settings_calib$name,".csv")
  print( paste0( "writing calibrated parameters to ", filn ) )
  write_csv( df, path = filn )
  
  return( settings_calib )

}

##------------------------------------------------------------
## Generic cost function of model-observation (mis-)match
##------------------------------------------------------------
cost_rmse <- function( par, inverse = FALSE ){

  ## execute model for this parameter set
  out <- system( paste0("echo ", simsuite, " ", sprintf( "%f", par[1] ), " | ./run", model ), intern = TRUE )

  ## read output from calibration run
  out <- read_fwf( outfilnam, col_positions )
  
  ## Combine obs and mod by columns
  out <- bind_cols( obs, out )
  
  ## Calculate cost (RMSE)
  cost <- sqrt( mean( (out$gpp_mod - out$gpp_obs )^2, na.rm = TRUE ) )
  
  if (inverse) cost <- 1.0 / cost

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

  ## Initialise daily dataframe (WITHOUT LEAP YEARS, SOFUN USES FIXED 365-DAYS YEARS!)
  ddf <- init_dates_dataframe( year(settings_sims$date_start[[sitename]]), year(settings_sims$date_end[[sitename]]), noleap = TRUE )

  if ("gpp" %in% settings_calib$targetvars){

    if (settings_calib$datasource$gpp=="fluxnet2015"){

      ## Make sure data is available for this site
      error <- check_download_fluxnet2015( settings_input, settings_sims, sitename )

      ddf <- get_obs_bysite_gpp_fluxnet2015( sitename, settings_calib$path_fluxnet2015, settings_calib$timescale ) %>%
             right_join( ddf, by = "date" )

    }

  }

  if ("wcont" %in% settings_calib$targetvars){

    if (settings_calib$datasource$wcont=="fluxnet2015"){

      ## Make sure data is available for this site
      error <- check_download_fluxnet2015( settings_input, settings_sims, sitename )

      ddf <- get_obs_bysite_wcont_fluxnet2015( sitename, settings_calib$path_fluxnet2015, settings_calib$timescale ) %>%
             right_join( ddf, by = "date" )

    }

  }

  return(ddf)

}

