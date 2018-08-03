calib_sofun <- function( setup, settings_calib, settings_sims, overwrite=FALSE ){

  ##----------------------------------------------------------------
  ## Collect observational data used as calibration target
  ##----------------------------------------------------------------
  if (file.exists("ddf_obs.Rdata")&!overwrite){
    load("ddf_obs.Rdata")
  } else {
    print("Collecting observational target data takes long ...")
    ddf_obs <- get_obs( settings_calib, settings_sims )
    save( ddf_obs, file = "ddf_obs.Rdata" )
  }

  # ## test plot
  # test <- filter(ddf_obs, sitename=="FR-Pue" & year(date) %in% c(2003) )
  # test$min <- apply( select( test, GPP_NT_VUT_REF, GPP_DT_VUT_REF, gpp_obs_gepisat ), 1, FUN = min, na.rm = TRUE )
  # test$max <- apply( select( test, GPP_NT_VUT_REF, GPP_DT_VUT_REF, gpp_obs_gepisat ), 1, FUN = max, na.rm = TRUE )
  # with( test, plot( date, gpp_obs, type="l" ) )
  # with( test, polygon( c(date, rev(date)), c( min, rev(max)), border = NA, col = rgb(0,0,0,0.3) ) )

  ## subset and make global 
  obs <<- ddf_obs %>%
    ## "filter" data, i.e. replace by NA if above/below a certaint temperature or soil moisture threshold
    mutate( gpp_obs = ifelse( temp < settings_calib$filter_temp_min, NA, gpp_obs ) ) %>% # "filtering" by minimum temperature
    mutate( gpp_obs = ifelse( temp > settings_calib$filter_temp_max, NA, gpp_obs ) ) %>% # "filtering" by maximum temperature
    mutate( gpp_obs = ifelse( soilm_obs_mean < settings_calib$filter_soilm_min, NA, gpp_obs ) ) %>% # "filtering" by minimum soil moisture
    dplyr::select( date, sitename, one_of( paste0( settings_calib$targetvars, "_obs") ) )

  print("...done.")  

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

  } else if (settings_calib$method=="optimr"){
    ##----------------------------------------------------------------
    ## calibrate the model parameters using R optimr()
    ##----------------------------------------------------------------
    require(optimr)
    ptm <- proc.time()
    optim_par_optimr = optimr(
        par   = lapply( settings_calib$par, function(x) x$init ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
        fn    = cost_rmse,
        control = list( maxit = settings_calib$maxit )
        )
    proc.time() - ptm
    print(optim_par_optimr$par)

    filn <- paste0("out_optimr_", settings_calib$name, ".Rdat")
    print( paste0( "writing output from optimr function to ", filn ) )
    save( optim_par_optimr, file = filn )

    ## save optimised parameters
    opt <- optim_par_optimr$par[1]
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

    ## For now, take mean across GPP_NT_VUT_REF, GPP_DT_VUT_REF, and GePiSaT-GPP
    # ddf$gpp_obs <- apply( dplyr::select( ddf, GPP_NT_VUT_REF, GPP_DT_VUT_REF, gpp_obs_gepisat ), 1, FUN = mean, na.rm=TRUE )
    ddf$gpp_obs <-  apply( dplyr::select( ddf, gpp_obs_fluxnet2015, gpp_obs_gepisat ), 1, FUN = weighted.mean, c(2,1), na.rm=TRUE ) 
    ddf <- ddf %>% mutate( gpp_obs = ifelse( is.nan(gpp_obs), NA, gpp_obs ) )

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

