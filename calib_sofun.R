calib_sofun <- function( setup, settings_calib, settings_sims ){

  ##----------------------------------------------------------------
  ## Collect observational data used as calibration target
  ##----------------------------------------------------------------
  ddf_obs <- get_obs( settings_calib, settings_sims )

  ## subset and make global 
  ## XXX todo: filter by low temperature and soil moisture
  obs <<- ddf_obs %>% select( date, sitename, one_of( paste0( settings_calib$targetvars, "_obs") ) )

  ##----------------------------------------------------------------
  ## Set up for calibration ensemble
  ##----------------------------------------------------------------
  ## Write run names (site names) for calibration
  settings_calib$path_runnames <- paste0( settings$path_input, "run/runnames_calib_", settings$name, ".txt" )
  zz <- file( settings_calib$path_runnames, "w")
  tmp <- purrr::map( as.list(settings_calib$sitenames), ~cat( ., "\n", file=zz ) )
  close(zz)

  ## global variables (used inside cost function)
  model    <<- setup$model
  simsuite <<- settings_sims$name

  ## save current working directory
  here <- getwd()
  setwd( settings$dir_sofun )

  ## example run to get output file structure
  out <- system( paste0("echo ", simsuite, " ", sprintf( "%f", 1.0 ), " | ./run", model, "_simsuite"), intern = TRUE )  
  outfilnam <<- paste0( settings$dir_sofun, "output_calib/calibtargets_tmp_fluxnet2015.txt" )
  col_positions <<- fwf_empty( outfilnam, skip = 0, col_names = paste0( settings_calib$targetvars, "_mod" ), comment = "" )
  mod <- read_fwf( outfilnam, col_positions )
  
  ## xxx try
  # cost <- cost_rmse(0.05)

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
        par   = lapply( settings_calib$par, function(x) x$best ) %>% unlist(),  # initial parameter value, if NULL will be generated automatically
        fn    = cost_rmse,
        lower = lapply( settings_calib$par, function(x) x$lower ) %>% unlist(),
        upper = lapply( settings_calib$par, function(x) x$upper ) %>% unlist(),
        control=list( temperature=4000, max.call=settings_calib$maxit )
      )
    proc.time() - ptm
    print(optim_par_gensa$par)

  }

  setwd( here )
  return( optim_par_gensa$par )

}

##------------------------------------------------------------
## Generic cost function of model-observation (mis-)match
##------------------------------------------------------------
cost_rmse <- function( par ){

  ## execute model for this parameter set
  out <- system( paste0("echo ", simsuite, " ", sprintf( "%f", par ), " | ./run", model, "_simsuite"), intern = TRUE )

  ## read output from calibration run
  out <- read_fwf( outfilnam, col_positions )
  
  ## Combine obs and mod by columns
  out <- bind_cols( obs, out )
  
  ## Calculate cost (RMSE)
  cost <- sqrt( mean( (out$gpp_mod - out$gpp_obs )^2, na.rm = TRUE ) )
  
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

  return( ddf_obs )

}


##------------------------------------------------------------
## Function returns a data frame (tibble) containing all the observational data
## used as target variables for calibration for a given site, covering specified dates.
##------------------------------------------------------------
get_obs_bysite <- function( sitename, settings_calib, settings_sims ){

  require(plyr)
  require(rlang)
  source("init_dates_dataframe.R")

  ## Initialise daily dataframe (WITHOUT LEAP YEARS, SOFUN USES FIXED 365-DAYS YEARS!)
  ddf <- init_dates_dataframe( year(settings_sims$date_start[[sitename]]), year(settings_sims$date_end[[sitename]]), noleap = TRUE )

  if ("gpp" %in% settings_calib$targetvars){

    if (settings_calib$datasource$gpp=="fluxnet2015"){

      ddf <- get_obs_bysite_gpp_fluxnet2015( sitename, settings_calib, settings_sims ) %>%
             right_join( ddf, by = "date" )

    }

  }

  if ("wcont" %in% settings_calib$targetvars){

    if (settings_calib$datasource$wcont=="fluxnet2015"){

      ddf <- get_obs_bysite_wcont_fluxnet2015( sitename, settings_calib, settings_sims ) %>%
             right_join( ddf, by = "date" )

    }

  }

  return(ddf)

}

##----------------------------------------------------------------------
## Function for reading observational GPP data from FLUXNET 2015 dataset
## and defining calibration target (which flux decomposition method etc.)
##----------------------------------------------------------------------
get_obs_bysite_gpp_fluxnet2015 <- function( sitename, settings_calib, settings_sims ){

  # source("clean_fluxnet.R")

  ## Get GPP data from FLUXNET 2015 dataset
  getvars <- c( "GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "LE_F_MDS", "LE_F_MDS_QC", "NEE_VUT_REF_NIGHT_QC", "NEE_VUT_REF_DAY_QC" )

  ## Make sure data is available for this site
  error <- check_download_fluxnet2015( settings_input, settings_sims, sitename )

  ## Take only file for this site
  if (settings_calib$timescale=="d"){
    ## Daily
    filn <- list.files( settings_calib$path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_DD.*.csv" ), 
      recursive = TRUE 
      )
  } else  if (settings_calib$timescale=="w"){
    ## Weekly
    filn <- list.files( settings_calib$path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_WW.*.csv" ), 
      recursive = TRUE 
      )
  } else  if (settings_calib$timescale=="m"){
    ## Monthly
    filn <- list.files( settings_calib$path_fluxnet2015, 
      pattern = paste0(..., collapse = NULL), 
      recursive = TRUE 
      )
  } else  if (settings_calib$timescale=="y"){
    ## Annual
    filn <- list.files( settings_calib$path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_YY.*.csv" ), 
      recursive = TRUE 
      )
  }

  if (length(filn)==0) abort(paste0("No files found for timescale ", settings_calib$timescale, "in sub-directories of ", settings_calib$path_fluxnet2015 ) )
  if (length(filn)>1)  abort(paste0("Multiple files found for timsescale", settings_calib$timescale, "in sub-directories of ", settings_calib$path_fluxnet2015))
  
  ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
  ddf <- get_obs_fluxnet2015_raw( sitename, 
    path = paste0(settings_calib$path_fluxnet2015, filn), 
    getvars = getvars, 
    freq = "d" 
    )

  ## Convert units
  ddf <- ddf %>% 
    ## given in umolCO2 m-2 s-1. converted to gC m-2 d-1
    mutate_at( vars(starts_with("GPP_")), funs(convert_gpp_fluxnet2015) ) %>%

    ## W m-2 -> J m-2 d-1
    mutate_at( vars(starts_with("LE_")), funs(convert_le_fluxnet2015))


  ## clean data
  if (any( !(c("GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "NEE_VUT_REF_NIGHT_QC", "NEE_VUT_REF_DAY_QC") %in% getvars) )) abort("Not all variables read from file that are needed for data cleaning.")
  out_clean <- clean_fluxnet_gpp( ddf$GPP_NT_VUT_REF, ddf$GPP_DT_VUT_REF, ddf$NEE_VUT_REF_NIGHT_QC, ddf$NEE_VUT_REF_DAY_QC, cutoff=0.5 )
  ddf$GPP_NT_VUT_REF <- out_clean$gpp_nt
  ddf$GPP_DT_VUT_REF <- out_clean$gpp_dt

  if (any( !(c("LE_F_MDS", "LE_F_MDS_QC") %in% getvars) )) abort("Not all variables read from file that are needed for data cleaning.")
  # ddf$LE_F_MDS_good <- clean_fluxnet_et( ddf$LE_F_MDS, ddf$LE_F_MDS_QC, cutoff=0.5 )
  ddf$LE_F_MDS      <- clean_fluxnet_et( ddf$LE_F_MDS, ddf$LE_F_MDS_QC, cutoff=0.2 )

  ## define which data is to be used as target for calibration 'gpp_obs' and 'transp_obs'
  ddf <- ddf %>% mutate( gpp_obs = (GPP_NT_VUT_REF + GPP_DT_VUT_REF)/2.0,
                         transp_obs = LE_F_MDS ) %>%
                 select( date, gpp_obs, transp_obs )

  return(ddf)

}

##----------------------------------------------------------------------
## Function for reading observational GPP data from FLUXNET 2015 dataset
##----------------------------------------------------------------------
get_obs_bysite_wcont_fluxnet2015 <- function( sitename, settings_calib, settings_sims ){

  # source("clean_fluxnet.R")

  getvars <- "SWC"

  ## Make sure data is available for this site
  error <- check_download_fluxnet2015( settings_input, settings_sims, sitename )

  ## Take only file for this site
  if (settings_calib$timescale=="d"){
    ## Daily
    filn <- list.files( settings_calib$path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_DD.*.csv" ), 
      recursive = TRUE 
      )
  } else  if (settings_calib$timescale=="w"){
    ## Weekly
    filn <- list.files( settings_calib$path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_WW.*.csv" ), 
      recursive = TRUE 
      )
  } else  if (settings_calib$timescale=="m"){
    ## Monthly
    filn <- list.files( settings_calib$path_fluxnet2015, 
      pattern = paste0(..., collapse = NULL), 
      recursive = TRUE 
      )
  } else  if (settings_calib$timescale=="y"){
    ## Annual
    filn <- list.files( settings_calib$path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_YY.*.csv" ), 
      recursive = TRUE 
      )
  }

  if (length(filn)==0) abort(paste0("No files found for timescale ", settings_calib$timescale, "in sub-directories of ", settings_calib$path_fluxnet2015 ) )

  ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
  ddf <- get_obs_fluxnet2015_raw( sitename, 
    path = paste0(settings_calib$path_fluxnet2015, filn),
    vars = getvars, 
    freq = "d" 
    )

  swcvars   <- select( vars(ddf), starts_with("SWC") ) %>% select( vars(ddf), !ends_with("QC") ) %>% names()
  swcqcvars <- select( vars(ddf), starts_with("SWC") ) %>% select( vars(ddf),  ends_with("QC") ) %>% names()

  # map( as.list(seq(length(swcvars))), ~clean_fluxnet_swc( ddf[[ swcvars[.] ]], ddf[[ swcqcvars[.] ]]) )

  if (length(swcvars)>0){
    for (ivar in 1:length(swcvars)){
      ddf[[ swcvars[ivar] ]] <- clean_fluxnet_swc( ddf[[ swcvars[ivar] ]], ddf[[ swcqcvars[ivar] ]] )
    }
  }
  
  return(ddf)

}   


get_obs_fluxnet2015_raw <- function( sitename, path, getvars, freq="d" ){
  ##--------------------------------------------------------------------
  ## Function returns a dataframe containing all the data of flux-derived
  ## GPP for the station implicitly given by path (argument).
  ## Specific for FLUXNET 2015 data
  ## Returns data in units given in the fluxnet 2015 dataset
  ##--------------------------------------------------------------------
  require(dplyr)
  require(readr)
  require(lubridate)

  ## get data
  df <-  read_csv( path, na="-9999", col_types = cols() )

  ## get dates, their format differs slightly between temporal resolution
  if ( freq=="y" ){

    df <- df %>% mutate( year = TIMESTAMP ) %>% mutate( date = ymd( paste0( as.character(year), "-01-01" ) ) )      

  } else if ( freq=="w"){

    df <- df %>% mutate( date_start = ymd(TIMESTAMP_START), date_end = ymd(TIMESTAMP_END) ) %>% 
                 mutate( date = date_start )

  } else if ( freq=="m" ){

    df <- df %>% mutate( year = substr( TIMESTAMP, start = 1, stop = 4 ), month = substr( TIMESTAMP, start = 5, stop = 6 ) ) %>%
                 mutate( date = ymd( paste0( as.character(year), "-", as.character(month), "-01" ) ) )

  } else if ( freq=="d" ){

    df <- df %>% mutate( date = ymd( TIMESTAMP ) )

  }

  ## convert to numeric (weirdly isn't always)
  if ( identical( getvars , "SWC" ) ){
    df <- df %>% mutate_at( vars(starts_with(getvars)), funs(as.numeric)) %>%
                 select( date, starts_with(getvars) )
  } else {
    df <- df %>%  mutate_at( vars(one_of(getvars)), funs(as.numeric)) %>%
                  select( date, one_of(getvars) )
  }

  return( df )

}

## Converts units of GPP variables from FLUXNET 2015 to SOFUN standard
convert_gpp_fluxnet2015 <- function( gpp ){
  # in FLUXNET 2015 given in umolCO2 m-2 s-1. converted to gC m-2 d-1
  c_molmass <- 12.0107  # molar mass of C
  gpp_coverted <- gpp * 1e-6 * 60 * 60 * 24 * c_molmass
  return(gpp_coverted)

}

## Converts units of latent energy (LE) variables from FLUXNET 2015 to SOFUN standard
convert_le_fluxnet2015 <- function( le ){
  ## W m-2 -> J m-2 d-1
  le_converted <- le * 60 * 60 * 24
  return(le_converted)

}

clean_fluxnet_gpp <- function( gpp_nt, gpp_dt, qflag_reichstein, qflag_lasslop, cutoff=0.80 ){
  ##--------------------------------------------------------------------
  ## Cleans daily data using criteria 1-4 as documented in Tramontana et al., 2016
  ## gpp_nt: based on nighttime flux decomposition ("NT")
  ## gpp_dt: based on daytime flux decomposition ("DT")
  ##--------------------------------------------------------------------

  ## Remove data points that are based on too much gap-filled data in the underlying half-hourly data
  gpp_nt[ which(qflag_reichstein < cutoff) ] <- NA  ## based on fraction of data based on gap-filled half-hourly
  gpp_dt[ which(qflag_lasslop    < cutoff) ] <- NA  ## based on fraction of data based on gap-filled half-hourly

  ## Remove data points where the two flux decompositions are inconsistent,
  ## i.e. where the residual of their regression is above the 97.5% or below the 2.5% quantile. 
  res  <- as.numeric(gpp_nt) - as.numeric(gpp_dt)
  q025 <- quantile( res, probs = 0.025, na.rm=TRUE )
  q975 <- quantile( res, probs = 0.975, na.rm=TRUE )

  gpp_nt[ res > q975 | res < q025  ] <- NA
  gpp_dt[ res > q975 | res < q025  ] <- NA

  ## remove negative GPP
  gpp_nt[ which(gpp_nt<0) ] <- NA
  gpp_dt[ which(gpp_dt<0) ] <- NA

  return( list( gpp_nt=gpp_nt, gpp_dt=gpp_dt ) )
}

clean_fluxnet_et <- function( et, qflag_et, cutoff=0.2 ){
  ##--------------------------------------------------------------------
  ##--------------------------------------------------------------------
  source( "identify_pattern.R" )

  ## Remove data points that are based on too much gap-filled data in the underlying half-hourly data
  # frac_data_thresh <- 0.2  ## fraction of data based on gap-filled half-hourly
  et[ qflag_et < cutoff ] <- NA

  if ( any(!is.na(qflag_et)) ){ et[ is.na(qflag_et) ] <- NA }

  et <- identify_pattern( et )

  return( et )
}

clean_fluxnet_swc <- function( swc, qflag_swc, frac_data_thresh=0.2 ){
  ##--------------------------------------------------------------------
  ## frac_data_thresh: fraction of data based on gap-filled half-hourly
  ##--------------------------------------------------------------------
  ## Remove data points that are based on too much gap-filled data in the underlying half-hourly data
  swc[ qflag_swc < frac_data_thresh ] <- NA
  swc <- as.numeric( swc )

  return( swc )
}


