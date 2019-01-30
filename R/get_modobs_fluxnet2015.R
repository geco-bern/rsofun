get_modobs_fluxnet2015 <- function( sitename, simsuite, outputset, list_modobs=NA, getvars=c( "gpp", "wcont", "aet", "pet" ), add_swcvars=TRUE, overwrite=overwrite, overwrite_dosites=overwrite_dosites, outdir="./" ){

  # ## XXX debug------------------------------------------
  # sitename = "AR-SLu"
  # simsuite = "fluxnet2015"
  # list_modobs = list()
  # outputset = c( "s15" )
  # getvars   = c( "gpp", "wcont", "aet", "pet" )
  # add_swcvars = TRUE
  # overwrite   = TRUE
  # overwrite_dosites = TRUE
  # outdir="./"
  # ##----------------------------------------------------

  avl2015 <- TRUE
  avl_mod <- rep( TRUE, length(outputset) )

  norm_to_max <- function( vec ){
    vec <- vec / quantile( vec, probs=0.97, na.rm = TRUE )
    return( vec )
  }

  ##//////////////////////////////////////////////////////////
  ## SOFUN output data for each output set -> ddf as list
  ##---------------------------------------------------------
  print( paste( "getting model output data ..." ) )
  site <- list()
  ddf  <- list()
  wdf  <- list()
  mdf  <- list()
  adf  <- list()

  kdx <- 0
  for (iset in outputset){
    kdx <- kdx + 1

    dirnam_mod <- paste( myhome, "sofun/output_nc_fluxnet2015_sofun/", iset, "/", sep="" )

    ddf_tmp <- get_daily_modelout( sitename, dirnam_mod, getvars )

    if (is.na(ddf_tmp)) { 
    
      avl_mod[kdx]          <- FALSE
      missing_mod[[ iset ]] <- c( missing_mod[[ iset ]], sitename )
    
    } else {

      ## For comparison with FLUXNET 2015 data, normalise simulated soil moisture to between 0 and 1
      print( "WARNING: Normalising simulated soil moisture to maximum (=1) for comparability with observational data.")
      ddf_tmp <- ddf_tmp %>% dplyr::mutate( wcont = wcont / max( wcont, na.rm = TRUE ) )  

      ## add to list
      ddf[[ iset ]] <- ddf_tmp

    }

    ##---------------------------------------------------------
    ## Aggregate to model output from daily to monthly
    ##---------------------------------------------------------
    mdf[[ iset ]] <- ddf[[ iset ]] %>%  dplyr::group_by( month(date) ) %>%
                                        dplyr::summarise( date = min( date ),   ## 'date' is now the first day of the month
                                                   pet = sum( pet ),
                                                   aet = sum( aet ),
                                                   wcont = mean( wcont ),
                                                   gpp = sum( gpp )
                                                  )

    ##---------------------------------------------------------
    ## Aggregate to model output from daily to weekly
    ##---------------------------------------------------------
    wdf[[ iset ]] <- ddf[[ iset ]] %>%  dplyr::group_by( week(date) ) %>%
                                        dplyr::summarise( date = min( date ),   ## 'date' is now the first day of the week
                                                   pet = sum( pet ),
                                                   aet = sum( aet ),
                                                   wcont = mean( wcont ),
                                                   gpp = sum( gpp )
      )
    
    ##---------------------------------------------------------
    ## Aggregate to model output from daily to annual
    ##---------------------------------------------------------
    adf[[ iset ]] <- ddf[[ iset ]] %>%  dplyr::group_by( year(date) ) %>%
                                        dplyr::summarise( date = min( date ),    ## 'date' is now the first day of the year
                                                   pet = sum( pet ),
                                                   aet = sum( aet ),
                                                   wcont = mean( wcont ),
                                                   gpp = sum( gpp )
                                                  )

  }


  ##//////////////////////////////////////////////////////////
  ## FLUX DATA FROM FLUXNET
  ##---------------------------------------------------------
  ## Get daily FLUXNET data
  ##---------------------------------------------------------
  print( paste( "getting daily FLUXNET 2015 data ..." ) )
  ddf_obs <- get_fluxdata_fluxnet2015_daily( sitename, add_swcvars=add_swcvars )
  
  if (any(!is.na(ddf_obs))){ 

    ## Normalise observational soil moisture to within minimum (=0) and maximum (=1), and
    ddf_obs$obs_swc <- ddf_obs$obs_swc %>% dplyr::mutate_at( vars(starts_with("SWC_F_MDS")), funs(norm_to_max(.)) )
    
    ## get mean soil observational moisture across different depths (if available)
    ddf_obs$obs_swc <- ddf_obs$obs_swc %>%
      dplyr::mutate( soilm_obs_mean = apply( dplyr::select( ddf_obs$obs_swc, starts_with("SWC_F_MDS") ), 1, FUN=mean, na.rm=TRUE ) ) %>%
      dplyr::mutate( soilm_obs_mean = ifelse( is.nan(soilm_obs_mean), NA, soilm_obs_mean ) )
  
  } else {

    print( paste( "error opening", path ) )
    avl2015 <- FALSE

  }

  ##---------------------------------------------------------
  ## Get weekly FLUXNET data
  ##---------------------------------------------------------
  print( paste( "getting weekly FLUXNET 2015 data ..." ) )
  wdf_obs <- get_fluxdata_fluxnet2015( sitename, freq="w" ) 
  
  ##---------------------------------------------------------
  ## Get monthly FLUXNET data
  ##---------------------------------------------------------
  print( paste( "getting monthly FLUXNET 2015 data ..." ) )
  mdf_obs <- get_fluxdata_fluxnet2015( sitename, freq="m" ) 
  
  ##---------------------------------------------------------
  ## Get annual FLUXNET data
  ##---------------------------------------------------------
  print( paste( "getting annual FLUXNET 2015 data ..." ) )
  adf_obs <- get_fluxdata_fluxnet2015( sitename, freq="y" ) 
  

  ##//////////////////////////////////////////////////////////
  ## METEO DATA FROM FLUXNET
  ##---------------------------------------------------------
  ## daily
  ddf_inclim <- get_meteo_fluxnet2015( sitename, freq="d" ) 

  ## weekly
  wdf_inclim <- get_meteo_fluxnet2015( sitename, freq="w" ) 

  ## monhtly
  mdf_inclim <- get_meteo_fluxnet2015( sitename, freq="m" ) 

  ## annual
  adf_inclim <- get_meteo_fluxnet2015( sitename, freq="y" ) 

  
  ##//////////////////////////////////////////////////////////
  ## FAPAR DATA FROM MODIS
  ##---------------------------------------------------------
  ## fAPAR
  print( paste( "getting fapar input data (MODIS FPAR) ..." ) )
  filn <- paste0( myhome, "sofun/input_fluxnet2015_sofun/sitedata/fapar/", sitename, "/dfapar_MODIS_FPAR_MCD15A3H_", sitename, "_gee_subset.csv" )
  ddf_infpar <- try( readr::read_csv( filn ) )
  if (class(ddf_infpar)=="try-error"){
    missing_infpar <- c( missing_infpar, sitename )    
  } else {
    ddf_infpar <- ddf_infpar %>% dplyr::rename( fpar = modisvar_interpol ) %>% dplyr::select( date, fpar ) %>% dplyr::mutate( date = ymd(date) )
  }

  ## monthly
  mdf_infpar <- ddf_infpar %>%  dplyr::group_by( month(date) ) %>%
                                dplyr::summarise( fpar = mean( fpar ), date = min(date) ) ## 'date' is now the first day of the month

  ## weekly
  wdf_infpar <- ddf_infpar %>%  dplyr::group_by( week(date) ) %>%
                                dplyr::summarise( fpar = mean( fpar ), date = min(date) ) ## 'date' is now the first day of the week
  
  ## annual
  adf_infpar <- ddf_infpar %>%  dplyr::group_by( year(date) ) %>%
                                dplyr::summarise( fpar = mean( fpar ), date = min(date) ) ## 'date' is now the first day of the year

  ##---------------------------------------------------------
  ## Check if any data is available
  ##---------------------------------------------------------
  if (length(ddf)==0){
    cont <- FALSE
    ddf  <- NA
  } else {
    cont <- TRUE
  }

  if (cont){
    if (avl2015){

      ## add daily data to list
      ddf$obs     <- ddf_obs$obs
      ddf$inp     <- ddf_inclim %>% dplyr::left_join( ddf_infpar, by = "date" )
      ddf$swc_obs <- ddf_obs$obs_swc

      ## add weekly data to list
      wdf$obs     <- wdf_obs
      wdf$inp     <- wdf_inclim %>% dplyr::left_join( wdf_infpar, by = "date" )

      ## add monthly data to list
      mdf$obs     <- mdf_obs
      mdf$inp     <- mdf_inclim %>% dplyr::left_join( mdf_infpar, by = "date" )

      ## add annual data to list
      adf$obs     <- adf_obs
      adf$inp     <- adf_inclim %>% dplyr::left_join( adf_infpar, by = "date" )

    } else {
    
      ddf$obs <- NA
      ddf$inp <- NA
      ddf$swc_obs <- NA

      wdf$obs <- NA
      wdf$inp <- NA
      wdf$swc_obs <- NA

      mdf$obs <- NA
      mdf$inp <- NA
      mdf$swc_obs <- NA

      adf$obs <- NA
      adf$inp <- NA
      adf$swc_obs <- NA

    }

    ##---------------------------------------------------------
    ## add list to list of this site
    ##---------------------------------------------------------
    list_modobs[[ sitename ]]$ddf <- ddf
    list_modobs[[ sitename ]]$wdf <- wdf
    list_modobs[[ sitename ]]$mdf <- mdf
    list_modobs[[ sitename ]]$adf <- adf

  } else {

    list_modobs[[ sitename ]]$ddf <- NA
    list_modobs[[ sitename ]]$wdf <- NA
    list_modobs[[ sitename ]]$mdf <- NA
    list_modobs[[ sitename ]]$adf <- NA

  }

  return( list_modobs )

}
