get_modobs_fluxnet2015 <- function( sitename, simsuite, outputset, data=NA, getvars=c( "gpp", "wcont", "aet", "pet" ), add_swcvars=TRUE, overwrite=overwrite, overwrite_dosites=overwrite_dosites, outdir="./" ){

  # ## XXX debug------------------------------------------
  # simsuite <- "fluxnet2015"
  # outputset <- c( "s11", "s12", "s13" )
  # getvars   <- c( "gpp", "wcont", "aet", "pet", "ra" )
  # add_swcvars <- TRUE
  # overwrite   <- TRUE
  # overwrite_dosites <- TRUE 
  # ##----------------------------------------------------

  source( "get_fluxdata_fluxnet2015.R" )
  source( "get_meteo_fluxnet2015.R" )

  avl2015 <- TRUE
  avl_mod <- rep( TRUE, length(outputset) )

  ##---------------------------------------------------------
  ## read SOFUN output data for each output set -> ddf as list
  ##---------------------------------------------------------
  print( paste( "getting model output data ..." ) )
  site <- list()
  ddf  <- list()

  kdx <- 0
  for (iset in outputset){
    kdx <- kdx + 1

    dirnam_mod <- paste( myhome, "sofun/output_nc_fluxnet2015_sofun/", iset, "/", sep="" )

    ddf_tmp <- try( get_daily_modelout( sitename, dirnam_mod, getvars ) )

    if (class(ddf_tmp)=="try-error") { 
    
      print( paste( "error opening", path ) )
      avl_mod[kdx]          <- FALSE
      missing_mod[[ iset ]] <- c( missing_mod[[ iset ]], sitename )
    
    } else {

      ## add to list
      ddf[[ iset ]] <- ddf_tmp

    }
  }


  ##---------------------------------------------------------
  ## read FLUXNET 2015 data -> ddf_obs, ddf_swc_obs
  ##---------------------------------------------------------
  print( paste( "getting FLUXNET 2015 data ..." ) )
  dirnam_obs <- paste0( myhome, "data/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked/" )
  allfiles <- list.files( dirnam_obs )
  allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
  allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
  filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
  path <- paste0( dirnam_obs, filnam_obs )
  
  out <- try( get_fluxdata_fluxnet2015( path, add_swcvars=add_swcvars ) ) 
  
  if (class(out)=="try-error") { 
    print( paste( "error opening", path ) )
    avl2015 <- FALSE
    missing_2015 <- c( missing_2015, sitename )
  } 


  ##---------------------------------------------------------
  ## read input data -> ddf_in
  ##---------------------------------------------------------
  ## climate
  print( paste( "getting climate input data ..." ) )
  dirnam_obs <- paste0( myhome, "data/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked/" )
  allfiles <- list.files( dirnam_obs )
  allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
  filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
  filn <- paste0( dirnam_obs, filnam_obs )
  if ( length(filnam_obs)>0 ){ 
    ddf_inclim <- try( get_meteo_fluxnet2015( filn ) ) 
  }
  if (class(ddf_inclim)=="try-error"){
    missing_inclim <- c( missing_inclim, sitename )    
  } else {
    ddf_inclim <- ddf_inclim %>% mutate( date = ymd(date) )
  }
  
  ## fAPAR
  print( paste( "getting fapar input data (MODIS FPAR) ..." ) )
  filn <- paste0( myhome, "sofun/input_fluxnet2015_sofun/sitedata/fapar/", sitename, "/dfapar_MODIS_FPAR_MCD15A3H_", sitename, "_gee_subset.csv" )
  ddf_infpar <- try( read_csv( filn ) )
  if (class(ddf_infpar)=="try-error"){
    missing_infpar <- c( missing_infpar, sitename )    
  } else {
    ddf_infpar <- ddf_infpar %>% rename( fpar = modisvar_interpol ) %>% select( date, fpar ) %>% mutate( date = ymd(date) )
  }


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

      ## add to list
      ddf$obs     <- out$obs
      ddf$inp     <- ddf_inclim %>% left_join( ddf_infpar, by = "date" )
      ddf$swc_obs <- out$obs_swc

    } else {
    
      ddf$obs <- NA
      ddf$inp <- NA
      ddf$swc_obs <- NA

    }

    ##---------------------------------------------------------
    ## add ddf list to list of this site
    ##---------------------------------------------------------
    fluxnet[[ sitename ]]$ddf <- ddf

  } else {

    fluxnet[[ sitename ]]$ddf <- NA

  }

  return( fluxnet )

}
