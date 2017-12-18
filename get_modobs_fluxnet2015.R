get_modobs_fluxnet2015 <- function( sitename, simsuite, outputset, data=NA, getvars=c( "gpp", "wcont", "aet", "pet" ), add_swcvars=TRUE, overwrite=overwrite, overwrite_dosites=overwrite_dosites, outdir="./" ){

  # ## XXX debug------------------------------------------
  # simsuite = "fluxnet2015"
  # data = fluxnet
  # outputset = c( "s14" )
  # getvars   = c( "gpp", "wcont", "aet", "pet" )
  # add_swcvars = TRUE
  # overwrite   = TRUE
  # overwrite_dosites = TRUE
  # outdir="./"
  # ##----------------------------------------------------

  source( "get_fluxdata_fluxnet2015.R" )
  source( "get_meteo_fluxnet2015.R" )

  avl2015 <- TRUE
  avl_mod <- rep( TRUE, length(outputset) )

  norm_to_max <- function( vec ){
    vec <- ( vec - min( vec, na.rm=TRUE ) ) / ( max( vec, na.rm=TRUE ) - min( vec, na.rm=TRUE ) )
    return( vec )
  }

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

      ## For comparison with FLUXNET 2015 data, normalise simulated soil moisture to between 0 and 1
      print( "WARNING: Normalising simulated soil moisture to maximum (=1) for comparability with observational data.")
      ddf_tmp <- ddf_tmp %>% mutate( wcont = wcont / max( wcont, na.rm = TRUE ) )  

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
  } else {
    ## Normalise observational soil moisture to within minimum (=0) and maximum (=1), and
    out$obs_swc <- out$obs_swc %>% mutate_at( vars(starts_with("SWC_F_MDS")), funs(norm_to_max(.)) )
    
    ## get mean soil observational moisture across different depths (if available)
    out$obs_swc <- out$obs_swc %>%
      mutate( soilm_obs_mean = apply( select( out$obs_swc, starts_with("SWC_F_MDS") ), 1, FUN=mean, na.rm=TRUE ) ) %>%
      mutate( soilm_obs_mean = ifelse( is.nan(soilm_obs_mean), NA, soilm_obs_mean ) )
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
    data[[ sitename ]]$ddf <- ddf

  } else {

    data[[ sitename ]]$ddf <- NA

  }

  return( data )

}
