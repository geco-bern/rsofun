get_daily_modelout <- function( expname, dirnam_mod, vars ){

  # ## xxx debug >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # expname <- "FR-Pue"
  # dirnam_mod <- "~/sofun/output_nc_fluxnet2015_sofun/s15/"
  # vars <- c("gpp","wcont","aet","pet")
  # ## <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  ## read GPP file to initialise data frame and get years
  filnam_mod <- paste0( expname, ".d.gpp.nc" )
  path       <- paste0( dirnam_mod, filnam_mod )
  nc         <- ncdf4::nc_open( path )
  gpp        <- ncdf4::ncvar_get( nc, varid = "gpp" )
  time       <- ncdf4::ncvar_get( nc, varid = "time" )
  ncdf4::nc_close(nc)

  ## convert to a ymd datetime object
  time <- conv_noleap_to_ymd( time, since="2001-01-01" )

  ddf <- tibble( date=time, gpp=gpp )

  readvars <- vars[ vars!="fapar" & vars!="gpp" ]

  if (class(nc)!="try-error") { 

    for (ivar in readvars){
      filnam_mod <- paste0( expname, ".d.", ivar, ".nc" )
      path       <- paste0( dirnam_mod, filnam_mod )
      nc         <- ncdf4::nc_open( path )
      addvar     <- ncdf4::ncvar_get( nc, varid = ivar )
      ddf        <- tibble( date=time, ivar=addvar ) %>% setNames( c("date", ivar) ) %>% right_join( ddf, by = "date" )
    }

  } else {
    ddf <- NA
  }

  return( ddf )
}
