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

cleandata_nn <- function( data, varnam ){
  ##------------------------------------------------
  ## Remove cold days and days where GPP is negative
  ##------------------------------------------------
  require( dplyr )

  if (varnam=="gpp_obs"){
    data <- filter( data, !is.na(gpp_obs) )
    data <- filter( data, gpp_obs > 0.0 )

  } else if (varnam=="et_obs"){
    data <- filter( data, !is.na( et_obs ) )    

  } else if (varnam=="wue_obs"){
    data <- filter( data, !is.na( wue_obs ) )    

  } else if (varnam=="lue_obs"){
    data <- filter( data, !is.na( lue_obs ) )    

  }

  return( data )
}

