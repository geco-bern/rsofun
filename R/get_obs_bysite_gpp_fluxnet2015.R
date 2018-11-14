#' Get FLUXNET 2015 observational data for one site.
#'
#' Function for reading observational GPP data from FLUXNET 2015 dataset
#' and defining calibration target (which flux decomposition method etc.)
#'
#' @param sitename A character string specifying the site name for which FLUXNET 2015 data is searched (based on the site name appearing as part of the respective file name). Defaults to NA.
#' @param path_fluxnet2015 A character string specifying the local path of FLUXNET 2015 data.
#' @param timescale A character specifying the time scale of FLUXNET 2015 data. Any of \code{c("d", "w", "m", "y")} for daily, weekly, monthly, or yearly, respectively.
#' @param methodd A character string specifying the data based on which GPP flux decompisiton method is to be used. Defaults to \code{"NT"} for nighttime decomposition. Any of \code{c("DT", "NT")}  . 
#'
#' @return A data frame (tibble) containint observational data.
#' @export
#'
#' @examples df <- get_obs_bysite_gpp_fluxnet2015( "FR-Pue", "./inst/extdata/", timescale = "d, method = "NT" )
#' 
get_obs_bysite_gpp_fluxnet2015 <- function( sitename, path_fluxnet2015, timescale, method = "NT" ){

  ## Get GPP data from FLUXNET 2015 dataset
  getvars <- c( 
    "GPP_NT_VUT_REF", "GPP_DT_VUT_REF",                
    "LE_F_MDS", "LE_F_MDS_QC", 
    "NEE_VUT_REF_NIGHT_QC", "NEE_VUT_REF_DAY_QC", # quality flag that goes with GPP 
    "TA_F" # air temperature, used for filtering
    )

  ## Take only file for this site
  if (timescale=="d"){
    ## Daily
    filn <- list.files( path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_DD.*.csv" ), 
      recursive = TRUE 
      )
  } else  if (timescale=="w"){
    ## Weekly
    filn <- list.files( path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_WW.*.csv" ), 
      recursive = TRUE 
      )
  } else  if (timescale=="m"){
    ## Monthly
    filn <- list.files( path_fluxnet2015, 
      pattern = paste0("FLX_", sitename, ".*_FLUXNET2015_FULLSET_MM.*.csv"), 
      recursive = TRUE 
      )
  } else  if (timescale=="y"){
    ## Annual
    filn <- list.files( path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_YY.*.csv" ), 
      recursive = TRUE 
      )
  }

  if (length(filn)==0) abort(paste0("No files found for timescale ", timescale, " in sub-directories of ", path_fluxnet2015 ) )
  if (length(filn)>1){
    filn <- filn[which(grepl("3.csv", filn))]
    # warn(paste0("Multiple files found for timsescale ", timescale, " in sub-directories of ", path_fluxnet2015, ". Taking only ", filn ) )
  }
  
  ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
  df <- get_obs_fluxnet2015_raw( sitename, 
    path = paste0(path_fluxnet2015, filn), 
    freq = timescale 
    ) %>%

    ## convert to numeric (weirdly isn't always) and subset (select)
    mutate_at( vars(one_of(getvars)), funs(as.numeric)) %>%
    dplyr::select( date, one_of(getvars), starts_with("SWC_") ) %>%  # get soil water content data for filtering later
    mutate_at( vars(starts_with("SWC_")), funs(as.numeric) ) %>%

    ## XXX THIS IS WRONG! DATA IS ALREADY GIVEN IN gC m-2 d-1
    # ## Convert units
    # ## given in umolCO2 m-2 s-1. converted to gC m-2 d-1
    # mutate_at( vars(starts_with("GPP_")), funs(convert_gpp_fluxnet2015) ) %>%
    ## XXXXXXXXXXXXX

    ## W m-2 -> J m-2 d-1
    mutate_at( vars(starts_with("LE_")), funs(convert_le_fluxnet2015))
    
  # ## Convert units to gC m-2 mo-1 and gC m-2 w-1 (per month/week instead of per day)
  # if (timescale=="m") df <- df %>% mutate( ndaymonth = days_in_month(date) ) %>%
  #                                  mutate_at( vars( starts_with("GPP_") ), funs( "*"(.,ndaymonth)) )
  # if (timescale=="w") df <- df %>% mutate_at( vars( starts_with("GPP_") ), funs( "*"(.,7.0)) )
  
  ## clean data
  if (any( !(c("GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "NEE_VUT_REF_NIGHT_QC", "NEE_VUT_REF_DAY_QC") %in% getvars) )) abort("Not all variables read from file that are needed for data cleaning.")
  out_clean <- clean_fluxnet_gpp( df$GPP_NT_VUT_REF, df$GPP_DT_VUT_REF, df$NEE_VUT_REF_NIGHT_QC, df$NEE_VUT_REF_DAY_QC, cutoff=0.5 )
  df$GPP_NT_VUT_REF <- out_clean$gpp_nt
  df$GPP_DT_VUT_REF <- out_clean$gpp_dt

  if (any( !(c("LE_F_MDS", "LE_F_MDS_QC") %in% getvars) )) abort("Not all variables read from file that are needed for data cleaning.")
  # df$LE_F_MDS_good <- clean_fluxnet_et( df$LE_F_MDS, df$LE_F_MDS_QC, cutoff=0.5 )
  df$LE_F_MDS <- clean_fluxnet_et( df$LE_F_MDS, df$LE_F_MDS_QC, cutoff=0.2 )

  ## Soil moisture related stuff for daily data
  if (timescale=="d"){
    tmp <- df %>% dplyr::select( starts_with("SWC") )
    if (ncol(tmp)>0){
      swcvars   <- tmp %>% dplyr::select( -ends_with("QC") ) %>% names()
      swcqcvars <- tmp %>% dplyr::select(  ends_with("QC") ) %>% names()
      
      # map( as.list(seq(length(swcvars))), ~clean_fluxnet_swc( df[[ swcvars[.] ]], df[[ swcqcvars[.] ]]) )
      if (length(swcvars)>0){
        for (ivar in 1:length(swcvars)){
          df[[ swcvars[ivar] ]] <- clean_fluxnet_swc( df[[ swcvars[ivar] ]], df[[ swcqcvars[ivar] ]], frac_data_thresh=1.0 )
        }
      }
      
      df <- df %>%
        ## Normalise mean observational soil moisture to within minimum (=0) and maximum (=1), and
        mutate_at( vars(starts_with("SWC_F_MDS")), funs(norm_to_max(.)) ) %>%
        
        ## get mean observational soil moisture across different depths (if available)
        mutate( soilm_obs_mean = apply( dplyr::select( ., one_of(swcvars) ), 1, FUN = mean, na.rm = TRUE ) ) %>%
        mutate( soilm_obs_mean = ifelse( is.nan(soilm_obs_mean), NA, soilm_obs_mean ) )
      
    } else {
      df <- df %>% mutate( soilm_obs_mean = NA )
    }
    
  } else {
    df <- df %>% mutate( soilm_obs_mean = NA )
  }

  ## define which data is to be used as target for calibration 'gpp_obs'
  if (identical(method, "NT")){
    df <- df %>% mutate( gpp_obs = GPP_NT_VUT_REF )

  } else if (identical(method, "DT")){
    df <- df %>% mutate( gpp_obs = GPP_DT_VUT_REF )

  } else if ( identical(method, c("DT", "NT")) || identical(method, c("NT", "DT")) ){
    df$gpp_obs <- apply( dplyr::select( df, GPP_NT_VUT_REF, GPP_DT_VUT_REF ), 1, FUN = mean, na.rm = FALSE )

  } else {
    df <- df %>% mutate( gpp_obs = NA )
    
  }

  df <- df %>%  mutate( transp_obs = LE_F_MDS, temp = TA_F, gpp_obs = ifelse( is.nan(gpp_obs), NA, gpp_obs ) ) %>%
                dplyr::select( date, gpp_obs, transp_obs, soilm_obs_mean, temp, GPP_NT_VUT_REF, GPP_DT_VUT_REF )

  return(df)

}

##----------------------------------------------------------------------
## Function for reading observational GPP data from FLUXNET 2015 dataset
##----------------------------------------------------------------------
get_obs_bysite_wcont_fluxnet2015 <- function( sitename, path_fluxnet2015, timescale ){

  getvars <- "SWC"

  ## Take only file for this site
  if (timescale=="d"){
    ## Daily
    filn <- list.files( path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_DD.*.csv" ), 
      recursive = TRUE 
      )
  } else  if (timescale=="w"){
    ## Weekly
    filn <- list.files( path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_WW.*.csv" ), 
      recursive = TRUE 
      )
  } else  if (timescale=="m"){
    ## Monthly
    filn <- list.files( path_fluxnet2015, 
      pattern = paste0(..., collapse = NULL), 
      recursive = TRUE 
      )
  } else  if (timescale=="y"){
    ## Annual
    filn <- list.files( path_fluxnet2015, 
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_YY.*.csv" ), 
      recursive = TRUE 
      )
  }

  if (length(filn)==0) abort(paste0("No files found for timescale ", timescale, "in sub-directories of ", path_fluxnet2015 ) )

  ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
  ddf <- get_obs_fluxnet2015_raw( sitename, 
    path = paste0(path_fluxnet2015, filn),
    freq = "d" 
    )

  ## convert to numeric (weirdly isn't always) and subset (select)
  if ( identical( getvars , "SWC" ) ){
    df <- df %>% mutate_at( vars(starts_with(getvars)), funs(as.numeric)) %>%
                 dplyr::select( date, starts_with(getvars) )
  } else {
    df <- df %>%  mutate_at( vars(one_of(getvars)), funs(as.numeric)) %>%
                  dplyr::select( date, one_of(getvars) )
  }


  swcvars   <- dplyr::select( vars(ddf), starts_with("SWC") ) %>% dplyr::select( vars(ddf), !ends_with("QC") ) %>% names()
  swcqcvars <- dplyr::select( vars(ddf), starts_with("SWC") ) %>% dplyr::select( vars(ddf),  ends_with("QC") ) %>% names()

  # map( as.list(seq(length(swcvars))), ~clean_fluxnet_swc( ddf[[ swcvars[.] ]], ddf[[ swcqcvars[.] ]], frac_data_thresh=0.5 ) )

  if (length(swcvars)>0){
    for (ivar in 1:length(swcvars)){
      ddf[[ swcvars[ivar] ]] <- clean_fluxnet_swc( ddf[[ swcvars[ivar] ]], ddf[[ swcqcvars[ivar] ]], frac_data_thresh=0.5 )
    }
  }
  
  return(ddf)

}   


get_obs_fluxnet2015_raw <- function( sitename, path, freq="d" ){
  ##--------------------------------------------------------------------
  ## Function returns a dataframe containing all the data of the FLUXNET 
  ## 2015 data file of respective temporal resolution.
  ## Returns data in units given in the fluxnet 2015 dataset
  ##--------------------------------------------------------------------
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

  return( df )

}

# ## Converts units of GPP variables from FLUXNET 2015 to SOFUN standard
# convert_gpp_fluxnet2015 <- function( gpp ){
#   # in FLUXNET 2015 given in umolCO2 m-2 s-1. converted to gC m-2 d-1
#   c_molmass <- 12.0107  # molar mass of C
#   gpp_coverted <- gpp * 1e-6 * 60 * 60 * 24 * c_molmass
#   return(gpp_coverted)

# }

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
  ## Remove data points that are based on too much gap-filled data in the underlying half-hourly data
  # frac_data_thresh <- 0.2  ## fraction of data based on gap-filled half-hourly
  et[ qflag_et < cutoff ] <- NA

  if ( any(!is.na(qflag_et)) ){ et[ is.na(qflag_et) ] <- NA }

  et <- identify_pattern( et )

  return( et )
}

clean_fluxnet_swc <- function( swc, qflag_swc, frac_data_thresh=1.0 ){
  ##--------------------------------------------------------------------
  ## frac_data_thresh: fraction of data based on gap-filled half-hourly
  ##--------------------------------------------------------------------
  ## Remove data points that are based on too much gap-filled data in the underlying half-hourly data
  swc[ which( qflag_swc < frac_data_thresh ) ] <- NA
  swc <- as.numeric( swc )

  return( swc )
}

norm_to_max <- function( vec ){
  vec <- ( vec - min( vec, na.rm=TRUE ) ) / ( max( vec, na.rm=TRUE ) - min( vec, na.rm=TRUE ) )
  return( vec )
}
