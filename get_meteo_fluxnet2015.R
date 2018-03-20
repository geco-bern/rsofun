get_meteo_fluxnet2015 <- function( sitename, freq="y" ){
  ##--------------------------------------------------------------------
  ## Function returns a dataframe containing all the data of flux-derived
  ## GPP for the station implicitly given by path (argument).
  ## Specific for FLUXNET 2015 data
  ## Returns variables in the following units:
  ## temp: deg C
  ## vpd : Pa
  ## prec: mm d-1
  ## nrad: J m-2 d-1
  ## swin: J m-2 d-1
  ## ppfd: mol m-2 d-1 
  ##--------------------------------------------------------------------
  require(dplyr)
  require(readr)
  require(lubridate)

  # ## xxx debug -------------
  # sitename = "FR-Pue"
  # add_swcvars = TRUE
  # freq = "d"
  # ## -----------------------
  
  ## from flux to energy conversion, umol/J (Meek et al., 1984), same as used in SPLASH (see Eq.50 in spash_doc.pdf)
  kfFEC <- 2.04

  if ( freq=="y" ){
    ## Annual data
    print( paste( "getting annual FLUXNET 2015 data for site", sitename ) )
    dirnam_obs <- paste0( myhome, "data/FLUXNET-2015_Tier1/20160128/point-scale_none_1y/original/unpacked/" )
    allfiles <- list.files( dirnam_obs )
    allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
    allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
    filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
    path <- paste0( dirnam_obs, filnam_obs ) 

  } else if ( freq=="m" ){
    ## Monthly data
    print( paste( "getting monthly FLUXNET 2015 data for site", sitename ) )
    dirnam_obs <- paste0( myhome, "data/FLUXNET-2015_Tier1/20160128/point-scale_none_1m/original/unpacked/" )
    allfiles <- list.files( dirnam_obs )
    allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
    allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
    filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
    path <- paste0( dirnam_obs, filnam_obs ) 

  } else if ( freq=="w" ){
    ## Weekly data
    print( paste( "getting weekly FLUXNET 2015 data for site", sitename ) )
    dirnam_obs <- paste0( myhome, "data/FLUXNET-2015_Tier1/20160128/point-scale_none_7d/original/unpacked/" )
    allfiles <- list.files( dirnam_obs )
    allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
    allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
    filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
    path <- paste0( dirnam_obs, filnam_obs ) 

  } else if ( freq=="d" ){
    ## Daily data
    print( paste( "getting annual FLUXNET 2015 data for site", sitename ) )
    dirnam_obs <- paste0( myhome, "data/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked/" )
    allfiles <- list.files( dirnam_obs )
    allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
    allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
    filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
    path <- paste0( dirnam_obs, filnam_obs ) 

  }

  if ( length(filnam_obs)>0 ){ 

    ## get daily meteo data
    meteo <-  read_csv( path, na="-9999", col_types = cols() )

    ## get dates, their format differs slightly between temporal resolution
    if ( freq=="y" ){

      meteo <- meteo %>% mutate( year = TIMESTAMP ) %>% mutate( date = ymd( paste0( as.character(year), "-01-01" ) ) )      

    } else if ( freq=="w"){

      meteo <- meteo %>% mutate( date_start = ymd(TIMESTAMP_START), date_end = ymd(TIMESTAMP_END) ) %>% 
                         mutate( date = date_start )

    } else if ( freq=="m" ){

      meteo <- meteo %>% mutate( year = substr( TIMESTAMP, start = 1, stop = 4 ), month = substr( TIMESTAMP, start = 5, stop = 6 ) ) %>%
                         mutate( date = ymd( paste0( as.character(year), "-", as.character(month), "-01" ) ) )
    
    } else if ( freq=="d" ){

      meteo <- meteo %>% mutate( date = ymd( TIMESTAMP ) )

    }

    meteo <- meteo %>%  rename( temp = TA_F,
                                vpd  = VPD_F,
                                prec = P_F,
                                swin = SW_IN_F
                              ) %>%
                        mutate( swin = swin * 60 * 60 * 24,   # given in W m-2, required in J m-2 d-1 
                                ppfd = swin * kfFEC * 1.0e-6, # convert from J/m2/d to mol/m2/d
                                vpd  = vpd * 1e2,             # given in hPa, required in Pa
                                ccov = NA,
                                nrad = ifelse( is.element( "NETRAD", names(.) ), as.numeric(NETRAD) * 60 * 60 * 24, NA ) # given in W m-2 (avg.), required in J m-2 (daily total)
                              ) %>% 
                        select( date, temp, prec, nrad, ppfd, vpd, ccov )

  } else {

    meteo <- NA

  }

  return( meteo )
}