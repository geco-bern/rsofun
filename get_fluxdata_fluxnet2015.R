get_fluxdata_fluxnet2015 <- function( sitename, freq="y" ){
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
  # freq = "m"
  # ## -----------------------

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

  }

  if (length(filnam_obs)>0){

    ## from flux to energy conversion, umol/J (Meek et al., 1984), same as used in SPLASH (see Eq.50 in spash_doc.pdf)
    kfFEC <- 2.04

    ## molar mass of C
    c_molmass <- 12.0107

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

    }

    ## convert units. given in umolCO2 m-2 s-1. converted to gC m-2 d-1
    df <- df %>%  mutate(
                          GPP_NT_VUT_REF     = as.numeric(GPP_NT_VUT_REF)    ,
                          GPP_NT_VUT_USTAR50 = as.numeric(GPP_NT_VUT_USTAR50),
                          GPP_DT_VUT_REF     = as.numeric(GPP_DT_VUT_REF)    ,
                          GPP_DT_VUT_USTAR50 = as.numeric(GPP_DT_VUT_USTAR50),
                          LE_F_MDS           = as.numeric(LE_F_MDS),             ## W m-2 -> J m-2 d-1
                          gpp_obs            = ( GPP_NT_VUT_REF + GPP_DT_VUT_REF ) / 2  
                          ) %>%
                  select( date, GPP_NT_VUT_REF, GPP_NT_VUT_USTAR50, GPP_DT_VUT_REF, GPP_DT_VUT_USTAR50, LE_F_MDS, gpp_obs )

  } else {

    df <- NA

  }

  return( df )
}