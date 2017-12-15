get_meteo_fluxnet2015 <- function( path ){
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
  require(lubridate)

  ## from flux to energy conversion, umol/J (Meek et al., 1984), same as used in SPLASH (see Eq.50 in spash_doc.pdf)
  kfFEC <- 2.04

  ## get daily meteo data
  meteo <-  read_csv( path, na="-9999", col_types = cols() ) %>%
            mutate( date = ymd( TIMESTAMP ) ) %>%
            rename( temp = TA_F,
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

  return( meteo )
}