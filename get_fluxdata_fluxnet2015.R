get_fluxdata_fluxnet2015 <- function( path, add_swcvars=FALSE ){
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

  source("clean_fluxnet.R")

  ## from flux to energy conversion, umol/J (Meek et al., 1984), same as used in SPLASH (see Eq.50 in spash_doc.pdf)
  kfFEC <- 2.04

  ## molar mass of C
  c_molmass <- 12.0107

  ## get data
  ddf <-  read_csv( path, na="-9999", col_types = cols() ) %>%
          mutate( date = ymd( TIMESTAMP ) )

  ## clean data
  out_clean <- clean_fluxnet_gpp( ddf$GPP_NT_VUT_REF, ddf$GPP_DT_VUT_REF, ddf$NEE_VUT_REF_NIGHT_QC, ddf$NEE_VUT_REF_DAY_QC, cutoff=0.5 )
  ddf$GPP_NT_VUT_REF <- out_clean$gpp_nt
  ddf$GPP_DT_VUT_REF <- out_clean$gpp_dt

  ddf$LE_F_MDS_good <- clean_fluxnet_et( ddf$LE_F_MDS, ddf$LE_F_MDS_QC, cutoff=0.5 )
  ddf$LE_F_MDS      <- clean_fluxnet_et( ddf$LE_F_MDS, ddf$LE_F_MDS_QC, cutoff=0.2 )

  ## convert units. given in umolCO2 m-2 s-1. converted to gC m-2 d-1
  ddf <- ddf %>% mutate(
                        GPP_NT_VUT_REF     = GPP_NT_VUT_REF     * 1e-6 * 60 * 60 * 24 * c_molmass,
                        GPP_NT_VUT_USTAR50 = GPP_NT_VUT_USTAR50 * 1e-6 * 60 * 60 * 24 * c_molmass,
                        GPP_DT_VUT_REF     = GPP_DT_VUT_REF     * 1e-6 * 60 * 60 * 24 * c_molmass,
                        GPP_DT_VUT_USTAR50 = GPP_DT_VUT_USTAR50 * 1e-6 * 60 * 60 * 24 * c_molmass,
                        LE_F_MDS           = LE_F_MDS                  * 60 * 60 * 24,  ## W m-2 -> J m-2 d-1
                        LE_F_MDS_good      = LE_F_MDS_good             * 60 * 60 * 24   ## W m-2 -> J m-2 d-1
                        )


  if (add_swcvars){

    full <- ddf
    ddf  <- ddf %>% dplyr::select( date, GPP_NT_VUT_REF, GPP_NT_VUT_USTAR50, GPP_DT_VUT_REF, GPP_DT_VUT_USTAR50, LE_F_MDS, LE_F_MDS_good ) 

    ## collect additional SWC variables for different depths provided for each site
    ddf_swc <- full %>% dplyr::select( date )
    relevant <- names(full)[(is.element( substr(names(full), start=1, stop=3), "SWC" ))]
    swcvars <- relevant[ which( !substr( relevant, start=nchar(relevant)-1, stop=nchar(relevant) )=="QC") ]
    swcqcvars <- relevant[ which( substr( relevant, start=nchar(relevant)-1, stop=nchar(relevant) )=="QC") ]
    if (length(swcvars)>0){
      for (ivar in 1:length(swcvars)){
        ddf_swc[[ swcvars[ivar] ]] <- clean_fluxnet_swc( full[[ swcvars[ivar] ]], full[[ swcqcvars[ivar] ]] )
      }
    }
    out <- list( obs=ddf, obs_swc=ddf_swc )
  
  } else {
   
    ddf <- ddf %>% dplyr::select( date, GPP_NT_VUT_REF, GPP_NT_VUT_USTAR50, GPP_DT_VUT_REF, GPP_DT_VUT_USTAR50, LE_F_MDS, LE_F_MDS_good ) 
    out <- list( obs=ddf, obs_swc=NA )

  }

  return( out )
}