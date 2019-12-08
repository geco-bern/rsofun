#' Filter daily observations
#'
#' Filters observational data points for \code{gpp_obs} (hard-coded) in one data source, based on data availability from another data source.
#'
#' @param ddf xxx
#' @param filter_days_source xxx
#' @param path_gepisat xxx
#'
#' @return xxx
#' @export
#'
#' @examples xxx
#' 
filter_days <- function( ddf, filter_days_source, path_gepisat ){

  ## replace all 'gpp_obs' from FLUXNET 2015 data with NA if GPP from DT decomposition is NA
  if ("fluxnet2015_DT" %in% filter_days_source){
    # print("Filtering days based on GPP_DT_VUT_REF")
    ddf <- ddf %>% dplyr::mutate( gpp_obs = ifelse( is.na(GPP_DT_VUT_REF), NA, gpp_obs ) )  
  }

  ## replace all 'gpp_obs' from FLUXNET 2015 data with NA if GPP from NT decomposition is NA
  if ("fluxnet2015_NT" %in% filter_days_source){
    # print("Filtering days based on GPP_NT_VUT_REF")
    ddf <- ddf %>% dplyr::mutate( gpp_obs = ifelse( is.na(GPP_NT_VUT_REF), NA, gpp_obs ) )  
  }

  if ("fluxnet2015_Ty" %in% filter_days_source){
    # print("Filtering days based on gpp_obs_gepisat")
    ## Filter out data points based on GePiSaT data
    
    avl_gepisat <- TRUE
    if (!("gpp_obs_gepisat" %in% names(ddf))){
      ## get GePiSaT data
      ## Make sure data is available for this site
      error <- check_download_gepisat( path_gepisat, unique(ddf$sitename) )
      
      ddf_gepisat <- get_obs_bysite_gpp_gepisat( unique(ddf$sitename), path_gepisat, "d" )
      
      ## add to other data frame and take take weighted average for updated 'gpp_obs'
      if (!is.null(ddf_gepisat)){
        
        ddf <- ddf_gepisat %>%
          ## Some GPP data looks weird when its error in resp. day is zero. Exclude this data.
          dplyr::mutate( gpp_obs = ifelse( gpp_err_obs == 0.0, NA, gpp_obs ) ) %>% 
          dplyr::rename( gpp_obs_gepisat = gpp_obs ) %>%
          dplyr::right_join( ddf, by = "date" )
          
      } else {
        ## No GePiSaT data available for this site. Consider all GPP data missing (NA).
        ddf <- ddf %>% dplyr::mutate( gpp_obs = NA )
        avl_gepisat <- FALSE
      }
      
    }
    
    ## replace all 'gpp_obs' from FLUXNET 2015 data with NA if GePiSaT-GPP is NA
    if (avl_gepisat) ddf <- ddf %>% dplyr::mutate( gpp_obs = ifelse( is.na(gpp_obs_gepisat), NA, gpp_obs ) )  

  }
  return(ddf)
}

