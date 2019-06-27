get_vpd_day_fluxnet2015 <- function(dir){
  
  ## loop over all HH files in the directory 'dir'
  out <- purrr::map( as.list(list.files(dir, pattern = "HH")),
              ~get_vpd_day_fluxnet2015_byfile(paste0(dir, .)))
  
  return(out)
}

get_vpd_day_fluxnet2015_byfile <- function(filename_hh, write=FALSE){
  
  ## read half-hourly data
  if (!file.exists(filename_hh)) rlang::abort(paste("Half-hourly file does not exist:", filename_hh))
  
  df <- readr::read_csv(filename_hh) %>% 
    dplyr::mutate( date_start = lubridate::ymd_hm( TIMESTAMP_START ),
                   date_end   = lubridate::ymd_hm( TIMESTAMP_END ) ) %>%
    dplyr::mutate( date = date_start ) %>% 
    
    ## retain only daytime data = when incoming shortwave radiation is positive
    dplyr::filter(SW_IN_F>0) %>% 
    
    ## take mean over daytime values
    dplyr::mutate(date_day = lubridate::as_date(date_start)) %>% 
    dplyr::group_by(date_day) %>%
    dplyr::summarise(VPD_F_DAY = mean(VPD_F, na.rm=TRUE),
                     VPD_F_DAY_QC = sum(is.element(VPD_F_QC, c(0,1)))/n(),
                     VPD_F_DAY_MDS = mean(VPD_F_MDS, na.rm=TRUE),
                     VPD_F_DAY_MDS_QC = sum(is.element(VPD_F_MDS_QC, c(0,1)))/n(),
                     VPD_DAY_ERA = mean(VPD_ERA, na.rm=TRUE) ) %>% 
    dplyr::rename(date = date_day)
    
  ## write to csv file  
  if (write){
    filename_dd <- filename_hh %>% 
      stringr::str_replace("HH", "DD") %>% 
      stringr::str_replace(".csv", "_VPD_DAY.csv")
    print(paste("Writing file with daytime VPD as:", filename_dd))
    readr::write_csv(df, path=filename_dd)
  }

  return(df)
}
