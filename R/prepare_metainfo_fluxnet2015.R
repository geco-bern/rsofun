get_pointdata_elv_watch <- function( lon, lat, filn ){
  ##--------------------------------------------------------------------
  ## Extract monthly data from files for each year and attach to the 
  ## monthly dataframe (at the right location).
  ## Original data in K, returns data in K
  ##--------------------------------------------------------------------
  if ( !file.exists( filn ) ) {
    source("download_file_cx1.R")
    path_remote <- "/work/bstocker/labprentice/data/watch_wfdei/WFDEI-elevation.nc"
    path_local <- filn
    download_file_cx1( path_remote, path_local )
  }

  if ( file.exists( filn ) ){
    
    cmd <- paste( paste0( "./extract_pointdata_byfil.sh " ), filn, "elevation", "lon", "lat", sprintf( "%.2f", lon ), sprintf( "%.2f", lat ) )
    system( cmd )
    out <- read.table( "./out.txt" )$V1

  } else {

    abort("Could not find WATCH-WFDEI elevation file nor download it.")

  }
  return( out )
}


long_to_wide_fluxnet2015 <- function( sitename, long ){

  require(dplyr)
  require(readr)
  require(tidyr)
  require(rlang)
  require(purrr)
  
  sub <- long %>% filter( SITE_ID==sitename )
    
  ## remove variable groups that have lots of duplicates w.r.t. variable
  sub <- sub %>% filter( VARIABLE_GROUP!="GRP_TEAM_MEMBER", VARIABLE!="NETWORK", VARIABLE_GROUP!="GRP_DM_FERT_M", VARIABLE_GROUP!="GRP_DM_AGRICULTURE", VARIABLE_GROUP!="GRP_DM_PESTICIDE", VARIABLE_GROUP!="GRP_DM_PLANTING", VARIABLE_GROUP!="GRP_DM_TILL"  )

  ## determine duplicates
  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )

  ##  treat duplicates
  ## Use only first entry for 'reference paper'
  if (nrow(df_nduplicates)>0){
    if ("REFERENCE_PAPER" %in% df_nduplicates$VARIABLE %>% as.character()){
      varname <- "REFERENCE_PAPER"
      groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
      sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
    }

    ## Use only first entry for 'reference usage'
    df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
    if (nrow(df_nduplicates)>0){
      if ("REFERENCE_USAGE" %in% df_nduplicates$VARIABLE %>% as.character()){
        varname <- "REFERENCE_USAGE"
        groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
        sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
      }
    
      ## Use only first entry for 'reference usage'
      df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
      if (nrow(df_nduplicates)>0){
        if ("REFERENCE_DOI" %in% df_nduplicates$VARIABLE %>% as.character()){
          varname <- "REFERENCE_DOI"
          groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
          sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
        }

        ## Use only first entry for 'reference usage'
        df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
        if (nrow(df_nduplicates)>0){
          if ("REFERENCE_COMMENT" %in% df_nduplicates$VARIABLE %>% as.character()){
            varname <- "REFERENCE_COMMENT"
            groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
            sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
          }          

          ## Use only first entry for 'DOM_DIST_MGMT'
          df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
          if (nrow(df_nduplicates)>0){
            if ("DOM_DIST_MGMT" %in% df_nduplicates$VARIABLE %>% as.character()){
              varname <- "DOM_DIST_MGMT"
              groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
              sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
            }

            ## Use only first entry for 'DM_COMMENT'
            df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
            if (nrow(df_nduplicates)>0){
              if ("DM_COMMENT" %in% df_nduplicates$VARIABLE %>% as.character()){
                varname <- "DM_COMMENT"
                groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
              }

              ## Use only first entry for 'DM_DATE'
              df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
              if (nrow(df_nduplicates)>0){
                if ("DM_DATE" %in% df_nduplicates$VARIABLE %>% as.character()){
                  varname <- "DM_DATE"
                  groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                  sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                }

                ## Use only first entry for 'DM_DATE_UNC'
                df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                if (nrow(df_nduplicates)>0){
                  if ("DM_DATE_UNC" %in% df_nduplicates$VARIABLE %>% as.character()){
                    varname <- "DM_DATE_UNC"
                    groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                    sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                  }

                  ## Use only first entry for 'DM_SURF'
                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                  if (nrow(df_nduplicates)>0){
                    if ("DM_SURF" %in% df_nduplicates$VARIABLE %>% as.character()){
                      varname <- "DM_SURF"
                      groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                      sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                    }

                    ## Use only first entry for 'DM_SURF_MEAS_UNC'
                    df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                    if (nrow(df_nduplicates)>0){
                      if ("DM_SURF_MEAS_UNC" %in% df_nduplicates$VARIABLE %>% as.character()){
                        varname <- "DM_SURF_MEAS_UNC"
                        groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                        sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                      }

                      ## Use only first entry for 'DM_DATE_START'
                      df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                      if (nrow(df_nduplicates)>0){
                        if ("DM_DATE_START" %in% df_nduplicates$VARIABLE %>% as.character()){
                          varname <- "DM_DATE_START"
                          groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                          sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                        } 
                        

                        ## Use only first entry for 'DM_DATE_END'
                        df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                        if (nrow(df_nduplicates)>0){
                          if ("DM_DATE_END" %in% df_nduplicates$VARIABLE %>% as.character()){
                            varname <- "DM_DATE_END"
                            groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                            sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                          }

                          ## Use only first entry for 'DM_DATE_END'
                          df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                          if (nrow(df_nduplicates)>0){
                            if ("DM_GRAZE" %in% df_nduplicates$VARIABLE %>% as.character()){
                              varname <- "DM_GRAZE"
                              groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                              sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                            }

                            ## some towers had a new location (very slight) after re-installation
                            df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                            if (nrow(df_nduplicates)>0){

                              if ("LOCATION_COMMENT" %in% df_nduplicates$VARIABLE){
                                
                                groupid <- sub %>% filter( VARIABLE=="LOCATION_COMMENT") %>% filter( DATAVALUE!="Reinstallation after management activities" ) %>% dplyr::select( GROUP_ID )
                                
                                if (length(unlist(groupid))>1){
                                  print(sub %>% filter( VARIABLE=="LOCATION_COMMENT"))
                                  ans <- readline(prompt = "Enter row number of which group should be used ")
                                  groupid <- groupid[as.numeric(ans),]
                                }
                                
                                if (length(unlist(groupid))==1){
                                  
                                  ## determine variables available for this group id
                                  duplicatedvars <- sub %>% filter( GROUP_ID==groupid$GROUP_ID ) %>% dplyr::select( VARIABLE ) %>% unlist() %>% as.character()
                                  
                                  ## remove rows if VARIABLE is element of duplicatedvars and if its GROUP_ID is not equal to groupid
                                  sub <- sub %>% filter( !(VARIABLE %in% duplicatedvars & GROUP_ID!=groupid$GROUP_ID )  )
                                  
                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                } else {
                                  abort("fuck")
                                }
                                
                              } 

                              df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                              if (nrow(df_nduplicates)>0){
                                
                                if ("FLUX_MEASUREMENTS_VARIABLE" %in% df_nduplicates$VARIABLE %>% as.character()){
                                  ## Often, info is given for measurements of multiple GHGs. Take only info relevant for CO2
                                  groupid <- sub %>% filter( VARIABLE=="FLUX_MEASUREMENTS_VARIABLE") %>% filter( DATAVALUE=="CO2" ) %>% dplyr::select( GROUP_ID )
                                  
                                  if ( length(unlist(groupid))>1 ){
                                    ## sometimes there are chamber measurements and eddy covariance, use only the latter
                                    tmp <- sub %>% filter( GROUP_ID %in% groupid$GROUP_ID & VARIABLE == "FLUX_MEASUREMENTS_METHOD" )
                                    tmp$DATAVALUE <- as.character(unlist(tmp$DATAVALUE))
                                    ## get group id for chambers method
                                    groupid_eddy <- tmp %>% filter( DATAVALUE=="Eddy Covariance") %>% dplyr::select( GROUP_ID )
                                    groupid <- groupid %>% filter( GROUP_ID==groupid_eddy$GROUP_ID )
                                    
                                    if ( length(unlist(groupid))>1 ){
                                      ## apparently, both are Eddy Covariance. Use only first.
                                      groupid <- groupid[1,]
                                    }
                                                                      
                                  }
                                  
                                }
                    
                                if (length(unlist(groupid))==1){
                                  
                                  ## determine variables available for this group id
                                  duplicatedvars <- sub %>% filter( GROUP_ID==groupid$GROUP_ID ) %>% dplyr::select( VARIABLE ) %>% unlist() %>% as.character()
                                  
                                  ## remove rows if VARIABLE is element of duplicatedvars and if its GROUP_ID is not equal to groupid
                                  sub <- sub %>% filter( !(VARIABLE %in% duplicatedvars & GROUP_ID!=groupid$GROUP_ID )  )
                                  
                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                  if (nrow(df_nduplicates)>0){
                                    ## treat remaining duplicated variables
                                    
                                    ## all rows that have a duplicate w.r.t. VARIABLE
                                    tmp <- sub %>% dplyr::select( -VARIABLE_GROUP, -GROUP_ID )
                                    rownr_duplicated <- tmp %>% dplyr::select( VARIABLE ) %>% unlist() %>% as.character() %>% duplicated() %>% which()
                                    varname_duplicated <- tmp[ rownr_duplicated,]$VARIABLE %>% as.character()
                                    rownr_duplicated_all <- which( tmp$VARIABLE%in%varname_duplicated )
                                    
                                    print("duplicated rows:")
                                    print( tmp[rownr_duplicated_all,] )
                                    
                                    ans <- readline( prompt = "continue taking maximum w.r.t. DATAVALUE? (y/n) ")
                                    
                                    if (ans=="y"){
                                      
                                      tmp$DATAVALUE[rownr_duplicated_all] <- as.numeric( as.character( unlist( tmp[ rownr_duplicated_all, "DATAVALUE" ] ) ) )
                                      droprownr_duplicated <- rownr_duplicated_all[ which.min( tmp$DATAVALUE[rownr_duplicated_all] ) ]
                                      sub <- sub[-droprownr_duplicated,]
                                      
                                    } else {
                                      
                                      ans <- readline( prompt = "continue using only first occurrence of duplicates (y/n) ")
                                      if (ans=="y"){
                                        sub <- sub[-rownr_duplicated,]
                                      }
                                      
                                    }
                                  }
                                  
                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                  if (nrow(df_nduplicates)>0){
                                    abort("shitfuck.")
                                  }
                                }
                              }
                            }
                          }                          
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
    
  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
  if (nrow(df_nduplicates)==0 && nrow(sub)>0){
    wide <- sub %>% dplyr::select( -VARIABLE_GROUP, -GROUP_ID ) %>% spread( VARIABLE, DATAVALUE )
  } else {
    abort("remaining duplicates")
    print(df_nduplicates)
    wide <- NULL
  }

  return(wide)

}

prepare_metainfo_fluxnet2015 <- function( settings_sims, settings_input, overwrite=TRUE, filn_elv_watch=NA ){

  require(dplyr)
  require(readr)
  require(tidyr)
  require(rlang)
  require(purrr)
  
  source("download_file_cx1.R")

  ##--------------------------------------------------------------------
  ## read meta info file and reshape to wide format
  ##--------------------------------------------------------------------
  widefiln <- paste0( settings_input$path_cx1data, "/FLUXNET-2015_Tier1/FLX_AA-Flx_BIF_LATEST_WIDE.csv")
  longfiln <- paste0( settings_input$path_cx1data, "/FLUXNET-2015_Tier1/FLX_AA-Flx_BIF_LATEST.csv")
  if (!file.exists(longfiln)){
    warn( "FLUXNET 2015 meta info file missing." )
    warn( paste0("Expected file path: ", longfiln) )
    warn( "Get it from CX1 (manually) and place it at path above. See you soon.")
    abort("Aborting.")
  }
  if (!file.exists(widefiln) || overwrite){
    long <- read.csv( paste0( settings_input$path_cx1data, "/FLUXNET-2015_Tier1/FLX_AA-Flx_BIF_LATEST.csv"), sep = ";" ) %>% as_tibble()
    wide <- purrr::map( as.list(unique(long$SITE_ID)), ~long_to_wide_fluxnet2015( ., long ) ) %>% 
      bind_rows() %>% 
      write_csv( path = widefiln )
    siteinfo <- wide
  } else {
    siteinfo <- read_csv( widefiln )
  }
  
  ##--------------------------------------------------------------------
  ## Complementing information on start year and end year from file names (more reliable data than in the meta info file)
  ##--------------------------------------------------------------------
  ## rename variables (columns) where necessary
  # siteinfo <- rename( siteinfo, c( "LOCATION_ELEV"="elv", "SITE_ID"="mysitename", "LOCATION_LONG"="lon", "LOCATION_LAT"="lat",
  #   "FLUX_MEASUREMENTS_DATE_START"="year_start", "FLUX_MEASUREMENTS_DATE_END"="year_end", "IGBP"="classid" ) )
  siteinfo <- dplyr::rename( siteinfo,
    elv=LOCATION_ELEV, mysitename=SITE_ID, lon=LOCATION_LONG, lat=LOCATION_LAT,
    year_start=FLUX_MEASUREMENTS_DATE_START, year_end=FLUX_MEASUREMENTS_DATE_END, classid=IGBP
    )
  
  ## over-write data as numeric
  siteinfo$lon <- as.numeric( siteinfo$lon )
  siteinfo$lat <- as.numeric( siteinfo$lat )
  siteinfo$elv <- as.numeric( siteinfo$elv )
  siteinfo$year_start <- as.numeric( siteinfo$year_start )
  siteinfo$year_end   <- as.numeric( siteinfo$year_end   )
  
  ## "Manually" get year start and year end from file names
  # print( paste0("ingesting more data from files using ", settings_input$path_cx1data, "/FLUXNET-2015_Tier1/doc/filelist_DD.txt ..."))
  moredata <- as.data.frame( read.table( paste0( settings_input$path_cx1data, "/FLUXNET-2015_Tier1/doc/filelist_DD.txt") ) )
  colnames( moredata ) <- "filnam"
  moredata$mysitename <- substr( as.character(moredata$filnam), start=5, stop=10 )
  moredata$year_start <- substr( as.character(moredata$filnam), start=35, stop=38 )
  moredata$year_end   <- substr( as.character(moredata$filnam), start=40, stop=43 )

  missing_data_for_sites <- c()
  for (idx in seq(dim(siteinfo)[1])){
    tmp <- moredata[ which( as.character( as.character(siteinfo$mysitename[idx]) )==moredata$mysitename ), ]
    if (dim(tmp)[1]==0) {
      missing_data_for_sites <- c( missing_data_for_sites, as.character(siteinfo$mysitename[idx]) )
    } else {
      # print(paste("overwriting for site", tmp$mysitename," with year_start, year_end", tmp$year_start, tmp$year_end  ) )
      if (!is.na(tmp$year_start)) { siteinfo$year_start[idx] <- tmp$year_start }
      if (!is.na(tmp$year_end))   { siteinfo$year_end[idx]   <- tmp$year_end   }
    }
  }

  ## Some year_start and year_end data are given in a weird format (adding digits for months)
  ## Assume first 4 digits are representing the year, cut accordingly
  for (idx in seq(dim(siteinfo)[1])){
    if ( !is.na(siteinfo$year_start[idx]) ){
      if ( nchar( as.character( siteinfo$year_start[idx]) ) > 4 ) {
        siteinfo$year_start[idx] <- substr( as.character(siteinfo$year_start[idx]), start=1, stop=4 )
      }
    }
    if ( !is.na(siteinfo$year_end[idx])){
      if ( nchar( as.character(siteinfo$year_end[idx]) ) > 4 )   {
        siteinfo$year_end[idx]   <- substr( as.character(siteinfo$year_end[idx]), start=1, stop=4 )
      }
    }
  }

  # ## Exclude sites where not data is given (but just happen to appear in the meta info file)
  # siteinfo <- siteinfo[ !is.na(siteinfo$year_end), ]
  # siteinfo <- siteinfo[ !is.na(siteinfo$year_start), ]
  missing_metainfo_for_data <- c()
  for (idx in seq(dim(moredata)[1])){
    tmp <- siteinfo[ which( as.character( moredata$mysitename[idx] )==siteinfo$mysitename ), ]
    if (dim(tmp)[1]==0){
      missing_metainfo_for_data <- c( missing_metainfo_for_data, as.character(moredata$mysitename[idx]))
    }
  }

  ## Add number of years for which data is available
  siteinfo$years_data <- as.numeric( siteinfo$year_end ) - as.numeric( siteinfo$year_start ) + 1

  ## exclude sites for which no data is available
  siteinfo <- siteinfo[ which( !is.element( siteinfo$mysitename, missing_data_for_sites) ), ]

  ##--------------------------------------------------------------------
  ## Get C3/C4 information from an additional file
  ##--------------------------------------------------------------------
  filn <- paste0( settings_input$path_cx1data, "FLUXNET-2015_Tier1/siteinfo_fluxnet_sofun_withC3C4info.csv" )
  if (!file.exists(filn)){
    download_file_cx1(  path_remote = "/work/bstocker/labprentice/data/FLUXNET-2015_Tier1/siteinfo_fluxnet_sofun_withC3C4info.csv", 
                        path_local  = paste0( settings_input$path_cx1data, "FLUXNET-2015_Tier1/" )
                        )
  }

  # siteinfo <- read_delim( filn, delim = ";" ) %>%
  siteinfo <- read_csv( filn ) %>%
    dplyr::select( mysitename, type ) %>%
    right_join( siteinfo, by = "mysitename" )

  ## There was an error with mutate
  siteinfo$c4 <- ifelse( (siteinfo$type=="C4" | siteinfo$type=="C3C4" ), TRUE, FALSE )

  ##--------------------------------------------------------------------
  ## Add water holding capacity information
  ##--------------------------------------------------------------------
  filn <- paste0( settings_input$path_cx1data, "FLUXNET-2015_Tier1/siteinfo_fluxnet2015_sofun+whc.csv" )
  if (!file.exists(filn)){
    download_file_cx1(  path_remote = "/work/bstocker/labprentice/data/FLUXNET-2015_Tier1/siteinfo_fluxnet2015_sofun+whc.csv", 
                        path_local  = paste0( settings_input$path_cx1data, "FLUXNET-2015_Tier1/" )
                        )
  }

  siteinfo <- read_csv( filn ) %>%
              dplyr::select( mysitename, whc ) %>%
              right_join( siteinfo, by = "mysitename" )

  ##--------------------------------------------------------------------
  ## Add elevation information by reading from WATCH-WFDEI elevation map
  ##--------------------------------------------------------------------
  if (!is.na(filn_elv_watch)){

    siteinfo$elv_watch <- purrr::map_dbl( as.list(1:nrow(siteinfo)), ~get_pointdata_elv_watch( siteinfo$lon[.], siteinfo$lat[.], filn_elv_watch ) )
    siteinfo <- siteinfo %>% mutate( elv = ifelse( is.na(elv), elv_watch, elv ) )

  }

  # ##--------------------------------------------------------------------
  # ## Get more stations which are in LaThuile (free-fair-use - FFU) dataset but not in 2015
  # ##--------------------------------------------------------------------
  # dataFFU <- read_csv( paste0( settings_input$path_cx1data, "/gepisat/fluxdata_free-fair-use/Fluxdata_Meta-Data.csv") )
  # dataFFU_accurate <- read_csv( paste0(settings_input$path_cx1data, "/gepisat/fluxdata_free-fair-use/CommonAnc_LATEST.csv") )
  
  # ## rename variables (columns) where necessary
  # dataFFU <- dplyr::rename( dataFFU, c( elv=ele, mysitename=stationid ) )
  
  # for (idx in seq(dim(dataFFU)[1])){
    
  #   if ( !is.element( dataFFU$mysitename[idx], siteinfo$mysitename ) ){
    
  #     print( paste( "adding station", dataFFU$mysitename[idx] ) )
  #     tmp <- siteinfo[1,]

  #     tmpFFU <- dataFFU[idx,]
  #     tmpFFU_accurate <- dataFFU_accurate[ which(dataFFU_accurate$Site.ID==tmpFFU$mysitename), ]

  #     tmp$mysitename <- tmpFFU$mysitename
  #     tmp$lon <-        tmpFFU_accurate$Longitude
  #     tmp$lat <-        tmpFFU_accurate$Latitude
  #     tmp$elv <-        ifelse( (tmpFFU_accurate$Elevation!=" TBD"), tmpFFU_accurate$Elevation, tmpFFU$elv )
  #     tmp$year_start <- tmpFFU$year_start
  #     tmp$year_end   <- tmpFFU$year_end
  #     tmp$years_data <- tmpFFU$years_data
  #     tmp$classid    <- tmpFFU$classid

  #     siteinfo <- rbind( siteinfo, tmp )
  #   }
  # }
  # siteinfo <- siteinfo[ order(siteinfo$mysitename), ]

  # # ## Get more accurate lon/lat/elv info for some sites
  # # for (idx in seq(dim(siteinfo)[1])){
  # #   tmpFFU_accurate <- dataFFU_accurate[ which(dataFFU_accurate$Site.ID==siteinfo$mysitename[idx]), ]
  # #   if (dim(tmpFFU_accurate)[1]==1){
  # #     siteinfo$lon[idx] <- tmpFFU_accurate$Longitude
  # #     siteinfo$lat[idx] <- tmpFFU_accurate$Latitude
  # #     siteinfo$elv[idx] <- ifelse( !is.na(as.numeric(tmpFFU_accurate$Elevation)), tmpFFU_accurate$Elevation, siteinfo$elv[idx] )
  # #   }
  # # }

  # siteinfo$elv[ siteinfo$elv==-9999 ] <- NA
  # siteinfo$elv <- as.numeric( siteinfo$elv )

  ##--------------------------------------------------------------------
  ## write to file
  ##--------------------------------------------------------------------
  # print( paste0("Writing (light) meta info file: ", settings_sims$path_siteinfo ) )
  # print( "Full and light meta info is returned by this function as list." )
  light <- dplyr::select( siteinfo, mysitename, lon, lat, elv, year_start, year_end, years_data, classid, whc, c4 )
  light %>% write_csv( path = settings_sims$path_siteinfo )

  return( list( full = siteinfo, light = light ) )
}


