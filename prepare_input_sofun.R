##--------------------------------------------------------------------
## Creates a dataframe (tibble) with rows for each date (ymd object from 
## library lubridate) from 'yrstart' to 'yrend'. Intervals of dates is 
## specified by argument 'freq'. 
##--------------------------------------------------------------------
init_dates_dataframe <- function( yrstart, yrend, startmoy=1, startdoy=1, freq="days", endmoy=12, enddom=31, noleap=FALSE ){

  require(dplyr)
  require(lubridate)

  if (freq=="days"){
    startdate <- ymd( paste0( as.character(yrstart), "-", sprintf( "%02d", startmoy), "-01" ) ) + days( startdoy - 1 )
    enddate   <- ymd( paste0( as.character(yrend  ), "-", sprintf( "%02d", endmoy  ), "-", sprintf( "%02d", enddom  ) ) )    
  } else if (freq=="months"){
    ## date is always the 15th of each month
    startdate <- ymd( paste0( as.character(yrstart), "-", sprintf( "%02d", startmoy), "-15" ) )
    enddate   <- ymd( paste0( as.character(yrend  ), "-", sprintf( "%02d", endmoy  ), "-15" ) )    
  }

  ddf <-  tibble( date=seq( from = startdate, to = enddate, by = freq ) ) %>% 
          mutate( ndayyear = ifelse( leap_year(year(date)), 366, 365  ) ) %>%
          mutate( year_dec = year(date) + (yday(date) - 1) / ndayyear ) %>% 
          select( -ndayyear )

  if (noleap) ddf <- ddf %>% filter( !( month(date)==2 & mday(date)==29 ) )

  return( ddf )

}

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
get_meteo_fluxnet2015 <- function( sitename, path=NA, freq="d" ){

  require(dplyr)
  require(readr)
  require(lubridate)
  
  ## from flux to energy conversion, umol/J (Meek et al., 1984), same as used in SPLASH (see Eq.50 in spash_doc.pdf)
  kfFEC <- 2.04

  if (is.na(path)){

    if ( freq=="y" ){
      ## Annual data
      print( paste( "getting annual FLUXNET 2015 data for site", sitename ) )
      dirnam_obs <- paste0( path_cx1data, "/FLUXNET-2015_Tier1/20160128/point-scale_none_1y/original/unpacked/" )
      allfiles <- list.files( dirnam_obs )
      allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
      allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
      filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
      path <- paste0( dirnam_obs, filnam_obs ) 

    } else if ( freq=="m" ){
      ## Monthly data
      print( paste( "getting monthly FLUXNET 2015 data for site", sitename ) )
      dirnam_obs <- paste0( path_cx1data, "/FLUXNET-2015_Tier1/20160128/point-scale_none_1m/original/unpacked/" )
      allfiles <- list.files( dirnam_obs )
      allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
      allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
      filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
      path <- paste0( dirnam_obs, filnam_obs ) 

    } else if ( freq=="w" ){
      ## Weekly data
      print( paste( "getting weekly FLUXNET 2015 data for site", sitename ) )
      dirnam_obs <- paste0( path_cx1data, "/FLUXNET-2015_Tier1/20160128/point-scale_none_7d/original/unpacked/" )
      allfiles <- list.files( dirnam_obs )
      allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
      allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
      filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
      path <- paste0( dirnam_obs, filnam_obs ) 

    } else if ( freq=="d" ){
      ## Daily data
      print( paste( "getting annual FLUXNET 2015 data for site", sitename ) )
      dirnam_obs <- paste0( path_cx1data, "/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked/" )
      allfiles <- list.files( dirnam_obs )
      allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
      allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
      filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
      path <- paste0( dirnam_obs, filnam_obs ) 

    }

  }

  ## read data
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

  ## rename variables and unit conversions
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

  return( meteo )

}

##--------------------------------------------------------------------
## Extract monthly data from files for each year and attach to the 
## monthly dataframe (at the right location).
## Original data in K, returns data in K
##--------------------------------------------------------------------
get_pointdata_temp_wfdei <- function( lon, lat, mo, yr, ignore_leap=TRUE, path ){
  
  require(lubridate)
  ndaymonth_all <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymonth <- ndaymonth_all[mo]

  filn <- paste0( path, "/Tair_daily/Tair_daily_WFDEI_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )
  if ( file.exists( filn ) ){
    print( paste( "extracting from", filn ) )
    system( paste( "./extract_pointdata_byfil.sh ", filn, "Tair", "lon", "lat", sprintf("%.2f",lon), sprintf("%.2f",lat) ) )
    dtemp <- read.table( "out.txt" )$V1 - 273.15  # conversion from Kelving to Celsius
    if ( ignore_leap && mo==2 && length(dtemp==29) ){ dtemp <- dtemp[1:28] }
  } else {
    print(paste("get_pointdata_temp_wfdei(): file does not exist:", filn ))
    dtemp <- rep( NA, ndaymonth )
  }
  dates <- seq( from = ymd( paste0( as.character(yr), "-", as.character(mo), "-", sprintf( "%02d", 1  ) ) ),
                to   = ymd( paste0( as.character(yr), "-", as.character(mo), "-", sprintf( "%02d", ndaymonth  ) ) ),
                by   = "days"
                )
  ddf <- tibble( date=dates, temp_watch=dtemp )
  
  return( ddf )

}

##--------------------------------------------------------------------
## Extract monthly data from files for each year and attach to the 
## monthly dataframe (at the right location). 
## Original data in kg/m2s, returns data in kg/m2/month.
##--------------------------------------------------------------------
get_pointdata_prec_wfdei <- function( lon, lat, mo, yr, ignore_leap=TRUE, path ){

  require(lubridate)
  ndaymonth_all <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymonth <- ndaymonth_all[mo]
  
  ## rain
  filn <- paste0( path, "/Rainf_daily/Rainf_daily_WFDEI_CRU_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )
  if ( file.exists( filn ) ){
    print( paste( "extracting from", filn ) )
    system( paste( "./extract_pointdata_byfil.sh ", filn, "Rainf", "lon", "lat", sprintf("%.2f",lon), sprintf("%.2f",lat) ) )
    dprec <- read.table( "out.txt" )$V1
    dprec <- dprec*60*60*24 # kg/m2/s -> mm/day
  } else {
    # print( paste( "file", filn, "does not exist." ) )
    dprec <- rep( NA, ndaymonth )
  }
  # print( paste( "rain only: ", mprec))

  ## snow
  filn <- paste0( path, "/Snowf_daily/Snowf_daily_WFDEI_CRU_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )
  if ( file.exists( filn ) ){
    print( paste( "extracting from", filn ) )
    system( paste( "./extract_pointdata_byfil.sh ", filn, "Snowf", "lon", "lat", sprintf("%.2f",lon), sprintf("%.2f",lat) ) )
    dsnow <- read.table( "out.txt" )$V1
    dsnow <- dsnow*60*60*24 # kg/m2/s -> mm/day
    dprec <- dprec + dsnow
    # print( paste( "snow only: ", sum( dprec*60*60*24 )))
  }

  ## ignore leap years
  if ( ignore_leap && mo==2 && length(dprec==29) ){ dprec <- dprec[1:28] }

  dates <- seq( from = ymd( paste0( as.character(yr), "-", as.character(mo), "-", sprintf( "%02d", 1  ) ) ),
                to   = ymd( paste0( as.character(yr), "-", as.character(mo), "-", sprintf( "%02d", ndaymonth  ) ) ),
                by   = "days"
                )
  ddf <- tibble( date=dates, prec_watch=dprec )

  return( ddf )

}

##--------------------------------------------------------------------
## Extract monthly data from files for each year and attach to the 
## monthly dataframe (at the right location).
## Original data in K, returns data in K
##--------------------------------------------------------------------
get_pointdata_qair_wfdei <- function( lon, lat, mo, yr, ignore_leap=TRUE, path ){

  require(lubridate)
  ndaymonth_all <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymonth <- ndaymonth_all[mo]

  filn <- paste0( path, "/Qair_daily/Qair_daily_WFDEI_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )
  if ( file.exists( filn ) ){
    print( paste( "extracting from", filn ) )
    system( paste( "./extract_pointdata_byfil.sh ", filn, "Qair", "lon", "lat", sprintf("%.2f",lon), sprintf("%.2f",lat) ) )
    dqair <- read.table( "out.txt" )$V1
    if ( ignore_leap && mo==2 && length(dqair==29) ){ dqair <- dqair[1:28] }
  } else {
    dqair <- rep( NA, ndaymonth )
  }

  dates <- seq( from = ymd( paste0( as.character(yr), "-", as.character(mo), "-", sprintf( "%02d", 1  ) ) ),
                to   = ymd( paste0( as.character(yr), "-", as.character(mo), "-", sprintf( "%02d", ndaymonth  ) ) ),
                by   = "days"
                )
  ddf <- tibble( date=dates, qair_watch=dqair )

  return( ddf )

}

##--------------------------------------------------------------------
## Extract monthly data from files for each year and attach to the 
## monthly dataframe (at the right location).
## Original data in K, returns data in K
##--------------------------------------------------------------------
get_pointdata_ppfd_wfdei <- function( lon, lat, mo, yr, ignore_leap=TRUE, path ){
  
  require(lubridate)
  ndaymonth_all <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymonth <- ndaymonth_all[mo]

  ## conversion factor from SPLASH: flux to energy conversion, umol/J (Meek et al., 1984)
  kfFEC <- 2.04    

  filn <- paste0( path, "/SWdown_daily/SWdown_daily_WFDEI_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )
  if ( file.exists( filn ) ){
    print( paste( "extracting from", filn ) )
    system( paste( "./extract_pointdata_byfil.sh ", filn, "SWdown", "lon", "lat", sprintf("%.2f",lon), sprintf("%.2f",lat) ) )
    dswdown <- read.table( "out.txt" )$V1
    dppfd <- dswdown * kfFEC  # W m-2 -> umol s-1 m-2
    dppfd <- 1.0e-6 * dppfd * 60 * 60 * 24  # umol m-2 s-1 -> mol m-2 d-1
    if ( ignore_leap && mo==2 && length(dppfd==29) ){ dppfd <- dppfd[1:28] }
  } else {
    dppfd <- rep( NA, ndaymonth )
  }

  dates <- seq( from = ymd( paste0( as.character(yr), "-", as.character(mo), "-", sprintf( "%02d", 1  ) ) ),
                to   = ymd( paste0( as.character(yr), "-", as.character(mo), "-", sprintf( "%02d", ndaymonth  ) ) ),
                by   = "days"
                )
  ddf <- tibble( date=dates, ppfd_watch=dppfd )

  return( ddf )

}

##----------------------------------------------------------------   
## Calculates atm. pressure for a given elevation
## Ref:      Allen et al. (1998). Adopted from SPLASH.
## arguments: 
## elv : elevation above sea level, m
## patm : atmospheric pressure for a given elevation, Pa
##----------------------------------------------------------------   
calc_patm <- function( elv ){
  ## Parameters from SPLASH
  kPo = 101325  
  kL  = 0.0065  
  kTo = 288.15  
  kG  = 9.80665 
  kMa = 0.028963
  kR  = 8.3143  

  patm <- kPo*(1.0 - kL*elv/kTo)**(kG*kMa/(kR*kL))

  return( patm )

}

##-----------------------------------------------------------------------
## Output:   vapor pressure deficit, Pa (vpd)
## Features: Returns vapor pressure deficit
## Ref:      Eq. 5.1, Abtew and Meleese (2013), Ch. 5 Vapor Pressure 
##           Calculation Methods, in Evaporation and Evapotranspiration: 
##           Measurements and Estimations, Springer, London.
##             vpd = 0.611*exp[ (17.27 tc)/(tc + 237.3) ] - ea
##             where:
##                 tc = average daily air temperature, deg C
##                 eact  = actual vapor pressure, Pa
##-----------------------------------------------------------------------
## arguments
## eact  ## vapor pressure (Pa)
## qair  ## specific humidity (g g-1)
## tc    ## temperature, deg C
## tmin  ## (optional) min daily air temp, deg C 
## tmax  ## (optional) max daily air temp, deg C 
##
## function return variable
## vpd   ##  vapor pressure deficit, Pa  
##-----------------------------------------------------------------------
calc_vpd <- function( eact=NA, qair=NA, tc=NA, tmin=NA, tmax=NA, elv=NA ){

  kTo = 288.15   # base temperature, K (Prentice, unpublished)
  kR  = 8.3143   # universal gas constant, J/mol/K (Allen, 1973)
  kMv = 18.02    # molecular weight of water vapor, g/mol (Tsilingiris, 2008)
  kMa = 28.963   # molecular weight of dry air, g/mol (Tsilingiris, 2008)

  if ( !is.na(tmin) && !is.na(tmax) ) {

    my_tc <- 0.5 * (tmin + tmax)

  } else {

    my_tc <- tc

  }

  if (is.na(eact) && !is.na(qair) && !is.na(elv)){

    ## calculate the mass mising ratio of water vapor to dry air (dimensionless)
    wair <- qair / (1 - qair)

    ## calculate atmopheric pressure (Pa) assuming standard conditions at sea level (elv=0)
    patm <- calc_patm( elv )

    ## calculate water vapor pressure 
    rv <- kR / kMv
    rd <- kR / kMa
    eact = patm * wair * rv / (rd + wair * rv)

  }

  ## calculate saturation water vapour pressure in Pa
  esat <- 611.0 * exp( (17.27 * my_tc)/(my_tc + 237.3) )

  ## calculate VPD in units of Pa
  vpd <- ( esat - eact )    

  ## this empirical equation may lead to negative values for VPD (happens very rarely). assume positive...
  vpd <- max( 0.0, vpd )

  return( vpd )

}

##--------------------------------------------------------------------
## Extract monthly data from files for each year and attach to the 
## monthly dataframe (at the right location).
## Original data in K, returns data in K
##--------------------------------------------------------------------
get_pointdata_monthly_cru <- function( varnam, lon, lat, settings, yrend ){

  require(lubridate)
  require(dplyr)

  filn <- list.files( settings$path_cru, pattern=paste0( varnam, ".dat.nc" ) )

  if ( length( filn )!=0 ){

    cmd <- paste( "./extract_pointdata_byfil.sh ", paste0( settings$path_cru, filn ), varnam, "lon", "lat", sprintf("%.2f",lon), sprintf("%.2f",lat) )
    print( paste( "executing command:", cmd ) )
    system( cmd )
    mdata <- read.table( "./out.txt" )$V1
    mdf <-  init_dates_dataframe( 1901, yrend, freq="months" ) %>%
            mutate( mdata = mdata )

    if (is.na(mdf$mdata[1])) mdf <- "noland"            

  } else {

    mdf <- init_dates_dataframe( 1901, yrend, freq="months" ) %>%
           mutate( mdata = NA )

  }

  return( mdf )

}

##--------------------------------------------------------------------
## Finds the closest land cell in the CRU dataset at the same latitude
##--------------------------------------------------------------------
find_nearest_cruland_by_lat <- function( lon, lat, filn ){

  library(ncdf4)
  
  nc <- nc_open( filn, readunlim=FALSE )
  crufield <- ncvar_get( nc, varid="TMP" )
  lon_vec <- ncvar_get( nc, varid="LON" )
  lat_vec <- ncvar_get( nc, varid="LAT" )
  crufield[crufield==-9999] <- NA
  nc_close(nc)

  ilon <- which.min( abs( lon_vec - lon ) )
  ilat <- which.min( abs( lat_vec - lat ) )

  if (!is.na(crufield[ilon,ilat])) {print("WRONG: THIS SHOULD BE NA!!!")}
  for (n in seq(2*length(lon_vec))){
    ilon_look <- (-1)^(n+1)*round((n+0.1)/2)+ilon
    if (ilon_look > length(lon_vec)) {ilon_look <- ilon_look %% length(lon_vec)} ## Wrap search around globe in latitudinal direction
    if (ilon_look < 1)               {ilon_look <- ilon_look + length(lon_vec) }
    print(paste("ilon_look",ilon_look))
    if (!is.na(crufield[ilon_look,ilat])) {
      break
    }
  }
  # if (!is.na(crufield[ilon_look,ilat])) {print("SUCCESSFULLY FOUND DATA")}
  return( lon_vec[ ilon_look ] )
  
}

##-----------------------------------------------------------
## Downloads WATCH-WFDEI data from CX1
##-----------------------------------------------------------
download_watch_wfdei_from_cx1_path <- function( path, getfiles ){

  ## the path of WATCH-WFDEI daily data on cx1
  origpath <- "/work/bstocker/labprentice/data/watch_wfdei/"

  ## create required directory
  if (!dir.exists(path)) system( paste0("mkdir -p ", path ) )

  ## files are in sub-directories, determine them
  subdir <- getfiles %>% dirname() %>% unique()
  if (!dir.exists(paste0( path, "/", subdir ))) system( paste0("mkdir -p ", paste0( path, "/", subdir ) ) )

  if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto CX1: " )
  error <- purrr::map( as.list(getfiles[1]), ~system( paste0( "rsync -avz ", uname, "@login.cx1.hpc.ic.ac.uk:", origpath, ., " ", paste0( path, subdir ) ) ) )

  ## Show files in directory
  print( paste0("Files in directory: ", path) )
  print( list.files( path ) )

}


## Returns the file names of missing files for this year
check_watch_wfdei_year <- function( year, varnam, settings_input ){

  ## construct file names of all months' files (12 for each year)
  allfiles <- purrr::map_chr( as.list(sprintf("%02d", 1:12 )), ~paste0( varnam, "_daily/", varnam, "_daily_WFDEI_", as.character(year), ., ".nc" ) )
  avlfiles <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_", as.character(year), ".*.nc"), recursive = TRUE )

  getfiles <- allfiles[!(allfiles %in% avlfiles)]

  return(getfiles)

}

##-----------------------------------------------------------
## Manages the path specification for WATCH-WFDEI data download from CX1
##-----------------------------------------------------------
download_watch_wfdei_from_cx1 <- function( varnam, settings_input, settings_sims ){

  require(rlang)

  ## determine simulation years ocurring in this ensemble
  allyears <- seq( from = purrr::map_dbl( settings_sims$date_start, ~year(.) ) %>% min(),
                   to   = purrr::map_dbl( settings_sims$date_start, ~year(.) ) %>% max(),
                   by   = 1 ) %>% as.list

  ## Determine missing files for this variable, given start and end years of all the simulations in this ensemble
  getfiles <- purrr::map( allyears, ~check_watch_wfdei_year( ., varnam, settings_input ) ) %>% unlist()
  
  ## Interactive part
  ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
  if (ans=="y"){
    ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
    if (ans=="y"){
      ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_watch_wfdei, "? (y/n)") )
      if (ans=="y"){
        error <- download_watch_wfdei_from_cx1_path( settings_input$path_watch_wfdei, getfiles )
      } else {
        path <- readline( prompt = "Please specify a new path: " )
        settings_input$path_watch_wfdei <- path
        error <- download_watch_wfdei_from_cx1_path( settings_input$path_watch_wfdei, getfiles )
      }
    } else {
      abort( "WATCH-WFDEI data download not possible.")
    }
  } else {
    abort( "WATCH-WFDEI data download not possible.")
  }

  return(error)

}

##-----------------------------------------------------------
## Downloads CRU TS 4.01 data from CX1
##-----------------------------------------------------------
download_cru_from_cx1_filn <- function( varnam, settings_input, filn ){

  origpath <- "/work/bstocker/labprentice/data/cru/ts_4.01/"
  filn <-  paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc")
  if (!dir.exists(settings_input$path_cru_ts4_01)) system(paste0("mkdir -p ", settings_input$path_cru_ts4_01))
  if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto CX1: " )
  system( paste0( "rsync -avz ", uname, "@login.cx1.hpc.ic.ac.uk:", origpath, filn, " ", settings_input$path_cru_ts4_01 ) )

  return(NULL)
}

##-----------------------------------------------------------
## Manages the path specification for CRU TS 4.01 data download from CX1
##-----------------------------------------------------------
download_cru_from_cx1 <- function( varnam, settings_input ){

  require(rlang)

  ## the path of CRU TS 4.01 data on cx1
  origpath <- "/work/bstocker/labprentice/data/cru/ts_4.01/" 
  filn <-  paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc")
  
  ## Interactive part
  ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
  if (ans=="y"){
    ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
    if (ans=="y"){
      ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_cru_ts4_01, "? (y/n)") )
      if (ans=="y"){
        error <- download_cru_from_cx1_filn( varnam, settings_input, filn = filn )
      } else {
        path <- readline( prompt = "Please specify a new path: " )
        settings_input$path_cru_ts4_01 <- path
        error <- download_cru_from_cx1_filn( varnam, settings_input, filn = filn )
      }
    } else {
      abort( "CRU TS 4.01 data download not possible.")
    }
  } else {
    abort( "CRU TS 4.01 data download not possible.")
  }

  return(error)

}

##--------------------------------------------------------------------------
## Checks if FLUXNET 2015 files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_fluxnet2015 <- function( settings_input, settings_sims, sitename=NA ){

  require(purrr)
  require(dplyr)
  require(rlang)

  ## Check if any data is available in the specified directory
  filelist <- list.files( settings_input$path_fluxnet2015, pattern = "FLX_.*_FLUXNET2015_FULLSET_DD.*.csv" )

  if (length(filelist)==0){
    ## No files found at specified location
    warn( paste0("No files found for fluxnet2015 in directory ", settings_input$path_fluxnet2015) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a certain directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = "FLX_.*_FLUXNET2015_FULLSET_DD.*.csv", recursive = TRUE )

    if (length(filelist)==0){
     
      ## Search from home
      warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      if (ans=="y"){
        filelist <- list.files( "~/", pattern = "FLX_.*_FLUXNET2015_FULLSET_DD.*.csv", recursive = TRUE )
      } else {
        ## Still no files found at specified location. Try to download from Imperial CX1 and place in <settings_input$path_fluxnet2015>
        warn( "Initiating download from Imperial CX1..." )
        error <- download_fluxnet2015_from_cx1( settings_input )
        filelist <- list.files( settings_input$path_fluxnet2015, pattern = "FLX_.*_FLUXNET2015_FULLSET_DD.*.csv" )
      }

      if (length(filelist)==0){
        ## Still no files found at specified location. Try to download from Imperial CX1 and place in <settings_input$path_fluxnet2015>
        warn( "Initiating download from Imperial CX1..." )
        error <- download_fluxnet2015_from_cx1( settings_input )

      }

    }

  }

  if (!is.na(sitename)){
    ## Check if a file is available for a given site
    filelist <- list.files( settings_input$path_fluxnet2015, pattern = paste0("FLX_", sitename, "_FLUXNET2015_FULLSET_DD.*.csv") )

    if (length(filelist)==0){
      ## Download missing file
      error <- download_fluxnet2015_from_cx1_path( settings_input$path_fluxnet2015, sitename )
    }

  }

}

##--------------------------------------------------------------------------
## Checks if WATCH-WFDEI files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_watch_wfdei <- function( varnam, settings_input, settings_sims ){

  require(purrr)
  require(dplyr)
  require(rlang)

  ## Determine file name, given <settings_input$path_fluxnet2015>
  ## look for data in the given directory
  filelist <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )

  if (length(filelist)==0){

    ## No files found at specified location
    warn( paste0("No files found for WATCH-WFDEI in directory ", settings_input$path_watch_wfdei) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a different directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )

    if (length(filelist)==0){
     
      ## Search from home
      warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      
      if (ans=="y"){
      
        filelist <- list.files( "~/", pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )
      
      } else {
        
        ## Still no files found at specified location. Try to download from Imperial CX1 and place in <settings_input$path_watch_wfdei>
        warn( "Initiating download from Imperial CX1..." )
        error <- download_watch_wfdei_from_cx1( varnam, settings_input, settings_sims )
        filelist <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )
      }

      if (length(filelist)==0){
        ## Still no files found at specified location. Try to download from Imperial CX1 and place in <settings_input$path_watch_wfdei>
        warn( "Initiating download from Imperial CX1..." )
        error <- download_watch_wfdei_from_cx1( varnam, settings_input, settings_sims )

      }

    }

  } 

  ## Check if files are now available at specified location.
  filelist <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )
  if (length(filelist)==0) abort("Download of FLUXNET-2015_Tier1 data was not successful. No files found.")
  
}

##--------------------------------------------------------------------------
## Checks if CRU TS 4.01 files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_cru_ts4_01 <- function( varnam, settings_input, settings_sims ){

  require(purrr)
  require(dplyr)
  require(rlang)

  ## Determine file name, given <settings_input$path_fluxnet2015>
  ## look for data in the given directory
  filelist <- list.files( settings_input$path_cru_ts4_01, pattern = paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc") )

  if (length(filelist)==0){

    ## No files found at specified location
    warn( paste0("No files found for CRU TS 4.01 in directory ", settings_input$path_cru_ts4_01) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a different directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc") )

    if (length(filelist)==0){
     
      ## Search from home
      warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      
      if (ans=="y"){
      
        filelist <- list.files( "~/", pattern = paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc") )
      
      } else {
        
        ## Still no files found at specified location. Try to download from Imperial CX1 and place in <settings_input$path_cru_ts4_01>
        warn( "Initiating download from Imperial CX1..." )
        error <- download_cru_from_cx1( varnam, settings_input )
        filelist <- list.files( settings_input$path_cru_ts4_01, pattern = paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc") )
      }

      if (length(filelist)==0){
        ## Still no files found at specified location. Try to download from Imperial CX1 and place in <settings_input$path_cru_ts4_01>
        warn( "Initiating download from Imperial CX1..." )
        error <- download_cru_from_cx1( varnam, settings_input )

      }

    }

  } 

  ## Check if files are now available at specified location.
  filelist <- list.files( settings_input$path_cru_ts4_01, pattern = paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc") )
  if (length(filelist)==0) abort("Download of CRU TS 4.01 data was not successful. No files found.")
  
}


##--------------------------------------------------------------------------
## Checks if MODIS_FPAR_MCD15A3H files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_MODIS_FPAR_MCD15A3H <- function( settings_input, settings_sims, sitename=NA ){

  require(purrr)
  require(dplyr)
  require(rlang)

  error <- 0

  ## Determine file name, given <settings_input$path_MODIS_FPAR_MCD15A3H>
  ## look for data for this site in the given directory
  filelist <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = "dfapar_MODIS_FPAR_MCD15A3H_.*_gee_subset.csv" )

  if (length(filelist)==0){

    ## No files found at specified location
    warn( paste0("No files found for MODIS_FPAR_MCD15A3H in directory ", settings_input$path_MODIS_FPAR_MCD15A3H) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a certain directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = "dfapar_MODIS_FPAR_MCD15A3H_.*_gee_subset.csv", recursive = TRUE )

    if (length(filelist)==0){
     
      ## Search from home
      warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      if (ans=="y"){
        filelist <- list.files( "~/", pattern = "dfapar_MODIS_FPAR_MCD15A3H_.*_gee_subset.csv", recursive = TRUE )
      } else {
        ## Still no files found at specified location. Try to download from Imperial CX1 and place in <settings_input$path_MODIS_FPAR_MCD15A3H>
        warn( "Initiating download from Imperial CX1..." )
        error <- download_MODIS_FPAR_MCD15A3H_from_cx1( settings_input )
        filelist <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = "dfapar_MODIS_FPAR_MCD15A3H_.*_gee_subset.csv" )
      }

      if (length(filelist)==0){
        ## Still no files found at specified location. Try to download from Imperial CX1 and place in <settings_input$path_MODIS_FPAR_MCD15A3H>
        warn( "Initiating download from Imperial CX1..." )
        error <- download_MODIS_FPAR_MCD15A3H_from_cx1( settings_input )

      }

    }

  }

  if (!is.na(sitename)){
    ## Check if a file is available for a given site
    filelist <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = paste0("dfapar_MODIS_FPAR_MCD15A3H_", sitename, "_gee_subset.csv") )

    if (length(filelist)==0){
      ## Download missing file
      error <- download_MODIS_FPAR_MCD15A3H_from_cx1_path( settings_input$path_MODIS_FPAR_MCD15A3H, sitename )
    }

  }
  return( error )
}

##--------------------------------------------------------------------------
## Checks if CMIP CO2 files are available and initiates download if not.
##--------------------------------------------------------------------------
check_download_cmip_co2 <- function( settings_input, settings_sims, sitename=NA ){

  require(purrr)
  require(dplyr)
  require(rlang)

  error <- 0

  ## Determine file name, given <settings_input$path_cx1data>/co2
  ## look for data for this site in the given directory
  filn <- "cCO2_rcp85_const850-1765.dat"
  localdir <- paste0( settings_input$path_cx1data, "/co2/")
  filelist <- list.files( localdir, pattern = filn )

  if (length(filelist)==0){

    ## get user name from user
    if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto CX1: " )

    origpath <- "/work/bstocker/labprentice/data/co2/"

    ## No files found at specified location
    warn( paste0("Downloading files for CMIP CO2 into directory: ", localdir ) )
    system( paste0( "rsync -avz ", uname, "@login.cx1.hpc.ic.ac.uk:", origpath, filn, " ", localdir ) )

  }

  if (!is.na(sitename)){
    ## Check if a file is available for a given site
    localdir_bysite <- paste0( settings_sims$path_input, "/sitedata/co2/", sitename )
    if (!dir.exists(localdir_bysite)) system( paste0("mkdir -p ", localdir_bysite ) )
    filelist <- list.files( localdir_bysite, pattern = filn )

    if (length(filelist)==0){
      ## link file into site-level directory
      system( paste0( "ln -svf ", localdir, filn, " ", localdir_bysite, "/" ) )
    }

  }
  return( error )
}

##--------------------------------------------------------------------
## Function returns daily data frame with columns for watch data 
## (temp_watch, prec_watch, vpd_qair_watch_temp_watch, [ppfd_watch])
##--------------------------------------------------------------------
get_watch_daily <- function( lon, lat, elv, date_start, date_end, settings_input ){

  require(dplyr)
  require(lubridate)
  require(purrr)

  ## WATCH-WFDEI data is available monthly. Create vector with all required months.
  bymonths <- seq( date_start, date_end, by = "months" )

  ## extract temperature data
  ddf_temp <- purrr:map( as.list(bymonths), ~get_pointdata_temp_wfdei( lon, lat, month(.), year(.), ignore_leap=TRUE, path=settings_input$path_watch_wfdei ) ) %>%
              bind_rows()

  ## extract precipitation data (sum of snowfall and rainfall)
  ddf_prec <- purrr:map( as.list(bymonths), ~get_pointdata_prec_wfdei( lon, lat, month(.), year(.), ignore_leap=TRUE, path=settings_input$path_watch_wfdei ) ) %>%
              bind_rows()

  ## extract PPFD data
  ddf_ppfd <- purrr:map( as.list(bymonths), ~get_pointdata_ppfd_wfdei( lon, lat, month(.), year(.), ignore_leap=TRUE, path=settings_input$path_watch_wfdei ) ) %>%
              bind_rows()

  ## extract air humidity data and calculate VPD based on temperature
  ddf_qair <- purrr:map( as.list(bymonths), ~get_pointdata_qair_wfdei( lon, lat, month(.), year(.), ignore_leap=TRUE, path=settings_input$path_watch_wfdei ) ) %>%
              bind_rows() %>%
              ## merge temperature data in here for VPD calculation
              left_join( ddf_temp, by = "date" ) %>%
              ## calculate VPD
              mutate( vpd_qair_watch_temp_watch = calc_vpd( qair=qair_watch, tc=temp_watch, elv=elv ) ) %>%
              ## drop temperature data again to avoid duplication
              dplyr::select( -temp_watch )


  ## merge all data frames
  ddf <- ddf_temp %>% left_join( ddf_prec, by = c("date", "year_dec") ) %>%
                      left_join( ddf_qair, by = c("date", "year_dec") ) %>%
                      left_join( ddf_ppfd, by = c("date", "year_dec") )


  return( ddf )

}


##--------------------------------------------------------------------
## Get monthly data from CRU
##--------------------------------------------------------------------
get_clim_cru_monthly <- function( lon, lat, settings, cruvars ){

  require(dplyr)
  require(lubridate)

  ## get last year for which data is available
  filn <- list.files( settings$path_cru_ts4_01, pattern="cld.dat.nc")
  start <- regexpr( 20, filn)[1]
  stop <- start + 3
  yrend <- substr( filn, start, stop ) %>% as.numeric %>% ifelse( length(.)==0, 2010, . )

  ## cloud cover
  mdf_ccov <- get_pointdata_monthly_cru( "cld", lon, lat, settings, yrend=yrend )

  ## Check if data is available at that location, otherwise use nearest gridcell
  if (!is.data.frame(mdf_ccov)){
    lon_look <- find_nearest_cruland_by_lat( lon, lat, paste0( settings$path_cru, filn ) )
    mdf_ccov <- get_pointdata_monthly_cru( "cld", lon_look, lat, settings, yrend=yrend )
  } else {
    lon_look <- lon
  }
  mdf_ccov <- mdf_ccov %>% rename( ccov_cru = mdata )    
  mdf <- mdf_ccov    

  ## precipitation
  if ("prec" %in% cruvars){
    mdf_prec <- get_pointdata_monthly_cru( "pre", lon_look, lat, settings, yrend=yrend ) %>% rename( prec_cru = mdata )
    mdf <- mdf %>% left_join( mdf_prec, by = c("date", "year_dec") )
  }

  ## wet days
  if ("wetd" %in% cruvars){
    mdf_wetd <- get_pointdata_monthly_cru( "wet", lon_look, lat, settings, yrend=yrend ) %>% rename( wetd_cru = mdata )
    mdf <- mdf %>% left_join( mdf_wetd, by = c("date", "year_dec") )
  }

  ## air temperature
  if ("temp" %in% cruvars){
    mdf_temp <- get_pointdata_monthly_cru( "tmp", lon_look, lat, settings, yrend=yrend ) %>% rename( temp_cru = mdata )
    mdf <- mdf %>% left_join( mdf_temp, by = c("date", "year_dec") )
  }

  ## VPD 
  ## calculated as a function of vapour pressure and temperature, vapour
  ## pressure is given by CRU data.
  if ("vap" %in% cruvars){
    mdf_vap <-  get_pointdata_monthly_cru( "vap", lon_look, lat, settings, yrend=yrend ) %>%  
                rename( vap_cru = mdata ) %>%
                ## merge temperature data in here for VPD calculation
                left_join( mdf_temp, by =  c("date", "year_dec") ) %>%
                ## calculate VPD (vap is in hPa)
                mutate( vpd_vap_cru_temp_cru = calc_vpd( eact=1e2*vap_cru, tc=temp_cru ) ) %>% 
                ## avoid duplicate 
                select( -temp_cru )
    mdf <- mdf %>% left_join( mdf_vap, by = c("date", "year_dec") )
  }


  return( mdf )

}


##--------------------------------------------------------------------
## Interpolates monthly data to daily data using polynomials or linear
## for a single year
##--------------------------------------------------------------------
expand_clim_cru_monthly_byyr <- function( yr, mdf, cruvars ){

  require(dplyr)
  require(lubridate)

  nmonth <- 12

  startyr_cru <- year(mdf$date) %>% unique() %>% first()
  endyr_cru   <- year(mdf$date) %>% unique() %>% last()
  
  yr_pvy <- max(startyr_cru, yr-1)
  yr_nxt <- min(endyr_cru, yr+1)

  ## add first and last year to head and tail of 'mdf'
  first <- mdf[1:12,] %>% select( -year_dec ) %>% mutate( date = date - years(1) )
  last  <- mdf[(nrow(mdf)-11):nrow(mdf),] %>% select( -year_dec ) %>% mutate( date = date + years(1) )

  ddf <- init_dates_dataframe( yr, yr )

  ##--------------------------------------------------------------------
  ## air temperature: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("temp" %in% cruvars){
    mtemp     <- dplyr::filter( mdf, year(date)==yr     )$temp_cru
    mtemp_pvy <- dplyr::filter( mdf, year(date)==yr_pvy )$temp_cru
    mtemp_nxt <- dplyr::filter( mdf, year(date)==yr_nxt )$temp_cru
    if (length(mtemp_pvy)==0){
      mtemp_pvy <- mtemp
    }
    if (length(mtemp_nxt)==0){
      mtemp_nxt <- mtemp
    }

    ddf_temp <- init_dates_dataframe( yr, yr ) %>%
                mutate( temp_cru_int = monthly2daily( mtemp, "polynom", mtemp_pvy[nmonth], mtemp_nxt[1], leapyear = leap_year(yr) ) )
    ddf <- ddf %>% left_join( ddf_temp, by = c("date", "year_dec") )
  }

  ##--------------------------------------------------------------------
  ## precipitation: interpolate using weather generator
  ##--------------------------------------------------------------------
  if ("prec" %in% cruvars){
    mprec <- dplyr::filter( mdf, year(date)==yr )$prec_cru
    mwetd <- dplyr::filter( mdf, year(date)==yr )$wetd_cru

    ddf_prec <- init_dates_dataframe( yr, yr)
    if (any(!is.na(mprec))&&any(!is.na(mwetd))){
      ddf_prec <- ddf_prec %>% mutate( prec_cru_gen = get_daily_prec( mprec, mwetd, leapyear = leap_year(yr) ) )
    }
    ddf <- ddf %>% left_join( ddf_prec, by = c("date", "year_dec") )
  }

  ##--------------------------------------------------------------------
  ## cloud cover: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("ccov" %in% cruvars){
    mccov     <- dplyr::filter( mdf, year(date)==yr     )$ccov_cru
    mccov_pvy <- dplyr::filter( mdf, year(date)==yr_pvy )$ccov_cru
    mccov_nxt <- dplyr::filter( mdf, year(date)==yr_nxt )$ccov_cru
    if (length(mccov_pvy)==0){
      mccov_pvy <- mccov
    }
    if (length(mccov_nxt)==0){
      mccov_nxt <- mccov
    }

    ddf_ccov <- init_dates_dataframe( yr, yr ) %>%
                mutate( ccov_cru_int = monthly2daily( mccov, "polynom", mccov_pvy[nmonth], mccov_nxt[1], leapyear = leap_year(yr) ) )

    ## Reduce CCOV to a maximum 100%
    ddf_ccov$ccov_cru_int[ ddf_ccov$ccov_cru_int>100.0 ] <- 100.0
    ddf <- ddf %>% left_join( ddf_ccov, by = c("date", "year_dec") )
  }

  ##--------------------------------------------------------------------
  ## VPD: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("vap" %in% cruvars){
    mvpd     <- dplyr::filter( mdf, year(date)==yr     )$vpd_vap_cru_temp_cru
    mvpd_pvy <- dplyr::filter( mdf, year(date)==yr_pvy )$vpd_vap_cru_temp_cru
    mvpd_nxt <- dplyr::filter( mdf, year(date)==yr_nxt )$vpd_vap_cru_temp_cru
    if (length(mvpd_pvy)==0){
      mvpd_pvy <- mvpd
    }
    if (length(mvpd_nxt)==0){
      mvpd_nxt <- mvpd
    }

    ddf_vpd <- init_dates_dataframe( yr, yr ) %>%
               mutate( vpd_vap_cru_temp_cru_int = monthly2daily( mvpd, "polynom", mvpd_pvy[nmonth], mvpd_nxt[1], leapyear = (yr %% 4 == 0) ) )
    ddf <- ddf %>% left_join( ddf_vpd, by = c("date", "year_dec") )
  }

  return( ddf )

}

##--------------------------------------------------------------------
## Interpolates monthly data to daily data using polynomials or linear
## for a single year
##--------------------------------------------------------------------
expand_clim_cru_monthly <- function( mdf, cruvars ){

  require(dplyr)
  require(purrr)

  ddf_yr_list <- purrr::map( as.list( unique( year( mdf$date ) ) ), ~expand_clim_cru_monthly_byyr( ., mdf, cruvars ) )

  ddf <- bind_rows( ddf_yr_list )

  return( ddf )

}


monthly2daily <- function( mval, method="polynom", mval_prev=mval[nmonth], mval_next=mval[1], leapyear=FALSE ){

  # mval <- 20*sin( seq(0, 2*pi, 2*pi/11)-0.5*pi)
  # mval_prev <- mval[12]
  # mval_next <- mval[1]

  if (leapyear){
    ndaymonth <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  } else {
    ndaymonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  }
  nmonth <- length(ndaymonth)
  dval <- rep(NA,sum(ndaymonth))

  if (method=="polynom"){

    # Starting conditons of december in previous year
    startt <- -30.5            # midpoint between Nov-Dec of previous year
    endt <- 0.5                # midpoint between Dec-Jan of this year
    dt <- 31.0                 # number of Dec days
    lastmonthtemp <- mval_prev # Dec mean temperature
    day <- 0                   # initialisation of this years days

    for (month in 1:nmonth){
      dtold <- dt
      dt <- (ndaymonth[month])
      startt <- endt
      endt <- endt + dt
      if (month<nmonth) {
        dtnew <- (ndaymonth[month+1])
        nextmonthtemp <- mval[month+1]
      } else {
        dtnew <- (ndaymonth[1])
        nextmonthtemp <- mval_next
      }

      starttemp <- (mval[month]*dt+lastmonthtemp*dtold)/(dt+dtold)
      endtemp <- (nextmonthtemp*dtnew+mval[month]*dt)/(dtnew+dt)
      deltatemp <- endtemp-starttemp
      
      # calculate vars for a,b,c coefficients in polynom y <- ax^2 +bx + c
      d2t <- endt^2.0 - startt^2.0
      d3t <- endt^3.0 - startt^3.0

      # Take a sheet of paper and try solve the polynom, well here is the outcome
      polya <- (mval[month]*dt - deltatemp*d2t/dt/2.0 - starttemp*dt + deltatemp*startt) / (d3t/3.0 - d2t^2.0/dt/2.0 - dt*startt^2.0 + startt*d2t)
      polyb <- deltatemp/dt - polya*(startt+endt)
      polyc <- starttemp - polya*startt^2.0 - polyb*startt

      # calculate daily values with the polynom function
      for (d in 1:ndaymonth[month]) {
        day <- day + 1
        dval[day] <- polya*(day)^2.0 + polyb*(day) + polyc
      }
      lastmonthtemp <- mval[month]
    }

    # calculate monthly means after interpolation - not absolutely identical to input
    mtempint <- rep(NA,nmonth)
    day <- 0
    for (m in 1:nmonth){
      mtempint[m] <- 0.0
      for (d in 1:ndaymonth[m]){
        day <- day + 1
        mtempint[m] <- mtempint[m]+dval[day]/(ndaymonth[m])
      }
    }

  } else if (method=="step"){

    dval[] <- rep( mval, times=ndaymonth )

  } else {
    print( "Method (2nd argument) not valid." )
  }

  return(dval) 

}


get_daily_prec <- function( mval_prec, mval_wet, set_seed=FALSE, leapyear=FALSE ){
  #--------------------------------------------------------------------
  # Distributes monthly total precipitation to days, given number of 
  # monthly wet days. Adopted from LPX.
  #--------------------------------------------------------------------
  if (leapyear){
    ndaymonth <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  } else {
    ndaymonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  }
  ndayyear <- sum(ndaymonth)
  nmonth <- length(ndaymonth)

  c1 <- 1.0
  c2 <- 1.2

  if (set_seed) {set.seed(0)}
  prdaily_random <- array( NA, dim=c(ndayyear,2))
  for (doy in 1:ndayyear){
    prdaily_random[doy,] <- runif(2)
  }

  dval_prec <- rep(NA,ndayyear)
  doy <- 0
  prob <- 0.0
  prob_rain <- rep(NA,nmonth)
  mprecave <- rep(NA,nmonth)
  mprecip <- rep(NA,nmonth)
  for (moy in 1:nmonth){
    prob_rain[moy] <- 0.0
    mprecave[moy] <- 0.0
    mprecip[moy] <- 0.0      
  }
  daysum <- 0

  set.seed( prdaily_random[1,1] * 1e7 )
 
  for (moy in 1:nmonth){
    if ( mval_wet[moy]<=1.0 ) {mval_wet[moy] <- 1.0}
    prob_rain[moy] <- mval_wet[moy] / ndaymonth[moy]
    mprecave[moy] <- mval_prec[moy] / mval_wet[moy]
    dry <- TRUE
    iloop <- 0


    while( dry ){
      iloop <- iloop + 1
      nwet <- 0
      for (dm in 1:ndaymonth[moy]){
        doy <- doy + 1
 
        # Transitional probabilities (Geng et al. 1986)
        if (doy>1) {
          if (dval_prec[doy-1] < 0.1) {
            prob <- 0.75 * prob_rain[moy]
          } else { 
            prob <- 0.25 + (0.75 * prob_rain[moy])
          }
        }        
        # Determine we randomly and use Krysanova / Cramer estimates of 
        # parameter values (c1,c2) for an exponential distribution
        if (iloop==1) { 
          vv <- prdaily_random[doy,1]
        } else {
          # xxx problem: rand() generates a random number that leads to floating point exception
          vv <- runif(1)
        }
    
    
        if (vv>prob) {        
          dval_prec[doy] <- 0.0
        } else {        
          nwet <- nwet + 1        
          v1 <- prdaily_random[doy,2]        
          dval_prec[doy] <- ((-log(v1))^c2) * mprecave[moy] * c1         
          if (dval_prec[doy] < 0.1) dval_prec[doy] <- 0.0        
        }
    
        mprecip[moy] <- mprecip[moy] + dval_prec[doy]
      }
  
      # If it never rained this month and mprec[moy]>0 and mval_wet[moy]>0, do
      # again
      dry <- (nwet==0 && iloop<50 && mval_prec[moy]>0.1)
      if (iloop>50) {
        print('Daily.F, prdaily: Warning stopped after 50 tries in cell')
      }

      # Reset counter to start of month          
      if (dry) {
        doy <- doy - ndaymonth[moy]
      }

    } #while

    # normalise generated precipitation by monthly CRU values
    if ( moy > 1 ) {daysum <- daysum + ndaymonth[moy-1]}
    if ( mprecip[moy] < 1.0 ) {mprecip[moy] <- 1.0}
    for (dm in 1:ndaymonth[moy]){
      doy <- daysum + dm
      dval_prec[doy] <- dval_prec[doy] * (mval_prec[moy] / mprecip[moy])
      if ( dval_prec[doy] < 0.1 ) {dval_prec[doy] <- 0.0}
      # dval_prec[doy] <- mval_prec[moy] / ndaymonth[moy]  #no generator
    }
       
    # Alternative: equal distribution of rain for fixed number of wet days
    # prob <- prob_rain[moy] + prob
    # if (prob.ge.1.0) then   
    #   dval_prec[doy] <- mprec[moy]
    #   prob <- prob-1.0
    # } else {
    #   dval_prec[doy] <- 0.0
    #   prob <- prob
    # }
                    
  } 
  
  return( dval_prec )
  
}

##-----------------------------------------------------------
## fills gaps (NAs) by (1.) linear interpolation, (2.) extending first/last to head/tail
##-----------------------------------------------------------
fill_gaps <- function( vec, is.prec = FALSE ){
  
  xvals <- seq(length(vec))

  if ( is.prec ){
    ## assume precipitation = 0 where missing
    if (any(is.na(vec))){
      vec[ is.na(vec) ] <- 0.0
    }

  } else {
    ## linear approximation
    if ( any(is.na(vec)) && any(!is.na(vec)) ){
        vec <- approx( xvals, vec, xout=xvals )$y
    }

    ## extend to missing in head and tail
    if ( any(is.na(vec))  && any(!is.na(vec)) ){
      for (idx in seq(length(vec))){
        if ( any( is.na( tail( vec, n=idx ) ) ) && any( !is.na( tail( vec, n=(idx+1) ) ) ) ){
          vec[ (length(vec)-idx+1):length(vec) ] <- vec[ (length(vec)-idx) ]
          break
        }
      }
    }  

  }

  return( vec )

}


## write output with standard formatting
write_sofunformatted <- function( filnam, data ){

  if ( is.vector( data ) ){
    len <- length(data)
    formatted <- vector( "character", len )
    for (i in 1:len){
      formatted[i] <- sprintf("%16.6f", data[i] )
    }
    writeLines( formatted, filnam )
  } else if ( is.data.frame( data ) && length( dim( data ) )==2 ){
    len <- dim( data )[1]
    formatted <- vector( "character", len )
    for (i in 1:len){
      formatted[i] <- sprintf("%16.6f    %f", data[i,1], data[i,2] )
    }
    writeLines( formatted, filnam )    
  }

}


##-----------------------------------------------------------
## Downloads fluxnet data from CX1
##-----------------------------------------------------------
download_fluxnet2015_from_cx1_path <- function( path, sitename=NA ){

  ## get user name from user
  if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto CX1: " )

  ## the path of fluxnet daily data on cx1
  origpath <- "/work/bstocker/labprentice/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked/"

  if (is.na(sitename)){
    ## Download the whole bunch

    ## create required directory
    if (!dir.exists(path)) system( paste0("mkdir -p ", path ) )
    system( paste0( "rsync -avz ", uname, "@login.cx1.hpc.ic.ac.uk:", origpath, " ", path ) )

  } else {
    ## Download only data for a specific site
    ## get a file list of what's on CX1
    filelist <- system( paste0( "ssh ", uname, "@login.cx1.hpc.ic.ac.uk ls ", origpath ), intern = TRUE )

    ## use one file(s) for this site
    filelist <- filelist[ grepl(sitename, filelist) ]
    filelist <- filelist[ grepl("_FLUXNET2015_FULLSET_DD_", filelist) ]

    purrr::map( as.list(filelist), ~system( paste0( "rsync -avz ", uname, "@login.cx1.hpc.ic.ac.uk:", origpath, .," ", path ) ) )
    
  }

}

##-----------------------------------------------------------
## Manages the path specification for fluxnet data download from CX1
##-----------------------------------------------------------
download_fluxnet2015_from_cx1 <- function( settings_input ){
  
  require(rlang)

  ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
  if (ans=="y"){
    ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
    if (ans=="y"){
      ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_fluxnet2015, "? (y/n)") )
      if (ans=="y"){
        error <- download_fluxnet2015_from_cx1_path( settings_input$path_fluxnet2015 )
      } else {
        path <- readline( prompt = "Please specify a new path: " )
        settings_input$path_fluxnet2015 <- path
        error <- download_fluxnet2015_from_cx1_path( settings_input$path_fluxnet2015 )
      }
    } else {
      abort( "FLUXNET 2015 data download not possible.")
    }
  } else {
    abort( "FLUXNET 2015 data download not possible.")
  }

  return(error)

}

##-----------------------------------------------------------
## Downloads MODIS FPAR data from CX1
##-----------------------------------------------------------
download_MODIS_FPAR_MCD15A3H_from_cx1_path <- function( path, sitename=NA ){

  error <- 0

  ## get user name from user
  if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto CX1: " )

  ## the path of fluxnet daily data on cx1
  origpath <- "/work/bstocker/labprentice/data/fapar_MODIS_FPAR_MCD15A3H_fluxnet2015_gee_subset/"

  if (is.na(sitename)){
    ## Download the whole bunch

    ## create required directory
    if (!dir.exists(path)) system( paste0("mkdir -p ", path ) )

    system( paste0( "rsync -avz ", uname, "@login.cx1.hpc.ic.ac.uk:", origpath, " ", path ) )

  } else {
    ## Download only data for a specific site
    ## get a file list of what's on CX1
    filelist <- system( paste0( "ssh ", uname, "@login.cx1.hpc.ic.ac.uk ls ", origpath ), intern = TRUE )

    ## use one file(s) for this site
    filelist <- filelist[ grepl(sitename, filelist) ]
    filelist <- filelist[ grepl("fapar_MODIS_FPAR_MCD15A3H_", filelist) ]

    if (length(filelist)==0){
      ## no data available for this site
      error <- 1
      warn(paste0("No MODIS_FPAR_MCD15A3H data available for site ", sitename ) )
    } else {
      purrr::map( as.list(filelist), ~system( paste0( "rsync -avz ", uname, "@login.cx1.hpc.ic.ac.uk:", origpath, .," ", path ) ) )    
    }

  }
  return( error )
}


##-----------------------------------------------------------
## Manages the path specification for MODIS FPAR data download from CX1
##-----------------------------------------------------------
download_MODIS_FPAR_MCD15A3H_from_cx1 <- function( settings_input ){
  
  require(rlang)

  ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
  if (ans=="y"){
    ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
    if (ans=="y"){
      ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_MODIS_FPAR_MCD15A3H, "? (y/n)") )
      if (ans=="y"){
        error <- download_MODIS_FPAR_MCD15A3H_from_cx1_path( settings_input$path_MODIS_FPAR_MCD15A3H )
      } else {
        path <- readline( prompt = "Please specify a new path: " )
        settings_input$path_MODIS_FPAR_MCD15A3H <- path
        error <- download_MODIS_FPAR_MCD15A3H_from_cx1_path( settings_input$path_MODIS_FPAR_MCD15A3H )
      }
    } else {
      abort( "MODIS_FPAR_MCD15A3H data download not possible.")
    }
  } else {
    abort( "MODIS_FPAR_MCD15A3H data download not possible.")
  }

  return(error)

}


##-----------------------------------------------------------
## Returns a dataframe with all climate input data for one site
## and writes this to CSV and Fortran-formatted input files
## on the fly.
##-----------------------------------------------------------
prepare_input_sofun_climate_bysite <- function( sitename, settings_input, settings_sims ){

  require(purrr)
  require(dplyr)
  require(rlang)

  ## Initialise daily dataframe (WITHOUT LEAP YEARS, SOFUN USES FIXED 365-DAYS YEARS!)
  ddf <- init_dates_dataframe( year(settings_sims$date_start[[sitename]]), year(settings_sims$date_end[[sitename]]), noleap = TRUE )

  ##----------------------------------------------------------------------
  ## Read daily FLUXNET 2015 meteo data for each site (reads all variables)
  ## A file must be found containing the site name in the file name and located in <settings_input$path_fluxnet2015>
  ##----------------------------------------------------------------------
  if (any( c( 
    "fluxnet2015" %in% settings_input$temperature, 
    "fluxnet2015" %in% settings_input$precipitation, 
    "fluxnet2015" %in% settings_input$vpd, 
    "fluxnet2015" %in% settings_input$ppfd
    ))){

    ## Make sure data is available for this site
    error <- check_download_fluxnet2015( settings_input, settings_sims, sitename )

    ## Take only file for this site
    filn <- list.files( settings_input$path_fluxnet2015, pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_DD.*.csv" ) )

    ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
    ddf_meteo_fluxnet2015 <- get_meteo_fluxnet2015( sitename, path = paste0(settings_input$path_fluxnet2015, filn), freq="d" ) %>%
      rename( temp_fluxnet2015 = temp, 
              prec_fluxnet2015 = prec,
              nrad_fluxnet2015 = nrad,
              ppfd_fluxnet2015 = ppfd,
              vpd_fluxnet2015  = vpd, 
              ccov_fluxnet2015 = ccov
              )

    ddf <- ddf %>% left_join( ddf_meteo_fluxnet2015, by="date" )

  }

  ##----------------------------------------------------------------------
  ## Read WATCH-WFDEI data (extracting from NetCDF files for this site)
  ##----------------------------------------------------------------------
  if (any( c( 
    "watch_wfdei" %in% settings_input$temperature, 
    "watch_wfdei" %in% settings_input$precipitation, 
    "watch_wfdei" %in% settings_input$vpd, 
    "watch_wfdei" %in% settings_input$ppfd
    ))){

    ddf_watch <- get_watch_daily( lon = settings_sims$lon[[sitename]], 
                                  lat = settings_sims$lat[[sitename]], 
                                  elv = settings_sims$elv[[sitename]], 
                                  date_start = settings_sims$date_start[[sitename]], 
                                  date_end= settings_sims$date_end[[sitename]], 
                                  settings_input
                                  )

    ddf <- ddf %>% left_join( ddf_watch, by="date" )

  }

  ##----------------------------------------------------------------------
  ## Read CRU data (extracting from NetCDF files for this site)
  ##----------------------------------------------------------------------
  if (any( c( 
      "cru_ts4_01" %in% settings_input$temperature, 
      "cru_ts4_01" %in% settings_input$precipitation, 
      "cru_ts4_01" %in% settings_input$vpd, 
      "cru_ts4_01" %in% settings_input$ppfd,
      "cru_ts4_01" %in% settings_input$cloudcover
    ))){

    cruvars <- c()
    if ("cru_ts4_01" %in% settings_input$temperature)   cruvars <- c(cruvars, "temp")
    if ("cru_ts4_01" %in% settings_input$cloudcover)    cruvars <- c(cruvars, "ccov")
    if ("cru_ts4_01" %in% settings_input$precipitation) cruvars <- c(cruvars, "prec")
    if ("cru_ts4_01" %in% settings_input$precipitation) cruvars <- c(cruvars, "wetd")
    if ("cru_ts4_01" %in% settings_input$vpd)           cruvars <- c(cruvars, "vap")
    if ("cru_ts4_01" %in% settings_input$vpd)           cruvars <- c(cruvars, "temp")

    ## First get monthly data
    mdf_cru <- get_clim_cru_monthly(  lon = settings_sims$lon[[sitename]], 
                                      lat = settings_sims$lat[[sitename]], 
                                      settings = settings_input, 
                                      cruvars 
                                      )

    ## expand monthly to daily data
    ddf_cru <- expand_clim_cru_monthly( mdf_cru, cruvars )

    ddf <- ddf %>% left_join( ddf_cru, by = "date" )

  }

  ## Add site name to dataframe (is merged by rows with ddf of other sites)
  ddf <- ddf %>% mutate( sitename = sitename )

  ##----------------------------------------------------------------------
  ## Write climate data to CSV files: 
  ## <settings_sims$path_input>/sitedata/climate/<sitename>/clim_daily_<sitename>.csv 
  ## (may be read by Python directly???)
  ##----------------------------------------------------------------------
  dir <- paste0( settings_sims$path_input, "/sitedata/climate/", sitename )
  if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
  filn <- paste0( dir, "/clim_daily_", sitename, ".csv" )
  write_csv( ddf, path = filn )

  ##----------------------------------------------------------------------
  ## Write fortran-formatted ascii files with daily values for each year 
  ## based on the CSV file written above (clim_daily_<sitename>.csv)
  ## Necessary because reading from CSV is a pain in Fortran.
  ##----------------------------------------------------------------------
  if (settings_sims$implementation=="fortran"){

    # print("Warning: flexible priorities for input sources not implemented. Using first FLUXNET-2015 data, then WATCH-WFDEI, then CRU" )

    out <- ddf

    ## temperature
    out <- out %>%  mutate( temp = temp_fluxnet2015 )
    if ("temp_watch" %in% names(ddf) && "temp_cru_int" %in% names(ddf) ){
      out <- out %>% mutate( temp = ifelse( !is.na(temp), temp, ifelse( !is.na(temp_watch), temp_watch, temp_cru_int ) ) )
    } else if ("temp_watch" %in% names(ddf)){
      out <- out %>% mutate( temp = ifelse( !is.na(temp), temp, temp_watch ) )
    }

    ## precipitation
    out <- out %>%  mutate( prec = prec_fluxnet2015 )
    if ("prec_watch" %in% names(ddf) && "prec_cru_int" %in% names(ddf) ){
      out <- out %>% mutate( prec = ifelse( !is.na(prec), prec, ifelse( !is.na(prec_watch), prec_watch, prec_cru_gen ) ) )
    } else if ("prec_watch" %in% names(ddf)){
      out <- out %>% mutate( prec = ifelse( !is.na(prec), prec, prec_watch ) )
    }

    ## cloud cover
    if ( "ccov_cru_int" %in% names(ddf) ){
      out <- out %>%  mutate( ccov = ccov_cru_int )
    } else {
      out <- out %>%  mutate( ccov = NA )
    }
    
    ## VPD
    out <- out %>%  mutate( vpd = vpd_fluxnet2015 )
    if ("vpd_qair_watch_temp_watch" %in% names(ddf) && "vpd_vap_cru_temp_cru_int" %in% names(ddf) ){
      out <- out %>% mutate( vpd = ifelse( !is.na(vpd), vpd, ifelse( !is.na(vpd_qair_watch_temp_watch), vpd_qair_watch_temp_watch, vpd_vap_cru_temp_cru_int ) ) )
    } else if ("vpd_qair_watch_temp_watch" %in% names(ddf)){
      out <- out %>% mutate( vpd = ifelse( !is.na(vpd), vpd, temp_watch ) )
    }

    ## ppfd
    out <- out %>%  mutate( ppfd = ppfd_fluxnet2015 )
    if ("ppfd_watch" %in% names(ddf) ){
      out <- out %>% mutate( ppfd = ifelse( !is.na(ppfd), ppfd, ifelse( !is.na(ppfd_watch), ppfd_watch, NA ) ) )
    } 

    out <- out %>% mutate(  temp   = fill_gaps( temp   ),
                            prec   = fill_gaps( prec, is.prec=TRUE ),
                            temp   = fill_gaps( temp   ),
                            ccov   = fill_gaps( ccov   ),
                            vpd    = fill_gaps( vpd    ),
                            ppfd   = fill_gaps( ppfd   )
                            # netrad = ifelse( in_netrad, fill_gaps( netrad ), NA )
                           ) %>% 
                    dplyr::filter( !( month(date)==2 & mday(date)==29 ) )
                   

    for (yr in unique(year(out$date))){

      sub <- dplyr::filter( out, year(date)==yr )

      dirnam <- paste0( settings_sims$path_input, "/sitedata/climate/", sitename, "/", as.character(yr), "/" )
      if (!dir.exists(dirnam)) system( paste0( "mkdir -p ", dirnam ) )

      filnam <- paste0( dirnam, "dtemp_", sitename, "_", yr, ".txt" )
      write_sofunformatted( filnam, sub$temp )
      
      filnam <- paste0( dirnam, "dprec_", sitename, "_", yr, ".txt" )
      write_sofunformatted( filnam, sub$prec )

      filnam <- paste0( dirnam, "dfsun_", sitename, "_", yr, ".txt" )
      write_sofunformatted( filnam, ( 100.0 - sub$ccov ) / 100.0 )

      filnam <- paste0( dirnam, "dvpd_", sitename, "_", yr, ".txt" )
      write_sofunformatted( filnam, sub$vpd )

      filnam <- paste0( dirnam, "dppfd_", sitename, "_", yr, ".txt" )
      write_sofunformatted( filnam, sub$ppfd )

    }          
  
  }

  return( ddf )

}


##-----------------------------------------------------------
## Returns a dataframe with fAPAR input data for one site
## and writes this to CSV and Fortran-formatted input files
## on the fly.
##-----------------------------------------------------------
prepare_input_sofun_fapar_bysite <- function( sitename, settings_input, settings_sims ){

  require(readr)
  require(lubridate)

  print(paste0("Getting fAPAR data for site ", sitename, " ..." ) )

  ## Initialise daily dataframe (WITHOUT LEAP YEARS, SOFUN USES FIXED 365-DAYS YEARS!)
  ddf <- init_dates_dataframe( year(settings_sims$date_start[[sitename]]), year(settings_sims$date_end[[sitename]]), noleap = TRUE ) %>%
         ## Add site name to dataframe (is merged by rows with ddf of other sites)
         mutate( sitename = sitename )

  if (settings_input$fapar=="MODIS_FPAR_MCD15A3H"){

    ## Make sure data is available for this site
    error <- check_download_MODIS_FPAR_MCD15A3H( settings_input, settings_sims, sitename )

    if (error!=1){
      ## Take only file for this site
      filn <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = paste0("dfapar_MODIS_FPAR_MCD15A3H_", sitename, "_gee_subset.csv") )

      ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
      ## IMPORTANT: This is gapfilled data. Original data is in <settings_input$path_MODIS_FPAR_MCD15A3H>/raw/
      ## Gap-filling is done with 'getin/gapfill_modis.R'. The gapfilling step is not yet implemented within prepare_input_sofun().
      ddf_MODIS_FPAR_MCD15A3H <- read_csv( paste0( settings_input$path_MODIS_FPAR_MCD15A3H, filn ) ) %>%
        select( date, fapar = modisvar_interpol)

      ddf <- ddf %>% left_join( ddf_MODIS_FPAR_MCD15A3H, by = "date" )

    }

  }

  if (error!=1){
    ##----------------------------------------------------------------------
    ## Write fapar data to CSV files: 
    ## <settings_sims$path_input>/sitedata/fapar/<sitename>/fapar_daily_<sitename>.csv 
    ## (may be read by Python directly???)
    ##----------------------------------------------------------------------
    dir <- paste0( settings_sims$path_input, "/sitedata/fapar/", sitename )
    if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
    filn <- paste0( dir, "/fapar_daily_", sitename, ".csv" )
    write_csv( ddf, path = filn )

    ##----------------------------------------------------------------------
    ## Write fortran-formatted ascii files with daily values for each year 
    ## based on the CSV file written above (clim_daily_<sitename>.csv)
    ## Necessary because reading from CSV is a pain in Fortran.
    ##----------------------------------------------------------------------
    if (settings_sims$implementation=="fortran"){

      ## get mean seasonal cycle. This is relevant for years where no MODIS data is available.
      ddf_meandoy <- ddf %>% mutate( doy=yday(date) ) %>% group_by( doy ) %>% summarise( meandoy = mean( fapar , na.rm=TRUE ) )

      ## in separate formatted file 
      for (yr in unique(year(ddf$date))){

        ## subset only this year
        ddf_sub <- dplyr::filter( ddf, year(date)==yr ) %>% mutate( doy=yday(date) )

        ## fill gaps with mean seasonal cycle (for pre-MODIS years, entire year is mean seasonal cycle)
        if (nrow(ddf_sub)==0) ddf_sub <- init_dates_dataframe( yr, yr ) %>% mutate( fapar = NA ) %>% dplyr::filter( !( month(date)==2 & mday(date)==29 ) ) %>% mutate( doy=yday(date) )
        ddf_sub <- ddf_sub %>% left_join( ddf_meandoy, by="doy" )

        ## fill gaps with mean by DOY/MOY
        ddf_sub$fapar[ which( is.na( ddf_sub$fapar ) ) ] <- ddf_sub$meandoy[ which( is.na( ddf_sub$fapar ) ) ]

        ## define directory name for SOFUN input
        dirnam <- paste0( settings_sims$path_input, "/sitedata/fapar/", sitename, "/", as.character(yr), "/" )
        system( paste( "mkdir -p", dirnam ) )

        ## daily
        filnam <- paste0( dirnam, "dfapar_", sitename, "_", yr, ".txt" )
        write_sofunformatted( filnam, ddf_sub$fapar )
        
      }
    }
  }

  return(ddf)

}



##-----------------------------------------------------------
## Creates all the input files/links necessary to run simulations
##-----------------------------------------------------------
prepare_input_sofun <- function( settings_input, settings_sims, return_data=FALSE ){

  require(lubridate)
  require(dplyr)
  require(purrr)

  if (settings_sims$lonlat){
    ##-----------------------------------------------------------
    ## LONLAT: Simulation on a spatial grid (longitude/latitude).
    ## In this case, <settings$name> is the simulation name.
    ## Link forcing files into the input directory (no copying to 
    ## save space).
    ## Note that this links files from paths that correspond to 
    ## how input files are stored on CX1. Therefore, the simplest
    ## way is to first mirror respective directories to your local
    ## workstation from where simulation is to be executed.
    ##-----------------------------------------------------------
    ## Grid information
    ##--------------------------------------
    dirn <- 'input/global/grid'
    system( paste0('mkdir -p ', dirn) )

    ## elevation
    system( paste0( "ln -svf ", settings_input$path_cx1data, "watch_wfdei/WFDEI-elevation.nc ", dirn ) )

    ## land masks at 1 deg and 0.5 deg resolution
    system( paste0( "ln -svf ", settings_input$path_cx1data, "landmasks/gicew_1x1deg.cdf ", dirn ))
    system( paste0( "ln -svf ", settings_input$path_cx1data, "landmasks/gicew_halfdeg.cdf ", dirn ))

    ## CO2
    ##--------------------------------------
    dirn <- 'input/global/co2'
    system( paste0( "ln -svf ", settings_input$path_cx1data, "co2/cCO2_rcp85_const850-1765.dat ", dirn ))

    ## fapar (fapar3g)
    ##--------------------------------------
    dirn <- 'input/global/fapar'
    system( paste0('mkdir -p ', dirn) )
    system( paste0("ln -svf ", settings_input$path_cx1data, "fAPAR/fAPAR3g_v2/fAPAR3g_v2_1982_2016_FILLED.nc ", dirn ))

    ## soil (necessary in Fortran implementation)
    ##--------------------------------------
    dirn <- 'input/global/soil'
    system( paste0('mkdir -p ', dirn) )
    system( paste0("ln -svf ", settings_input$path_cx1data, 'soil/soilgrids/whc_soilgrids_halfdeg_FILLED.nc', dirn ) )
    system( paste0("ln -svf ", settings_input$path_cx1data, 'soil/hwsd/soil_type_hwsd_halfdeg.cdf', dirn ) )

    ## land cover (necessary in Fortran implementation)
    ##--------------------------------------
    dirn <- 'input/global/landcover'
    system( paste0('mkdir -p ', dirn) )
    system( paste0("ln -svf ", settings_input$path_cx1data, 'landcover/modis_landcover_halfdeg_2010_FILLED.nc', dirn ) )


    ## WATCH-WFDEI climate input data
    ##--------------------------------------
    dirn <- './input/global/climate'
    if (!dir.exists(dirn)) system( paste0('mkdir -p ', dirn) )

    ## temperature
    src <- paste0(settings_input$path_cx1data, 'watch_wfdei/Tair_daily/*')
    dst <- 'input/global/climate/temp'
    if (!dir.exists(dst)) system( paste0('mkdir -p ', dst) )
    system( paste0('ln -svf ', src, ' ', dst) )

    ## precipitation (rain and snow)
    dst <- 'input/global/climate/prec'
    if (!dir.exists(dst)) system( paste0('mkdir -p ' + dst) )

    src <- paste0(settings_input$path_cx1data, 'watch_wfdei/Rainf_daily/*')
    system( paste0('ln -svf ', src, ' ', dst) )

    src <- paste0(settings_input$path_cx1data, 'watch_wfdei/Snowf_daily/*')
    system( paste0('ln -svf ', src, ' ', dst) )

    ## humidity (specific humidity in the case of WATCH-WFDEI)
    src <- paste0(settings_input$path_cx1data, 'watch_wfdei/Qair_daily/*')
    dst <- 'input/global/climate/humd'
    if (!dir.exists(dst)) system( paste0('mkdir -p ' + dst) )
    system( paste0('ln -svf ', src, ' ', dst) )

    ## solar (shortwave) radiation
    src <- paste0(settings_input$path_cx1data, 'watch_wfdei/SWdown_daily/*')
    dst <- 'input/global/climate/srad'
    if (!dir.exists(dst)) system( paste0('mkdir -p ', dst) )
    system( paste0('ln -svf ', src, ' ', dst) )

    ## CRU climate input data (only ccov)
    ##--------------------------------------
    ## cloud cover
    src <- paste0(settings_input$path_cx1data, 'cru/ts_3.23/cru_ts3.23.1901.2014.cld.dat.nc')
    dst <- 'input/global/climate/ccov'
    if (!dir.exists(dst)) system( paste0('mkdir -p ', dst) )
    system( paste0('ln -svf ', src, ' ', dst) )


  } else {
    #-----------------------------------------------------------
    # Site-scale simulation(s)
    # Ensemble of multiple site-scale simulations that "go toghether"
    # In this case, <settings$name> is the name of the ensemble (e.g., "fluxnet2015")
    #-----------------------------------------------------------
    # If FLUXNET 2015 data is required, make sure it's available locally    
    #-----------------------------------------------------------
    if (any( c( 
      "fluxnet2015" %in% settings_input$temperature, 
      "fluxnet2015" %in% settings_input$precipitation, 
      "fluxnet2015" %in% settings_input$vpd, 
      "fluxnet2015" %in% settings_input$ppfd
      ))){

      error <- check_download_fluxnet2015( settings_input, settings_sims )

    }

    ##-----------------------------------------------------------
    ## If WATCH-WFDEI data is required, make sure it's available locally
    ##-----------------------------------------------------------
    if ("watch_wfdei" %in% settings_input$temperature)   error <- check_download_watch_wfdei( varnam = "Tair", settings_input, settings_sims )
    if ("watch_wfdei" %in% settings_input$precipitation) error <- check_download_watch_wfdei( varnam = "Snowf_daily", settings_input, settings_sims )
    if ("watch_wfdei" %in% settings_input$precipitation) error <- check_download_watch_wfdei( varnam = "Rainf_daily", settings_input, settings_sims )
    if ("watch_wfdei" %in% settings_input$vpd)           error <- check_download_watch_wfdei( varnam = "Qair_daily", settings_input, settings_sims )
    if ("watch_wfdei" %in% settings_input$ppfd)          error <- check_download_watch_wfdei( varnam = "SWdown_daily", settings_input, settings_sims )

    ##-----------------------------------------------------------
    ## If CRU TS data is required, make sure it's available locally
    ##-----------------------------------------------------------
    if ("cru_ts4_01" %in% settings_input$temperature)   error <- check_download_cru_ts4_01( varnam = "tmp", settings_input, settings_sims )
    if ("cru_ts4_01" %in% settings_input$precipitation) error <- check_download_cru_ts4_01( varnam = "wet", settings_input, settings_sims )
    if ("cru_ts4_01" %in% settings_input$precipitation) error <- check_download_cru_ts4_01( varnam = "pre", settings_input, settings_sims )
    if ("cru_ts4_01" %in% settings_input$vpd)           error <- check_download_cru_ts4_01( varnam = "vap", settings_input, settings_sims )
    if ("cru_ts4_01" %in% settings_input$vpd)           error <- check_download_cru_ts4_01( varnam = "tmp", settings_input, settings_sims )
    if ("cru_ts4_01" %in% settings_input$cloudcover)    error <- check_download_cru_ts4_01( varnam = "cld", settings_input, settings_sims )

    ##-----------------------------------------------------------
    ## If MODIS_FPAR_MCD15A3H data is required, make sure it's available locally    
    ##-----------------------------------------------------------
    if (settings_input$fapar=="MODIS_FPAR_MCD15A3H") error <- check_download_MODIS_FPAR_MCD15A3H( settings_input, settings_sims )

    ##-----------------------------------------------------------
    ## Make sure CMIP standard CO2 data is available locally    
    ##-----------------------------------------------------------
    if (settings_input$co2=="cmip") error <- check_download_cmip_co2( settings_input, settings_sims )

    ##-----------------------------------------------------------
    ## Loop over all sites and prepare input files by site.
    ##-----------------------------------------------------------
    ## Climate input files
    ddf_climate <-  purrr::map( as.list(settings_sims$sitenames), ~prepare_input_sofun_climate_bysite( ., settings_input, settings_sims ) ) %>%
                    bind_rows()

    ## fAPAR input files
    ddf_fapar <-  purrr::map( as.list(settings_sims$sitenames), ~prepare_input_sofun_fapar_bysite( ., settings_input, settings_sims ) ) %>%
                  bind_rows()

    ## CO2 file: link into site-specific input directories
    error_list <- purrr::map( as.list(settings_sims$sitenames), ~check_download_cmip_co2( settings_input, settings_sims, . ) )

  }

  if (return_data){
    return( ddf_climate %>% left_join( ddf_fapar, by=c("date", "sitename")) )
  } else {
    return("Brexit.")
  }

}
