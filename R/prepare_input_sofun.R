#' Processes SOFUN model input.
#'
#' Handles the processing of model inputs (including the optional downloading from
#' a remote server), and links input files to the SOFUN model input directory (\code{lonlat} setup),
#' or writes text files of time series (site-scale setup). 
#' 
#' @param settings_input A list containging the model input settings. See vignette_rsofun.pdf for more information and examples.
#' @param settings_sims A list containing model simulation settings from \code{\link{prepare_setup_sofun}}.  See vignette_rsofun.pdf for more information and examples.
#' @param return_data If \code{TRUE}, input data is returned as a named list of data frames (tibbles), containing input data for each site (not available in the \code{lonlat} setup).
#' @param overwrite_climate if \code{TRUE}, yearly climate input text files in the site-scale setup are overwritten.
#' @param overwrite_fapar if \code{TRUE}, yearly fAPAR input text files in the site-scale setup are overwritten.
#' @param overwrite_csv_climate if \code{TRUE}, climate input CSV files in the site-scale setup are overwritten.
#' @param overwrite_csv_fapar if \code{TRUE}, fAPAR input CSV files in the site-scale setup are overwritten.
#' @param verbose if \code{TRUE}, additional messages are printed.
#'
#' @return if \code{return_data == TRUE}, a named list of data frames (tibbles) containing input data for each site is returned. Otherwise, a depressing character string is returned.
#' @export
#'
#' @examples inputdata <- prepare_input_sofun( settings_input = settings_input, settings_sims = settings_sims, return_data = TRUE, overwrite_climate = FALSE, overwrite_fapar = TRUE, verbose = TRUE )
#' 
prepare_input_sofun <- function( settings_input, settings_sims, return_data=FALSE, overwrite_climate=FALSE, overwrite_fapar=FALSE, overwrite_csv_climate=FALSE, overwrite_csv_fapar=FALSE, verbose=FALSE ){

  if (settings_sims$setup == "lonlat"){
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

    ## avoid returning data for lonlat simulations (too big)
    return_data <- FALSE

    dirn <- paste0(settings_sims$path_input, "global")
    system( paste0('mkdir -p ', dirn) )

    ## Grid information
    ##--------------------------------------
    dirn <- paste0(settings_sims$path_input, "global/grid") 
    system( paste0('mkdir -p ', dirn) )

    ## elevation
    if (!file.exists(settings_input$path_elevation)) abort("Elevation input file specified by settings_input$path_elevation is not available.")
    system( paste0( "ln -svf ", settings_input$path_elevation, " ", dirn ) )

    ## land masks at 1 deg and 0.5 deg resolution
    if (!file.exists(settings_input$path_landmask)) abort("Land mask input file specified by settings_input$path_landmask is not available.")
    system( paste0( "ln -svf ", settings_input$path_landmask, " ", dirn ) )      

    ## CO2 (globally uniform)
    ##--------------------------------------
    dirn <- paste0(settings_sims$path_input, "global/co2") 
    system( paste0('mkdir -p ', dirn) )
    if (!file.exists(settings_input$path_co2)) abort("CO2 input file specified by settings_input$path_co2 is not available.")
    system( paste0( "ln -svf ", settings_input$path_co2, " ", dirn ) )

    ## soil (necessary in Fortran implementation)
    ##--------------------------------------
    dirn <- paste0(settings_sims$path_input, "global/soil") 
    system( paste0('mkdir -p ', dirn) )
    if (!file.exists(settings_input$path_whc)) abort("Soil water holding capacity input file specified by settings_input$path_whc is not available.")
    system( paste0("ln -svf ", settings_input$path_whc, ' ', dirn ) )

    if (!file.exists(settings_input$path_soiltype)) abort("Soil type input file specified by settings_input$path_soiltype is not available.")
    system( paste0("ln -svf ", settings_input$path_soiltype, ' ', dirn ) )

    ## land cover (necessary in Fortran implementation)
    ##--------------------------------------
    dirn <- paste0(settings_sims$path_input, "global/landcover") 
    system( paste0('mkdir -p ', dirn) )
    if (!file.exists(settings_input$path_landcover)) abort("Land cover input file specified by settings_input$path_landcover is not available.")
    system( paste0("ln -svf ", settings_input$path_landcover, ' ', dirn ) )

    ## fapar (fapar3g)
    ##--------------------------------------
    dirn <- paste0(settings_sims$path_input, "global/fapar") 
    system( paste0('mkdir -p ', dirn) )
    if (settings_input$fapar == "fAPAR3g"){
      if (!file.exists(settings_input$path_fAPAR3g)) abort("fAPAR input file specified by settings_input$path_fAPAR3g is not available.")
      system( paste0("ln -svf ", settings_input$path_fAPAR3g, " ", dirn ))
    }

    ## WATCH-WFDEI climate input data
    ##--------------------------------------
    dirn <- paste0(settings_sims$path_input, "global/climate") 
    if (!dir.exists(dirn)) system( paste0('mkdir -p ', dirn) )

    ## temperature
    if (settings_input$temperature == "watch_wfdei"){
      src <- paste0(settings_input$path_watch_wfdei, 'Tair_daily/*')
      dst <- paste0( dirn, '/temp')
      if (!dir.exists(dst)) system( paste0('mkdir -p ', dst) )
      system( paste0('ln -svf ', src, ' ', dst) )
    }

    ## precipitation (rain and snow)
    dst <- paste0( dirn, '/prec' )
    if (!dir.exists(dst)) system( paste0('mkdir -p ', dst) )

    if (settings_input$precipitation == "watch_wfdei"){
      src <- paste0(settings_input$path_watch_wfdei, 'Rainf_daily/*')
      system( paste0('ln -svf ', src, ' ', dst) )

      src <- paste0(settings_input$path_watch_wfdei, 'Snowf_daily/*')
      system( paste0('ln -svf ', src, ' ', dst) )
    }

    ## vapour pressure deficit (specific humidity in the case of WATCH-WFDEI, converted online)
    if (settings_input$vpd == "watch_wfdei"){
      src <- paste0(settings_input$path_watch_wfdei, 'Qair_daily/*')
      dst <- paste0( dirn, '/humd' )
      if (!dir.exists(dst)) system( paste0('mkdir -p ', dst) )
      system( paste0('ln -svf ', src, ' ', dst) )
    }

    ## photosynthetic photon flux density (PPFD) (solar (shortwave) radiation in the case of WATCH-WFDEI, converted online)
    if (settings_input$ppfd == "watch_wfdei"){
      src <- paste0(settings_input$path_watch_wfdei, 'SWdown_daily/*')
      dst <- paste0( dirn, '/srad' )
      if (!dir.exists(dst)) system( paste0('mkdir -p ', dst) )
      system( paste0('ln -svf ', src, ' ', dst) )
    }

    ## CRU climate input data (only ccov)
    ##--------------------------------------
    ## cloud cover
    if (settings_input$cloudcover == "cru"){
      filn <- list.files( settings_input$path_cru, pattern = "*cld*" )
      src <- paste0( settings_input$path_cru, filn )
      dst <- paste0( dirn, '/ccov' )
      if (!dir.exists(dst)) system( paste0('mkdir -p ', dst) )
      system( paste0('ln -svf ', src, ' ', dst) )
    }

  } else {
    #-----------------------------------------------------------
    # Site-scale simulation(s)
    # Ensemble of multiple site-scale simulations that "go toghether"
    # In this case, <settings$name> is the name of the ensemble (e.g., "fluxnet2015")
    #-----------------------------------------------------------
    if (!is.na(settings_input$data)){
      ## Data is provided as a list of data frames.  
      ## Climate input files
      ddf_climate <- settings_input$data %>% dplyr::select( -fapar ) %>% bind_rows()

      ## prepare the fapar input files for each site
      ddf_fapar <- settings_input$data %>% dplyr::select( fapar ) %>% bind_rows()


    } else {

      # If FLUXNET 2015 data is required, make sure it's available locally    
      #-----------------------------------------------------------
      if (any( c( 
        "fluxnet2015" %in% settings_input$temperature, 
        "fluxnet2015" %in% settings_input$precipitation, 
        "fluxnet2015" %in% settings_input$vpd, 
        "fluxnet2015" %in% settings_input$ppfd,
        "fluxnet2015" %in% settings_input$netrad
        ))){

        error <- check_download_fluxnet2015( settings_input$path_fluxnet2015 )

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
      if ("cru" %in% settings_input$temperature)   error <- check_download_cru( varnam = "tmp", settings_input, settings_sims )
      if ("cru" %in% settings_input$precipitation) error <- check_download_cru( varnam = "wet", settings_input, settings_sims )
      if ("cru" %in% settings_input$precipitation) error <- check_download_cru( varnam = "pre", settings_input, settings_sims )
      if ("cru" %in% settings_input$vpd)           error <- check_download_cru( varnam = "vap", settings_input, settings_sims )
      if ("cru" %in% settings_input$vpd)           error <- check_download_cru( varnam = "tmp", settings_input, settings_sims )
      if ("cru" %in% settings_input$cloudcover)    error <- check_download_cru( varnam = "cld", settings_input, settings_sims )

      ##-----------------------------------------------------------
      ## If fapar data is required, make sure it's available locally    
      ##-----------------------------------------------------------
      if (settings_input$fapar=="MODIS_FPAR_MCD15A3H"){

        error <- check_download_MODIS_FPAR_MCD15A3H( settings_input, settings_sims )

      } else if (settings_input$fapar=="MODIS_EVI_MOD13Q1"){

        error <- check_download_MODIS_EVI_MOD13Q1( settings_input, settings_sims )

      } else if (is.na(settings_input$fapar)){

        rlang::warn("No fapar data prepared.")

      }


      ##-----------------------------------------------------------
      ## Make sure CMIP standard CO2 data is available locally    
      ##-----------------------------------------------------------
      error <- check_download_co2( settings_input, settings_sims )

      ##-----------------------------------------------------------
      ## Loop over all sites and prepare input files by site.
      ##-----------------------------------------------------------
      ## Climate input files
      if (overwrite_climate || return_data || overwrite_csv_climate){
        ddf_climate <-  purrr::map( as.list(settings_sims$sitenames), ~prepare_input_sofun_climate_bysite( ., settings_input, settings_sims, overwrite = overwrite_climate, overwrite_csv = overwrite_csv_climate, verbose = verbose ) ) %>%
                        bind_rows()
      }

      ## prepare the fapar input files for each site
      if (overwrite_fapar || return_data || overwrite_csv_fapar){
        ddf_fapar <-  purrr::map( as.list(settings_sims$sitenames), ~prepare_input_sofun_fapar_bysite( ., settings_input, settings_sims, overwrite = overwrite_fapar, overwrite_csv = overwrite_csv_fapar, verbose = verbose ) ) %>%
                      bind_rows()
      }

      ## CO2 file: link into site-specific input directories
      error_list <- purrr::map( as.list(settings_sims$sitenames), ~check_download_co2( settings_input, settings_sims, . ) )

      ## fAPAR input files: see below

    }

  }

  ##-----------------------------------------------------------
  ## fAPAR input files
  ##-----------------------------------------------------------
  ## first, place a text file in the fapar input data directory that specifies where it's coming from (needs to be read online by Fortran)                
  dir <- paste0( settings_sims$dir_sofun, "./input" )
  if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
  zz <- file( paste0(dir, "/dfapar_source.txt"), "w")
  # if (settings_input$splined_fapar) {fapar_forcing_source <- paste0( settings_input$fapar, "_spl" )} else {fapar_forcing_source <- settings_input$fapar}
  tmp <- cat( paste0("fapar_forcing_source                    ", settings_input$fapar) , "\n", file=zz )  # the blanks are necessary! 
  close(zz)

  if (return_data){

    return( ddf_climate %>% left_join( ddf_fapar, by=c("date", "sitename")) )

  } else {

    return("Brexit.")

  }

}

##-----------------------------------------------------------
## Returns a dataframe with all climate input data for one site
## and writes this to CSV and Fortran-formatted input files
## on the fly.
##-----------------------------------------------------------
prepare_input_sofun_climate_bysite <- function( sitename, settings_input, settings_sims, overwrite=FALSE, overwrite_csv=FALSE, verbose=FALSE ){

  if (verbose) print(paste("prepare_input_sofun_climate_bysite() for site", sitename ))

  ## path of CSV file with data for this site
  dir <- paste0( settings_sims$path_input, "/sitedata/climate/", sitename )
  if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
  csvfiln <- paste0( dir, "/clim_daily_", sitename, ".csv" )

  if (file.exists(csvfiln)&&!overwrite_csv){

    ddf <- readr::read_csv( csvfiln )

  } else {

    ## Initialise daily dataframe (WITHOUT LEAP YEARS, SOFUN USES FIXED 365-DAYS YEARS!)
    ddf <- init_dates_dataframe( year(settings_sims$date_start[[sitename]]), year(settings_sims$date_end[[sitename]]), noleap = TRUE )

    ##----------------------------------------------------------------------
    ## Read daily FLUXNET 2015 meteo data for each site (reads all variables)
    ## A file must be found containing the site name in the file name and located in <settings_input$path_fluxnet2015>
    ##----------------------------------------------------------------------
    fluxnetvars <- c()
    if ("fluxnet2015" %in% settings_input$temperature)   fluxnetvars <- c( fluxnetvars, "temp" )
    if ("fluxnet2015" %in% settings_input$precipitation) fluxnetvars <- c( fluxnetvars, "prec" )
    if ("fluxnet2015" %in% settings_input$vpd)           fluxnetvars <- c( fluxnetvars, "vpd" )
    if ("fluxnet2015" %in% settings_input$ppfd)          fluxnetvars <- c( fluxnetvars, "ppfd" )
    if ("fluxnet2015" %in% settings_input$netrad)        fluxnetvars <- c( fluxnetvars, "nrad" )

    if (length(fluxnetvars)>0){

      ## Make sure data is available for this site
      error <- check_download_fluxnet2015( settings_input$path_fluxnet2015, sitename )
      ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
      ddf <- get_meteo_fluxnet2015( sitename, dir = settings_input$path_fluxnet2015, freq="d" ) %>%
        dplyr::select( date, one_of(fluxnetvars) ) %>% 
        setNames( c("date", paste0( fluxnetvars, "_fluxnet2015" ))) %>%
        right_join( ddf, by = "date" )

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

      ddf <- get_watch_daily( lon = settings_sims$lon[[sitename]], 
                              lat = settings_sims$lat[[sitename]], 
                              elv = settings_sims$elv[[sitename]], 
                              date_start = settings_sims$date_start[[sitename]], 
                              date_end= settings_sims$date_end[[sitename]], 
                              settings_input
                              ) %>% 
              right_join( ddf, by="date" )

    }

    ##----------------------------------------------------------------------
    ## Fill missing variables
    ##----------------------------------------------------------------------
    if (is.na(settings_input$cloudcover)){
      rlang::warn("Filling column ccov_dummy with value 50 (%).")
      ddf <- ddf %>% mutate( ccov_dummy = 50 )
    }

    ##----------------------------------------------------------------------
    ## Read CRU data (extracting from NetCDF files for this site)
    ##----------------------------------------------------------------------
    if (any( c( 
      "cru" %in% settings_input$temperature, 
      "cru" %in% settings_input$precipitation, 
      "cru" %in% settings_input$vpd, 
      "cru" %in% settings_input$ppfd,
      "cru" %in% settings_input$cloudcover
      ))){

      cruvars <- c()
      if ("cru" %in% settings_input$temperature)   cruvars <- c(cruvars, "temp")
      if ("cru" %in% settings_input$cloudcover)    cruvars <- c(cruvars, "ccov")
      if ("cru" %in% settings_input$precipitation) cruvars <- c(cruvars, "prec")
      if ("cru" %in% settings_input$precipitation) cruvars <- c(cruvars, "wetd")
      if ("cru" %in% settings_input$vpd)           cruvars <- c(cruvars, "vap")
      if ("cru" %in% settings_input$vpd)           cruvars <- c(cruvars, "temp")

      ## First get monthly data
      mdf_cru <- get_clim_cru_monthly(  lon = settings_sims$lon[[sitename]], 
                                        lat = settings_sims$lat[[sitename]], 
                                        settings = settings_input, 
                                        cruvars 
                                        )

      ## expand monthly to daily data
      if (length(cruvars)>0) ddf <- expand_clim_cru_monthly( mdf_cru, cruvars ) %>%
        right_join( ddf, by = "date" )

    }

    ##----------------------------------------------------------------------
    ## Write climate data to CSV files: 
    ## <settings_sims$path_input>/sitedata/climate/<sitename>/clim_daily_<sitename>.csv 
    ## (may be read by Python directly???)
    ##----------------------------------------------------------------------
    write_csv( ddf, path = csvfiln )

  }

  ## Add site name to dataframe (is merged by rows with ddf of other sites)
  ddf <- ddf %>% dplyr::select( -(starts_with("year_dec")) ) %>% mutate( sitename = sitename )

  ## Check if fortran-formatted text files are written already
  filelist <- list.files( paste0( settings_sims$path_input, "/sitedata/climate/", sitename, "/", as.character( lubridate::year( ddf$date[1] ) ), "/" ) )
  
  if (length(filelist)==0 || overwrite){
    ##----------------------------------------------------------------------
    ## Write fortran-formatted ascii files with daily values for each year 
    ## based on the CSV file written above (clim_daily_<sitename>.csv)
    ## Necessary because reading from CSV is a pain in Fortran.
    ##----------------------------------------------------------------------
    if (settings_sims$implementation=="fortran" || settings_sims$implementation=="python"){

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

      if (settings_sims$in_netrad){
        ## nrad
        out <- out %>%  mutate( nrad = nrad_fluxnet2015 )
        if ("nrad_watch" %in% names(ddf) ){
          out <- out %>% mutate( nrad = ifelse( !is.na(nrad), nrad, ifelse( !is.na(nrad_watch), nrad_watch, NA ) ) )
        } 
      } else {
        ## cloud cover
        if ( "ccov_cru_int" %in% names(ddf) ){
          out <- out %>%  mutate( ccov = ccov_cru_int )
        } else if ("ccov_dummy" %in% names(ddf)){
          out <- out %>% mutate( ccov = ccov_dummy )
        }
      }

      out <- out %>% mutate(  temp   = fill_gaps( temp   ),
                              prec   = fill_gaps( prec, is.prec=TRUE ),
                              temp   = fill_gaps( temp   ),
                              vpd    = fill_gaps( vpd    ),
                              ppfd   = fill_gaps( ppfd   )
                             ) %>% 
                      dplyr::filter( !( lubridate::month(date)==2 & lubridate::mday(date)==29 ) )
      
      ## Help. I don't know why this doesn't work with ifelse inside mutate
      if (settings_sims$in_netrad){
        out <- out %>% mutate( nrad = fill_gaps( nrad ) )
      } else {
        out <- out %>% mutate( ccov = fill_gaps( ccov ) )
      }

      for (yr in unique(year(out$date))){

        sub <- dplyr::filter( out, year(date)==yr )

        dirnam <- paste0( settings_sims$path_input, "/sitedata/climate/", sitename, "/", as.character(yr), "/" )
        if (!dir.exists(dirnam)) system( paste0( "mkdir -p ", dirnam ) )

        filnam <- paste0( dirnam, "dtemp_", sitename, "_", yr, ".txt" )
        write_sofunformatted( filnam, sub$temp )
        
        filnam <- paste0( dirnam, "dprec_", sitename, "_", yr, ".txt" )
        write_sofunformatted( filnam, sub$prec )

        filnam <- paste0( dirnam, "dvpd_", sitename, "_", yr, ".txt" )
        write_sofunformatted( filnam, sub$vpd )

        filnam <- paste0( dirnam, "dppfd_", sitename, "_", yr, ".txt" )
        write_sofunformatted( filnam, sub$ppfd )

        if (settings_sims$in_netrad){
          filnam <- paste0( dirnam, "dnetrad_", sitename, "_", yr, ".txt" )
          write_sofunformatted( filnam, sub$nrad )
        } else {
          filnam <- paste0( dirnam, "dfsun_", sitename, "_", yr, ".txt" )
          write_sofunformatted( filnam, ( 100.0 - sub$ccov ) / 100.0 )
        }

      }          
    
    }

  } 

  return( ddf )

}

##-----------------------------------------------------------
## Returns a dataframe with fAPAR input data for one site
## and writes this to CSV and Fortran-formatted input files
## on the fly.
##-----------------------------------------------------------
prepare_input_sofun_fapar_bysite <- function( sitename, settings_input, settings_sims, overwrite=FALSE, overwrite_csv=FALSE, verbose=FALSE ){

  if (verbose) print(paste0("Getting fAPAR data for site ", sitename, " ..." ) )

  ## File path of fAPAR CSV file for this site
  dir <- paste0( settings_sims$path_input, "/sitedata/fapar/", sitename )
  
  if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
  
  csvfiln <- paste0( dir, "/fapar_daily_", sitename, ".csv" )

  if (file.exists(csvfiln)&&!overwrite_csv){

    ddf <- readr::read_csv( csvfiln ) %>% mutate( fapar = as.numeric(fapar) )

  } else {

    ## Initialise daily dataframe (WITHOUT LEAP YEARS, SOFUN USES FIXED 365-DAYS YEARS!)
    ddf <- init_dates_dataframe( year(settings_sims$date_start[[sitename]]), year(settings_sims$date_end[[sitename]]), noleap = TRUE ) %>%
           ## Add site name to dataframe (is merged by rows with ddf of other sites)
           mutate( sitename = sitename )

    ##----------------------------------------------------------------------
    ## Download (if necessary) and read
    ##----------------------------------------------------------------------
    if (settings_input$fapar=="MODIS_FPAR_MCD15A3H"){

      ## Make sure data is available for this site
      error <- check_download_MODIS_FPAR_MCD15A3H( settings_input, settings_sims, sitename )

      if (error!=1){
        ## Take only file for this site
        filn <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = paste0("dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_", sitename, "_gee_subset.csv") )

        ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
        ## IMPORTANT: This is gapfilled data. Original data is in <settings_input$path_MODIS_FPAR_MCD15A3H>/raw/
        ## Gap-filling is done with 'getin/gapfill_modis.R'. The gapfilling step is not yet implemented within prepare_input_sofun().
        if (length(filn)!=0){
          tmp <- readr::read_csv( paste0( settings_input$path_MODIS_FPAR_MCD15A3H, filn ) )
          if (settings_input$splined_fapar){
            tmp <- tmp %>% dplyr::select( date, fapar = spline )
          } else {
            tmp <- tmp %>% dplyr::select( date, fapar = interpl )
          }
          ddf <- tmp %>%
            mutate( fapar = as.numeric(fapar) ) %>%
            right_join( ddf, by = "date" )
        } else {
          error <- 1
          ddf <- ddf %>% mutate( fapar = NA )
        }

      }

    } else if (settings_input$fapar=="MODIS_EVI_MOD13Q1"){
      
      ## Make sure data is available for this site
      error <- check_download_MODIS_EVI_MOD13Q1( settings_input, settings_sims, sitename )
      
      if (error!=1){
        ## Take only file for this site
        filn <- list.files( settings_input$path_MODIS_EVI_MOD13Q1, pattern = paste0("dfapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_", sitename, "_gee_subset.csv") )
        
        ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
        ## IMPORTANT: This is gapfilled data. Original data is in <settings_input$path_MODIS_EVI_MOD13Q1>/raw/
        ## Gap-filling is done with 'getin/gapfill_modis.R'. The gapfilling step is not yet implemented within prepare_input_sofun().
        if (length(filn)!=0){
          tmp <- readr::read_csv( paste0( settings_input$path_MODIS_EVI_MOD13Q1, filn ) )
          if (settings_input$splined_fapar){
            tmp <- tmp %>% dplyr::select( date, fapar = spline )
          } else {
            tmp <- tmp %>% dplyr::select( date, fapar = interpl )
          }
          ddf <- tmp %>%
            mutate( fapar = as.numeric(fapar) ) %>%
            right_join( ddf, by = "date" )
        } else {
          error <- 1
          ddf <- ddf %>% mutate( fapar = NA )
        }
      }
      
    }    

    if (error!=1){
      ##----------------------------------------------------------------------
      ## Write fapar data to CSV files: 
      ## <settings_sims$path_input>/sitedata/fapar/<sitename>/fapar_daily_<sitename>.csv 
      ## (may be read by Python directly???)
      ##----------------------------------------------------------------------
      write_csv( ddf, path = csvfiln )

      filelist <- list.files( paste0( settings_sims$path_input, "/sitedata/fapar/", sitename, "/", as.character(year(ddf$date[1])), "/" ) )
      
      if (length(filelist)==0 || overwrite){
        ##----------------------------------------------------------------------
        ## Write Fortran-formatted ASCII files with daily values for each year 
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
    }

  }

  ## Add site name to dataframe (is merged by rows with ddf of other sites)
  ddf <- ddf %>% dplyr::select( -(starts_with("year_dec")) )
  
  return(ddf)

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
get_meteo_fluxnet2015 <- function( sitename, dir=NA, path=NA, freq="d" ){

  ## from flux to energy conversion, umol/J (Meek et al., 1984), same as used in SPLASH (see Eq.50 in spash_doc.pdf)
  kfFEC <- 2.04

  if (is.na(path)){

    if ( freq=="y" ){
      ## Annual data
      print( paste( "getting annual FLUXNET 2015 data for site", sitename ) )
      allfiles <- list.files( dir )
      allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
      allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
      filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
      path <- paste0( dir, filnam_obs ) 

    } else if ( freq=="m" ){
      ## Monthly data
      print( paste( "getting monthly FLUXNET 2015 data for site", sitename ) )
      allfiles <- list.files( dir )
      allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
      allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
      filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
      path <- paste0( dir, filnam_obs ) 

    } else if ( freq=="w" ){
      ## Weekly data
      print( paste( "getting weekly FLUXNET 2015 data for site", sitename ) )
      allfiles <- list.files( dir )
      allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
      allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
      filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
      path <- paste0( dir, filnam_obs ) 

    } else if ( freq=="d" ){
      ## Daily data
      print( paste( "getting daily FLUXNET 2015 data for site", sitename ) )
      allfiles <- list.files( dir )
      allfiles <- allfiles[ which( grepl( "FULLSET", allfiles ) ) ]
      allfiles <- allfiles[ which( grepl( "3.csv", allfiles ) ) ]
      filnam_obs <- allfiles[ which( grepl( sitename, allfiles ) ) ]
      path <- paste0( dir, filnam_obs ) 

    }

  } else if (is.na(path)) {
    abort("get_meteo_fluxnet2015(): Either path or dir must be provided as arguments.")
  }

  ## read data
  meteo <-  readr::read_csv( path, na="-9999", col_types = cols() )

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
  meteo <- meteo %>%  dplyr::rename( temp = TA_F,
                              vpd  = VPD_F,
                              prec = P_F,
                              swin = SW_IN_F
                            ) %>%
                      mutate( swin = swin * 60 * 60 * 24,   # given in W m-2, required in J m-2 d-1 
                              ppfd = swin * kfFEC * 1.0e-6, # convert from J/m2/d to mol/m2/d
                              vpd  = vpd * 1e2,             # given in hPa, required in Pa
                              ccov = NA
                            ) 
  
  ## I don't know how to do this better with a single condition to be evaluated
  if (is.element( "NETRAD", names(meteo) )){
    meteo <- meteo %>%  mutate( nrad = NETRAD * (60 * 60 * 24) ) %>% # given in W m-2 (avg.), required in J m-2 (daily total)
                        dplyr::select( date, temp, prec, nrad, ppfd, vpd, ccov )
  } else {
    meteo <- meteo %>% mutate( nrad = NA )
  }
  
  ## subset data
  meteo <- meteo %>% dplyr::select( date, temp, prec, nrad, ppfd, vpd, ccov )

  return( meteo )

}

##--------------------------------------------------------------------
## Function returns daily data frame with columns for watch data 
## (temp_watch, prec_watch, vpd_qair_watch_temp_watch, [ppfd_watch])
##--------------------------------------------------------------------
get_watch_daily <- function( lon, lat, elv, date_start, date_end, settings_input ){

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

  ## extract net radiation data XXX NOT IMPLEMENTED
  rlang::warn( "get_watch_daily(): Net radiation data extraction from WATCH-WFDEI is not implemented. Filling with NAs." )
  ddf_nrad <- ddf_ppfd %>% dplyr::rename( nrad_watch = ppfd_watch ) %>% mutate( nrad_watch = NA )

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
## Extract monthly data from files for each year and attach to the 
## monthly dataframe (at the right location).
## Original data in K, returns data in K
##--------------------------------------------------------------------
get_pointdata_temp_wfdei <- function( lon, lat, mo, yr, ignore_leap=TRUE, path ){
  
  ndaymonth_all <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymonth <- ndaymonth_all[mo]

  filn <- paste0( path, "/Tair_daily/Tair_daily_WFDEI_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )
  if ( file.exists( filn ) ){
    print( paste( "extracting from", filn ) )
    system( paste0( path.package("rsofun"), "/bash/extract_pointdata_byfil.sh ", filn, " Tair", " lon", " lat ", sprintf("%.2f",lon), " ", sprintf("%.2f",lat) ) )
    dtemp <- read.table( paste0( path.package("rsofun"), "/tmp/out.txt") )$V1 - 273.15  # conversion from Kelving to Celsius
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

  ndaymonth_all <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymonth <- ndaymonth_all[mo]
  
  ## rain
  filn <- paste0( path, "/Rainf_daily/Rainf_daily_WFDEI_CRU_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )
  if ( file.exists( filn ) ){
    print( paste( "extracting from", filn ) )
    system( paste0( path.package("rsofun"), "/bash/extract_pointdata_byfil.sh ", filn, " Rainf", " lon", " lat ", sprintf("%.2f",lon), " ", sprintf("%.2f",lat) ) )
    dprec <- read.table( paste0( path.package("rsofun"), "/tmp/out.txt") )$V1
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
    system( paste0( path.package("rsofun"), "/bash/extract_pointdata_byfil.sh ", filn, " Snowf", " lon", " lat ", sprintf("%.2f",lon), " ", sprintf("%.2f",lat) ) )
    dsnow <- read.table( paste0( path.package("rsofun"), "/tmp/out.txt") )$V1
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

  ndaymonth_all <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymonth <- ndaymonth_all[mo]

  filn <- paste0( path, "/Qair_daily/Qair_daily_WFDEI_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )
  if ( file.exists( filn ) ){
    print( paste( "extracting from", filn ) )
    system( paste0( path.package("rsofun"), "/bash/extract_pointdata_byfil.sh ", filn, " Qair", " lon", " lat ", sprintf("%.2f",lon), " ", sprintf("%.2f",lat) ) )
    dqair <- read.table( path.package("rsofun"), "/tmp/out.txt" )$V1
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
  
  ndaymonth_all <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymonth <- ndaymonth_all[mo]

  ## conversion factor from SPLASH: flux to energy conversion, umol/J (Meek et al., 1984)
  kfFEC <- 2.04    

  filn <- paste0( path, "/SWdown_daily/SWdown_daily_WFDEI_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )
  if ( file.exists( filn ) ){
    print( paste( "extracting from", filn ) )
    system( paste0( path.package("rsofun"), "/bash/extract_pointdata_byfil.sh ", filn, " SWdown", " lon", " lat ", sprintf("%.2f",lon), " ", sprintf("%.2f",lat) ) )
    dswdown <- read.table( paste0( path.package("rsofun"), "/tmp/out.txt") )$V1
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

##--------------------------------------------------------------------
## Extract monthly data from files for each year and attach to the 
## monthly dataframe (at the right location).
## Original data in K, returns data in K
##--------------------------------------------------------------------
get_pointdata_monthly_cru <- function( varnam, lon, lat, settings, yrend ){

  filn <- list.files( settings$path_cru, pattern=paste0( varnam, ".dat.nc" ) )

  if ( length( filn )!=0 ){

    cmd <- paste0( path.package("rsofun"), "/bash/extract_pointdata_byfil.sh ", paste0( settings$path_cru, filn ), " ", varnam, " lon", " lat", sprintf("%.2f",lon), " ", sprintf("%.2f",lat) )
    print( paste( "executing command:", cmd ) )
    system( cmd )
    mdata <- read.table( paste0( path.package("rsofun"), "/tmp/out.txt") )$V1
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
## Get monthly data from CRU
##--------------------------------------------------------------------
get_clim_cru_monthly <- function( lon, lat, settings, cruvars ){

  ## get last year for which data is available
  filn <- list.files( settings$path_cru, pattern="cld.dat.nc")
  start <- regexpr( 20, filn)[1]
  stop <- start + 3
  yrend <- substr( filn, start, stop ) %>% as.numeric %>% ifelse( length(.)==0, 2010, . )

  ## cloud cover
  mdf <- get_pointdata_monthly_cru( "cld", lon, lat, settings, yrend=yrend )

  ## Check if data is available at that location, otherwise use nearest gridcell
  if (!is.data.frame(mdf)){
    lon_look <- find_nearest_cruland_by_lat( lon, lat, paste0( settings$path_cru, filn ) )
    mdf <- get_pointdata_monthly_cru( "cld", lon_look, lat, settings, yrend=yrend )
  } else {
    lon_look <- lon
  }
  mdf <- mdf %>% dplyr::rename( ccov_cru = mdata )    

  ## precipitation
  if ("prec" %in% cruvars){
    mdf <- get_pointdata_monthly_cru( "pre", lon_look, lat, settings, yrend=yrend ) %>% dplyr::rename( prec_cru = mdata ) %>% 
      right_join( mdf, by = c("date", "year_dec") )
  }

  ## wet days
  if ("wetd" %in% cruvars){
    mdf <- get_pointdata_monthly_cru( "wet", lon_look, lat, settings, yrend=yrend ) %>% dplyr::rename( wetd_cru = mdata ) %>% 
      right_join( mdf, by = c("date", "year_dec") )
  }

  ## air temperature
  if ("temp" %in% cruvars){
    mdf <- get_pointdata_monthly_cru( "tmp", lon_look, lat, settings, yrend=yrend ) %>% dplyr::rename( temp_cru = mdata ) %>% 
      right_join( mdf, by = c("date", "year_dec") )
  }

  ## VPD 
  ## calculated as a function of vapour pressure and temperature, vapour
  ## pressure is given by CRU data.
  if ("vap" %in% cruvars){
    mdf <-  get_pointdata_monthly_cru( "vap", lon_look, lat, settings, yrend=yrend ) %>%  
                dplyr::rename( vap_cru = mdata ) %>%
                ## merge temperature data in here for VPD calculation
                left_join( mdf_temp, by =  c("date", "year_dec") ) %>%
                ## calculate VPD (vap is in hPa)
                mutate( vpd_vap_cru_temp_cru = calc_vpd( eact=1e2*vap_cru, tc=temp_cru ) ) %>% 
                ## avoid duplicate 
                dplyr::select( -temp_cru ) %>% 
                right_join( mdf, by = c("date", "year_dec") )
  }


  return( mdf )

}


##--------------------------------------------------------------------
## Interpolates monthly data to daily data using polynomials or linear
## for a single year
##--------------------------------------------------------------------
expand_clim_cru_monthly <- function( mdf, cruvars ){

  ddf_yr_list <- purrr::map( as.list( unique( year( mdf$date ) ) ), ~expand_clim_cru_monthly_byyr( ., mdf, cruvars ) )

  ddf <- bind_rows( ddf_yr_list )

  return( ddf )

}


##--------------------------------------------------------------------
## Interpolates monthly data to daily data using polynomials or linear
## for a single year
##--------------------------------------------------------------------
expand_clim_cru_monthly_byyr <- function( yr, mdf, cruvars ){

  nmonth <- 12

  startyr_cru <- year(mdf$date) %>% unique() %>% first()
  endyr_cru   <- year(mdf$date) %>% unique() %>% last()
  
  yr_pvy <- max(startyr_cru, yr-1)
  yr_nxt <- min(endyr_cru, yr+1)

  ## add first and last year to head and tail of 'mdf'
  first <- mdf[1:12,] %>% dplyr::select( -year_dec ) %>% mutate( date = date - years(1) )
  last  <- mdf[(nrow(mdf)-11):nrow(mdf),] %>% dplyr::select( -year_dec ) %>% mutate( date = date + years(1) )

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

    ddf <- init_dates_dataframe( yr, yr ) %>%
           mutate( temp_cru_int = monthly2daily( mtemp, "polynom", mtemp_pvy[nmonth], mtemp_nxt[1], leapyear = leap_year(yr) ) ) %>% 
           right_join( ddf, by = c("date", "year_dec") )
  }

  ##--------------------------------------------------------------------
  ## precipitation: interpolate using weather generator
  ##--------------------------------------------------------------------
  if ("prec" %in% cruvars){
    mprec <- dplyr::filter( mdf, year(date)==yr )$prec_cru
    mwetd <- dplyr::filter( mdf, year(date)==yr )$wetd_cru

    if (any(!is.na(mprec))&&any(!is.na(mwetd))){
      ddf <-  init_dates_dataframe( yr, yr ) %>% 
              mutate( prec_cru_gen = get_daily_prec( mprec, mwetd, leapyear = leap_year(yr) ) ) %>% 
              right_join( ddf, by = c("date", "year_dec") )
    }
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

    ddf <-  init_dates_dataframe( yr, yr ) %>%
            mutate( ccov_cru_int = monthly2daily( mccov, "polynom", mccov_pvy[nmonth], mccov_nxt[1], leapyear = leap_year(yr) ) ) %>%
            ## Reduce CCOV to a maximum 100%
            mutate( ccov_cru_int = ifelse( ccov_cru_int > 100, 100, ccov_cru_int ) ) %>%
            right_join( ddf, by = c("date", "year_dec") )

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

    ddf <- init_dates_dataframe( yr, yr ) %>%
               mutate( vpd_vap_cru_temp_cru_int = monthly2daily( mvpd, "polynom", mvpd_pvy[nmonth], mvpd_nxt[1], leapyear = (yr %% 4 == 0) ) ) %>% 
               right_join( ddf, by = c("date", "year_dec") )
  }

  return( ddf )

}

##--------------------------------------------------------------------
## Finds the closest land cell in the CRU dataset at the same latitude
##--------------------------------------------------------------------
find_nearest_cruland_by_lat <- function( lon, lat, filn ){

  if (!requireNamespace("ncdf4", quietly = TRUE))
    stop("Please, install 'ncdf4' package")

  nc <- ncdf4::nc_open( filn, readunlim=FALSE )
  crufield <- ncdf4::ncvar_get( nc, varid="TMP" )
  lon_vec <- ncdf4::ncvar_get( nc, varid="LON" )
  lat_vec <- ncdf4::ncvar_get( nc, varid="LAT" )
  crufield[crufield==-9999] <- NA
  ncdf4::nc_close(nc)

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


##--------------------------------------------------------------------------
## Checks if WATCH-WFDEI files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_watch_wfdei <- function( varnam, settings_input, settings_sims ){

  ## Determine file name, given <settings_input$path_fluxnet2015>
  ## look for data in the given directory
  filelist <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )

  if (length(filelist)==0){

    ## No files found at specified location
    rlang::warn( paste0("No files found for WATCH-WFDEI in directory ", settings_input$path_watch_wfdei) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a different directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )

    if (length(filelist)==0){
     
      ## Search from home
      rlang::warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      
      if (ans=="y"){
      
        filelist <- list.files( "~/", pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )
      
      } else {
        
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_watch_wfdei>
        if (settings_input$get_from_remote){
          rlang::warn( "Initiating download from remote server..." )
          getfiles <- getfilenames_watch_wfdei( settings_input$date_start, settings_input$date_end )
          error <- download_from_remote(
            settings_input$path_remote_watch_wfdei,  
            settings_input$path_watch_wfdei, 
            getfiles = getfiles, 
            uname = settings_input$uname_remote, 
            address_remote = settings_input$address_remote 
            )        
        }
        filelist <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )
      }

      if (length(filelist)==0){
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_watch_wfdei>
        abort("check_download_watch_wfdei(): No files downloaded.")
      }

    }

  } 

  ## Check if files are now available at specified location.
  filelist <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )
  if (length(filelist)==0) abort("check_download_watch_wfdei(): No files downloaded.")
  
}

## Returns the WATCH-WFDEI file names that are still missing locally 
getfilenames_watch_wfdei <- function( date_start, date_end ){

  ## determine simulation years ocurring in this ensemble
  allyears <- seq( from = purrr::map_dbl( settings_sims$date_start, ~year(.) ) %>% min(),
                   to   = purrr::map_dbl( settings_sims$date_end, ~year(.) ) %>% max(),
                   by   = 1 ) %>% as.list

  ## Determine missing files for this variable, given start and end years of all the simulations in this ensemble
  getfiles <- purrr::map( allyears, ~check_watch_wfdei_year( ., varnam, settings_input ) ) %>% unlist()

  return(getfiles)  

}


##--------------------------------------------------------------------------
## Returns the file names of missing files for this year
##--------------------------------------------------------------------------
check_watch_wfdei_year <- function( year, varnam, settings_input ){

  ## construct file names of all months' files (12 for each year)
  allfiles <- purrr::map_chr( as.list(sprintf("%02d", 1:12 )), ~paste0( varnam, "_daily/", varnam, "_daily_WFDEI_", as.character(year), ., ".nc" ) )
  avlfiles <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_", as.character(year), ".*.nc"), recursive = TRUE )

  getfiles <- allfiles[!(allfiles %in% avlfiles)]

  return(getfiles)

}

##--------------------------------------------------------------------------
## Checks if CRU TS files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_cru <- function( varnam, settings_input, settings_sims ){

  ## Determine file name, given <settings_input$path_fluxnet2015>
  ## look for data in the given directory
  getfiles <- list.files( settings_input$path_cru, pattern = paste0( varnam, ".dat.nc" ) )

  if (length(getfiles)==0){

    ## No files found at specified location
    rlang::warn( paste0("No files found for CRU TS in directory ", settings_input$path_cru) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a different directory? Enter the path from which search is to be done: ")
    getfiles <- list.files( path, pattern = paste0( "cru_ts*.1901.2016.", varnam, ".dat.nc") )

    if (length(getfiles)==0){
     
      ## Search from home
      rlang::warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      
      if (ans=="y"){
      
        getfiles <- list.files( "~/", pattern = paste0( "cru_ts*.1901.2016.", varnam, ".dat.nc") )
      
      } else {
        
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_cru>
        if (settings_input$get_from_remote){
          rlang::warn( "Initiating download from remote server ..." )
          error <- download_from_remote( 
            settings_input$path_remote_cru, 
            settings_input$path_cru, 
            pattern = varnam, 
            uname = settings_input$uname_remote, 
            address_remote = settings_input$address_remote 
            )
        }
        getfiles <- list.files( settings_input$path_cru, pattern = paste0( "cru_ts*.1901.2016.", varnam, ".dat.nc") )
      }

      if (length(getfiles)==0){
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_cru>
        abort("check_download_cru(): No files downloaded from remote.")
      }

    }

  } 

  ## Check if files are now available at specified location.
  getfiles <- list.files( settings_input$path_cru, pattern = paste0( varnam, ".dat.nc" ) )
  if (length(getfiles)==0) abort("Download of CRU data was not successful. No files found.")
  
}

##--------------------------------------------------------------------------
## Checks if MODIS_FPAR_MCD15A3H files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_MODIS_FPAR_MCD15A3H <- function( settings_input, settings_sims, sitename=NA ){

  error <- 0

  ## Determine file name, given <settings_input$path_MODIS_FPAR_MCD15A3H>
  ## look for data for this site in the given directory
  filelist <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = "dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_.*_gee_subset.csv" )

  if (length(filelist)==0){

    ## No files found at specified location
    rlang::warn( paste0("No files found for MODIS_FPAR_MCD15A3H in directory ", settings_input$path_MODIS_FPAR_MCD15A3H) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a certain directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = "dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_.*_gee_subset.csv", recursive = TRUE )

    if (length(filelist)==0){
     
      ## Search from home
      rlang::warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      if (ans=="y"){
        filelist <- list.files( "~/", pattern = "dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_.*_gee_subset.csv", recursive = TRUE )
      
      } else {
      
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_MODIS_FPAR_MCD15A3H>
        if (settings_input$get_from_remote){
          rlang::warn( "Initiating download from remote server..." )
          error <- download_from_remote( 
            settings_input$path_remote_MODIS_FPAR_MCD15A3H, 
            settings_input$path_MODIS_FPAR_MCD15A3H, 
            pattern = NA,
            uname = settings_input$uname_remote, 
            address_remote = settings_input$address_remote 
            )
        }
        filelist <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = "dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_.*_gee_subset.csv" )
      }

      if (length(filelist)==0){
        if (settings_input$get_from_remote){
          ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_MODIS_FPAR_MCD15A3H>
          rlang::warn( "Initiating download from remote server..." )
          error <- download_from_remote( 
            settings_input$path_remote_MODIS_FPAR_MCD15A3H, 
            settings_input$path_MODIS_FPAR_MCD15A3H, 
            pattern = NA,
            uname = settings_input$uname_remote, 
            address_remote = settings_input$address_remote 
            )
        }
      }

    }

  }

  if (!is.na(sitename)){
    ## Check if a file is available for a given site
    filelist <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = paste0("dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_", sitename, "_gee_subset.csv") )

    if (length(filelist)==0){
      if (settings_input$get_from_remote){
        ## Download missing file
        error <- download_from_remote( 
          settings_input$path_remote_MODIS_FPAR_MCD15A3H, 
          settings_input$path_MODIS_FPAR_MCD15A3H, 
          pattern = sitename,
          uname = settings_input$uname_remote, 
          address_remote = settings_input$address_remote 
          )
      }
    }

  }
  return( error )
}


##--------------------------------------------------------------------------
## Checks if MODIS_EVI_MOD13Q1 files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_MODIS_EVI_MOD13Q1 <- function( settings_input, settings_sims, sitename=NA ){

  error <- 0

  ## Determine file name, given <settings_input$path_MODIS_EVI_MOD13Q1>
  ## look for data for this site in the given directory
  filelist <- list.files( settings_input$path_MODIS_EVI_MOD13Q1, pattern = "fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_.*_gee_subset.csv" )

  if (length(filelist)==0){

    ## No files found at specified location
    rlang::warn( paste0("No files found for MODIS_EVI_MOD13Q1 in directory ", settings_input$path_MODIS_EVI_MOD13Q1) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a certain directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = "fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_.*_gee_subset.csv", recursive = TRUE )

    if (length(filelist)==0){
     
      ## Search from home
      rlang::warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      if (ans=="y"){
        filelist <- list.files( "~/", pattern = "fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_.*_gee_subset.csv", recursive = TRUE )
      } else {
        if (settings_input$get_from_remote){
          ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_MODIS_EVI_MOD13Q1>
          rlang::warn( "Initiating download from remote server..." )
          error <- download_from_remote( 
            settings_input$path_remote_MODIS_EVI_MOD13Q1, 
            settings_input$path_MODIS_EVI_MOD13Q1, 
            sitename = NA,
            uname = settings_input$uname_remote, 
            address_remote = settings_input$address_remote 
            )
        }
        filelist <- list.files( settings_input$path_MODIS_EVI_MOD13Q1, pattern = "fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_.*_gee_subset.csv" )
      }

      if (length(filelist)==0){
        if (settings_input$get_from_remote){
          ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_MODIS_EVI_MOD13Q1>
          rlang::warn( "Initiating download from remote server..." )
          error <- download_from_remote( 
            settings_input$path_remote_MODIS_EVI_MOD13Q1, 
            settings_input$path_MODIS_EVI_MOD13Q1, 
            sitename = NA,
            uname = settings_input$uname_remote, 
            address_remote = settings_input$address_remote 
            )
        }
      }

    }

  }

  if (!is.na(sitename)){
    ## Check if a file is available for a given site
    filelist <- list.files( settings_input$path_MODIS_EVI_MOD13Q1, pattern = paste0("fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_", sitename, "_gee_subset.csv") )

    if (length(filelist)==0 && settings_input$get_from_remote){
      ## Download missing file
      error <- download_from_remote( 
        settings_input$path_remote_MODIS_EVI_MOD13Q1, 
        settings_input$path_MODIS_EVI_MOD13Q1, 
        pattern = sitename,
        uname = settings_input$uname_remote, 
        address_remote = settings_input$address_remote 
        )
    }

  }
  return( error )
}

##--------------------------------------------------------------------------
## Checks if CMIP CO2 files are available and initiates download if not.
##--------------------------------------------------------------------------
check_download_co2 <- function( settings_input, settings_sims, sitename = NA ){

  error <- 0

  if (!file.exists(settings_input$path_co2) && settings_input$get_from_remote){

    error <- download_from_remote( settings_input$path_remote_co2, settings_input$path_co2, uname = settings_input$uname_remote, address_remote = settings_input$address_remote )

    # origpath <- dirname(settings_input$path_co2)

    # ## No files found at specified location
    # system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", settings_input$path_remote_co2, " ", localdir ) )

  }

  if (!is.na(sitename)){
    ## Check if a file is available for a given site
    localdir_bysite <- paste0( settings_sims$path_input, "/sitedata/co2/", sitename )
    if (!dir.exists(localdir_bysite)) system( paste0("mkdir -p ", localdir_bysite ) )
    filelist <- list.files( localdir_bysite, pattern = basename(settings_input$path_co2) )

    if (length(filelist)==0){
      ## link file into site-level directory
      system( paste0( "ln -svf ", settings_input$path_co2, " ", localdir_bysite, "/" ) )
    }

  }
  return( error )
}




# ##-----------------------------------------------------------
# ## Manages the path specification for CRU TS 4.01 data download from CX1
# ##-----------------------------------------------------------
# download_cru_from_remote <- function( varnam, settings_input ){

#   # ## the path of CRU TS 4.01 data on cx1
#   # origpath <- "/work/bstocker/labprentice/data/cru/ts_4.01/" 
#   # filn <-  paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc")
  
#   ## Interactive part
#   ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
#   if (ans=="y"){
#     ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
#     if (ans=="y"){
#       ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_cru, "? (y/n)") )
#       if (ans=="y"){
#         error <- download_cru_from_remote_filn( varnam, settings_input, filn = filn )
#       } else {
#         path <- readline( prompt = "Please specify a new path: " )
#         settings_input$path_cru <- path
#         error <- download_cru_from_remote_filn( varnam, settings_input, filn = filn )
#       }
#     } else {
#       abort( "CRU TS 4.01 data download not possible.")
#     }
#   } else {
#     abort( "CRU TS 4.01 data download not possible.")
#   }

#   return(error)

# }


# ##-----------------------------------------------------------
# ## Downloads CRU TS 4.01 data from CX1
# ##-----------------------------------------------------------
# download_cru_from_remote_filn <- function( varnam, settings_input, filn ){

#   origpath <- "/work/bstocker/labprentice/data/cru/ts_4.01/"

#   filn <-  paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc")

#   if (!dir.exists(settings_input$path_cru)) system(paste0("mkdir -p ", settings_input$path_cru))
#   if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto remote server: " )

#   system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", origpath, filn, " ", settings_input$path_cru ) )

#   return(NULL)
# }


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


# ##-----------------------------------------------------------
# ## Downloads MODIS FPAR data from CX1
# ##-----------------------------------------------------------
# download_MODIS_FPAR_MCD15A3H_from_remote_path <- function( path, sitename=NA ){

#   error <- 0

#   ## get user name from user
#   if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto remote server: " )

#   ## the path of fluxnet daily data on cx1
#   origpath <- "/work/bstocker/labprentice/data/fapar_MODIS_FPAR_MCD15A3H_fluxnet2015_gee_subset/"

#   if (is.na(sitename)){
#     ## Download the whole bunch

#     ## create required directory
#     if (!dir.exists(path)) system( paste0("mkdir -p ", path ) )

#     system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", origpath, " ", path ) )

#   } else {
#     ## Download only data for a specific site
#     ## get a file list of what's on CX1
#     filelist <- system( paste0( "ssh ", uname, "@", settings$address_remote, " ls ", origpath ), intern = TRUE )

#     ## use one file(s) for this site
#     filelist <- filelist[ grepl(sitename, filelist) ]
#     filelist <- filelist[ grepl("fapar_MODIS_FPAR_MCD15A3H_", filelist) ]

#     if (length(filelist)==0){
#       ## no data available for this site
#       error <- 1
#       rlang::warn(paste0("No MODIS_FPAR_MCD15A3H data available for site ", sitename ) )
#     } else {
#       purrr::map( as.list(filelist), ~system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", origpath, .," ", path ) ) )    
#     }

#   }
#   return( error )
# }


# ##-----------------------------------------------------------
# ## Downloads MODIS EVI data from CX1
# ##-----------------------------------------------------------
# download_MODIS_EVI_MOD13Q1_from_remote_path <- function( path, sitename=NA ){

#   error <- 0

#   ## get user name from user
#   if (!exists("uname") && is.null(settings_input$uname)) uname <<- readline( prompt = "Enter your user name for logging onto remote server: " )

#   ## the path of fluxnet daily data on cx1
#   origpath <- settings$path_remote_MODIS_EVI_MOD13Q1

#   if (is.na(sitename)){
#     ## Download the whole bunch

#     ## create required directory
#     if (!dir.exists(path)) system( paste0("mkdir -p ", path ) )

#     system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", settings$path_remote_MODIS_EVI_MOD13Q1 ) )

#   } else {
#     ## Download only data for a specific site
#     ## get a file list of what's on CX1
#     filelist <- system( paste0( "ssh ", uname, "@", settings$address_remote, " ls ", dirname(settings$path_remote_MODIS_EVI_MOD13Q1) ), intern = TRUE )

#     ## use one file(s) for this site
#     filelist <- filelist[ grepl(sitename, filelist) ]
#     filelist <- filelist[ grepl("evi_MOD13Q1_gee_MOD13Q1_", filelist) ]

#     if (length(filelist)==0){
#       ## no data available for this site
#       error <- 1
#       rlang::warn(paste0("No MODIS_EVI_MOD13Q1 data available for site ", sitename ) )
#     } else {
#       purrr::map( as.list(filelist), ~system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", settings$path_remote_MODIS_EVI_MOD13Q1, .," ", settings$path_MODIS_EVI_MOD13Q1 ) ) )    
#     }

#   }
#   return( error )
# }


# ##-----------------------------------------------------------
# ## Manages the path specification for MODIS FPAR data download from CX1
# ##-----------------------------------------------------------
# download_MODIS_FPAR_MCD15A3H_from_remote <- function( settings_input ){
  
#   ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
#   if (ans=="y"){
#     ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
#     if (ans=="y"){
#       ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_MODIS_FPAR_MCD15A3H, "? (y/n)") )
#       if (ans=="y"){
#         # error <- download_MODIS_FPAR_MCD15A3H_from_remote_path( settings_input$path_MODIS_FPAR_MCD15A3H )
#         error <- download_from_remote_path(  settings_input$path_MODIS_FPAR_MCD15A3H )
#         download_from_remote_path( dir_remote, dir_local, sitename = NA, uname = NULL, address_remote = NULL )
#       } else {
#         path <- readline( prompt = "Please specify a new path: " )
#         # settings_input$path_MODIS_FPAR_MCD15A3H <- path
#         # error <- download_MODIS_FPAR_MCD15A3H_from_remote_path( settings_input$path_MODIS_FPAR_MCD15A3H )
#       }
#     } else {
#       abort( "MODIS_FPAR_MCD15A3H data download not possible.")
#     }
#   } else {
#     abort( "MODIS_FPAR_MCD15A3H data download not possible.")
#   }

#   return(error)

# }


# ##-----------------------------------------------------------
# ## Manages the path specification for WATCH-WFDEI data download from CX1
# ##-----------------------------------------------------------
# download_watch_wfdei_from_remote <- function( varnam, settings_input, settings_sims ){

#   # ## determine simulation years ocurring in this ensemble
#   # allyears <- seq( from = purrr::map_dbl( settings_sims$date_start, ~year(.) ) %>% min(),
#   #                  to   = purrr::map_dbl( settings_sims$date_start, ~year(.) ) %>% max(),
#   #                  by   = 1 ) %>% as.list

#   # ## Determine missing files for this variable, given start and end years of all the simulations in this ensemble
#   # getfiles <- purrr::map( allyears, ~check_watch_wfdei_year( ., varnam, settings_input ) ) %>% unlist()

#   ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_watch_wfdei, "? (y/n)") )
#   if (ans=="y"){
#     error <- download_watch_wfdei_from_remote_path( settings_input$path_watch_wfdei, getfiles )
#   } else {
#     path <- readline( prompt = "Please specify a new path: " )
#     settings_input$path_watch_wfdei <- path
#     error <- download_watch_wfdei_from_remote_path( settings_input$path_watch_wfdei, getfiles )
#   }

#   return(error)

# }

# ##-----------------------------------------------------------
# ## Downloads WATCH-WFDEI data from CX1 xxxxx
# ##-----------------------------------------------------------
# download_watch_wfdei_from_remote_path <- function( path, getfiles ){

#   ## the path of WATCH-WFDEI daily data on cx1
#   origpath <- "/work/bstocker/labprentice/data/watch_wfdei/"

#   ## create required directory
#   if (!dir.exists(path)) system( paste0("mkdir -p ", path ) )

#   ## files are in sub-directories, determine them
#   subdir <- getfiles %>% dirname() %>% unique()
#   if (!dir.exists(paste0( path, "/", subdir ))) system( paste0("mkdir -p ", paste0( path, "/", subdir ) ) )

#   if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto remote server: " )
#   error <- purrr::map( as.list(getfiles[1]), ~system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", origpath, ., " ", paste0( path, subdir ) ) ) )

#   ## Show files in directory
#   print( paste0("Files in directory: ", path) )
#   print( list.files( path ) )

# }


# ##-----------------------------------------------------------
# ## Manages the path specification for MODIS EVI data download from CX1
# ##-----------------------------------------------------------
# download_MODIS_EVI_MOD13Q1_from_remote <- function( settings_input ){
  
#   ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
#   if (ans=="y"){
#     ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
#     if (ans=="y"){
#       ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_MODIS_EVI_MOD13Q1, "? (y/n)") )
#       if (ans=="y"){
#         error <- download_MODIS_EVI_MOD13Q1_from_remote_path( settings_input$path_MODIS_EVI_MOD13Q1 )
#       } else {
#         path <- readline( prompt = "Please specify a new path: " )
#         settings_input$path_MODIS_EVI_MOD13Q1 <- path
#         error <- download_MODIS_EVI_MOD13Q1_from_remote_path( settings_input$path_MODIS_EVI_MOD13Q1 )
#       }
#     } else {
#       abort( "MODIS_EVI_MOD13Q1 data download not possible.")
#     }
#   } else {
#     abort( "MODIS_EVI_MOD13Q1 data download not possible.")
#   }

#   return(error)

# }

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

  ## Warning: this is crude, if not wrong. 
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

