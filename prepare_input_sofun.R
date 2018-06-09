prepare_input_sofun <- function( settings_input, settings_sims ){

  if (settings$lonlat){
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
    ##-----------------------------------------------------------
    ## Site-scale simulation(s)
    ##-----------------------------------------------------------
    if (settings_sims$ensemble){
      ##-----------------------------------------------------------
      ## Ensemble: multiple site-scale simulations that "go toghether"
      ## In this case, <settings$name> is the name of the ensemble (e.g., "fluxnet2015")
      ##-----------------------------------------------------------
      if (any( c( 
        settings_input$temperature=="fluxnet2015", 
        settings_input$precipitation=="fluxnet2015", 
        settings_input$vpd=="fluxnet2015", 
        settings_input$ppfd=="fluxnet2015"
        ))){
        ## Read daily FLUXNET 2015 meteo data for each site (reads all variables)
        ## A file must be found containing the site name in the file name and located in <settings_input$path_fluxnet2015>
        for (sitename in settings_sims$sitenames){

          ## Determine file name, given <settings_input$path_fluxnet2015>
          filnam <- 

          ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
          meteo_fluxnet2015 <- get_meteo_fluxnet2015( sitename, freq="d", path=filnam )

          ## Write to CSV files: <settings_sims$path_input>/sitedata/climate/<sitename>/clim_daily_<sitename>.csv (may be read by Python directly)

          if (settings_sims$implementation=="fortran"){
            ## Write fortran-formatted ascii files with daily values for each year based on the CSV file written above (clim_daily_<sitename>.csv)
          
          }

        }

      }

    }
  }

}