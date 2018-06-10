##-----------------------------------------------------------
## Write "site" parameter file into <settings$path_input>/site_paramfils/<settings$grid>.parameter,
## containing information:
## - landmask file name
## - topography (elevation) file name
## - soil type file name
## These files are given for each <settings$grid> identifier, therefore no need to specify them in the settings.
##-----------------------------------------------------------
write_site_parameter_bysite <- function( sitename, settings ){

  ## create path of the site parameter file for this site
  path <- paste0( settings$path_input, "/site_paramfils/", sitename, ".parameter" )

  if (settings$implementation=="fortran"){
      
    system( paste0( "cp SITENAME.parameter ", path ) )
    
    ## set lon, lat and elv
    system( paste0( "sed -i ", systr, " 's/XXXLONXXX/", format( settings$lon[[sitename]], nsmall=4 ), "/g' ", path ) )
    system( paste0( "sed -i ", systr, " 's/XXXLATXXX/", format( settings$lat[[sitename]], nsmall=4 ), "/g' ", path ) )
    system( paste0( "sed -i ", systr, " 's/XXXALTXXX/", format( settings$elv[[sitename]], nsmall=0 ), "/g' ", path ) )
    system( paste0( "sed -i ", systr, " 's/XXXWHCXXX/", format( settings$whc[[sitename]], nsmall=0 ), "/g' ", path ) )


  } else if (settings$implementation=="python"){

  }
  
  return( path )
  
}

##-----------------------------------------------------------
## Write "simulation" parameter file into <settings$path_input>/run/<settings$grid>.parameter,
## containing information:
## - run name (will be used for output file names)
## - model spinup length
## - year (AD) of simulation start and end
## - file name of model parameters
## - input directory path
## - CO2 forcing file name (will be linked into input/global/co2)
## - fAPAR forcing file name (will be linked into input/global/fapar/)
## - switches for holding inputs constant (climate, CO2, etc.)
## - switches for writing variables to output files
##-----------------------------------------------------------
write_simulation_parameter_bysite <- function( sitename, settings ){

  require(lubridate)
  require(dplyr)
  
  ## create path of the simulation parameter file for this site
  path <- paste0( settings$path_input, "/run/", sitename, ".sofun.parameter" )

  if (settings$implementation=="fortran"){

    source("create_simulation_parameter_file.R")
    path <- create_simulation_parameter_file( 
              path                 = path,
              simname              = sitename,
              sitename             = sitename,
              firstyeartrend       = settings$date_start[[sitename]] %>% year(),
              spinupyears          = settings$spinupyears,
              nyeartrend           = settings$date_end[[sitename]] %>% year() - settings$date_start[[sitename]] %>% year() + 1,
              recycle              = settings$recycle,
              daily_out_startyr    = settings$date_start[[sitename]] %>% year(),
              daily_out_endyr      = settings$date_end[[sitename]] %>% year(),
              co2filnam            = "cCO2_rcp85_const850-1765.dat",
              lGr3                 = settings$is_c3[[sitename]],
              lGr4                 = settings$is_c4[[sitename]],
              fapar_forcing_source = "dfapar_MODIS_FPAR_MCD15A3H",
              in_ppfd              = settings$in_ppfd,
              loutplant            = settings$loutplant,
              loutgpp              = settings$loutgpp,
              loutwaterbal         = settings$loutwaterbal,
              loutdtemp_soil       = settings$loutdtemp_soil,
              loutdgpp             = settings$loutdgpp,
              loutdrd              = settings$loutdrd,
              loutdtransp          = settings$loutdtransp,
              lncoutdtemp          = settings$lncoutdtemp,
              lncoutdfapar         = settings$lncoutdfapar,
              lncoutdgpp           = settings$lncoutdgpp, 
              lncoutdwaterbal      = settings$lncoutdwaterbal
              )

  } else if (settings$implementation=="python"){
    
    print("prepare_setup_sofun.R: write_simulation_parameter_bysite(): python tbc.")

  }

  return( path )

}


##-----------------------------------------------------------
## Prepares the setup for running SOFUN with simulation settings defined by argument 'settings'. 
## In the Fortran version, these settings are read in run-time from parameter files (text files, 
## one for each site, and simulation).
##-----------------------------------------------------------
prepare_setup_sofun <- function( settings ){

  require(readr)
  require(dplyr)
  require(lubridate)
  require(purrr)

  if (settings$lonlat){
    ##-----------------------------------------------------------
    ## LONLAT: Simulation on a spatial grid (longitude/latitude)
    ## In this case, <settings$name> is the simulation name.
    ##-----------------------------------------------------------
    ## Write site parameter files for each site
    ## create new simuation parameter file for this experiment
    dirnam <- paste0( settings$path_input, "/site_paramfils/" )
    if (!dir.exists(dirnam)) system( paste( "mkdir -p ", dirnam ) ) 
    settings$path_site_paramfil <- write_site_parameter_bysite( settings$name, settings )

    ## Write simulation parameter files for each site
    dirnam <- paste0( settings$path_input, "/run/" )
    if (!dir.exists(dirnam)) system( paste( "mkdir -p ", dirnam ) ) 
    settings$path_simulation_paramfil <- write_simulation_parameter_bysite( settings$name, settings )

    ## link input directories
    system( paste0( "unlink ", settings$dir_sofun, "input/global") )
    system( paste0( "ln -svf ", settings$path_input, "global ", settings$dir_sofun, "input/global") )


  } else {
    ## Site-scale simulation(s)
    
    if (settings$ensemble){
      ##-----------------------------------------------------------
      ## Ensemble: multiple site-scale simulations that "go toghether"
      ## In this case, <settings$name> is the name of the ensemble (e.g., "fluxnet2015")
      ##-----------------------------------------------------------
      ## Read info that varies between sites from the meta information file <settings$path_siteinfo>:
      ## - site name, must be: column number 1 
      ## - longitude of site, column must be named 'lon'
      ## - latitude of site, column must be named 'lat'
      ## - elevation of site, column must be named 'elv'
      ## - years for which simulation is to be done (corresponding to data availability from site), 
      ##   requires two columns named 'year_start' and 'year_end'.
      siteinfo <- read_csv( settings$path_siteinfo )

      ##--------------------------------------
      ## Complement settings with meta info for each site
      ##--------------------------------------
      ## Start year
      year_start <- sapply( siteinfo[,1], function(x) siteinfo$year_start[ which(siteinfo[,1]==x) ] ) %>% as.list()
      date_start <- purrr::map( year_start, ~ymd( paste0( ., "-01-01" ) ) )
      sitenam_colnam <- names(siteinfo)[1]
      names(date_start) <- siteinfo[,1][[sitenam_colnam]]

      ## End year
      year_end <- sapply( siteinfo[,1], function(x) siteinfo$year_end[ which(siteinfo[,1]==x) ] ) %>% as.list()
      date_end <- purrr::map( year_end, ~ymd( paste0( ., "-01-01" ) ) )
      names(date_end) <- siteinfo[,1][[sitenam_colnam]]

      ## Longitude
      lon <- sapply( siteinfo[,1], function(x) siteinfo$lon[ which(siteinfo[,1]==x) ] ) %>% as.list()
      names(lon) <- siteinfo[,1][[sitenam_colnam]]

      ## Latitude
      lat <- sapply( siteinfo[,1], function(x) siteinfo$lat[ which(siteinfo[,1]==x) ] ) %>% as.list()
      names(lat) <- siteinfo[,1][[sitenam_colnam]]

      ## Elevation
      elv <- sapply( siteinfo[,1], function(x) siteinfo$elv[ which(siteinfo[,1]==x) ] ) %>% as.list()
      names(elv) <- siteinfo[,1][[sitenam_colnam]]

      ## Water holding capacity of the soil at the site
      whc <- sapply( siteinfo[,1], function(x) siteinfo$whc[ which(siteinfo[,1]==x) ] ) %>% as.list()
      names(whc) <- siteinfo[,1][[sitenam_colnam]]
      
      ## Complement settings list
      settings$sitenames  <- siteinfo[,1][[sitenam_colnam]]
      settings$date_start <- date_start  # is a list
      settings$date_end   <- date_end    # is a list
      settings$lon        <- lon         # is a list
      settings$lat        <- lat         # is a list
      settings$elv        <- elv         # is a list
      settings$whc        <- whc         # is a list

      ##--------------------------------------
      ## Write site and simulation parameter files
      ##--------------------------------------
      ## Write site parameter files for each site
      print("writing site parameter files...")
      dirnam <- paste0( settings$path_input, "/site_paramfils/" )
      if (!dir.exists(dirnam)) system( paste( "mkdir -p ", dirnam ) ) 
      settings$path_site_paramfil <- purrr::map( as.list(settings$sitenames), ~write_site_parameter_bysite( ., settings ) )
      names(settings$path_site_paramfil) <- siteinfo[,1][[sitenam_colnam]]

      ## Write simulation parameter files for each site
      print("writing simulation parameter files...")
      dirnam <- paste0( settings$path_input, "/run/" )
      if (!dir.exists(dirnam)) system( paste( "mkdir -p ", dirnam ) ) 
      settings$path_simulation_paramfil <- purrr::map( as.list(settings$sitenames), ~write_simulation_parameter_bysite( ., settings ) )
      names(settings$path_simulation_paramfil) <- siteinfo[,1][[sitenam_colnam]]

      ## Write runnames (typically corresponds to site names) belonging to this ensemble into a text file 
      ## located in the run directory.
      settings$path_runnames <- paste0( settings$path_input, "run/runnames_", settings$name, ".txt" )
      zz <- file( settings$path_runnames, "w")
      tmp <- purrr::map( as.list(settings$sitenames), ~cat( ., "\n", file=zz ) )
      close(zz)

      ##--------------------------------------
      ## Link directories
      ##--------------------------------------
      ## use same site and simulation parameter files for cnmodel and cmodel simulations
      if (settings$name == 'fluxnet_fixalloc'){
        ensemble_name <- 'fluxnet_cnmodel'
      } else if (settings$name == 'olson_cmodel'){
        ensemble_name <- 'olson'
      } else if (settings$name == 'campi_cmodel'){
        ensemble_name <- 'campi'
      } else if (settings$name == 'fluxnet2015_cmodel'){
        ensemble_name <- 'fluxnet2015'
      } else {
        ensemble_name <- settings$name
      }

      system( paste0( "unlink ", settings$dir_sofun, "run") )
      system( paste0( "unlink ", settings$dir_sofun, "site_paramfils") )
      system( paste0( "unlink ", settings$dir_sofun, "input/sitedata") )

      system( paste0( "ln -svf ", settings$path_input, "run ", settings$dir_sofun, "run") )
      system( paste0( "ln -svf ", settings$path_input, "site_paramfils ", settings$dir_sofun, "site_paramfils") )
      system( paste0( "ln -svf ", settings$path_input, "sitedata ", settings$dir_sofun, "input/sitedata") )

    }
  }

  ## link output directories (same for lonlat and ensemble)
  system( paste0( "unlink ", settings$dir_sofun, "/output" ) )
  system( paste0( "ln -svf ", settings$path_output, " ", settings$dir_sofun, "/output" ))

  system( paste0( "unlink ", settings$dir_sofun, "/output_nc" ) )
  system( paste0( "ln -svf ", settings$path_output, " ", settings$dir_sofun, "/output_nc" ))

  return(settings)

}
