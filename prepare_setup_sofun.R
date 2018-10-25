
##-----------------------------------------------------------
## Prepares the setup for running SOFUN with simulation settings defined by argument 'settings'. 
## In the Fortran version, these settings are read in run-time from parameter files (text files, 
## one for each site, and simulation).
##-----------------------------------------------------------
prepare_setup_sofun <- function( settings, settings_calib = NA, write_paramfils = TRUE ){

  require(readr)
  require(dplyr)
  require(lubridate)
  require(purrr)
  require(rlang)

  ## Make sure the SOFUN model directory exists
  if (!dir.exists(settings$dir_sofun)) system( paste0( "mkdir -p ", settings$dir_sofun ) )
  
  ##-----------------------------------------------------------
  ## Get SOFUN from github (necessary even if you don't compile
  ## the code because of the parameter files)
  ##-----------------------------------------------------------
  if (!dir.exists(paste0(settings$dir_sofun, "params_std"))){
    ## Assuming SOFUN has not been cloned yet. Clone it now.
    ## First clean directory
    system( paste0( "rm -rf ", settings$dir_sofun, "/*" ) )

    ## first clone into a temporary directory
    warn("Cloning SOFUN from github...")
    system( paste0( "git clone https://github.com/stineb/sofun.git tmp" ) )
    
    ## then move all its contents to the SOFUN directory and delete the temporary directory
    system( paste0("mv tmp/*  ", settings$dir_sofun ) )
    system( paste0("mv tmp/.git ", settings$dir_sofun ) )
    system( "rm -rf tmp")
    
    ## checkout the required branch of SOFUN, here 'pnmodel' (hard-coded)
    here <- getwd()
    setwd( settings$dir_sofun )
    warn("Switching to branch pnmodel...")
    system( "git checkout pnmodel")
    setwd( here )
    
    warn("...done.")
    if (!dir.exists(paste0(settings$dir_sofun, "params_std"))) abort("Aborting. SOFUN could not be cloned for an unknown reason.")
  }

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

    settings$date_start <- list( ymd( paste0( as.character( settings$firstyeartrend ), "-01-01" ) ) )  # is a list
    names(settings$date_start) <- settings$name

    settings$date_end <- list( ymd( paste0( as.character( settings$firstyeartrend + settings$nyeartrend - 1 ), "-12-31" ) ) )  # is a list
    names(settings$date_end) <- settings$name

    settings$nyears <- list( settings$nyeartrend )
    names(settings$nyears) <- settings$name

    ## change additional settings to a named list
    settings$c3 <- list( settings$c3 )
    names(settings$c3) <- settings$name

    settings$c4 <- list( settings$c4 )
    names(settings$c4) <- settings$name

    ## Write simulation parameter files for each site
    dirnam <- paste0( settings$path_input, "/run/" )
    if (!dir.exists(dirnam)) system( paste( "mkdir -p ", dirnam ) ) 
    settings$path_simulation_paramfil <- write_simulation_parameter_bysite( settings$name, settings, settings_calib = NA )

    ## link input directories
    if (file.exists(paste0(settings$dir_sofun, "run")))            system( paste0( "unlink ", settings$dir_sofun, "run") )
    if (file.exists(paste0(settings$dir_sofun, "site_paramfils"))) system( paste0( "unlink ", settings$dir_sofun, "site_paramfils") )
    if (file.exists(paste0(settings$dir_sofun, "input/global")))   system( paste0( "unlink ", settings$dir_sofun, "input/global") )

    system( paste0( "ln -sf ", settings$path_input, "run ", settings$dir_sofun, "run") )
    system( paste0( "ln -sf ", settings$path_input, "site_paramfils ", settings$dir_sofun, "site_paramfils") )
    
    if (!dir.exists(paste0(settings$dir_sofun, "input"))) system(paste0("mkdir -p ", settings$dir_sofun, "input"))
    system( paste0( "ln -sf ", settings$path_input, "global ", settings$dir_sofun, "input/global") )
    
    if (!dir.exists(paste0( settings$dir_sofun, "params"))) system( paste0( "mkdir ", settings$dir_sofun, "params"))
    system( paste0( "cp ", settings$dir_sofun, "params_std/* ", settings$dir_sofun, "params/") )  # not linking, but copying so that files may be overwritten after calibration


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

      ## Number of years
      nyears <- sapply( siteinfo[,1], function(x) (siteinfo$year_end[ which(siteinfo[,1]==x) ] - siteinfo$year_start[ which(siteinfo[,1]==x) ] + 1) ) %>% as.list()
      names(nyears) <- siteinfo[,1][[sitenam_colnam]]

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

      ## Whether there are C4 grasses at the sites
      c4 <- sapply( siteinfo[,1], function(x) siteinfo$c4[ which(siteinfo[,1]==x) ] ) %>% as.list()
      names(c4) <- siteinfo[,1][[sitenam_colnam]]
      
      ## Complement settings list
      settings$sitenames  <- siteinfo[,1][[sitenam_colnam]]
      settings$date_start <- date_start  # is a list
      settings$date_end   <- date_end    # is a list
      settings$nyears     <- nyears      # is a list
      settings$lon        <- lon         # is a list
      settings$lat        <- lat         # is a list
      settings$elv        <- elv         # is a list
      settings$whc        <- whc         # is a list
      settings$c4         <- c4          # is a list

      ##--------------------------------------
      ## Write site and simulation parameter files
      ##--------------------------------------
      ## Write site parameter files for each site
      if (write_paramfils){
        print("writing site parameter files...")
        dirnam <- paste0( settings$path_input, "/site_paramfils/" )
        if (!dir.exists(dirnam)) system( paste( "mkdir -p ", dirnam ) ) 
        settings$path_site_paramfil <- purrr::map( as.list(settings$sitenames), ~write_site_parameter_bysite( ., settings ) )
        names(settings$path_site_paramfil) <- siteinfo[,1][[sitenam_colnam]]
      }

      ## Write simulation parameter files for each site
      if (write_paramfils){
        print("writing simulation parameter files...")
        dirnam <- paste0( settings$path_input, "/run/" )
        if (!dir.exists(dirnam)) system( paste( "mkdir -p ", dirnam ) ) 
        settings$path_simulation_paramfil <- purrr::map( as.list(settings$sitenames), ~write_simulation_parameter_bysite( ., settings, settings_calib ) )
        names(settings$path_simulation_paramfil) <- siteinfo[,1][[sitenam_colnam]]
      }

      ## Write runnames (typically corresponds to site names) belonging to this ensemble into a text file 
      ## located in the run directory.
      settings$path_runnames <- paste0( settings$path_input, "run/runnames_", settings$name, ".txt" )
      zz <- file( settings$path_runnames, "w")
      tmp <- purrr::map( as.list(settings$sitenames), ~cat( ., "\n", file=zz ) )
      close(zz)

      # ## Write total number of simulation years in this ensemble to file (needed in calibration setup)
      # if (!identical(settings_calib, NA)){
      #   filn_totrunyears <- paste0( settings$path_input, "run/totrunyears_calib.txt" )
      #   zz <- file( filn_totrunyears, "w")
      #   tmp <- purrr::map( unlist(settings$nyears)[names(unlist(settings$nyears)) %in% settings_calib$sitenames] %>% sum(), ~cat( ., "\n", file=zz ) )
      #   close(zz)

      #   ## Write total number of calibration targets to file
      #   filn_nvars_calib <- paste0( settings$path_input, "run/nvars_calib.txt" )
      #   zz <- file( filn_nvars_calib, "w")
      #   tmp <- cat( length( settings_calib$targetvars ), "\n", file=zz )
      #   close(zz)      
      # }
      
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
      
      if (file.exists(paste0(settings$dir_sofun, "run")))            system( paste0( "unlink ", settings$dir_sofun, "run") )
      if (file.exists(paste0(settings$dir_sofun, "site_paramfils"))) system( paste0( "unlink ", settings$dir_sofun, "site_paramfils") )
      if (file.exists(paste0(settings$dir_sofun, "input/sitedata"))) system( paste0( "unlink ", settings$dir_sofun, "input/sitedata") )

      system( paste0( "ln -sf ", settings$path_input, "run ", settings$dir_sofun, "run") )
      system( paste0( "ln -sf ", settings$path_input, "site_paramfils ", settings$dir_sofun, "site_paramfils") )
      if (!dir.exists(paste0(settings$dir_sofun, "input"))) system(paste0("mkdir -p ", settings$dir_sofun, "input"))
      system( paste0( "ln -sf ", settings$path_input, "sitedata ", settings$dir_sofun, "input/sitedata") )
      if (!dir.exists(paste0( settings$dir_sofun, "params"))) system( paste0( "mkdir ", settings$dir_sofun, "params"))
      system( paste0( "cp ", settings$dir_sofun, "params_std/* ", settings$dir_sofun, "params/") )  # not linking, but copying so that files may be overwritten after calibration

    }
  }

  ## link output directories (same for lonlat and ensemble)
  lnk <- paste0( settings$dir_sofun, "/output" )
  src <- strsplit( settings$path_output, "/") %>% unlist() %>% paste(., collapse="/")
  if (file.exists(lnk)) system( paste0( "unlink ", lnk ) )
  if (!file.exists((src))) system( paste0( "mkdir -p ", src ) )
  system( paste0( "ln -sf ", src, " ", lnk ) )

  lnk <- paste0( settings$dir_sofun, "/output_nc" )
  src <- strsplit( settings$path_output_nc, "/") %>% unlist() %>% paste(., collapse="/")
  if (file.exists(lnk)) system( paste0( "unlink ", lnk ) )
  if (!file.exists((src))) system( paste0( "mkdir -p ", src ) )
  system( paste0( "ln -sf ", src, " ", lnk ))

  return(settings)

}

##-----------------------------------------------------------
## Write "site" parameter file into <settings$path_input>/site_paramfils/<settings$grid>.parameter
##
## For lonlat simulations, this contains:
## - landmask file name
## - topography (elevation) file name
## - soil type file name
##
## For site-scale simulations, this contains:
## - longitude
## - latitude
## - altitude (elevevation)
## - soil code (type)
## - whc (water holding capacity)
##
## These files are given for each <settings$grid> identifier, therefore no need to specify them in the settings.
##-----------------------------------------------------------
write_site_parameter_bysite <- function( sitename, settings ){

  ## create path of the site parameter file for this site
  path <- paste0( settings$path_input, "/site_paramfils/", sitename, ".parameter" )

  if (settings$implementation=="fortran"){

    if (settings$lonlat){

      system( paste0( "cp GRIDFILENAMES_", settings$grid, ".parameter ", path ) )

    } else {

      system( paste0( "cp SITENAME.parameter ", path ) )
      
      ## set lon, lat and elv
      system( paste0( "sed -i ", systr, " 's/XXXLONXXX/", format( settings$lon[[sitename]], nsmall=4 ), "/g' ", path ) )
      system( paste0( "sed -i ", systr, " 's/XXXLATXXX/", format( settings$lat[[sitename]], nsmall=4 ), "/g' ", path ) )
      system( paste0( "sed -i ", systr, " 's/XXXALTXXX/", format( settings$elv[[sitename]], nsmall=0 ), "/g' ", path ) )
      system( paste0( "sed -i ", systr, " 's/XXXWHCXXX/", format( settings$whc[[sitename]], nsmall=0 ), "/g' ", path ) )

    }

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
write_simulation_parameter_bysite <- function( sitename, settings_sim, settings_calib=NA ){

  require(lubridate)
  require(dplyr)
  
  ## create path of the simulation parameter file for this site
  path <- paste0( settings_sim$path_input, "/run/", sitename, ".sofun.parameter" )

  if (settings_sim$implementation=="fortran"){

    source("create_simulation_parameter_file.R")
    path <- create_simulation_parameter_file( 
              path                 = path,
              simname              = sitename,
              sitename             = sitename,
              firstyeartrend       = settings_sim$date_start[[sitename]] %>% year(),
              spinupyears          = settings_sim$spinupyears,
              nyeartrend           = settings_sim$date_end[[sitename]] %>% year() - settings_sim$date_start[[sitename]] %>% year() + 1,
              recycle              = settings_sim$recycle,
              daily_out_startyr    = settings_sim$date_start[[sitename]] %>% year(),
              daily_out_endyr      = settings_sim$date_end[[sitename]] %>% year(),
              co2filnam            = "cCO2_rcp85_const850-1765.dat",
              lGr3                 = ifelse( is.na( settings_sim$c4[[sitename]] ), TRUE,  ifelse( settings_sim$c4[[sitename]]==TRUE, FALSE, TRUE  ) ),
              lGr4                 = ifelse( is.na( settings_sim$c4[[sitename]] ), FALSE, ifelse( settings_sim$c4[[sitename]]==TRUE, TRUE, FALSE  ) ),
              # fapar_forcing_source = ifelse( settings_sim$lonlat, settings_sim$fapar_forcing_source, NA ),
              in_ppfd              = settings_sim$in_ppfd,
              soilmstress          = settings_sim$soilmstress,
              tempstress           = settings_sim$tempstress ,
              loutplant            = settings_sim$loutplant,
              loutgpp              = settings_sim$loutgpp,
              loutwaterbal         = settings_sim$loutwaterbal,
              loutdtemp_soil       = settings_sim$loutdtemp_soil,
              loutdgpp             = settings_sim$loutdgpp,
              loutdrd              = settings_sim$loutdrd,
              loutdtransp          = settings_sim$loutdtransp,
              lncoutdtemp          = settings_sim$lncoutdtemp,
              lncoutdfapar         = settings_sim$lncoutdfapar,
              lncoutdgpp           = settings_sim$lncoutdgpp, 
              lncoutdwaterbal      = settings_sim$lncoutdwaterbal,
              lcalibgpp            = ifelse( identical(settings_calib, NA), FALSE, ("gpp" %in% settings_calib$targetvars) ),
              lcalibfapar          = ifelse( identical(settings_calib, NA), FALSE, ("fapar" %in% settings_calib$targetvars) ),
              lcalibtransp         = ifelse( identical(settings_calib, NA), FALSE, ("transp" %in% settings_calib$targetvars) )
              )

  } else if (settings_sim$implementation=="python"){
    
    print("prepare_setup_sofun.R: write_simulation_parameter_bysite(): python tbc.")

  }

  return( path )

}
