#' Prepare the SOFUN model setup.
#'
#' Prepares the setup for running SOFUN, including linking parameter, input, and output directories, 
#' creating simulation and site parameter files, and complementing the settings (passed as argument).
#'
#' @param settings A named list containing the simulation settings (see vignette_rsofun.pdf for more information and examples)
#' @param setup A named list containing the model setup specification.
#' @param write_paramfils if \code{TRUE}, simulation and site parameter files are written.
#'
#' @return The complemented list of the settings, passed as argument \code{settings}.
#' @export
#'
#' @examples settings <- prepare_setup_sofun( settings, write_paramfils = TRUE )
#' 
prepare_setup_sofun <- function( settings, setup, write_paramfils = TRUE ){

  ## Make sure the SOFUN model directory exists
  if (!dir.exists(settings$dir_sofun)){
    inform( paste0( "creating directory ", settings$dir_sofun ) )
    system( paste0( "mkdir -p ", settings$dir_sofun ) )
  } 
  
  ##-----------------------------------------------------------
  ## Get SOFUN from github (necessary even if you don't compile
  ## the code because of the parameter files)
  ##-----------------------------------------------------------
  if ( settings$implementation=="fortran"){

    if (!dir.exists(paste0(settings$dir_sofun, "params_std"))){
      ## Assuming SOFUN has not been cloned yet. Clone it now.
      ## First clean directory
      system( paste0( "rm -rf ", settings$dir_sofun, "/*" ) )

      ## first clone into a temporary directory
      rlang::warn("Cloning SOFUN from github...")
      system( paste0( "git clone -b pnmodel https://github.com/stineb/sofun.git ", dirname(settings$dir_sofun) ) )
      
      # ## checkout the required branch of SOFUN, here 'pnmodel' (hard-coded)
      # here <- getwd()
      # setwd( settings$dir_sofun )
      # rlang::warn("Switching to branch pnmodel...")
      # system( "git checkout pnmodel")
      # setwd( here )
      
      rlang::warn("...done.")
      if (!dir.exists(paste0(settings$dir_sofun, "params_std"))) rlang::abort("Aborting. SOFUN could not be cloned for an unknown reason.")
    }

    ## Get executable
    if (setup$do_compile){

      cmd <- paste0("make ", setup$model)
      system( cmd )

    } else if (!file.exists(paste0("run", setup$model))){

      print("Copying executable provided by rsofun and compiled on a 64-bit UNIX machine with gfortran into the SOFUN run directory...")
      system( paste0( "cp ", path.package("rsofun"), "/extdata/run", setup$model, " ." ) )

      if (!file.exists(paste0("run", setup$model))) rlang::abort( paste( "Executable is not available: ", paste0("run", setup$model)) )

    }

  } else {

    rlang::abort("prepare_setup_sofun(): rsofun is not available for other SOFUN implementations than the Fortran one.")

  }

  if (settings$setup!="simple"){

    ## Complement output booleans as FALSE if missing
    settings$loutplant      = ifelse( is.null(settings$loutplant), FALSE, settings$loutplant)
    settings$loutgpp        = ifelse( is.null(settings$loutgpp), FALSE, settings$loutgpp)
    settings$loutwaterbal   = ifelse( is.null(settings$loutwaterbal), FALSE, settings$loutwaterbal)
    settings$loutforcing    = ifelse( is.null(settings$loutforcing), FALSE, settings$loutforcing )

    settings$loutdgpp       = ifelse( is.null(settings$loutdgpp), FALSE, settings$loutdgpp)
    settings$loutdrd        = ifelse( is.null(settings$loutdrd), FALSE, settings$loutdrd )
    settings$loutdtransp    = ifelse( is.null(settings$loutdtransp), FALSE, settings$loutdtransp)
    settings$loutdalpha     = ifelse( is.null(settings$loutdalpha), FALSE, settings$loutdalpha)
    settings$loutdaet       = ifelse( is.null(settings$loutdaet), FALSE, settings$loutdaet  )
    settings$loutdpet       = ifelse( is.null(settings$loutdpet), FALSE, settings$loutdpet  )
    settings$loutdwcont     = ifelse( is.null(settings$loutdwcont), FALSE, settings$loutdwcont)
    settings$loutdtemp      = ifelse( is.null(settings$loutdtemp), FALSE, settings$loutdtemp )
    settings$loutdfapar     = ifelse( is.null(settings$loutdfapar), FALSE, settings$loutdfapar)
    settings$loutdtemp_soil = ifelse( is.null(settings$loutdtemp_soil), FALSE, settings$loutdtemp_soil)

    if (settings$setup=="lonlat"){
      ##-----------------------------------------------------------
      ## LONLAT: Simulation on a spatial grid (longitude/latitude)
      ## In this case, <settings$name> is the simulation name.
      ##-----------------------------------------------------------
      ## Write site parameter files for each site
      ## create new simuation parameter file for this experiment
      dirnam <- paste0( settings$path_input, "/site_paramfils/" )
      if (!dir.exists(dirnam)) system( paste( "mkdir -p ", dirnam ) ) 
      settings$path_site_paramfil <- write_site_parameter_bysite( settings$name, settings )

      ## In this case, the site name (used to define the run name) is name of the whole shabang
      settings$sitenames <- settings$name

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
      settings$path_simulation_paramfil <- write_simulation_parameter_bysite( settings$name, settings )

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

    } else if (settings$setup=="site") {
      ## Site-scale simulation(s)
      
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
      if (!file.exists(settings$path_siteinfo)) rlang::abort( "prepare_setup_sofun(): File specified by settings$path_siteinfo does not exist." )
      siteinfo <- readr::read_csv( settings$path_siteinfo )

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
        settings$path_simulation_paramfil <- purrr::map( as.list(settings$sitenames), ~write_simulation_parameter_bysite( ., settings ) )
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
      #   tmp <- cat( length( calibvars ), "\n", file=zz )
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

  }

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

    if (settings$setup=="lonlat"){

      system( paste0( "cp ", path.package("rsofun"), "/extdata/GRIDFILENAMES_", settings$grid, ".parameter ", path ) )

    } else {

      system( paste0( "cp ", path.package("rsofun"), "/extdata/SITENAME.parameter ", path ) )
      
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
write_simulation_parameter_bysite <- function( sitename, settings_sim ){

  ## create path of the simulation parameter file for this site
  path <- paste0( settings_sim$path_input, "/run/", sitename, ".sofun.parameter" )

  if (settings_sim$implementation=="fortran"){

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

              in_netrad            = settings_sim$in_netrad,
              in_ppfd              = settings_sim$in_ppfd,
              soilmstress          = settings_sim$soilmstress,
              tempstress           = settings_sim$tempstress,

              loutplant            = settings_sim$loutplant,
              loutgpp              = settings_sim$loutgpp,
              loutwaterbal         = settings_sim$loutwaterbal,
              loutforcing          = settings_sim$loutforcing,

              loutdgpp             = settings_sim$loutdgpp,
              loutdrd              = settings_sim$loutdrd,
              loutdtransp          = settings_sim$loutdtransp,
              loutdalpha           = settings_sim$loutdalpha,
              loutdaet             = settings_sim$loutdaet,
              loutdpet             = settings_sim$loutdpet,
              loutdwcont           = settings_sim$loutdwcont,
              loutdtemp            = settings_sim$loutdtemp,
              loutdfapar           = settings_sim$loutdfapar,
              loutdtemp_soil       = settings_sim$loutdtemp_soil,

              lcalibgpp            = ("gpp" %in% settings_sim$calibvars),
              lcalibfapar          = ("fapar" %in% settings_sim$calibvars),
              lcalibtransp         = ("transp" %in% settings_sim$calibvars)
              )

  } else if (settings_sim$implementation=="python"){
    
    print("prepare_setup_sofun.R: write_simulation_parameter_bysite(): python tbc.")

  }

  return( path )

}


create_simulation_parameter_file <- function(
  path,
  simname,
  sitename,
  firstyeartrend,
  spinupyears,
  nyeartrend,
  recycle,
  daily_out_startyr,
  daily_out_endyr,
  outdt                = 1,
  co2filnam,
  noydepfilnam         = NA,
  nhxdepfilnam         = NA,
  noyfertfilnam        = NA,
  nhxfertfilnam        = NA,
  grharvestfilnam      = NA,
  # fapar_forcing_source = NA,
  soilmstress          = FALSE,
  tempstress           = FALSE,
  const_nfert_year     = NA,
  const_clim_year      = NA,
  const_lu_year        = NA,
  const_co2_year       = NA,
  const_ndep_year      = NA,
  in_netrad            = FALSE,
  in_ppfd              = FALSE,
  lTrE                 = FALSE,
  lTNE                 = FALSE,
  lTrD                 = FALSE,
  lTND                 = FALSE,
  lGr3                 = FALSE,
  lGN3                 = FALSE,
  lGr4                 = FALSE,
  loutplant            = FALSE,
  loutgpp              = FALSE,
  loutwaterbal         = FALSE,
  loutforcing          = FALSE,
  loutdgpp             = FALSE,
  loutdrd              = FALSE,
  loutdtransp          = FALSE,
  loutdwcont           = FALSE,
  loutdaet             = FALSE,
  loutdpet             = FALSE,
  loutdalpha           = FALSE,
  loutdtemp            = FALSE,
  loutdfapar           = FALSE,
  loutdtemp_soil       = FALSE,
  lcalibgpp            = FALSE,
  lcalibtransp         = FALSE,
  lcalibfapar          = FALSE
  ){


  system( paste0( "cp ", path.package("rsofun"), "/extdata/EXPNAME.sofun.parameter ", path ) )

  ## pattern replace site name
  system( paste0( "sed -i ", systr, " 's/XXXsitenameXXX/", sitename, "/g' ", path ) )

  ## first simulation year (=start of the experiment - 1)
  system( paste0( "sed -i ", systr, " 's/XXXfirstyeartrendXXX/", format( firstyeartrend, digits=4 ), "/g' ", path ) )

  ## number of spinup years
  system( paste0( "sed -i ", systr, " 's/XXXspinupyearsXXX/", as.character( spinupyears ), "/g' ", path ) )

  ## number of simulation years
  system( paste0( "sed -i ", systr, " 's/XXXnyeartrendXXX/", as.character( nyeartrend ), "/g' ", path ) )

  ## climate recycling periodicity for spinup
  system( paste0( "sed -i ", systr, " 's/XXXrecycleXXX/", as.character( recycle ), "/g' ", path ) )

  ## output years for daily output
  system( paste0( "sed -i ", systr, " 's/XXXdaily_out_startyrXXX/", format( daily_out_startyr, digits=4 ), "/g' ", path ) )
  system( paste0( "sed -i ", systr, " 's/XXXdaily_out_endyrXXX/"  , format( daily_out_endyr  , digits=4 )  , "/g' ", path ) )

  # NetCDF output periodicity (d)
  system( paste0( "sed -i ", systr, " 's/XXXoutdtXXX/", as.character( outdt ), "/g' ", path ) )

  ## CO2 file name
  system( paste0( "sed -i ", systr, " 's/XXXco2filnamXXX/", co2filnam, "/g' ", path ) )

  ## constant conditions flags
  if (!is.na(const_nfert_year)){
    system( paste0( "sed -i ", systr, " 's/XXXconst_nfert_yearXXX/", as.character( const_nfert_year ), "/g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXconst_nfert_yearXXX/", as.character( -9999 ), "/g' ", path ) )  
  }
  if (!is.na(const_clim_year)){
    system( paste0( "sed -i ", systr, " 's/XXXconst_clim_yearXXX/", as.character( const_clim_year ), "/g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXconst_clim_yearXXX/", as.character( -9999 ), "/g' ", path ) )  
  }
  if (!is.na(const_lu_year)){
    system( paste0( "sed -i ", systr, " 's/XXXconst_lu_yearXXX/", as.character( const_lu_year ), "/g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXconst_lu_yearXXX/", as.character( -9999 ), "/g' ", path ) )  
  }
  if (!is.na(const_co2_year)){
    system( paste0( "sed -i ", systr, " 's/XXXconst_co2_yearXXX/", as.character( const_co2_year ), "/g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXconst_co2_yearXXX/", as.character( -9999 ), "/g' ", path ) )  
  }
  if (!is.na(const_ndep_year)){
    system( paste0( "sed -i ", systr, " 's/XXXconst_ndep_yearXXX/", as.character( const_ndep_year ), "/g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXconst_ndep_yearXXX/", as.character( -9999 ), "/g' ", path ) )  
  }

  ## Additional variables prescribed from data
  if (in_netrad){
    system( paste0( "sed -i ", systr, " 's/XXXin_netradXXX/.true./g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXin_netradXXX/.false./g' ", path ) )  
  }
  if (in_ppfd){
    system( paste0( "sed -i ", systr, " 's/XXXin_ppfdXXX/.true./g' ", path ) )  
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXin_ppfdXXX/.false./g' ", path ) )  
  }
  
  ## Ndep file name
  if (!is.na(noydepfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXnoydepfilnamXXX/", noydepfilnam, "/g' ", path ) )
  }
  if (!is.na(nhxdepfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXnhxdepfilnamXXX/", nhxdepfilnam, "/g' ", path ) )
  }

  ## Nfert file name
  if (!is.na(noyfertfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXnoyfertfilnamXXX/", noyfertfilnam, "/g' ", path ) )
  }
  if (!is.na(nhxfertfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXnhxfertfilnamXXX/", nhxfertfilnam, "/g' ", path ) )
  }

  # ## fAPAR forcing file name code ('evi_modissubset' or 'modis')
  # if (!is.na(fapar_forcing_source)){
  #   system( paste0( "sed -i ", systr, " 's/XXXfapar_forcing_sourceXXX/", fapar_forcing_source, "/g' ", path ) )
  # } else {
  #   system( paste0( "sed -i ", systr, " 's/XXXfapar_forcing_sourceXXX/NA/g' ", path ) )
  # }

  ## switch for soil moisture stress function
  if (soilmstress){
    system( paste0( "sed -i ", systr, " 's/XXXsoilmstressXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXsoilmstressXXX/.false./g' ", path ) )
  }  

  ## switch for temperature stress function
  if (tempstress){
    system( paste0( "sed -i ", systr, " 's/XXXtempstressXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXtempstressXXX/.false./g' ", path ) )
  }  

  ## grass harvest file name
  if (!is.na(grharvestfilnam)){
    system( paste0( "sed -i ", systr, " 's/XXXgrharvestfilnamXXX/", grharvestfilnam, "/g' ", path ) )
  }

  ## PFTs activated for this simulation
  if (lTrE){
    system( paste0( "sed -i ", systr, " 's/XXXlTrEXXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlTrEXXX/.false./g' ", path ) )        
  }
  if (lTNE){
    system( paste0( "sed -i ", systr, " 's/XXXlTNEXXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlTNEXXX/.false./g' ", path ) )        
  }
  if (lTrD){
    system( paste0( "sed -i ", systr, " 's/XXXlTrDXXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlTrDXXX/.false./g' ", path ) )        
  }
  if (lTND){
    system( paste0( "sed -i ", systr, " 's/XXXlTNDXXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlTNDXXX/.false./g' ", path ) )        
  }
  if (lGr3){
    system( paste0( "sed -i ", systr, " 's/XXXlGr3XXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlGr3XXX/.false./g' ", path ) )        
  }
  if (lGN3){
    system( paste0( "sed -i ", systr, " 's/XXXlGN3XXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlGN3XXX/.false./g' ", path ) )        
  }
  if (lGr4){
    system( paste0( "sed -i ", systr, " 's/XXXlGr4XXX/.true./g' ", path ) )        
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlGr4XXX/.false./g' ", path ) )        
  }

  ## select outputs to be written
  if (loutplant){
    system( paste0( "sed -i ", systr, " 's/XXXloutplantXXXilXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutplantXXX/.false./g' ", path ) )
  }
  if (loutgpp){
    system( paste0( "sed -i ", systr, " 's/XXXloutgppXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutgppXXX/.false./g' ", path ) )
  }
  if (loutwaterbal){
    system( paste0( "sed -i ", systr, " 's/XXXloutwaterbalXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutwaterbalXXX/.false./g' ", path ) )
  }
  if (loutforcing){
    system( paste0( "sed -i ", systr, " 's/XXXloutforcingXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutforcingXXX/.false./g' ", path ) )
  }
  if (loutdgpp){
    system( paste0( "sed -i ", systr, " 's/XXXloutdgppXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdgppXXX/.false./g' ", path ) )
  }
  if (loutdrd){
    system( paste0( "sed -i ", systr, " 's/XXXloutdrdXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdrdXXX/.false./g' ", path ) )
  }
  if (loutdtransp){
    system( paste0( "sed -i ", systr, " 's/XXXloutdtranspXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdtranspXXX/.false./g' ", path ) )
  }
  if (loutdwcont){
    system( paste0( "sed -i ", systr, " 's/XXXloutdwcontXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdwcontXXX/.false./g' ", path ) )
  }
  if (loutdaet){
    system( paste0( "sed -i ", systr, " 's/XXXloutdaetXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdaetXXX/.false./g' ", path ) )
  }
  if (loutdpet){
    system( paste0( "sed -i ", systr, " 's/XXXloutdpetXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdpetXXX/.false./g' ", path ) )
  }
  if (loutdalpha){
    system( paste0( "sed -i ", systr, " 's/XXXloutdalphaXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdalphaXXX/.false./g' ", path ) )
  }
  if (loutdtemp){
    system( paste0( "sed -i ", systr, " 's/XXXloutdtempXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdtempXXX/.false./g' ", path ) )
  }
  if (loutdfapar){
    system( paste0( "sed -i ", systr, " 's/XXXloutdfaparXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdfaparXXX/.false./g' ", path ) )
  }
  if (loutdtemp_soil){
    system( paste0( "sed -i ", systr, " 's/XXXloutdtemp_soilXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXloutdtemp_soilXXX/.false./g' ", path ) )
  }

  if (lcalibgpp){
    system( paste0( "sed -i ", systr, " 's/XXXlcalibgppXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlcalibgppXXX/.false./g' ", path ) )
  }
  if (lcalibfapar){
    system( paste0( "sed -i ", systr, " 's/XXXlcalibfaparXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlcalibfaparXXX/.false./g' ", path ) )
  }
  if (lcalibtransp){
    system( paste0( "sed -i ", systr, " 's/XXXlcalibtranspXXX/.true./g' ", path ) )
  } else {
    system( paste0( "sed -i ", systr, " 's/XXXlcalibtranspXXX/.false./g' ", path ) )
  }


  # print( paste0( "finished writing ", path ) )

  return( path )

}
