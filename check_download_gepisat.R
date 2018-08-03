##--------------------------------------------------------------------------
## Checks if GePiSaT files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_gepisat <- function( path, settings_sims, sitename=NA ){

  require(purrr)
  require(dplyr)
  require(rlang)

  ## Check if any data is available in the specified directory
  filelist <- list.files( path, pattern = ".*_daily_GPP.txt" )

  if (length(filelist)==0){
    ## No files found at specified location
    warn( paste0("No files found for GePiSaT in directory ", path) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a certain directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = ".*_daily_GPP.txt", recursive = TRUE )

    if (length(filelist)==0){
     
      ## Search from home
      warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      if (ans=="y"){
        filelist <- list.files( "~/", pattern = ".*_daily_GPP.txt", recursive = TRUE )
      } else {
        ## Still no files found at specified location. Try to download from Imperial CX1 and place in <path>
        warn( "Initiating download from Imperial CX1..." )
        error <- download_gepisat_from_cx1( settings_input )
        filelist <- list.files( path, pattern = ".*_daily_GPP.txt" )
      }

      if (length(filelist)==0){
        ## Still no files found at specified location. Try to download from Imperial CX1 and place in <path>
        warn( "Initiating download from Imperial CX1..." )
        error <- download_gepisat_from_cx1( settings_input )

      }

    }

  }

  if (!is.na(sitename)){
    ## Check if a file is available for a given site
    filelist <- list.files( path, pattern = paste0( sitename, ".*_daily_GPP.txt" ) )

    if (length(filelist)==0){
      ## Download missing file
      error <- download_gepisat_from_cx1_path( path, sitename )
    }

  }

}

##-----------------------------------------------------------
## Manages the path specification for GePiSaT data download from CX1
##-----------------------------------------------------------
download_gepisat_from_cx1 <- function( path ){
  
  require(rlang)

  ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
  if (ans=="y"){
    ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
    if (ans=="y"){
      ans <- readline( prompt = paste0("Are you still happy with downloading to ", path, "? (y/n)") )
      if (ans=="y"){
        error <- download_gepisat_from_cx1_path( path )
      } else {
        path <- readline( prompt = "Please specify a new path: " )
        error <- download_gepisat_from_cx1_path( path )
      }
    } else {
      abort( "GePiSaT data download not possible.")
    }
  } else {
    abort( "GePiSaT data download not possible.")
  }

  return(error)

}

##-----------------------------------------------------------
## Downloads GePiSaT data from CX1
##-----------------------------------------------------------
download_gepisat_from_cx1_path <- function( path, sitename=NA ){

  ## get user name from user
  if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto CX1: " )

  ## the path of GePiSaT daily data on cx1
  origpath <- "/work/bstocker/labprentice/data/gepisat/v3_fluxnet2015/daily_gpp/"

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

    purrr::map( as.list(filelist), ~system( paste0( "rsync -avz ", uname, "@login.cx1.hpc.ic.ac.uk:", origpath, .," ", path ) ) )
    
  }

}



