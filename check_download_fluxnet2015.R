##--------------------------------------------------------------------------
## Checks if FLUXNET 2015 files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_fluxnet2015 <- function( settings_input, settings_sims, sitename = NA ){

  ## Check if any data is available in the specified directory
  filelist <- list.files( settings_input$path_fluxnet2015, pattern = "FLX_.*_FLUXNET2015_FULLSET_DD.*.csv" )

  if (length(filelist)==0){
    ## No files found at specified location
    warn( paste0("No files found for fluxnet2015 in directory ", settings_input$path_fluxnet2015) )

    ## Search at a different location?
    settings_input$path_fluxnet2015 <- readline( prompt="Would you like to search for files recursively from a certain directory? Enter the path from which search is to be done: ")
    filelist <- list.files( settings_input$path_fluxnet2015, pattern = "FLX_.*_FLUXNET2015_FULLSET_DD.*.csv", recursive = TRUE )

    if (length(filelist)==0){
      ## Search from home
      warn( paste0("Still nothing found at specified location ", settings_input$path_fluxnet2015 ) )

      ## Search recursively from home directory?
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      if (ans=="y") filelist <- list.files( "~/", pattern = "FLX_.*_FLUXNET2015_FULLSET_DD.*.csv", recursive = TRUE )

      if (length(filelist)==0){
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_fluxnet2015>
        abort( "No FLUXNET 2015 files downloaded." )

      } else {

        if (!is.na(sitename)){
          ## Download only data for a specific site
          ## get a file list of what's on the remote server
          getfiles <- system( paste0( "ssh ", settings_input$uname, "@", settings_input$address_remote, " ls ", settings_input$path_remote_fluxnet2015 ), intern = TRUE )

          ## use one file(s) for this site
          getfiles <- getfiles[ grepl(sitename, getfiles) ]
          getfiles <- getfiles[ grepl("_FLUXNET2015_FULLSET_DD_", getfiles) ]
        } else {
          getfiles <- NA
        }

        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_fluxnet2015>
        warn( "Initiating download from remote server..." )
        error <- download_from_remote( 
          settings_input$path_remote_fluxnet2015,
          settings_input$path_fluxnet2015,
          getfiles = getfiles,
          uname = settings_input$uname, 
          address_remote = settings_input$address_remote 
         )
        getfiles <- list.files( settings_input$path_fluxnet2015, pattern = "FLX_.*_FLUXNET2015_FULLSET_DD.*.csv" )
      }

    }

  }

}
