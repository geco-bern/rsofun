#' Download file from a remote server.
#'
#' Downloads all files in a given directly from remote server, or only files that contain 'pattern'
#' 
#' @param dir_remote A character string specifying the path of the directory that contains files to be downloaded from the remote server.
#' @param dir_local A character string specifying the path of the directory that contains files to be downloaded to the local machine.
#' @param pattern (Optional) A character string of the pattern used to filter files to be downloaded.
#' @param uname A character string specifying the user name for the log in on the remote server.
#' @param address_remote A character string specifying the address of the login node of the remote server.
#' @param getfiles (Optional) A vector of character strings specifying the names of the files that are to be downloaded.
#'
#' @return TODO
#' @export
#'
#' @examples
#' 
download_from_remote <- function( dir_remote, dir_local, pattern = NA, uname = NULL, address_remote = NULL, getfiles = NA ){

  error <- 0

  ## get user name from user
  if (!exists("uname") && is.null(uname)) uname <<- readline( prompt = "Enter your user name for logging onto remote server: " )
  if (is.null(address_remote))   address_remote <<- readline( prompt = "Enter your address of remote server: " )

  if (is.na(pattern) && is.na(getfiles)){
    ## Download all files in the specified remote directory
    ## create required directory
    if (!dir.exists(dir_local)) system( paste0("mkdir -p ", dir_local ) )

    system( paste0( "rsync -avz ", uname, "@", address_remote, ":", dir_remote, " ", dir_local ) )

  } else {
    ## Download only data for a specific site or only specific files, given their full name

    if (!is.na(pattern)&&is.na(getfiles)){
      ## Define file names of files to download, given 'pattern'
      ## get a file list of what's on CX1
      getfiles <- system( paste0( "ssh ", uname, "@", address_remote, " ls ", dir_remote ), intern = TRUE )

      ## use only file(s) for this site
      getfiles <- getfiles[ grepl(pattern, getfiles) ]
    
    } else if (!is.na(pattern)&&!is.na(getfiles)){

      abort("download_from_remote_path(): Specify only one argument of getfiles and pattern.")

    }

    ## create required directory locally
    if (!dir.exists(dir_local)) system( paste0("mkdir -p ", dir_local ) ) 

    ## Check if required files are in sub-directories. If so, create them locally as on remote
    subdir <- getfiles %>% dirname() %>% unique()
    if (subdir!="" && !dir.exists(paste0( dir_local, "/", subdir ))) system( paste0("mkdir -p ", paste0( dir_local, "/", subdir ) ) )

    if (length(getfiles)==0){
      ## no data available for this site
      error <- 1
      rlang::warn(paste0("No files available for ", dir_remote ) )
    } else {
      if (subdir==""){
        error <- purrr::map( as.list(getfiles), ~system( paste0( "rsync -avz ", uname, "@", address_remote, ":", dir_remote, .," ", dir_local ) ) )    
      } else {
        error <- purrr::map( as.list(getfiles[1]), ~system( paste0( "rsync -avz ", uname, "@", address_remote, ":", origpath, ., " ", paste0( dir_local, subdir ) ) ) )
      }

    }

  }
  return( error )
}

# ##-----------------------------------------------------------
# ## Manages the path specification for MODIS FPAR data download from CX1
# ##-----------------------------------------------------------
# download_from_remote <- function( dir_remote, dir_local, pattern = NA, uname = NULL, address_remote = NULL, getfiles = NA ){

#   ans <- readline( prompt = paste0("Are you still happy with downloading to ", dir_local, "? (y/n)") )
#   if (ans=="y"){
#     error <- download_from_remote_path( dir_remote, dir_local, pattern, uname, address_remote, getfiles )
#   } else {
#     dir_local <- readline( prompt = "Please specify a new path: " )
#     error <- download_from_remote_path( dir_remote, dir_local, pattern, uname, address_remote, getfiles )
#   }

#   return(error)

# }

