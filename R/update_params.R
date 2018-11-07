#' Update parameters
#'
#' TODO: Describe
#'
#' @param params_opt TODO
#' @param settings TODO
#'
#' @return
#' @export
#'
# @examples
update_params <- function( params_opt, settings ){
  system( paste0( "cp ", settings$dir_sofun, "params_std/* ", settings$dir_sofun, "params/") )
  
  if ("kphio" %in% names(params_opt)){
    filn <- paste0( settings$dir_sofun, "params/params_gpp_pmodel.dat") 
    content <- readLines( filn )
    linenr <- which( grepl("kphio", content ) )
    len <- str_length(content[linenr])
    paramvals <- str_sub( content[linenr], start = 41, end = len )
    content[linenr] <- str_replace( content[linenr], paramvals, format( params_opt$kphio, digits = 8 ) )
    writeLines( content, con = filn )
  }

  if ("temp_ramp_edge" %in% names(params_opt)){
    filn <- paste0( settings$dir_sofun, "params/params_gpp_pmodel.dat") 
    content <- readLines( filn )
    linenr <- which( grepl("temp_ramp_edge", content ) )
    len <- str_length(content[linenr])
    paramvals <- str_sub( content[linenr], start = 41, end = len )
    content[linenr] <- str_replace( content[linenr], paramvals, format( params_opt$temp_ramp_edge, digits = 8 ) )
    writeLines( content, con = filn )
  }

  if ("soilm_par_a" %in% names(params_opt)){
    filn <- paste0( settings$dir_sofun, "params/params_gpp_pmodel.dat") 
    content <- readLines( filn )
    linenr <- which( grepl("soilm_par_a", content ) )
    len <- str_length(content[linenr])
    paramvals <- str_sub( content[linenr], start = 41, end = len )
    content[linenr] <- str_replace( content[linenr], paramvals, format( params_opt$soilm_par_a, digits = 8 ) )
    writeLines( content, con = filn )
  }

  if ("soilm_par_b" %in% names(params_opt)){
    filn <- paste0( settings$dir_sofun, "params/params_gpp_pmodel.dat") 
    content <- readLines( filn )
    linenr <- which( grepl("soilm_par_b", content ) )
    len <- str_length(content[linenr])
    paramvals <- str_sub( content[linenr], start = 41, end = len )
    content[linenr] <- str_replace( content[linenr], paramvals, format( params_opt$soilm_par_b, digits = 8 ) )
    writeLines( content, con = filn )
  }

  return(NULL)
}
