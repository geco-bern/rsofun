update_params <- function( params_opt, settings ){
  require(stringr)
  
  if ("kphio" %in% names(params_opt)){
    filn <- paste0( settings$dir_sofun, "params/params_gpp_pmodel.dat") 
    content <- readLines( filn )
    linenr <- which( grepl("kphio", content ) )
    len <- str_length(content[linenr])
    paramvals <- str_sub( content[linenr], start = 41, end = len )
    content[linenr] <- str_replace( content[linenr], paramvals, format( params_opt$kphio, digits = 8 ) )
    writeLines( content, con = filn )
  }
  return(NULL)
}
