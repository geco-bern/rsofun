#' Updates calibratable parameters.
#'
#' Updates the text files containing model parameters read online by SOFUN.
#'
#' @param params_opt A named list of calibrated parameter values with names corresponding to the SOFUN model parameter names.  
#' @param dir_sofun A character string specifying the path of the SOFUN parent directory.
#' @param setup A list specifying the model setup
#'
#' @return Returns the values that are written into parameter files.
#' @export
#'
update_params <- function( params_opt, dir_sofun, setup ){
  
  system( paste0( "cp ", dir_sofun, "/params_std/* ", dir_sofun, "params/") )

  ## P-model parameters
  if (!setup$model %in% c("swbm", "splash")){

    filn <- paste0( dir_sofun, "/params/params_gpp_pmodel.dat") 
    content <- readLines( filn )
    
    if ("kphio" %in% names(params_opt)){
      linenr <- which( grepl("kphio", content ) )
      len <- str_length(content[linenr])
      paramvals <- str_sub( content[linenr], start = 41, end = len )
      content[linenr] <- str_replace( content[linenr], paramvals, format( params_opt$kphio, digits = 8 ) )
    }
    
    if ("temp_ramp_edge" %in% names(params_opt)){
      linenr <- which( grepl("temp_ramp_edge", content ) )
      len <- str_length(content[linenr])
      paramvals <- str_sub( content[linenr], start = 41, end = len )
      content[linenr] <- str_replace( content[linenr], paramvals, format( params_opt$temp_ramp_edge, digits = 8 ) )
    }
    
    if ("soilm_par_a" %in% names(params_opt)){
      linenr <- which( grepl("soilm_par_a", content ) )
      len <- str_length(content[linenr])
      paramvals <- str_sub( content[linenr], start = 41, end = len )
      content[linenr] <- str_replace( content[linenr], paramvals, format( params_opt$soilm_par_a, digits = 8 ) )
    }
    
    if ("soilm_par_b" %in% names(params_opt)){
      linenr <- which( grepl("soilm_par_b", content ) )
      len <- str_length(content[linenr])
      paramvals <- str_sub( content[linenr], start = 41, end = len )
      content[linenr] <- str_replace( content[linenr], paramvals, format( params_opt$soilm_par_b, digits = 8 ) )
    }
    
    writeLines( content, con = filn )
    
  }
}
