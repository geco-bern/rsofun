## Wrapper function for simple setup, running the P-model.
pmodel <- function( temp, vpd, co2, ppfd, fapar, elv, implementation = "r", sofundir = NA ){

  if ( implementation %in% c("r", "R") || is.na(implementation) ){
    ## Default is running the P-model all in R
    out <- rpmodel( fapar, ppfd, co2, temp, cpalpha = 1.0, vpd, elv )
    out <- out$gpp

  } else if (implementation %in% c( "fortran", "Fortran", "f90", "F90", "Fortran90")){

    here <- getwd() # save current working directory
    setwd( settings_sims$dir_sofun )    

    out <- system2( "./rundemo_pmodel", 
                    input = c( as.character(temp), as.character(vpd), as.character(co2), as.character(ppfd), as.character(fapar), as.character(elv) ),
                    stdout = TRUE, stderr = FALSE )
    out <- as.numeric(out)
  
    setwd( here )
    
  } else {

    abort("pmodel(): argument implementation not recognized.")
  
  }

  return( out )

}
