#' Wrapper for P-model 
#'
#' A wrapper function for running different implementations of the P-model
#' 
#' @param temp Temperature, relevant for photosynthesis (deg C)
#' @param vpd Vapour pressure deficit (Pa)
#' @param co2 Atmospheric CO2 concentration (ppm)
#' @param ppfd (Optional) Photosynthetic photon flux density (mol/m2, defaults to \code{NA})
#' @param fapar (Optional) Fraction of absorbed photosynthetically active radiation (unitless, defaults to \code{NA})
#' @param elv Elevation above sea-level (m.a.s.l.)
#' @param kphio Quantum yield efficiency parameter
#' @param implementation Any of \code{c("R","Fortran")} specifying the implementation of the P-model to be used. Defaults to \code{"R"}.
#' @param sofundir A character string specifying the path of the main directory of the SOFUN repository. Required only for \code{implementation=="Fortran"}.
#'
#' @return
#' 
#' @export
#'
#' @examples out_rpmodel <- pmodel( temp=10, vpd=300, co2=300, ppfd=10, fapar=0.7, elv=300, kphio=0.06 )
#' 
pmodel <- function( temp, vpd, co2, ppfd, fapar, elv, kphio, implementation = "R", sofundir = NA ){

  if ( implementation %in% c("r", "R") || is.na(implementation) ){
    ## Default is running the P-model all in R
    out <- rpmodel( fapar, ppfd, co2, temp, cpalpha = 1.0, vpd, elv, kphio = kphio, returnvar = "gpp" )

  } else if (implementation %in% c( "Fortran", "fortran", "f90", "F90", "Fortran90")){

    here <- getwd() # save current working directory
    setwd( sofundir )    

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
