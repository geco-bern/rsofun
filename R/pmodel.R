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
                    stdout = TRUE, stderr = FALSE ) %>%
            stringr::str_squish() %>%
            stringr::str_split( pattern=" ") %>%
            lapply( FUN = as.numeric ) %>% 
            unlist()
  
    ## This has to correspond to the order of elements in the derived type 'outtype_pmodel', see gpp_pmodel.mod.f90
    varnams <- c(
      "gammastar",           # temperature-dependent photorespiratory compensation point (Pa)
      "kmm",                 # Michaelis-Menten coefficient (Pa)
      "ci",                  # leaf-internal partial pressure, (Pa)
      "chi",                 # = ci/ca, leaf-internal to ambient CO2 partial pressure, ci/ca (unitless)
      "iwue",                # intrinsic water use efficiency = A / gs = ca - ci = ca ( 1 - chi ) , unitless
      "lue",                 # light use efficiency (mol CO2 / mol photon)
      "assim",               # leaf-level assimilation rate (mol CO2 m-2 s-1)
      "gs",                  # stomatal conductance to CO2, expressed per units absorbed light (mol H2O m-2 m-1 / (mol light m-2))
      "gpp",                 # gross primary productivity (g CO2 m-2 d-1)
      "vcmax",               # canopy-level maximum carboxylation capacity per unit ground area (mol CO2 m-2 s-1)
      "vcmax25",             # canopy-level Vcmax25 (Vcmax normalized to 25 deg C) (mol CO2 m-2 s-1)
      "vcmax_unitfapar",     # Vcmax per unit fAPAR (mol CO2 m-2 s-1)
      "vcmax_unitiabs",      # Vcmax per unit absorbed light (mol CO2 m-2 s-1 mol-1)
      "ftemp_inst_vcmax",    # Instantaneous temperature response factor of Vcmax (unitless)
      "ftemp_inst_rd",       # Instantaneous temperature response factor of Rd (unitless)
      "rd",                  # Dark respiration (mol CO2 m-2 s-1)
      "rd_unitfapar",        # Dark respiration per unit fAPAR (mol CO2 m-2 s-1)
      "rd_unitiabs",         # Dark respiration per unit absorbed light (mol CO2 m-2 s-1)
      "actnv",               # Canopy-level total metabolic leaf N per unit ground area (g N m-2)
      "actnv_unitfapar",     # Metabolic leaf N per unit fAPAR (g N m-2)
      "actnv_unitiabs",      # Metabolic leaf N per unit absorbed light (g N m-2 mol-1)
      "transp"               # Canopy-level total transpiration rate (g H2O (mol photons)-1)
      )

    names(out) <- varnams
    out <- as.list(out)

    setwd( here )
    
  } else {

    abort("pmodel(): argument implementation not recognized.")
  
  }

  return( out )

}
