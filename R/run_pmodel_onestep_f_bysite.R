#' Run P-model (single time step)
#' 
#' Run P-model on a single site for a single time step. This does not include the simulation of ecosystem-level quantities,
#' water limitation, nor a simulation of water fluxes. Instead, this corresponds to a leaf-level representation of the 
#' acclimation of photosynthesis.
#'
#' @param lc4 Locigical specifying whether P-model simulation is for C4 (as opposed to C3). Defaults to \code{FALSE}.
#' @param forcing A data frame of forcing climate data, used as input (single row).
#' @param params_modl A named list of free (calibratable) model parameters. See \code{\link{runread_pmodel_f}}
#' @param makecheck A logical specifying whether checks are performed 
#'  to verify forcings and model parameters. \code{TRUE} by default.
#'
#' For further specifications of above inputs and examples see \code{\link{p_model_drivers}} or \code{\link{p_model_drivers_vcmax25}}

#' @import dplyr
#' 
#' @returns Model output is provided as a tidy dataframe, with columns:
#' \describe{
#'   \item{\code{vcmax}}{Maximum rate of RuBisCO carboxylation 
#'       (Vcmax) (in mol C m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{jmax}}{Maximum rate of electron transport for RuBP regeneration
#'       (in mol CO\eqn{_2} m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{vcmax25}}{Maximum rate of carboxylation (Vcmax), 
#'       normalised to 25\eqn{^o}C (in mol C m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{jmax25}}{Maximum rate of electron transport, normalised to 
#'       25\eqn{^o}C (in mol C m\eqn{^{-2}} s\eqn{^{-1}}).}
#'   \item{\code{gs_accl}}{Acclimated stomatal conductance (in 
#'       mol C (mol photons)\eqn{^{-1}} Pa\eqn{^{-1}}. (Multiply by 
#'       ppfd (mol photons m\eqn{^{-2}} d\eqn{^{-1}}) and fapar 
#'       to express per unit ground area and time.)}
#'   \item{\code{chi}}{Ratio of leaf-internal to ambient CO\eqn{_{2}}, ci:ca (unitless).}
#'   \item{\code{iwue}}{Intrinsic water use efficiency (iWUE) (unitless, 
#'       multiply with patm (Pa) to get iWUE in Pa).}
#'   \item{\code{rd}}{Dark respiration (Rd) in gC m\eqn{^{-2}} s\eqn{^{-1}}. 
#'       (Multiply by 1/12 (mol C / gC) to convert to mol C m\eqn{^{-2}} s\eqn{^{-1}}.)}
#'   \item{\code{bigdelta}}{13C isotope discrimination of leaf assimilates 
#'        against atmospheric signature (permil).}
#'   } 
#'   
#' @details TBC
#'
#' @export
#' @useDynLib rsofun
#'
#' @examples
#' # Define model parameter values from previous work
#' params_modl <- list(
#'   kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
#'   kphio_par_a        = 0.0,        # disable temperature-dependence of kphio
#'   kphio_par_b        = 1.0,
#'   beta_unitcostratio = 146.0,
#'   rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
#'   kc_jmax            = 0.41
#' )
#' 
#' # Run the Fortran P-model 
#' run_pmodel_onestep_f_bysite(
#'   lc4 = FALSE,
#'   forcing = data.frame(
#'     temp  = 20,           # temperature, deg C
#'     vpd   = 1000,         # Pa,
#'     ppfd  = 300/10^6,     # mol/m2/s
#'     co2   = 400,          # ppm,
#'     patm  = 101325        # Pa
#'   ),
#'   params_modl = list(
#'     kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
#'     kphio_par_a        = 0.0,        # disable temperature-dependence of kphio
#'     kphio_par_b        = 1.0,
#'     beta_unitcostratio = 146.0,
#'     rd_to_vcmax        = 0.014,      # from Atkin et al. 2015 for C3 herbaceous
#'     kc_jmax            = 0.41
#'   ),
#'   makecheck = TRUE
#' )

run_pmodel_onestep_f_bysite <- function(
    lc4,
    forcing,
    params_modl,
    makecheck = TRUE
){
  
  # base state, always execute the call
  continue <- TRUE
    
  # re-define units and naming of forcing dataframe
  # keep the order of columns - it's critical for Fortran (reading by column number)
  forcing_features <- c(
      'temp',
      'vpd',
      'ppfd',
      'co2',
      'patm'
  )
  forcing <- forcing %>% 
    dplyr::select(all_of(forcing_features))
  
  # validate input
  if (makecheck){

    is.nanull <- function(x) ifelse(any(is.null(x), is.na(x)), TRUE, FALSE)
    
    # list variable to check for
    check_vars <- c(
      "temp",
      "vpd",
      'ppfd',
      "co2",
      "patm"
    )
    
    # create a loop to loop over a list of variables
    # to check validity
    data_integrity <- lapply(check_vars, function(check_var){
      if (any(is.nanull(forcing[check_var]))){
        sitename <- "undefined site"
        warning(sprintf("Error: Missing value in %s for %s",
                        check_var, sitename))
        return(FALSE)
      } else {
        return(TRUE)
      }
    })
    
    # only run simulation if all checked variables are valid
    # suppress warning on coercion of list to single logical
    if (suppressWarnings(!all(data_integrity))){
      continue <- FALSE
    }

     
    # model parameters to check
    if ( sum( names(params_modl) %in% c('kphio', 'kphio_par_a', 'kphio_par_b',
                                        'beta_unitcostratio', 'rd_to_vcmax', 'kc_jmax')
    ) != 6){
      warning(" Returning a dummy data frame. Incorrect model parameters.")
      continue <- FALSE
    }
  }
  
  if (continue){
    ## C wrapper call
    pmodelonestep_out <- .Call(
      'pmodel_onestep_f_C',
      ## Simulation parameters
      lc4       = as.logical(lc4),
      par       = c(as.numeric(params_modl$kphio), # model parameters as vector in order
                    as.numeric(params_modl$kphio_par_a),
                    as.numeric(params_modl$kphio_par_b),
                    as.numeric(params_modl$beta_unitcostratio),
                    as.numeric(params_modl$rd_to_vcmax),
                    as.numeric(params_modl$kc_jmax)),
      forcing   = as.matrix(forcing)
    )
    pmodelonestep_out <- t(pmodelonestep_out)

  } else {
    pmodelonestep_out <- array(dim = c(1,9))
  }

  out <- build_out_pmodel_onestep(pmodelonestep_out)

  return(out)
}

# Build R output
build_out_pmodel_onestep <- function(pmodelonestep_out){
  out <- pmodelonestep_out %>%
      as.matrix() %>% 
      as.data.frame() %>% 
      stats::setNames(
        c("vcmax",
          "jmax", 
          "vcmax25", 
          "jmax25", 
          "gs_accl", 
          "chi", 
          "iwue", 
          "rd",
          "bigdelta"
          )
      ) %>%
      as_tibble(.name_repair = "check_unique")
  
  return(out)
}

.onUnload <- function(libpath) {
  library.dynam.unload("rsofun", libpath)
}
