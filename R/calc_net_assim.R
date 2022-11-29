#' Calculate net assimilation
#' 
#' Net carbon assimilation calculation
#'
#' @param par A vector of parameters \code{c(vcmax, gs)}, with the maximum rate of 
#' RuBisCO carboxylation (\code{vcmax}) in mol C m\eqn{^{-2}} d\eqn{^{-1}}, and 
#' the stomatal conductance (\code{gs}) in mol C m\eqn{^{-2}} d\eqn{^{-1}} Pa\eqn{-1}.
#' @param args A vector of arguments \code{c(kmm, gammastar, ns_star, ca, vpd, beta)},
#' being
#' \describe{
#' \item{\code{kmm}}{Michaelis-Menten coefficient for photosynthesis in Pa.}
#' \item{\code{gammastar}}{Photorespiratory compensation point in Pa.}
#' \item{\code{ns_star}}{Change in the viscosity of water, relative to its value
#' at 25\eqn{^{o}}C, unitless.}
#' \item{\code{ca}}{Ambient CO\eqn{_2} partial pressure, measured in Pa.}
#' \item{\code{vpd}}{Daytime water vapour pressure deficit, measured in Pa.}
#' \item{\code{beta}}{Unit cost ratio, unitless.}
#' } 
#' @param iabs Amount of absorbed light, in mol m\eqn{^{-2}}.
#' @param kphio Quantum yield efficiency parameter, in mol mol\eqn{^{-1}}.
#' @param a_unitcost Unit cost of transpiration ...
#'
#' @return Net carbon assimilation
#' @export

calc_net_assim <- function(
  par,
  args,
  iabs,
  kphio,
  a_unitcost 
  ){
  
  vcmax <- par[1]
  gs    <- par[2]
  
  kmm       <- args[1]
  gammastar <- args[2]
  ns_star   <- args[3]
  ca        <- args[4]
  vpd       <- args[5]
  beta      <- args[6]
  
  ## Get assimilation and ci, given Vcmax and gs
  ## by solving the equation system
  ## assim = vcmax * (ci - gammastar)/(ci + kmm)
  ## assim = gs * (ca - ci)
  a_quad <- -gs
  b_quad <- gs * ca - gs * kmm - vcmax
  c_quad <- gs * ca * kmm + vcmax * gammastar

  root_ci <- try( polyroot( c(c_quad, b_quad, a_quad) ) )

  if (inherits(root_ci, "try-error")){

    return( NA )

  } else {

    ci <- Re(root_ci)[which(Re(root_ci)>0)]   # take positive root
    
    # det <- sqrt( b_quad^2 - 4 * a_quad * c_quad )
    # ci <- -b_quad - det / (2 * a_quad)    # smaller root

    ## A_j
    a_j <- iabs * kphio * (ci - gammastar) / (ci + 2 * gammastar)

    ## A_c
    a_c <- vcmax * (ci - gammastar) / (ci + kmm)

    ## I don't understand if this is correct... 
    # It must be inconsistent with the Vcmax-related ci calculated above.
    assim <- min(a_j, a_c)
    
    # All above is correct. That is, A and ci are correctly
    # back-calculated from Vcmax and gs
    
    ## only cost ratio is defined. for this here we need absolute values.
    # Set randomly
    cost_transp <- a_unitcost * 1.6 * ns_star * gs * vpd
    cost_vcmax  <- a_unitcost * beta * vcmax
    
    ## quantity to maximise
    net_assim <- assim - cost_transp - cost_vcmax
    
    return( net_assim )

  }
}

