calc_net_assim <- function( par, args, iabs, kphio, a_unitcost ){
  
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

  if (class(root_ci)=="try-error"){

    return( NA )

  } else {

    ci <- Re(root_ci)[which(Re(root_ci)>0)]   # take positive root
    
    # det <- sqrt( b_quad^2 - 4 * a_quad * c_quad )
    # ci <- -b_quad - det / (2 * a_quad)    # smaller root

    ## A_j
    a_j <- iabs * kphio * (ci - gammastar) / (ci + 2 * gammastar)

    ## A_c
    a_c <- vcmax * (ci - gammastar) / (ci + kmm)

    ## I don't understand if this is correct... It must be inconsistent with the Vcmax-related ci calculated above.
    assim <- min(a_j, a_c)
    
    ## All above is correct. That is, A and ci are correctly back-calculated from Vcmax and gs
    
    ## only cost ratio is defined. for this here we need absolute values. Set randomly
    cost_transp <- a_unitcost * 1.6 * ns_star * gs * vpd
    cost_vcmax  <- a_unitcost * beta * vcmax
    
    ## quantity to maximise
    net_assim <- assim - cost_transp - cost_vcmax
    
    return( net_assim )

  }

}

