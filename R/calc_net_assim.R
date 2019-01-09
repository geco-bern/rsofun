
calc_net_assim <- function( par, args ){
  
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

  root_ci <- polyroot( c(c_quad, b_quad, a_quad) )
  ci <- Re(root_ci)[which(Re(root_ci)>0)]   # take positive root

  # det <- sqrt( b_quad^2 - 4 * a_quad * c_quad )
  # ci <- -b_quad - det / (2 * a_quad)    # smaller root

  assim <- vcmax * (ci - gammastar) / (ci + kmm)
  
  ## only cost ratio is defined. for this here we need absolute values. Set randomly
  any <- 0.001
  
  cost_transp <- any * 1.6 * gs * vpd
  cost_vcmax  <- any * (beta / ns_star) * vcmax
  
  net_assim <- assim - cost_transp - cost_vcmax
  
  return( net_assim )
}
