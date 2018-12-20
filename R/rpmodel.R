#' P-model in R
#'
#' R implementation of the P-model and its corrolary predictions (Prentice et al., 2014; Han et al., 2017)
#' 
#' @param tc Temperature, relevant for photosynthesis (deg C)
#' @param vpd Vapour pressure deficit (Pa)
#' @param co2 Atmospheric CO2 concentration (ppm)
#' @param elv Elevation above sea-level (m.a.s.l.)
#' @param kphio Quantum yield efficiency parameter
#' @param fapar (Optional) Fraction of absorbed photosynthetically active radiation (unitless, defaults to \code{NA})
#' @param ppfd (Optional) Photosynthetic photon flux density (mol/m2, defaults to \code{NA})
#' @param method (Optional) A character string specifying which method is to be used for calculating Vcmax and light use efficiency. Defaults to \code{"wanghan"}, 
#' based on Wang Han et al. 2017 Nature Plants. Available is also \code{"smith"}, following the method by Smith et al., 2019 Ecology Letters.
#' @param do_ftemp_kphio (Optional) A logical specifying whether temperature-dependence of quantum yield efficiency after Bernacchi et al., 2003 PCE 
#' is to be accounted for. Defaults to \code{TRUE}.
#' @param returnvar (Optional) A character string of vector of character strings specifying which variables are to be returned (see return below).
#'
#' @return A named list of numeric values with 
#' \itemize{
#'         \item \code{gammastar}: photorespiratory compensation point, (Pa)
#'         \item \code{kmm}: Michaelis-Menten coefficient for photosynthesis (Pa)
#'         \item \code{ci}: leaf-internal partial pressure, (Pa)
#'         \item \code{chi}: = ci/ca, leaf-internal to ambient CO2 partial pressure, ci/ca (unitless)
#'         \item \code{iwue}: intrinsic water use efficiency (unitless)
#'         \item \code{lue}: light use efficiency (mol CO2 / mol photon)
#'         \item \code{gpp}: gross primary productivity (g C m-2, calculated only if fAPAR and PPFD are not 'dummy')
#'         \item \code{vcmax}: maximum carboxylation capacity per unit ground area (mol CO2 m-2 s-1)
#'         \item \code{vcmax25}: Vcmax25 (vcmax normalized to 25 deg C) (mol CO2 m-2 s-1)
#'         \item \code{vcmax_unitfapar}: Vcmax per fAPAR (mol CO2 m-2 s-1)
#'         \item \code{vcmax_unitiabs}: Vcmax per unit absorbed light (xxx units)
#'         \item \code{rd}: Dark respiration (mol CO2 m-2 s-1)
#'         \item \code{rd_unitfapar}: Dark respiration per fAPAR (mol CO2 m-2 s-1)
#'         \item \code{rd_unitiabs}: Dark respiration per unit absorbed light (mol CO2 m-2 s-1)
#'         \item \code{actnv}: Active metabolic leaf N (canopy-level), mol N/m2-ground
#'         \item \code{actnv_unitfapar}: Active metabolic leaf N (leaf-level, top of canopy), mol N/m2-leaf
#'         \item \code{actnv_unitiabs}: Active metabolic leaf N per unit absorbed light, mol N/m2/mol
#' }  
#' 
#' @export
#'
#' @examples out_rpmodel <- rpmodel( tc=10, vpd=300, co2=300, elv=300, kphio=0.06 )
#' 
rpmodel <- function( tc, vpd, co2, elv, kphio, fapar = NA, ppfd = NA, method="wanghan", do_ftemp_kphio = TRUE, returnvar = NULL ){
  #-----------------------------------------------------------------------
  # Output:   list of P-model predictions:
  #
  # ci               : leaf-internal partial pressure, (Pa)
  # chi              : = ci/ca, leaf-internal to ambient CO2 partial pressure, ci/ca (unitless)
  # iwue             : intrinsic water use efficiency (unitless)
  # lue              : light use efficiency (mol CO2 / mol photon)
  # gpp              : gross primary productivity (g C m-2, calculated only if fAPAR and PPFD are not 'dummy')
  # vcmax            : maximum carboxylation capacity per unit ground area (mol CO2 m-2 s-1)
  # vcmax25          : Vcmax25 (vcmax normalized to 25 deg C) (mol CO2 m-2 s-1)
  # vcmax_unitfapar  : Vcmax per fAPAR (mol CO2 m-2 s-1)
  # vcmax_unitiabs   : Vcmax per unit absorbed light (xxx units)
  # rd               : Dark respiration (mol CO2 m-2 s-1)
  # rd_unitfapar     : Dark respiration per fAPAR (mol CO2 m-2 s-1)
  # rd_unitiabs      : Dark respiration per unit absorbed light (mol CO2 m-2 s-1)
  # actnv            : Active metabolic leaf N (canopy-level), mol N/m2-ground
  # actnv_unitfapar  : Active metabolic leaf N (leaf-level, top of canopy), mol N/m2-leaf
  # actnv_unitiabs   : Active metabolic leaf N per unit absorbed light, mol N/m2/mol
  #-----------------------------------------------------------------------

  #-----------------------------------------------------------------------
  # Fixed parameters
  #-----------------------------------------------------------------------
  c_molmass <- 12.0107  # molecular mass of carbon (g)
  kPo   <- 101325.0     # standard atmosphere, Pa (Allen, 1973)
  kTo   <- 25.0         # base temperature, deg C (Prentice, unpublished)
  # beta <- 244.033
  beta <- 146.0         # unit cost ratio (see Prentice et al.,2014)
  rd_to_vcmax <- 0.015  # Ratio of Rdark to Vcmax25, number from Atkin et al., 2015 for C3 herbaceous
    
  # Metabolic N ratio (N per unit Vcmax)
  # Reference: Harrison et al., 2009, Plant, Cell and Environment; Eq. 3
  #-----------------------------------------------------------------------
  mol_weight_rubisco <- 5.5e5    # molecular weight of Rubisco, (g R)(mol R)-1
  n_conc_rubisco     <- 1.14e-2  # N concentration in rubisco, (mol N)(g R)-1
  cat_turnover_per_site <- 2.33  # catalytic turnover rate per site at 25 deg C, (mol CO2)(mol R sites)-1; use 2.33 instead of (3.5) as not all Rubisco is active (see Harrison et al., 2009)  
  cat_sites_per_mol_R   <- 8.0   # number of catalytic sites per mol R, (mol R sites)(mol R)-1

  # Metabolic N ratio (mol N s (mol CO2)-1 )
  n_v <- mol_weight_rubisco * n_conc_rubisco / ( cat_turnover_per_site * cat_sites_per_mol_R )

  ## parameters for Narea -- under construction
  # sla <- 0.0014       # specific leaf area (m2/gC)

  # N in cell walls: Slope of WN~LMA is 0.0002 mol N / g leaf mass (Hikosaka&Shigeno, 2009)
  # With 0.5 g C / g leaf mass and 14 g N / mol N: n_cw = 0.0056 g N / g C

  # ncw <- 0.0056          # N:C ratio in cell walls, working hypothesis: leaf N is solely determined by Vcmax25
  # n_v  <- 1.0/40.96    # gN ??mol-1 s-1. Value 40.96 is 'sv' in Table 2 in Kattge et al., 2009, GCB, C3 herbaceous
  ## -- under construction

  #-----------------------------------------------------------------------
  # Temperature dependence of quantum yield efficiency
  #-----------------------------------------------------------------------
  ## 'do_ftemp_kphio' is not actually a stress function, but is the temperature-dependency of 
  ## the quantum yield efficiency after Bernacchi et al., 2003 PCE
  if (do_ftemp_kphio){
    ftemp_kphio <- calc_ftemp_kphio( tc )
  } else {
    ftemp_kphio <- 1.0
  }

  #-----------------------------------------------------------------------
  # Photosynthesis model parameters depending on temperature, pressure, and CO2.
  #-----------------------------------------------------------------------
  ## atmospheric pressure as a function of elevation (Pa)
  patm <- calc_patm( elv )

  ## ambient CO2 partial pression (Pa)
  ca <- co2_to_ca( co2, patm )

  ## photorespiratory compensation point - Gamma-star (Pa)
  gammastar <- calc_gammastar( tc, patm = NA )     ## XXX Todo: replace 'NA' here with 'patm'

  ## Michaelis-Menten coef. (Pa)
  kmm <- calc_k( tc, patm )   ## XXX Todo: replace 'NA' here with 'patm'

  ## viscosity correction factor = viscosity( temp, press )/viscosity( 25 degC, 1013.25 Pa) 
  ns      <- calc_viscosity_h2o( tc, patm )  # Pa s 
  ns25    <- calc_viscosity_h2o( kTo, kPo )  # Pa s 
  ns_star <- ns / ns25  # (unitless)

  ##-----------------------------------------------------------------------
  ## Optimal ci
  ##-----------------------------------------------------------------------
  ## The heart of the P-model: calculate ci:ca ratio (chi) and additional terms
  out_lue <- lue_vpd_full( kmm, gammastar, ns_star, ca, vpd, beta  )

  ## leaf-internal CO2 partial pressure (Pa)
  ci <- out_lue$chi * ca  

  ##-----------------------------------------------------------------------
  ## Corrolary preditions
  ##-----------------------------------------------------------------------
  # ## stomatal conductance
  # gs <- gpp  / ( ca - ci )

  ## intrinsic water use efficiency 
  iwue = ( ca - ci ) / ( 1.6 * patm )

  ##-----------------------------------------------------------------------
  ## Vcmax and light use efficiency
  ##-----------------------------------------------------------------------
  if (method=="smith"){

    ## constants
    theta <- 0.85    # should be calibratable?
    c_cost <- 0.05336251

    # mc <- (ci - gammastar) / (ci + kmm)                       # Eq. 6
    # print(paste("mc should be equal: ", mc, out_lue$mc ) )

    # mj <- (ci - gammastar) / (ci + 2.0 * gammastar)           # Eq. 8
    # print(paste("mj should be equal: ", mj, out_lue$mj ) )

    # mjov <- (ci + kmm) / (ci + 2.0 * gammastar)               # mj/mc, used in several instances below
    # print(paste("mjov should be equal: ", mjov, out_lue$mjov ) )

    omega <- calc_omega( theta = theta, c_cost = c_cost, m = out_lue$mj )             # Eq. S4
    omega_star <- 1.0 + omega - sqrt( (1.0 + omega)^2 - (4.0 * theta * omega) )       # Eq. 18
    
    # calculate Vcmax-star, which corresponds to Vcmax at a reference temperature 'tcref'
    vcmax_unitiabs_star  <- kphio * ftemp_kphio * out_lue$mjov * omega_star / (8.0 * theta)               # Eq. 19
    
    ## tcref is the optimum temperature in K, assumed to be the temperature at which Vcmax* is operating. 
    ## tcref is estimated based on its relationship to growth temperature following Kattge & Knorr 2007
    tcref <- 0.44 * tc + 24.92

    ## calculated acclimated Vcmax at prevailing growth temperatures
    ftemp_inst_vcmax <- calc_ftemp_inst_vcmax( tc, tc, tcref = tcref )
    vcmax_unitiabs <- vcmax_unitiabs_star * ftemp_inst_vcmax   # Eq. 20
    
    ## calculate Jmax
    jmax_over_vcmax <- (8.0 * theta * omega) / (out_lue$mjov * omega_star)             # Eq. 15 / Eq. 19
    jmax_prime <- jmax_over_vcmax * vcmax_unitiabs 

    ## light use efficiency
    lue <- c_molmass * kphio * ftemp_kphio * out_lue$mj * omega_star / (8.0 * theta) # * calc_ftemp_inst_vcmax( tc, tc, tcref = tcref )     # treat theta as a calibratable parameter

    ## xxx test
    m     <- out_lue$mj
    mc    <- out_lue$mc
    jvrat <- jmax_over_vcmax


  } else if (method=="wanghan"){

    ## Include effect of Jmax limitation
    mprime <- calc_mprime( out_lue$mj )

    ## Light use efficiency (gpp per unit absorbed light)
    lue <- kphio * ftemp_kphio * mprime * c_molmass

    ## Vcmax normalised per unit absorbed PPFD (assuming iabs=1), without Jmax limitation
    vcmax_unitiabs <- kphio * ftemp_kphio * out_lue$mjov

    ## xxx test
    omega       <- NA
    m           <- NA
    mc          <- NA
    omega_star  <- NA
    vcmax_unitiabs_star <- NA
    vcmax_star  <- NA
    vcmax_prime <- NA
    jvrat       <- NA
    jmax_prime  <- NA
    ftemp_inst_vcmax <- NA


  } else {

    rlang::abort("rpmodel(): argument method not idetified.")

  }


  ##-----------------------------------------------------------------------
  ## Corrolary preditions (This is prelimirary!)
  ##-----------------------------------------------------------------------

  ## Vcmax25 (vcmax normalized to 25 deg C)
  ftemp25_inst_vcmax  <- calc_ftemp_inst_vcmax( tc, tc, tcref = 25.0 )
  vcmax25_unitiabs  <- vcmax_unitiabs  / ftemp25_inst_vcmax

  ## Dark respiration at growth temperature
  ftemp_inst_rd <- calc_ftemp_inst_rd( tc )
  rd_unitiabs  <- rd_to_vcmax * (ftemp_inst_rd / ftemp25_inst_vcmax) * vcmax_unitiabs 

  ## active metabolic leaf N (canopy-level), mol N/m2-ground (same equations as for nitrogen content per unit leaf area, gN/m2-leaf)
  actnv_unitiabs  <- vcmax25_unitiabs  * n_v


  if (!is.na(ppfd)){
    ##-----------------------------------------------------------------------
    ## Calculate quantities scaling with light assuming fAPAR = 1
    ## representing leaf-level at the top of the canopy.
    ##-----------------------------------------------------------------------
    ## Vcmax normalised per unit fAPAR (assuming fAPAR=1)
    vcmax_unitfapar <- ppfd * vcmax_unitiabs

    ## Vcmax25 (vcmax normalized to 25 deg C)
    vcmax25_unitfapar <- ppfd * vcmax25_unitiabs

    ## Dark respiration per unit fAPAR (assuming fAPAR=1)
    rd_unitfapar <- ppfd * rd_unitiabs

    ## active metabolic leaf N (canopy-level), mol N/m2-ground (same equations as for nitrogen content per unit leaf area, gN/m2-leaf)
    actnv_unitfapar <- ppfd * actnv_unitiabs


    if (!is.na(fapar)){
      ##-----------------------------------------------------------------------
      ## Calculate quantities scaling with absorbed light
      ##-----------------------------------------------------------------------
      ## absorbed photosynthetically active radiation (mol/m2)
      iabs <- fapar * ppfd 

      ## Canopy-level quantities 
      ## Defined per unit ground level -> scaling with aborbed light (iabs)
      ##-----------------------------------------------------------------------
      ## Gross primary productivity
      gpp <- iabs * lue  # in g C m-2 s-1

      ## Vcmax per unit ground area is the product of the intrinsic quantum 
      ## efficiency, the absorbed PAR, and 'n'
      vcmax <- iabs * vcmax_unitiabs

      ## (vcmax normalized to 25 deg C)
      vcmax25 <- iabs * vcmax25_unitiabs

      ## Dark respiration
      rd <- iabs * rd_unitiabs

      ## active metabolic leaf N (canopy-level), mol N/m2-ground (same equations as for nitrogen content per unit leaf area, gN/m2-leaf)
      actnv <- iabs * actnv_unitiabs

      ## xxx test
      vcmax_star  <- iabs * vcmax_unitiabs_star

    } else {

      gpp <- NA
      vcmax <- NA
      vcmax25 <- NA
      rd <- NA
      actnv <- NA

    }

  } else {

    vcmax_unitfapar <- NA
    vcmax25_unitfapar <- NA
    rd_unitfapar <- NA
    actnv_unitfapar <- NA

    gpp <- NA
    vcmax <- NA
    vcmax25 <- NA
    rd <- NA
    actnv <- NA
    
  }

  ## construct list for output
  out <- list( 
              gammastar       = gammastar,
              kmm             = kmm,
              ci              = ci,
              chi             = out_lue$chi,
              iwue            = iwue,
              lue             = lue,
              gpp             = gpp,        

              ## additional for testint:----------------
              ftemp_inst_vcmax = ftemp_inst_vcmax,
              omega           = omega,
              m               = m,
              mc              = mc,
              omega_star      = omega_star,
              vcmax_star      = vcmax_star,
              vcmax_unitiabs_star = vcmax_unitiabs_star,
              jvrat           = jvrat,
              jmax_prime      = jmax_prime,
              ##-----------------------------------------

              vcmax           = vcmax,    
              vcmax25         = vcmax25,
              vcmax_unitfapar = vcmax_unitfapar,
              vcmax_unitiabs  = vcmax_unitiabs,
              rd              = rd,          
              rd_unitfapar    = rd_unitfapar,          
              rd_unitiabs     = rd_unitiabs, 
              actnv           = actnv,    
              actnv_unitfapar = actnv_unitfapar, 
              actnv_unitiabs  = actnv_unitiabs
              )

  if (!is.null(returnvar)) out <- out[returnvar]

  return( out )

}

# From Nick's code:
# calculate omega for vcmax calculation
calc_omega <- function( theta, c_cost, m ){
  
  cm <- 4 * c_cost / m                        # simplification term for omega calculation
  v  <- 1/(cm * (1 - theta * cm)) - 4 * theta # simplification term for omega calculation
  
  # account for non-linearities at low m values
  capP <- (((1/1.4) - 0.7)^2 / (1-theta)) + 3.4
  aquad <- -1
  bquad <- capP
  cquad <- -(capP * theta)
  m_star <- (4 * c_cost) / polyroot(c(aquad, bquad, cquad))
  
  omega <- ifelse(  m < Re(m_star[1]), 
                    -( 1 - (2 * theta) ) - sqrt( (1 - theta) * v), 
                    -( 1 - (2 * theta))  + sqrt( (1 - theta) * v)
                    )
  
  return(omega)

}

lue_approx <- function( temp, vpd, elv, ca, gs ){
  #-----------------------------------------------------------------------
  # Input:    - float, 'temp' : deg C, air temperature
  #           - float, 'vpd'  : Pa, vapour pressure deficit
  #           - float, 'elv'  : m, elevation above sea level
  #           - float, 'ca'   : Pa, ambient CO2 partial pressure
  #           - float, 'gs'   : Pa, photores. comp. point (Gamma-star)
  # Output:   list: 'm' (unitless), 'chi' (unitless)
  # Features: Returns list containing light use efficiency (m) and ci/ci ratio (chi)
  #           based on the approximation of the theoretical relationships
  #           of chi with temp, vpd, and elevation.
  #           Is now based on SI units as inputs.
  #-----------------------------------------------------------------------
  ## Wang-Han Equation
  whe <- exp( 
    4.644
    + 0.0545 * ( temp - 25.0 )
    - 0.5 * log( vpd )    # convert vpd from Pa to kPa 
    - 8.15e-5 * elv       # convert elv from m to km
    )

  ## leaf-internal-to-ambient CO2 partial pressure (ci/ca) ratio
  chi <- whe / ( 1.0 + whe )

  ##  m
  gamma <- gs / ca
  m <- (chi - gamma) / (chi + 2 * gamma)

  out <- list( chi=chi, m=m, n=NA )
  return(out)
}


lue_vpd_simpl <- function( kmm, gs, ns_star, ca, vpd, beta ){
  #-----------------------------------------------------------------------
  # Input:    - float, 'kmm' : Pa, Michaelis-Menten coeff.
  #           - float, 'ns_star'  : (unitless) viscosity correction factor for water
  #           - float, 'vpd' : Pa, vapor pressure deficit
  # Output:   float, ratio of ci/ca (chi)
  # Features: Returns an estimate of leaf internal to ambient CO2
  #           partial pressure following the "simple formulation".
  # Depends:  - kc
  #           - ns
  #           - vpd
  #-----------------------------------------------------------------------

  ## leaf-internal-to-ambient CO2 partial pressure (ci/ca) ratio
  xi  <- sqrt( beta * kmm / (1.6 * ns_star))
  chi <- xi / (xi + sqrt(vpd))

  ## light use efficiency (m)
  ## consistent with this, directly return light-use-efficiency (m)
  m <- ( xi * (ca - gs) - gs * sqrt( vpd ) ) / ( xi * (ca + 2.0 * gs) + 2.0 * gs * sqrt( vpd ) )

  ## n 
  gamma <- gs / ca
  kappa <- kmm / ca
  n <- (chi + kappa) / (chi + 2 * gamma)

  out <- list( chi=chi, m=m, n=n )
  return(out)
}


lue_vpd_full <- function( kmm, gammastar, ns_star, ca, vpd, beta ){
  #-----------------------------------------------------------------------
  # Input:    - float, 'kmm' : Pa, Michaelis-Menten coeff.
  #           - float, 'ns_star'  : (unitless) viscosity correction factor for water
  #           - float, 'vpd' : Pa, vapor pressure deficit
  # Output:   float, ratio of ci/ca (chi)
  # Features: Returns an estimate of leaf internal to ambient CO2
  #           partial pressure following the "simple formulation".
  # Depends:  - kc
  #           - ns
  #           - vpd
  #-----------------------------------------------------------------------

  ## leaf-internal-to-ambient CO2 partial pressure (ci/ca) ratio
  xi  <- sqrt( (beta * ( kmm + gammastar ) ) / ( 1.6 * ns_star ) )
  chi <- gammastar / ca + ( 1.0 - gammastar / ca ) * xi / ( xi + sqrt(vpd) )

  ## consistent with this, directly return light-use-efficiency (mc)
  ## see Eq. 13 in 'Simplifying_LUE.pdf'

  ## light use efficiency (mc)
  # mc <- (ca - gammastar)/(ca + 2.0 * gammastar + 3.0 * gammastar * sqrt( (1.6 * vpd) / (beta * (K + gammastar) / ns_star ) ) )

  # Define variable substitutes:
  vdcg <- ca - gammastar
  vacg <- ca + 2.0 * gammastar
  vbkg <- beta * (kmm + gammastar)

  # Check for negatives:
  if (vbkg > 0){
    vsr <- sqrt( 1.6 * ns_star * vpd / vbkg )

    # Based on the mc' formulation (see Regressing_LUE.pdf)
    mj <- vdcg / ( vacg + 3.0 * gammastar * vsr )
  }

  ## alternative variables
  gamma <- gammastar / ca
  kappa <- kmm / ca

  # ## mj
  # mj_test <- (chi - gamma) / (chi + 2 * gamma)
  # print(paste("mj should be equal: ", mj, mj_test))

  ## mc
  mc <- (chi - gamma) / (chi + kappa)

  ## mj:mv
  mjov <- (chi + kappa) / (chi + 2 * gamma)

  out <- list( chi=chi, mc=mc, mj=mj, mjov=mjov )
  return(out)
}


calc_mprime <- function( mc ){
  #-----------------------------------------------------------------------
  # Input:  mc   (unitless): factor determining LUE
  # Output: mpi (unitless): modiefied m accounting for the co-limitation
  #                         hypothesis after Prentice et al. (2014)
  #-----------------------------------------------------------------------
  kc <- 0.41          # Jmax cost coefficient

  mpi <- mc^2 - kc^(2.0/3.0) * (mc^(4.0/3.0))

  # Check for negatives:
  if (mpi > 0){ mpi <- sqrt(mpi) }
  return(mpi)    
}


co2_to_ca <- function( co2, patm ){
  #-----------------------------------------------------------------------
  # Input:    - float, annual atm. CO2, ppm (co2)
  #           - float, monthly atm. pressure, Pa (patm)
  # Output:   - ca in units of Pa
  # Features: Converts ca (ambient CO2) from ppm to Pa.
  #-----------------------------------------------------------------------
  ca   <- ( 1.0e-6 ) * co2 * patm         # Pa, atms. CO2
  return( ca )
}


calc_k <- function( tc, patm ) {
  #-----------------------------------------------------------------------
  # Input:    - float, air temperature, deg C (temp)
  #           - float, atmospheric pressure, Pa (patm)
  # Output:   float, Pa (mmk)
  # Features: Returns the temperature & pressure dependent Michaelis-Menten
  #           coefficient, K (Pa).
  # Ref:      Bernacchi et al. (2001), Improved temperature response 
  #           functions for models of Rubisco-limited photosynthesis, 
  #           Plant, Cell and Environment, 24, 253--259.
  #-----------------------------------------------------------------------
  dhac <- 79430      # J/mol
  dhao <- 36380      # J/mol
  kR   <- 8.3145     # J/mol/K
  kco  <- 2.09476e5  # O2 partial pressure, US Standard Atmosphere

  if (is.na(patm)){
    kc25 <- 39.97      # Pa, assuming 25 deg C & 98.716 kPa
    ko25 <- 2.748e4    # Pa, assuming 25 deg C & 98.716 kPa
  } else {
    rat  <- patm / calc_patm(0)
    kc25 <- 41.03 * rat 
    ko25 <- 28210 * rat 
  }

  kc <- kc25 * exp( dhac * (tc - 25.0)/(298.15 * kR * (tc + 273.15)) ) 
  ko <- ko25 * exp( dhao * (tc - 25.0)/(298.15 * kR * (tc + 273.15)) ) 

  po <- kco * (1e-6) * patm # O2 partial pressure
  k  <- kc * (1.0 + po/ko)

  return(k)
}

# #----- Nick's code
# # calcualte the Michaelis-Menton coefficient (Pa) for Rubisco from temperature
# calc_km_pa = function( tc, patm ) {
  
  
#   dhac <- 79430  
#   dhao <- 36380 
#   kR <- 8.314        
#   kco <- 2.09476e5      

#   rat <- patm / calc_patm(0)
#   kc25 <- 41.03 * rat 
#   ko25 <- 28210 * rat 
  
#   temp_k <- 273.15 + tc

#   kc <- kc25 * exp( dhac * ((temp_k - 298.15) / (298.15 * kR * temp_k)))
#   ko <- ko25 * exp( dhao * ((temp_k - 298.15) / (298.15 * kR * temp_k)))
  
#   po <- kco * (1e-6) * patm  # O2 partial pressure
#   Km_pa <- kc * (1.0 + po/ko)
  
#   return(Km_pa) 
# }
# ##--------

calc_gammastar <- function( tc, patm = NA ) {
  #-----------------------------------------------------------------------
  # Input:    float, air temperature, degrees C (tc)
  # Output:   float, gamma-star, Pa (gammastar)
  # Features: Returns the temperature-dependent photorespiratory 
  #           compensation point, Gamma star (Pascals), based on constants 
  #           derived from Bernacchi et al. (2001) study.
  # Ref:      Bernacchi et al. (2001), Improved temperature response 
  #           functions for models of Rubisco-limited photosynthesis, 
  #           Plant, Cell and Environment, 24, 253--259.
  #-----------------------------------------------------------------------
  Hgm  <- 37830    # J/mol
  Rgas <- 8.3145   # J/mol/K

  if (!is.na(patm)){

    ## better formulation, accounts for elevation-dependence
    O2   <- 2.09476e5 # ppm
    patm0 <- calc_patm(0.0)
    O2_0 <- O2 * 1e-6 * patm0
    O2_z <- O2 * 1e-6 * patm
    
    patm <- patm
    rat  <- patm / patm0

    gammastar25 <- 4.332 * rat  # Pa

  } else {

    ## old formulation, elevation-independent
    gammastar25 <- 4.220    # Pa, assuming 25 deg C & 98.716 kPa

  }

  gammastar_pa <- gammastar25 * exp( Hgm * ( tc - 25.0 ) / ( 298.15 * Rgas * ( tc + 273.15 ) ) )

  if (!is.na(patm)){
  
    ## better formulation, accounts for elevation-dependence
    gammastar <- gammastar_pa * (O2_z / O2_0) # vary based on oxygen due to Rubisco specificity factor
  
  } else {
  
    ## old formulation, elevation-independent
    gammastar <- gammastar_pa
  
  }

  return( gammastar )
}


calc_ftemp_inst_vcmax <- function( tcleaf, tcgrowth, tcref = 25.0 ){
  #-----------------------------------------------------------------------
  # arguments
  # tcleaf: temperature (degrees C)
  # tref: is 'to' in Nick's set it to 25 C (=298.15 K in other cals)
  #
  # function return variable
  # fv: temperature response factor, relative to 25 deg C.
  #
  # Output:   Factor fv to correct for instantaneous temperature response
  #           of Vcmax for:
  #
  #               Vcmax(temp) = fv * Vcmax(25 deg C) 
  #
  # Ref:      Wang Han et al. (in prep.)
  #-----------------------------------------------------------------------
  # loal parameters
  Ha    <- 71513  # activation energy (J/mol)
  Hd    <- 200000 # deactivation energy (J/mol)
  Rgas  <- 8.3145 # universal gas constant (J/mol/K)
  a_ent <- 668.39 # offset of entropy vs. temperature relationship from Kattge & Knorr (2007) (J/mol/K)
  b_ent <- 1.07   # slope of entropy vs. temperature relationship from Kattge & Knorr (2007) (J/mol/K^2)
  
  tkref <- tcref + 273.15  # to Kelvin

  # conversion of temperature to Kelvin, tcleaf is the instantaneous leaf temperature in degrees C. 
  tkleaf <- tcleaf + 273.15

  # calculate entropy following Kattge & Knorr (2007), negative slope and y-axis intersect is when expressed as a function of temperature in degrees Celsius, not Kelvin !!!
  dent <- a_ent - b_ent * tcgrowth   # 'tcgrowth' corresponds to 'tmean' in Nicks, 'tc25' is 'to' in Nick's

  fva <- exp( (Ha * (tkleaf - tkref))/(tkleaf * tkref * Rgas) )
  fvb <- (1 + exp( (tkref * dent - Hd)/(Rgas * tkref) ) ) / (1 + exp( (tkleaf * dent - Hd)/(Rgas * tkleaf) ) )
  fv  <- fva * fvb

  return( fv ) 
}

# # From Nick's code:
# # calculate the temperature response multiplier for Vcmax and Jmax (Kattge and Knorr 2007)
# calc_ftemp_inst_vcmax_smith <- function( tcleaf, tcgrowth, tcref = 25.0 ){
#   ## tcref: optimal temperature, assuming that the plant is at
#   ## tcgrowth: average temperature that the plant is acclimated to
#   ## tcleaf: instantaneous temperature at the leaf surface, changing at short time scale

#   # local parameters
#   Ha    <- 71513
#   Hd    <- 200000
#   Rgas  <- 8.3145
#   a_ent <- 668.39
#   b_ent <- 1.07

#   # conversion of temperature to Kelvin, tc is the growth temperature in degrees C. 
#   tkleaf <- tcleaf + 273.15

#   # calculate entropy following Kattge & Knorr (2007), negative slope and y-axis intersect is when expressed as a function of temperature in degrees Celsius, not Kelvin !!!
#   dent <- a_ent - b_ent * tcgrowth

#   tkref <- tcref + 273.15
  
#   fva <- exp( Ha * (tkleaf - tkref) / (tkleaf * tkref * Rgas))

#   # # this is wrong:
#   # fvb <- (1 + exp( (tkref * dent - Hd)/(tkref * Rgas) ) / ( 1 + exp( (tkleaf * dent - Hd) / (tkleaf * Rgas ))) )

#   # this is correct:
#   fvb <- (1 + exp( (tkref * dent - Hd)/(Rgas * tkref) ) ) / (1 + exp( (tkleaf * dent - Hd)/(Rgas * tkleaf) ) )

#   fv  <- fva * fvb
  
#   return( fv )
# }


calc_ftemp_inst_rd <- function( tc ){
  #-----------------------------------------------------------------------
  # arguments:
  # tc                  # temperature (degrees C)
  # function return variable:
  # fr                  # temperature response factor, relative to 25 deg C.
  # Output:   Factor fr to correct for instantaneous temperature response
  #           of Rd (dark respiration) for:
  #
  #               Rd(temp) = fr * Rd(25 deg C) 
  #
  # Ref:      Heskel et al. (2016) used by Wang Han et al. (in prep.)
  #-----------------------------------------------------------------------
  # loal parameters
  apar <- 0.1012
  bpar <- 0.0005
  tk25  <- 298.15 # 25 deg C in Kelvin

  # conversion of temperature to Kelvin
  tk <- tc + 273.15

  fr <- exp( apar * (tc - 25.0) - bpar * (tc^2 - 25.0^2) )
 
  return(fr) 
}

calc_patm <- function( elv ){
  #-----------------------------------------------------------------------
  # Input:    - elevation, m (elv)
  # Output:   - float, atmospheric pressure at elevation 'elv', Pa (patm)
  # Features: Returns the atmospheric pressure as a function of elevation
  #           and standard atmosphere (1013.25 hPa)
  # Depends:  - connect_sql
  #           - flux_to_grid
  #           - get_data_point
  #           - get_msvidx
  # Ref:      Allen et al. (1998)
  #-----------------------------------------------------------------------

  # Define constants:
  kPo <- 101325   # standard atmosphere, Pa (Allen, 1973)
  kTo <- 298.15   # base temperature, K (Prentice, unpublished)
  kL <- 0.0065    # temperature lapse rate, K/m (Allen, 1973)
  kG <- 9.80665   # gravitational acceleration, m/s^2 (Allen, 1973)
  kR <- 8.3143    # universal gas constant, J/mol/K (Allen, 1973)
  kMa <- 0.028963 # molecular weight of dry air, kg/mol (Tsilingiris, 2008)

  # Convert elevation to pressure, Pa:
  patm <- kPo*(1.0 - kL*elv/kTo)**(kG*kMa/(kR*kL))
  
  return(patm)
}

density_h2o <- function( tc, p ){
  #-----------------------------------------------------------------------
  # Input:    - float, air temperature (tc), degrees C
  #           - float, atmospheric pressure (p), Pa
  # Output:   float, density of water, kg/m^3
  # Features: Calculates density of water at a given temperature and 
  #           pressure using the Tumlirz Equation
  # Ref:      F.H. Fisher and O.E Dial, Jr. (1975) Equation of state of 
  #           pure water and sea water, Tech. Rept., Marine Physical 
  #           Laboratory, San Diego, CA.
  #-----------------------------------------------------------------------

  # Calculate lambda, (bar cm^3)/g:
  my_lambda <- 1788.316 + 
          21.55053*tc + 
        -0.4695911*tc*tc + 
     (3.096363e-3)*tc*tc*tc + 
    -(7.341182e-6)*tc*tc*tc*tc

  # Calculate po, bar
  po <- 5918.499 + 
           58.05267*tc + 
         -1.1253317*tc*tc + 
     (6.6123869e-3)*tc*tc*tc + 
    -(1.4661625e-5)*tc*tc*tc*tc

  # Calculate vinf, cm^3/g
  vinf <- 0.6980547 +
    -(7.435626e-4)*tc +
     (3.704258e-5)*tc*tc +
    -(6.315724e-7)*tc*tc*tc +
     (9.829576e-9)*tc*tc*tc*tc +
   -(1.197269e-10)*tc*tc*tc*tc*tc +
    (1.005461e-12)*tc*tc*tc*tc*tc*tc +
   -(5.437898e-15)*tc*tc*tc*tc*tc*tc*tc +
     (1.69946e-17)*tc*tc*tc*tc*tc*tc*tc*tc +
   -(2.295063e-20)*tc*tc*tc*tc*tc*tc*tc*tc*tc

  # Convert pressure to bars (1 bar <- 100000 Pa)
  pbar <- (1e-5)*p
  
  # Calculate the specific volume (cm^3 g^-1):
  v <- vinf + my_lambda/(po + pbar)

  # Convert to density (g cm^-3) -> 1000 g/kg; 1000000 cm^3/m^3 -> kg/m^3:
  rho <- (1e3/v)

  return(rho)
}


calc_viscosity_h2o <- function( tc, p ) {
  #-----------------------------------------------------------------------
  # Input:    - float, ambient temperature (tc), degrees C
  #           - float, ambient pressure (p), Pa
  # Return:   float, viscosity of water (mu), Pa s
  # Features: Calculates viscosity of water at a given temperature and 
  #           pressure.
  # Depends:  density_h2o
  # Ref:      Huber, M. L., R. A. Perkins, A. Laesecke, D. G. Friend, J. V. 
  #           Sengers, M. J. Assael, ..., K. Miyagawa (2009) New 
  #           international formulation for the viscosity of H2O, J. Phys. 
  #           Chem. Ref. Data, Vol. 38(2), pp. 101-125.
  #-----------------------------------------------------------------------

  # Define reference temperature, density, and pressure values:
  tk_ast  <- 647.096    # Kelvin
  rho_ast <- 322.0      # kg/m^3
  mu_ast  <- 1e-6       # Pa s

  # Get the density of water, kg/m^3
  rho <- density_h2o(tc, p)

  # Calculate dimensionless parameters:
  tbar <- (tc + 273.15)/tk_ast
  tbarx <- tbar^(0.5)
  tbar2 <- tbar^2
  tbar3 <- tbar^3
  rbar <- rho/rho_ast

  # Calculate mu0 (Eq. 11 & Table 2, Huber et al., 2009):
  mu0 <- 1.67752 + 2.20462/tbar + 0.6366564/tbar2 - 0.241605/tbar3
  mu0 <- 1e2*tbarx/mu0

  # Create Table 3, Huber et al. (2009):
  h_array <- array(0.0, dim=c(7,6))
  h_array[1,] <- c(0.520094, 0.0850895, -1.08374, -0.289555, 0.0, 0.0)  # hj0
  h_array[2,] <- c(0.222531, 0.999115, 1.88797, 1.26613, 0.0, 0.120573) # hj1
  h_array[3,] <- c(-0.281378, -0.906851, -0.772479, -0.489837, -0.257040, 0.0) # hj2
  h_array[4,] <- c(0.161913,  0.257399, 0.0, 0.0, 0.0, 0.0) # hj3
  h_array[5,] <- c(-0.0325372, 0.0, 0.0, 0.0698452, 0.0, 0.0) # hj4
  h_array[6,] <- c(0.0, 0.0, 0.0, 0.0, 0.00872102, 0.0) # hj5
  h_array[7,] <- c(0.0, 0.0, 0.0, -0.00435673, 0.0, -0.000593264) # hj6

  # Calculate mu1 (Eq. 12 & Table 3, Huber et al., 2009):
  mu1 <- 0.0
  ctbar <- (1.0/tbar) - 1.0
  # print(paste("ctbar",ctbar))
  # for i in xrange(6):
  for (i in 1:6){
    coef1 <- ctbar^(i-1)
    # print(paste("i, coef1", i, coef1))
    coef2 <- 0.0
    for (j in 1:7){
      coef2 <- coef2 + h_array[j,i] * (rbar - 1.0)^(j-1)
    }
    mu1 <- mu1 + coef1 * coef2    
  }
  mu1 <- exp( rbar * mu1 )
  # print(paste("mu1",mu1))

  # Calculate mu_bar (Eq. 2, Huber et al., 2009)
  #   assumes mu2 = 1
  mu_bar <- mu0 * mu1

  # Calculate mu (Eq. 1, Huber et al., 2009)
  mu <- mu_bar * mu_ast    # Pa s

  return( mu )
}


calc_viscosity_h2o_vogel <- function( tc ) {
  #-----------------------------------------------------------------------
  # Input:    - float, ambient temperature (tc), degrees C
  # Return:   float, viscosity of water (mu), Pa s
  # Features: Calculates viscosity of water at a given temperature and 
  #           pressure.
  # Depends:  density_h2o
  # Ref:      Vogel ...
  #-----------------------------------------------------------------------

  tk <- tc + 272.15

  a <- -3.7188
  b <- 578.919
  c <- 137.546

  visc <- 1e-3 * exp(a + b/(tk - c))

  return( visc )
}

calc_ftemp_kphio <- function( tc ){
  #////////////////////////////////////////////////////////////////
  # Calculates the instantaneous temperature response of the quantum
  # yield efficiency based on Bernacchi et al., 2003 PCE (Equation
  # and parameter values taken from Appendix B)
  #----------------------------------------------------------------
  ftemp <- 0.352 + 0.022 * tc - 3.4e-4 * tc^2
  
  return(ftemp)  
}


# calc_dgpp <- function( fapar, dppfd, mlue ) {
#   ##//////////////////////////////////////////////////////////////////
#   ## Calculates daily GPP (mol CO2)
#   ##------------------------------------------------------------------
#   ## GPP is light use efficiency multiplied by absorbed light and C-P-alpha
#   dgpp <- fapar * dppfd * mlue

#   return( dgpp )
# }


# calc_drd <- function( lai, meanmppfd, mrd_unitiabs ){
#   ##//////////////////////////////////////////////////////////////////
#   ## Calculates daily dark respiration (Rd) based on monthly mean 
#   ## PPFD (assumes acclimation on a monthly time scale) (mol CO2).
#   ## meanmppfd is monthly mean PPFD, averaged over daylight seconds (mol m-2 s-1)
#   ##------------------------------------------------------------------
#   fapar <- get_fapar( lai )

#   ## Dark respiration takes place during night and day (24 hours)
#   drd <- fapar * meanmppfd * mrd_unitiabs * 60.0 * 60.0 * 24.0

#   return( drd )
# }


# calc_dtransp <- function( lai, dppfd, transp_unitiabs ) {
#   ##//////////////////////////////////////////////////////////////////
#   ## Calculates daily GPP (mol H2O).
#   ##------------------------------------------------------------------
#   fapar <- get_fapar( lai )

#   ## GPP is light use efficiency multiplied by absorbed light and C-P-alpha
#   dtransp <- fapar * dppfd * transp_unitiabs

#   return( dtransp )
# }


# calc_vcmax <- function( lai, meanmppfd, vcmax_unitiabs ) {
#   ##//////////////////////////////////////////////////////////////////
#   ## Calculates leaf-level metabolic N content per unit leaf area as a
#   ## function of Vcmax25.
#   ##------------------------------------------------------------------
#   fapar <- get_fapar( lai )

#   ## Calculate leafy-scale Rubisco-N as a function of LAI and current LUE
#   vcmax <- fapar * meanmppfd * vcmax_unitiabs / lai

#   return( vcmax )

# }


# calc_nr_leaf <- function( lai, mactnv_unitiabs, meanmppfd ) { 
#   ##//////////////////////////////////////////////////////////////////
#   ## Calculates leaf-level metabolic N content per unit leaf area as a
#   ## function of Vcmax25.
#   ##------------------------------------------------------------------
#   fapar <- get_fapar( lai )

#   ## Calculate leafy-scale Rubisco-N as a function of LAI and current LUE
#   nr_leaf <- max( fapar * meanmppfd[] * mactnv_unitiabs[] ) / lai

#   return( nr_leaf )
# }


# calc_n_rubisco_area <- function( vcmax25 ){
#   #-----------------------------------------------------------------------
#   # Input:    - vcmax25 : leaf level Vcmax  at 25 deg C, (mol CO2) m-2 s-1
#   # Output:   - n_area  : Rubisco N content per unit leaf area, (g N)(m-2 leaf)
#   # Features: Returns Rubisco N content per unit leaf area for a given 
#   #           Vcmax.
#   # Reference: Harrison et al., 2009, Plant, Cell and Environment; Eq. 3
#   #-----------------------------------------------------------------------

#   mol_weight_rubisco <- 5.5e5    # molecular weight of Rubisco, (g R)(mol R)-1
#   n_conc_rubisco     <- 1.14e-2  # N concentration in rubisco, (mol N)(g R)-1
#   mol_weight_n       <- 14.0067  # molecular weight of N, (g N)(mol N)-1
#   cat_turnover_per_site <- 3.5   # catalytic turnover rate per site at 25 deg C, (mol CO2)(mol R sites)-1
#   cat_sites_per_mol_R   <- 8.0   # number of catalytic sites per mol R, (mol R sites)(mol R)-1

#   # Metabolic N ratio
#   n_v <- mol_weight_rubisco * n_conc_rubisco * mol_weight_n / ( cat_turnover_per_site * cat_sites_per_mol_R )

#   n_rubisco_area <- vcmax25 * n_v

#   return(n_rubisco_area)
# }

# get_fapar <- function( lai ){
#   ##////////////////////////////////////////////////////////////////
#   ## Function returns fractional plant cover an individual
#   ## Eq. 7 in Sitch et al., 2003
#   ##----------------------------------------------------------------
#   kbeer <- 0.5

#   fapar <- ( 1.0 - exp( -1.0 * kbeer * lai ) )

#   return( fapar )

# }

