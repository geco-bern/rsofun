# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Contains P model functions adopted from GePiSaT
# ////////////////////////////////////////////////////////////////////////
# pmodel.R 
# __version__ 1.0
#
# written by Benjamin Stocker, adopted from Python code written by Tyler Davis
# Imperial College London
#
# 2015-03-16 -- created
# 2015-03-16 -- updated
# 2015-11-23 -- checked for consistency with Fortran 90 version and added to gepisat repository
#
# ------------
# description:
# ------------
# This collection of function implements the Global ecosystem
# in Space and Time (GePiSaT) model of the terrestrial biosphere. 
#
# GePiSaT takes a simple approach to modelling terrestrial gross primary 
# production (GPP) by making the use of the optimality principle of vegetation 
# minimizing the summed costs associated with maintaining carbon fixation and 
# water transport capabilities (Prentice et al., 2014).
#
# Currently this includes the following:
#
# ----------
# changelog:
# ----------
# VERSION 1.0.0
#  - file created [2015-03-16]
#
# VERSION 1.0.1
#  - 'lue_approx' function now based on SI units.
# 
# VERSION 1.0.2
#  - added functions that are used in SOFUN implementation (F90 version)
#
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#  Function Definitions
# ////////////////////////////////////////////////////////////////////////
pmodel <- function( fpar, ppfd, co2, tc, cpalpha, vpd, elv, method="full" ){
  #-----------------------------------------------------------------------
  # Input:    - fpar (unitless)    : monthly fraction of absorbed photosynthetically active radiation
  #           - ppfd (mol/m2)      : monthly photon flux density
  #           - co2 (ppm)          : atmospheric CO2 concentration
  #           - tc (deg C)         : monthly air temperature
  #           - cpalpha (unitless, within [0,1.26]) : monthly Cramer-Prentice-alpha
  #           - vpd (Pa)           : mean monthly vapor pressure -- CRU data is in hPa
  #           - elv (m)            : elevation above sea-level
  # Output:   gpp (mol/m2/month)   : gross primary production
  #-----------------------------------------------------------------------

  ## P-model parameters
  kphio <- 0.093      # quantum efficiency (Long et al., 1993)
  kPo   <- 101325.0   # standard atmosphere, Pa (Allen, 1973)
  kTo   <- 25.0       # base temperature, deg C (Prentice, unpublished)

  #-----------------------------------------------------------------------
  # Email from Tyler (10.3.2015):
  # I was estimating values of β based on the Wang Han approximation equation 
  # of χ using both the simplified and "more precise" expressions for χ and ξ 
  # (Prentice et al., 2014, Ecology Letters).  After examination, Colin and I 
  # noticed that the value of β is not significantly influenced by the 
  # expressions for χ and ξ. Since then, Colin has theorised the use of a 
  # "ground state" universal value of β, which is derived from the Wang Han 
  # equation at sea level (i.e., z = 0 m and Patm = 101325 Pa), standard temp-
  # erature (i.e., Tair = 25 deg C) and a non-influencial VPD (i.e., 
  # D = 1000 Pa). Based on these climatological values, the following were 
  # calculated:
  #   a. Γ* = 4.220 Pa
  #   b. K = 70.842 Pa
  #   c. η* = 1.0
  #   d. χ = 0.767
  #   e. β = 244.033
  # Results from modelled versus "observed" monthly GPP based on the universal 
  # value of β are promising. Colin and I are currently in the works on the next 
  # set of improvements, which, as I far as I know, will be based on this uni-
  # versal value of β.
  #-----------------------------------------------------------------------
  beta <- 244.033
  
  #-----------------------------------------------------------------------
  # Metabolic N ratio (N per unit Vcmax)
  # Reference: Harrison et al., 2009, Plant, Cell and Environment; Eq. 3
  #-----------------------------------------------------------------------
  mol_weight_rubisco <- 5.5e5    # molecular weight of Rubisco, (g R)(mol R)-1
  n_conc_rubisco     <- 1.14e-2  # N concentration in rubisco, (mol N)(g R)-1
  cat_turnover_per_site <- 2.33  # catalytic turnover rate per site at 25 deg C, (mol CO2)(mol R sites)-1; use 2.33 instead of (3.5) as not all Rubisco is active (see Harrison et al., 2009)  
  cat_sites_per_mol_R   <- 8.0   # number of catalytic sites per mol R, (mol R sites)(mol R)-1

  # Metabolic N ratio (mol N s (mol CO2)-1 )
  n_v <- mol_weight_rubisco * n_conc_rubisco / ( cat_turnover_per_site * cat_sites_per_mol_R )

  # Ratio of Rdark to Vcmax25, number from Atkin et al., 2015 for C3 herbaceous
  rd_to_vcmax <- 0.015

  ## parameters for Narea -- under construction
  # sla <- 0.0014       # specific leaf area (m2/gC)

  # N in cell walls: Slope of WN~LMA is 0.0002 mol N / g leaf mass (Hikosaka&Shigeno, 2009)
  # With 0.5 g C / g leaf mass and 14 g N / mol N: n_cw = 0.0056 g N / g C

  # ncw <- 0.0056          # N:C ratio in cell walls, working hypothesis: leaf N is solely determined by Vcmax25
  # n_v  <- 1.0/40.96    # gN µmol-1 s-1. Value 40.96 is 'sv' in Table 2 in Kattge et al., 2009, GCB, C3 herbaceous
  ## -- under construction


  ## absorbed photosynthetically active radiation (mol/m2)
  iabs <- fpar * ppfd

  ## atmospheric pressure as a function of elevation (Pa)
  patm <- calc_patm( elv )

  ## ambient CO2 partial pression (Pa)
  ca   <- co2_to_ca( co2, patm )

  ## photorespiratory compensation point - Gamma-star (Pa)
  gs   <- calc_gstar_gepisat( tc )

  ## function of alpha to reduce GPP in strongly water-stressed months (unitless)
  fa   <- calc_fa( cpalpha )

  ## Michaelis-Menten coef. (Pa)
  kmm  <- calc_k( tc, patm )

  ## viscosity correction factor = viscosity( temp, press )/viscosity( 25 degC, 1013.25 Pa) 
  ns      <- viscosity_h2o( tc, patm )  # Pa s 
  ns25    <- viscosity_h2o( kTo, kPo )  # Pa s 
  ns_star <- ns / ns25  # (unitless)


  if (method=="approx"){
    ##-----------------------------------------------------------------------
    ## A. APPROXIMATIVE METHOD
    ##-----------------------------------------------------------------------

    lue.out <- lue_approx( tc, vpd, elv, ca, gs )

  } else {
    ##-----------------------------------------------------------------------
    ## B. THEORETICAL METHOD
    ##-----------------------------------------------------------------------
    if (method=="simpl") {

      ## B.1 SIMPLIFIED FORMULATION 
      ##-----------------------------------------------------------------------
      lue.out <- lue_vpd_simpl( kmm, gs, ns, ca, vpd, beta  )

    } else if (method=="full"){

      ## B.2 FULL FORMULATION
      ##-----------------------------------------------------------------------
      lue.out <- lue_vpd_full( kmm, gs, ns_star, ca, vpd, beta  )

    }

  }

  ## LUE-functions return m, n, and chi
  m   <- lue.out$m
  n   <- lue.out$n
  chi <- lue.out$chi

  ##-----------------------------------------------------------------------
  ## Calculate function return variables
  ##-----------------------------------------------------------------------

  ## GPP per unit ground area is the product of the intrinsic quantum 
  ## efficiency, the absorbed PAR, the function of alpha (drought-reduction),
  ## and 'm'
  m   <- calc_mprime( m )
  gpp <- iabs * kphio * fa * m  # in mol m-2 s-1

  ## Light use efficiency (gpp per unit iabs)
  lue <- kphio * fa * m

  ## leaf-internal CO2 partial pressure (Pa)
  ci <- chi * ca

  ## stomatal conductance
  gs <- gpp  / ( ca - ci )

  ## Vcmax per unit ground area is the product of the intrinsic quantum 
  ## efficiency, the absorbed PAR, and 'n'
  vcmax <- iabs * kphio * n

  ## Vcmax normalised per unit fAPAR (assuming fAPAR=1)
  vcmax_unitfapar <- ppfd * kphio * n 

  ## Vcmax normalised per unit absorbed PPFD (assuming iabs=1)
  vcmax_unitiabs <- kphio * n 

  ## Vcmax25 (vcmax normalized to 25 deg C)
  factor25_vcmax    <- calc_vcmax25( 1.0, tc )
  vcmax25           <- factor25_vcmax * vcmax
  vcmax25_unitfapar <- factor25_vcmax * vcmax_unitfapar
  vcmax25_unitiabs  <- factor25_vcmax * vcmax_unitiabs

  ## Dark respiration
  rd <- rd_to_vcmax * vcmax

  ## Dark respiration per unit fAPAR (assuming fAPAR=1)
  rd_unitfapar <- rd_to_vcmax * vcmax_unitfapar

  ## Dark respiration per unit absorbed PPFD (assuming iabs=1)
  rd_unitiabs <- rd_to_vcmax * vcmax_unitiabs

  ## active metabolic leaf N (canopy-level), mol N/m2-ground (same equations as for nitrogen content per unit leaf area, gN/m2-leaf)
  actnv <- vcmax25 * n_v
  actnv_unitfapar <- vcmax25_unitfapar * n_v
  actnv_unitiabs  <- vcmax25_unitiabs  * n_v

  ## Transpiration (E)
  ## Using 
  ## - E = 1.6 gs D
  ## - gs = A / (ca (1-chi))
  ## (- chi = ci / ca)
  ## => E = (1.6 A D) / (ca - ci)
  transp           <- (1.6 * iabs * kphio * fa * m * vpd) / (ca - ci)   # gpp <- iabs * kphio * fa * m
  transp_unitfapar <- (1.6 * ppfd * kphio * fa * m * vpd) / (ca - ci)
  transp_unitiabs  <- (1.6 * 1.0  * kphio * fa * m * vpd) / (ca - ci)

  ## construct list for output
  out <- list( 
              gpp=gpp,                       # mol CO2 m-2 s-1 (given that ppfd is provided in units of s-1)
              gs=gs,
              ci=ci,
              vcmax=vcmax,                   # mol CO2 m-2 s-1 (given that ppfd is provided in units of s-1)
              vcmax25=vcmax25,               # mol CO2 m-2 s-1 (given that ppfd is provided in units of s-1)
              vcmax_unitfapar=vcmax_unitfapar,
              vcmax_unitiabs=vcmax_unitiabs,
              factor25_vcmax=factor25_vcmax, # unitless
              rd=rd,                         # mol CO2 m-2 s-1 
              rd_unitfapar=rd_unitfapar,     # mol CO2 m-2 s-1 
              rd_unitiabs=rd_unitiabs, 
              actnv=actnv,                   # mol N/m2 ground area (canopy-level)
              actnv_unitfapar=actnv_unitfapar, 
              actnv_unitiabs=actnv_unitiabs, 
              lue=lue,
              transp=transp,
              transp_unitfapar=transp_unitfapar,
              transp_unitiabs=transp_unitiabs
              )
  return( out )
}


calc_dgpp <- function( fapar, dppfd, mlue ) {
  ##//////////////////////////////////////////////////////////////////
  ## Calculates daily GPP (mol CO2)
  ##------------------------------------------------------------------
  ## GPP is light use efficiency multiplied by absorbed light and C-P-alpha
  dgpp <- fapar * dppfd * mlue

  return( dgpp )
}


calc_drd <- function( lai, meanmppfd, mrd_unitiabs ){
  ##//////////////////////////////////////////////////////////////////
  ## Calculates daily dark respiration (Rd) based on monthly mean 
  ## PPFD (assumes acclimation on a monthly time scale) (mol CO2).
  ## meanmppfd is monthly mean PPFD, averaged over daylight seconds (mol m-2 s-1)
  ##------------------------------------------------------------------
  fapar <- get_fapar( lai )

  ## Dark respiration takes place during night and day (24 hours)
  drd <- fapar * meanmppfd * mrd_unitiabs * 60.0 * 60.0 * 24.0

  return( drd )
}


calc_dtransp <- function( lai, dppfd, transp_unitiabs ) {
  ##//////////////////////////////////////////////////////////////////
  ## Calculates daily GPP (mol H2O).
  ##------------------------------------------------------------------
  fapar <- get_fapar( lai )

  ## GPP is light use efficiency multiplied by absorbed light and C-P-alpha
  dtransp <- fapar * dppfd * transp_unitiabs

  return( dtransp )
}


calc_vcmax <- function( lai, meanmppfd, vcmax_unitiabs ) {
  ##//////////////////////////////////////////////////////////////////
  ## Calculates leaf-level metabolic N content per unit leaf area as a
  ## function of Vcmax25.
  ##------------------------------------------------------------------
  fapar <- get_fapar( lai )

  ## Calculate leafy-scale Rubisco-N as a function of LAI and current LUE
  vcmax <- fapar * meanmppfd * vcmax_unitiabs / lai

  return( vcmax )

}


calc_nr_leaf <- function( lai, mactnv_unitiabs, meanmppfd ) { 
  ##//////////////////////////////////////////////////////////////////
  ## Calculates leaf-level metabolic N content per unit leaf area as a
  ## function of Vcmax25.
  ##------------------------------------------------------------------
  fapar <- get_fapar( lai )

  ## Calculate leafy-scale Rubisco-N as a function of LAI and current LUE
  nr_leaf <- max( fapar * meanmppfd[] * mactnv_unitiabs[] ) / lai

  return( nr_leaf )
}


calc_n_rubisco_area <- function( vcmax25 ){
  #-----------------------------------------------------------------------
  # Input:    - vcmax25 : leaf level Vcmax  at 25 deg C, (mol CO2) m-2 s-1
  # Output:   - n_area  : Rubisco N content per unit leaf area, (g N)(m-2 leaf)
  # Features: Returns Rubisco N content per unit leaf area for a given 
  #           Vcmax.
  # Reference: Harrison et al., 2009, Plant, Cell and Environment; Eq. 3
  #-----------------------------------------------------------------------

  mol_weight_rubisco <- 5.5e5    # molecular weight of Rubisco, (g R)(mol R)-1
  n_conc_rubisco     <- 1.14e-2  # N concentration in rubisco, (mol N)(g R)-1
  mol_weight_n       <- 14.0067  # molecular weight of N, (g N)(mol N)-1
  cat_turnover_per_site <- 3.5   # catalytic turnover rate per site at 25 deg C, (mol CO2)(mol R sites)-1
  cat_sites_per_mol_R   <- 8.0   # number of catalytic sites per mol R, (mol R sites)(mol R)-1

  # Metabolic N ratio
  n_v <- mol_weight_rubisco * n_conc_rubisco * mol_weight_n / ( cat_turnover_per_site * cat_sites_per_mol_R )

  n_rubisco_area <- vcmax25 * n_v

  return(n_rubisco_area)
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


lue_vpd_full <- function( kmm, gs, ns_star, ca, vpd, beta ){
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
  xi  <- sqrt( (beta * ( kmm + gs ) ) / ( 1.6 * ns_star ) )
  chi <- gs / ca + ( 1.0 - gs / ca ) * xi / ( xi + sqrt(vpd) )

  ## consistent with this, directly return light-use-efficiency (m)
  ## see Eq. 13 in 'Simplifying_LUE.pdf'

  ## light use efficiency (m)
  # m <- (ca - gs)/(ca + 2.0 * gs + 3.0 * gs * sqrt( (1.6 * vpd) / (beta * (K + gs) / ns_star ) ) )

  # Define variable substitutes:
  vdcg <- ca - gs
  vacg <- ca + 2.0 * gs
  vbkg <- beta * (kmm + gs)

  # Check for negatives:
  if (vbkg > 0){
    vsr <- sqrt( 1.6 * ns_star * vpd / vbkg )

    # Based on the m' formulation (see Regressing_LUE.pdf)
    m <- vdcg / ( vacg + 3.0 * gs * vsr )
  }

  ## n 
  gamma <- gs / ca
  kappa <- kmm / ca
  n <- (chi + kappa) / (chi + 2 * gamma)


  out <- list( chi=chi, m=m, n=n )
  return(out)
}


calc_mprime <- function( m ){
  #-----------------------------------------------------------------------
  # Input:  m   (unitless): factor determining LUE
  # Output: mpi (unitless): modiefied m accounting for the co-limitation
  #                         hypothesis after Prentice et al. (2014)
  #-----------------------------------------------------------------------
  kc <- 0.41          # Jmax cost coefficient

  mpi <- m^2 - kc^(2.0/3.0) * (m^(4.0/3.0))

  # Check for negatives:
  if (mpi > 0){ mpi <- sqrt(mpi) }
  return(mpi)    
}


get_fapar <- function( lai ){
  ##////////////////////////////////////////////////////////////////
  ## Function returns fractional plant cover an individual
  ## Eq. 7 in Sitch et al., 2003
  ##----------------------------------------------------------------
  kbeer <- 0.5

  fapar <- ( 1.0 - exp( -1.0 * kbeer * lai ) )

  return( fapar )

}


calc_fa <- function( cpalpha ){
  #-----------------------------------------------------------------------
  # Input:  cpalpha (unitless, within [0,1.26]): monthly Cramer-Prentice-alpha
  # Output: fa (unitless, within [0,1]): function of alpha to reduce GPP 
  #                                      in strongly water-stressed months
  #-----------------------------------------------------------------------
  fa <- ( cpalpha / 1.26 )^(0.25)
  return(fa)
}


co2_to_ca <- function( co2, patm ){
  #-----------------------------------------------------------------------
  # Input:    - float, annual atm. CO2, ppm (co2)
  #           - float, monthly atm. pressure, Pa (patm)
  # Output:   - ca in units of Pa
  # Features: Converts ca (ambient CO2) from ppm to Pa.
  #-----------------------------------------------------------------------
  ca   <- ( 1.e-6 ) * co2 * patm         # Pa, atms. CO2
  return( ca )
}


ca_to_co2 <- function( ca, patm ){
  #-----------------------------------------------------------------------
  # Input:    - float, ambient CO2, Pa (ca)
  #           - float, monthly atm. pressure, Pa (patm)
  # Output:   - co2 in units of Pa
  # Features: Converts ca (ambient CO2) from Pa to ppm.
  #-----------------------------------------------------------------------
  co2   <- ca * ( 1.e6 ) / patm
  return( co2 )
}


calc_vpd <- function( temp, vap, tmin=NA, tmax=NA ){
  #-----------------------------------------------------------------------
  # Input:    - mean monthly temperature, deg C (temp)
  #           - mean monthly vapor pressure, hPa (vap) -- CRU data is in hPa
  #           - (optional) mean monthly min daily air temp, deg C (tmin)
  #           - (optional) mean monthly max daily air temp, deg C (tmax)
  # Output:   mean monthly vapor pressure deficit, Pa (vpd)
  # Features: Returns mean monthly vapor pressure deficit
  # Ref:      Eq. 5.1, Abtew and Meleese (2013), Ch. 5 Vapor Pressure 
  #           Calculation Methods, in Evaporation and Evapotranspiration: 
  #           Measurements and Estimations, Springer, London.
  #             vpd = 0.611*exp[ (17.27 tc)/(tc + 237.3) ] - ea
  #             where:
  #                 temp = average daily air temperature, deg C
  #                 vap  = actual vapor pressure, kPa
  #-----------------------------------------------------------------------
  if ( !is.na(tmin) && !is.na(tmax) ){
    temp <- 0.5 * (tmin + tmax)
  }

  ## calculate VPD in units of kPa
  vpd <- ( 0.611 * exp( (17.27 * temp)/(temp + 237.3) ) - 0.10 * vap )    

  ## convert to Pa
  vpd <- vpd * 1000

  return( vpd )
}


calc_k <- function(tc, patm) {
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

  kc25 <- 39.97      # Pa, assuming 25 deg C & 98.716 kPa
  ko25 <- 2.748e4    # Pa, assuming 25 deg C & 98.716 kPa
  dhac <- 79430      # J/mol
  dhao <- 36380      # J/mol
  kR   <- 8.3145     # J/mol/K
  kco  <- 2.09476e5  # ppm, US Standard Atmosphere

  kc <- kc25 * exp( dhac * (tc - 25.0)/(298.15 * kR * (tc + 273.15)) ) 
  ko <- ko25 * exp( dhao * (tc - 25.0)/(298.15 * kR * (tc + 273.15)) ) 

  po <- kco * (1e-6) * patm # O2 partial pressure
  k  <- kc * (1.0 + po/ko)

  return(k)
}


calc_k_colin <- function( tc ) {
  #-----------------------------------------------------------------------
  # Input:    - float, air temperature, deg C (tc)
  # Output:   float, Pa (mmk)
  # Features: Returns the temperature & pressure dependent Michaelis-Menten
  #           coefficient, K (Pa).
  # Ref:      Colin's documents
  #-----------------------------------------------------------------------

  ## conversion to temperature in Kelvin
  tk <- tc + 273.15

  kc25 <- 41.03      # Pa, assuming 25 deg C & 98.716 kPa
  ko25 <- 28210      # Pa
  dhac <- 79430      # J/mol
  dhao <- 36380      # J/mol
  kR   <- 8.3145     # J/mol/K
  kco  <- 2.09476e5  # ppm, US Standard Atmosphere

  kc <- kc25 * exp( dhac / kR * (1.0/298.15 - 1.0/tk) )
  ko <- ko25 * exp( dhao / kR * (1.0/298.15 - 1.0/tk) )

  po <- kco * (1e-6) * patm # O2 partial pressure
  k  <- kc * (1.0 + po/ko )

  return(k)
}

calc_gstar_gepisat <- function( tc ) {
  #-----------------------------------------------------------------------
  # Input:    float, air temperature, degrees C (tc)
  # Output:   float, gamma-star, Pa (gs)
  # Features: Returns the temperature-dependent photorespiratory 
  #           compensation point, Gamma star (Pascals), based on constants 
  #           derived from Bernacchi et al. (2001) study.
  # Ref:      Bernacchi et al. (2001), Improved temperature response 
  #           functions for models of Rubisco-limited photosynthesis, 
  #           Plant, Cell and Environment, 24, 253--259.
  #-----------------------------------------------------------------------

  gs25 <- 4.220    # Pa, assuming 25 deg C & 98.716 kPa)
  dha  <- 37830    # J/mol
  kR   <- 8.3145   # J/mol/K

  gs <- gs25 * exp( dha * ( tc - 25.0 ) / ( 298.15 * kR * ( tc + 273.15 ) ) )

  return( gs )
}


calc_gstar_wh <- function( temp ){
  #-----------------------------------------------------------------------
  ## Returns the temperature-dependent photorespiratory compensation point, 
  ## Gamma star (Pascals), based on constants derived from 
  ## Bernacchi et al. (2001) study.
  #-----------------------------------------------------------------------

  ## wang han's values
  gs25 <- 42.75  # corresponds to what Colin gave me
  k    <- 0.0512

  gs <- gs25 * exp( k * ( temp - 25 ) )

  return( gs )

}

calc_gstar_colin <- function( tc ){
  #-----------------------------------------------------------------------
  # Input:    tc: air temperature (degrees C)
  # Output:   gs: gamma-star (Pa)
  # Features: Returns the temperature-dependent photorespiratory 
  #           compensation point, Gamma star (Pascals), based on constants 
  #           derived from Bernacchi et al. (2001) study.
  # Ref:      Colin's document
  #-----------------------------------------------------------------------

  ## conversion to temperature in Kelvin
  tk <- tc + 273.15

  gs25 <- 4.275    # corresponds to what Colin gave me
  kR   <- 8.3145   # J/mol/K
  dha  <- 37830    # J/mol

  gs <- gs25 * exp( ( dha / kR ) * ( 1/298.15 - 1.0/tk ) )
  
  return( gs )
}


calc_vcmax25 <- function( vcmax, tc ){
  #-----------------------------------------------------------------------
  # Input:    - gcmax  : Vcmax at a given temperature tc 
  #           - tc     : air temperature (degrees C)
  # Output:   vcmax25  : Vcmax at 25 deg C
  # Features: Returns the temperature-corrected Vcmax at 25 deg C
  # Ref:      Analogue function like 'calc_gstar_gepisat'
  #-----------------------------------------------------------------------

  dhav <- 65330    # J/mol
  kR   <- 8.3145   # J/mol/K

  vcmax25 <- vcmax * exp( -dhav * ( tc - 25.0 ) / ( 298.15 * kR * ( tc + 273.15 ) ) )
  return( vcmax25 )
}


# calc_vcmax25_colin <- function( vcmax, tc ){
#   #-----------------------------------------------------------------------
#   # Input:    - gcmax  : Vcmax at a given temperature tc 
#   #           - tc     : air temperature (degrees C)
#   # Output:   vcmax25  : Vcmax at 25 deg C
#   # Features: Returns the temperature-corrected Vcmax at 25 deg C
#   # Ref:      Colin's document
#   #-----------------------------------------------------------------------

#   ## conversion to temperature in Kelvin
#   tk <- tc + 273.15

#   dhav <- 65330    # J/mol
#   kR   <- 8.3145   # J/mol/K

#   vcmax25 <- vcmax * exp( -dhav/kR * (1/298.15 - 1/tk) )
#   return( vcmax25 )
# }


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
  
  return (patm)
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


viscosity_h2o <- function( tc, p ) {
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


viscosity_h2o_vogel <- function( tc ) {
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

