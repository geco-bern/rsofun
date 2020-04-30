module md_photosynth
  !//////////////////////////////////////////////////////////////////////
  ! P-MODEL PHOTOSYNTHESIS MODULE
  ! Is in a separate module here because two different md_gpp modules 
  ! (gpp_lm3ppa_pmodel, and gpp_pmodel) use it and interact with different 
  ! model structures.
  !------------------------------------------------------------------------
  use md_params_core, only: kPo, c_molmass, dummy

  implicit none

  private
  public pmodel, outtype_pmodel

  !----------------------------------------------------------------
  ! MODULE-SPECIFIC, PRIVATE VARIABLES
  !----------------------------------------------------------------
  ! Function return variables as derived types
  type outtype_pmodel
    real :: gammastar           ! temperature-dependent photorespiratory compensation point (Pa)
    real :: kmm                 ! Michaelis-Menten coefficient (Pa)
    real :: ca                  ! leaf-external (ambient) partial pressure, (Pa)
    real :: ci                  ! leaf-internal partial pressure, (Pa)
    real :: chi                 ! = ci/ca, leaf-internal to ambient CO2 partial pressure, ci/ca (unitless)
    real :: xi                  ! relative cost parameter, Eq. 9 in Stocker et al., 2019 GMD
    real :: iwue                ! intrinsic water use efficiency = A / gs = ca - ci = ca ( 1 - chi ) , unitless
    real :: lue                 ! light use efficiency (mol CO2 / mol photon)
    real :: assim               ! leaf-level assimilation rate (mol CO2 m-2 s-1)
    real :: gs                  ! stomatal conductance to CO2 (mol C Pa-1 m-2 s-1)
    real :: gs_unitfapar        ! stomatal conductance to CO2 per unit fapar (mol C Pa-1 m-2 s-1)
    real :: gs_unitiabs         ! stomatal conductance to CO2 per unit absorbed light (mol C Pa-1 m-2 s-1)
    real :: gpp                 ! gross primary productivity (g CO2 m-2 d-1)
    real :: vcmax               ! canopy-level maximum carboxylation capacity per unit ground area (mol CO2 m-2 s-1)
    real :: jmax                ! canopy-level maximum rate of electron transport (mol m-2 s-1)
    real :: vcmax25             ! canopy-level Vcmax25 (Vcmax normalized to 25 deg C) (mol CO2 m-2 s-1)
    real :: vcmax_unitfapar     ! Vcmax per unit fAPAR (mol CO2 m-2 s-1)
    real :: vcmax_unitiabs      ! Vcmax per unit absorbed light (mol CO2 m-2 s-1 mol-1)
    real :: ftemp_inst_vcmax    ! Instantaneous temperature response factor of Vcmax (unitless)
    real :: ftemp_inst_rd       ! Instantaneous temperature response factor of Rd (unitless)
    real :: rd                  ! Dark respiration (mol CO2 m-2 s-1)
    real :: rd_unitfapar        ! Dark respiration per unit fAPAR (mol CO2 m-2 s-1)
    real :: rd_unitiabs         ! Dark respiration per unit absorbed light (mol CO2 m-2 s-1)
    real :: actnv               ! Canopy-level total metabolic leaf N per unit ground area (g N m-2)
    real :: actnv_unitfapar     ! Metabolic leaf N per unit fAPAR (g N m-2)
    real :: actnv_unitiabs      ! Metabolic leaf N per unit absorbed light (g N m-2 mol-1)
    real :: transp              ! Canopy-level total transpiration rate (g H2O (mol photons)-1)
  end type outtype_pmodel

  type outtype_chi
    real :: chi                 ! = ci/ca, leaf-internal to ambient CO2 partial pressure, ci/ca (unitless)
    real :: ci                  ! leaf-internal CO2 partial pressure (Pa)
    real :: mj                  ! ci-limitation factor of light-limited assimilation (unitless)
    real :: mc                  ! ci-limitation factor of Rubisco-limited assimilation (unitless)
    real :: mjoc                ! ratio of light over Rubisco-limted ci-limitation factor (unitless)
    real :: xi                  ! relative cost parameter, Eq. 9 in Stocker et al., 2019 GMD
  end type outtype_chi

  !-----------------------------------------------------------------------
  ! Metabolic N ratio (N per unit Vcmax)
  ! Reference: Harrison et al., 2009, Plant, Cell and Environment; Eq. 3
  !-----------------------------------------------------------------------
  real, parameter :: mol_weight_rubisco    = 5.5e5    ! molecular weight of Rubisco, (g R)(mol R)-1
  real, parameter :: n_conc_rubisco        = 1.14e-2  ! N concentration in rubisco, (mol N)(g R)-1
  real, parameter :: cat_turnover_per_site = 2.33     ! catalytic turnover rate per site at 25 deg C, (mol CO2)(mol R sites)-1; use 2.33 instead of (3.5) as not all Rubisco is active (see Harrison et al., 2009)  
  real, parameter :: cat_sites_per_mol_R   = 8.0      ! number of catalytic sites per mol R, (mol R sites)(mol R)-1

  ! Metabolic N ratio (= 336.3734 mol N s (mol CO2)-1 )
  real, parameter :: n_v = mol_weight_rubisco * n_conc_rubisco / ( cat_turnover_per_site * cat_sites_per_mol_R )

contains

  function pmodel( fapar, ppfd, co2, tc, vpd, patm, c4, &
    method_optci, method_jmaxlim, kphio, beta, rd_to_vcmax ) result( out_pmodel )
    !//////////////////////////////////////////////////////////////////
    ! Implements the P-model, providing predictions for ci, Vcmax, and 
    ! light use efficiency, etc. 
    ! If fapar and ppfd are provided, calculates GPP, replacing separate
    ! function calc_dgpp().
    !------------------------------------------------------------------
    ! arguments
    real, intent(in) :: fapar        ! fraction of absorbed photosynthetically active radiation (unitless) 
    real, intent(in) :: ppfd         ! photon flux density (mol/m2)
    real, intent(in) :: co2          ! atmospheric CO2 concentration (ppm), relevant for fast responses
    real, intent(in) :: tc           ! air temperature (deg C), relevant for fast responses
    real, intent(in) :: vpd          ! vapor pressure (Pa), relevant for fast responses
    real, intent(in) :: patm         ! atmospheric pressure (Pa), relevant for fast responses
    logical, intent(in) :: c4        ! whether or not C4 photosynthesis pathway is followed. If .false., it's C3.
    character(len=*), intent(in) :: method_optci    ! Method used for deriving optimal ci:ca
    character(len=*), intent(in) :: method_jmaxlim  ! Method used for accounting for Jmax limitation
    real, intent(in) :: kphio        ! apparent quantum yield efficiency       
    real, intent(in) :: beta         ! cost parameter (unitless)
    real, intent(in) :: rd_to_vcmax  ! parameter for ratio of dark respiration at 25 deg C to Vcmax25

    ! function return value
    type(outtype_pmodel) :: out_pmodel

    ! local variables
    real :: iabs                ! absorbed photosynthetically active radiation (mol/m2)
    real :: kmm                 ! Michaelis-Menten coefficient (Pa)
    real :: gammastar           ! photorespiratory compensation point - Gamma-star (Pa)
    real :: ca                  ! ambient CO2 partial pressure, (Pa)
    real :: gs                  ! stomatal conductance to CO2 (mol CO2 Pa-1 m-2 s-1)
    real :: gs_unitfapar        ! stomatal conductance to CO2 (mol CO2 Pa-1 m-2 s-1)
    real :: gs_unitiabs         ! stomatal conductance to CO2 (mol CO2 Pa-1 m-2 s-1)
    real :: ci                  ! leaf-internal partial pressure, (Pa)
    real :: chi                 ! = ci/ca, leaf-internal to ambient CO2 partial pressure, ci/ca (unitless)
    real :: xi                  ! relative cost parameter, Eq. 9 in Stocker et al., 2019
    real :: ns                  ! viscosity of H2O at ambient temperatures (Pa s)
    real :: ns25                ! viscosity of H2O at 25 deg C (Pa s)
    real :: ns_star             ! viscosity correction factor (unitless)
    real :: mprime              ! factor in light use model with Jmax limitation
    real :: iwue                ! intrinsic water use efficiency = A / gs = ca - ci = ca ( 1 - chi ) , unitless
    real :: lue                 ! light use efficiency (mol CO2 / mol photon)
    real :: gpp                 ! gross primary productivity (g CO2 m-2 d-1)
    real :: jmax                ! canopy-level maximum rate of electron transport (XXX)
    real :: vcmax               ! canopy-level maximum carboxylation capacity per unit ground area (mol CO2 m-2 s-1)
    real :: vcmax25             ! canopy-level Vcmax25 (Vcmax normalized to 25 deg C) (mol CO2 m-2 s-1)
    real :: vcmax_unitfapar     ! Vcmax per unit fAPAR (mol CO2 m-2 s-1)
    real :: vcmax25_unitfapar   ! Vcmax25 per unit fAPAR (mol CO2 m-2 s-1)
    real :: vcmax_unitiabs      ! Vcmax per unit absorbed light (mol CO2 m-2 s-1 mol-1)
    real :: vcmax25_unitiabs    ! Vcmax25 per unit absorbed light (mol CO2 m-2 s-1 mol-1)
    real :: ftemp_inst_vcmax    ! Instantaneous temperature response factor of Vcmax (unitless)
    real :: ftemp_inst_rd       ! Instantaneous temperature response factor of Rd (unitless)
    real :: rd                  ! Dark respiration (mol CO2 m-2 s-1)
    real :: rd_unitfapar        ! Dark respiration per unit fAPAR (mol CO2 m-2 s-1)
    real :: rd_unitiabs         ! Dark respiration per unit absorbed light (mol CO2 m-2 s-1)
    real :: actnv               ! Canopy-level total metabolic leaf N per unit ground area (g N m-2)
    real :: actnv_unitfapar     ! Metabolic leaf N per unit fAPAR (g N m-2)
    real :: actnv_unitiabs      ! Metabolic leaf N per unit absorbed light (g N m-2 mol-1)
    real :: transp              ! Canopy-level total transpiration rate (g H2O (mol photons)-1)
    real :: fact_jmaxlim        ! Jmax limitation factor (unitless)

    ! local variables for Jmax limitation following Nick Smith's method
    real :: omega, omega_star, vcmax_unitiabs_star, tcref, jmax_over_vcmax, jmax_prime, jvrat

    real, parameter :: theta = 0.85
    real, parameter :: c_cost = 0.05336251

    ! xxx test
    real :: gs_test

    type(outtype_chi) :: out_optchi

    ! Prevent floating point exception for extremely low temperatures
    if (tc > -20.0) then
      !-----------------------------------------------------------------------
      ! Calculate photosynthesis model parameters depending on temperature, pressure, and CO2.
      !-----------------------------------------------------------------------
      ! ambient CO2 partial pression (Pa)
      ca = co2_to_ca( co2, patm )

      ! photorespiratory compensation point - Gamma-star (Pa)
      gammastar = calc_gammastar( tc, patm )

      ! ! XXX PMODEL_TEST: ok
      ! print*,'tc        ', tc
      ! print*,'patm      ', patm
      ! print*,'vpd       : ', vpd
      ! print*,'gammastar ', gammastar

      ! Michaelis-Menten coef. (Pa)
      kmm  = calc_kmm( tc, patm )
      
      ! ! XXX PMODEL_TEST: ok
      ! print*, 'kmm ', kmm

      ! viscosity correction factor = viscosity( temp, press )/viscosity( 25 degC, 1013.25 Pa) 
      ns      = calc_viscosity_h2o( tc, patm )  ! Pa s 
      ns25    = calc_viscosity_h2o( 25.0, kPo )  ! Pa s 
      ns_star = ns / ns25                       ! (unitless)

      ! ! XXX PMODEL_TEST: ok
      ! print*, 'ns_star ', ns_star

      !-----------------------------------------------------------------------
      ! Optimal ci
      ! The heart of the P-model: calculate ci:ca ratio (chi) and additional terms
      !-----------------------------------------------------------------------
      if (c4) then

        out_optchi = calc_chi_c4()

      else

        select case (method_optci)

          case ("prentice14")

            !-----------------------------------------------------------------------
            ! B.2 FULL FORMULATION
            !-----------------------------------------------------------------------
            out_optchi = calc_optimal_chi( kmm, gammastar, ns_star, ca, vpd, beta )
          
          case default

            stop 'PMODEL: select valid method'

        end select

      end if 

      ! ratio of leaf internal to ambient CO2
      chi = out_optchi%chi

      ! ! XXX PMODEL_TEST: ok
      ! print*, 'chi ', chi

      ! leaf-internal CO2 partial pressure (Pa)
      ci = out_optchi%ci

      !-----------------------------------------------------------------------
      ! Corrolary preditions
      !-----------------------------------------------------------------------
      ! intrinsic water use efficiency 
      iwue = ( ca - ci ) / ( 1.6 * patm )

      !-----------------------------------------------------------------------
      ! Vcmax and light use efficiency
      !-----------------------------------------------------------------------
      if (c4) then
        ! Identical to method_jmaxlim = "wang17"

        ! Include effect of Jmax limitation.
        ! In this case, out_optchi%mj = 1, and mprime = 0.669
        mprime = calc_mprime( out_optchi%mj )

        ! Light use efficiency (gpp per unit absorbed light)
        lue = kphio * mprime * c_molmass  ! in g CO2 / (mol light)

        ! Vcmax normalised per unit absorbed PPFD (assuming iabs=1), with Jmax limitation
        vcmax_unitiabs = kphio * out_optchi%mjoc * mprime / out_optchi%mj


      else if (method_jmaxlim=="wang17") then

        ! Include effect of Jmax limitation
        mprime = calc_mprime( out_optchi%mj )

        ! Light use efficiency (gpp per unit absorbed light)
        lue = kphio * mprime * c_molmass  ! in g CO2 / (mol light)

        ! Vcmax normalised per unit absorbed PPFD (assuming iabs=1), with Jmax limitation
        vcmax_unitiabs = kphio * out_optchi%mjoc * mprime / out_optchi%mj

        ! ! xxx test
        ! print*,'kphio           : ', kphio
        ! print*,'out_optchi%mjoc : ', out_optchi%mjoc 
        ! print*,'mprime          : ', mprime
        ! print*,'out_optchi%mj   : ', out_optchi%mj
        ! print*,'vcmax_unitiabs  : ', vcmax_unitiabs

      else if (method_jmaxlim=="smith19") then

        ! mc = (ci - gammastar) / (ci + kmm)                       ! Eq. 6
        ! print(paste("mc should be equal: ", mc, out_optchi%mc ) )

        ! mj = (ci - gammastar) / (ci + 2.0 * gammastar)           ! Eq. 8
        ! print(paste("mj should be equal: ", mj, out_optchi%mj ) )

        ! mjoc = (ci + kmm) / (ci + 2.0 * gammastar)               ! mj/mc, used in several instances below
        ! print(paste("mjoc should be equal: ", mjoc, out_optchi%mjoc ) )

        omega = calc_omega( theta = theta, c_cost = c_cost, m = out_optchi%mj )             ! Eq. S4
        omega_star = 1.0 + omega - sqrt( (1.0 + omega)**2 - (4.0 * theta * omega) )       ! Eq. 18
        
        ! calculate Vcmax-star, which corresponds to Vcmax at a reference temperature 'tcref'
        vcmax_unitiabs_star  = kphio * out_optchi%mjoc * omega_star / (8.0 * theta)               ! Eq. 19
        
        ! tcref is the optimum temperature in K, assumed to be the temperature at which Vcmax* is operating. 
        ! tcref is estimated based on its relationship to growth temperature following Kattge & Knorr 2007
        tcref = 0.44 * tc + 24.92

        ! calculated acclimated Vcmax at prevailing growth temperatures
        ftemp_inst_vcmax = calc_ftemp_inst_vcmax( tc, tc, tcref = tcref )
        vcmax_unitiabs = vcmax_unitiabs_star * ftemp_inst_vcmax   ! Eq. 20
        
        ! calculate Jmax
        jmax_over_vcmax = (8.0 * theta * omega) / (out_optchi%mjoc * omega_star)             ! Eq. 15 / Eq. 19
        jmax_prime = jmax_over_vcmax * vcmax_unitiabs 

        ! light use efficiency
        lue = c_molmass * kphio * out_optchi%mj * omega_star / (8.0 * theta) ! * calc_ftemp_inst_vcmax( tc, tc, tcref = tcref )     ! treat theta as a calibratable parameter


      else if (method_jmaxlim=="none") then

        ! Light use efficiency (gpp per unit absorbed light)
        lue = kphio * out_optchi%mj * c_molmass

        ! Vcmax normalised per unit absorbed PPFD (assuming iabs=1), with Jmax limitation
        vcmax_unitiabs = kphio * out_optchi%mjoc

      else

        stop 'PMODEL: select valid method'

      end if

      ! ! XXX PMODEL_TEST: ok
      ! print*, 'mj ', out_optchi%mj

      ! ! XXX PMODEL_TEST: ok
      ! print*, 'chi ', chi

      ! ! XXX PMODEL_TEST: ok
      ! print*, 'mprime ', mprime

      ! ! XXX PMODEL_TEST: ok
      ! print*, 'lue ', lue

      ! ! XXX PMODEL_TEST: ok
      ! print*, 'kphio ', kphio

      !-----------------------------------------------------------------------
      ! Corrolary preditions (This is prelimirary!)
      !-----------------------------------------------------------------------
      ! Vcmax25 (vcmax normalized to 25 deg C)
      ftemp_inst_vcmax  = calc_ftemp_inst_vcmax( tc, tc, tcref = 25.0 )
      vcmax25_unitiabs  = vcmax_unitiabs  / ftemp_inst_vcmax

      ! Dark respiration at growth temperature
      ftemp_inst_rd = calc_ftemp_inst_rd( tc )
      rd_unitiabs   = rd_to_vcmax * (ftemp_inst_rd / ftemp_inst_vcmax) * vcmax_unitiabs 

      ! active metabolic leaf N (canopy-level), mol N/m2-ground (same equations as for nitrogen content per unit leaf area, gN/m2-leaf)
      actnv_unitiabs  = vcmax25_unitiabs  * n_v

      ! stomatal conductance to CO2, expressed per unit absorbed light
      gs_unitiabs = (lue / c_molmass) / ( ca - ci )


      if (ppfd /= dummy) then
        !-----------------------------------------------------------------------
        ! Calculate quantities scaling with light assuming fAPAR = 1
        ! representing leaf-level at the top of the canopy.
        !-----------------------------------------------------------------------
        ! ! leaf-level assimilation rate
        ! assim = lue * ppfd

        ! ! Transpiration (E)
        ! transp = 1.6 * gs * vpd

        ! Vcmax normalised per unit fAPAR (assuming fAPAR=1)
        vcmax_unitfapar = ppfd * vcmax_unitiabs

        ! Vcmax25 (vcmax normalized to 25 deg C)
        vcmax25_unitfapar = ppfd * vcmax25_unitiabs

        ! Dark respiration per unit fAPAR (assuming fAPAR=1)
        rd_unitfapar = ppfd * rd_unitiabs

        ! active metabolic leaf N (canopy-level), mol N/m2-ground (same equations as for nitrogen content per unit leaf area, gN/m2-leaf)
        actnv_unitfapar = ppfd * actnv_unitiabs

        ! stomatal conductance to CO2, expressed per unit absorbed fAPAR
        gs_unitfapar = ppfd * gs_unitiabs

        
        if (fapar /= dummy) then
          !-----------------------------------------------------------------------
          ! Calculate quantities scaling with absorbed light
          !-----------------------------------------------------------------------
          ! absorbed photosynthetically active radiation (mol/m2)
          iabs = fapar * ppfd 

          ! ! XXX PMODEL_TEST: ok
          ! print*, 'iabs ', iabs

          ! Canopy-level quantities 
          ! Defined per unit ground level -> scaling with aborbed light (iabs)
          !-----------------------------------------------------------------------
          ! Gross primary productivity
          gpp = iabs * lue ! in g C m-2 s-1

          ! ! XXX PMODEL_TEST: ok
          ! print*, 'gpp ', gpp

          ! Vcmax per unit ground area is the product of the intrinsic quantum 
          ! efficiency, the absorbed PAR, and 'n'
          vcmax = iabs * vcmax_unitiabs  ! = iabs * kphio * n 

          ! ! XXX PMODEL_TEST: ok
          ! print*, 'vcmax ', vcmax

          ! (vcmax normalized to 25 deg C)
          vcmax25 = iabs * vcmax25_unitiabs  ! = factor25_vcmax * vcmax

          ! ! XXX PMODEL_TEST: ok
          ! print*, 'vcmax25 ', vcmax25

          ! Dark respiration
          rd = iabs * rd_unitiabs ! = rd_to_vcmax * vcmax

          ! ! XXX PMODEL_TEST: ok
          ! print*, 'rd ', rd

          ! active metabolic leaf N (canopy-level), mol N/m2-ground (same equations as for nitrogen content per unit leaf area, gN/m2-leaf)
          actnv = iabs * actnv_unitiabs ! = vcmax25 * n_v

          ! stomatal conductance to CO2
          gs = iabs * gs_unitiabs

          ! ! xxx text
          ! gs_test = (gpp / c_molmass) / (ca - ci)
          ! print*,'pmodel(): gs, gs_test ', gs, gs_test

          ! Derive Jmax using again A_J = A_C
          ! print*,'kphio, iabs, fact_jmaxlim, vcmax, ci, gammastar :::::', kphio, iabs, fact_jmaxlim, vcmax, ci, gammastar
          fact_jmaxlim = vcmax * (ci + 2.0 * gammastar) / (kphio * iabs * (ci + kmm))
          jmax = 4.0 * kphio * iabs / sqrt( (1.0/fact_jmaxlim)**2 - 1.0 )
          ! print*,'ok'

        else

          gpp     = dummy
          vcmax   = dummy
          vcmax25 = dummy
          rd      = dummy
          actnv   = dummy
          jmax    = dummy

        end if

      else

        vcmax_unitfapar   = dummy
        vcmax25_unitfapar = dummy
        rd_unitfapar      = dummy
        actnv_unitfapar   = dummy
        
        gpp               = dummy
        vcmax             = dummy
        vcmax25           = dummy
        rd                = dummy
        actnv             = dummy
        jmax              = dummy

      end if

    else

      actnv_unitiabs   = 0.0
      actnv_unitfapar  = 0.0
      actnv            = 0.0
      rd_unitiabs      = 0.0
      rd_unitfapar     = 0.0
      rd               = 0.0
      ftemp_inst_rd    = 0.0
      ftemp_inst_vcmax = 0.0
      vcmax_unitiabs   = 0.0
      vcmax_unitfapar  = 0.0
      vcmax25          = 0.0
      vcmax            = 0.0
      jmax             = 0.0
      gpp              = 0.0
      lue              = 0.0
      iwue             = 0.0
      chi              = 0.0
      ci               = 0.0
      kmm              = 0.0
      gammastar        = 0.0

    end if

    ! construct list for output
    out_pmodel%gammastar        = gammastar
    out_pmodel%kmm              = kmm
    out_pmodel%ca               = ca
    out_pmodel%ci               = ci
    out_pmodel%chi              = chi
    out_pmodel%xi               = xi
    out_pmodel%iwue             = iwue
    out_pmodel%lue              = lue
    out_pmodel%gpp              = gpp
    out_pmodel%vcmax            = vcmax
    out_pmodel%jmax             = jmax
    out_pmodel%vcmax25          = vcmax25
    out_pmodel%vcmax_unitfapar  = vcmax_unitfapar
    out_pmodel%vcmax_unitiabs   = vcmax_unitiabs
    out_pmodel%ftemp_inst_vcmax = ftemp_inst_vcmax
    out_pmodel%ftemp_inst_rd    = ftemp_inst_rd
    out_pmodel%rd               = rd
    out_pmodel%rd_unitfapar     = rd_unitfapar
    out_pmodel%rd_unitiabs      = rd_unitiabs
    out_pmodel%actnv            = actnv
    out_pmodel%actnv_unitfapar  = actnv_unitfapar
    out_pmodel%actnv_unitiabs   = actnv_unitiabs
    out_pmodel%gs_unitiabs      = gs_unitiabs
    out_pmodel%gs_unitfapar     = gs_unitfapar
    out_pmodel%gs               = gs


  end function pmodel


  function calc_optimal_chi( kmm, gammastar, ns_star, ca, vpd, beta ) result( out_optchi )
    !//////////////////////////////////////////////////////////////////
    ! Output:   float, ratio of ci/ca (chi)
    ! Features: Returns an estimate of leaf internal to ambient CO2
    !           partial pressure following the "simple formulation".
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in) :: kmm       ! Pa, Michaelis-Menten coeff.
    real, intent(in) :: gammastar ! Pa, photores. comp. point (Gamma-star)
    real, intent(in) :: ns_star   ! (unitless) viscosity correction factor for water
    real, intent(in) :: ca        ! Pa, ambient CO2 partial pressure
    real, intent(in) :: vpd       ! Pa, vapor pressure deficit
    real, intent(in) :: beta

    ! function return value
    type(outtype_chi) :: out_optchi

    ! local variables
    real :: chi                   ! leaf-internal-to-ambient CO2 partial pressure (ci/ca) ratio (unitless)
    real :: ci                    ! leaf-internal CO2 partial pressure (ci/ca) (Pa)
    real :: xi                    ! relative cost parameter
    real :: gamma                 ! variable substitute
    real :: kappa                 ! variable substitute
    real :: mc, mj, mjoc          ! ci-limitation factor Rubisco- and light-limited assimilation and their ratio, resp.

    ! variable substitutes
    real :: vdcg, vacg, vbkg, vsr

    ! leaf-internal-to-ambient CO2 partial pressure (ci/ca) ratio
    xi  = sqrt( ( beta * ( kmm + gammastar ) ) / ( 1.6 * ns_star ) )     ! Eq. 9 in Stocker et al., 2019
    chi = gammastar / ca + ( 1.0 - gammastar / ca ) * xi / ( xi + sqrt(vpd) )       ! Eq. 8 in Stocker et al., 2019
    ci  = chi * ca

    ! Define variable substitutes:
    vdcg = ca - gammastar
    vacg = ca + 2.0 * gammastar
    vbkg = beta * (kmm + gammastar)

    ! Check for negatives:
    if (vbkg > 0) then
      vsr = sqrt( 1.6 * ns_star * vpd / vbkg )

      ! Based on the m' formulation (see Regressing_LUE.pdf)
      mj = vdcg / ( vacg + 3.0 * gammastar * vsr )
    end if

    gamma = gammastar / ca
    kappa = kmm / ca

    ! mc
    mc = (chi - gamma) / (chi + kappa)

    ! mj:mv
    mjoc  = (chi + kappa) / (chi + 2 * gamma)

    ! return derived type
    out_optchi%chi  = chi
    out_optchi%ci   = ci
    out_optchi%mj   = mj
    out_optchi%mc   = mc
    out_optchi%mjoc = mjoc
    out_optchi%xi   = xi

  end function calc_optimal_chi


  function calc_chi_c4() result( out_chi )
    !//////////////////////////////////////////////////////////////////
    ! Output:   float, ratio of ci/ca (chi)
    ! Features: Returns an estimate of leaf internal to ambient CO2
    !           partial pressure following the "simple formulation".
    !-----------------------------------------------------------------------
    ! function return value
    type(outtype_chi) :: out_chi

    ! return derived type
    out_chi%chi  = 9999.9
    out_chi%mj   = 1.0
    out_chi%mc   = 1.0
    out_chi%mjoc = 1.0

  end function calc_chi_c4


  function calc_mprime( m ) result( mprime )
    !-----------------------------------------------------------------------
    ! Input:  m   (unitless): factor determining LUE
    ! Output: mpi (unitless): modiefied m accounting for the co-limitation
    !                         hypothesis after Prentice et al. (2014)
    !-----------------------------------------------------------------------
    ! argument
    real, intent(in) :: m

    ! local variables
    real, parameter :: kc = 0.41          ! Jmax cost coefficient

    ! function return variable
    real :: mprime

    ! square of m-prime (mpi)
    mprime = m**2 - kc**(2.0/3.0) * (m**(4.0/3.0))

    ! Check for negatives and take root of square
    if (mprime > 0) then
      mprime = sqrt(mprime)
    else
      print*,'negative mprime (', mprime, '). Setting to zero.'
      mprime = 0.0
    end if 
    
  end function calc_mprime


  function calc_omega( theta, c_cost, m ) result( omega )
    !-----------------------------------------------------------------------
    ! Adopted from Nick Smith's code:
    ! Calculate omega, see Smith et al., 2019 Ecology Letters
    !-----------------------------------------------------------------------
    use md_sofunutils, only: findroot_quadratic

    ! arguments
    real, intent(in) :: theta
    real, intent(in) :: c_cost
    real, intent(in) :: m

    ! function return variable
    real :: omega

    ! local variables
    real :: cm, v, capP, aquad, bquad, cquad, m_star
    real, dimension(2) :: root

    cm = 4.0 * c_cost / m                        ! simplification term for omega calculation
    v  = 1.0/(cm * (1.0 - theta * cm)) - 4.0 * theta ! simplification term for omega calculation
    
    ! account for non-linearities at low m values
    capP  = (((1.0/1.4) - 0.7)**2 / (1.0-theta)) + 3.4
    aquad = -1.0
    bquad = capP
    cquad = -(capP * theta)
    root  = findroot_quadratic( aquad, bquad, cquad )
    m_star = (4.0 * c_cost) / root(1)
    
    if (m < m_star) then
      omega = -( 1.0 - (2 * theta) ) - sqrt( (1.0 - theta) * v)
    else
      omega = -( 1.0 - (2 * theta))  + sqrt( (1.0 - theta) * v)
    end if
    
  end function calc_omega


  ! function findroot_quadratic( aquad, bquad, cquad, return_smallroot ) result( root )
  !   !-----------------------------------------------------------------------
  !   ! Returns the solution for a quadratic function:
  !   ! a + bx + cx^2 = 0
  !   ! Per default returns root2 
  !   !-----------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in) :: aquad, bquad, cquad

  !   ! function return variable
  !   real :: root

  !   ! local variables
  !   real :: d, root1, root2

  !   d = b*b - 4.0*a*c
  !   if (d >= 0.0) then              ! is it solvable?
  !     d     = sqrt(d)
  !     root1 = (-b + d)/(2.0*a)     ! first root
  !     root2 = (-b - d)/(2.0*a)     ! second root
  !   else                            ! complex roots
  !     stop 'findroot_quadratic(): There is no real root.'
  !   end if

  ! end function findroot_quadratic


  function co2_to_ca( co2, patm ) result( ca )
    !-----------------------------------------------------------------------
    ! Output:   - ca in units of Pa
    ! Features: Converts ca (ambient CO2) from ppm to Pa.
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in) :: co2     ! ambient CO2 in units of ppm
    real, intent(in) :: patm    ! monthly atm. pressure, Pa

    ! function return variable
    real :: ca ! ambient CO2 in units of Pa

    ca = ( 1.e-6 ) * co2 * patm         ! Pa, atms. CO2
      
  end function co2_to_ca


  function ca_to_co2( ca, patm ) result( co2 )
    !-----------------------------------------------------------------------
    ! Output:   - co2 in units of Pa
    ! Features: Converts ca (ambient CO2) from Pa to ppm.
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in) :: ca        ! ambient CO2 in units of Pa
    real, intent(in) :: patm      ! monthly atm. pressure, Pa

    ! function return variable
    real :: co2

    co2   = ca * ( 1.e6 ) / patm
    
  end function ca_to_co2


  function calc_kmm( tc, patm ) result( kmm )
    !-----------------------------------------------------------------------
    ! Features: Returns the temperature & pressure dependent Michaelis-Menten
    !           coefficient, K (Pa).
    ! Ref:      Bernacchi et al. (2001), Improved temperature response 
    !           functions for models of Rubisco-limited photosynthesis, 
    !           Plant, Cell and Environment, 24, 253--259.
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in) :: tc               ! air temperature, deg C 
    real, intent(in) :: patm             ! atmospheric pressure, Pa

    ! local variables
    real, parameter :: dhac = 79430      ! J/mol, Activation energy, Bernacchi et al. (2001)
    real, parameter :: dhao = 36380      ! J/mol, Activation energy, Bernacchi et al. (2001)
    real, parameter :: kc25 = 39.97      ! Pa, assuming 25 deg C & assuming elevation of 227.076 m.a.s.l.
    real, parameter :: ko25 = 27480      ! Pa, assuming 25 deg C & assuming elevation of 227.076 m.a.s.l.
    real, parameter :: kco  = 2.09476d5  ! ppm, US Standard Atmosphere
    real :: kc, ko, po, rat, tk

    ! function return variable
    real :: kmm                           ! temperature & pressure dependent Michaelis-Menten coefficient, K (Pa).

    ! convert to Kelvin
    tk = tc + 273.15

    kc = kc25 * calc_ftemp_arrhenius( tk, dhac )
    ko = ko25 * calc_ftemp_arrhenius( tk, dhao )

    po  = kco * (1.0e-6) * patm ! O2 partial pressure
    kmm = kc * (1.0 + po/ko)

  end function calc_kmm


  function calc_gammastar( tc, patm ) result( gammastar )
    !-----------------------------------------------------------------------
    ! Features: Returns the temperature-dependent photorespiratory 
    !           compensation point, Gamma star (Pascals), based on constants 
    !           derived from Bernacchi et al. (2001) study. Corresponds
    !           to 'calc_gammastar_colin' in pmodel.R.
    ! Ref:      Colin's document
    !-----------------------------------------------------------------------
    use md_sofunutils, only: calc_patm

    ! arguments
    real, intent(in) :: tc                 ! air temperature (degrees C)
    real, intent(in) :: patm               ! air pressure (Pa)

    ! local variables
    real, parameter :: dha    = 37830      ! J/mol, activation energy, Bernacchi et al. (2001)
    real, parameter :: gs25_0 = 4.332      ! Pa, assuming 25 deg C and sea level (1013.25 mbar)

    real :: tk           ! air temperature (Kelvin)
    real :: gammastar25  ! photorespiratory compensation point at 25 deg C and corrected for atmospheric pressure

    ! function return variable
    real :: gammastar   ! gamma-star (Pa)

    gammastar25 = gs25_0 * patm / calc_patm(0.0) 

    ! conversion to temperature in Kelvin
    tk = tc + 273.15
    gammastar = gammastar25 * calc_ftemp_arrhenius( tk, dha )

  end function calc_gammastar


  function calc_ftemp_inst_vcmax( tcleaf, tcgrowth, tcref ) result( fv )
    !-----------------------------------------------------------------------
    ! arguments
    ! tcleaf: temperature (degrees C)
    ! tref: is 'to' in Nick's set it to 25 C (=298.15 K in other cals)
    !
    ! function return variable
    ! fv: temperature response factor, relative to 25 deg C.
    !
    ! Output:   Factor fv to correct for instantaneous temperature response
    !           of Vcmax for:
    !
    !               Vcmax(temp) = fv * Vcmax(25 deg C) 
    !
    ! Ref:      Wang Han et al. (in prep.)
    !-----------------------------------------------------------------------
    use md_params_core, only: kR           ! Universal gas constant, J/mol/K

    ! arguments
    real, intent(in) :: tcleaf
    real, intent(in) :: tcgrowth
    real, intent(in), optional :: tcref

    ! function return variable
    real :: fv

    ! loal parameters
    real, parameter :: Ha    = 71513  ! activation energy (J/mol)
    real, parameter :: Hd    = 200000 ! deactivation energy (J/mol)
    real, parameter :: a_ent = 668.39 ! offset of entropy vs. temperature relationship from Kattge & Knorr (2007) (J/mol/K)
    real, parameter :: b_ent = 1.07   ! slope of entropy vs. temperature relationship from Kattge & Knorr (2007) (J/mol/K^2)
    
    ! local variables
    real :: tkref, tkleaf, dent, fva, fvb, mytcref

    if (present(tcref)) then
      mytcref = tcref
    else
      mytcref = 298.15
    end if

    tkref = mytcref + 273.15  ! to Kelvin

    ! conversion of temperature to Kelvin, tcleaf is the instantaneous leaf temperature in degrees C. 
    tkleaf = tcleaf + 273.15

    ! calculate entropy following Kattge & Knorr (2007), negative slope and y-axis intersect is when expressed as a function of temperature in degrees Celsius, not Kelvin !!!
    dent = a_ent - b_ent * tcgrowth   ! 'tcgrowth' corresponds to 'tmean' in Nicks, 'tc25' is 'to' in Nick's
    fva = calc_ftemp_arrhenius( tkleaf, Ha, tkref )
    fvb = (1.0 + exp( (tkref * dent - Hd)/(kR * tkref) ) ) / (1.0 + exp( (tkleaf * dent - Hd)/(kR * tkleaf) ) )
    fv  = fva * fvb

  end function calc_ftemp_inst_vcmax


  function calc_ftemp_inst_rd( tc ) result( fr )
    !-----------------------------------------------------------------------
    ! Output:   Factor fr to correct for instantaneous temperature response
    !           of Rd (dark respiration) for:
    !
    !               Rd(temp) = fr * Rd(25 deg C) 
    !
    ! Ref:      Heskel et al. (2016) used by Wang Han et al. (in prep.)
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in) :: tc      ! temperature (degrees C)

    ! function return variable
    real :: fr                  ! temperature response factor, relative to 25 deg C.

    ! loal parameters
    real, parameter :: apar = 0.1012
    real, parameter :: bpar = 0.0005
    real, parameter :: tk25 = 298.15 ! 25 deg C in Kelvin

    ! local variables
    real :: tk                  ! temperature (Kelvin)

    ! conversion of temperature to Kelvin
    tk = tc + 273.15

    fr = exp( apar * (tc - 25.0) - bpar * (tc**2 - 25.0**2) )
    
  end function calc_ftemp_inst_rd


  function calc_ftemp_arrhenius( tk, dha, tkref ) result( ftemp )
    !-----------------------------------------------------------------------
    ! Calculates the factor to account for the temperature response following 
    ! Arrhenius: 
    !
    !               var(T) = ftemp * var(T=T_ref)
    !
    ! T_ref is 25 deg C (=298.13 K) per default.
    !-----------------------------------------------------------------------
    use md_params_core, only: kR           ! Universal gas constant, J/mol/K

    ! arguments
    real, intent(in) :: tk                 ! temperature (Kelvin)
    real, intent(in) :: dha                ! activation energy (J/mol)
    real, intent(in), optional :: tkref    ! reference temperature 

    ! local variables
    real :: mytkref                        ! reference temperature 

    ! function return variable
    real :: ftemp

    if (present(tkref)) then
      mytkref = tkref
    else
      mytkref = 298.15
    end if

    ftemp = exp( dha * (tk - mytkref) / (mytkref * kR * tk) )

  end function calc_ftemp_arrhenius

  ! XXX REMOVED BECAUSE IT'S NOW IN SOFUNUTILS
  ! function calc_patm( elv ) result( patm )
  !   !-----------------------------------------------------------------------
  !   ! Features: Returns the atmospheric pressure as a function of elevation
  !   !           and standard atmosphere (1013.25 hPa)
  !   ! Depends:  - connect_sql
  !   !           - flux_to_grid
  !   !           - get_data_point
  !   !           - get_msvidx
  !   ! Ref:      Allen et al. (1998)
  !   !-----------------------------------------------------------------------
  !   ! argument
  !   real, intent(in) :: elv           ! elevation above sea level, m

  !   ! local variables
  !   real, parameter :: kPo = 101325   ! standard atmosphere, Pa (Allen, 1973)
  !   real, parameter :: kTo = 298.15   ! base temperature, K (Prentice, unpublished)
  !   real, parameter :: kL  = 0.0065   ! temperature lapse rate, K/m (Allen, 1973)
  !   real, parameter :: kG  = 9.80665  ! gravitational acceleration, m/s**2 (Allen, 1973)
  !   real, parameter :: kR  = 8.3145   ! universal gas constant, J/mol/K (Allen, 1973)
  !   real, parameter :: kMa = 0.028963 ! molecular weight of dry air, kg/mol (Tsilingiris, 2008)

  !   ! function return variable
  !   real :: patm    ! atmospheric pressure at elevation 'elv', Pa 

  !   ! Convert elevation to pressure, Pa:
  !   patm = kPo*(1.0 - kL*elv/kTo)**(kG*kMa/(kR*kL))
    
  ! end function calc_patm


  function calc_density_h2o( tc, patm ) result( density_h2o )
    !-----------------------------------------------------------------------
    ! Features: Calculates density of water at a given temperature and 
    !           pressure using the Tumlirz Equation
    ! Ref:      F.H. Fisher and O.E Dial, Jr. (1975) Equation of state of 
    !           pure water and sea water, Tech. Rept., Marine Physical 
    !           Laboratory, San Diego, CA.
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in) :: tc      ! air temperature (tc), degrees C
    real, intent(in) :: patm    ! atmospheric pressure (patm), Pa

    ! local variables
    real :: my_lambda, po, vinf, pbar, vau

    ! function return variable
    real :: density_h2o  ! density of water, kg/m**3

    ! Calculate lambda, (bar cm**3)/g:
    my_lambda = 1788.316 + &
                21.55053*tc + &
            (-0.4695911)*tc*tc + &
           (3.096363e-3)*tc*tc*tc + &
    (-1.0)*(7.341182e-6)*tc*tc*tc*tc

    ! Calculate po, bar
    po = 5918.499 + & 
                58.05267*tc + & 
            (-1.1253317)*tc*tc + & 
          (6.6123869e-3)*tc*tc*tc + & 
    (-1.0)*(1.4661625e-5)*tc*tc*tc*tc

    ! Calculate vinf, cm**3/g
    vinf = 0.6980547 + &
    (-1.0)*(7.435626e-4)*tc + &
           (3.704258e-5)*tc*tc + &
    (-1.0)*(6.315724e-7)*tc*tc*tc + &
           (9.829576e-9)*tc*tc*tc*tc + &
    (-1.0)*(1.197269e-10)*tc*tc*tc*tc*tc + &
          (1.005461e-12)*tc*tc*tc*tc*tc*tc + &
    (-1.0)*(5.437898e-15)*tc*tc*tc*tc*tc*tc*tc + &
           (1.69946e-17)*tc*tc*tc*tc*tc*tc*tc*tc + &
    (-1.0)*(2.295063e-20)*tc*tc*tc*tc*tc*tc*tc*tc*tc

    ! Convert pressure to bars (1 bar = 100000 Pa)
    pbar = (1e-5)*patm
    
    ! Calculate the specific volume (cm**3 g**-1):
    vau = vinf + my_lambda/(po + pbar)

    ! Convert to density (g cm**-3) -> 1000 g/kg; 1000000 cm**3/m**3 -> kg/m**3:
    density_h2o = (1.0e3/vau)

  end function calc_density_h2o


  function calc_viscosity_h2o( tc, patm ) result( viscosity_h2o )
    !-----------------------------------------------------------------------
    ! Features: Calculates viscosity of water at a given temperature and 
    !           pressure.
    ! Depends:  density_h2o
    ! Ref:      Huber, M. L., R. A. Perkins, A. Laesecke, D. G. Friend, J. V. 
    !           Sengers, M. J. Assael, ..., K. Miyagawa (2009) New 
    !           international formulation for the viscosity of H2O, J. Phys. 
    !           Chem. Ref. Data, Vol. 38(2), pp. 101-125.
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in) :: tc      ! air temperature (tc), degrees C
    real, intent(in) :: patm    ! atmospheric pressure (patm), Pa

    ! local variables
    real, parameter :: tk_ast  = 647.096    ! Kelvin
    real, parameter :: rho_ast = 322.0      ! kg/m**3
    real, parameter :: mu_ast  = 1e-6       ! Pa s

    real, dimension(7,6) :: h_array
    real :: rho                             ! density of water kg/m**3
    real :: tbar, tbarx, tbar2, tbar3, rbar, mu0, mu1, ctbar, mu_bar, &
      coef1, coef2
    integer :: i, j                         ! counter variables

    ! function return variable
    real :: viscosity_h2o

    ! print*,'----- in calc_viscosity_h2o() ------'
    ! print*,'tc ', tc
    ! print*,'patm ', patm

    ! Get the density of water, kg/m**3
    rho = calc_density_h2o(tc, patm)
    ! print*,'rho ', rho

    ! Calculate dimensionless parameters:
    tbar = (tc + 273.15)/tk_ast
    tbarx = tbar**(0.5)
    tbar2 = tbar**2
    tbar3 = tbar**3
    rbar = rho/rho_ast
    ! print*,'rbar ', rbar

    ! Calculate mu0 (Eq. 11 & Table 2, Huber et al., 2009):
    mu0 = 1.67752 + 2.20462/tbar + 0.6366564/tbar2 - 0.241605/tbar3
    mu0 = 1e2*tbarx/mu0
    ! print*,'mu0 ', mu0

    ! Create Table 3, Huber et al. (2009):
    h_array(1,:) = (/0.520094, 0.0850895, -1.08374, -0.289555, 0.0, 0.0/)  ! hj0
    h_array(2,:) = (/0.222531, 0.999115, 1.88797, 1.26613, 0.0, 0.120573/) ! hj1
    h_array(3,:) = (/-0.281378, -0.906851, -0.772479, -0.489837, -0.257040, 0.0/) ! hj2
    h_array(4,:) = (/0.161913,  0.257399, 0.0, 0.0, 0.0, 0.0/) ! hj3
    h_array(5,:) = (/-0.0325372, 0.0, 0.0, 0.0698452, 0.0, 0.0/) ! hj4
    h_array(6,:) = (/0.0, 0.0, 0.0, 0.0, 0.00872102, 0.0/) ! hj5
    h_array(7,:) = (/0.0, 0.0, 0.0, -0.00435673, 0.0, -0.000593264/) ! hj6

    ! Calculate mu1 (Eq. 12 & Table 3, Huber et al., 2009):
    mu1 = 0.0
    ctbar = (1.0/tbar) - 1.0
    do i=1,6
      coef1 = ctbar**(i-1)
      coef2 = 0.0
      do j=1,7
        ! print*,i, j, ' h_array(j,i): ',h_array(j,i)
        coef2 = coef2 + h_array(j,i) * (rbar - 1.0)**(j-1)
        ! print*,i, j, ' coef2: ',coef2
      end do
      mu1 = mu1 + coef1 * coef2    
    end do
    mu1 = exp( rbar * mu1 )
    ! print*, 'mu1 ', mu1

    ! Calculate mu_bar (Eq. 2, Huber et al., 2009)
    !   assumes mu2 = 1
    mu_bar = mu0 * mu1

    ! Calculate mu (Eq. 1, Huber et al., 2009)
    viscosity_h2o = mu_bar * mu_ast    ! Pa s

    ! print*,'----- END calc_viscosity_h2o() ------'

  end function calc_viscosity_h2o


end module md_photosynth