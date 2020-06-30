module md_gpp_pmodel
  !////////////////////////////////////////////////////////////////
  ! P-MODEL MODULE
  ! 
  ! Contains P-model functions for calculating ci, vcmax, and light
  ! use efficiency (LUE) as a function of ambient conditions (temperature,
  ! vapour pressure difference, CO2, and elevation), following 
  ! Prentice et al., 2014, and Wang et al., 2017. This is implemented
  ! by function pmodel().
  ! 
  ! Outputs of pmodel() can used within a LUE model to calculate gross 
  ! primary production by multiplying with aborbed light. This is 
  ! implemented by function calc_dgpp().
  !
  ! High-level wrapper functions and subroutines (getlue(), and 
  ! gpp()) are provided for use within SOFN.
  !
  ! An empirical soil moisture stress function is implemented 
  ! following Stocker et al., 2018.
  !
  ! Note that getpar_modl_gpp() must be invoked prior to first
  ! call of any other function/subroutine. An example is given 
  ! by demo_pmodel.f90.
  !
  ! Copyright (C) 2015, see LICENSE, Benjamin Stocker
  ! Written by Benjamin Stocker, partly based on Python code by
  ! Tyler Davis.
  !----------------------------------------------------------------
  ! load core parameters
  use md_params_core, only: nmonth, npft, nlu, c_molmass, h2o_molmass, maxgrid, ndayyear, kTkelvin, dummy
  use md_tile_pmodel, only: tile_type, tile_fluxes_type
  use md_interface_pmodel, only: myinterface
  use md_forcing_pmodel, only: climate_type, vegcover_type
  use md_plant_pmodel, only: params_pft_plant

  implicit none

  private
  public params_pft_gpp, getpar_modl_gpp, gpp, &
    calc_ftemp_kphio, calc_dgpp, calc_drd
    
  !-----------------------------------------------------------------------
  ! Uncertain (unknown) parameters. Runtime read-in
  !-----------------------------------------------------------------------
  type paramstype_gpp
    real :: beta         ! Unit cost of carboxylation (dimensionless)
    real :: soilm_par_a
    real :: soilm_par_b
    real :: rd_to_vcmax  ! Ratio of Rdark to Vcmax25, number from Atkin et al., 2015 for C3 herbaceous
    real :: tau_acclim   ! acclimation time scale of photosynthesis (d)
  end type paramstype_gpp

  type(paramstype_gpp) :: params_gpp

  ! PFT-DEPENDENT PARAMETERS
  type pftparamstype_gpp
    real :: kphio        ! quantum efficiency (Long et al., 1993)  
  end type pftparamstype_gpp

  type(pftparamstype_gpp), dimension(npft) :: params_pft_gpp

  !----------------------------------------------------------------
  ! Module-specific state variables
  !----------------------------------------------------------------
  real, dimension(npft) :: dassim           ! daily leaf-level assimilation rate (per unit leaf area) [gC/m2/d]

contains

  subroutine gpp( tile, tile_fluxes, co2, climate, vegcover, do_soilmstress, do_tempstress, init)
    !//////////////////////////////////////////////////////////////////
    ! Wrapper function to call to P-model. 
    ! Calculates meteorological conditions with memory based on daily
    ! varying variables.
    ! Calculates soil moisture and temperature stress functions.
    ! Calls P-model.
    !------------------------------------------------------------------
    use md_sofunutils, only: dampen_variability
    use md_photosynth, only: pmodel, outtype_pmodel

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    real, intent(in)    :: co2                               ! atmospheric CO2 (ppm)
    type(climate_type)  :: climate
    type(vegcover_type) :: vegcover
    logical, intent(in) :: do_soilmstress                    ! whether empirical soil miosture stress function is applied to GPP
    logical, intent(in) :: do_tempstress                     ! whether empirical temperature stress function is applied to GPP
    logical, intent(in) :: init                              ! is true on the very first simulation day (first subroutine call of each gridcell)

    ! local variables
    type( outtype_pmodel ) :: out_pmodel                     ! list of P-model output variables
    integer    :: pft
    integer    :: lu
    real       :: iabs
    real       :: soilmstress
    real       :: ftemp_kphio
    real       :: tk

    real, save :: co2_memory
    real, save :: vpd_memory
    real, save :: temp_memory
    real, save :: patm_memory

    ! ! xxx test
    ! real :: a_c, a_j, a_returned, fact_jmaxlim

    !----------------------------------------------------------------
    ! Calculate environmental conditions with memory, time scale 
    ! relevant for Rubisco turnover
    !----------------------------------------------------------------
    if (init) then
      co2_memory  = co2
      temp_memory = climate%dtemp
      vpd_memory  = climate%dvpd
      patm_memory = climate%dpatm
    end if 

    co2_memory  = dampen_variability( co2,            params_gpp%tau_acclim, co2_memory )
    temp_memory = dampen_variability( climate%dtemp,  params_gpp%tau_acclim, temp_memory )
    vpd_memory  = dampen_variability( climate%dvpd,   params_gpp%tau_acclim, vpd_memory )
    patm_memory = dampen_variability( climate%dpatm,  params_gpp%tau_acclim, patm_memory )

    tk = climate%dtemp + kTkelvin

    if ( tile_fluxes(1)%canopy%dayl > 0.0 .and. climate%dtemp > -5.0 ) then
      !----------------------------------------------------------------
      ! P-model call for C3 plants to get a list of variables that are 
      ! acclimated to slowly varying conditions
      !----------------------------------------------------------------
      do pft=1,npft
        out_pmodel = pmodel( &
                            fapar          = tile(1)%canopy%fapar, &
                            ppfd           = climate%dppfd, &
                            co2            = co2_memory, &
                            tc             = temp_memory, &
                            vpd            = vpd_memory, &
                            patm           = patm_memory, &
                            c4             = params_pft_plant(pft)%c4, &
                            method_optci   = "prentice14", &
                            method_jmaxlim = "wang17", &
                            kphio          = params_pft_gpp(pft)%kphio, &
                            beta           = params_gpp%beta, &
                            rd_to_vcmax    = params_gpp%rd_to_vcmax &
                            )

        ! simple:
        lu = 1

        !----------------------------------------------------------------
        ! xxx try:
        tile(lu)%plant(pft)%fpc_grid = 0.5
        !----------------------------------------------------------------

        iabs = tile(lu)%canopy%fapar * climate%dppfd * tile(lu)%plant(pft)%fpc_grid

        !----------------------------------------------------------------
        ! Calculate soil moisture stress as a function of soil moisture, mean alpha and vegetation type (grass or not)
        !----------------------------------------------------------------
        if (do_soilmstress) then
          soilmstress = calc_soilmstress( tile(1)%soil%phy%wscal, 0.0, params_pft_plant(1)%grass )
        else
          soilmstress = 1.0
        end if    

        !----------------------------------------------------------------
        ! Include instantaneous temperature effect on quantum yield efficiency
        !----------------------------------------------------------------
        if (do_tempstress) then
          ftemp_kphio = calc_ftemp_kphio( climate%dtemp, params_pft_plant(pft)%c4 )
        else
          ftemp_kphio = 1.0
        end if

        !----------------------------------------------------------------
        ! GPP
        !----------------------------------------------------------------
        tile_fluxes(lu)%plant(pft)%dgpp = iabs * out_pmodel%lue * ftemp_kphio * soilmstress

        !----------------------------------------------------------------
        ! Dark respiration
        !----------------------------------------------------------------
        tile_fluxes(lu)%plant(pft)%drd = iabs * out_pmodel%rd_unitiabs * ftemp_kphio * soilmstress * c_molmass


        ! !----------------------------------------------------------------
        ! ! CALCULATE PREDICTED GPP FROM P-model output
        ! ! using instantaneous (daily) LAI, PPFD, Cramer-Prentice-alpha
        ! !----------------------------------------------------------------
        ! do pft=1,npft

        !   print*,'params_pft_plant(pft)%lu_category ', params_pft_plant(pft)%lu_category
        !   print*,'tile(lu)%plant(pft)%fpc_grid', tile(lu)%plant(pft)%fpc_grid
        !   print*,'tile_fluxes(lu)%canopy%dayl', tile_fluxes(lu)%canopy%dayl
        !   print*,'climate%dtemp', climate%dtemp

        !   ! land use category (gridcell tile)
        !   lu = params_pft_plant(pft)%lu_category
        !     !----------------------------------------------------------------
        !     ! Calculate soil moisture stress as a function of soil moisture, mean alpha and vegetation type (grass or not)
        !     !----------------------------------------------------------------
        !     print*,'tile(lu)%soil%phy%wscal ', tile(lu)%soil%phy%wscal
        !     print*,'params_pft_plant(pft)%grass ', params_pft_plant(pft)%grass
        !     print*,'do_soilmstress ', do_soilmstress
        !     if (do_soilmstress) then
        !       ! soilmstress = calc_soilmstress( tile(lu)%soil%phy%wscal, 0.0, params_pft_plant(pft)%grass )
        !       soilmstress = 1.0
        !     else
        !       soilmstress = 1.0
        !     end if

        !     !----------------------------------------------------------------
        !     ! Include instantaneous temperature effect on quantum yield efficiency
        !     !----------------------------------------------------------------
        !     if (do_tempstress) then
        !       ftemp_kphio = calc_ftemp_kphio( climate%dtemp )
        !     else
        !       ftemp_kphio = 1.0
        !     end if

        !     ! GPP
        !     tile_fluxes(lu)%canopy%dgpp = calc_dgpp( tile(lu)%canopy%fapar, tile(lu)%plant(pft)%fpc_grid, climate%dppfd, out_pmodel%lue, ftemp_kphio, soilmstress )

        !     !----------------------------------------------------------------
        !     ! xxx test
        !     !----------------------------------------------------------------
        !     ! light-limited assimilation rate
        !     fact_jmaxlim = 1.0 / sqrt(1.0 + (4.0 * params_pft_gpp(pft)%kphio * dfapar * dppfd / out_pmodel%jmax)**2)
        !     a_j = params_pft_gpp(pft)%kphio * dfapar * dppfd * (out_pmodel%ci - out_pmodel%gammastar)/(out_pmodel%ci + 2 * out_pmodel%gammastar) * fact_jmaxlim

        !     ! Rubisco-limited assimilation rate
        !     a_c = out_pmodel%vcmax * (out_pmodel%ci - out_pmodel%gammastar)/(out_pmodel%ci + out_pmodel%kmm)

        !     ! output from pmodel()
        !     a_returned = out_pmodel%gpp / c_molmass

        !     print*,'a_j, a_c, a_returned, dgpp : ', a_j, a_c, a_returned, dgpp / c_molmass
        !     !----------------------------------------------------------------

        !     ! transpiration
        !     ! dtransp(pft) = calc_dtransp( dfapar, plant(pft)%acrown, dppfd, out_pmodel%transp_unitiabs, ftemp_kphio, soilmstress )
        !     dtransp(pft) = calc_dtransp( dfapar, plant(pft)%acrown, dppfd, out_pmodel%transp_unitiabs, climate%dtemp )

        !     !----------------------------------------------------------------
        !     ! Dark respiration
        !     !----------------------------------------------------------------
        !     tile_fluxes(lu)%canopy%drd = calc_drd( vegcover%dfapar, tile(lu)%plant(pft)%fpc_grid, climate%dppfd, out_pmodel%rd_unitiabs, ftemp_kphio, soilmstress )

        !     !----------------------------------------------------------------
        !     ! Leaf-level assimilation rate
        !     !----------------------------------------------------------------
        !     tile_fluxes(lu)%canopy%assim = calc_dassim( tile_fluxes(lu)%canopy%dgpp, tile_fluxes(lu)%canopy%dayl )

        !     ! !----------------------------------------------------------------
        !     ! ! stomatal conductance
        !     ! !----------------------------------------------------------------
        !     ! print*,'3'
        !     ! tile_fluxes(lu)%canopy%dgs = calc_dgs( dassim(pft), climate%dvpd, out_pmodel%ca, out_pmodel%gammastar, out_pmodel%xi )

        !     ! ! print*,'set-point gs:' dassim * dgs_unitiabs

        !     ! !----------------------------------------------------------------
        !     ! ! canopy conductance
        !     ! !----------------------------------------------------------------
        !     ! print*,'4'
        !     ! tile(lu)%canopy%dgc = calc_g_canopy( tile_fluxes(lu)%canopy%dgs, tile(lu)%canopy%lai, tk )

        !     ! tile(lu)%plant%vcmax25 = out_pmodel%vcmax25

        !     ! print*,'dgs per unit day (not second) - should be equal to what gpp/(ca-ci) in pmodel(): ', dgs_unitiabs * gpp / c_molmass
        !     ! stop

        !     ! ! Canopy-level Vcmax (actually changes only monthly)
        !     ! dvcmax_canop(pft) = calc_vcmax_canop( dfapar, out_pmodel%vcmax_unitiabs, meanmppfd )

        !     ! ! Leaf-level Vcmax
        !     ! dvcmax_leaf(pft) = out_pmodel%vcmax_unitiabs * meanmppfd

        !   else  

        !     tile_fluxes(lu)%canopy%dgpp = 0.0
        !     tile_fluxes(lu)%canopy%drd  = 0.0

        !   end if 

        ! end do
      end do
    end if

  end subroutine gpp


  function calc_dgpp( fapar, fpc_grid, dppfd, lue, ftemp_kphio, soilmstress ) result( my_dgpp )
    !//////////////////////////////////////////////////////////////////
    ! Calculates daily GPP given mean daily light use efficiency following
    ! a simple light use efficie model approach.
    !------------------------------------------------------------------
    ! arguments
    real, intent(in) :: fapar       ! fraction of absorbed photosynthetically active radiation (unitless)
    real, intent(in) :: fpc_grid    ! foliar projective cover, used for dividing grid cell area (unitless)
    real, intent(in) :: dppfd       ! daily total photon flux density (mol m-2)
    real, intent(in) :: lue         ! light use efficiency (g CO2 mol-1)
    real, intent(in) :: ftemp_kphio  ! air temperature (deg C)
    real, intent(in) :: soilmstress ! soil moisture stress factor (unitless)

    ! function return variable
    real :: my_dgpp                 ! Daily total gross primary productivity (gC m-2 d-1)

    ! GPP is light use efficiency multiplied by absorbed light and soil moisture stress function
    my_dgpp = fapar * fpc_grid * dppfd * soilmstress * lue * ftemp_kphio

  end function calc_dgpp


  function calc_dassim( dgpp, daylength ) result( my_dassim )
    !//////////////////////////////////////////////////////////////////
    ! Calculates assimilation rate, mean over daylight hours.
    ! Use *_unitfapar to get something representative of top-of-canopy.
    !------------------------------------------------------------------
    ! arguments
    real, intent(in) :: dgpp            ! daily total GPP (g CO2 m-2 d-1)
    real, intent(in) :: daylength       ! day length (h)

    ! function return variable
    real :: my_dassim                   ! canopy mean assimilation rate, mean over daylight hours (mol CO2 m-2 s-1)

    ! Assimilation rate, average over daylight hours
    if (daylength>0.0) then
      my_dassim = dgpp / ( 60.0 * 60.0 * daylength * c_molmass )
    else
      my_dassim = 0.0
    end if

  end function calc_dassim


  function calc_dgs( dassim, vpd, ca, gammastar, xi ) result( dgs )
    !//////////////////////////////////////////////////////////////////
    ! Calculates leaf-level stomatal conductance to CO2.
    ! This uses instantaneous VPD and is therefore not calculated inside
    ! the P-model function. The slope parameter 'xi' is representative
    ! for the acclimated response.
    !------------------------------------------------------------------
    ! arguments
    real, intent(in) :: dassim          ! daily mean assimilation rate (mol CO2 m-2 s-1)
    real, intent(in) :: vpd             ! vapour pressure deficit (Pa)
    real, intent(in) :: ca              ! ambient CO2 partial pressure (Pa)
    real, intent(in) :: gammastar       ! CO2 compensation point (Pa)
    real, intent(in) :: xi              ! slope parameter of stomatal response derived from P-model optimality, corresponding to sqrt(beta*(K+gammastar)/(1.6*etastar)) (Pa)
    ! real, intent(in) :: dgs_unitiabs    ! stomatal conductance per unit absorbed light (mol CO2 Pa-1 m-2 s-1 / mol light)

    ! function return variable
    real :: dgs                         ! leaf-level stomatal conductance to H2O, mean over daylight hours ( mol CO2 Pa-1 m-2 s-1 )

    ! Leaf-level assimilation rate, average over daylight hours
    ! dgs = dassim * dgs_unitiabs
    dgs = (1.0 + xi / sqrt(vpd)) * dassim / (ca - gammastar)
    ! print*,'instantaneous gs: ', dgs 
    
  end function calc_dgs


  function calc_drd( fapar, fpc_grid, dppfd, rd_unitiabs, ftemp_kphio, soilmstress ) result( my_drd )
    !//////////////////////////////////////////////////////////////////
    ! Calculates daily total dark respiration (Rd) based on monthly mean 
    ! PPFD (assumes acclimation on a monthly time scale).
    ! Not described in Stocker et al., XXX.
    !------------------------------------------------------------------
    ! arguments
    real, intent(in) :: fapar           ! fraction of absorbed photosynthetically active radiation
    real, intent(in) :: fpc_grid        ! foliar projective cover
    real, intent(in) :: dppfd           ! daily total photon flux density (mol m-2)
    real, intent(in) :: rd_unitiabs
    real, intent(in) :: ftemp_kphio      ! this day's air temperature, deg C
    real, intent(in) :: soilmstress     ! soil moisture stress factor

    ! function return variable
    real :: my_drd

    ! Dark respiration takes place during night and day (24 hours)
    my_drd = fapar * fpc_grid * dppfd * soilmstress * rd_unitiabs * ftemp_kphio * c_molmass

  end function calc_drd


  function calc_dtransp( fapar, acrown, dppfd, transp_unitiabs, ftemp_kphio, soilmstress ) result( my_dtransp )
    !//////////////////////////////////////////////////////////////////
    ! Calculates daily transpiration. 
    ! Exploratory only.
    ! Not described in Stocker et al., XXX.
    !------------------------------------------------------------------
    ! arguments
    real, intent(in) :: fapar
    real, intent(in) :: acrown
    real, intent(in) :: dppfd              ! daily total photon flux density, mol m-2
    real, intent(in) :: transp_unitiabs
    real, intent(in) :: ftemp_kphio              ! this day's air temperature
    real, intent(in) :: soilmstress        ! soil moisture stress factor

    ! function return variable
    real :: my_dtransp

    ! GPP is light use efficiency multiplied by absorbed light and C-P-alpha
    my_dtransp = fapar * acrown * dppfd * soilmstress * transp_unitiabs * ftemp_kphio * h2o_molmass

  end function calc_dtransp


  function calc_vcmax_canop( fapar, vcmax_unitiabs, meanmppfd ) result( my_vcmax )
    !//////////////////////////////////////////////////////////////////
    ! Calculates canopy-level summed carboxylation capacity (Vcmax). To get
    ! value per unit leaf area, divide by LAI.
    ! Not described in Stocker et al., XXX.
    !------------------------------------------------------------------
    ! arguments
    real, intent(in) :: fapar
    real, intent(in) :: vcmax_unitiabs
    real, intent(in) :: meanmppfd

    ! function return variable
    real :: my_vcmax    ! canopy-level Vcmax [gCO2/m2-ground/s]

    ! Calculate leafy-scale Rubisco-N as a function of LAI and current LUE
    my_vcmax = fapar * meanmppfd * vcmax_unitiabs

  end function calc_vcmax_canop


  function calc_g_canopy( g_stomata, lai, tk ) result( g_canopy )
    !/////////////////////////////////////////////////////////////////////////
    ! Calculates canopy conductance, proportional to the leaf area index.
    ! Since g_stomata is taken here from the photosynthesis module, we don't 
    ! use Eq. 8 in Zhang et al. (2017).
    !-------------------------------------------------------------------------
    use md_params_core, only: kR

    ! arguments
    real, intent(in) :: g_stomata      ! stomatal conductance (mol CO2 Pa-1 m-2 s-1)
    real, intent(in) :: lai            ! leaf area index, single-sided (unitless)
    real, intent(in) :: tk             ! (leaf) temperature (K)

    ! function return variable
    real :: g_canopy    ! canopy conductance (m s-1)

    ! canopy conductance scales with LAI. Including unit conversion to m s-1.
    g_canopy = 1.6 * g_stomata * kR * tk * lai

  end function calc_g_canopy


  function calc_soilmstress( soilm, meanalpha, isgrass ) result( outstress )
    !//////////////////////////////////////////////////////////////////
    ! Calculates empirically-derived stress (fractional reduction in light 
    ! use efficiency) as a function of soil moisture
    ! Input:  soilm (unitless, within [0,1]): daily varying soil moisture
    ! Output: outstress (unitless, within [0,1]): function of alpha to reduce GPP 
    !         in strongly water-stressed months
    !-----------------------------------------------------------------------
    ! argument
    real, intent(in) :: soilm                 ! soil water content (fraction)
    real, intent(in) :: meanalpha             ! mean annual AET/PET, average over multiple years (fraction)
    logical, intent(in), optional :: isgrass  ! vegetation cover information to distinguish sensitivity to low soil moisture

    real, parameter :: x0 = 0.0
    real, parameter :: x1 = 0.6

    real :: y0, beta

    ! function return variable
    real :: outstress

    if (soilm > x1) then
      outstress = 1.0
    else

      y0 = (params_gpp%soilm_par_a + params_gpp%soilm_par_b * meanalpha)

      ! if (present(isgrass)) then
      !   if (isgrass) then
      !     y0 = apar_grass + bpar_grass * meanalpha
      !   else
      !     y0 = apar + bpar * meanalpha
      !   end if
      ! else
      !   y0 = apar + bpar * meanalpha
      ! end if

      beta = (1.0 - y0) / (x0 - x1)**2
      outstress = 1.0 - beta * ( soilm - x1 )**2
      outstress = max( 0.0, min( 1.0, outstress ) )
    end if

  end function calc_soilmstress


  function calc_ftemp_kphio( dtemp, c4 ) result( ftemp )
    !////////////////////////////////////////////////////////////////
    ! Calculates the instantaneous temperature response of the quantum
    ! yield efficiency based on Bernacchi et al., 2003 PCE (Equation
    ! and parameter values taken from Appendix B)
    !----------------------------------------------------------------
    ! arguments
    real, intent(in) :: dtemp    ! (leaf) temperature in degrees celsius
    logical, intent(in) :: c4

    ! function return variable
    real :: ftemp

    if (c4) then
      ftemp = -0.008 + 0.00375 * dtemp - 0.58e-4 * dtemp**2   ! Based on calibrated values by Shirley
    else
      ftemp = 0.352 + 0.022 * dtemp - 3.4e-4 * dtemp**2  ! Based on Bernacchi et al., 2003
    end if
    
  end function calc_ftemp_kphio


  subroutine getpar_modl_gpp()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads module-specific parameters from input file.
    !----------------------------------------------------------------
    use md_sofunutils, only: getparreal

    ! local variables
    integer :: pft

    !----------------------------------------------------------------
    ! PFT-independent parameters
    !----------------------------------------------------------------
    ! unit cost of carboxylation
    params_gpp%beta  = 146.000000

    ! Ratio of Rdark to Vcmax25, number from Atkin et al., 2015 for C3 herbaceous
    params_gpp%rd_to_vcmax  = 0.01400000

    ! Apply identical temperature ramp parameter for all PFTs
    params_gpp%tau_acclim     = 10.0
    params_gpp%soilm_par_a    = myinterface%params_calib%soilm_par_a     ! is provided through standard input
    params_gpp%soilm_par_b    = myinterface%params_calib%soilm_par_b     ! is provided through standard input

    ! PFT-dependent parameter(s)
    params_pft_gpp(:)%kphio = myinterface%params_calib%kphio  ! is provided through standard input

  end subroutine getpar_modl_gpp


end module md_gpp_pmodel
