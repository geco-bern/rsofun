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
  use md_sofunutils, only: radians
  use md_grid, only: gridtype

  implicit none

  private
  public params_pft_gpp, getpar_modl_gpp, gpp
    
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

  subroutine gpp( tile, tile_fluxes, co2, climate, vegcover, grid, do_soilmstress, do_tempstress, init)
    !//////////////////////////////////////////////////////////////////
    ! Wrapper function to call to P-model. 
    ! Calculates meteorological conditions with memory based on daily
    ! varying variables.
    ! Calculates soil moisture and temperature stress functions.
    ! Calls P-model.
    !------------------------------------------------------------------
    ! use md_plant, only: params_pft_plant, plant_type, plant_fluxes_type
    use md_sofunutils, only: dampen_variability
    use md_photosynth, only: pmodel, zero_pmodel, outtype_pmodel, calc_ftemp_inst_vcmax, calc_ftemp_inst_jmax

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    real, intent(in)    :: co2                               ! atmospheric CO2 (ppm)
    type(climate_type)  :: climate
    type(vegcover_type) :: vegcover
    type(gridtype)      :: grid
    logical, intent(in) :: do_soilmstress                    ! whether empirical soil miosture stress function is applied to GPP
    logical, intent(in) :: do_tempstress                     ! whether empirical temperature stress function is applied to GPP
    logical, intent(in) :: init                              ! is true on the very first simulation day (first subroutine call of each gridcell)

    ! ! input-output arguments
    ! real, dimension(npft), intent(inout)  :: dgpp            ! daily total gross primary productivity (gC m-2 d-1)
    ! real, dimension(npft), intent(inout)  :: drd             ! daily total dark respiraiton (gC m-2 d-1)
    ! real, dimension(npft), intent(inout)  :: dtransp         ! daily total transpiration (XXX)

    ! local variables
    type(outtype_pmodel) :: out_pmodel              ! list of P-model output variables
    type(climate_type)   :: climate_acclimation     ! list of climate variables to which P-model calculates acclimated traits
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
    real, save :: ppfd_memory

    ! xxx test
    real :: a_c, a_j, a_returned, fact_jmaxlim

    !----------------------------------------------------------------
    ! Convert daily mean environmental conditions to conditions to
    ! which photosynthesis is acclimated to (daytime mean, or mid-day
    ! mean) 
    !----------------------------------------------------------------
    ! climate_acclimation = calc_climate_acclimation( climate, grid, "daytime" )
    climate_acclimation = climate

    !----------------------------------------------------------------
    ! Calculate environmental conditions with memory, time scale 
    ! relevant for Rubisco turnover
    !----------------------------------------------------------------
    if (init) then
      co2_memory  = co2
      temp_memory = climate_acclimation%dtemp
      vpd_memory  = climate_acclimation%dvpd
      patm_memory = climate_acclimation%dpatm
      ppfd_memory = climate_acclimation%dppfd
    end if 

    co2_memory  = dampen_variability( co2,                       params_gpp%tau_acclim, co2_memory  )
    temp_memory = dampen_variability( climate_acclimation%dtemp, params_gpp%tau_acclim, temp_memory )
    vpd_memory  = dampen_variability( climate_acclimation%dvpd,  params_gpp%tau_acclim, vpd_memory  )
    patm_memory = dampen_variability( climate_acclimation%dpatm, params_gpp%tau_acclim, patm_memory )
    ppfd_memory = dampen_variability( climate_acclimation%dppfd, params_gpp%tau_acclim, ppfd_memory )

    tk = climate_acclimation%dtemp + kTkelvin


    pftloop: do pft=1,npft
      
      lu = 1
    
      !----------------------------------------------------------------
      ! Instantaneous temperature effect on quantum yield efficiency
      !----------------------------------------------------------------
      ! ftemp_kphio = calc_ftemp_kphio( climate%dtemp, params_pft_plant(pft)%c4 )

      ! take the slowly varying temperature for governing quantum yield variations
      ftemp_kphio = calc_ftemp_kphio( temp_memory, params_pft_plant(pft)%c4 )

      !----------------------------------------------------------------
      ! P-model call to get a list of variables that are 
      ! acclimated to slowly varying conditions
      !----------------------------------------------------------------
      if (tile(lu)%plant(pft)%fpc_grid > 0.0 .and. &      ! PFT is present
          grid%dayl > 0.0 .and.                    &      ! no arctic night
          temp_memory > -5.0 ) then                      ! minimum temp threshold to avoid fpe

        !----------------------------------------------------------------
        ! With fAPAR = 1.0 (full light) for simulating Vcmax25
        !----------------------------------------------------------------
        ! print*,'kphio, ppfd_memory, co2_memory, temp_memory, vpd_memory, patm_memory ', params_pft_gpp(pft)%kphio * ftemp_kphio, ppfd_memory, co2_memory, temp_memory, vpd_memory, patm_memory 

        out_pmodel = pmodel(  &
                              kphio          = params_pft_gpp(pft)%kphio * ftemp_kphio, &
                              beta           = params_gpp%beta, &
                              ppfd           = ppfd_memory, &
                              co2            = co2_memory, &
                              tc             = temp_memory, &
                              vpd            = vpd_memory, &
                              patm           = patm_memory, &
                              c4             = params_pft_plant(pft)%c4, &
                              method_optci   = "prentice14", &
                              method_jmaxlim = "wang17" &
                              )

        ! ! xxx test
        ! out_pmodel = pmodel(  &
        !                       kphio          = 5.16605116e-02, &
        !                       beta           = params_gpp%beta, &
        !                       ppfd           = 1.77466325E-04, &
        !                       co2            = 369.548828, &
        !                       tc             = 7.46864176, &
        !                       vpd            = 433.062012, &
        !                       patm           = 98229.4453, &
        !                       c4             = params_pft_plant(pft)%c4, &
        !                       method_optci   = "prentice14", &
        !                       method_jmaxlim = "wang17" &
        !                       )

      else

        ! PFT is not present 
        out_pmodel = zero_pmodel()

      end if

      ! simple:
      if (nlu > 1) stop 'gpp: think about nlu > 1'
      lu = 1

      !----------------------------------------------------------------
      ! Calculate soil moisture stress as a function of soil moisture, mean alpha and vegetation type (grass or not)
      !----------------------------------------------------------------
      if (do_soilmstress) then
        soilmstress = calc_soilmstress( tile(1)%soil%phy%wscal, 0.0, params_pft_plant(1)%grass )
      else
        soilmstress = 1.0
      end if    

      !----------------------------------------------------------------
      ! GPP
      ! This still does a linear scaling of daily GPP - knowingly wrong
      ! but not too dangerous...
      !----------------------------------------------------------------
      tile_fluxes(lu)%plant(pft)%dgpp = tile(lu)%plant(pft)%fpc_grid * tile(lu)%canopy%fapar * climate%dppfd * myinterface%params_siml%secs_per_tstep * out_pmodel%lue * soilmstress
      
      !! print*,'gpp',tile_fluxes(lu)%plant(pft)%dgpp
      !! print*,'fpcgrid',tile(lu)%plant(pft)%fpc_grid
      !! print*,'fapar',tile(lu)%canopy%fapar
      !! print*,'ppfd', climate%dppfd
      !! print*,'secspertstep', myinterface%params_siml%secs_per_tstep
      !! print*,'lue', out_pmodel%lue
      !! print*,'soilmstress', soilmstress

      !----------------------------------------------------------------
      ! Dark respiration
      !----------------------------------------------------------------
      tile_fluxes(lu)%plant(pft)%drd = tile(lu)%plant(pft)%fpc_grid * tile(lu)%canopy%fapar * out_pmodel%vcmax25 * calc_ftemp_inst_rd( climate%dtemp ) * c_molmass

      !----------------------------------------------------------------
      ! Vcmax and Jmax
      !----------------------------------------------------------------
      ! acclimated quantities
      tile_fluxes(lu)%plant(pft)%vcmax25 = out_pmodel%vcmax25
      tile_fluxes(lu)%plant(pft)%jmax25  = out_pmodel%jmax25
      tile_fluxes(lu)%plant(pft)%chi     = out_pmodel%chi
      tile_fluxes(lu)%plant(pft)%iwue    = out_pmodel%iwue

      ! quantities with instantaneous temperature response
      tile_fluxes(lu)%plant(pft)%vcmax = calc_ftemp_inst_vcmax( climate%dtemp, climate%dtemp, tcref = 25.0 ) * out_pmodel%vcmax25
      tile_fluxes(lu)%plant(pft)%jmax  = calc_ftemp_inst_jmax(  climate%dtemp, climate%dtemp, tcref = 25.0 ) * out_pmodel%jmax25

      !----------------------------------------------------------------
      ! Stomatal conductance
      !----------------------------------------------------------------
      tile_fluxes(lu)%plant(pft)%gs_accl = out_pmodel%gs_setpoint

    end do pftloop

  end subroutine gpp



  ! function calc_dgpp( fapar, fpc_grid, dppfd, lue, ftemp_kphio, soilmstress ) result( my_dgpp )
  !   !//////////////////////////////////////////////////////////////////
  !   ! Calculates daily GPP given mean daily light use efficiency following
  !   ! a simple light use efficie model approach.
  !   !------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in) :: fapar       ! fraction of absorbed photosynthetically active radiation (unitless)
  !   real, intent(in) :: fpc_grid    ! foliar projective cover, used for dividing grid cell area (unitless)
  !   real, intent(in) :: dppfd       ! daily total photon flux density (mol m-2)
  !   real, intent(in) :: lue         ! light use efficiency (g CO2 mol-1)
  !   real, intent(in) :: ftemp_kphio  ! air temperature (deg C)
  !   real, intent(in) :: soilmstress ! soil moisture stress factor (unitless)

  !   ! function return variable
  !   real :: my_dgpp                 ! Daily total gross primary productivity (gC m-2 d-1)

  !   ! GPP is light use efficiency multiplied by absorbed light and soil moisture stress function
  !   my_dgpp = fapar * fpc_grid * dppfd * soilmstress * lue * ftemp_kphio

  ! end function calc_dgpp


  ! function calc_dassim( dgpp, daylength ) result( my_dassim )
  !   !//////////////////////////////////////////////////////////////////
  !   ! Calculates assimilation rate, mean over daylight hours.
  !   ! Use *_unitfapar to get something representative of top-of-canopy.
  !   !------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in) :: dgpp            ! daily total GPP (g CO2 m-2 d-1)
  !   real, intent(in) :: daylength       ! day length (h)

  !   ! function return variable
  !   real :: my_dassim                   ! canopy mean assimilation rate, mean over daylight hours (mol CO2 m-2 s-1)

  !   ! Assimilation rate, average over daylight hours
  !   if (daylength>0.0) then
  !     my_dassim = dgpp / ( 60.0 * 60.0 * daylength * c_molmass )
  !   else
  !     my_dassim = 0.0
  !   end if

  ! end function calc_dassim


  ! function calc_dgs( dassim, vpd, ca, gammastar, xi ) result( dgs )
  !   !//////////////////////////////////////////////////////////////////
  !   ! Calculates leaf-level stomatal conductance to CO2.
  !   ! This uses instantaneous VPD and is therefore not calculated inside
  !   ! the P-model function. The slope parameter 'xi' is representative
  !   ! for the acclimated response.
  !   !------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in) :: dassim          ! daily mean assimilation rate (mol CO2 m-2 s-1)
  !   real, intent(in) :: vpd             ! vapour pressure deficit (Pa)
  !   real, intent(in) :: ca              ! ambient CO2 partial pressure (Pa)
  !   real, intent(in) :: gammastar       ! CO2 compensation point (Pa)
  !   real, intent(in) :: xi              ! slope parameter of stomatal response derived from P-model optimality, corresponding to sqrt(beta*(K+gammastar)/(1.6*etastar)) (Pa)
  !   ! real, intent(in) :: dgs_unitiabs    ! stomatal conductance per unit absorbed light (mol CO2 Pa-1 m-2 s-1 / mol light)

  !   ! function return variable
  !   real :: dgs                         ! leaf-level stomatal conductance to H2O, mean over daylight hours ( mol CO2 Pa-1 m-2 s-1 )

  !   ! Leaf-level assimilation rate, average over daylight hours
  !   ! dgs = dassim * dgs_unitiabs
  !   dgs = (1.0 + xi / sqrt(vpd)) * dassim / (ca - gammastar)
  !   ! print*,'instantaneous gs: ', dgs 
    
  ! end function calc_dgs


  ! function calc_drd( fapar, fpc_grid, dppfd, rd_unitiabs, ftemp_kphio, soilmstress ) result( my_drd )
  !   !//////////////////////////////////////////////////////////////////
  !   ! Calculates daily total dark respiration (Rd) based on monthly mean 
  !   ! PPFD (assumes acclimation on a monthly time scale).
  !   ! Not described in Stocker et al., XXX.
  !   !------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in) :: fapar           ! fraction of absorbed photosynthetically active radiation
  !   real, intent(in) :: fpc_grid        ! foliar projective cover
  !   real, intent(in) :: dppfd           ! daily total photon flux density (mol m-2)
  !   real, intent(in) :: rd_unitiabs
  !   real, intent(in) :: ftemp_kphio      ! this day's air temperature, deg C
  !   real, intent(in) :: soilmstress     ! soil moisture stress factor

  !   ! function return variable
  !   real :: my_drd

  !   ! Dark respiration takes place during night and day (24 hours)
  !   my_drd = fapar * fpc_grid * dppfd * soilmstress * rd_unitiabs * ftemp_kphio * c_molmass

  ! end function calc_drd


  ! function calc_dtransp( fapar, acrown, dppfd, transp_unitiabs, ftemp_kphio, soilmstress ) result( my_dtransp )
  !   !//////////////////////////////////////////////////////////////////
  !   ! Calculates daily transpiration. 
  !   ! Exploratory only.
  !   ! Not described in Stocker et al., XXX.
  !   !------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in) :: fapar
  !   real, intent(in) :: acrown
  !   real, intent(in) :: dppfd              ! daily total photon flux density, mol m-2
  !   real, intent(in) :: transp_unitiabs
  !   real, intent(in) :: ftemp_kphio              ! this day's air temperature
  !   real, intent(in) :: soilmstress        ! soil moisture stress factor

  !   ! function return variable
  !   real :: my_dtransp

  !   ! GPP is light use efficiency multiplied by absorbed light and C-P-alpha
  !   my_dtransp = fapar * acrown * dppfd * soilmstress * transp_unitiabs * ftemp_kphio * h2o_molmass

  ! end function calc_dtransp


  ! function calc_vcmax_canop( fapar, vcmax_unitiabs, meanmppfd ) result( my_vcmax )
  !   !//////////////////////////////////////////////////////////////////
  !   ! Calculates canopy-level summed carboxylation capacity (Vcmax). To get
  !   ! value per unit leaf area, divide by LAI.
  !   ! Not described in Stocker et al., XXX.
  !   !------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in) :: fapar
  !   real, intent(in) :: vcmax_unitiabs
  !   real, intent(in) :: meanmppfd

  !   ! function return variable
  !   real :: my_vcmax    ! canopy-level Vcmax [gCO2/m2-ground/s]

  !   ! Calculate leafy-scale Rubisco-N as a function of LAI and current LUE
  !   my_vcmax = fapar * meanmppfd * vcmax_unitiabs

  ! end function calc_vcmax_canop


  ! function calc_g_canopy( g_stomata, lai, tk ) result( g_canopy )
  !   !/////////////////////////////////////////////////////////////////////////
  !   ! Calculates canopy conductance, proportional to the leaf area index.
  !   ! Since g_stomata is taken here from the photosynthesis module, we don't 
  !   ! use Eq. 8 in Zhang et al. (2017).
  !   !-------------------------------------------------------------------------
  !   use md_params_core, only: kR

  !   ! arguments
  !   real, intent(in) :: g_stomata      ! stomatal conductance (mol CO2 Pa-1 m-2 s-1)
  !   real, intent(in) :: lai            ! leaf area index, single-sided (unitless)
  !   real, intent(in) :: tk             ! (leaf) temperature (K)

  !   ! function return variable
  !   real :: g_canopy    ! canopy conductance (m s-1)

  !   ! canopy conductance scales with LAI. Including unit conversion to m s-1.
  !   g_canopy = 1.6 * g_stomata * kR * tk * lai

  ! end function calc_g_canopy


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
      ftemp = (-0.008 + 0.00375 * dtemp - 0.58e-4 * dtemp**2) * 8.0 ! Based on calibrated values by Shirley
      if (ftemp < 0.0) then
        ftemp = 0.0
      else
        ftemp = ftemp
      end if    
    else
      ftemp = 0.352 + 0.022 * dtemp - 3.4e-4 * dtemp**2  ! Based on Bernacchi et al., 2003
    end if
    
  end function calc_ftemp_kphio


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


  ! function calc_climate_acclimation( climate, grid, method ) result( climate_acclimation )
  !   !//////////////////////////////////////////////////////////////////
  !   ! Convert daily mean environmental conditions to conditions to
  !   ! which photosynthesis is acclimated to (daytime mean, or mid-day
  !   ! mean) 
  !   ! References:
  !   ! Jones, H. G. (2013) Microclimate: a Quantitative Approach to Environmental Plant
  !   !   Pysiology, Cambridge Press
  !   !------------------------------------------------------------------
  !   ! argument  
  !   type(climate_type) :: climate
  !   type(gridtype)     :: grid
  !   character(len=*)   :: method

  !   ! function return variable
  !   type(climate_type) :: climate_acclimation

  !   ! local variables
  !   real :: rx, tcgrowth_cru, tcmean_cru

  !   select case (method)
      
  !     case ("daytime")
  !       !----------------------------------------------------------------
  !       ! Mean daytime values
  !       !----------------------------------------------------------------
  !       ! Daytime mean temperature assuming the diurnal temperature cycle 
  !       ! to follow a sine curve, with daylight hours determined by latitude and month
  !       ! based on (Jones et al. 2013) as used in Peng et al. (in review)
  !       !----------------------------------------------------------------
  !       rx = -1.0 * tan(radians(grid%lat)) * tan(radians(grid%decl_angle))   ! decl: monthly average solar declination XXX how to calculate the declination angle

  !       !if (rx < -1) then
  !         !print*,'rx       ', rx
  !       !  rx = -1 !some grid data has outlier (-1.01 or -1.02), let's just assume them approximate to -1 so that no FPE
  !         !print*,'grid%lat       ', grid%lat
  !         !print*,'grid%decl_angle       ', grid%decl_angle
  !         !print*,'rx < -1      ', rx
  !       !else  
  !       !end if
 
  !       !if (rx >= 1) then
  !       !  rx = 0.99 !some grid data has outlier (1 or 1.01), let's just assume them approximate to 1 so that no FPE
  !         !print*,'grid%lat       ', grid%lat
  !         !print*,'grid%decl_angle       ', grid%decl_angle
  !         !print*,'rx > 1      ', rx
  !       !else  
  !       !end if
  !       if (rx > -1 .and. rx < 1) then
  !         tcgrowth_cru = climate%dtmax * (0.5 + (1.0 - rx**2) / (2.0 * acos(rx))) + &
  !         climate%dtmin * (0.5 - (1.0 - rx**2) / (2.0 * acos(rx)))
  !         tcmean_cru   = (climate%dtmax + climate%dtmin) / 2.0
  !       !----------------------------------------------------------------
  !       ! Take difference of daytime temperature to mean temperature based on 
  !       ! monthly CRU data (using Tmin and Tmax) and add it to the daily mean
  !       ! temperature based on daily WATCH-WFDEI data (Tmin and Tmax not available)
  !       !----------------------------------------------------------------
  !         climate_acclimation%dtemp = climate%dtemp + tcgrowth_cru - tcmean_cru
  !       else
  !         climate_acclimation%dtemp = climate%dtemp
  !       end if  
  !       !----------------------------------------------------------------
  !       ! Daytime mean radiation
  !       !----------------------------------------------------------------
  !       if (grid%dayl==0.0) then
  !         climate_acclimation%dppfd = 0.0
  !       else  
  !         climate_acclimation%dppfd = climate%dppfd * myinterface%params_siml%secs_per_tstep / grid%dayl
  !       end if

  !       !climate_acclimation%dppfd = climate%dppfd * interface%params_siml%secs_per_tstep / grid%dayl

  !       !----------------------------------------------------------------
  !       ! vpd is based on Tmin and Tmax and represents a daily (24.0) mean
  !       ! currently, we have no good method to derive daytime mean other than
  !       ! using growth temperature and the common formula, but this is not 
  !       ! used here (yet?).
  !       !----------------------------------------------------------------
  !       climate_acclimation%dvpd = climate%dvpd 

  !       ! atmospheric pressure has no diurnal cycle
  !       climate_acclimation%dpatm = climate%dpatm 

  !     case default

  !       stop "calc_climate_acclimation: Provide valid method."

  !   end select

  ! end function calc_climate_acclimation  


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
    params_gpp%tau_acclim     = 30.0
    params_gpp%soilm_par_a    = myinterface%params_calib%soilm_par_a     ! is provided through standard input
    params_gpp%soilm_par_b    = myinterface%params_calib%soilm_par_b     ! is provided through standard input

    ! PFT-dependent parameter(s)
    params_pft_gpp(:)%kphio = myinterface%params_calib%kphio  ! is provided through standard input

  end subroutine getpar_modl_gpp


end module md_gpp_pmodel
