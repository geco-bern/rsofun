module md_gpp_pmodel
  !////////////////////////////////////////////////////////////////
  ! Module containing a wrapper for using the P-model photosynthesis
  ! scheme.
  !----------------------------------------------------------------
  use md_params_core
  use md_tile_pmodel, only: tile_type, tile_fluxes_type
  use md_interface_pmodel, only: myinterface
  use md_forcing_pmodel, only: climate_type, vegcover_type
  use md_plant_pmodel, only: params_pft_plant
  use md_sofunutils, only: radians
  use md_grid, only: gridtype
  use md_photosynth, only: pmodel, zero_pmodel, outtype_pmodel, calc_ftemp_inst_vcmax, calc_ftemp_inst_jmax, &
    calc_ftemp_inst_rd, calc_kphio_temp, calc_soilmstress

  implicit none

  private
  public params_pft_gpp, gpp, getpar_modl_gpp
    
  !-----------------------------------------------------------------------
  ! Uncertain (unknown) parameters. Runtime read-in
  !-----------------------------------------------------------------------
  type paramstype_gpp
    real :: beta         ! Unit cost of carboxylation (dimensionless)
    real :: soilm_thetastar
    real :: soilm_betao
    real :: rd_to_vcmax  ! Ratio of Rdark to Vcmax25, number from Atkin et al., 2015 for C3 herbaceous
    real :: tau_acclim   ! acclimation time scale of photosynthesis (d)
    real :: tau_acclim_tempstress
    real :: par_shape_tempstress
    real :: kc_jmax
  end type paramstype_gpp

  ! PFT-DEPENDENT PARAMETERS
  type pftparamstype_gpp
    real :: kphio        ! quantum yield efficiency at optimal temperature, phi_0 (Stocker et al., 2020 GMD Eq. 10)
    real :: kphio_par_a  ! shape parameter of temperature-dependency of quantum yield efficiency
    real :: kphio_par_b  ! optimal temperature of quantum yield efficiency (deg C)
  end type pftparamstype_gpp

  type(paramstype_gpp) :: params_gpp
  type(pftparamstype_gpp), dimension(npft) :: params_pft_gpp

contains

  subroutine gpp( tile, tile_fluxes, co2, climate, grid, init, in_ppfd)
    !//////////////////////////////////////////////////////////////////
    ! Wrapper function to call to P-model. 
    ! Calculates meteorological conditions with memory based on daily
    ! varying variables.
    ! Calculates soil moisture and temperature stress functions.
    ! Calls P-model.
    !------------------------------------------------------------------
    ! use md_plant, only: params_pft_plant, plant_type, plant_fluxes_type
    use md_sofunutils, only: dampen_variability

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    real, intent(in)    :: co2                               ! atmospheric CO2 (ppm)
    type(climate_type)  :: climate
    type(gridtype)      :: grid
    logical, intent(in) :: init                              ! is true on the very first simulation day (first subroutine call of each gridcell)
    logical, intent(in) :: in_ppfd                           ! whether to use PPFD from forcing or from SPLASH output

    ! local variables
    type(outtype_pmodel) :: out_pmodel              ! list of P-model output variables
    type(climate_type)   :: climate_acclimation     ! list of climate variables to which P-model calculates acclimated traits
    integer    :: pft
    integer    :: lu
    real       :: soilmstress
    real       :: kphio_temp          ! quantum yield efficiency after temperature influence
    real       :: tk

    real, save :: co2_memory
    real, save :: vpd_memory
    real, save :: temp_memory
    real, save :: patm_memory
    real, save :: ppfd_memory
    integer, save :: count

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
      count = 0
      co2_memory  = co2
      temp_memory = climate_acclimation%dtemp
      vpd_memory  = climate_acclimation%dvpd
      patm_memory = climate_acclimation%dpatm
      ppfd_memory = climate_acclimation%dppfd
    end if 

    count = count + 1

    co2_memory  = dampen_variability( co2,                       params_gpp%tau_acclim, co2_memory  )
    temp_memory = dampen_variability( climate_acclimation%dtemp, params_gpp%tau_acclim, temp_memory )
    vpd_memory  = dampen_variability( climate_acclimation%dvpd,  params_gpp%tau_acclim, vpd_memory  )
    patm_memory = dampen_variability( climate_acclimation%dpatm, params_gpp%tau_acclim, patm_memory )
    ppfd_memory = dampen_variability( climate_acclimation%dppfd, params_gpp%tau_acclim, ppfd_memory )

    tk = climate_acclimation%dtemp + kTkelvin


    pftloop: do pft=1,npft
      
      lu = 1
    
      !----------------------------------------------------------------
      ! Low-temperature effect on quantum yield efficiency 
      !----------------------------------------------------------------
      ! take the instananeously varying temperature for governing quantum yield variations
      if (abs(params_pft_gpp(pft)%kphio_par_a) < eps) then
        kphio_temp = params_pft_gpp(pft)%kphio
      else
        kphio_temp = calc_kphio_temp( climate%dtemp, &
                                      params_pft_plant(pft)%c4, &
                                      params_pft_gpp(pft)%kphio, &
                                      params_pft_gpp(pft)%kphio_par_a, &
                                      params_pft_gpp(pft)%kphio_par_b )
      end if
      
      !----------------------------------------------------------------
      ! P-model call to get a list of variables that are 
      ! acclimated to slowly varying conditions
      !----------------------------------------------------------------
      if (tile(lu)%plant(pft)%fpc_grid > 0.0 .and. &      ! PFT is present
          grid%dayl > 0.0 .and.                    &      ! no arctic night
          temp_memory > -5.0 ) then                       ! minimum temp threshold to avoid fpe

        !================================================================
        ! P-model call to get acclimated quantities as a function of the
        ! damped climate forcing.
        !----------------------------------------------------------------
        out_pmodel = pmodel(  &
                              kphio          = kphio_temp, &
                              beta           = params_gpp%beta, &
                              kc_jmax        = params_gpp%kc_jmax, &
                              ppfd           = ppfd_memory, &
                              co2            = co2_memory, &
                              tc             = temp_memory, &
                              vpd            = vpd_memory, &
                              patm           = patm_memory, &
                              c4             = params_pft_plant(pft)%c4, &
                              method_optci   = "prentice14", &
                              method_jmaxlim = "wang17" &
                              )

      else

        ! PFT is not present 
        out_pmodel = zero_pmodel()

      end if

      ! simple:
      ! if (nlu > 1) stop 'gpp: think about nlu > 1'
      lu = 1

      !================================================================
      ! Instantaneous responses using the acclimated photosynthetic 
      ! capacities.
      !----------------------------------------------------------------
      ! Calculate soil moisture stress as a function of soil moisture, mean alpha and vegetation type (grass or not)
      !----------------------------------------------------------------
      soilmstress = calc_soilmstress( tile(1)%soil%phy%wcont, &
                                      params_gpp%soilm_thetastar, &
                                      params_gpp%soilm_betao )

      !----------------------------------------------------------------
      ! GPP
      ! This still does a linear scaling of daily GPP - knowingly wrong
      ! but not too dangerous...
      !----------------------------------------------------------------
      if( in_ppfd ) then
        ! Take input daily PPFD (in mol/m^2)
        tile_fluxes(lu)%plant(pft)%dgpp = tile(lu)%plant(pft)%fpc_grid * tile(lu)%canopy%fapar &
          * climate%dppfd * myinterface%params_siml%secs_per_tstep * out_pmodel%lue * soilmstress
      else
        ! Take daily PPFD generated by SPLASH (in mol/m^2/d)
        tile_fluxes(lu)%plant(pft)%dgpp = tile(lu)%plant(pft)%fpc_grid * tile(lu)%canopy%fapar &
          * tile_fluxes(lu)%canopy%ppfd_splash * out_pmodel%lue * soilmstress
      end if

      !----------------------------------------------------------------
      ! Dark respiration
      !----------------------------------------------------------------
      tile_fluxes(lu)%plant(pft)%drd = tile(lu)%plant(pft)%fpc_grid * tile(lu)%canopy%fapar &
        * out_pmodel%vcmax25 * params_gpp%rd_to_vcmax * calc_ftemp_inst_rd( climate%dtemp ) * c_molmass &
        * myinterface%params_siml%secs_per_tstep

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



  ! function calc_dgpp( fapar, fpc_grid, dppfd, lue, kphio_temp, soilmstress ) result( my_dgpp )
  !   !//////////////////////////////////////////////////////////////////
  !   ! Calculates daily GPP given mean daily light use efficiency following
  !   ! a simple light use efficie model approach.
  !   !------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in) :: fapar       ! fraction of absorbed photosynthetically active radiation (unitless)
  !   real, intent(in) :: fpc_grid    ! foliar projective cover, used for dividing grid cell area (unitless)
  !   real, intent(in) :: dppfd       ! daily total photon flux density (mol m-2)
  !   real, intent(in) :: lue         ! light use efficiency (g CO2 mol-1)
  !   real, intent(in) :: kphio_temp  ! air temperature (deg C)
  !   real, intent(in) :: soilmstress ! soil moisture stress factor (unitless)

  !   ! function return variable
  !   real :: my_dgpp                 ! Daily total gross primary productivity (gC m-2 d-1)

  !   ! GPP is light use efficiency multiplied by absorbed light and soil moisture stress function
  !   my_dgpp = fapar * fpc_grid * dppfd * soilmstress * lue * kphio_temp

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


  ! function calc_drd( fapar, fpc_grid, dppfd, rd_unitiabs, kphio_temp, soilmstress ) result( my_drd )
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
  !   real, intent(in) :: kphio_temp      ! this day's air temperature, deg C
  !   real, intent(in) :: soilmstress     ! soil moisture stress factor

  !   ! function return variable
  !   real :: my_drd

  !   ! Dark respiration takes place during night and day (24 hours)
  !   my_drd = fapar * fpc_grid * dppfd * soilmstress * rd_unitiabs * kphio_temp * c_molmass

  ! end function calc_drd


  ! function calc_dtransp( fapar, acrown, dppfd, transp_unitiabs, kphio_temp, soilmstress ) result( my_dtransp )
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
  !   real, intent(in) :: kphio_temp              ! this day's air temperature
  !   real, intent(in) :: soilmstress        ! soil moisture stress factor

  !   ! function return variable
  !   real :: my_dtransp

  !   ! GPP is light use efficiency multiplied by absorbed light and C-P-alpha
  !   my_dtransp = fapar * acrown * dppfd * soilmstress * transp_unitiabs * kphio_temp * h2o_molmass

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
    ! unit cost of carboxylation, b/a' in Eq. 3 (Stocker et al., 2020 GMD)
    params_gpp%beta = myinterface%params_calib%beta_unitcostratio ! 146.000000

    ! Ratio of Rdark to Vcmax25, fitted slope of Rd25/Vcmax25 (Wang et al., 2020 GCB, 10.1111/gcb.14980, Table S6)
    params_gpp%rd_to_vcmax = myinterface%params_calib%rd_to_vcmax ! 0.01400000

    ! Jmax cost coefficient, c* in Stocker et al., 2020 GMD (Eq 15) and Wang et al., 2017
    params_gpp%kc_jmax = myinterface%params_calib%kc_jmax  ! 0.41

    ! Acclimation time scale for photosynthesis (d), multiple lines of evidence suggest about monthly is alright 
    params_gpp%tau_acclim = myinterface%params_calib%tau_acclim  ! 30.0

    ! Re-interpreted soil moisture stress parameter, previously thetastar = 0.6
    params_gpp%soilm_thetastar = myinterface%params_calib%soilm_thetastar

    ! Re-interpreted soil moisture stress parameter, previosly determined by Eq. 22
    params_gpp%soilm_betao = myinterface%params_calib%soilm_betao

    ! quantum yield efficiency at optimal temperature, phi_0 (Stocker et al., 2020 GMD Eq. 10)
    params_pft_gpp(:)%kphio = myinterface%params_calib%kphio

    ! shape parameter of temperature-dependency of quantum yield efficiency
    params_pft_gpp(:)%kphio_par_a = myinterface%params_calib%kphio_par_a

    ! optimal temperature of quantum yield efficiency
    params_pft_gpp(:)%kphio_par_b = myinterface%params_calib%kphio_par_b


  end subroutine getpar_modl_gpp


end module md_gpp_pmodel
