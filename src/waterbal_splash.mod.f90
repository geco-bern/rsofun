module md_waterbal
  !////////////////////////////////////////////////////////////////
  ! Ecosystem water balance using SPLASH.
  ! Code adopted from https://doi.org/10.5281/zenodo.376293
  ! Written by Benjamin Stocker, partly based on Python code by
  ! Tyler Davis (under GPL2.1).
  !----------------------------------------------------------------
  use md_params_core
  use md_tile_pmodel, only: tile_type, tile_fluxes_type
  use md_forcing_pmodel, only: climate_type
  use md_grid, only: gridtype
  use md_interface_pmodel, only: myinterface
  use md_sofunutils, only: radians, dgsin, dgcos, degrees

  implicit none

  private
  public waterbal, solar, getpar_modl_waterbal

  integer, parameter :: int4=SELECTED_INT_KIND(4)

  !-----------------------------------------------------------------------
  ! Uncertain (unknown) parameters. Runtime read-in
  !-----------------------------------------------------------------------
  real :: maxmeltrate       ! maximum snow melting rate (mm d-1) (Orth et al., 2013) 
  real :: kA                ! constant for dRnl (Monteith & Unsworth, 1990)
  real :: kalb_sw           ! shortwave albedo (Federer, 1968)
  real :: kalb_vis          ! visible light albedo (Sellers, 1985)
  real :: kb                ! constant for dRnl (Linacre, 1968)
  real :: kc                ! cloudy transmittivity (Linacre, 1968)
  real :: kCw               ! supply constant, mm/hr (Federer, 1982)
  real :: kd                ! angular coefficient of transmittivity (Linacre, 1968)
  real :: ke                ! eccentricity for 2000 CE (Berger, 1978)
  real :: keps              ! obliquity for 2000 CE, degrees (Berger, 1978)
  real :: kw                ! entrainment factor (Lhomme, 1997; Priestley & Taylor, 1972)
  real :: komega            ! longitude of perihelion for 2000 CE, degrees (Berger, 1978)

  !----------------------------------------------------------------
  ! MODULE-SPECIFIC, PRIVATE VARIABLES
  !----------------------------------------------------------------
  real :: dr                           ! distance factor
  real :: hs                           ! sunset hour angle
  real :: hn                           ! net radiation cross-over hour angle
  real :: tau                          ! transmittivity (unitless)
  real :: ru                           ! variable substitute for u
  real :: rv                           ! variable substitute for v
  real :: rw                           ! variable substitute (W/m^2)
  real :: energy_to_mm                 ! Conversion factor to convert energy (J m-2 day) to mass (mm day-1)

  ! holds return variables of function get_snow_rain()
  type outtype_snow_rain
    real :: snow_updated     ! snow depth in water equivalents (mm)
    real :: liquid_to_soil   ! water 
  end type outtype_snow_rain

  logical, parameter :: splashtest = .false.

  integer (kind = int4), parameter :: ET_PT = 0, ET_PT_DIFFUSIOM = 1, ET_PT_PM = 2

contains

  subroutine waterbal( tile, tile_fluxes, grid, climate, fapar, using_phydro, using_gs, using_pml )
    !/////////////////////////////////////////////////////////////////////////
    ! Calculates soil water balance
    !-------------------------------------------------------------------------
    ! arguments
    type(tile_type), dimension(nlu), intent(inout)        :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(gridtype), intent(in)                            :: grid
    type(climate_type), intent(in)                        :: climate
    real, dimension(nlu), intent(in)                      :: fapar
    logical, intent(in)                                   :: using_phydro
    logical, intent(in)                                   :: using_gs
    logical, intent(in)                                   :: using_pml

    ! local variables
    type(outtype_snow_rain) :: out_snow_rain
    integer                 :: lu              ! land unit (gridcell tile)
    real                    :: sw              ! evaporative supply rate (mm/h)

    ! Loop over gricell tiles
    luloop: do lu=1,nlu

      ! Calculate evaporative supply rate, mm/h
      sw = kCw * tile(lu)%soil%phy%wcont / tile(lu)%soil%params%whc

      !---------------------------------------------------------
      ! Canopy transpiration and soil evaporation
      !---------------------------------------------------------
      call calc_et( tile_fluxes(lu), grid, climate, sw, fapar(lu), using_phydro, using_gs, using_pml )

      !---------------------------------------------------------
      ! Update soil moisture and snow pack
      !---------------------------------------------------------
      out_snow_rain = get_snow_rain( &
        climate%dprec * myinterface%params_siml%secs_per_tstep + tile_fluxes(lu)%canopy%dcn, &
        climate%dsnow * myinterface%params_siml%secs_per_tstep, &
        climate%dtemp, &
        tile(lu)%soil%phy%snow &
        )
      tile(lu)%soil%phy%snow = out_snow_rain%snow_updated 

      ! Update soil moisture
      tile(lu)%soil%phy%wcont = tile(lu)%soil%phy%wcont + out_snow_rain%liquid_to_soil - tile_fluxes(lu)%canopy%daet

      ! Bucket model for runoff generation
      if (tile(lu)%soil%phy%wcont > tile(lu)%soil%params%whc) then

        ! -----------------------------------
        ! Bucket is full 
        ! -----------------------------------
        ! determine NO3 leaching fraction 
        tile_fluxes(lu)%canopy%dfleach = 1.0 - tile(lu)%soil%params%whc / tile(lu)%soil%phy%wcont

        ! add remaining water to monthly runoff total
        tile_fluxes(lu)%canopy%dro = tile(lu)%soil%phy%wcont - tile(lu)%soil%params%whc

        ! set soil moisture to capacity
        tile(lu)%soil%phy%wcont = tile(lu)%soil%params%whc

      else if (tile(lu)%soil%phy%wcont < 0.0) then
        ! -----------------------------------
        ! Bucket is empty
        ! -----------------------------------
        ! set soil moisture to zero
        ! and reduce total actual evapotranspiration (daet) by reducing canopy transpiration (daet_canop)
        tile_fluxes(lu)%canopy%daet_canop = tile_fluxes(lu)%canopy%daet_canop + tile(lu)%soil%phy%wcont *  &
                                            (tile_fluxes(lu)%canopy%daet_canop/ tile_fluxes(lu)%canopy%daet)
                                            ! Is this numerically stable?
        tile_fluxes(lu)%canopy%daet_soil  = tile_fluxes(lu)%canopy%daet_soil  + tile(lu)%soil%phy%wcont * & 
                                            (tile_fluxes(lu)%canopy%daet_soil / tile_fluxes(lu)%canopy%daet) 
                                            ! Is this numerically stable?
        tile_fluxes(lu)%canopy%daet       = tile_fluxes(lu)%canopy%daet       + tile(lu)%soil%phy%wcont
        
        tile_fluxes(lu)%canopy%daet_e_canop = tile_fluxes(lu)%canopy%daet_canop / energy_to_mm 
        tile_fluxes(lu)%canopy%daet_e_soil  = tile_fluxes(lu)%canopy%daet_soil  / energy_to_mm 
        tile_fluxes(lu)%canopy%daet_e       = tile_fluxes(lu)%canopy%daet       / energy_to_mm
        
        tile(lu)%soil%phy%wcont        = 0.0
        tile_fluxes(lu)%canopy%dro     = 0.0
        tile_fluxes(lu)%canopy%dfleach = 0.0

      else
        ! No runoff occurrs
        tile_fluxes(lu)%canopy%dro     = 0.0
        tile_fluxes(lu)%canopy%dfleach = 0.0

      end if

      ! water scalar (fraction of plant-available water holding capacity; water storage at wilting point is already accounted for in tile(lu)%soil%params%whc)
      ! WHC = FC - PWP
      ! WSCAL = (WCONT - PWP) / (FC - PWP)
      tile(lu)%soil%phy%wscal = tile(lu)%soil%phy%wcont / tile(lu)%soil%params%whc

    end do luloop

  end subroutine waterbal


  subroutine solar( tile_fluxes, grid, climate, doy, in_netrad ) 
    !/////////////////////////////////////////////////////////////////////////
    ! This subroutine calculates daily PPFD. Code is an extract of the subroutine
    ! 'evap', adopted from the evap() function in GePiSaT (Python version). 
    ! This subroutine ('get_solar') is called before the daily loop.
    ! Output:
    ! - daily extraterrestrial solar radiation (dra), J/m^2
    ! - daily PPFD (dppfd), mol/m^2
    !-------------------------------------------------------------------------  
    ! arguments
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(gridtype), intent(inout)                         :: grid
    type(climate_type), intent(in)                        :: climate
    integer, intent(in)                                   :: doy        ! day of year
    logical, intent(in)                                   :: in_netrad

    !---------------------------------------------------------
    ! 2. Calculate heliocentric longitudes (nu and lambda), degrees
    ! Store daily return values for later use in subroutine 'evap'.
    ! Other variables defined and over-written below may be stored
    ! for later use in 'evap' the same way. However function 
    ! 'out_berger' is by far the most expensive one. This is there-
    ! fore a compromise.
    !---------------------------------------------------------
    ! Berger (1978)
    call get_berger_tls( doy, grid )
    ! grid%nu     = out_berger%nu
    ! grid%lambda = out_berger%lambda

    !---------------------------------------------------------
    ! 3. Calculate distance factor (dr), unitless
    !---------------------------------------------------------
    dr = calc_dr( grid%nu )

    !---------------------------------------------------------
    ! 4. Calculate declination angle, degrees
    !---------------------------------------------------------
    grid%decl_angle = calc_decl_angle( grid%lambda )
    
    !---------------------------------------------------------
    ! 5. Calculate variable substitutes (ru and rv), unitless
    !---------------------------------------------------------
    call calc_ru_rv( grid%decl_angle, grid%lat )

    !---------------------------------------------------------
    ! 6. Calculate the sunset hour angle (hs), degrees
    !---------------------------------------------------------
    hs = calc_hs( ru, rv )

    !---------------------------------------------------------
    ! 6.a Calculate day length from sunset hour angle, seconds
    !---------------------------------------------------------
    grid%dayl = 24.0 * 60 * 60 * hs / 180.0  ! hs is in degrees (pi = 180 deg)

    !---------------------------------------------------------
    ! 7. Calculate daily extraterrestrial solar radiation (dra), J/m^2/d
    !---------------------------------------------------------
    ! Eq. 1.10.3, Duffy & Beckman (1993)
    tile_fluxes(:)%canopy%dra = ( secs_per_day / pi ) * kGsc * dr * ( radians(ru) * hs + rv * dgsin(hs) )

    !---------------------------------------------------------
    ! 8. Calculate transmittivity (tau), unitless
    !---------------------------------------------------------
    tau = calc_tau( climate%dfsun, grid%elv )

    !---------------------------------------------------------
    ! 9. Calculate daily PPFD (dppfd), mol/m^2
    !---------------------------------------------------------
    ! Eq. 57, SPLASH 2.0 Documentation
    tile_fluxes(:)%canopy%ppfd_splash = (1.0e-6) * kfFEC * ( 1.0 - kalb_vis ) * tau * tile_fluxes(:)%canopy%dra

    !---------------------------------------------------------
    ! 10. Estimate net longwave radiation (out_evap%rnl), W m-2
    !---------------------------------------------------------
    ! Eq. 11, Prentice et al. (1993); Eq. 5 and 6, Linacre (1968)
    tile_fluxes(:)%canopy%rnl = ( kb + (1.0 - kb ) * climate%dfsun ) * ( kA - climate%dtemp )
    
    !---------------------------------------------------------
    ! 11. Calculate variable substitute (rw), W m-2 -- shortwave radiation?
    !---------------------------------------------------------
    rw = ( 1.0 - kalb_sw ) * tau * kGsc * dr

    !---------------------------------------------------------
    ! 12. Calculate net radiation cross-over hour angle (hn), degrees
    !---------------------------------------------------------
    if ((tile_fluxes(1)%canopy%rnl - rw * ru)/(rw * rv) >= 1.0) then
      ! Net radiation negative all day
      hn = 0.0
    else if ((tile_fluxes(1)%canopy%rnl - rw * ru)/(rw * rv) <= -1.0) then
      ! Net radiation positive all day
      hn = 180.0
    else
      !hn = degrees( dacos((tile_fluxes%canopy%rnl - rw*ru)/(rw*rv)) )
      hn = degrees( acos((tile_fluxes(1)%canopy%rnl - rw*ru)/(rw*rv)) )   ! use acos with single precision compilation
    end if

    !---------------------------------------------------------
    ! 13. Calculate daytime total net radiation (tile_fluxes%canopy%drn), J m-2 d-1
    !---------------------------------------------------------
    ! Eq. 53, SPLASH 2.0 Documentation
    ! Jaideep Note: reverted this change for testing against old phydro
    if (in_netrad) then
      tile_fluxes(:)%canopy%drn = climate%dnetrad * myinterface%params_siml%secs_per_tstep
    else
      tile_fluxes(:)%canopy%drn = (secs_per_day/pi) * (hn*(pi/180.0)*(rw*ru - tile_fluxes(:)%canopy%rnl) + rw*rv*dgsin(hn))
    end if
    ! tile_fluxes(:)%canopy%drn = (secs_per_day/pi) * (hn*(pi/180.0)*(rw*ru - tile_fluxes(:)%canopy%rnl) + rw*rv*dgsin(hn))

    !---------------------------------------------------------
    ! 14. Calculate nighttime total net radiation (tile_fluxes(:)%canopy%drnn), J m-2 d-1
    !---------------------------------------------------------
    ! Eq. 56, SPLASH 2.0 Documentation
    ! adopted bugfix from Python version (iss#13)
    ! Jaideep Note: reverted this change for testing against old phydro
    if (in_netrad) then
      tile_fluxes(:)%canopy%drnn = 0.0
    else  
      tile_fluxes(:)%canopy%drnn = (86400.0/pi)*(radians(rw*ru*(hs-hn)) + rw*rv*(dgsin(hs)-dgsin(hn)) - &
        tile_fluxes(:)%canopy%rnl * (pi - radians(hn)))
    end if
    ! tile_fluxes(:)%canopy%drnn = (86400.0/pi)*(radians(rw*ru*(hs-hn)) + rw*rv*(dgsin(hs)-dgsin(hn)) - &
    !   tile_fluxes(:)%canopy%rnl * (pi - radians(hn)))

    ! if (splashtest) then
    !   print*,'transmittivity, tau: ', tau
    !   print*,'daily TOA radiation: ', (1.0e-6)*tile_fluxes(:)%canopy%dra
    !   print*,'sunset angle, hs: ', hs
    !   print*,'true anomaly, nu: ', grid%nu
    !   print*,'true longitude, lambda: ', grid%lambda
    !   print*,'distance factor, dr: ', dr
    !   print*,'declination, grid%decl_angle: ', grid%decl_angle
    !   print*,'variable substitute, ru: ', ru
    !   print*,'variable substitute, rv: ', rv
    !   print*,'daily PPFD: ', tile_fluxes(:)%canopy%ppfd_splash
    ! end if
  
  end subroutine solar


  subroutine calc_et( tile_fluxes, grid, climate, sw, fapar, using_phydro, using_gs, using_pml )
    !/////////////////////////////////////////////////////////////////////////
    !
    !-------------------------------------------------------------------------  
    use md_sofunutils, only: calc_patm
    use md_sofunutils, only: dampen_variability

    ! arguments
    type(tile_fluxes_type), intent(inout) :: tile_fluxes
    type(gridtype), intent(in)            :: grid
    type(climate_type), intent(in)        :: climate
    real, intent(in)                      :: sw            ! evaporative supply rate, mm/hr
    real, intent(in)                      :: fapar          
    logical, intent(in)                   :: using_phydro
    logical, intent(in)                   :: using_gs   ! Should Pmodel/Phydro gs be used in ET calc? (otherwise, PT formulation will be used)
    logical, intent(in)                   :: using_pml  ! If using Pmodel/Phydro gs, should ET be calculated using PM equation (otherwise, diffusion equation will be used)

    ! local variables
    real :: gamma                           ! psychrometric constant (Pa K-1) ! xxx Zhang et al. use it in units of (kPa K-1), probably they use sat_slope in kPa/K, too.
    real :: sat_slope                       ! slope of saturation vapour pressure vs. temperature curve, Pa K-1
    real :: lv                              ! enthalpy of vaporization, J/kg
    real :: cp                              ! heat capacity of moist air, J kg-1 K-1
    real :: rho_water                       ! density of water (g m-3)
    real :: f_soil_aet                      ! Fractional reduction of soil AET due to moisture limitation
    real :: p_over_pet_memory               ! P/PET
    real, save :: p_memory = 0.0            ! precipitation, damped variability
    real, save :: pet_memory = 0.0          ! equilibrium evapotranspiration, damped variability

    real :: rx                             ! variable substitute (mm/hr)/(W/m^2)
    real :: hi, cos_hi                     ! intersection hour angle, degrees

    ! Used when using_pml == .true.
    real :: ga                              ! aerodynamic conductance to water vapour
    real :: epsilon                         ! variable substitute
    real :: gw                              ! canopy conductance to water vapour

    ! Used when using_gs == .true.
    real :: dpet_soil                       ! potential soil evaporation (not limited by soil moisture), mm d-1

    !---------------------------------------------------------
    ! Calculate water-to-energy conversion (econ), m^3/J
    !---------------------------------------------------------
    ! Slope of saturation vap press temp curve, Pa K-1
    sat_slope = calc_sat_slope( climate%dtemp )

    ! Heat capacity of moist air, J kg-1 K-1
    cp = calc_cp_moist_air( climate%dtemp )

    ! Enthalpy of vaporization, J/kg
    lv = calc_enthalpy_vap( climate%dtemp )
    
    ! Density of water, kg/m^3
    rho_water = density_h2o( climate%dtemp, calc_patm( grid%elv ) )

    ! Psychrometric constant, Pa/K
    gamma = psychro( climate%dtemp, calc_patm( grid%elv ) )
    
    ! Eq. 51, SPLASH 2.0 Documentation
    ! tile_fluxes%canopy%econ = 1.0 / ( lv * rho_water ) ! this is to convert energy into mass (water) - JAIDEEP: This is correct. J m-2 s-1 x (kg-1 m3) x (J-1 kg) = m3 m-2 s-1 = m s-1
    energy_to_mm = 1.0e3 / ( lv * rho_water ) ! (J m-2 d-1) x (kg-1 m3) x (J-1 kg) x (mm m-1) = m3 m-2 d-1 = mm d-1

    ! JAIDEEP: If it's just conversion from mass to energy, the above formula is correct. This already has the Priestly Taylor factor (s/(s+y)) built in, so this 
    ! should not be used for mere conversion. I would suggest you use just the factor s/(s+y) separately in the respective equations for clarity.
    tile_fluxes%canopy%econ = sat_slope / (lv * rho_water * (sat_slope + gamma)) ! MORE PRECISELY - this is to convert energy into mass (water).

    !---------------------------------------------------------
    ! Daily condensation, mm d-1
    !---------------------------------------------------------
    tile_fluxes%canopy%dcn = 1000.0 * tile_fluxes%canopy%econ * abs(tile_fluxes%canopy%drnn) ! Jaideep: Why abs here? drnn must be negative (emitted from earth) for condensation right? 

    !---------------------------------------------------------
    ! Estimate daily EET, mm d-1
    !---------------------------------------------------------
    ! Eq. 70, SPLASH 2.0 Documentation
    tile_fluxes%canopy%deet = 1000.0 * tile_fluxes%canopy%econ * tile_fluxes%canopy%drn

    !---------------------------------------------------------
    ! Estimate daily PET, mm d-1
    !---------------------------------------------------------
    ! Eq. 72, SPLASH 2.0 Documentation
    tile_fluxes%canopy%dpet   = ( 1.0 + kw ) * tile_fluxes%canopy%deet
    tile_fluxes%canopy%dpet_e = tile_fluxes%canopy%dpet / energy_to_mm ! JAIDEEP FIXME [resolved]: Oops! This is a case where you should use a simple mass-energy conversion, not econ
    
    !---------------------------------------------------------
    ! Calculate variable substitute (rx), (mm/hr)/(W/m^2)
    !---------------------------------------------------------
    rx = 1000.0 * 3600.0 * ( 1.0 + kw ) * tile_fluxes%canopy%econ

    !---------------------------------------------------------
    ! Calculate the intersection hour angle (hi), degrees
    !---------------------------------------------------------
    cos_hi = sw/(rw*rv*rx) + tile_fluxes%canopy%rnl/(rw*rv) - ru/rv   ! sw contains info of soil moisture (evaporative supply rate)
    
    if (cos_hi >= 1.0) then
      ! Supply exceeds demand:
      hi = 0.0
    elseif (cos_hi <= -1.0) then
      ! Supply limits demand everywhere:
      hi = 180.0
    else
      hi = degrees(acos(cos_hi))
    end if
    
    !---------------------------------------------------------
    ! Estimate daily AET (tile_fluxes%canopy%daet), mm d-1
    ! Estimate daily LE  (tile_fluxes%canopy%daet_e), J d-1
    !---------------------------------------------------------
    ! JAIDEEP FIXME: soil PET calcs should be identical for P and Phydro, but depending on whether in_netrad is used or not, 
    !     when implementing in_netrad condition, uncomment the lines marked by arrows
    if (.not. using_gs) then
      !---------------------------------------------------------
      ! SPLASH AET
      !---------------------------------------------------------
      ! When not using stomatal conductance, we use Priestly-Taylor formulation for the whole gridcell using all of incoming net radiation
      ! Eq. 81, SPLASH 2.0 Documentation
      tile_fluxes%canopy%daet = (24.0/pi) * (radians(sw * hi) + rx * rw * rv * (dgsin(hn) - dgsin(hi)) + &
        radians((rx * rw * ru - rx * tile_fluxes%canopy%rnl) * (hn - hi))) ! JAIDEEP FIXME: Technically correct, but for clarity, apply radians to just (hn-hi) ?
      tile_fluxes%canopy%daet_e = tile_fluxes%canopy%daet / energy_to_mm
    
      if (using_pml) then
        print*,'Warning: simulation parameter use_pml == .true. but not used in combination with SPLASH-AET (using_gs == .false.).'
      end if

    else
      !---------------------------------------------------------
      ! 2-source ET (soil and canopy) following Zhang et al., 2017 (doi:10.1002/2017JD027025)
      !---------------------------------------------------------
      ! total AET = canopy_AET + f * soil_AET_wet, where f = running_average(P/PET)
      !---------------------------------------------------------
      ! Potential soil evaporation
      !---------------------------------------------------------
      ! potential soil evaporation, not limited by history of P/PET (mm d-1)
      dpet_soil = (1.0 - fapar) * tile_fluxes%canopy%drn * tile_fluxes%canopy%econ * 1000.0  ! econ converts energy into mm evaporation

      !---------------------------------------------------------
      ! soil moisture limitation factor 
      !---------------------------------------------------------
      ! as a function of history of P/PET
      ! This corresponds to the calculation of f in Zhang et al., 2017 Eq. 9,
      ! but a continuous dampening (low pass filter, using dampen_variability()) is applied here instead of a running sum.
      p_memory   = dampen_variability(climate%dprec * secs_per_day, 30.0, p_memory )
      pet_memory = dampen_variability(dpet_soil, 30.0, pet_memory )

      if (pet_memory > 0.0) then
        p_over_pet_memory = p_memory / pet_memory  ! corresponds to f in Zhang et al., 2017 Eq. 9, (+ 1e-6) to avoid division by zero
        f_soil_aet = max(min(p_over_pet_memory, 1.0), 0.0)
      else
        f_soil_aet = 1.0
      end if

      ! print*,'water limitation on soil evaporation: ', f_soil_aet
      
      !---------------------------------------------------------
      ! Actual soil evaporation (mm d-1 and J d-1)
      !---------------------------------------------------------
      tile_fluxes%canopy%daet_soil = f_soil_aet * dpet_soil
      tile_fluxes%canopy%daet_e_soil = tile_fluxes%canopy%daet_soil / energy_to_mm

      !---------------------------------------------------------
      ! Actual canopy evaporation (mm d-1 and J d-1)
      !---------------------------------------------------------
      if (using_pml) then
        !---------------------------------------------------------
        ! Canopy transpiration using the Penman-Monteith equation
        !---------------------------------------------------------

        !---------------------------------------------------------
        ! Implementation of PML model (Zhang et al., 2017)
        !---------------------------------------------------------
        ! Aerodynamic conductance (m s-1)
        ga = calc_g_aero(myinterface%canopy_height, climate%dwind, myinterface%reference_height)

        ! variable substitute as used in Zhang et al. 2017 JGR    
        epsilon = sat_slope / gamma

        ! Convert stomatal conductance to CO2 [mol Pa-1 m-2 s-1] to 
        ! stomatal conductance to water [m s-1]
        ! Adopted from photosynth_phydro.mod.f90
        ! gs_accl was computed using soilmstress in ggp_pmodel.mod.f90
        ! print*,'in waterbal: gs_accl ', tile_fluxes%canopy%gs_accl
        gw = tile_fluxes%canopy%gs_accl * 1.6 * kR * (climate%dtemp + kTkelvin)
        
        ! latent energy flux from canopy (W m-2) 
        ! See also calc_transpiration_pm() in photosynth_phydro.mod.f90
        tile_fluxes%canopy%daet_e_canop = (epsilon * fapar * tile_fluxes%canopy%drn + (rho_water * cp / gamma) &
          * ga * climate%dvpd) / (epsilon + 1.0 + ga / gw) 

        ! print*,'-----------------------'
        ! print*,'canopy_height ', myinterface%canopy_height
        ! print*,'dwind ', climate%dwind
        ! print*,'reference_height ', myinterface%reference_height
        ! print*,'epsilon ', epsilon
        ! print*,'fapar   ', fapar
        ! print*,'net rad ', tile_fluxes%canopy%drn
        ! print*,'rho_watr', rho_water
        ! print*,'cp      ', cp
        ! print*,'gamma   ', gamma
        ! print*,'ga      ', ga
        ! print*,'vpd     ', climate%dvpd
        ! print*,'gw      ', gw
        ! print*,'-----------------------'

        ! ! W m-2 ---> mol m-2 s-1
        ! tile_fluxes%canopy%daet_canop = tile_fluxes%canopy%daet_e_canop &
        !   * (55.5 / par_env%lv)

        ! W m-2 ---> kg m-2 s-1
        ! XXX test: these units don't convert
        tile_fluxes%canopy%daet_canop = tile_fluxes%canopy%daet_e_canop * energy_to_mm

        ! print*,'PML: tile_fluxes%canopy%daet_canop, tile_fluxes%canopy%daet_soil ', tile_fluxes%canopy%daet_canop, tile_fluxes%canopy%daet_soil

      else
        !---------------------------------------------------------
        ! Canopy transpiration using the diffusion equation (mm d-1)
        !---------------------------------------------------------
        ! Transpiration via diffusion is calculated in gpp(). Take
        ! the canopy-level sum (weighted over PFTs by their fractional coverage)
        tile_fluxes%canopy%daet_canop = tile_fluxes%canopy%dtransp
        tile_fluxes%canopy%daet_e_canop = tile_fluxes%canopy%daet_canop / energy_to_mm   ! mm d-1 ---> J m-2 d-1 

        ! print*,'DIF: tile_fluxes%canopy%daet_canop, tile_fluxes%canopy%daet_soil ', tile_fluxes%canopy%daet_canop, tile_fluxes%canopy%daet_soil

      end if

      tile_fluxes%canopy%daet = tile_fluxes%canopy%daet_canop + tile_fluxes%canopy%daet_soil 
      tile_fluxes%canopy%daet_e = tile_fluxes%canopy%daet_e_canop + tile_fluxes%canopy%daet_e_soil

      ! print*,'waterbal: tile_fluxes%canopy%daet_canop, tile_fluxes%canopy%daet_soil ', tile_fluxes%canopy%daet_canop, tile_fluxes%canopy%daet_soil

    end if
    
    ! xxx debug
    ! if (splashtest) then
    !   print*,'slope of saturation, s', sat_slope
    !   print*,'enthalpy of vaporization: ', lv
    !   print*,'water density at 1 atm calculated: ', rho_water
    !   print*,'calculating psychrometric const. with patm: ', calc_patm(grid%elv)
    !   print*,'psychrometric constant: ', gamma
    !   print*,'daily condensation: ', tile_fluxes%canopy%dcn
    !   print*,'daily EET: ', tile_fluxes%canopy%deet
    !   print*,'daily PET: ', tile_fluxes%canopy%dpet
    !   print*,'variable substitute, rx: ', rx
    !   print*,'intersection hour angle, hi: ', hi
    !   print*,'daily AET set to: ', tile_fluxes%canopy%daet
    ! end if

    !---------------------------------------------------------
    ! Calculate Cramer-Prentice-Alpha, (unitless)
    !---------------------------------------------------------
    if (tile_fluxes%canopy%deet>0.0) then 
      tile_fluxes%canopy%cpa = tile_fluxes%canopy%daet / tile_fluxes%canopy%deet
    else
      tile_fluxes%canopy%cpa = 1.0 + kw
    end if

  end subroutine calc_et


  function calc_g_aero(h_canopy, v_wind, z_measurement) result(g_aero)
    !/////////////////////////////////////////////////////////////////////////
    ! Aerodynamic conductance [m s-1]
    ! Copied from photosynth_phydro.mod.f90
    ! To convert to mol m-2 s-1, see this: https://rdrr.io/cran/bigleaf/man/ms.to.mol.html (but not convincing)
    ! Refs: 
    !    Eq 13 in Leuning et al (2008). https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2007WR006562
    !    Eq 7 in Zhang et al (2008): https://agupubs.onlinelibrary.wiley.com/doi/10.1002/2017JD027025
    !    Box 4 in https://www.fao.org/3/x0490e/x0490e06.htm 
    !-------------------------------------------------------------------------
    real, intent(in) :: h_canopy        ! canopy height (m)
    real, intent(in) :: v_wind          ! wind speed (m s-1)
    real, intent(in) :: z_measurement   ! reference height (m)
    real :: k_karman, d, z_om, z_ov

    ! function return variable
    real :: g_aero             ! Aerodynamic conductance [m s-1]
    
    k_karman = 0.41            ! von Karman's constant [-]
    d = h_canopy * 2.0 / 3.0   ! zero-plane displacement height [m]
    z_om = 0.123 * h_canopy    ! roughness lengths governing transfer of water and momentum [m]
    z_ov = 0.1 * z_om
    
    g_aero = (k_karman * k_karman * v_wind) / (log((z_measurement - d) / z_om) * log((z_measurement - d) / z_ov))

  end function calc_g_aero

  
  function get_snow_rain( pr, sn, tc, snow ) result( out_snow_rain )
    !/////////////////////////////////////////////////////////////////////////
    ! Translates precipitation into change in snow depth and liquid water
    ! input to soil.
    !-------------------------------------------------------------------------
    ! arguments
    real, intent(in) :: pr     ! daily precip (mm), includes condensation
    real, intent(in) :: sn     ! daily precip as snow (mm water equivalent) 
    real, intent(in) :: tc     ! mean monthly temperature (deg C)
    real, intent(in) :: snow   ! snow depth, water equivalents (mm)

    ! function return variable
    type( outtype_snow_rain ) :: out_snow_rain

    ! local variables
    real :: fsnow                             ! fraction of precipitation as snow (temperature dependent)
    real :: melt                              ! snow melting rate (mm d-1)
    real, parameter :: temp_threshold = 1.0   ! deg C

    if ( snow > 0.0 .and. tc > temp_threshold ) then
      melt  = min( snow, maxmeltrate * ( tc - temp_threshold ) )
    else
      melt = 0.0
    end if 

    if (abs(sn - dummy) < eps) then
      fsnow = max( min( 1.0,  1.0 - ( 1.0 / 2.0 ) * tc ), 0.0 )
      out_snow_rain%snow_updated   = snow + fsnow * pr - melt
      out_snow_rain%liquid_to_soil = pr * ( 1.0 - fsnow ) + melt
    else
      out_snow_rain%snow_updated   = snow + sn - melt
      out_snow_rain%liquid_to_soil = pr + melt
    end if

  end function get_snow_rain


  function calc_dr( nu ) result( dr )
    !---------------------------------------------------------
    ! Calculates distance factor (dr), unitless
    !---------------------------------------------------------
    ! arguments
    real, intent(in) :: nu

    ! local variables
    real :: rho

    ! function return variable
    real :: dr

    ! Berger et al. (1993)
    rho = (1.0 - ke**2)/(1.0 + ke * dgcos( nu ))        
    dr = (1.0/rho)**2

  end function calc_dr


  function calc_decl_angle( lambda ) result( delta )
    !---------------------------------------------------------
    ! Calculates declination angle (delta), degrees
    !---------------------------------------------------------
    ! arguments
    real, intent(in) :: lambda

    ! function return variable
    real :: delta

    ! Woolf (1968)
    delta = asin( dgsin( lambda ) * dgsin( keps ) )   ! xxx use asin with single-precision compilation
    delta = degrees( delta )

  end function calc_decl_angle


  subroutine calc_ru_rv( delta, lat )
    !---------------------------------------------------------
    ! Calculates variable substitutes (u and v), unitless
    !---------------------------------------------------------
    ! arguments
    real, intent(in) :: delta
    real, intent(in) :: lat

    ru = dgsin(delta) * dgsin(lat)
    rv = dgcos(delta) * dgcos(lat)

  end subroutine calc_ru_rv


  function calc_hs( ru, rv ) result( hs )
    !---------------------------------------------------------
    ! Calculates the sunset hour angle (hs), degrees
    !---------------------------------------------------------
    ! arguments
    real, intent(in) :: ru, rv

    ! function return variable
    real :: hs

    ! Note: u/v == tan(delta)*tan(lat)
    ! Eq. 3.22, Stine & Geyer (2001)
    if ((ru/rv) >= 1.0) then
      ! Polar day (no sunset)
      hs = 180.0 
    elseif ((ru/rv) <= -1.0) then
      ! Polar night (no sunrise)
      hs = 0.0
    else
      hs = degrees(acos(-1.0*ru/rv))
    end if

  end function calc_hs


  function calc_tau( sf, elv ) result( tau )
    !---------------------------------------------------------
    ! Calculates transmittivity (tau), unitless
    !---------------------------------------------------------
    ! arguments
    real, intent(in) :: sf     ! sunshine fraction
    real, intent(in) :: elv    ! elevation

    ! local variables
    real :: tau_o

    ! function return variable
    real :: tau

    ! Eq. 11, Linacre (1968)
    tau_o = (kc + kd*sf)

    ! Eq. 2, Allen (1996)
    tau = tau_o*(1.0 + (2.67e-5)*elv)

  end function calc_tau


  subroutine getpar_modl_waterbal()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads waterbalance module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    ! constant for dRnl (Monteith & Unsworth, 1990)
    kA       = 107.0
    
    ! shortwave albedo (Federer, 1968)
    kalb_sw  = 0.17
    
    ! visible light albedo (Sellers, 1985)
    kalb_vis = 0.03
    
    ! constant for dRnl (Linacre, 1968)
    kb       = 0.2
    
    ! cloudy transmittivity (Linacre, 1968)
    kc       = 0.25
    
    ! supply constant, mm/hr (Federer, 1982)
    kCw      = 1.05 
    
    ! angular coefficient of transmittivity (Linacre, 1968)
    kd       = 0.5
    
    ! eccentricity for 2000 CE (Berger, 1978)
    ke       = 0.0167
    
    ! obliquity for 2000 CE, degrees (Berger, 1978)
    keps     = 23.44

    ! ! solar constant, W/m^2 (Kopp & Lean, 2011)
    ! kGsc     = 1360.8
    
    ! entrainment factor (Lhomme, 1997; Priestley & Taylor, 1972)
    kw       = 0.26
    
    ! longitude of perihelion for 2000 CE, degrees (Berger, 1978)
    komega   = 283.0

    ! maximum snow melting rate (mm d-1) (Orth et al., 2013)
    maxmeltrate = 3.0

  end subroutine getpar_modl_waterbal

  ! xxx put these functions into a 'contain' within calling SR?


  subroutine get_berger_tls( day, grid )
    !----------------------------------------------------------------   
    ! Returns true anomaly and true longitude for a given day
    ! Reference: Berger, A. L. (1978), Long term variations of daily 
    ! insolation and quaternary climatic changes, J. Atmos. Sci., 35, 
    ! 2362-2367.
    !----------------------------------------------------------------   
    ! arguments
    integer, intent(in)           :: day   ! day of the year
    type(gridtype), intent(inout) :: grid

    ! ! function return value
    ! type(outtype_berger) :: out_berger  ! stores output of function berger_tls

    ! local variables
    real :: anm, ranm, anv, ranv
    real :: dlamm                ! Mean longitude for day of year
    real :: xee, xec, xse        ! variable substitutes
    real :: xlam                 ! Mean longitude for vernal equinox
    real :: tmp1, tmp2, tmp3     ! variable substitutes

    ! Variable substitutes:
    xee = ke**2 
    xec = ke**3
    xse = sqrt(1.0 - xee)

    ! Mean longitude for vernal equinox:
    tmp1 = (ke/2.0 + xec/8.0)*(1.0 + xse)*dgsin(komega)
    tmp2 = xee/4.0*(0.5 + xse)*dgsin(2.0*komega)
    tmp3 = xec/8.0*(1.0/3.0 + xse)*dgsin(3.0*komega)
    xlam = tmp1 - tmp2 + tmp3
    xlam = degrees(2.0*xlam)

    ! Mean longitude for day of year:
    dlamm = xlam + (day - 80.0)*(360.0/ndayyear)

    ! Mean anomaly:
    anm = dlamm - komega
    ranm = radians(anm)

    ! True anomaly:
    ranv = (ranm + (2.0*ke - xec/4.0)*sin(ranm) + 5.0/4.0*xee*sin(2.0*ranm) + 13.0/12.0*xec*sin(3.0*ranm))  ! xxx use sin with single-precision compilation
    anv = degrees(ranv)

    ! True longitude:
    grid%lambda = anv + komega
    if (grid%lambda < 0.0) then
        grid%lambda = grid%lambda + 360.0
    else if (grid%lambda > 360.0) then
        grid%lambda = grid%lambda - 360.0
    endif

    ! True anomaly:
    grid%nu = (grid%lambda - komega)
    if (grid%nu < 0.0) then
        grid%nu = grid%nu + 360.0
    endif

  end subroutine get_berger_tls


  function calc_sat_slope( tc ) result( sat_slope )
    !----------------------------------------------------------------   
    ! Calculates the slope of the sat pressure temp curve, Pa/K
    ! Ref:      Eq. 13, Allen et al. (1998)
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: tc ! air temperature, degrees C

    ! function return value
    real :: sat_slope  ! slope of the sat pressure temp curve, Pa/K

    sat_slope = (17.269)*(237.3)*(610.78)*(exp(tc*17.269/(tc + 237.3))/((tc + 237.3)**2))

  end function calc_sat_slope


  function calc_enthalpy_vap( tc ) result( enthalpy_vap )
    !----------------------------------------------------------------   
    ! Calculates the enthalpy of vaporization, J/kg
    ! Ref:      Eq. 8, Henderson-Sellers (1984)
    !----------------------------------------------------------------   

    ! arguments
    real, intent(in) :: tc ! air temperature, degrees C

    ! function return value
    real ::  enthalpy_vap ! enthalpy of vaporization, J/kg

    enthalpy_vap = 1.91846e6*((tc + 273.15)/(tc + 273.15 - 33.91))**2

  end function calc_enthalpy_vap


  function density_h2o( tc, press )
    !----------------------------------------------------------------   
    ! Calculates density of water at a given temperature and pressure
    ! Ref: Chen et al. (1977)
    !----------------------------------------------------------------   

    ! arguments
    real, intent(in) :: tc     ! air temperature (degrees C)
    real, intent(in) :: press  ! atmospheric pressure (Pa)

    ! local variables
    real :: po, ko, ca, cb
    real :: pbar               ! atmospheric pressure (bar)

    ! function return value
    real :: density_h2o  ! density of water, kg/m^3

    ! Calculate density at 1 atm:
    po = (&
             0.99983952&
             + 6.788260e-5  *tc&
             - 9.08659e-6   *tc*tc&
             + 1.022130e-7  *tc*tc*tc  &
             - 1.35439e-9   *tc*tc*tc*tc&
             + 1.471150e-11 *tc*tc*tc*tc*tc&
             - 1.11663e-13  *tc*tc*tc*tc*tc*tc&
             + 5.044070e-16 *tc*tc*tc*tc*tc*tc*tc&
             - 1.00659e-18  *tc*tc*tc*tc*tc*tc*tc*tc&
         )

    ! Calculate bulk modulus at 1 atm:
    ko = (&
             19652.17&
             + 148.1830   *tc&
             - 2.29995    *tc*tc&
             + 0.01281    *tc*tc*tc&
             - 4.91564e-5 *tc*tc*tc*tc&
             + 1.035530e-7*tc*tc*tc*tc*tc&
         )

    ! Calculate temperature dependent coefficients:
    ca = (&
             3.26138&
             + 5.223e-4  *tc&
             + 1.324e-4  *tc*tc&
             - 7.655e-7  *tc*tc*tc&
             + 8.584e-10 *tc*tc*tc*tc&
         )
    cb = (&
             7.2061e-5&
             - 5.8948e-6  *tc&
             + 8.69900e-8 *tc*tc&
             - 1.0100e-9  *tc*tc*tc&
             + 4.3220e-12 *tc*tc*tc*tc&
         )

    ! Convert atmospheric pressure to bar (1 bar = 100000 Pa)
    pbar = (1.0e-5)*press

    density_h2o = 1000.0*po*(ko + ca*pbar + cb*pbar**2.0) &
      /(ko + ca*pbar + cb*pbar**2.0 - pbar)

  end function density_h2o


  function calc_cp_moist_air(tc) result(cp)
    !----------------------------------------------------------------   
    ! Calculate the specific heat capacity of moist air, J kg-1 K-1
    ! Eq. 47, Tsilingiris (2008)
    !----------------------------------------------------------------   
    real, intent(in) :: tc     ! temperature (deg C)
    real :: my_tc

    ! function return variable
    real :: cp
  
    my_tc = max(min(tc, 100.0), 0.0)
    
    cp = 1.0e3*(&
               1.0045714270&
             + 2.050632750e-3  *my_tc&
             - 1.631537093e-4  *my_tc*my_tc&
             + 6.212300300e-6  *my_tc*my_tc*my_tc&
             - 8.830478888e-8  *my_tc*my_tc*my_tc*my_tc&
             + 5.071307038e-10 *my_tc*my_tc*my_tc*my_tc*my_tc&
            )
       
  end function calc_cp_moist_air


  function psychro( tc, patm )
    !----------------------------------------------------------------   
    ! Calculates the psychrometric constant for a given temperature and pressure
    ! Ref: Allen et al. (1998); Tsilingiris (2008) 
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: tc    ! air temperature, degrees C
    real, intent(in) :: patm  ! atmospheric pressure, Pa

    ! local variables
    real :: lv  ! latent heat of vaporization (J/kg)
    real :: cp  ! specific heat capacity of moist air

    ! function return value
    real :: psychro  ! psychrometric constant, Pa/K

    ! local variables
    real :: my_tc    ! adjusted temperature to avoid numerical blow-up 

    ! Calculate the specific heat capacity of moist air (J/kg/K)
    cp = calc_cp_moist_air(tc)

    ! Calculate latent heat of vaporization, J/kg
    lv = calc_enthalpy_vap(tc)

    ! Calculate psychrometric constant, Pa/K
    ! Eq. 8, Allen et al. (1998)
    psychro = cp * kMa * patm / (kMv * lv)

  end function psychro

end module md_waterbal
