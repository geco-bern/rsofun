module md_waterbal
  !////////////////////////////////////////////////////////////////
  ! SPLASH WATERBALANCE MODULE
  ! Contains the "main" subroutine 'waterbal' and all necessary 
  ! subroutines for handling input/output. 
  ! Every module that implements 'waterbal' must contain this list 
  ! of subroutines (names that way).
  !   - getpar_modl_waterbal
  !   - initio_waterbal
  !   - initoutput_waterbal
  !   - getout_daily_waterbal
  !   - getout_monthly_waterbal
  !   - writeout_ascii_waterbal
  !   - waterbal
  ! Required module-independent model state variables (necessarily 
  ! updated by 'waterbal') are:
  !   - daytime net radiation ('rn')
  !   - soil water conent ('psoilphys%wcont')
  !   - runoff ('soilphys%dro')
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  ! ...
  !----------------------------------------------------------------
  use md_params_core, only: ndayyear, nmonth, nlu, maxgrid, kTo, kR, &
    kMv, kMa, kfFEC, secs_per_day, dummy

  implicit none

  private
  public solartype, evap, waterbal, get_solar, getpar_modl_waterbal, get_rlm_waterbal,  &
    init_rlm_waterbal, getrlm_daily_waterbal

  !----------------------------------------------------------------
  ! Public, module-specific state variables
  !----------------------------------------------------------------
  ! Collection of solar radiation-related variables used across modules
  ! Variables are a function of latitude, elevation, and 
  ! sunshine fraction (all variables independent of soil moisture)
  type solartype
    real, dimension(ndayyear) :: dayl        ! day length (hours)
    real, dimension(ndayyear) :: dra         ! daily TOA solar irradiation (J/m2)
    real, dimension(ndayyear) :: dppfd       ! daily total PPFD (mol m-2 d-1)
    real, dimension(nmonth)   :: mppfd       ! monthly total PPFD (mol m-2 month-1)
    real, dimension(nmonth)   :: meanmppfd   ! monthly mean PPFD, averaged over daylight seconds (mol m-2 s-1)
  end type solartype

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
  real :: kGsc              ! solar constant, W/m^2 (Kopp & Lean, 2011)
  real :: kw                ! entrainment factor (Lhomme, 1997; Priestley & Taylor, 1972)
  real :: komega            ! longitude of perihelion for 2000 CE, degrees (Berger, 1978)
  real :: vpdstress_par_a   ! Parameter for Oren et al.-VPD stress function (see calc_vpdstress())
  real :: vpdstress_par_b   ! Parameter for Oren et al.-VPD stress function (see calc_vpdstress())
  real :: vpdstress_par_m   ! Parameter for Oren et al.-VPD stress function (see calc_vpdstress())

  !----------------------------------------------------------------
  ! MODULE-SPECIFIC, PRIVATE VARIABLES
  !----------------------------------------------------------------
  ! Orbit parameters 
  type outtype_berger
    real :: nu
    real :: lambda
  end type outtype_berger

  type( outtype_berger ), dimension(ndayyear) :: out_berger    ! stores output of function berger_tls

  ! Radiation variables. aet, sw, and cpa are affected by soil moisture.
  type evaptype
    real :: rn         ! daily net radiation (J/m2/d)
    real :: rnn        ! nighttime net radiation (J/m^2/d)
    real :: rnl        ! net longwave radiation (W/m^2)
    real :: eet        ! daily EET (mm d-1)
    real :: pet        ! daily PET (mm d-1)
    real :: pet_e      ! daily PET (J m-2 d-1)
    real :: cn         ! daily condensation (mm d-1)
    real :: aet        ! daily AET (mm d-1)
    real :: aet_e      ! daily AET (J m-2 d-1)
    real :: cpa        ! Cramer-Prentice-Alpha = AET / EET (unitless)
    real :: econ       ! water-to-energy conversion factor (econ), m^3/J
  end type evaptype

  ! SPLASH state variables
  type( evaptype ) , dimension(nlu) :: evap

  ! holds return variables of function get_snow_rain()
  type outtype_snow_rain
    real :: snow_updated     ! snow depth in water equivalents (mm)
    real :: liquid_to_soil   ! water 
  end type outtype_snow_rain

  !----------------------------------------------------------------
  ! MODULE-SPECIFIC, KNOWN PARAMETERS
  !----------------------------------------------------------------
  logical :: outenergy = .true.

  !----------------------------------------------------------------
  ! Module-specific rolling mean variables
  !----------------------------------------------------------------
  ! real, allocatable, dimension(:,:), save :: rlmalpha       ! rolling mean of annual mean alpha (AET/PET)
  integer, parameter :: nyrs_rlmalpha = 5                   ! number of years for rolling mean (=width of sliding window)

  !----------------------------------------------------------------
  ! Module-specific variables for rolling annual mean calculations
  !----------------------------------------------------------------
  real, allocatable, dimension(:,:)   :: rlmalpha

  character(len=7) :: in_ppfd       ! information whether PPFD is prescribed from meteo file for global attribute in NetCDF file

contains

  subroutine waterbal( soil, tile_fluxes, plant_fluxes, doy, jpngr, lat, elv, pr, sn, tc, sf, netrad, vpd, fapar )
    !/////////////////////////////////////////////////////////////////////////
    ! Calculates daily and monthly quantities for one year
    !-------------------------------------------------------------------------
    use md_params_core, only: ndayyear, ndaymonth, npft
    use md_tile, only: soil_type, tile_fluxes_type
    use md_plant, only: plant_fluxes_type
    use md_interface, only: myinterface

    ! arguments
    type( soil_type ), dimension(nlu), intent(inout)        :: soil
    type( tile_fluxes_type ), dimension(nlu), intent(inout) :: tile_fluxes
    type(plant_fluxes_type), dimension(npft), intent(inout) :: plant_fluxes  ! derived type containing plant-related flux variables  (overwritten at each time step)
    integer, intent(in) :: doy    ! day of year
    integer, intent(in) :: jpngr  ! gridcell number
    real, intent(in)    :: lat    ! latitude (degrees)
    real, intent(in)    :: elv    ! altitude (m)
    real, intent(in)    :: pr     ! daily precip as rain (liquid) (mm) 
    real, intent(in)    :: sn     ! daily precip as snow (mm water equivalent) 
    real, intent(in)    :: tc     ! mean monthly temperature (deg C)
    real, intent(in)    :: sf     ! mean monthly sunshine fraction (unitless)
    real, intent(in)    :: netrad ! net radiation (W m-2), may be dummy (in which case this is not used)
    real, intent(in)    :: vpd    ! vapour pressure deficit (Pa)
    real, intent(in)    :: fapar  ! fraction of absorbed photosynthetically active radiation (unitless)

    ! local variables
    real :: wcont_prev                   ! soil moisture (water content) before being updated (mm)
    real :: wbal                         ! daily water balance (mm), temporary variable

    integer :: lu                        ! land unit (gridcell tile)
    integer :: moy                       ! month of year
    integer :: idx                       ! day of year corresponding to yesterday
    integer :: dm                        ! day of month

    type( outtype_snow_rain )   :: out_snow_rain

    ! for the case netrad is prescribed following SWBM
    real, parameter :: beta = 0.66
    real, parameter :: exp_et = 0.06


    ! Loop over gricell tiles
    do lu=1,nlu

      ! Calculate evaporative supply rate, mm/h
      tile_fluxes(lu)%sw = kCw * soil(lu)%phy%wcont / soil(lu)%params%whc

      ! Calculate radiation and evaporation quantities
      ! print*,'calling evap with arguments ', lat, doy, elv, sf, tc, tile_fluxes(lu)%sw
      evap(lu) = get_evap( lat, doy, elv, sf, tc, tile_fluxes(lu)%sw, netrad )

      if (myinterface%params_siml%calc_aet_fapar_vpd) then
        ! Overwrite AET calculated by get_evap(), instead using vdpstress * fapar * pet
        evap(lu)%aet = calc_vdpstress( vpd ) * fapar * evap(lu)%pet
        evap(lu)%aet_e = evap(lu)%aet / (evap(lu)%econ * 1000.0)
      end if

      ! take acual evaporation
      if (npft>1) stop 'waterbal_splash: Think of soething when npft > 1.'
      plant_fluxes(:)%dlatenth = evap(lu)%aet_e

      if (netrad/=dummy) then
        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 21. Estimate daily AET (out_evap%aet), mm d-1
        ! WARNING: This follows SWBM not SPLASH
        ! Needs to be done here because wcont is not available in get_evap()
        ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! calculate actual ET from potential ET and Eq. 2 in Orth et al., 2013, limited to <=1
        evap(lu)%aet = evap(lu)%pet * beta * min( 1.0, ( soil(lu)%phy%wcont / soil(lu)%params%whc )**exp_et )
        ! print*,'aet: ', evap(lu)%aet

        evap(lu)%cpa = max( 0.0, evap(lu)%aet / evap(lu)%eet )

      end if

      ! Update soil moisture and snow pack
      out_snow_rain = get_snow_rain( pr + evap(lu)%cn, sn, tc, soil(lu)%phy%snow )
      soil(lu)%phy%snow = out_snow_rain%snow_updated 

      ! Update soil moisture
      soil(lu)%phy%wcont = soil(lu)%phy%wcont + out_snow_rain%liquid_to_soil - evap(lu)%aet

      ! Bucket model for runoff generation
      ! print*,'3'
      if (soil(lu)%phy%wcont>soil(lu)%params%whc) then
        ! -----------------------------------
        ! Bucket is full 
        ! -----------------------------------
        ! * determine NO3 leaching fraction 
        tile_fluxes(lu)%dfleach = 1.0 - soil(lu)%params%whc / soil(lu)%phy%wcont

        ! * add remaining water to monthly runoff total
        tile_fluxes(lu)%dro = soil(lu)%phy%wcont - soil(lu)%params%whc

        ! * set soil moisture to capacity
        soil(lu)%phy%wcont = soil(lu)%params%whc

      elseif (soil(lu)%phy%wcont<0.0) then
        ! -----------------------------------
        ! Bucket is empty
        ! -----------------------------------
        ! * set soil moisture to zero
        evap(lu)%aet            = evap(lu)%aet + soil(lu)%phy%wcont
        soil(lu)%phy%wcont      = 0.0
        tile_fluxes(lu)%dro     = 0.0
        tile_fluxes(lu)%dfleach = 0.0

      else
        ! No runoff occurrs
        tile_fluxes(lu)%dro     = 0.0
        tile_fluxes(lu)%dfleach = 0.0

      end if

      ! water-filled pore space
      soil(lu)%phy%wscal = soil(lu)%phy%wcont / soil(lu)%params%whc

      ! save daily water balance for output
      tile_fluxes(lu)%dwbal = out_snow_rain%liquid_to_soil

    end do

  end subroutine waterbal


  function get_solar( lat, elv, sf, ppfd ) result( out_solar )
    !/////////////////////////////////////////////////////////////////////////
    ! This subroutine calculates daily PPFD. Code is an extract of the subroutine
    ! 'evap', adopted from the evap() function in GePiSaT (Python version). 
    ! This subroutine ('get_solar') is called before the daily loop.
    ! Output:
    ! - daily extraterrestrial solar radiation (dra), J/m^2
    ! - daily PPFD (dppfd), mol/m^2
    !-------------------------------------------------------------------------  
    use md_params_core, only: ndayyear, pi, dummy
    use md_sofunutils, only: daily2monthly

    ! arguments
    real, intent(in)                      :: lat           ! latitude, degrees
    real, intent(in)                      :: elv           ! elevation, metres
    real, intent(in), dimension(ndayyear) :: sf            ! fraction of sunshine hours 
    real, intent(in), dimension(ndayyear) :: ppfd          ! photon flux density (mol m-2 d-1), may be dummy (in which case this is not used)

    ! function return variable
    type( solartype ) :: out_solar

    ! local variables
    integer            :: doy
    real               :: dr                           ! distance factor
    real               :: delta                        ! declination angle 
    real               :: ru                           ! variable substitute for u
    real               :: rv                           ! variable substitute for v
    real               :: hs                           ! sunset hour angle
    real               :: tau                          ! transmittivity (unitless)
    real               :: rw                           ! variable substitute (W/m^2)
    real, dimension(2) :: out_ru_rv      ! function return variable containing 'ru' and 'rv'.

    real, dimension(ndayyear) :: daysecs ! daylight seconds for each DOY
    real, dimension(nmonth)   :: monsecs ! daylight seconds for each MOY

    ! initialise members of solartype
    out_solar%dayl(:)      = 0.0
    out_solar%dra(:)       = 0.0
    out_solar%dppfd(:)     = 0.0
    out_solar%mppfd(:)     = 0.0
    out_solar%meanmppfd(:) = 0.0

    do doy=1,ndayyear
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 2. Calculate heliocentric longitudes (nu and lambda), degrees
      ! Store daily return values for later use in subroutine 'evap'.
      ! Other variables defined and over-written below may be stored
      ! for later use in 'evap' the same way. However function 
      ! 'out_berger' is by far the most expensive one. This is there-
      ! fore a compromise.
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Berger (1978)
      out_berger(doy) = get_berger_tls( doy )

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 3. Calculate distance factor (dr), unitless
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      dr = calc_dr( out_berger(doy)%nu )

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 4. Calculate declination angle (delta), degrees
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      delta = calc_delta( out_berger(doy)%lambda )

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 5. Calculate variable substitutes (u and v), unitless
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      out_ru_rv = calc_ru_rv( delta, lat )
      ru = out_ru_rv(1)
      rv = out_ru_rv(2)

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 6. Calculate the sunset hour angle (hs), degrees
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      hs = calc_hs( ru, rv )

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 6.a Calculate day length from sunset hour angle, h
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      out_solar%dayl(doy) = 24.0 * hs / 180.0  ! hs is in degrees (pi = 180 deg)

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 7. Calculate daily extraterrestrial solar radiation (dra), J/m^2/d
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Eq. 1.10.3, Duffy & Beckman (1993)
      out_solar%dra(doy) = ( secs_per_day / pi ) * kGsc * dr * ( radians(ru) * hs + rv * dgsin(hs) )

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 8. Calculate transmittivity (tau), unitless
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tau = calc_tau( sf(doy), elv )

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 9. Calculate daily PPFD (dppfd), mol/m^2
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (ppfd(1)/=dummy) then
        out_solar%dppfd(doy) = ppfd(doy)
        in_ppfd = ".true. "
      else
        ! Eq. 57, SPLASH 2.0 Documentation
        out_solar%dppfd(doy) = (1.0e-6) * kfFEC * ( 1.0 - kalb_vis ) * tau * out_solar%dra(doy)
        in_ppfd = ".false."
      end if

    end do

    ! Calculate monthly average daylight PPFD 
    daysecs(:)         = out_solar%dayl(:) * 60.0 * 60.0              ! conversion of daylight hours to seconds
    monsecs(:)         = daily2monthly( daysecs(:), "sum" )
    out_solar%mppfd(:) = daily2monthly( out_solar%dppfd(:), "sum" )   ! mol m-2 month-1

    ! In polar regions, 'monsecs' an be zero in winter months. PPFD is zero then too.
    where ( monsecs(:) > 0.0 )
      out_solar%meanmppfd(:) = out_solar%mppfd(:) / monsecs(:) ! mol m-2 s-1
    else where
      out_solar%meanmppfd(:) = 0.0
    end where

    !-------------------------------------------------------------   
    ! Refs: Allen, R.G. (1996), Assessing integrity of weather data for 
    !         reference evapotranspiration estimation, Journal of Irrigation
    !         and Drainage Engineering, vol. 122, pp. 97--106.
    !       Allen, R.G., L.S. Pereira, D. Raes, M. Smith (1998), 
    !         'Meteorological data,' Crop evapotranspiration - Guidelines for 
    !         computing crop water requirements - FAO Irrigation and drainage 
    !         paper 56, Food and Agriculture Organization of the United 
    !         Nations, online: http://www.fao.org/docrep/x0490e/x0490e07.htm
    !       Berger, A.L. (1978), Long-term variations of daily insolation and 
    !         quarternary climatic changes, Journal of Atmospheric Sciences, 
    !         vol. 35, pp. 2362--2367.
    !       Berger, A.L., M.F. Loutre, and C. Tricot (1993), Insolation and 
    !         Earth's orbital periods, J. Geophys. Res., 98, 10341--10362.
    !       Duffie, J. A. and W. A. Beckman (1991). Solar engineering of 
    !         thermal processes. 4th ed. New Jersey: John Wiley and Sons
    !       Federer (1982), Transpirational supply and demand: plant, soil, 
    !         and atmospheric effects evaluated by simulation, Water 
    !         Resources Research, vol. 18, no. 2, pp. 355--362.
    !       Ge, S., R.G. Smith, C.P. Jacovides, M.G. Kramer, R.I. Carruthers 
    !         (2011), Dynamics of photosynthetic photon flux density (PPFD) 
    !         and estimates in coastal northern California, Theoretical and 
    !         Applied Climatology, vol. 105, pp. 107--118.
    !       Henderson-Sellers, B. (1984), A new formula for latent heat of 
    !         vaporization of water as a function of temperature, Quarterly 
    !         Journal of the Royal Meteorological Society 110, pp. 1186–1190
    !       Linacre (1968), Estimating the net-radiation flux, Agricultural 
    !         Meteorology, vol. 5, pp. 49--63.
    !       Prentice, I.C., M.T. Sykes, W. Cramer (1993), A simulation model 
    !         for the transient effects of climate change on forest 
    !         landscapes, Ecological Modelling, vol. 65, pp. 51--70.
    !       Priestley, C.H.B. and R.J. Taylor (1972), On the assessment of 
    !         surface heat flux and evaporation using large-scale parameters, 
    !         Monthly Weather Review, vol. 100 (2), pp. 81--92.
    !       Spencer, J. W. (1971), Fourier series representation of the 
    !         position of the sun, Search, vol. 2, p. 172.
    !       Stine, W. B. and M. Geyer (2001). “Power from the Sun”. 
    !         online: http://www.powerfromthesun.net/Book/chapter03/chapter03
    !       Wetherald, R.T., S. Manabe (1972), Response to joint ocean-
    !         atmosphere model to the seasonal variation of the solar 
    !         radiation, Monthly Weather Review, vol. 100 (1), pp. 42--59.
    !       Woolf, H. M. (1968). On the computation of solar evaluation 
    !         angles and the determination of sunrise and sunset times. 
    !         Tech. rep. NASA-TM-X-164. National Aeronautics and Space 
    !         Administration (NASA).
    !-------------------------------------------------------------   
  end function get_solar


  function get_evap( lat, doy, elv, sf, tc, sw, netrad ) result( out_evap )
    !/////////////////////////////////////////////////////////////////////////
    ! This subroutine calculates daily evaporation quantities. Code is 
    ! adopted from the evap() function in GePiSaT (Python version). 
    ! This subroutine ('evap') is called within the daily loop.
    ! Output:
    ! - daily net longwave radiation (out_evap%drnl), W/m^2
    ! - daily daytime net radiation (out_evap%drn), J/m^2
    ! - daily nighttime net radiation (out_evap%rnn), J/m^2
    ! - daily EET (out_evap%eet), mm
    ! - daily PET (out_evap%pet), mm
    ! - daily AET (out_evap%aet), mm
    ! - daily condensation (out_evap%cn), mm
    !-------------------------------------------------------------------------  
    use md_params_core, only: ndayyear, pi, dummy
    use md_sofunutils, only: calc_patm

    ! arguments
    real,    intent(in) :: lat           ! latitude, degrees
    integer, intent(in) :: doy           ! day of the year (formerly 'n')
    real,    intent(in) :: elv           ! elevation, metres
    real,    intent(in) :: sf            ! fraction of sunshine hours
    real,    intent(in) :: tc            ! mean daily air temperature, C
    real,    intent(in) :: sw            ! evaporative supply rate, mm/hr
    real,    intent(in) :: netrad        ! net radiation, integrated over day (J m-2 d-1)

    ! function return variable
    type( evaptype )  :: out_evap

    ! local variables
    real :: dr                           ! distance factor
    real :: delta                        ! declination angle 
    real :: ru                           ! variable substitute for u
    real :: rv                           ! variable substitute for v
    real :: hs                           ! sunset hour angle
    real :: tau                          ! transmittivity (unitless)
    real :: rw                           ! variable substitute (W/m^2)
    real :: hn                           ! net radiation cross-over hour angle
    real :: sat_slope                    ! slope of saturation vap press temp curve, Pa/K
    real :: pw                           ! density of water, kg/m^3
    real :: lv                           ! enthalpy of vaporization, J/kg
    real :: gamma                        ! psychrometric constant, Pa/K
    real :: econ                         ! Eq. 58, SPLASH 2.0 Documentation
    real :: rx                           ! variable substitute (mm/hr)/(W/m^2)
    real :: hi, cos_hi                   ! intersection hour angle, degrees
    real, dimension(2) :: out_ru_rv      ! function return variable containing 'ru' and 'rv'.

    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Calculate water-to-energy conversion (econ), m^3/J
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Slope of saturation vap press temp curve, Pa/K
    sat_slope = calc_sat_slope(tc)
    
    ! Enthalpy of vaporization, J/kg
    lv = calc_enthalpy_vap(tc)
    
    ! Density of water, kg/m^3
    pw = density_h2o(tc, calc_patm(elv))
    
    ! Psychrometric constant, Pa/K
    gamma = psychro(tc, calc_patm(elv))
    
    ! Eq. 51, SPLASH 2.0 Documentation
    ! out_evap%econ = 1.0 / ( lv * pw ) ! this is to convert energy into mass (water)
    out_evap%econ = sat_slope / (lv * pw * (sat_slope + gamma)) ! MORE PRECISELY - this is to convert energy into mass (water)

    if (netrad/=dummy) then
      !--------------------------------------------------
      ! MODE 1: Net radiation is prescribed
      ! ==> get condensation from negative net radiation
      !--------------------------------------------------

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 13./14. Note: this corresponds to daytime plus nighttime net radiation (out_evap%rn), J/m^2
      ! ==> coppy prescribed value to state variable net radiation, 'rn'
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      out_evap%rn = netrad      

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 17. Estimate daily EET (out_evap%eet), mm d-1
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Eq. 70, SPLASH 2.0 Documentation
      out_evap%eet = 1000.0 * out_evap%econ * out_evap%rn

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 18. Estimate daily PET (out_evap%pet), mm d-1
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Eq. 72, SPLASH 2.0 Documentation
      out_evap%pet = ( 1.0 + kw ) * out_evap%eet
      ! print*,'pet: ', out_evap%pet

      ! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! ! 16.A. Calculate daily condensation (out_evap%cn), mm d-1
      ! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! if (out_evap%pet < 0.0) then
      !   out_evap%cn = -1.0 * beta * out_evap%pet
      ! else
      !   out_evap%cn = 0.0
      ! end if

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 16.A. Calculate daily condensation in case net radiation is negative (out_evap%cn), mm d-1
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (out_evap%rn < 0.0) then
        out_evap%cn = 1000.0 * out_evap%econ * abs(out_evap%rn)
      else
        out_evap%cn = 0.0
      end if

    else
      !--------------------------------------------------
      ! MODE 2: Net radiation is calculated online
      !--------------------------------------------------

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 3. Calculate distance factor (dr), unitless
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      dr = calc_dr( out_berger(doy)%nu )
      
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 4. Calculate declination angle (delta), degrees
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      delta = calc_delta( out_berger(doy)%lambda )

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 5. Calculate variable substitutes (u and v), unitless
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      out_ru_rv = calc_ru_rv( delta, lat )
      ru = out_ru_rv(1)
      rv = out_ru_rv(2)

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 6. Calculate the sunset hour angle (hs), degrees
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      hs = calc_hs( ru, rv )
      
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 8. Calculate transmittivity (tau), unitless
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tau = calc_tau( sf, elv )

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 10. Estimate net longwave radiation (out_evap%rnl), W/m^2
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Eq. 11, Prentice et al. (1993); Eq. 5 and 6, Linacre (1968)
      out_evap%rnl = ( kb + (1.0 - kb ) * sf ) * ( kA - tc )
    
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 11. Calculate variable substitute (rw), W/m^2
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      rw = ( 1.0 - kalb_sw ) * tau * kGsc * dr

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 12. Calculate net radiation cross-over hour angle (hn), degrees
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if ((out_evap%rnl - rw*ru)/(rw*rv) >= 1.0) then
        ! Net radiation negative all day
        hn = 0.0
      else if ((out_evap%rnl - rw*ru)/(rw*rv) <= -1.0) then
        ! Net radiation positive all day
        hn = 180.0
      else
        !hn = degrees( dacos((out_evap%rnl - rw*ru)/(rw*rv)) )
        hn = degrees( acos((out_evap%rnl - rw*ru)/(rw*rv)) )   ! use acos with single precision compilation
      end if

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 13. Calculate daytime net radiation (out_evap%rn), J/m^2
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Eq. 53, SPLASH 2.0 Documentation
      out_evap%rn = (secs_per_day/pi) * (hn*(pi/180.0)*(rw*ru - out_evap%rnl) + rw*rv*dgsin(hn))

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 14. Calculate nighttime net radiation (out_evap%rnn), J/m^2
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Eq. 56, SPLASH 2.0 Documentation
      ! adopted bugfix from Python version (iss#13)
      out_evap%rnn = (86400.0/pi)*(radians(rw*ru*(hs-hn)) + rw*rv*(dgsin(hs)-dgsin(hn)) - out_evap%rnl * (pi - radians(hn)))

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 16. Calculate daily condensation (out_evap%cn), mm d-1
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Eq. 68, SPLASH 2.0 Documentation
      out_evap%cn = 1000.0 * out_evap%econ * abs(out_evap%rnn)
      
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 17. Estimate daily EET (out_evap%eet), mm d-1
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Eq. 70, SPLASH 2.0 Documentation
      out_evap%eet = 1000.0 * out_evap%econ * out_evap%rn

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 18. Estimate daily PET (out_evap%pet), mm d-1
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Eq. 72, SPLASH 2.0 Documentation
      out_evap%pet   = ( 1.0 + kw ) * out_evap%eet
      out_evap%pet_e = out_evap%pet / (out_evap%econ * 1000)

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 19. Calculate variable substitute (rx), (mm/hr)/(W/m^2)
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      rx = 1000.0 * 3600.0 * ( 1.0 + kw ) * out_evap%econ
      
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 20. Calculate the intersection hour angle (hi), degrees
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      cos_hi = sw/(rw*rv*rx) + out_evap%rnl/(rw*rv) - ru/rv   ! sw contains info of soil moisture (evaporative supply rate)
      if (cos_hi >= 1.0) then
        ! Supply exceeds demand:
        hi = 0.0
      elseif (cos_hi <= -1.0) then
        ! Supply limits demand everywhere:
        hi = 180.0
      else
        hi = degrees(acos(cos_hi))
      end if

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 21. Estimate daily AET (out_evap%aet), mm d-1
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! Eq. 81, SPLASH 2.0 Documentation
      out_evap%aet = (24.0/pi)*(radians(sw*hi) + rx*rw*rv*(dgsin(hn) - dgsin(hi)) + radians((rx*rw*ru - rx*out_evap%rnl)*(hn - hi)))
      out_evap%aet_e = out_evap%aet / (out_evap%econ * 1000)

      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 22. Calculate Cramer-Prentice-Alpha, (unitless)
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (out_evap%eet>0.0) then 
        out_evap%cpa = out_evap%aet / out_evap%eet
      else
        out_evap%cpa = 1.0 + kw
      end if

    end if


    ! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! ! 23. CRUDEFIX: When temperature is below zero, set bot PET and AET to zero. 
    ! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! if (tc < 0.0) then
    !   out_evap%pet = 0.0
    !   out_evap%eet = 0.0
    !   out_evap%aet = 0.0
    ! end if

    ! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! ! 24. CRUDEFIX: When AET > PET, set AET to PET
    ! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! if (out_evap%aet > out_evap%pet) then
    !   out_evap%aet = out_evap%pet
    ! end if

    ! ! checks
    ! if (out_evap%pet< -5.0) then
    !   print*,'doy:', doy
    !   print*,'lat', lat
    !   print*,'doy', doy
    !   print*,'elv', elv
    !   print*,'sf', sf
    !   print*,'tc', tc
    !   print*,'sw', sw
    !   print*,'pet', out_evap%pet
    !   stop 'pet < -5.0'
    ! end if

    ! if (out_evap%aet>out_evap%pet) then
    !   print*,'doy:', doy
    !   print*,'lat', lat
    !   print*,'doy', doy
    !   print*,'elv', elv
    !   print*,'sf', sf
    !   print*,'tc', tc
    !   print*,'sw', sw
    !   stop 'aet > pet'
    ! end if

    !-------------------------------------------------------------   
    ! Refs: Allen, R.G. (1996), Assessing integrity of weather data for 
    !         reference evapotranspiration estimation, Journal of Irrigation
    !         and Drainage Engineering, vol. 122, pp. 97--106.
    !       Allen, R.G., L.S. Pereira, D. Raes, M. Smith (1998), 
    !         'Meteorological data,' Crop evapotranspiration - Guidelines for 
    !         computing crop water requirements - FAO Irrigation and drainage 
    !         paper 56, Food and Agriculture Organization of the United 
    !         Nations, online: http://www.fao.org/docrep/x0490e/x0490e07.htm
    !       Berger, A.L. (1978), Long-term variations of daily insolation and 
    !         quarternary climatic changes, Journal of Atmospheric Sciences, 
    !         vol. 35, pp. 2362--2367.
    !       Berger, A.L., M.F. Loutre, and C. Tricot (1993), Insolation and 
    !         Earth's orbital periods, J. Geophys. Res., 98, 10341--10362.
    !       Duffie, J. A. and W. A. Beckman (1991). Solar engineering of 
    !         thermal processes. 4th ed. New Jersey: John Wiley and Sons
    !       Federer (1982), Transpirational supply and demand: plant, soil, 
    !         and atmospheric effects evaluated by simulation, Water 
    !         Resources Research, vol. 18, no. 2, pp. 355--362.
    !       Ge, S., R.G. Smith, C.P. Jacovides, M.G. Kramer, R.I. Carruthers 
    !         (2011), Dynamics of photosynthetic photon flux density (PPFD) 
    !         and estimates in coastal northern California, Theoretical and 
    !         Applied Climatology, vol. 105, pp. 107--118.
    !       Henderson-Sellers, B. (1984), A new formula for latent heat of 
    !         vaporization of water as a function of temperature, Quarterly 
    !         Journal of the Royal Meteorological Society 110, pp. 1186–1190
    !       Linacre (1968), Estimating the net-radiation flux, Agricultural 
    !         Meteorology, vol. 5, pp. 49--63.
    !       Prentice, I.C., M.T. Sykes, W. Cramer (1993), A simulation model 
    !         for the transient effects of climate change on forest 
    !         landscapes, Ecological Modelling, vol. 65, pp. 51--70.
    !       Priestley, C.H.B. and R.J. Taylor (1972), On the assessment of 
    !         surface heat flux and evaporation using large-scale parameters, 
    !         Monthly Weather Review, vol. 100 (2), pp. 81--92.
    !       Spencer, J. W. (1971), Fourier series representation of the 
    !         position of the sun, Search, vol. 2, p. 172.
    !       Stine, W. B. and M. Geyer (2001). “Power from the Sun”. 
    !         online: http://www.powerfromthesun.net/Book/chapter03/chapter03
    !       Wetherald, R.T., S. Manabe (1972), Response to joint ocean-
    !         atmosphere model to the seasonal variation of the solar 
    !         radiation, Monthly Weather Review, vol. 100 (1), pp. 42--59.
    !       Woolf, H. M. (1968). On the computation of solar evaluation 
    !         angles and the determination of sunrise and sunset times. 
    !         Tech. rep. NASA-TM-X-164. National Aeronautics and Space 
    !         Administration (NASA).
    !-------------------------------------------------------------   
  end function get_evap

  
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

    if (sn==dummy) then
      fsnow = max( min( 1.0,  1.0 - ( 1.0 / 2.0 ) * tc ), 0.0 )
      out_snow_rain%snow_updated   = snow + fsnow * pr - melt
      out_snow_rain%liquid_to_soil = pr * ( 1.0 - fsnow ) + melt
    else
      out_snow_rain%snow_updated   = snow + sn - melt
      out_snow_rain%liquid_to_soil = pr + melt
    end if

  end function get_snow_rain


  function calc_vdpstress( vpd ) result( out_vpdstress )
    !/////////////////////////////////////////////////////////////////////////
    ! Calculates a VPD stress function based on Oren et al. 2001 Eq. 4
    ! Reference: 
    ! Oren et al.: Sensitivity of mean canopy stomatal conductance
    ! to vapor pressure deficit in a flooded Taxodium distichum L. forest:
    ! hydraulic and non-hydraulic effectsOecologia (2001) 126:21–29, 
    ! DOI 10.1007/s004420000497
    !-------------------------------------------------------------------------
    ! arguments
    real, intent(in) :: vpd    ! Vapour pressure deficit (Pa)

    ! function return variable
    real :: out_vpdstress

    if (vpd<1) then
      out_vpdstress = 1.0
    else
      out_vpdstress = vpdstress_par_a * (vpdstress_par_b - vpdstress_par_m * (log(0.001) + log(vpd)))
      if (out_vpdstress > 1.0) out_vpdstress = 1.0
      if (out_vpdstress < 0.0) out_vpdstress = 0.0
    end if

  end function calc_vdpstress


  function calc_dr( nu ) result( dr )
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Calculates distance factor (dr), unitless
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


  function calc_delta( lambda ) result( delta )
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Calculates declination angle (delta), degrees
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! arguments
    real, intent(in) :: lambda

    ! function return variable
    real :: delta

    ! Woolf (1968)
    delta = asin( dgsin( lambda ) * dgsin( keps ) )   ! xxx use asin with single-precision compilation
    delta = degrees( delta )

  end function calc_delta


  function calc_ru_rv( delta, lat ) result( out_ru_rv )
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Calculates variable substitutes (u and v), unitless
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! arguments
    real, intent(in) :: delta
    real, intent(in) :: lat

    ! local variables
    real :: ru, rv

    ! function return variable
    real, dimension(2) :: out_ru_rv

    ru = dgsin(delta) * dgsin(lat)
    rv = dgcos(delta) * dgcos(lat)

    out_ru_rv(1) = ru
    out_ru_rv(2) = rv

  end function calc_ru_rv


  function calc_hs( ru, rv ) result( hs )
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Calculates the sunset hour angle (hs), degrees
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Calculates transmittivity (tau), unitless
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    use md_interface, only: myinterface

    ! constant for dRnl (Monteith & Unsworth, 1990)
    kA       = 107.0
    
    ! shortwave albedo (Federer, 1968)
    kalb_sw  = 0.17
    
    ! visible light albedo (Sellers, 1985) xxx planetary albedo? xxx
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

    ! solar constant, W/m^2 (Kopp & Lean, 2011)
    kGsc     = 1360.8
    
    ! entrainment factor (Lhomme, 1997; Priestley & Taylor, 1972)
    kw       = 0.26
    
    ! longitude of perihelion for 2000 CE, degrees (Berger, 1978)
    komega   = 283.0

    ! maximum snow melting rate (mm d-1) (Orth et al., 2013)
    maxmeltrate = 3.0

    ! Parameter for Oren et al.-VPD stress function (see calc_vpdstress())
    vpdstress_par_a = myinterface%params_calib%vpdstress_par_a

    ! Parameter for Oren et al.-VPD stress function (see calc_vpdstress())
    vpdstress_par_b = myinterface%params_calib%vpdstress_par_b

    ! Parameter for Oren et al.-VPD stress function (see calc_vpdstress())
    vpdstress_par_m = myinterface%params_calib%vpdstress_par_m


  end subroutine getpar_modl_waterbal

  ! xxx put these functions into a 'contain' within calling SR?

  function dgcos( x ) result( dgcos_out )
    !----------------------------------------------------------------   
    ! Calculates the cosine of an angle given in degrees. Equal to 
    ! 'dsin' in Python version.
    !----------------------------------------------------------------   
    use md_params_core, only: pi

    ! arguments
    real, intent(in) :: x  ! angle, degrees (0-360)

    ! function return value
    real :: dgcos_out ! cosine value of x when x is in degrees

    !dgcos = dcos(x*pi/180.0)
    dgcos_out = cos(x*pi/180.0)  ! xxx use cos with single-precision compilation

  end function dgcos


  function dgsin( x ) result( dgsin_out )
    !----------------------------------------------------------------   
    ! Calculates the sinus of an angle given in degrees. Equal to 
    ! 'dsin' in Python version.
    !----------------------------------------------------------------   
    use md_params_core, only: pi

    ! arguments
    real, intent(in) :: x  ! angle, degrees (0-360)

    ! function return value
    real :: dgsin_out ! sinus value of x when x is in degrees

    !dgsin_out = dsin(x*pi/180.0)
    dgsin_out = sin(x*pi/180.0)   ! xxx use cos with single-precision compilation

  end function dgsin


  function degrees( x ) result( degrees_out )
    !----------------------------------------------------------------   
    ! Returns corresponding degrees if x is given in radians
    !----------------------------------------------------------------   
    use md_params_core, only: pi

    ! arguments
    real, intent(in) :: x  ! angle, radians

    ! function return value
    real :: degrees_out

    degrees_out = x*180.0/pi

  end function degrees


  function radians( x ) result( radians_out )
    !----------------------------------------------------------------   
    ! Returns corresponding radians if x is given in degrees
    !----------------------------------------------------------------   
    use md_params_core, only: pi

    ! arguments
    real, intent(in) :: x  ! angle, radians

    ! function return value
    real :: radians_out

    radians_out = x*pi/180.0

  end function radians


  function get_berger_tls( day ) result( out_berger )
    !----------------------------------------------------------------   
    ! Returns true anomaly and true longitude for a given day
    ! Reference: Berger, A. L. (1978), Long term variations of daily 
    ! insolation and quaternary climatic changes, J. Atmos. Sci., 35, 
    ! 2362-2367.
    !----------------------------------------------------------------   
    ! arguments
    integer, intent(in) :: day   ! day of the year

    ! function return value
    type(outtype_berger) :: out_berger  ! stores output of function berger_tls

    ! local variables
    real :: anm, ranm, anv, ranv
    real :: dlamm                ! Mean longitude for day of year
    real :: my_nu
    real :: my_tls
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
    out_berger%lambda = anv + komega
    if (out_berger%lambda < 0.0) then
        out_berger%lambda = out_berger%lambda + 360.0
    else if (out_berger%lambda > 360.0) then
        out_berger%lambda = out_berger%lambda - 360.0
    endif

    ! True anomaly:
    out_berger%nu = (out_berger%lambda - komega)
    if (out_berger%nu < 0.0) then
        out_berger%nu = out_berger%nu + 360.0
    endif

  end function get_berger_tls


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


  function psychro( tc, press )
    !----------------------------------------------------------------   
    ! Calculates the psychrometric constant for a given temperature and pressure
    ! Ref: Allen et al. (1998); Tsilingiris (2008) 
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: tc     ! air temperature, degrees C
    real, intent(in) :: press  ! atmospheric pressure, Pa

    ! local variables
    real :: lv  ! latent heat of vaporization (J/kg)
    real :: cp

    ! function return value
    real :: psychro  ! psychrometric constant, Pa/K

    ! local variables
    real :: my_tc    ! adjusted temperature to avoid numerical blow-up 

    ! Adopted temperature adjustment from SPLASH, Python version
    my_tc = tc
    if (my_tc < 0) then
      my_tc = 0.0
    else if (my_tc > 100) then
      my_tc = 100.0
    end if

    ! Calculate the specific heat capacity of water, J/kg/K
    ! Eq. 47, Tsilingiris (2008)
    cp = 1.0e3*(&
               1.0045714270&
             + 2.050632750e-3  *my_tc&
             - 1.631537093e-4  *my_tc*my_tc&
             + 6.212300300e-6  *my_tc*my_tc*my_tc&
             - 8.830478888e-8  *my_tc*my_tc*my_tc*my_tc&
             + 5.071307038e-10 *my_tc*my_tc*my_tc*my_tc*my_tc&
            )

    ! Calculate latent heat of vaporization, J/kg
    lv = calc_enthalpy_vap(tc)

    ! Calculate psychrometric constant, Pa/K
    ! Eq. 8, Allen et al. (1998)
    psychro = cp * kMa * press / (kMv * lv)

  end function psychro


  subroutine init_rlm_waterbal( ngridcells )
    !////////////////////////////////////////////////////////////////
    ! Initialises waterbalance-specific output variables
    ! The same subroutine is used here for initialising rolling mean variables
    !----------------------------------------------------------------
    use md_interface, only: myinterface

    ! arguments
    integer, intent(in) :: ngridcells

    ! Rolling mean variables
    if (myinterface%steering%init) then
      if (.not.allocated(rlmalpha)) allocate( rlmalpha(nlu,ngridcells) )
    end if
    rlmalpha(:,:) = 0.0

  end subroutine init_rlm_waterbal


  subroutine getrlm_daily_waterbal( jpngr, doy )
    !////////////////////////////////////////////////////////////////
    ! Collect daily output variables
    ! so far not implemented for isotopes
    !----------------------------------------------------------------
    use md_interface, only: myinterface

    ! argument
    integer, intent(in) :: jpngr
    integer, intent(in) :: doy    

    if (evap(1)%pet > 0.0) then
      rlmalpha(:,jpngr)  = rlmalpha(:,jpngr) + (evap(:)%aet / evap(1)%pet) / ndayyear
    else
      rlmalpha(:,jpngr)  = rlmalpha(:,jpngr) + 1.0 / ndayyear
    end if

  end subroutine getrlm_daily_waterbal


  subroutine get_rlm_waterbal( phy, init )
    !/////////////////////////////////////////////////////////////////////////
    ! Calculates the rolling mean of relevant variables
    ! This requires the full arrays (all gridcells) to be stored.
    !-------------------------------------------------------------------------
    use md_params_core, only: nlu
    use md_tile, only: psoilphystype

    ! arguments
    type( psoilphystype ), dimension(:,:), intent(inout) :: phy
    logical :: init

    ! local variables
    integer, save :: ncalls
    integer :: nyrs_uptonow
    integer :: lu

    if (init) ncalls = 0
    ncalls = ncalls + 1
    nyrs_uptonow = min( ncalls, nyrs_rlmalpha )

    do lu=1,nlu
      phy(lu,:)%rlmalpha = ( phy(lu,:)%rlmalpha * (nyrs_uptonow - 1) + rlmalpha(lu,:) ) / nyrs_uptonow
    end do

  end subroutine get_rlm_waterbal


end module md_waterbal
