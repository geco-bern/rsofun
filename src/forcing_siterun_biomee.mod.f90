module md_forcing_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing treatment of forcing for BiomeE, linking
  ! what's obtained from R through SR biomee_f and what's needed by BiomeE.
  !----------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32, in=>int32
  use md_params_core, only: ntstepsyear, ndayyear, kTkelvin
  implicit none

  private
  public climate_type, getclimate, getco2

  type :: climate_type
    integer :: year          ! Year
    integer :: doy           ! day of the year
    real    :: hod           ! hour of the day
    real    :: PAR           ! umol m-2 s-1
    real    :: radiation     ! W/m2
    real    :: Tair          ! air temperature,  K
    real    :: Tsoil         ! soil temperature, K
    real    :: RH            ! relative humidity
    real    :: rain          ! kgH2O m-2 s-1
    real    :: windU         ! wind velocity (m s-1)
    real    :: P_air         ! pa
    real    :: CO2           ! mol CO2/mol dry air
    real    :: soilwater     ! soil moisture, vol/vol
    real    :: vpd           ! vapour pressure deficit (Pa)
  end type climate_type

contains

  function getclimate( nt, ntstepsyear, forcing, climateyear_idx ) result ( out_climate )
    !////////////////////////////////////////////////////////////////
    ! This function invokes file format specific "sub-functions/routines"
    ! to read from NetCDF. This nesting is necessary because this 
    ! cannot be done file-specific in SR sofun, but can be done here
    ! as this module is compilation-specific (only for global simulations)
    !----------------------------------------------------------------
    ! arguments
    integer, intent(in) :: nt ! number of time steps
    integer, intent(in) :: ntstepsyear   ! number of time steps per year of model
    ! integer, intent(in) :: ntstepsyear_forcing  ! number of time steps per year of forcing data
    real(kind=dp),  dimension(nt,13), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    integer, intent(in) :: climateyear_idx
    ! logical, intent(in) :: do_agg_climate

    ! local variables
    integer :: idx_start, idx_end, it
    real, parameter :: timestep = 1.0

    ! function return variable
    type(climate_type), dimension(ntstepsyear) :: out_climate

    ! !print*,'ntstepsyear', ntstepsyear
    ! !print*,'ntstepsyear_forcing', ntstepsyear_forcing

    idx_start = (climateyear_idx - 1) * ntstepsyear + 1
    idx_end   = idx_start + ntstepsyear - 1

    ! This is to read from ORNL file
    out_climate%year      = int(forcing(idx_start:idx_end, 1))
    out_climate%doy       = int(forcing(idx_start:idx_end, 2))
    out_climate%hod       = real(forcing(idx_start:idx_end, 3))
    out_climate%PAR       = real(forcing(idx_start:idx_end, 4))
    out_climate%radiation = real(forcing(idx_start:idx_end, 5))
    out_climate%Tair      = real(forcing(idx_start:idx_end, 6)) + 273.15
    out_climate%Tsoil     = real(forcing(idx_start:idx_end, 7)) + 273.15
    out_climate%RH        = real(forcing(idx_start:idx_end, 8)) * 0.01
    out_climate%rain      = real(forcing(idx_start:idx_end, 9))
    out_climate%windU     = real(forcing(idx_start:idx_end, 10))
    out_climate%P_air     = real(forcing(idx_start:idx_end, 11))
    out_climate%CO2       = real(forcing(idx_start:idx_end, 12)) * 1.0e-6 
    out_climate%soilwater = 0.8                                                   ! soil moisture, vol/vol

    !print*,'out_climate%year', out_climate(150)%year          ! Year
    !print*,'out_climate%doy', out_climate(150)%doy          ! day of the year
    !print*,'out_climate%hod', out_climate(150)%hod         ! hour of the day
    !print*,'out_climate%PAR, umol/m2/s', out_climate(150)%PAR         ! umol/m2/s  (1W/m2 = 2.02 umol/m2/s )         
    !print*,'out_climate%radiation, W/m2', out_climate(150)%radiation         ! W/m2
    !print*,'out_climate%Tair, K', out_climate(150)%Tair         ! air temperature, K
    !print*,'out_climate%Tsoil, K', out_climate(150)%Tsoil         ! soil temperature, K
    !print*,'out_climate%RH, fraction', out_climate(150)%RH         ! relative humidity as a fraction (0.xx)
    !print*,'out_climate%rain, kgH2O m-2 s-1', out_climate(150)%rain         ! kgH2O m-2 s-1
    !print*,'out_climate%windU, (m s-1)', out_climate(150)%windU        ! wind velocity (m s-1)
    !print*,'out_climate%P_air, ! pa', out_climate(150)%P_air        ! pa
    !print*,'out_climate%CO2, ! mol/mol', out_climate(150)%CO2        ! mol/mol

    do it=1,ntstepsyear
      out_climate(it)%vpd  = calc_vpd_rh( out_climate(it)%RH, (out_climate(it)%Tair - kTkelvin) )
    end do

    ! if (do_agg_climate) then
    !   out_climate(:) = aggregate_climate_byday( out_climate(idx_start:idx_end) )
    ! end if

  end function getclimate


  ! function aggregate_climate_byday( forcing ) result( forcing_agg )
  !   !////////////////////////////////////////////////////////////////
  !   ! Takes mean over fast time steps provided in input, and 
  !   ! returns aggregated values.
  !   !----------------------------------------------------------------
  !   type(climate_type), dimension(:), intent(in) :: forcing

  !   ! function return variable
  !   type(climate_type), dimension(ndayyear) :: forcing_agg

  !   ! local
  !   integer :: nt  ! number of time steps per year (may vary)
  !   integer :: doy, idx_start, idx_end
  !   real :: nt_day

  !   nt = size(forcing, 1)
  !   nt_day = nt / ndayyear

  !   do doy = 1, ndayyear

  !     idx_start = (doy - 1) * nt_day + 1
  !     idx_end = idx_start + nt_day - 1

  !     forcing_agg(doy)%year      = forcing(idx_start)%year
  !     forcing_agg(doy)%doy       = forcing(idx_start)%doy
  !     forcing_agg(doy)%hod       = 12.0
  !     forcing_agg(doy)%PAR       = sum( forcing(idx_start:idx_end)%PAR ) / nt_day         ! umol m-2 s-1
  !     forcing_agg(doy)%radiation = sum( forcing(idx_start:idx_end)%radiation ) / nt_day   ! W/m2
  !     forcing_agg(doy)%Tair      = sum( forcing(idx_start:idx_end)%Tair ) / nt_day        ! air temperature,  K
  !     forcing_agg(doy)%Tsoil     = sum( forcing(idx_start:idx_end)%Tsoil ) / nt_day       ! soil temperature, K
  !     forcing_agg(doy)%RH        = sum( forcing(idx_start:idx_end)%RH ) / nt_day          ! relative humidity
  !     forcing_agg(doy)%rain      = sum( forcing(idx_start:idx_end)%rain ) / nt_day        ! kgH2O m-2 s-1
  !     forcing_agg(doy)%windU     = sum( forcing(idx_start:idx_end)%windU ) / nt_day       ! wind velocity (m s-1)
  !     forcing_agg(doy)%P_air     = sum( forcing(idx_start:idx_end)%P_air ) / nt_day       ! pa
  !     forcing_agg(doy)%CO2       = sum( forcing(idx_start:idx_end)%CO2 ) / nt_day         ! ppm
  !     forcing_agg(doy)%soilwater = sum( forcing(idx_start:idx_end)%soilwater ) / nt_day   ! soil moisture, vol/vol
  !     forcing_agg(doy)%vpd       = sum( forcing(idx_start:idx_end)%vpd ) / nt_day   ! soil moisture, vol/vol

  !   end do

  ! end function aggregate_climate_byday


  function getco2( nt, forcing, forcingyear_idx ) result( pco2 )
    !////////////////////////////////////////////////////////////////
    !  Function reads this year's atmospheric CO2 from input
    !----------------------------------------------------------------
    ! arguments
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp),  dimension(nt,13), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    ! type(climate_type), dimension(nt), intent(in) :: forcing

    integer, intent(in) :: forcingyear_idx

    ! function return variable
    real, dimension(ntstepsyear) :: pco2

    ! local variables 
    integer :: idx_start, idx_end!, year
    real, parameter :: timestep = 1.0

    idx_start = (forcingyear_idx - 1) * ntstepsyear + 1
    idx_end   = idx_start + ntstepsyear - 1

    ! if (int(forcing(idx_start, 1)) /= forcingyear) stop 'getco2(): forcingyear does not correspond to index read from forcing'
    
    !year = real(forcing(idx_start:idx_end, 1))           ! Year
    pco2 = real(forcing(idx_start:idx_end,12)) * 1.0e-6  ! mol/mol

  end function getco2


  function calc_vpd_rh( rh, tc ) result( vpd )
    !////////////////////////////////////////////////////////////////////////
    ! Calculates vapor pressure deficit
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in)    :: rh      ! relative humidity (fraction <1)
    real, intent(in)    :: tc      ! daily mean air temperature (deg C), daily varying from WATCH-WFDEI (ACTUALLY NOT USED)

    ! function return variable
    real :: vpd         ! vapor pressure deficit (Pa)

    ! local variables
    real :: esat       ! saturation water vapor pressure (Pa) at given air temperature

    esat = 611.0 * exp( (17.27 * tc)/(tc + 237.3) )

    vpd = esat * (1.0 - rh)

  end function calc_vpd_rh


end module md_forcing_biomee

