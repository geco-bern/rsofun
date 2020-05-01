module md_forcing_lm3ppa
  !////////////////////////////////////////////////////////////////
  ! Module contains forcing variables (climate, co2, ...), and
  ! subroutines used to read forcing input files for a specific year
  ! ('forcingyear'), specifically for site scale simulations.
  ! This module is only used on the level of 'sofun', but not
  ! within 'biosphere', as these variables are passed on to 'biosphere'
  ! as arguments.
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32, in=>int32
  use md_params_core, only: ntstepsyear, kTkelvin
  implicit none

  private
  public climate_type, getclimate, getco2!, forcingData

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

  function getclimate( nt, ntstepsyear, ntstepsyear_forcing, forcing, climateyear_idx, climateyear, do_agg_climate ) result ( out_climate )
  ! function getclimate( nt, forcing, climateyear_idx, in_ppfd, in_netrad ) result ( out_climate )
    !////////////////////////////////////////////////////////////////
    ! This function invokes file format specific "sub-functions/routines"
    ! to read from NetCDF. This nesting is necessary because this 
    ! cannot be done file-specific in SR sofun, but can be done here
    ! as this module is compilation-specific (only for global simulations)
    !----------------------------------------------------------------
    ! arguments
    integer, intent(in) :: nt ! number of time steps
    integer, intent(in) :: ntstepsyear   ! number of time steps per year of model
    integer, intent(in) :: ntstepsyear_forcing  ! number of time steps per year of forcing data
    real,  dimension(nt,13), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    integer, intent(in) :: climateyear_idx, climateyear
    logical, intent(in) :: do_agg_climate

    ! local variables
    integer :: idx_start, idx_end, it
    real, parameter :: timestep = 1.0

    ! function return variable
    type(climate_type), dimension(ntstepsyear) :: out_climate

    idx_start = (climateyear_idx - 1) * ntstepsyear_forcing + 1
    idx_end   = idx_start + ntstepsyear_forcing - 1

    ! This is to read from ORNL file
    out_climate%year      = int(forcing(idx_start:idx_end, 1))                    ! Year
    out_climate%doy       = int(forcing(idx_start:idx_end, 2))                    ! day of the year
    out_climate%hod       = real(forcing(idx_start:idx_end, 3))                   ! hour of the day
    out_climate%PAR       = real(forcing(idx_start:idx_end, 4))                   ! umol/m2/s  (*2)         
    out_climate%radiation = real(forcing(idx_start:idx_end, 5))                   ! W/m2
    out_climate%Tair      = real(forcing(idx_start:idx_end, 6)) + 273.16          ! air temperature, K
    out_climate%Tsoil     = real(forcing(idx_start:idx_end, 7)) + 273.16          ! soil temperature, K
    out_climate%RH        = real(forcing(idx_start:idx_end, 8)) * 0.01            ! relative humidity (0.xx)
    out_climate%rain      = real(forcing(idx_start:idx_end, 9))/(timestep * 3600) ! kgH2O m-2 s-1
    out_climate%windU     = real(forcing(idx_start:idx_end, 10))                  ! wind velocity (m s-1)
    out_climate%P_air     = real(forcing(idx_start:idx_end, 11))                  ! pa
    out_climate%CO2       = real(forcing(idx_start:idx_end, 12)) * 1.0e-6         ! mol/mol
    out_climate%soilwater = 0.8                                                   ! soil moisture, vol/vol

    do it=1,ntstepsyear
      out_climate(it)%vpd  = calc_vpd_rh( out_climate(it)%RH, (out_climate(it)%Tair - kTkelvin) )
    end do

    if (do_agg_climate) then
      out_climate(:) = aggregate_climate_byday( out_climate(idx_start:idx_end) )
    end if

  end function getclimate


  function getco2( nt, forcing, forcingyear_idx, forcingyear ) result( pco2 )
    !////////////////////////////////////////////////////////////////
    !  Function reads this year's atmospheric CO2 from input
    !----------------------------------------------------------------
    ! arguments
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp),  dimension(nt,13), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    ! type(climate_type), dimension(nt), intent(in) :: forcing

    integer, intent(in) :: forcingyear_idx, forcingyear

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


end module md_forcing_lm3ppa

