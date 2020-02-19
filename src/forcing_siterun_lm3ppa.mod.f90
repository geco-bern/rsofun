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
  use md_params_core_lm3ppa, only: ntstepsyear
  implicit none

  private
  public climate_type, getclimate, getco2!, forcingData

  ! type climate_type
  !   integer(kind=in), dimension(ntstepsyear) :: year          ! Year
  !   integer(kind=in), dimension(ntstepsyear) :: doy           ! day of the year
  !   real(kind=sp), dimension(ntstepsyear)    :: hod           ! hour of the day
  !   real(kind=sp), dimension(ntstepsyear)    :: PAR           ! umol m-2 s-1
  !   real(kind=sp), dimension(ntstepsyear)    :: radiation     ! W/m2
  !   real(kind=sp), dimension(ntstepsyear)    :: Tair          ! air temperature,  K
  !   real(kind=sp), dimension(ntstepsyear)    :: Tsoil         ! soil temperature, K
  !   real(kind=sp), dimension(ntstepsyear)    :: RH            ! relative humidity
  !   real(kind=sp), dimension(ntstepsyear)    :: rain          ! kgH2O m-2 s-1
  !   real(kind=sp), dimension(ntstepsyear)    :: windU         ! wind velocity (m s-1)
  !   real(kind=sp), dimension(ntstepsyear)    :: P_air         ! pa
  !   ! real(kind=sp), dimension(ntstepsyear)    :: CO2           ! ppm
  !   real(kind=sp), dimension(ntstepsyear)    :: soilwater     ! soil moisture, vol/vol
  ! end type climate_type

  ! xxx temporary: for test remove again afterwards
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
     real    :: CO2           ! ppm
     real    :: soilwater     ! soil moisture, vol/vol
  end type climate_type

  ! Input forcing data
  !type(climate_type), pointer, save :: forcingData(:)

contains

  function getclimate( nt, forcing, climateyear_idx, climateyear ) result ( out_climate )
  ! function getclimate( nt, forcing, climateyear_idx, in_ppfd, in_netrad ) result ( out_climate )
    !////////////////////////////////////////////////////////////////
    ! This function invokes file format specific "sub-functions/routines"
    ! to read from NetCDF. This nesting is necessary because this 
    ! cannot be done file-specific in SR sofun, but can be done here
    ! as this module is compilation-specific (only for global simulations)
    !----------------------------------------------------------------
    ! arguments
    integer, intent(in) :: nt ! number of time steps

    real(kind=dp),  dimension(nt,13), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    !type(climate_type), dimension(nt), intent(in) :: forcing

    integer, intent(in) :: climateyear_idx, climateyear

    ! local variables
    integer :: idx_start, idx_end
    ! integer, dimension(2) :: shape_forcing
    real, parameter :: timestep = 1.0

    ! function return variable
    type(climate_type), dimension(ntstepsyear) :: out_climate

    idx_start = (climateyear_idx - 1) * ntstepsyear + 1
    idx_end   = idx_start + ntstepsyear - 1

    ! This is to read from ORNL file
    out_climate%year      = int(forcing(idx_start:idx_end, 1))           ! Year
    out_climate%doy       = int(forcing(idx_start:idx_end, 2))           ! day of the year
    out_climate%hod       = real(forcing(idx_start:idx_end, 3))           ! hour of the day
    out_climate%PAR       = real(forcing(idx_start:idx_end, 4)) * 2.0     ! umol/m2/s           ! umol m-2 s-1
    out_climate%radiation = real(forcing(idx_start:idx_end, 5))           ! W/m2
    out_climate%Tair      = real(forcing(idx_start:idx_end, 6)) + 273.16  ! air temperature, K
    out_climate%Tsoil     = real(forcing(idx_start:idx_end, 7)) + 273.16  ! soil temperature, K
    out_climate%RH        = real(forcing(idx_start:idx_end, 8)) * 0.01    ! relative humidity (0.xx)
    out_climate%rain      = real(forcing(idx_start:idx_end, 9))/(timestep * 3600) ! kgH2O m-2 s-1
    out_climate%windU     = real(forcing(idx_start:idx_end, 10))           ! wind velocity (m s-1)
    out_climate%P_air     = real(forcing(idx_start:idx_end, 11))           ! pa
    out_climate%CO2       = real(forcing(idx_start:idx_end, 12))           ! pa
    out_climate%soilwater = 0.8                                           ! soil moisture, vol/vol

    ! if (forcing(idx_start,1) /= climateyear) then
    !   print*,'forcing(idx_start)%year ', forcing(idx_start,1)
    !   print*,'climateyear ', climateyear
    !   print*,'climateyear_idx ', climateyear_idx
    !   stop 'getclimate(): climateyear does not correspond to index read from forcing'
    ! end if

    ! !xxx check if units as they are provided in input file need to be adjusted (to be done in R), for conversions see above.
    ! out_climate(:)%year      = int(forcing(idx_start:idx_end,1))          ! Year
    ! out_climate(:)%doy       = int(forcing(idx_start:idx_end,2))           ! day of the year
    ! out_climate(:)%hod       = real(forcing(idx_start:idx_end,3))           ! hour of the day
    ! out_climate(:)%PAR       = real(forcing(idx_start:idx_end,4))           ! umol m-2 s-1
    ! out_climate(:)%radiation = real(forcing(idx_start:idx_end,5))     ! W/m2
    ! out_climate(:)%Tair      = real(forcing(idx_start:idx_end,6))          ! air temperature,  K
    ! out_climate(:)%Tsoil     = real(forcing(idx_start:idx_end,7))         ! soil temperature, K
    ! out_climate(:)%RH        = real(forcing(idx_start:idx_end,8))            ! relative humidity
    ! out_climate(:)%rain      = real(forcing(idx_start:idx_end,9))          ! kgH2O m-2 s-1
    ! out_climate(:)%windU     = real(forcing(idx_start:idx_end,10))         ! wind velocity (m s-1)
    ! out_climate(:)%P_air     = real(forcing(idx_start:idx_end,11))         ! pa
    ! out_climate(:)%CO2       = real(forcing(idx_start:idx_end,12))           ! ppm
    ! out_climate(:)%soilwater = real(forcing(idx_start:idx_end,13))     ! soil moisture, vol/vol

    !out_climate(:) = forcing(idx_start:idx_end)

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


end module md_forcing_lm3ppa

