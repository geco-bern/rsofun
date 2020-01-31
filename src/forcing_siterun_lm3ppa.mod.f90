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
  use md_params_core_lm3ppa, only: ndayyear, nlu, dummy
  use md_grid, only: domaininfo_type, gridtype
  implicit none

  private
  public getco2, getninput, ninput_type, gettot_ninput, getfapar, getclimate_site, &
    getlanduse, landuse_type, climate_type

  type climate_type
    integer(kind=in), dimension(nhoursyear) :: year          ! Year
    integer(kind=in), dimension(nhoursyear) :: doy           ! day of the year
    real(kind=sp), dimension(nhoursyear)    :: hod           ! hour of the day
    real(kind=sp), dimension(nhoursyear)    :: PAR           ! umol m-2 s-1
    real(kind=sp), dimension(nhoursyear)    :: radiation     ! W/m2
    real(kind=sp), dimension(nhoursyear)    :: Tair          ! air temperature,  K
    real(kind=sp), dimension(nhoursyear)    :: Tsoil         ! soil temperature, K
    real(kind=sp), dimension(nhoursyear)    :: RH            ! relative humidity
    real(kind=sp), dimension(nhoursyear)    :: rain          ! kgH2O m-2 s-1
    real(kind=sp), dimension(nhoursyear)    :: windU         ! wind velocity (m s-1)
    real(kind=sp), dimension(nhoursyear)    :: P_air         ! pa
    ! real(kind=sp), dimension(nhoursyear)    :: CO2           ! ppm
    real(kind=sp), dimension(nhoursyear)    :: soilwater     ! soil moisture, vol/vol
  end type climate_type

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
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp),  dimension(nt,13), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    integer, intent(in) :: climateyear_idx, climateyear

    ! local variables
    integer :: idx_start, idx_end
    integer, dimension(2) :: shape_forcing
    real, parameter :: timestep = 1.0

    ! function return variable
    type( climate_type ) :: out_climate

    idx_start = (climateyear_idx - 1) * nhoursyear + 1
    idx_end   = idx_start + nhoursyear - 1
    
    ! Test if forcing dimensions are correct
    shape_forcing = shape(forcing)
    if (idx_end>shape_forcing(1)) then
      stop 'forcing array size does not have enough rows.'
    end if

    if (int(forcing(idx_start, 1)) /= climateyear) stop 'getclimate(): climateyear does not correspond to index read from forcing'

    ! This is to read from ORNL file
    out_climate%year      = real(forcing(idx_start:idx_end, 1))           ! Year
    out_climate%doy       = real(forcing(idx_start:idx_end, 2))           ! day of the year
    out_climate%hod       = real(forcing(idx_start:idx_end, 3))           ! hour of the day
    out_climate%PAR       = real(forcing(idx_start:idx_end, 4)) * 2.0     ! umol/m2/s           ! umol m-2 s-1
    out_climate%radiation = real(forcing(idx_start:idx_end, 5))           ! W/m2
    out_climate%Tair      = real(forcing(idx_start:idx_end, 6)) + 273.16  ! air temperature, K
    out_climate%Tsoil     = real(forcing(idx_start:idx_end, 7)) + 273.16  ! soil temperature, K
    out_climate%RH        = real(forcing(idx_start:idx_end, 8)) * 0.01    ! relative humidity (0.xx)
    out_climate%rain      = real(forcing(idx_start:idx_end, 9))/(timestep * 3600) ! kgH2O m-2 s-1
    out_climate%windU     = real(forcing(idx_start:idx_end, 10))           ! wind velocity (m s-1)
    out_climate%P_air     = real(forcing(idx_start:idx_end, 11))           ! pa
    out_climate%soilwater = 0.8                                           ! soil moisture, vol/vol

  end function getclimate



  function getco2( nt, forcing, forcingyear_idx, forcingyear ) result( pco2 )
    !////////////////////////////////////////////////////////////////
    !  Function reads this year's atmospheric CO2 from input
    !----------------------------------------------------------------
    ! arguments
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp),  dimension(nt,13), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    integer, intent(in) :: forcingyear_idx, forcingyear

    ! function return variable
    real :: pco2

    ! local variables 
    integer :: idx_start, idx_end, year
    real, parameter :: timestep = 1.0

    idx_start = (climateyear_idx - 1) * nhoursyear + 1
    idx_end   = idx_start + nhoursyear - 1

    if (int(forcing(idx_start, 1)) /= forcingyear) stop 'getclimate(): forcingyear does not correspond to index read from forcing'

    year = real(forcing(idx_start:idx_end, 1))           ! Year
    pco2 = real(forcing(idx_start:idx_end, 12)) * 1.0e-6  ! mol/mol

  end function getco2  

end module md_forcing_lm3ppa

