module md_forcing_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing treatment of forcing for BiomeE, linking
  ! what's obtained from R through SR biomee_f and what's needed by BiomeE.
  !----------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32, in=>int32
  use md_params_core, only: ntstepsyear, ndayyear, kTkelvin, kfFEC
  implicit none

  private
  public climate_type, getclimate

  type :: climate_type
    real    :: ppfd          ! mol m-2 s-1
    real    :: radiation     ! W m-2 (SW downwelling, computed from ppfd)
    real    :: Tair          ! air temperature,  K
    real    :: vpd           ! vapor pressure deficit (Pa)
    real    :: rain          ! kgH2O m-2 s-1
    real    :: windU         ! wind velocity (m s-1)
    real    :: P_air         ! pa
    real    :: CO2           ! mol CO2/mol dry air
    real    :: RH            ! relative humidity (fraction < 1, computed from vpd and Tair)
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
    real(kind=dp),  dimension(nt,7), intent(in)  :: forcing  ! array containing all temporally varying forcing data
    integer, intent(in) :: climateyear_idx

    ! local variables
    integer :: idx_start, idx_end, it

    ! function return variable
    type(climate_type), dimension(ntstepsyear) :: out_climate

    idx_start = (climateyear_idx - 1) * ntstepsyear + 1
    idx_end   = idx_start + ntstepsyear - 1

    ! This is to read from ORNL file
    out_climate%ppfd      = real(forcing(idx_start:idx_end, 1))
    out_climate%Tair      = real(forcing(idx_start:idx_end, 2)) + kTkelvin
    out_climate%vpd       = real(forcing(idx_start:idx_end, 3))
    out_climate%rain      = real(forcing(idx_start:idx_end, 4))
    out_climate%windU     = real(forcing(idx_start:idx_end, 5))
    out_climate%P_air     = real(forcing(idx_start:idx_end, 6))
    out_climate%CO2       = real(forcing(idx_start:idx_end, 7)) * 1.0e-6

    out_climate%radiation = out_climate%ppfd / (kfFEC * 1.0e-6)

    do it=1,ntstepsyear
      out_climate(it)%RH  = calc_rh_vpd( out_climate(it)%vpd, (out_climate(it)%Tair - kTkelvin) )
    end do

  end function getclimate

  function calc_rh_vpd( vpd, tc ) result( rh )
    !////////////////////////////////////////////////////////////////////////
    ! Calculates vapor pressure deficit
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in)    :: vpd     ! vapor pressure deficit (Pa)
    real, intent(in)    :: tc      ! daily mean air temperature (deg C)

    ! function return variable
    real :: rh         ! relative humidity (fraction <1)

    ! local variables
    real :: esat       ! saturation water vapor pressure (Pa) at given air temperature

    esat = 611.0 * exp( (17.27 * tc)/(tc + 237.3) )

    rh = 1.0 - (vpd / esat)

  end function calc_rh_vpd


end module md_forcing_biomee

