module md_rates
  !////////////////////////////////////////////////////////////////
  ! Module containing temperature and moisture response functions.
  !----------------------------------------------------------------
  implicit none

  private
  public ftemp, fmoist

contains

  function ftemp( temp, method, ref_temp )
    !////////////////////////////////////////////////////////////////
    !  Temperature response function
    !----------------------------------------------------------------
    ! arguments
    real, intent(in)             :: temp ! temperature [in Degrees Celsius]
    character(len=*), intent(in) :: method
    real, intent(in), optional   :: ref_temp

    ! local variables
    real                         :: ref_temp_local  ! local copy of ref_temp

    ! for lloyd and taylor method
    real, parameter :: E0 = 308.56      ! Activation Energy
    real, parameter :: T0 = 227.13      ! calibration temperature [K]
    real, parameter :: Tzero = 273.15   ! 0°C = 273.15 K 

    ! function return variable
    real :: ftemp

    ! set default reference temperature to 10 deg C
    if (present(ref_temp)) then
     ref_temp_local = ref_temp
    else
     ref_temp_local = 10.0
    endif

    select case (method)

      case ("lloyd_and_taylor")
        !----------------------------------------------------------------
        ! LLOYD AND TAYLOR
        ! Temperature response function is a modified Q10 relationship
        ! (Lloyd & Taylor 1994)
        !----------------------------------------------------------------
        if (temp.ge.-40.0) then 
          ! avoid numerical errors
          ftemp = exp(E0 * ((1.0 / (ref_temp_local + Tzero - T0)) - (1.0 / (temp + Tzero - T0))))
        else
          ! set temperature response to a constant at value of -40°C
          ftemp = exp(E0 * ((1.0 / (ref_temp_local + Tzero - T0)) - (1.0 / (-40.0 + Tzero - T0))))
        end if

      case default

        ! stop 'FTEMP: select valid method'

    end select

    return

  end function ftemp


  function fmoist( moist, method )
    !////////////////////////////////////////////////////////////////
    !  Temperature response function
    !----------------------------------------------------------------
    ! arguments
    real, intent(in)             :: moist ! temperature [in Degrees Celsius]
    character(len=*), intent(in) :: method

    ! function return variable
    real :: fmoist

    select case (method)

      case ("foley")
        !----------------------------------------------------------------
        ! FOLEY
        ! Calculates decomposition rate modifier for a given water fraction
        ! according to Foley 1995
        !----------------------------------------------------------------
        fmoist = ( 0.25 + ( 0.75 * moist ) )

      case default

        ! stop 'FMOIST: select valid method'

    end select

    return

  end function fmoist


end module md_rates

