module md_params_siml_biomee
  !////////////////////////////////////////////////////////////////
  ! Module for handling simulation parameters for BiomeE.
  !----------------------------------------------------------------
  use md_params_core, only: steering_parameters
  use, intrinsic :: iso_c_binding, only: c_double

  implicit none

  private
  public params_siml_biomee

  integer, public, parameter :: nvars_params_siml    = 10

  !----------------------------------------------------------------
  ! Derived type for simulation parameters
  !----------------------------------------------------------------
  type params_siml_biomee

    type(steering_parameters) :: steering
    logical :: do_U_shaped_mortality
    logical :: do_closedN_run
    character(len=30) :: method_photosynth
    character(len=30) :: method_mortality

    contains

    procedure populate

  end type params_siml_biomee

  contains

  subroutine populate(self, params_siml)
    class(params_siml_biomee), intent(inout) :: self
    real(kind=c_double), dimension(:), intent(in) :: params_siml

    self%steering%do_spinup        = int(params_siml(1)) /= 0
    self%steering%spinupyears      = int(params_siml(2))
    self%steering%recycle          = int(params_siml(3))
    self%steering%firstyeartrend   = int(params_siml(4))
    self%steering%nyeartrend       = int(params_siml(5))

    if (self%steering%do_spinup) then
      self%steering%runyears = self%steering%nyeartrend &
              + self%steering%spinupyears
    else
      self%steering%runyears    = self%steering%nyeartrend
      self%steering%spinupyears = 0
    endif

    ! Simulation parameters
    self%do_U_shaped_mortality = int(params_siml(7)) /= 0
    self%do_closedN_run        = int(params_siml(8)) /= 0

    ! this needs to be consistent with translation to code in run_biomee_f_bysite.R
    if (int(params_siml(9)) == 1) then
      self%method_photosynth = "gs_leuning"
    else
      self%method_photosynth = "pmodel"
    end if

    select case( int(params_siml(10)) )
    case (1)
      self%method_mortality = "cstarvation"
    case (2)
      self%method_mortality = "growthrate"
    case (3)
      self%method_mortality = "dbh"
    case (4)
      self%method_mortality = "const_selfthin"
    case (5)
      self%method_mortality = "bal"
    end select

  end subroutine populate

end module md_params_siml_biomee


