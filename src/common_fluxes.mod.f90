module md_common_fluxes
  !////////////////////////////////////////////////////////////////
  ! Definition of common_fluxes dereived type.
  ! Common fluxes are a set of fluxes present at every time resolution and present at both cohort and tile levels.
  !----------------------------------------------------------------

  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use md_params_core, only: dummy

  implicit none

  private

  ! Public types
  public common_fluxes


  type :: common_fluxes
    ! Fluxes common between cohorts and vegetation tiles
    ! Note: the unit depends on the context (timestep, daily, annual)
    real    :: Trsp          = 0.0
    real    :: GPP           = 0.0
    real    :: Resp          = 0.0
    real    :: Nup           = 0.0
    real    :: fixedN        = 0.0
    real    :: bigdelta      = dummy ! 13C isotope discrimination against atmospheric signature (permil)
    real    :: d13c_gpp      = dummy ! delta-13C isotopic signature, small delta (permil)

    ! initializing cumuative fluxes to 0.0
    ! initializing concentrations and fractionation factors to nonsense values)
    
    contains

    procedure npp
    procedure add
  end type common_fluxes

contains

  pure real function npp(self)
    ! Computes NPP as GPP - Resp
    class(common_fluxes), intent(in) :: self

    npp = self%gpp - self%resp
  end function npp

  pure subroutine add(self, delta, scale)
    ! Add delta quantities to partial fluxes (accounting)
    ! Optional scaling of the delta. By default: 1.0
    class(common_fluxes), intent(inout) :: self
    type(common_fluxes), intent(in) :: delta
    real, optional, intent(in) :: scale

    ! Local variable
    real :: scale_opt

    if (present(scale)) then
      scale_opt = scale
    else
      scale_opt = 1.0
    end if

    self%Trsp   = self%Trsp   + delta%Trsp   * scale_opt
    self%GPP    = self%GPP    + delta%GPP    * scale_opt
    self%Resp   = self%Resp   + delta%Resp   * scale_opt
    self%Nup    = self%Nup    + delta%Nup    * scale_opt
    self%fixedN = self%fixedN + delta%fixedN * scale_opt

    self%bigdelta = ieee_value(0.0, ieee_quiet_nan) ! xxx: undefined behavior
    self%d13c_gpp = ieee_value(0.0, ieee_quiet_nan) ! xxx: undefined behavior


  end subroutine add

end module md_common_fluxes
