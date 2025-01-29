module md_classdefs
  !////////////////////////////////////////////////////////////////
  ! Module contains Fortran 90 derived-type declarations to define
  ! material pools in SOFUN and functions applicable to pool types.
  ! Pools can be:
  ! - carbon, consisting of C-12
  ! - nitrogen, consisting of N-14
  ! - organic material, consisting of carbon and nitrogen (inherits
  !   their defitions).
  !----------------------------------------------------------------
  use md_params_core, only: eps

  implicit none

  private

  ! Public types
  public carbon, nitrogen, orgpool, common_fluxes

  ! Public functions
  public orgcp, orginit, ccp, cinit, ncp, ninit, update_fluxes


  ! Carbon, so far contains only c12 (to be extended for c13)
  type carbon
   real :: c12 = 0.0
  end type carbon

  type nitrogen
   real :: n14 = 0.0
  end type nitrogen

  ! Organic pools, contain carbon (c12) and nitrogen (n14)
  type orgpool
   type(carbon)   :: c
   type(nitrogen) :: n
  end type orgpool

  type :: common_fluxes
    ! Fluxes common between cohorts and vegetation tiles
    ! Note: the unit depends on the context (timestep, daily, annual)
    real    :: Trsp          = 0.0
    real    :: GPP           = 0.0
    real    :: Resp          = 0.0
    real    :: Nup           = 0.0
    real    :: fixedN        = 0.0

    contains

    procedure npp
  end type common_fluxes

contains
!=========================LOW-LEVEL================================

  pure real function npp(self)
    class(common_fluxes), intent(in) :: self

    npp = self%gpp - self%resp
  end function npp

  subroutine update_fluxes(fluxes, delta, scale)
    ! Add delta quantities to partial fluxes (accounting)
    ! Optional scaling of the delta. By default: 1.0
    type(common_fluxes), intent(inout) :: fluxes
    type(common_fluxes), intent(in) :: delta
    real, optional, intent(in) :: scale

    ! Local variable
    real :: scale_opt

    if (present(scale)) then
      scale_opt = scale
    else
      scale_opt = 1.0
    end if

    fluxes%Trsp   = fluxes%Trsp   + delta%Trsp   * scale_opt
    fluxes%GPP    = fluxes%GPP    + delta%GPP    * scale_opt
    fluxes%Resp   = fluxes%Resp   + delta%Resp   * scale_opt
    fluxes%Nup    = fluxes%Nup    + delta%Nup    * scale_opt
    fluxes%fixedN = fluxes%fixedN + delta%fixedN * scale_opt

  end subroutine update_fluxes

!--------------------------ORGANIC---------------------------------

  subroutine orgcp( amount, to, scale )
    !////////////////////////////////////////////////////////////////
    !  Generic SR to "copy" organic mass to pool (e.g. for output).
    !  Does NOT substract amount moved ('amount') from source
    !----------------------------------------------------------------
    type(orgpool), intent(in) :: amount
    type(orgpool), intent(inout) :: to
    real, optional, intent(in) :: scale

    if ( present( scale ) ) then
      call ccp( amount%c,to%c, scale )
      call ncp( amount%n,to%n, scale )
    else
      call ccp( amount%c,to%c )
      call ncp( amount%n,to%n )
    end if

  end subroutine orgcp

  subroutine orginit( pool )
    !////////////////////////////////////////////////////////////////
    !  Generic SR to initialise organic pool
    !----------------------------------------------------------------
    type(orgpool), intent(inout) :: pool

    call cinit(pool%c)
    call ninit(pool%n)

  end subroutine orginit


  !--------------------------CARBON----------------------------------

  subroutine ccp( amount, to, scale )
    !////////////////////////////////////////////////////////////////
    !  Generic SR to "copy" carbon to pool (e.g. for output).
    !  Does NOT substract amount moved ('amount') from source
    !----------------------------------------------------------------
    type(carbon), intent(in) :: amount
    type(carbon), intent(inout) :: to
    real, optional, intent(in) :: scale

    if ( present( scale ) ) then
      to%c12 = to%c12 + amount%c12 * scale
    else
      to%c12 = to%c12 + amount%c12 
    end if

  end subroutine ccp


  subroutine cinit(pool)
    !////////////////////////////////////////////////////////////////
    !  Generic SR to initialise organic pool
    !----------------------------------------------------------------
    type(carbon), intent(inout) :: pool

    pool%c12 = 0.0

  end subroutine cinit


  !--------------------------NITROGEN--------------------------------

  subroutine ncp( amount, to, scale )
    !////////////////////////////////////////////////////////////////
    !  Generic SR to "copy" nitrogen to pool (e.g. for output).
    !  Does NOT substract amount moved ('amount') from source
    !----------------------------------------------------------------
    type(nitrogen), intent(in) :: amount
    type(nitrogen), intent(inout) :: to
    real, intent(in), optional :: scale

    if ( present( scale ) ) then
      to%n14 = to%n14 + amount%n14 * scale
    else
      to%n14 = to%n14 + amount%n14 
    end if

  end subroutine ncp

  subroutine ninit(pool)
    !////////////////////////////////////////////////////////////////
    !  Generic SR to initialise organic pool
    !----------------------------------------------------------------
    type(nitrogen), intent(inout) :: pool

    pool%n14 = 0.0

  end subroutine ninit

end module md_classdefs
