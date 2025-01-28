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
    ! Note: the unit depends on the context
    real    :: Trsp          = 0.0
    real    :: GPP           = 0.0
    real    :: NPP           = 0.0
    real    :: Resp          = 0.0
    real    :: Nup           = 0.0
    real    :: fixedN        = 0.0
  end type common_fluxes

contains
!=========================LOW-LEVEL================================

  subroutine update_fluxes(fluxes, delta)
    ! Add delta quantities to partial fluxes (accounting)
    type(common_fluxes), intent(inout) :: fluxes
    type(common_fluxes), intent(in) :: delta

    fluxes%Trsp   = fluxes%Trsp   + delta%Trsp
    fluxes%GPP    = fluxes%GPP    + delta%GPP
    fluxes%NPP    = fluxes%NPP    + delta%NPP
    fluxes%Resp   = fluxes%Resp   + delta%Resp
    fluxes%Nup    = fluxes%Nup    + delta%Nup
    fluxes%fixedN = fluxes%fixedN + delta%fixedN

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
