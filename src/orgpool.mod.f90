module md_orgpool
  !////////////////////////////////////////////////////////////////
  ! Module contains Fortran 90 derived-type declarations to define
  ! material pools in SOFUN and functions applicable to pool types.
  !----------------------------------------------------------------

  implicit none

  private

  ! Public types
  public orgpool

  public operator(*), operator(/), operator(+), operator(-)

  ! Organic pools, contain carbon (c12) and nitrogen (n14)
  type orgpool
   real :: c12 = 0.0
   real :: n14 = 0.0

  contains

    procedure add_c12
    procedure add_n14

  end type orgpool

  interface operator (+)
    module procedure add
  end interface

  interface operator (-)
    module procedure sub
  end interface

  interface operator (*)
    module procedure scale_mul
  end interface

  interface operator (/)
    module procedure scale_div
  end interface

contains

  pure function add(p1, p2) result(res)
    type(orgpool), intent(in) :: p1, p2
    type(orgpool) :: res

    res = orgpool(p1%c12 + p2%c12, &
            p1%n14 + p2%n14)

  end function add

  pure function sub(p1, p2) result(res)
    type(orgpool), intent(in) :: p1, p2
    type(orgpool) :: res

    res = orgpool(p1%c12 - p2%c12, &
            p1%n14 - p2%n14)

  end function sub

  pure function scale_mul(p, k) result(res)
    type(orgpool), intent(in) :: p
    real, intent(in) :: k
    type(orgpool) :: res

    res = orgpool(p%c12 * k, &
            p%n14 * k)

  end function scale_mul

  pure function scale_div(p, k) result(res)
    type(orgpool), intent(in) :: p
    real, intent(in) :: k
    type(orgpool) :: res

    res = orgpool(p%c12 / k, &
            p%n14 / k)

  end function scale_div

  subroutine add_c12(self, delta)
    class(orgpool), intent(inout) :: self
    real, intent(in) :: delta

    self%c12 = self%c12+ delta
  end subroutine add_c12

  subroutine add_n14(self, delta)
    class(orgpool), intent(inout) :: self
    real, intent(in) :: delta

    self%n14 = self%n14 + delta
  end subroutine add_n14

end module md_orgpool
