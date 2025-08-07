module md_orgpool
  !////////////////////////////////////////////////////////////////
  ! Derived type describing material pools (C and N).
  !----------------------------------------------------------------

  implicit none

  private

  ! Public types
  public orgpool

  public operator(*), operator(/), operator(+), operator(-)

  ! Organic pools, contain carbon (c12) and nitrogen (n14)
  type orgpool
   real :: c12 = 0.0
   real :: d13 = -28.0  ! arbitrary starting value for delta-13C, permil
   real :: n14 = 0.0

  contains

    procedure add_c12
    procedure add_n14

  end type orgpool

  !===== Operators
  ! Operators are syntactical suggar for calling a function.
  ! For example, 'add(pool1, pool2)' can be written> 'pool1 + pool2'
  interface operator (+)
    module procedure add
  end interface

  interface operator (-)
    module procedure sub
  end interface

  ! Note: * operator is NOT commutative.
  ! Right: 'pool * 2.0' (the scalar must come in second position)
  ! Wrong: '2.0 * pool' (will not compile)
  interface operator (*)
    module procedure scale_mul
  end interface

  interface operator (/)
    module procedure scale_div
  end interface

contains

  pure function add(p1, p2) result(res)
    ! Returns a pool containing the sum of two pools.
    ! Isotopic signature (d13) mixing weighted by mass.
    ! Use operator +: 'p1 + p2'
    type(orgpool), intent(in) :: p1, p2
    type(orgpool) :: res

    res = orgpool( &
      p1%c12 + p2%c12, &
      (p1%d13 * p1%c12 + p2%d13 * p2%c12) / (p1%c12 + p2%c12), &
      p1%n14 + p2%n14 &
      )

  end function add

  pure function sub(p1, p2) result(res)
    ! Returns a pool containing the difference of two pools.
    ! Isotopic signature unchanged (no discrimination by subtraction) ! TODO: XXX, this should discriminate if p2 has a different signature
    ! Use operator -: 'p1 - p2'
    type(orgpool), intent(in) :: p1, p2
    type(orgpool) :: res

    res = orgpool( &
      p1%c12 - p2%c12, &
      p1%d13, &
      p1%n14 - p2%n14 &
      )

  end function sub

  pure function scale_mul(p, k) result(res)
    ! Returns a pool containing a scaled pool.
    ! Use operator *: 'p * k'
    ! Isotopic signature taken from initial pool (p)
    ! Attention, '*' is non-comutative. The scalar must appear in second position.
    type(orgpool), intent(in) :: p
    real, intent(in) :: k
    type(orgpool) :: res

    res = orgpool( &
      p%c12 * k, &
      p%d13, &
      p%n14 * k  &
      )

  end function scale_mul

  pure function scale_div(p, k) result(res)
    ! Returns a pool containing a scaled pool (by a factor 1/k).
    ! Isotopic signature taken from initial pool (p)
    ! Use operator /: 'p / k'
    type(orgpool), intent(in) :: p
    real, intent(in) :: k
    type(orgpool) :: res

    res = orgpool( &
      p%c12 / k, &
      p%d13, &
      p%n14 / k  &
      )

  end function scale_div

  subroutine add_c12(self, delta) ! TODO: XXX, we need to define if c12 is only C12 or any C (i.e. C12 and C13 (and C14))
    ! Add c12 amount to this pool
    class(orgpool), intent(inout) :: self
    real, intent(in) :: delta

    self%c12 = self%c12 + delta
  end subroutine add_c12

  subroutine add_n14(self, delta)
    ! Add n14 amount to this pool
    class(orgpool), intent(inout) :: self
    real, intent(in) :: delta

    self%n14 = self%n14 + delta
  end subroutine add_n14

end module md_orgpool
