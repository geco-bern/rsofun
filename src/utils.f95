module utils

  ! This is the second module, just to try if everything would work, by using multiple modules

  use, intrinsic :: iso_c_binding, only: c_double, c_int

  implicit none

  private
  public :: paramstype_siml, addPi

  real(kind=c_double), parameter :: pi = 3.141592654_c_double

  type paramstype_siml
    logical :: spinup 
  end type paramstype_siml

contains

  function addPi(x, n) result(y)

    integer(kind=c_int), intent(in) :: n
    real(kind=c_double), dimension(n), intent(in) :: x
    real(kind=c_double), dimension(n) :: y

    y = x + pi

  end function addPi

end module utils