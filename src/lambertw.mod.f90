module md_lambertw
  !////////////////////////////////////////////////////////////////
  ! Module contains the WAPR algorithm (TOMS743) that approximates 
  ! the Lambert-W function (inverse of x*exp(x)). 
  ! Adopted from code accessible through 
  ! https://people.sc.fsu.edu/~jburkardt/f_src/toms743/toms743.html
  ! 
  ! References:
  ! Fred Fritsch, RE Shafer, WP Crowley,
  ! Algorithm 443: Solution of the Transcendental Equation W*exp(W)=X,
  ! Communications of the ACM,
  ! Volume 16, Number 1, February 1973, pages 123-124.
  ! 
  ! Andrew Barry, S. J. Barry, Patricia Culligan-Hensley,
  ! Algorithm 743: WAPR - A Fortran routine for calculating real values of the W-function,
  ! ACM Transactions on Mathematical Software,
  ! Volume 21, Number 2, June 1995, pages 172-181.

  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  implicit none

  private
  public :: calc_wapr

contains

  function calc_wapr( x, branch, nerror, l ) result( out_wapr )
    !*****************************************************************************80
    !
    !! WAPR approximates the W function.
    !
    !  Discussion:
    !
    !    The call will fail if the input value X is out of range.
    !    The range requirement for the upper branch is:
    !      -exp(-1) <= X.
    !    The range requirement for the lower branch is:
    !      -exp(-1) < X < 0.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    15 June 2014
    !
    !  Author:
    !
    !    Original FORTRAN77 version by Andrew Barry, S. J. Barry, 
    !    Patricia Culligan-Hensley.
    !    This FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    Andrew Barry, S. J. Barry, Patricia Culligan-Hensley,
    !    Algorithm 743: WAPR - A Fortran routine for calculating real 
    !    values of the W-function,
    !    ACM Transactions on Mathematical Software,
    !    Volume 21, Number 2, June 1995, pages 172-181.
    !
    !  Parameters:
    !
    !    Input, real X, the argument.
    !
    !    Input, integer branch, indicates the desired branch.
    !    * 0, the upper branch;
    !    * nonzero, the lower branch.
    !
    !    Output, integer NERROR, the error flag.
    !    * 0, successful call.
    !    * 1, failure, the input X is out of range.
    !
    !    Input, integer L, indicates the interpretation of X.
    !    * 1, X is actually the offset from -(exp-1), so compute W(X-exp(-1)).
    !    * not 1, X is the argument; compute W(X);
    !
    !    Output, real WAPR, the approximate value of W(X).
    !---------------------------------------------------------------------------
    ! arguments
    real, intent(in)     :: x          ! argument of Lambert-W function 
    integer, intent(in)  :: branch     ! indicates the desired branch (0: the upper branch, nonzero: the lower branch)
    integer, intent(out) :: nerror     ! error flag (0: successful call, 1: failure, the input X is out of range)
    integer, intent(in)  :: l          ! indicates the interpretation of X. 1: X is actually the offset from -(exp-1), so compute W(X-exp(-1)). not 1: X is the argument; compute W(X)

    ! function return variable
    real :: out_wapr                   ! the approximate value of W(X)

    ! local variables
    real :: an2
    real :: an3
    real :: an4
    real :: an5
    real :: an6
    real :: c13
    real :: c23
    real :: d12
    real :: delx
    real :: em
    real :: em2
    real :: em9
    real :: eta

    integer :: i
    integer :: init
    integer :: m
    integer :: nbits
    integer :: niter

    real :: reta
    real :: s2
    real :: s21
    real :: s22
    real :: s23
    real :: t
    real :: tb
    real :: tb2
    real :: temp
    real :: temp2
    real :: ts
    real :: x0
    real :: x1
    real :: xx
    real :: zl
    real :: zn

    save an3
    save an4
    save an5
    save an6
    save c13
    save c23
    save d12
    save em
    save em2
    save em9
    save init
    save nbits
    save niter
    save s2
    save s21
    save s22
    save s23
    save tb
    save tb2
    save x0
    save x1

    data init / 0 /
    data niter / 1 /

    out_wapr = 0.0
    nerror = 0

    if ( init == 0 ) then

      init = 1

      call nbits_compute( nbits )

      if ( 56 <= nbits ) then
        niter = 2
      end if
      !
      !  Various mathematical constants.
      !
      em = -exp ( -1.0 )
      em9 = -exp ( -9.0 )
      c13 = 1.0 / 3.0
      c23 = 2.0 * c13
      em2 = 2.0 / em
      d12 = -em2
      tb = 0.5 ** nbits
      tb2 = sqrt ( tb )
      x0 = tb ** ( 1.0 / 6.0 ) * 0.5
      x1 = ( 1.0 - 17.0 * tb ** ( 2.0 / 7.0 ) ) * em
      an3 = 8.0 / 3.0
      an4 = 135.0 / 83.0
      an5 = 166.0 / 39.0
      an6 = 3167.0 / 3549.0
      s2 = sqrt ( 2.0 )
      s21 = 2.0 * s2 - 3.0
      s22 = 4.0 - 3.0 * s2
      s23 = s2 - 2.0

    end if

    if ( l == 1 ) then

      delx = x

      if ( delx < 0.0 ) then
        nerror = 1
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'WAPR - Fatal error!'
        write ( *, '(a)' ) '  The offset X is negative.'
        write ( *, '(a)' ) '  It must be nonnegative.'
        stop 1
      end if

      xx = x + em

    else

      if ( x < em ) then
        nerror = 1
        return
      else if ( x == em ) then
        out_wapr = -1.0
        return
      end if

      xx = x
      delx = xx - em

    end if

    if ( branch == 0 ) then
      !
      !  Calculations for Wp.
      !
      if ( abs ( xx ) <= x0 ) then
        out_wapr = xx / ( 1.0 + xx / ( 1.0 + xx &
          / ( 2.0 + xx / ( 0.6 + 0.34 * xx ))))
        return
      else if ( xx <= x1 ) then
        reta = sqrt ( d12 * delx )
        out_wapr = reta / ( 1.0 + reta / ( 3.0 + reta / ( reta &
          / ( an4 + reta / ( reta * an6 + an5 ) ) + an3 ) ) ) &
          - 1.0
        return
      else if ( xx <= 20.0 ) then
        reta = s2 * sqrt ( 1.0 - xx / em )
        an2 = 4.612634277343749 * sqrt ( sqrt ( reta + &
          1.09556884765625 ))
        out_wapr = reta / ( 1.0 + reta / ( 3.0 + ( s21 * an2 &
          + s22 ) * reta / ( s23 * ( an2 + reta )))) - 1.0
      else
        zl = log ( xx )
        out_wapr = log ( xx / log ( xx &
          / zl ** exp ( -1.124491989777808 / &
          ( 0.4225028202459761 + zl ))))
      end if
    !
    !  Calculations for Wm.
    !
    else

      if ( 0.0 <= xx ) then
        nerror = 1
        return
      else if ( xx <= x1 ) then
        reta = sqrt ( d12 * delx )
        out_wapr = reta / ( reta / ( 3.0 + reta / ( reta / ( an4 &
          + reta / ( reta * an6 - an5 ) ) - an3 ) ) - 1.0 ) - 1.0
        return
      else if ( xx <= em9 ) then
        zl = log ( -xx )
        t = -1.0 - zl
        ts = sqrt ( t )
        out_wapr = zl - ( 2.0 * ts ) / ( s2 + ( c13 - t &
          / ( 270.0 + ts * 127.0471381349219 )) * ts )
      else
        zl = log ( -xx )
        eta = 2.0 - em2 * xx
        out_wapr = log ( xx / log ( -xx / ( ( 1.0 &
          - 0.5043921323068457 * ( zl + 1.0 ) ) &
          * ( sqrt ( eta ) + eta / 3.0 ) + 1.0 )))
      end if

    end if

    do i = 1, niter
      zn = log ( xx / out_wapr ) - out_wapr
      temp = 1.0 + out_wapr
      temp2 = temp + c23 * zn
      temp2 = 2.0 * temp * temp2
      out_wapr = out_wapr * ( 1.0 + ( zn / temp ) * ( temp2 - zn ) &
        / ( temp2 - 2.0 * zn ) )
    end do

    return
  end function calc_wapr


  subroutine nbits_compute( nbits )
    !*****************************************************************************80
    !
    !! NBITS_COMPUTE computes the mantissa length minus one.
    !
    !  Discussion:
    !
    !    NBITS is the number of bits (less 1) in the mantissa of the
    !    floating point number number representation of your machine.
    !    It is used to determine the level of accuracy to which the W
    !    function should be calculated.
    !
    !    Most machines use a 24-bit matissa for single precision and
    !    53-56 bits for real. The IEEE standard is 53
    !    bits. The Fujitsu VP2200 uses 56 bits. Long word length
    !    machines vary, e.g., the Cray X/MP has a 48-bit mantissa for
    !    single precision.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    15 June 2014
    !
    !  Author:
    !
    !    Original FORTRAN77 version by Andrew Barry, S. J. Barry, 
    !    Patricia Culligan-Hensley.
    !    This FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    Andrew Barry, S. J. Barry, Patricia Culligan-Hensley,
    !    Algorithm 743: WAPR - A Fortran routine for calculating real 
    !    values of the W-function,
    !    ACM Transactions on Mathematical Software,
    !    Volume 21, Number 2, June 1995, pages 172-181.
    !
    !  Parameters:
    !
    !    Output, integer NBITS, the mantissa length, in bits, 
    !    minus one.
    !
    !----------------------------------------------------------------
    integer, intent(out) :: nbits
    real                 :: b
    integer              :: i
    real                 :: v

    nbits = 0

    b = 1.0

    do

      b = b / 2.0
      v = b + 1.0

      if ( v == 1.0 ) then
        return
      end if

      nbits = nbits + 1

    end do
    
    return
  end subroutine nbits_compute

end module md_lambertw
