module md_luluc
    !////////////////////////////////////////////////////////////////
    ! Module containing data structures and logic adding Land use &
    ! land use change capabilities to biomee.
    !----------------------------------------------------------------
    implicit none
    private
    public :: update_lu_state

    !type luluc_state
    !    real :: fraction
    !end type luluc_state

    contains

        subroutine update_lu_state(lu_state, forcing, n_lu)
        !////////////////////////////////////////////////////////////////
        ! Update the LU fractions 'lu_state' by applying the LU transitions
        ! defined in the square matrix 'forcing'.
        !----------------------------------------------------------------
            implicit none

            ! Arguments
            integer, intent(in) :: n_lu
            real, intent(in), dimension(n_lu, n_lu) :: forcing
            real, intent(inout), dimension(n_lu) :: lu_state

            ! Local variables
            integer :: i, j
            real :: delta

            do i = 1, n_lu
                do j = 1, n_lu
                    delta = forcing(i, j)
                    lu_state(i) = lu_state(i) - delta
                    lu_state(j) = lu_state(j) + delta
                end do
            end do

        end subroutine update_lu_state

end module md_luluc