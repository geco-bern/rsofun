module md_luluc
    !////////////////////////////////////////////////////////////////
    ! Module containing data structures and logic adding Land use &
    ! land use change capabilities to biomee.
    !----------------------------------------------------------------
    implicit none
    private
    public :: update_lu_state, populate_outarray_annual_land_use

    integer, public, parameter :: nvars_init_lu        = 1
    integer, public, parameter :: nvars_lu_out         = 2

    !type luluc_state
    !    real :: fraction
    !end type luluc_state

    contains

    subroutine update_lu_state(lu_state, forcing, tiles)
    !////////////////////////////////////////////////////////////////
    ! Update the LU fractions 'lu_state' by applying the LU transitions
    ! defined in the square matrix 'forcing'.
    !----------------------------------------------------------------

        use vegetation_tile_biomee
        ! Arguments
        real, intent(in), dimension(:, :) :: forcing ! n_lu * n_lu matrix
        real, intent(inout), dimension(:) :: lu_state
        type(vegn_tile_type), intent(inout), dimension(:), target :: tiles

        ! Local variables
        integer :: i, j
        real :: delta
        type(orgpool), dimension(size(lu_state), 4) :: transfer ! n_lu * 4 matrix of orgppols
        type(orgpool) :: trash
        real, dimension(size(lu_state)) :: old_lu_state, received, lost
        type(cohort_type), pointer :: cc
        type(cohort_item), pointer :: it
        type(vegn_tile_type), pointer :: vegn
        integer, parameter :: PMICR    = 1
        integer, parameter :: INORG    = 2
        integer, parameter :: PSOIL_FS = 3
        integer, parameter :: PSOIL_SL = 4

        transfer(:, :) = orgpool()
        trash = orgpool()
        old_lu_state = lu_state
        received = 0
        lost = 0

        ! For each transition
        ! i is the source tile
        ! j is the receiving tile
        do i = 1, size(lu_state)
            do j = 1, size(lu_state)
                delta = forcing(i, j)
                if (delta <= 0) cycle
                ! If there is a transition from i to j, we save the trasnfered amounts in 'transfer'
                transfer(j, PMICR) = transfer(j, PMICR) + tiles(i)%pmicr * delta
                transfer(j, INORG) = transfer(j, INORG) + tiles(i)%inorg * delta
                transfer(j, PSOIL_FS) = transfer(j, PSOIL_FS) + tiles(i)%psoil_fs * delta
                transfer(j, PSOIL_SL) = &
                        transfer(j, PSOIL_SL) + (tiles(i)%psoil_sl + tiles(i)%proot + tiles(i)%pseed + tiles(i)%plabl) &
                        * delta
                ! We add to the trash what is taken away (above ground)
                trash = trash + (tiles(i)%plabl + tiles(i)%pleaf + tiles(i)%pseed) * delta
                ! We keep track of the deltas
                received(j) = received(j) + delta
                lost(i) = lost(i) + delta
            end do
        end do

        ! We update the fractions using the deltas
        lu_state = lu_state + received - lost

        ! For each LU
        do j = 1, size(lu_state)
            vegn => tiles(j)

            if (lu_state(j) < eps) then
                ! If the fraction is almost zero (or negative), we kill the tile
                lu_state(j) = 0.0
                cycle
            end if

            if (old_lu_state(j) <= 0 .and. lu_state(j) > 0) then
                ! If a tile had null fraction and has now non-null, we initialize it.
                call vegn%initialize_vegn_tile()
            else
                it => vegn%cohorts()
                do while (associated(it))
                    cc => it%cohort
                    ! The cohort densities conveniently stir all the org pools so we just need to scale it to scale the pools
                    ! The formula is the same as below except that the transfer term is null.
                    cc%density = (cc%density   * (old_lu_state(j) - lost(j))) / lu_state(j)
                    it => it%next()
                end do
                call vegn%aggregate_cohorts() ! We aggregate to get the right density
            end if

            ! Subtract the lost quantities, add transfered ones, and normalize with the new fraction
            vegn%pmicr    = (vegn%pmicr    * (old_lu_state(j) - lost(j)) + transfer(j, PMICR))    / lu_state(j)
            vegn%inorg    = (vegn%inorg    * (old_lu_state(j) - lost(j)) + transfer(j, INORG))    / lu_state(j)
            vegn%psoil_fs = (vegn%psoil_fs * (old_lu_state(j) - lost(j)) + transfer(j, PSOIL_FS)) / lu_state(j)
            vegn%psoil_sl = (vegn%psoil_sl * (old_lu_state(j) - lost(j)) + transfer(j, PSOIL_SL)) / lu_state(j)
        end do

    end subroutine update_lu_state

    subroutine populate_outarray_annual_land_use(year, lu_state, output_annual_luluc_tile)
        use, intrinsic :: iso_fortran_env, dp=>real64

        ! Arguments
        integer, intent(in)                          :: year
        real, intent(in), dimension(:)               :: lu_state
        real(kind=dp), intent(inout), dimension(:,:) :: output_annual_luluc_tile

        output_annual_luluc_tile(1,:) = dble(year)
        output_annual_luluc_tile(2,:) = dble(lu_state)

    end subroutine populate_outarray_annual_land_use

end module md_luluc