module md_aggregated_tile_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing aggregated tile for BiomeE
  !----------------------------------------------------------------
  use md_lu_tile_biomee
  use md_product_pools
  use md_orgpool
  use md_params_core, only: eps
  use md_interface_in_biomee, only: inputs, MAX_LEVELS
  use md_cohort
  use md_cohort_linked_list, only: cohort_item

  implicit none

  private
  public :: aggregated_tile

  integer, public, parameter :: nvars_lu_out = 2

  type aggregated_tile

    type(lu_tile), allocatable, dimension(:) :: tiles
    type(product_pools) :: prod_pools

    contains

      procedure initialize
      procedure shut_down
      procedure update_lu_fractions
      procedure populate_outarray_annual_aggregated
      procedure n_lu

  end type aggregated_tile

contains

  pure integer function n_lu(self)
    class(aggregated_tile), intent(in) :: self

    n_lu = size(self%tiles)

  end function n_lu

  subroutine initialize(self, lu_fractions)
    class(aggregated_tile), intent(inout) :: self
    real, dimension(:) :: lu_fractions

    ! Local variables
    integer :: nb_lu, lu_idx

    nb_lu = size(lu_fractions)

    allocate(self%tiles(nb_lu))

    ! Initialize tiles
    do lu_idx = 1, nb_lu
      associate(lu => self%tiles(lu_idx))
        lu%fraction = lu_fractions(lu_idx)
        if (lu%non_empty()) call lu%vegn%initialize_vegn_tile(lu_idx)
      end associate
    end do
  end subroutine initialize

  subroutine shut_down(self)
    class(aggregated_tile), intent(inout) :: self

    ! Local variable
    integer :: lu_idx

    do lu_idx = 1, self%n_lu()
      call self%tiles(lu_idx)%shut_down()
    end do
    deallocate(self%tiles)
  end subroutine shut_down

  subroutine update_lu_fractions(self, luc_forcing)
    !////////////////////////////////////////////////////////////////
    ! Update the LU fractions 'lu_fractions' by applying the LU transitions
    ! defined in the square matrix 'luc_forcing'.
    !----------------------------------------------------------------

    ! Arguments
    class(aggregated_tile), intent(inout) :: self
    real, intent(in), dimension(:, :) :: luc_forcing ! n_lu * n_lu matrix
    type(orgpool) :: export


    ! Local variables
    integer :: i, j
    real :: delta
    type(orgpool), dimension(self%n_lu(), 4) :: transfer ! n_lu * 4 matrix of orgpools
    real, dimension(self%n_lu(), MAX_LEVELS) :: transfer_water
    real, dimension(self%n_lu()) :: old_lu_fractions, lu_fractions, received, lost
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it
    integer, parameter :: PMICR    = 1
    integer, parameter :: INORG    = 2
    integer, parameter :: PSOIL_FS = 3
    integer, parameter :: PSOIL_SL = 4

    transfer(:, :) = orgpool()
    transfer_water(:, :) = 0.0
    export = orgpool()
    lu_fractions = self%tiles(:)%fraction
    old_lu_fractions = lu_fractions
    received = 0
    lost = 0

    ! For each transition
    ! i is the source tile
    ! j is the receiving tile
    do i = 1, self%n_lu()
      do j = 1, self%n_lu()
        delta = luc_forcing(i, j)
        if (delta <= 0) cycle
        associate(lu => self%tiles(i))
          ! If there is a transition from i to j, we save the transfered pools in 'transfer'
          transfer(j, PMICR) = transfer(j, PMICR) + lu%vegn%pmicr * delta
          transfer(j, INORG) = transfer(j, INORG) + lu%vegn%inorg * delta
          transfer(j, PSOIL_SL) = transfer(j, PSOIL_SL) + lu%vegn%psoil_sl * delta
          ! Transfer to litter pool
          transfer(j, PSOIL_FS) = transfer(j, PSOIL_FS) &
                          + (lu%vegn%psoil_fs + lu%vegn%proot + lu%vegn%pseed + lu%vegn%plabl + lu%vegn%pleaf) &
                          * delta * (1.0 - inputs%init_lu(i)%oxidized_litter_fraction)
          ! We export above ground pools (= products)
          export = export + (lu%vegn%pwood + lu%vegn%psapw) * delta
          ! Water transfer
          transfer_water(j, :) = transfer_water(j, :) + lu%vegn%wcl(:) * delta
          ! We keep track of the deltas
          received(j) = received(j) + delta
          lost(i) = lost(i) + delta

        end associate
      end do
    end do

    ! We update the fractions using the deltas
    lu_fractions = lu_fractions + received - lost

    ! For each LU
    do j = 1, self%n_lu()
      associate(lu => self%tiles(j))

        if (lu_fractions(j) < eps) then
          ! If the fraction is almost zero (or negative), we kill the tile
          lu_fractions(j) = 0.0
          call lu%shut_down()
          cycle
        end if

        if (old_lu_fractions(j) <= 0 .and. lu_fractions(j) > 0) then
          ! If a tile had null fraction and has now non-null, we initialize it.
          call lu%vegn%initialize_vegn_tile(j)
        else
          it => lu%vegn%cohorts()
          do while (associated(it))
            cc => it%cohort
            ! The cohort densities conveniently stir all the org pools so we just need to scale it to scale the pools
            ! The formula is the same as below except that the transfer term is null.
            cc%density = (cc%density   * (old_lu_fractions(j) - lost(j))) / lu_fractions(j)
            it => it%next()
          end do
          call lu%vegn%aggregate_cohorts() ! We aggregate to get the right density
        end if

        ! Subtract the lost quantities, add transfered ones, and normalize with the new fraction
        lu%vegn%pmicr    = (lu%vegn%pmicr    * (old_lu_fractions(j) - lost(j)) + transfer(j, PMICR))    / lu_fractions(j)
        lu%vegn%inorg    = (lu%vegn%inorg    * (old_lu_fractions(j) - lost(j)) + transfer(j, INORG))    / lu_fractions(j)
        lu%vegn%psoil_fs = (lu%vegn%psoil_fs * (old_lu_fractions(j) - lost(j)) + transfer(j, PSOIL_FS)) / lu_fractions(j)
        lu%vegn%psoil_sl = (lu%vegn%psoil_sl * (old_lu_fractions(j) - lost(j)) + transfer(j, PSOIL_SL)) / lu_fractions(j)
        lu%vegn%wcl(:)   = (lu%vegn%wcl(:)   * (old_lu_fractions(j) - lost(j)) + transfer_water(j, :))  / lu_fractions(j)
        lu%fraction = lu_fractions(j)

      end associate
    end do

    call self%prod_pools%update(export)

  end subroutine update_lu_fractions

  subroutine populate_outarray_annual_aggregated(self, year, output_annual_aggregated)
    use, intrinsic :: iso_fortran_env, dp=>real64

    ! Arguments
    class(aggregated_tile), intent(inout) :: self
    integer, intent(in)                          :: year
    real(kind=dp), intent(out), dimension(:)   :: output_annual_aggregated

    ! Local variable
    integer :: lu_idx
    real :: tmp

    tmp = 0.0

    do lu_idx = 1, self%n_lu()
      associate(lu => self%tiles(lu_idx))

      tmp = tmp + lu%fraction

      end associate

    end do

    output_annual_aggregated(1) = dble(year)
    output_annual_aggregated(2) = dble(tmp)

  end subroutine populate_outarray_annual_aggregated

end module md_aggregated_tile_biomee
