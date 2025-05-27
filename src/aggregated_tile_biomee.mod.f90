module md_aggregated_tile_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing aggregated tile for BiomeE
  !----------------------------------------------------------------
  use md_lu_tile_biomee
  use md_product_pools
  use md_orgpool
  use md_params_core, only: eps
  use md_interface_in_biomee, only: inputs, MAX_LEVELS
  use md_interface_out_biomee
  use md_cohort
  use md_cohort_linked_list, only: cohort_stack_item

  implicit none

  private
  public :: aggregated_tile

  type aggregated_tile

    type(lu_tile), allocatable, dimension(:) :: tiles
    type(product_pools) :: prod_pools

    contains

      procedure n_lu
      procedure initialize
      procedure shut_down
      procedure update_lu_fractions
      procedure populate_outarrays
      procedure populate_outcohorts
      procedure populate_outdaily

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

  function update_lu_fractions(self, luc_forcing) result(export)
    !////////////////////////////////////////////////////////////////
    ! Update the LU fractions 'lu_fractions' by applying the LU transitions
    ! defined in the square matrix 'luc_forcing'.
    ! Returns the organic matter to be added to the product pools
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
    type(cohort_stack_item), pointer :: it
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
          ! NOTE: since 'pwood','psapw' are in kg C / m2 of tile
          !       and 'delta' is in m2 tile per m2 of landscape (or cell)
          !       'export' is kg C / m2 of landscape
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

  end function update_lu_fractions

  subroutine populate_outarrays(self, output_annual_aggregated, output_annual_tiles)
    use, intrinsic :: iso_fortran_env, dp=>real64

    ! Arguments
    class(aggregated_tile), intent(in)             :: self
    real(kind=dp), dimension(:), intent(out)       :: output_annual_aggregated
    real(kind=dp), dimension(:, :), intent(out)    :: output_annual_tiles

    ! Local variable
    integer :: lu_idx, year
    real    :: tot_fraction, max_age, max_volume, max_dbh
    real, dimension(nvars_annual_tile)    :: agg_array
    type(orgpool) :: prod_pool

    agg_array(:) = 0
    year = 0
    tot_fraction = 0
    max_age = 0
    max_volume = 0
    max_dbh = 0

    do lu_idx = 1, self%n_lu()
      associate(lu => self%tiles(lu_idx))
        ! If empty tile, skip
        if (.not. lu%non_empty()) then
          output_annual_tiles(ANNUAL_TILE_LU_FRACTION, lu_idx) = 0.0
          cycle
        end if

        year = int(lu%vegn%out_annual_tile(ANNUAL_TILE_YEAR))

        !===== Fill annual tiles
        output_annual_tiles(:, lu_idx) = dble(lu%vegn%out_annual_tile(:))
        ! We update the fraction manually as it is not updated in out_annual_tile
        output_annual_tiles(ANNUAL_TILE_LU_FRACTION, lu_idx) = lu%fraction

        !===== Fill aggregated output
        tot_fraction = tot_fraction + lu%fraction
        max_age = MAX(max_age, lu%vegn%out_annual_tile(ANNUAL_TILE_MAX_AGE))
        max_volume = MAX(max_age, lu%vegn%out_annual_tile(ANNUAL_TILE_MAX_VOLUME))
        max_dbh = MAX(max_age, lu%vegn%out_annual_tile(ANNUAL_TILE_MAX_DBH))
        agg_array = agg_array + lu%vegn%out_annual_tile * lu%fraction

      end associate

    end do

    output_annual_aggregated(ANNUAL_TILE_YEAR              ) = dble(year)
    output_annual_aggregated(ANNUAL_TILE_CAI               ) = dble(agg_array(ANNUAL_TILE_CAI            ))
    output_annual_aggregated(ANNUAL_TILE_LAI               ) = dble(agg_array(ANNUAL_TILE_LAI            ))
    output_annual_aggregated(ANNUAL_TILE_DENSITY           ) = dble(agg_array(ANNUAL_TILE_DENSITY        ))
    output_annual_aggregated(ANNUAL_TILE_DBH               ) = dble(agg_array(ANNUAL_TILE_DBH            ))
    output_annual_aggregated(ANNUAL_TILE_DENSITY12         ) = dble(agg_array(ANNUAL_TILE_DENSITY12      ))
    output_annual_aggregated(ANNUAL_TILE_DBH12             ) = dble(agg_array(ANNUAL_TILE_DBH12          ))
    output_annual_aggregated(ANNUAL_TILE_QMD12             ) = dble(agg_array(ANNUAL_TILE_QMD12          ))
    output_annual_aggregated(ANNUAL_TILE_NPP               ) = dble(agg_array(ANNUAL_TILE_NPP            ))
    output_annual_aggregated(ANNUAL_TILE_GPP               ) = dble(agg_array(ANNUAL_TILE_GPP            ))
    output_annual_aggregated(ANNUAL_TILE_RESP              ) = dble(agg_array(ANNUAL_TILE_RESP           ))
    output_annual_aggregated(ANNUAL_TILE_RH                ) = dble(agg_array(ANNUAL_TILE_RH             ))
    output_annual_aggregated(ANNUAL_TILE_PRCP              ) = dble(agg_array(ANNUAL_TILE_PRCP           ))
    output_annual_aggregated(ANNUAL_TILE_SOIL_W            ) = dble(agg_array(ANNUAL_TILE_SOIL_W         ))
    output_annual_aggregated(ANNUAL_TILE_TRSP              ) = dble(agg_array(ANNUAL_TILE_TRSP           ))
    output_annual_aggregated(ANNUAL_TILE_EVAP              ) = dble(agg_array(ANNUAL_TILE_EVAP           ))
    output_annual_aggregated(ANNUAL_TILE_RUNOFF            ) = dble(agg_array(ANNUAL_TILE_RUNOFF         ))
    output_annual_aggregated(ANNUAL_TILE_PLANT_C           ) = dble(agg_array(ANNUAL_TILE_PLANT_C        ))
    output_annual_aggregated(ANNUAL_TILE_SOIL_C            ) = dble(agg_array(ANNUAL_TILE_SOIL_C         ))
    output_annual_aggregated(ANNUAL_TILE_PLANT_N           ) = dble(agg_array(ANNUAL_TILE_PLANT_N        ))
    output_annual_aggregated(ANNUAL_TILE_SOIL_N            ) = dble(agg_array(ANNUAL_TILE_SOIL_N         ))
    output_annual_aggregated(ANNUAL_TILE_TOT_N             ) = dble(agg_array(ANNUAL_TILE_TOT_N          ))
    output_annual_aggregated(ANNUAL_TILE_NS_C              ) = dble(agg_array(ANNUAL_TILE_NS_C           ))
    output_annual_aggregated(ANNUAL_TILE_SEED_C            ) = dble(agg_array(ANNUAL_TILE_SEED_C         ))
    output_annual_aggregated(ANNUAL_TILE_LEAF_C            ) = dble(agg_array(ANNUAL_TILE_LEAF_C         ))
    output_annual_aggregated(ANNUAL_TILE_ROOT_C            ) = dble(agg_array(ANNUAL_TILE_ROOT_C         ))
    output_annual_aggregated(ANNUAL_TILE_SW_C              ) = dble(agg_array(ANNUAL_TILE_SW_C           ))
    output_annual_aggregated(ANNUAL_TILE_HW_C              ) = dble(agg_array(ANNUAL_TILE_HW_C           ))
    output_annual_aggregated(ANNUAL_TILE_NSN               ) = dble(agg_array(ANNUAL_TILE_NSN            ))
    output_annual_aggregated(ANNUAL_TILE_SEED_N            ) = dble(agg_array(ANNUAL_TILE_SEED_N         ))
    output_annual_aggregated(ANNUAL_TILE_LEAF_N            ) = dble(agg_array(ANNUAL_TILE_LEAF_N         ))
    output_annual_aggregated(ANNUAL_TILE_ROOT_N            ) = dble(agg_array(ANNUAL_TILE_ROOT_N         ))
    output_annual_aggregated(ANNUAL_TILE_SW_N              ) = dble(agg_array(ANNUAL_TILE_SW_N           ))
    output_annual_aggregated(ANNUAL_TILE_HW_N              ) = dble(agg_array(ANNUAL_TILE_HW_N           ))
    output_annual_aggregated(ANNUAL_TILE_MCRB_C            ) = dble(agg_array(ANNUAL_TILE_MCRB_C         ))
    output_annual_aggregated(ANNUAL_TILE_FASTSOM           ) = dble(agg_array(ANNUAL_TILE_FASTSOM        ))
    output_annual_aggregated(ANNUAL_TILE_SLOWSOM           ) = dble(agg_array(ANNUAL_TILE_SLOWSOM        ))
    output_annual_aggregated(ANNUAL_TILE_MCRB_N            ) = dble(agg_array(ANNUAL_TILE_MCRB_N         ))
    output_annual_aggregated(ANNUAL_TILE_FS_N              ) = dble(agg_array(ANNUAL_TILE_FS_N           ))
    output_annual_aggregated(ANNUAL_TILE_SL_N              ) = dble(agg_array(ANNUAL_TILE_SL_N           ))
    output_annual_aggregated(ANNUAL_TILE_INORG_N           ) = dble(agg_array(ANNUAL_TILE_INORG_N        ))
    output_annual_aggregated(ANNUAL_TILE_N_FIX             ) = dble(agg_array(ANNUAL_TILE_N_FIX          ))
    output_annual_aggregated(ANNUAL_TILE_N_UPTK            ) = dble(agg_array(ANNUAL_TILE_N_UPTK         ))
    output_annual_aggregated(ANNUAL_TILE_NYRMIN            ) = dble(agg_array(ANNUAL_TILE_NYRMIN         ))
    output_annual_aggregated(ANNUAL_TILE_NP2S              ) = dble(agg_array(ANNUAL_TILE_NP2S           ))
    output_annual_aggregated(ANNUAL_TILE_NLOSS             ) = dble(agg_array(ANNUAL_TILE_NLOSS          ))
    output_annual_aggregated(ANNUAL_TILE_TOTSEED_C         ) = dble(agg_array(ANNUAL_TILE_TOTSEED_C      ))
    output_annual_aggregated(ANNUAL_TILE_TOTSEED_N         ) = dble(agg_array(ANNUAL_TILE_TOTSEED_N      ))
    output_annual_aggregated(ANNUAL_TILE_SEEDLING_C        ) = dble(agg_array(ANNUAL_TILE_SEEDLING_C     ))
    output_annual_aggregated(ANNUAL_TILE_SEEDLING_N        ) = dble(agg_array(ANNUAL_TILE_SEEDLING_N     ))
    output_annual_aggregated(ANNUAL_TILE_MAX_AGE           ) = dble(max_age)
    output_annual_aggregated(ANNUAL_TILE_MAX_VOLUME        ) = dble(max_volume)
    output_annual_aggregated(ANNUAL_TILE_MAX_DBH           ) = dble(max_dbh)
    output_annual_aggregated(ANNUAL_TILE_NPP_L             ) = dble(agg_array(ANNUAL_TILE_NPP_L          ))
    output_annual_aggregated(ANNUAL_TILE_NPP_W             ) = dble(agg_array(ANNUAL_TILE_NPP_W          ))
    output_annual_aggregated(ANNUAL_TILE_DEADTREES_N       ) = dble(agg_array(ANNUAL_TILE_DEADTREES_N    ))
    output_annual_aggregated(ANNUAL_TILE_DEADTREES_C       ) = dble(agg_array(ANNUAL_TILE_DEADTREES_C    ))
    output_annual_aggregated(ANNUAL_TILE_M_TURNOVER        ) = dble(agg_array(ANNUAL_TILE_M_TURNOVER     ))
    output_annual_aggregated(ANNUAL_TILE_C_TURNOVER_TIME   ) = dble(agg_array(ANNUAL_TILE_C_TURNOVER_TIME))
    output_annual_aggregated(ANNUAL_TILE_LU_FRACTION       ) = dble(tot_fraction)
    prod_pool = self%prod_pools%get_pool(1)
    output_annual_aggregated(AGGREGATED_TILE_PROD_POOL_1_C ) = dble(prod_pool%c12)
    output_annual_aggregated(AGGREGATED_TILE_PROD_POOL_1_N ) = dble(prod_pool%n14)
    prod_pool = self%prod_pools%get_pool(2)
    output_annual_aggregated(AGGREGATED_TILE_PROD_POOL_2_C ) = dble(prod_pool%c12)
    output_annual_aggregated(AGGREGATED_TILE_PROD_POOL_2_N ) = dble(prod_pool%n14)

  end subroutine populate_outarrays

  subroutine populate_outcohorts(self, output_annual_cohorts)
    use, intrinsic :: iso_fortran_env, dp=>real64

    ! Arguments
    class(aggregated_tile), intent(in)             :: self
    real(kind=dp), dimension(:, :, :), intent(out) :: output_annual_cohorts

    ! Local variable
    integer :: lu_idx

    do lu_idx = 1, self%n_lu()
      associate(lu => self%tiles(lu_idx))
        ! If empty tile, skip
        if (lu%non_empty()) then
          output_annual_cohorts(:, :, lu_idx) = dble(lu%vegn%out_annual_cohorts(:, :))
        end if

      end associate

    end do

  end subroutine populate_outcohorts

  subroutine populate_outdaily(self, output_daily_tiles)
    use, intrinsic :: iso_fortran_env, dp=>real64

    ! Arguments
    class(aggregated_tile), intent(in)             :: self
    real(kind=dp), dimension(:, :, :), intent(out) :: output_daily_tiles

    ! Local variable
    integer :: lu_idx

    do lu_idx = 1, self%n_lu()
      associate(lu => self%tiles(lu_idx))
        ! If empty tile, skip
        if (lu%non_empty()) then
          output_daily_tiles(:, :, lu_idx) = dble(lu%vegn%out_daily_tile(:, :))
        end if

      end associate

    end do

  end subroutine populate_outdaily

end module md_aggregated_tile_biomee
