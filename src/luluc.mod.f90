module md_luluc
    !////////////////////////////////////////////////////////////////
    ! Module containing data structures and logic adding Land use &
    ! land use change capabilities to biomee.
    !----------------------------------------------------------------
    implicit none
    private
    public :: lu_state
    public :: update_lu_fractions, populate_outarray_annual_land_use

    integer, public, parameter :: nvars_lu_out         = 2

    type lu_state

        real :: fraction
    
    contains
        
        procedure non_empty
        
    end type lu_state

  contains
      
      pure logical function non_empty(self)
          class(lu_state), intent(in) :: self

          non_empty = self%fraction > 0.0
        
      end function non_empty
        
      function update_lu_fractions(lu_fractions, luc_forcing, tiles) result(export)
      !////////////////////////////////////////////////////////////////
      ! Update the LU fractions 'lu_fractions' by applying the LU transitions
      ! defined in the square matrix 'luc_forcing'.
      ! Returns the exported organic material
      !----------------------------------------------------------------
  
          use vegetation_tile_biomee
          ! Arguments
          real, intent(in), dimension(:, :) :: luc_forcing ! n_lu * n_lu matrix
          real, intent(inout), dimension(:) :: lu_fractions
          type(vegn_tile_type), intent(inout), dimension(:), target :: tiles
          type(orgpool) :: export

  
          ! Local variables
          integer :: i, j
          real :: delta
          type(orgpool), dimension(size(lu_fractions), 4) :: transfer ! n_lu * 4 matrix of orgpools
          real, dimension(size(lu_fractions), MAX_LEVELS) :: transfer_water
          real, dimension(size(lu_fractions)) :: old_lu_fractions, received, lost
          type(cohort_type), pointer :: cc
          type(cohort_item), pointer :: it
          type(vegn_tile_type), pointer :: vegn
          integer, parameter :: PMICR    = 1
          integer, parameter :: INORG    = 2
          integer, parameter :: PSOIL_FS = 3
          integer, parameter :: PSOIL_SL = 4
  
          transfer(:, :) = orgpool()
          export = orgpool()
          old_lu_fractions = lu_fractions
          received = 0
          lost = 0
  
          ! For each transition
          ! i is the source tile
          ! j is the receiving tile
          do i = 1, size(lu_fractions)
              do j = 1, size(lu_fractions)
                  delta = luc_forcing(i, j)
                  if (delta <= 0) cycle
                  vegn => tiles(i)
                  ! If there is a transition from i to j, we save the transfered pools in 'transfer'
                  transfer(j, PMICR) = transfer(j, PMICR) + vegn%pmicr * delta
                  transfer(j, INORG) = transfer(j, INORG) + vegn%inorg * delta
                  transfer(j, PSOIL_SL) = transfer(j, PSOIL_SL) + vegn%psoil_sl * delta
                  ! Transfer to litter pool
                  transfer(j, PSOIL_FS) = &
                          transfer(j, PSOIL_FS) + (vegn%psoil_fs + vegn%proot + vegn%pseed + vegn%plabl + vegn%pleaf) &
                          * delta * (1.0 - inputs%init_lu(i)%oxidized_litter_fraction)
                  ! We export above ground pools (= products)
                  export = export + (vegn%pwood + vegn%psapw) * delta
                  ! Water transfer
                  transfer_water(j, :) = transfer_water(j, :) + vegn%wcl(:) * delta
                  ! We keep track of the deltas
                  received(j) = received(j) + delta
                  lost(i) = lost(i) + delta
              end do
          end do
  
          ! We update the fractions using the deltas
          lu_fractions = lu_fractions + received - lost
  
          ! For each LU
          do j = 1, size(lu_fractions)
              vegn => tiles(j)
  
              if (lu_fractions(j) < eps) then
                  ! If the fraction is almost zero (or negative), we kill the tile
                  lu_fractions(j) = 0.0
                  call vegn%shut_down()
                  cycle
              end if
  
              if (old_lu_fractions(j) <= 0 .and. lu_fractions(j) > 0) then
                  ! If a tile had null fraction and has now non-null, we initialize it.
                  call vegn%initialize_vegn_tile(j)
              else
                  it => vegn%cohorts()
                  do while (associated(it))
                      cc => it%cohort
                      ! The cohort densities conveniently stir all the org pools so we just need to scale it to scale the pools
                      ! The formula is the same as below except that the transfer term is null.
                      cc%density = (cc%density   * (old_lu_fractions(j) - lost(j))) / lu_fractions(j)
                      it => it%next()
                  end do
                  call vegn%aggregate_cohorts() ! We aggregate to get the right density
              end if
  
              ! Subtract the lost quantities, add transfered ones, and normalize with the new fraction
              vegn%pmicr    = (vegn%pmicr    * (old_lu_fractions(j) - lost(j)) + transfer(j, PMICR))    / lu_fractions(j)
              vegn%inorg    = (vegn%inorg    * (old_lu_fractions(j) - lost(j)) + transfer(j, INORG))    / lu_fractions(j)
              vegn%psoil_fs = (vegn%psoil_fs * (old_lu_fractions(j) - lost(j)) + transfer(j, PSOIL_FS)) / lu_fractions(j)
              vegn%psoil_sl = (vegn%psoil_sl * (old_lu_fractions(j) - lost(j)) + transfer(j, PSOIL_SL)) / lu_fractions(j)
              vegn%wcl(:)   = (vegn%wcl(:)   * (old_lu_fractions(j) - lost(j)) + transfer_water(j, :))  / lu_fractions(j)
          end do

      end function update_lu_fractions
  
      subroutine populate_outarray_annual_land_use(year, lu_fractions, output_annual_luluc_tile)
          use, intrinsic :: iso_fortran_env, dp=>real64
  
          ! Arguments
          integer, intent(in)                          :: year
          real, intent(in), dimension(:)               :: lu_fractions
          real(kind=dp), intent(out), dimension(:,:)   :: output_annual_luluc_tile
  
          output_annual_luluc_tile(1,:) = dble(year)
          output_annual_luluc_tile(2,:) = dble(lu_fractions)
  
      end subroutine populate_outarray_annual_land_use

end module md_luluc