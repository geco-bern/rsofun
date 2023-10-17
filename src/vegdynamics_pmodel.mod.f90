module md_vegdynamics_pmodel
  !////////////////////////////////////////////////////////////////
  ! Vegetation cover definitions for P-model simulations.
  !---------------------------------------------------------------
  implicit none

  private
  public vegdynamics

contains

  subroutine vegdynamics( tile, fapar_prescr, fpc_grid_prescr )
    !//////////////////////////////////////////////////////////////////
    ! Updates canopy and tile variables
    !------------------------------------------------------------------
    use md_params_core, only: npft, nlu, nmonth, dummy
    use md_tile_pmodel, only: tile_type
    
    ! arguments
    type( tile_type ), dimension(nlu), intent(inout) :: tile

    ! arguments (may be dummy)
    real, intent(in) :: fapar_prescr
    real, dimension(npft), intent(in) :: fpc_grid_prescr

    ! local variables
    integer :: pft, lu
    
    do lu=1,nlu
      !------------------------------------------------------------------
      ! Add individuals
      !------------------------------------------------------------------
      do pft=1,npft

        ! Override interactively simulated fAPAR and foliar projective cover with data
        ! if (sum(fpc_grid_prescr(:))==0.0) print*,'sum of fpc_grid',sum(fpc_grid_prescr(:))

        if (fapar_prescr/=dummy) tile(lu)%canopy%fapar = fapar_prescr
        
        tile(lu)%plant(pft)%fpc_grid = fpc_grid_prescr(pft)

        ! ! get annually updated leaf traits (vary because of variations in light and CO2)
        ! call get_leaftraits( plant(pft), solar%meanmppfd(:), out_pmodel(pft,:)%actnv_unitiabs )

      end do
    end do
  
  end subroutine vegdynamics


end module md_vegdynamics_pmodel
