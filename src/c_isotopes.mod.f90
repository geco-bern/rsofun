module md_c_isotopes
  !////////////////////////////////////////////////////////////////
  ! Dynamics of "virtual leaf biomass" and isotopic composition
  !----------------------------------------------------------------
  use md_tile_pmodel
  use md_plant_pmodel
  use md_params_core
  use md_orgpool

  implicit none

  private
  public c_isotopes

contains

  subroutine c_isotopes( tile, tile_fluxes )
    !/////////////////////////////////////////////////////////////////////////
    ! Keeps track of 13C isotopic signature in "virtual leaf biomass"
    !-------------------------------------------------------------------------
    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(in) :: tile_fluxes

    ! local variables
    real, parameter :: k_decay_leaf = 2.0 / ndayyear
    integer :: pft, lu
    type(orgpool) :: gpp_as_pool


    pftloop: do pft=1,npft

      lu = params_pft_plant(pft)%lu_category

      gpp_as_pool = orgpool( &
        tile_fluxes(lu)%plant(pft)%dgpp, &
        tile_fluxes(lu)%plant(pft)%d13c_gpp, &
        tile_fluxes(lu)%plant(pft)%dgpp * tile(lu)%plant(pft)%r_ntoc_leaf &
      ) ! NOTE: TODO: this could be used to define dgpp::orgpool instead of dgpp::Real

      ! define biomass turnover of virtual leaf biomass (using k_decay_leaf), no change in isotopic signature

      ! Update pleaf mass and isotopic signature
      tile(lu)%plant(pft)%pleaf = &
        ! subtract biomass turnover from pool
        tile(lu)%plant(pft)%pleaf * exp( -k_decay_leaf ) + & ! no isotopic discr.
        ! add GPP to virtual leaf biomass (no respiration), keeping track of isotopic signature
        gpp_as_pool

    end do pftloop

  end subroutine c_isotopes

end module md_c_isotopes
