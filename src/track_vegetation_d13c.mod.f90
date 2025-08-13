module md_track_vegetation_d13c
  !////////////////////////////////////////////////////////////////
  ! Dynamics of "virtual leaf biomass" and isotopic composition
  !----------------------------------------------------------------
  use md_tile_pmodel
  use md_plant_pmodel
  use md_params_core
  use md_orgpool

  implicit none

  private
  public track_vegetation_d13c

contains

  subroutine track_vegetation_d13c( tile, tile_fluxes )
    !/////////////////////////////////////////////////////////////////////////
    ! Keeps track of 13C isotopic signature in "virtual leaf biomass"
    !-------------------------------------------------------------------------
    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(in) :: tile_fluxes

    ! local variables
    real, parameter :: k_decay_leaf = 2.0 / ndayyear
    integer :: pft, lu

    ! Update pleaf mass and isotopic signature, allocate all GPP to leaves, no respiration
    pftloop: do pft=1,npft

      lu = params_pft_plant(pft)%lu_category    ! get correct index

      ! define biomass turnover of virtual leaf biomass (using k_decay_leaf), no change in isotopic signature

      ! subtract biomass turnover from pool, no change in isotopic signature
      tile(lu)%plant(pft)%pleaf = tile(lu)%plant(pft)%pleaf * exp( -k_decay_leaf )

      ! add NPP to virtual leaf biomass (no respiration), keeping track of isotopic signature
      tile(lu)%plant(pft)%pleaf = tile(lu)%plant(pft)%pleaf + &
        orgpool(                               &
          tile_fluxes(lu)%plant(pft)%dgpp,     & ! no respiration, so only GPP considered
          tile_fluxes(lu)%plant(pft)%d13c_gpp, &
          tile_fluxes(lu)%plant(pft)%dgpp * tile(lu)%plant(pft)%r_ntoc_leaf &
        )

    end do pftloop

  end subroutine track_vegetation_d13c

end module md_track_vegetation_d13c
