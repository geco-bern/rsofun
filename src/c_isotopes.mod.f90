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

  subroutine c_isotopes( tile, tile_fluxes, d13c_atmosphere )
    !/////////////////////////////////////////////////////////////////////////
    ! Keeps track of 13C isotopic signature in "virtual leaf biomass"
    !-------------------------------------------------------------------------
    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(in) :: tile_fluxes
    real :: d13c_atmosphere

    ! local variables
    real, parameter :: k_decay_leaf = 2.0 / ndayyear
    real, parameter :: ntoc_leaf = 30  ! not used, but required to construct an orgpool
    integer :: pft, lu
    real :: f_decay_leaf  ! pool turnover fraction
    real :: d13c_gpp      ! delta-13C isotopic signature from GPP (permil)
    type(orgpool) :: decay_pleaf

    ! avoid numerical instability
    f_decay_leaf = 1.0 - exp( -k_decay_leaf )

    pftloop: do pft=1,npft

      lu = params_pft_plant(pft)%lu_category

      ! Calculate isotopic 13C signature of recent assimilates, given atmospheric 13C signature and discrimination (bigdelta)
      ! Discrimination is calculated as a function of ci:ca (chi) in gpp().
      ! Exact equation:
      d13c_gpp = (d13c_atmosphere - tile_fluxes(lu)%plant(pft)%bigdelta) / (tile_fluxes(lu)%plant(pft)%bigdelta / 1000.0 + 1.0)

      ! Approximative equation:
      ! d13c_gpp = d13c_atmosphere - tile_fluxes(lu)%plant(pft)%bigdelta 

      ! get biomass turnover of virtual leaf biomass, no change in isotopic signature
      decay_pleaf = tile(lu)%plant(pft)%pleaf * f_decay_leaf

      ! subtract biomass turnover from pool, no isotopic discrimination
      tile(lu)%plant(pft)%pleaf = tile(lu)%plant(pft)%pleaf - decay_pleaf

      ! add GPP to virtual leaf biomass (no respiration), keeping track of isotopic signature
      tile(lu)%plant(pft)%pleaf = tile(lu)%plant(pft)%pleaf + &
        orgpool( &
          tile_fluxes(lu)%plant(pft)%dgpp, &
          d13c_gpp, &
          tile_fluxes(lu)%plant(pft)%dgpp * ntoc_leaf &
          )

    end do pftloop

  end subroutine c_isotopes

end module md_c_isotopes
