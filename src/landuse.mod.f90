module md_landuse
  !////////////////////////////////////////////////////////////////
  !----------------------------------------------------------------
  use md_classdefs
  use md_tile
  use md_plant
  use md_params_core
    
  implicit none

  private
  public grharvest

contains

  subroutine grharvest( tile, tile_fluxes, doy )
    !////////////////////////////////////////////////////////////////
    ! Annual grass biomass harvest.
    !----------------------------------------------------------------
    use md_interface_pmodel, only: myinterface

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    integer, intent(in) :: doy

    ! local variables
    type( orgpool ) :: pleaf_init
    type( orgpool ) :: pleaf_turn
    type( orgpool ) :: plabl_turn
    real :: cleaf
    real :: nleaf
    real :: dleaf
    real :: dlabl
    real :: lai_new

    integer :: nitr
    integer :: pft
    integer :: lu

    do pft = 1, npft

      lu = params_pft_plant(pft)%lu_category

      if ( params_pft_plant(pft)%grass .and. myinterface%landuse(doy)%dfharv > 0.0 ) then

        ! number of iterations to match leaf C given leaf N
        nitr = 0

        ! store leaf C and N before turnover
        pleaf_init = tile(lu)%plant(pft)%pleaf

        ! reduce leaf C (given by turnover rate)
        cleaf = tile(lu)%plant(pft)%pleaf%c%c12 * (1.0 - myinterface%landuse(doy)%dfharv)

        ! get new LAI based on cleaf
        lai_new = get_lai( pft, cleaf, tile(lu)%plant(pft)%actnv_unitfapar )

        ! update canopy state (only variable fAPAR so far implemented)
        tile(lu)%plant(pft)%fapar_ind = get_fapar( lai_new )

        ! re-calculate metabolic and structural N, given new LAI and fAPAR
        call update_leaftraits( tile(lu)%plant(pft) )

        ! get updated leaf N
        nleaf = tile(lu)%plant(pft)%narea

        do while ( nleaf > pleaf_init%n%n14 )

          nitr = nitr + 1

          ! reduce leaf C a bit more
          cleaf = cleaf * pleaf_init%n%n14 / nleaf

          ! get new LAI based on cleaf
          lai_new = get_lai( pft, cleaf, tile(lu)%plant(pft)%actnv_unitfapar )

          ! update canopy state (only variable fAPAR so far implemented)
          tile(lu)%plant(pft)%fapar_ind = get_fapar( lai_new )

          ! re-calculate metabolic and structural N, given new LAI and fAPAR
          call update_leaftraits( tile(lu)%plant(pft) )

          ! get updated leaf N
          nleaf = tile(lu)%plant(pft)%narea

        end do

        if (nitr > 0) print*,'no. of iterations ', nitr
        if (nitr > 0) print*,'final reduction of leaf C ', cleaf / pleaf_init%c%c12
        if (nitr > 0) print*,'final reduction of leaf N ', nleaf / pleaf_init%n%n14

        ! update 
        tile(lu)%plant(pft)%lai_ind = lai_new
        tile(lu)%plant(pft)%pleaf%c%c12 = cleaf
        tile(lu)%plant(pft)%pleaf%n%n14 = nleaf

        ! determine C and N turned over
        pleaf_turn = orgminus( pleaf_init, tile(lu)%plant(pft)%pleaf )

        if ( pleaf_turn%c%c12 < -1.0 * eps ) then
          stop 'negative turnover C'
        else if ( pleaf_turn%c%c12 < 0.0 ) then
           pleaf_turn%c%c12 = 0.0
        end if
        if ( pleaf_turn%n%n14 < -1.0 * eps ) then
          stop 'negative turnover N'
        else if ( pleaf_turn%n%n14 < 0.0 ) then
           pleaf_turn%n%n14 = 0.0
        end if

        ! reduce leaf mass and root mass
        call orgmv( pleaf_turn, pleaf_turn, tile_fluxes(lu)%plant(pft)%dharv )

        ! labile pool harvest
        dleaf = ( 1.0 - cleaf / pleaf_init%c%c12 )
        dlabl = min( 1.0, max( 0.0, dleaf ) )
        plabl_turn = orgfrac( dlabl, tile(lu)%plant(pft)%plabl ) ! leaf turnover
        call orgmv( plabl_turn, tile(lu)%plant(pft)%plabl, tile_fluxes(lu)%plant(pft)%dharv )

      end if

    end do

  end subroutine grharvest

end module md_landuse
