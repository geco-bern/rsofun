module md_vegdynamics_cnmodel
  !////////////////////////////////////////////////////////////////
  ! Vegetation cover definitions for CN-model simulations.
  !---------------------------------------------------------------
  use md_tile_cnmodel
  use md_plant_cnmodel

  implicit none

  private
  public vegdynamics

  real, parameter :: diam_inc_init = 0.001 ! m

contains

  subroutine vegdynamics( tile, tile_fluxes, doy, init )
    !//////////////////////////////////////////////////////////////////
    ! Updates canopy and stand variables and calls 'estab_daily' to 
    ! simulate establishment of new individuals
    !------------------------------------------------------------------
    use md_params_core, only: nlu, npft, ndayyear
    ! use md_phenology, only: temppheno_type, params_pft_pheno
    use md_interface_cnmodel, only: myinterface

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    integer, intent(in) :: doy
    logical, intent(in) :: init

    ! local variables
    integer :: pft, lu

    !----------------------------------------------------------
    ! Add seed at the beginning of the simulation
    !----------------------------------------------------------
    do pft=1,npft
      lu = params_pft_plant(pft)%lu_category
      
      if (doy == 1 .and. init) then
        call init_plant( tile(lu)%plant(pft) )
        call init_plant_fluxes( tile_fluxes(lu)%plant(pft) )
        call estab_daily( tile(lu)%plant(pft) )
      end if

      ! add seed C and N to labile pool
      if (.not. myinterface%steering%closed_nbal) then
        call sowing_daily( tile(lu)%plant(pft), myinterface%landuse(doy)%cseed, myinterface%landuse(doy)%nseed )
      end if
    end do


    ! do pft=1,npft
    !   lu = params_pft_plant(pft)%lu_category

    !   if (params_pft_plant(pft)%grass) then
    !     !----------------------------------------------------------
    !     ! GRASSES, summergreen
    !     !----------------------------------------------------------

    !     if ( tile(lu)%plant(pft)%pheno(doy)%sprout ) then
    !       !----------------------------------------------------------
    !       ! beginning of season
    !       !----------------------------------------------------------
    !       call estab_daily( tile(lu)%plant(pft) )

    !       ! stop 'adding a seed'

    !     end if

    !   else

    !     stop 'estab_daily not implemented for non-summergreen'

    !   end if

    ! end do

  end subroutine vegdynamics


  subroutine estab_daily( plant )
    !//////////////////////////////////////////////////////////////////
    ! Calculates leaf-level metabolic N content per unit leaf area as a
    ! function of Vcmax25.
    !------------------------------------------------------------------
    ! arguments
    type(plant_type), intent(inout) :: plant

    ! ! initialise all pools of this PFT with zero
    ! call initpft( pft, jpngr )

    ! add C (and N) to labile pool (available for allocation)
    call add_seed( plant )

    if (params_pft_plant(plant%pftno)%grass) then
      plant%nind = 1.0
    else
      stop 'estab_daily not implemented for trees'
    end if

  end subroutine estab_daily


  subroutine sowing_daily( plant, cseed, nseed )
    !//////////////////////////////////////////////////////////////////
    ! Calculates leaf-level metabolic N content per unit leaf area as a
    ! function of Vcmax25.
    !------------------------------------------------------------------
    ! arguments
    type(plant_type), intent(inout) :: plant
    real, intent(in) :: cseed, nseed

    plant%plabl%c%c12 = plant%plabl%c%c12 + cseed
    plant%plabl%n%n14 = plant%plabl%n%n14 + nseed

    if (params_pft_plant(plant%pftno)%grass) then
      plant%nind = 1.0
    else
      stop 'sowing_daily not implemented for trees'
    end if

  end subroutine sowing_daily


  ! subroutine vegdynamics( tile, plant, solar, out_pmodel )
  !   !//////////////////////////////////////////////////////////////////
  !   ! Updates canopy and tile variables and calls 'estab' to 
  !   ! simulate establishment of new individuals
  !   !------------------------------------------------------------------
  !   use md_classdefs
  !   use md_params_core, only: npft, nlu, nmonth, ndayyear
  !   use md_plant, only: initpft, get_leaftraits, plant_type, params_pft_plant
  !   use md_allocation, only: update_tree
  !   use md_tile, only: tile_type
  !   use md_waterbal, only: solartype
  !   use md_gpp, only: outtype_pmodel

  !   ! arguments
  !   type( tile_type ), dimension(nlu), intent(inout)           :: tile
  !   type( plant_type ), dimension(npft), intent(in)            :: plant ! npft counts over PFTs in all land units (tiles)
  !   type( solartype ), intent(in)                              :: solar
  !   type( outtype_pmodel ), dimension(npft,nmonth), intent(in) :: out_pmodel

  !   ! local variables
  !   integer :: pft, lu

  !   do lu=1,nlu
  !     if (tile(lu)%fpc_grid == 0.0) then
  !       !------------------------------------------------------------------
  !       ! Add individuals
  !       !------------------------------------------------------------------
  !       do pft=1,npft
  !         if (params_pft_plant(pft)%lu_category==lu) then

  !           ! initialise all pools of this PFT with zero
  !           call initpft( plant(pft) )

  !           ! get annually updated leaf traits (vary because of variations in light and CO2)
  !           call get_leaftraits( plant(pft), solar%meanmppfd(:), out_pmodel(pft,:)%actnv_unitiabs )

  !           ! add a "seed" by forcing initial diameter increment
  !           call update_tree( plant(pft), diam_inc_init )

  !           ! give it some labile C and N to pay for turnover this year (until allocation at the end of year)
  !           plant(pft)%plabl = orgplus( &
  !             orgfrac( params_pft_plant(pft)%k_decay_leaf * ndayyear, plant(pft)%pleaf ), &
  !             orgfrac( params_pft_plant(pft)%k_decay_root * ndayyear, plant(pft)%proot ) &
  !             ) 

  !           ! xxx needs to be done: add implicit C and N fluxes to NPP and N-uptake

  !           ! Set number of individuals (XXX change this to m-2)
  !           tile(lu)%nind(pft)  = 1.0 
  !           tile%fpc_grid = tile(lu)%nind(pft) * plant(pft)%acrown

  !         end if
  !       end do
  !     end if
  !   end do

  ! end subroutine vegdynamics

end module md_vegdynamics_cnmodel

