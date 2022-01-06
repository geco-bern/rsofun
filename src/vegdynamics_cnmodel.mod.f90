module md_vegdynamics
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk

  implicit none

  private
  public vegdynamics

  real, parameter :: diam_inc_init = 0.001 ! m

contains

  subroutine vegdynamics( plant, temppheno )
    !//////////////////////////////////////////////////////////////////
    ! Updates canopy and stand variables and calls 'estab_daily' to 
    ! simulate establishment of new individuals
    !------------------------------------------------------------------
    use md_params_core, only: npft, ndayyear
    use md_phenology, only: temppheno_type, params_pft_pheno
    use md_plant, only: plant_type, params_pft_plant

    ! arguments
    type(plant_type), dimension(npft), intent(inout) :: plant
    type(temppheno_type), dimension(npft), intent(in) :: temppheno

    ! local variables
    integer :: pft

    do pft=1,npft

      if (params_pft_plant(pft)%grass) then
        !----------------------------------------------------------
        ! GRASSES, summergreen
        !----------------------------------------------------------

        if ( temppheno(pft)%sprout ) then
          !----------------------------------------------------------
          ! beginning of season
          !----------------------------------------------------------
          call estab_daily( plant(pft), pft )

          ! stop 'adding a seed'

        end if

      else

        stop 'estab_daily not implemented for non-summergreen'

      end if

    end do

  end subroutine vegdynamics


  subroutine estab_daily( plant, pft )
    !//////////////////////////////////////////////////////////////////
    ! Calculates leaf-level metabolic N content per unit leaf area as a
    ! function of Vcmax25.
    !------------------------------------------------------------------
    use md_plant, only: plant_type, params_plant, params_pft_plant
    use md_interface

    ! arguments
    type(plant_type), intent(inout) :: plant
    integer, intent(in) :: pft

    ! ! initialise all pools of this PFT with zero
    ! call initpft( pft, jpngr )

    ! add C (and N) to labile pool (available for allocation)
    call add_seed( plant )
    if ( .not. interface%steering%dofree_alloc ) params_plant%frac_leaf = 0.5
    
    if (params_pft_plant(pft)%grass) then
      plant%nind = 1.0
    else
      stop 'estab_daily not implemented for trees'
    end if


  end subroutine estab_daily


  subroutine add_seed( plant )
    !//////////////////////////////////////////////////////////////////
    ! To initialise plant pools, add "sapling" mass
    !------------------------------------------------------------------
    use md_classdefs
    use md_plant, only: plant_type, seed

    ! arguments
    type(plant_type), intent(inout) :: plant

    plant%plabl = orgplus( plant%plabl, seed )

  end subroutine add_seed


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
  !             orgfrac( params_pft_plant(pft)%k_decay_leaf_base * ndayyear, plant(pft)%pleaf ), &
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

end module md_vegdynamics

