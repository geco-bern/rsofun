module md_turnover
  !////////////////////////////////////////////////////////////////
  ! NPP_LPJ MODULE
  ! Contains the "main" subroutine 'npp' and all necessary 
  ! subroutines for handling input/output. 
  ! Every module that implements 'npp' must contain this list 
  ! of subroutines (names that way).
  !   - npp
  !   - getpar_modl_npp
  !   - initio_npp
  !   - initoutput_npp
  !   - getout_daily_npp
  !   - getout_monthly_npp
  !   - writeout_ascii_npp
  ! Required module-independent model state variables (necessarily 
  ! updated by 'waterbal') are:
  !   - daily NPP ('dnpp')
  !   - soil temperature ('xxx')
  !   - inorganic N _pools ('no3', 'nh4')
  !   - xxx 
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  use md_classdefs
  use md_params_core, only: nlu, npft, eps, nmonth
  use md_tile
  use md_plant

  implicit none

  private
  public turnover, turnover_root, turnover_leaf, turnover_labl

  ! !----------------------------------------------------------------
  ! ! Module-specific output variables
  ! !----------------------------------------------------------------
  ! real, dimension(:,:), allocatable :: outaCveg2lit
  ! real, dimension(:,:), allocatable :: outaNveg2lit

contains

  subroutine turnover( tile, tile_fluxes, doy )
    !////////////////////////////////////////////////////////////////
    !  Annual vegetation biomass turnover, called at the end of the
    !  year.
    !----------------------------------------------------------------
    use md_phenology, only: shedleaves

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    integer, intent(in) :: doy

    ! local variables
    integer :: pft
    integer :: lu
    real :: dlabl
    real :: dleaf
    real :: droot
    ! real :: balance

    ! xxx verbose
    logical, parameter :: verbose = .false.
    type( orgpool ) :: orgtmp, orgtmp2

    pftloop: do pft = 1, npft

      lu = params_pft_plant(pft)%lu_category

      if (tile(lu)%plant(pft)%plabl%c%c12 < -1.0 * eps) stop 'before turnover labile C is neg.'
      if (tile(lu)%plant(pft)%plabl%n%n14 < -1.0 * eps) stop 'before turnover labile N is neg.'

      !--------------------------------------------------------------
      ! Get turnover fractions
      ! Turnover-rates are reciprocals of tissue longevity
      ! dleaf=1.0/long_leaf(pft)
      ! assuming no continuous leaf turnover
      !--------------------------------------------------------------
      if (params_pft_plant(pft)%grass) then

        ! balance = plant_fluxes(pft)%dnpp%c12 - plant_fluxes(pft)%dcex

        if (shedleaves(doy,pft)) then

          droot = 1.0
          dleaf = 1.0
          dlabl = 1.0

          stop 'shedding the fucking leaves'
          
        else

          ! Increase turnover rate towards high LAI ( when using non-zero value for k_decay_leaf_width, e.g. 0.08 )
          dleaf =  (tile(lu)%plant(pft)%lai_ind * params_pft_plant(pft)%k_decay_leaf_width )**8 &
            + params_pft_plant(pft)%k_decay_leaf_base

          ! constant turnover rate
          droot = params_pft_plant(pft)%k_decay_root
          dlabl = params_pft_plant(pft)%k_decay_labl

        end if

      else

        stop 'turnover not implemented for non-grasses'

      endif

      !--------------------------------------------------------------
      ! Calculate leaf turnover in this day 
      !--------------------------------------------------------------
      if (verbose) print*, 'calling turnover_leaf() ... '
      if (verbose) print*, '              with state variables:'
      if (verbose) print*, '              pleaf = ', tile(lu)%plant(pft)%pleaf
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_af
      if (verbose) orgtmp  =  tile(lu)%plant(pft)%pleaf
      if (verbose) orgtmp2 =  tile(lu)%soil%plitt_af
      !--------------------------------------------------------------
      if ( dleaf>0.0 ) call turnover_leaf( dleaf, tile(lu), tile_fluxes(lu), pft ) !, jpngr
      !--------------------------------------------------------------
      if (verbose) print*, '              ==> returned: '
      if (verbose) print*, '              pleaf = ', tile(lu)%plant(pft)%pleaf
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_af
      if (verbose) print*, '              --- balance: '
      if (verbose) print*, '                  dlitt - dleaf                = ',  orgminus( &
                                                                                    orgminus( &
                                                                                      tile(lu)%soil%plitt_af, &
                                                                                      orgtmp2 &
                                                                                      ), &
                                                                                    orgminus( &
                                                                                      orgtmp, &
                                                                                      tile(lu)%plant(pft)%pleaf &
                                                                                      ) &
                                                                                    )

      !--------------------------------------------------------------
      ! Calculate root turnover in this day 
      !--------------------------------------------------------------
      if (verbose) print*, 'calling turnover_root() ... '
      if (verbose) print*, '              with state variables:'
      if (verbose) print*, '              pleaf = ', tile(lu)%plant(pft)%proot
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_bg
      if (verbose) orgtmp  =  tile(lu)%plant(pft)%proot
      if (verbose) orgtmp2 =  tile(lu)%soil%plitt_bg
      !--------------------------------------------------------------
      if ( droot>0.0 ) call turnover_root( droot, tile(lu), pft )
      !--------------------------------------------------------------
      if (verbose) print*, '              ==> returned: '
      if (verbose) print*, '              proot = ', tile(lu)%plant(pft)%proot
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_bg
      if (verbose) print*, '              --- balance: '
      if (verbose) print*, '                  dlitt - droot                = ',  orgminus( &
                                                                                    orgminus( &
                                                                                      tile(lu)%soil%plitt_bg, &
                                                                                      orgtmp2 &
                                                                                      ), &
                                                                                    orgminus( &
                                                                                      orgtmp, &
                                                                                      tile(lu)%plant(pft)%proot &
                                                                                      ) &
                                                                                    )

      !--------------------------------------------------------------
      ! Calculate labile turnover in this day 
      !--------------------------------------------------------------
      if (verbose) print*, 'calling turnover_root() ... '
      if (verbose) print*, '              with state variables:'
      if (verbose) print*, '              pleaf = ', tile(lu)%plant(pft)%plabl
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_af
      if (verbose) orgtmp  =  tile(lu)%plant(pft)%plabl
      if (verbose) orgtmp2 =  tile(lu)%soil%plitt_af
      !--------------------------------------------------------------
      if ( dlabl>0.0 ) call turnover_labl( dlabl, tile(lu), pft )
      !--------------------------------------------------------------
      if (verbose) print*, '              ==> returned: '
      if (verbose) print*, '              plabl = ', tile(lu)%plant(:)%plabl
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_af
      if (verbose) print*, '              --- balance: '
      if (verbose) print*, '                  dlitt - dlabl                = ',  orgminus( &
                                                                                    orgminus( &
                                                                                      tile(lu)%soil%plitt_af, &
                                                                                      orgtmp2 &
                                                                                      ), &
                                                                                    orgminus( &
                                                                                      orgtmp, &
                                                                                      tile(lu)%plant(pft)%proot &
                                                                                      ) &
                                                                                    )
    enddo pftloop

  end subroutine turnover


  subroutine turnover_leaf( dleaf, tile, tile_fluxes, pft )
    !//////////////////////////////////////////////////////////////////
    ! Execute turnover of fraction dleaf for leaf pool
    !------------------------------------------------------------------
    ! arguments
    real, intent(in) :: dleaf
    type( tile_type ), intent(inout)  :: tile
    type( tile_fluxes_type ), intent(in) :: tile_fluxes
    integer, intent(in) :: pft

    ! local variables
    type(orgpool) :: lm_turn
    type(orgpool) :: lm_init

    real :: nleaf
    real :: cleaf
    real :: dlai
    real :: lai_new
    real :: diff
    integer :: nitr

    ! number of iterations to match leaf C given leaf N
    nitr = 0

    ! store leaf C and N before turnover
    lm_init = tile%plant(pft)%pleaf

    ! reduce leaf C (given by turnover rate)
    cleaf = ( 1.0 - dleaf ) * tile%plant(pft)%pleaf%c%c12

    ! get new LAI based on cleaf
    lai_new = get_lai( pft, cleaf, tile_fluxes%canopy%ppfd_memory, tile_fluxes%plant(pft)%actnv_unitiabs )

    ! update canopy state (only variable fAPAR so far implemented)
    tile%plant(pft)%fapar_ind = get_fapar( lai_new )

    ! re-calculate metabolic and structural N, given new LAI and fAPAR
    call get_leaftraits( tile%plant(pft), tile_fluxes%canopy%ppfd_memory, tile_fluxes%plant(pft)%actnv_unitiabs )

    ! get updated leaf N
    nleaf = tile%plant(pft)%narea

    do while ( nleaf > lm_init%n%n14 )

      nitr = nitr + 1

      ! reduce leaf C a bit more
      cleaf = cleaf * lm_init%n%n14 / nleaf

      ! get new LAI based on cleaf
      lai_new = get_lai( pft, cleaf, tile_fluxes%canopy%ppfd_memory, tile_fluxes%plant(pft)%actnv_unitiabs )

      ! update canopy state (only variable fAPAR so far implemented)
      tile%plant(pft)%fapar_ind = get_fapar( lai_new )

      ! re-calculate metabolic and structural N, given new LAI and fAPAR
      call get_leaftraits( tile%plant(pft), tile_fluxes%canopy%ppfd_memory, tile_fluxes%plant(pft)%actnv_unitiabs )

      ! get updated leaf N
      nleaf = tile%plant(pft)%narea

      if (nitr>30) exit

    end do

    ! if (nitr>0) print*,'no. of iterations ', nitr
    ! if (nitr>0) print*,'final reduction of leaf C ', cleaf / lm_init%c%c12
    ! if (nitr>0) print*,'final reduction of leaf N ', nleaf / lm_init%n%n14

    ! update 
    tile%plant(pft)%lai_ind = lai_new
    tile%plant(pft)%pleaf%c%c12 = cleaf
    tile%plant(pft)%pleaf%n%n14 = nleaf

    ! determine C and N turned over
    lm_turn = orgminus( lm_init, tile%plant(pft)%pleaf )

    if ( lm_turn%c%c12 < -1.0*eps ) then
      stop 'negative turnover C'
    else if ( lm_turn%c%c12 < 0.0 ) then
       lm_turn%c%c12 = 0.0
    end if
    if ( lm_turn%n%n14 < -1.0*eps ) then
      stop 'negative turnover N'
    else if ( lm_turn%n%n14 < 0.0 ) then
       lm_turn%n%n14 = 0.0
    end if

    ! add all organic (fixed) C to litter
    ! call cmvRec( lm_turn%c, lm_turn%c, tile%plant(pft)%plitt_af%c, outaCveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind))
    call cmv( lm_turn%c, lm_turn%c, tile%soil%plitt_af%c, scale = real(tile%plant(pft)%nind) )

    ! resorb fraction of N
    call nmv( nfrac( params_plant%f_nretain, lm_turn%n ), lm_turn%n, tile%plant(pft)%plabl%n )

    ! rest goes to litter
    ! call nmvRec( lm_turn%n, lm_turn%n, tile%soil%plitt_af%n, outaNveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind) )
    call nmv( lm_turn%n, lm_turn%n, tile%soil%plitt_af%n, scale = real(tile%plant(pft)%nind) )

  end subroutine turnover_leaf


  subroutine turnover_root( droot, tile, pft )
    !//////////////////////////////////////////////////////////////////
    ! Execute turnover of fraction droot for root pool
    !------------------------------------------------------------------
    ! arguments
    real, intent(in)    :: droot
    type( tile_type ), intent(inout)  :: tile
    integer, intent(in) :: pft

    ! local variables
    type(orgpool) :: rm_turn

    ! determine absolute turnover
    rm_turn = orgfrac( droot, tile%plant(pft)%proot ) ! root turnover

    ! reduce leaf mass and root mass
    call orgsub( rm_turn, tile%plant(pft)%proot )

    ! add all organic (fixed) C to litter
    ! call cmvRec( rm_turn%c, rm_turn%c, tile%soil%plitt_bg%c, outaCveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind))
    call cmv( rm_turn%c, rm_turn%c, tile%soil%plitt_bg%c, scale = real(tile%plant(pft)%nind))

    ! retain fraction of N
    call nmv( nfrac( params_plant%f_nretain, rm_turn%n ), rm_turn%n, tile%plant(pft)%plabl%n )

    ! rest goes to litter
    ! call nmvRec( rm_turn%n, rm_turn%n, tile%soil%plitt_bg%n, outaNveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind) )
    call nmv( rm_turn%n, rm_turn%n, tile%soil%plitt_bg%n, scale = real(tile%plant(pft)%nind) )

  end subroutine turnover_root


  subroutine turnover_labl( dlabl, tile, pft )
    !//////////////////////////////////////////////////////////////////
    ! Execute turnover of fraction dlabl for labl pool
    !------------------------------------------------------------------
    ! arguments
    real, intent(in)    :: dlabl
    type( tile_type ), intent(inout)  :: tile
    integer, intent(in) :: pft

    ! local variables
    type(orgpool) :: lb_turn

    ! detelbine absolute turnover
    lb_turn = orgfrac( dlabl, tile%plant(pft)%plabl ) ! labl turnover

    !! xxx think of something more plausible to put the labile C and N to

    ! reduce leaf mass and labl mass
    call orgsub( lb_turn, tile%plant(pft)%plabl )

    ! call orgmvRec( lb_turn, lb_turn, tile%plant(pft)%plitt_af, outaCveg2lit(pft,jpngr), outaNveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind) )
    call orgmv( lb_turn, lb_turn, tile%soil%plitt_af, scale = real(tile%plant(pft)%nind) )

  end subroutine turnover_labl


  ! subroutine initoutput_turnover( ngridcells )
  !   !////////////////////////////////////////////////////////////////
  !   ! Initialises all daily variables with zero.
  !   ! Called at the beginning of each year by 'biosphere'.
  !   !----------------------------------------------------------------
  !   use md_interface, only: interface
  !   use md_params_core, only: npft

  !   ! arguments
  !   integer, intent(in) :: ngridcells
    
  !   ! annual output variables
  !   if (interface%params_siml%loutturnover) then

  !     if (interface%steering%init) then
  !       allocate( outaCveg2lit(npft,ngridcells) )
  !       allocate( outaCveg2lit(npft,ngridcells) )
  !     end if
      
  !     outaCveg2lit(:,:) = 0.0
  !     outaCveg2lit(:,:) = 0.0

  !   end if

  ! end subroutine initoutput_turnover

end module md_turnover
