module md_turnover
  !////////////////////////////////////////////////////////////////
  ! Biomass tissue turnover. Code adapted from LPX-Bern.
  !----------------------------------------------------------------
  use md_classdefs
  use md_params_core, only: nlu, npft, eps, nmonth, ndayyear
  use md_tile_cnmodel
  use md_plant_cnmodel

  implicit none

  private
  public turnover, turnover_root, turnover_leaf, turnover_labl

  logical, parameter :: verbose = .false.
  logical, parameter :: baltest = .false.
  real :: cbal1, cbal2
  real :: nbal1, nbal2
  type( orgpool ) :: orgtmp, orgtmp2

contains

  subroutine turnover( tile, tile_fluxes, doy )
    !////////////////////////////////////////////////////////////////
    !  Annual vegetation biomass turnover, called at the end of the
    !  year.
    !----------------------------------------------------------------
    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    integer, intent(in) :: doy

    ! local variables
    integer :: pft
    integer :: lu
    real :: dlabl, dleaf, droot, dseed, dwood

    real, parameter :: k_decay_wood = 1.0 / 100.0

    pftloop: do pft=1,npft

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

        ! Increase turnover rate during seed filling phase
        dleaf =  1.0 - exp( -params_pft_plant(pft)%k_decay_leaf )

        ! constant turnover rate
        droot = 1.0 - exp( -params_pft_plant(pft)%k_decay_root )
        dwood = 1.0 - exp( -params_pft_plant(pft)%k_decay_sapw )
        dlabl = 1.0 - exp( -params_pft_plant(pft)%k_decay_labl )
        dseed = 1.0 - exp( -params_pft_plant(pft)%k_decay_root  * 3.0)

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
      if (verbose) cbal1 = tile(lu)%plant(pft)%pleaf%c%c12 + tile(lu)%soil%plitt_af%c%c12
      if (verbose) nbal1 = tile(lu)%plant(pft)%plabl%n%n14 + tile(lu)%plant(pft)%pleaf%n%n14 + tile(lu)%soil%plitt_af%n%n14
      !--------------------------------------------------------------
      if ( dleaf > 0.0 .and. tile(lu)%plant(pft)%pleaf%c%c12 > 0.0 ) call turnover_leaf( dleaf, tile(lu), tile_fluxes(lu), pft )
      !--------------------------------------------------------------
      if (verbose) print*, '              ==> returned: '
      if (verbose) print*, '              pleaf = ', tile(lu)%plant(pft)%pleaf
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_af
      if (verbose) cbal2 = tile(lu)%plant(pft)%pleaf%c%c12 + tile(lu)%soil%plitt_af%c%c12
      if (verbose) nbal2 = tile(lu)%plant(pft)%plabl%n%n14 + tile(lu)%plant(pft)%pleaf%n%n14 + tile(lu)%soil%plitt_af%n%n14
      if (verbose) cbal1 = cbal2 - cbal1
      if (verbose) nbal1 = nbal2 - nbal1
      if (verbose) print*, '              --- balance: '
      if (verbose) print*, '                  d(clitt + cleaf)             = ', cbal1
      if (verbose) print*, '                  d(nlitt + nleaf)             = ', nbal1
      if (baltest .and. abs(cbal1) > eps) stop 'balance 1 not satisfied'
      if (baltest .and. abs(nbal1) > eps) stop 'balance 1 not satisfied'

      !--------------------------------------------------------------
      ! Calculate root turnover in this day 
      !--------------------------------------------------------------
      if (verbose) print*, 'calling turnover_root() ... '
      if (verbose) print*, '              with state variables:'
      if (verbose) print*, '              proot = ', tile(lu)%plant(pft)%proot
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_bg
      if (verbose) cbal1 = tile(lu)%plant(pft)%proot%c%c12 + tile(lu)%soil%plitt_bg%c%c12
      if (verbose) nbal1 = tile(lu)%plant(pft)%plabl%n%n14 + tile(lu)%plant(pft)%proot%n%n14 + tile(lu)%soil%plitt_bg%n%n14
      !--------------------------------------------------------------
      if ( droot > 0.0 .and. tile(lu)%plant(pft)%proot%c%c12 > 0.0  ) call turnover_root( droot, tile(lu), tile_fluxes(lu), pft )
      !--------------------------------------------------------------
      if (verbose) print*, '              ==> returned: '
      if (verbose) print*, '              proot = ', tile(lu)%plant(pft)%proot
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_bg
      if (verbose) cbal2 = tile(lu)%plant(pft)%proot%c%c12 + tile(lu)%soil%plitt_bg%c%c12
      if (verbose) nbal2 = tile(lu)%plant(pft)%plabl%n%n14 + tile(lu)%plant(pft)%proot%n%n14 + tile(lu)%soil%plitt_bg%n%n14
      if (verbose) cbal1 = cbal2 - cbal1
      if (verbose) nbal1 = nbal2 - nbal1
      if (verbose) print*, '              --- balance: '
      if (verbose) print*, '                  d(clitt + croot)             = ', cbal1
      if (verbose) print*, '                  d(nlitt + nroot)             = ', nbal1
      if (baltest .and. abs(cbal1) > eps) stop 'balance 1 not satisfied'
      if (baltest .and. abs(nbal1) > eps) stop 'balance 1 not satisfied'

      !--------------------------------------------------------------
      ! Calculate wood turnover in this day 
      !--------------------------------------------------------------
      if (verbose) print*, 'calling turnover_wood() ... '
      if (verbose) print*, '              with state variables:'
      if (verbose) print*, '              pwood = ', tile(lu)%plant(pft)%pwood
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_as
      if (verbose) cbal1 = tile(lu)%plant(pft)%pwood%c%c12 + tile(lu)%soil%plitt_as%c%c12
      if (verbose) nbal1 = tile(lu)%plant(pft)%plabl%n%n14 + tile(lu)%plant(pft)%pwood%n%n14 + tile(lu)%soil%plitt_as%n%n14
      !--------------------------------------------------------------
      if ( dwood > 0.0 .and. tile(lu)%plant(pft)%pwood%c%c12 > 0.0  ) call turnover_wood( dwood, tile(lu), pft )
      !--------------------------------------------------------------
      if (verbose) print*, '              ==> returned: '
      if (verbose) print*, '              pwood = ', tile(lu)%plant(pft)%pwood
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_as
      if (verbose) cbal2 = tile(lu)%plant(pft)%pwood%c%c12 + tile(lu)%soil%plitt_as%c%c12
      if (verbose) nbal2 = tile(lu)%plant(pft)%plabl%n%n14 + tile(lu)%plant(pft)%pwood%n%n14 + tile(lu)%soil%plitt_as%n%n14
      if (verbose) cbal1 = cbal2 - cbal1
      if (verbose) nbal1 = nbal2 - nbal1
      if (verbose) print*, '              --- balance: '
      if (verbose) print*, '                  d(clitt + cwood)             = ', cbal1
      if (verbose) print*, '                  d(nlitt + nwood)             = ', nbal1
      if (baltest .and. abs(cbal1) > eps) stop 'balance 1 not satisfied'
      if (baltest .and. abs(nbal1) > eps) stop 'balance 1 not satisfied'

      !--------------------------------------------------------------
      ! Calculate seed turnover in this day 
      !--------------------------------------------------------------
      if (verbose) print*, 'calling turnover_seed() ... '
      if (verbose) print*, '              with state variables:'
      if (verbose) print*, '              pseed = ', tile(lu)%plant(pft)%pseed
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_af
      if (verbose) cbal1 = tile(lu)%plant(pft)%pseed%c%c12 + tile(lu)%soil%plitt_af%c%c12
      if (verbose) nbal1 = tile(lu)%plant(pft)%pseed%n%n14 + tile(lu)%soil%plitt_af%n%n14
      !--------------------------------------------------------------
      if ( dseed > 0.0 .and. tile(lu)%plant(pft)%pseed%c%c12 > 0.0  ) call turnover_seed( dseed, tile(lu), pft )
      !--------------------------------------------------------------
      if (verbose) print*, '              ==> returned: '
      if (verbose) print*, '              pseed = ', tile(lu)%plant(pft)%pseed
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_af
      if (verbose) cbal2 = tile(lu)%plant(pft)%pseed%c%c12 + tile(lu)%soil%plitt_af%c%c12
      if (verbose) nbal2 = tile(lu)%plant(pft)%pseed%n%n14 + tile(lu)%soil%plitt_af%n%n14
      if (verbose) cbal1 = cbal2 - cbal1
      if (verbose) nbal1 = nbal2 - nbal1
      if (verbose) print*, '              --- balance: '
      if (verbose) print*, '                  d(clitt + cseed)             = ', cbal1
      if (verbose) print*, '                  d(nlitt + nseed)             = ', nbal1
      if (baltest .and. abs(cbal1) > eps) stop 'balance 1 not satisfied'
      if (baltest .and. abs(nbal1) > eps) stop 'balance 1 not satisfied'


      !--------------------------------------------------------------
      ! Calculate labile turnover in this day - add to leaf respiration
      !--------------------------------------------------------------
      if (verbose) print*, 'calling turnover_labl() ... '
      if (verbose) print*, '              with state variables:'
      if (verbose) print*, '              plabl = ', tile(lu)%plant(pft)%plabl
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_bg
      if (verbose) cbal1 = tile(lu)%plant(pft)%proot%c%c12 + tile(lu)%soil%plitt_bg%c%c12
      if (verbose) nbal1 = tile(lu)%plant(pft)%plabl%n%n14 + tile(lu)%plant(pft)%proot%n%n14 + tile(lu)%soil%plitt_bg%n%n14
      !--------------------------------------------------------------
      if ( dlabl > 0.0 .and. tile(lu)%plant(pft)%pleaf%c%c12 > 0.0 ) call turnover_labl( dlabl, tile(lu), tile_fluxes(lu), pft )
      !--------------------------------------------------------------
      if (verbose) print*, '              ==> returned: '
      if (verbose) print*, '              plabl = ', tile(lu)%plant(pft)%plabl
      if (verbose) print*, '              plitt = ', tile(lu)%soil%plitt_bg
      if (verbose) cbal2 = tile(lu)%plant(pft)%proot%c%c12 + tile(lu)%soil%plitt_bg%c%c12
      if (verbose) nbal2 = tile(lu)%plant(pft)%plabl%n%n14 + tile(lu)%plant(pft)%proot%n%n14 + tile(lu)%soil%plitt_bg%n%n14
      if (verbose) cbal1 = cbal2 - cbal1
      if (verbose) nbal1 = nbal2 - nbal1
      if (verbose) print*, '              --- balance: '
      if (verbose) print*, '                  d(clitt + croot)             = ', cbal1
      if (verbose) print*, '                  d(nlitt + nroot)             = ', nbal1
      if (baltest .and. abs(cbal1) > eps) stop 'balance 1 not satisfied'
      if (baltest .and. abs(nbal1) > eps) stop 'balance 1 not satisfied'
    
    enddo pftloop

  end subroutine turnover


  subroutine turnover_leaf( dleaf, tile, tile_fluxes, pft )
    !//////////////////////////////////////////////////////////////////
    ! Execute turnover of fraction dleaf for leaf pool
    !------------------------------------------------------------------
    ! arguments
    real, intent(in) :: dleaf     ! fraction decaying
    type( tile_type ), intent(inout) :: tile
    type( tile_fluxes_type ), intent(inout) :: tile_fluxes
    integer, intent(in) :: pft

    ! local variables
    type(orgpool)  :: lm_turn
    type(orgpool)  :: lm_init
    type(nitrogen) :: nres

    real :: nleaf
    real :: cleaf
    real :: dlai
    real :: diff
    integer :: nitr

    if (verbose) print*,'                Before leaf turnover:'
    if (verbose) print*,'                          LAI   = ', tile%plant(pft)%lai_ind
    if (verbose) print*,'                          fapar = ', tile%plant(pft)%fapar_ind
    if (verbose) print*,'                          pleaf = ', tile%plant(pft)%pleaf

    ! number of iterations to match leaf C given leaf N
    nitr = 0

    ! store leaf C and N before turnover
    lm_init = tile%plant(pft)%pleaf

    ! reduce leaf C (given by turnover rate)
    cleaf = ( 1.0 - dleaf ) * tile%plant(pft)%pleaf%c%c12
    ! cleaf = tile%plant(pft)%pleaf%c%c12 * exp( - dleaf * myinterface%params_siml%secs_per_tstep )

    ! get new LAI based on cleaf
    tile%plant(pft)%lai_ind = get_lai( pft, cleaf, tile%plant(pft)%actnv_unitfapar )

    ! update canopy state (only variable fAPAR so far implemented)
    tile%plant(pft)%fapar_ind = get_fapar( tile%plant(pft)%lai_ind )

    ! re-calculate metabolic and structural N, given new LAI and fAPAR
    call update_leaftraits( tile%plant(pft) )

    ! get updated leaf N
    nleaf = tile%plant(pft)%narea_canopy

    ! ! xxx debug
    ! nleaf = cleaf * r_ntoc_leaf

    if (verbose) print*,'                     AFTER INITIAL N TURNOVER'
    if (verbose) print*,'                                LAI   = ', tile%plant(pft)%lai_ind
    if (verbose) print*,'                                fapar = ', tile%plant(pft)%fapar_ind
    if (verbose) print*,'                                nleaf = ', nleaf

    do while ( nleaf > lm_init%n%n14 )

      nitr = nitr + 1

      ! reduce leaf C a bit more
      cleaf = cleaf * lm_init%n%n14 / nleaf

      ! get new LAI based on cleaf
      tile%plant(pft)%lai_ind = get_lai( pft, cleaf, tile%plant(pft)%actnv_unitfapar )

      ! update canopy state (only variable fAPAR so far implemented)
      tile%plant(pft)%fapar_ind = get_fapar( tile%plant(pft)%lai_ind )

      ! re-calculate metabolic and structural N, given new LAI and fAPAR
      call update_leaftraits( tile%plant(pft) )

      ! get updated leaf N
      nleaf = tile%plant(pft)%narea_canopy

      ! ! xxx debug
      ! nleaf = cleaf * r_ntoc_leaf

      if (verbose) print*,'                      N iteration: ', nitr
      if (verbose) print*,'                                LAI   = ', tile%plant(pft)%lai_ind
      if (verbose) print*,'                                fapar = ', tile%plant(pft)%fapar_ind
      if (verbose) print*,'                                nleaf = ', nleaf

      if (nitr > 30) exit

    end do

    if (verbose .and. nitr > 0) print*,'                      ------------------'
    if (verbose .and. nitr > 0) print*,'                      No. of iterations ', nitr
    if (verbose .and. nitr > 0) print*,'                      final reduction of leaf C ', cleaf / lm_init%c%c12
    if (verbose .and. nitr > 0) print*,'                      final reduction of leaf N ', nleaf / lm_init%n%n14

    ! update 
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
    nres = nfrac( params_plant%f_nretain, lm_turn%n )
    tile_fluxes%plant(pft)%dnup_res = tile_fluxes%plant(pft)%dnup_res + nres%n14
    call nmv( nres, lm_turn%n, tile%plant(pft)%plabl%n )

    ! rest goes to litter
    ! call nmvRec( lm_turn%n, lm_turn%n, tile%soil%plitt_af%n, outaNveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind) )
    call nmv( lm_turn%n, lm_turn%n, tile%soil%plitt_af%n, scale = real(tile%plant(pft)%nind) )

  end subroutine turnover_leaf


  subroutine turnover_root( droot, tile, tile_fluxes, pft )
    !//////////////////////////////////////////////////////////////////
    ! Execute turnover of fraction droot for root pool
    !------------------------------------------------------------------
    ! arguments
    real, intent(in)    :: droot
    type( tile_type ), intent(inout)  :: tile
    type( tile_fluxes_type ), intent(inout) :: tile_fluxes
    integer, intent(in) :: pft

    ! local variables
    type(orgpool) :: rm_turn
    type(nitrogen) :: nres

    ! determine absolute turnover
    rm_turn = orgfrac( droot, tile%plant(pft)%proot ) ! root turnover

    ! reduce leaf mass and root mass
    call orgsub( rm_turn, tile%plant(pft)%proot )

    ! add all organic (fixed) C to litter
    ! call cmvRec( rm_turn%c, rm_turn%c, tile%soil%plitt_bg%c, outaCveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind))
    call cmv( rm_turn%c, rm_turn%c, tile%soil%plitt_bg%c, scale = real(tile%plant(pft)%nind) )

    ! retain fraction of N
    nres = nfrac( params_plant%f_nretain, rm_turn%n )
    tile_fluxes%plant(pft)%dnup_res = tile_fluxes%plant(pft)%dnup_res + nres%n14
    call nmv( nres, rm_turn%n, tile%plant(pft)%plabl%n )

    ! rest goes to litter
    ! call nmvRec( rm_turn%n, rm_turn%n, tile%soil%plitt_bg%n, outaNveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind) )
    call nmv( rm_turn%n, rm_turn%n, tile%soil%plitt_bg%n, scale = real(tile%plant(pft)%nind) )

  end subroutine turnover_root


  subroutine turnover_wood( dwood, tile, pft )
    !//////////////////////////////////////////////////////////////////
    ! Execute turnover of fraction dwood for wood pool
    !------------------------------------------------------------------
    ! arguments
    real, intent(in)    :: dwood
    type( tile_type ), intent(inout)  :: tile
    integer, intent(in) :: pft

    call orgmv( orgfrac( dwood, tile%plant(pft)%pwood ), &
                tile%plant(pft)%pwood, &
                tile%soil%plitt_as, &
                scale = real(tile%plant(pft)%nind) )

    ! ! local variables
    ! type(orgpool) :: rm_turn

    ! ! determine absolute turnover
    ! rm_turn = orgfrac( dwood, tile%plant(pft)%pwood ) ! wood turnover

    ! ! reduce leaf mass and wood mass
    ! call orgsub( rm_turn, tile%plant(pft)%pwood )

    ! ! add all organic (fixed) C to litter
    ! ! call cmvRec( rm_turn%c, rm_turn%c, tile%soil%plitt_bg%c, outaCveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind))
    ! call cmv( rm_turn%c, rm_turn%c, tile%soil%plitt_bg%c, scale = real(tile%plant(pft)%nind) )

    ! ! retain fraction of N
    ! call nmv( nfrac( params_plant%f_nretain, rm_turn%n ), rm_turn%n, tile%plant(pft)%plabl%n )

    ! ! rest goes to litter
    ! ! call nmvRec( rm_turn%n, rm_turn%n, tile%soil%plitt_bg%n, outaNveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind) )
    ! call nmv( rm_turn%n, rm_turn%n, tile%soil%plitt_bg%n, scale = real(tile%plant(pft)%nind) )

  end subroutine turnover_wood


  subroutine turnover_seed( dseed, tile, pft )
    !//////////////////////////////////////////////////////////////////
    ! Execute turnover of fraction dseed for seed pool
    !------------------------------------------------------------------
    ! arguments
    real, intent(in)    :: dseed
    type( tile_type ), intent(inout)  :: tile
    integer, intent(in) :: pft

    call orgmv( orgfrac( dseed, tile%plant(pft)%pseed ), &
                tile%plant(pft)%pseed, &
                tile%soil%plitt_af, &
                scale = real(tile%plant(pft)%nind) )

  end subroutine turnover_seed


  subroutine turnover_labl( dlabl, tile, tile_fluxes, pft )
    !//////////////////////////////////////////////////////////////////
    ! labile C and N turnover.
    !------------------------------------------------------------------
    ! arguments
    real, intent(in) :: dlabl
    type( tile_type ), intent(inout)  :: tile
    type( tile_fluxes_type ), intent(inout)  :: tile_fluxes
    integer, intent(in) :: pft

    ! local variables
    type(orgpool) :: lb_turn

    ! detelbine absolute turnover
    lb_turn = orgfrac( dlabl, tile%plant(pft)%plabl ) ! labl turnover

    ! labile turnover to be recorded for output
    tile_fluxes%plant(pft)%dlabl = lb_turn

    ! reduce labile mass
    call orgsub( lb_turn, tile%plant(pft)%plabl )

    ! call orgmvRec( lb_turn, lb_turn, tile%plant(pft)%plitt_af, outaCveg2lit(pft,jpngr), outaNveg2lit(pft,jpngr), scale = real(tile%plant(pft)%nind) 
    call orgmv( lb_turn, lb_turn, tile%soil%plitt_af, scale = real(tile%plant(pft)%nind) )

  end subroutine turnover_labl


end module md_turnover
