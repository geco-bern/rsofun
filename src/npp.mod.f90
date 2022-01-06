module md_npp
  !////////////////////////////////////////////////////////////////
  ! NPP_LPJ MODULE
  ! Contains the "main" subroutine 'npp' and all necessary 
  ! subroutines for handling input/output. 
  ! Every module that implements 'npp' must contain this list 
  ! of subroutines (names that way).
  !   - npp
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
  use md_params_core, only: npft, ndayyear, eps, nlu
  use md_tile
  use md_plant
  use md_forcing_pmodel, only: climate_type, vegcover_type
  ! use md_interface_pmodel

  implicit none

  private
  public npp, calc_cexu, calc_resp_maint

contains

  subroutine npp( tile, tile_fluxes, climate )
    !/////////////////////////////////////////////////////////////////////////
    ! NET PRIMARY PRODUCTIVITY
    ! Calculate maintenance and growth respiration and substract this from GPP 
    ! to get NPP before additional root respiration for nutrient uptake (see 
    ! SR nuptake). NPP is defined so as to include C allocated to growth as 
    ! well as to exudation. Thus, exudation is not part of autotrophic respir-
    ! ation, but diverted to the quickly decaying exudates pool. Exudates decay
    ! is calculated in SR 'littersom' and is kept track of as soil respiration 
    ! ('rsoil'). This implies that growth respiration is "paid" also on exu-
    ! dates. 
    !-------------------------------------------------------------------------
    ! use md_turnover, only: turnover_leaf, turnover_root, turnover_labl

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(climate_type)  :: climate

    ! local variables
    type(plant_fluxes_type) :: pf
    type(plant_type) :: pp
    integer :: pft
    integer :: lu

    real, parameter :: dleaf_die = 0.012
    real, parameter :: droot_die = 0.012
    real, parameter :: dlabl_die = 0.0

    ! xxx debug
    real :: tmp

    pftloop: do pft=1,npft

      lu = params_pft_plant(pft)%lu_category

      ! as a short-hand
      pf = tile_fluxes(lu)%plant(pft)
      pp = tile(lu)%plant(pft)

      if (pp%plabl%c%c12 < 0.0) stop 'before npp labile C is neg.'
      if (pp%plabl%n%n14 < 0.0) stop 'before npp labile N is neg.'

      !/////////////////////////////////////////////////////////////////////////
      ! MAINTENANCE RESPIRATION
      ! use function 'resp_main'
      !-------------------------------------------------------------------------
      ! fine roots should have a higher repsiration coefficient than other tissues (Franklin et al., 2007).
      pf%drleaf = pf%drd  ! leaf respiration is given by dark respiration as calculated in P-model.       
      pf%drroot = calc_resp_maint( pp%proot%c%c12 * pp%nind, params_plant%r_root, climate%dtemp )
      if (params_pft_plant(pft)%tree) then
        pf%drsapw = calc_resp_maint( pp%psapw%c%c12 * pp%nind, params_plant%r_sapw, climate%dtemp )
      endif


      !/////////////////////////////////////////////////////////////////////////
      ! DAILY NPP AND C EXPORT
      ! NPP is the sum of C available for growth and for N uptake 
      ! This is where isotopic signatures are introduced because only 'dbminc'
      ! is diverted to a pool and re-emission to atmosphere gets delayed. Auto-
      ! trophic respiration is immediate, it makes thus no sense to calculate 
      ! full isotopic effects of gross exchange _fluxes.
      ! Growth respiration ('drgrow') is deduced from 'dnpp' in allocation SR.
      !-------------------------------------------------------------------------
      pf%dnpp = carbon( pf%dgpp - pf%drleaf - pf%drroot )
      pf%dcex = calc_cexu( pp%proot%c%c12, climate%dtemp )   

      ! !/////////////////////////////////////////////////////////////////////////
      ! ! SAFETY AND DEATH
      ! ! If negative C balance results from GPP - Rleaf - Rroot - Cex then ...
      ! ! ... first, change allocation to 100% leaves
      ! ! ... second, when this still leads to a complete depletion of the labile
      ! !     pool (negative values), shut down organism (zero GPP, NPP, etc., 
      ! !     but continuing turnover).
      ! !-------------------------------------------------------------------------
      ! ! ! This option (deactivate_root) leads to good results, the alternative leads to on-off growth. Unclear why.
      ! ! if ( (pf%dnpp%c12 - pf%dcex) < 0.0 ) then
      ! !   call deactivate_root( pf%dgpp, pf%drleaf, pp%plabl%c%c12, pp%proot, pf%drroot, pf%dnpp%c12, pf%dcex, dtemp, pp%plitt_bg )
      ! ! end if

      ! ! -------------------------------------------------------------------------
      ! ! the alternative formulation with shutting all fluxes down and decaying
      ! ! -------------------------------------------------------------------------
      ! if ( (pp%plabl%c%c12 + pf%dnpp%c12 - pf%dcex) < 0.0 ) then
      !   ! stop exuding
      !   pf%dcex = 0.0

      !   if ( ( pp%plabl%c%c12 + pf%dnpp%c12 ) < 0.0 ) then

      !     ! ! after C balance has become negative wait until it gets positive again to trigger sprouting
      !     ! ! print*,'setting check_sprout = T ', doy
      !     ! check_sprout = .true.

      !     ! slow death
      !     ! print*,'slow death', doy
      !     pf%dgpp   = 0.0
      !     pf%drleaf = 0.0
      !     pf%drroot = 0.0
      !     pf%drd    = 0.0
      !     pf%dcex   = 0.0
      !     pf%dnpp   = carbon(0.0)

      !     call turnover_leaf( dleaf_die, pft, jpngr )
      !     call turnover_root( droot_die, pft, jpngr )
      !     ! call turnover_labl( dlabl_die, pft, jpngr )

      !   end if

      ! else
      !   ! normal growth

      !   ! ! trigger sprouting now that C balance is positive again
      !   ! if (check_sprout) then
      !   !   ! print*,'sprouting next day'
      !   !   sprout(doy+1,pft) = .true.
      !   ! end if
      !   ! check_sprout = .false.

      !   ! ! print*,'normal growth', doy
      !   ! if ( .not. interface%steering%dofree_alloc ) frac_leaf(pft) = 0.5
      
      ! end if

      ! !/////////////////////////////////////////////////////////////////////////
      ! ! EXUDATES
      ! !-------------------------------------------------------------------------
      ! call ccp( carbon( pf%dcex ), pp%pexud )

      ! !/////////////////////////////////////////////////////////////////////////
      ! ! TO LABILE POOL
      ! ! NPP available for growth first enters the labile pool ('plabl ').
      ! ! XXX Allocation is called here without "paying"  growth respir.?
      ! !-------------------------------------------------------------------------
      ! tmp = pf%dnpp%c12 - pf%dcex
      ! print*,'GPP, Rl, Rr, Cex, dC, Cl, LAI, Cb: ', pf%dgpp, pf%drleaf, pf%drroot, pf%dcex, tmp, pp%pleaf, pp%lai_ind, pp%plabl

      ! call ccp( carbon( pf%dcex ), pp%pexud )
      ! call ccp( cminus( pf%dnpp, carbon(pf%dcex) ), pp%plabl%c )

      ! if (pp%plabl%c%c12 < (-1)*eps) stop 'after npp labile C is neg.'
      ! if (pp%plabl%n%n14 < (-1)*eps) stop 'after npp labile N is neg.'

      ! print*,'gpp, dclabl', doy, pf%dgpp, cminus( pf%dnpp, carbon(pf%dcex) )

      ! as a short-hand
      tile_fluxes(lu)%plant(pft) = pf
      tile(lu)%plant(pft) = pp

    end do pftloop

  end subroutine npp


  subroutine deactivate_root( mygpp, mydrleaf, myplabl, myproot, rroot, npp, cexu, dtemp, myplitt )
    !/////////////////////////////////////////////////////////////////////////
    ! Calculates amount of root mass supportable by (GPP-Rd+Clabl='avl'), so that
    ! NPP is zero and doesn't get negative. Moves excess from pool 'myproot' to
    ! pool 'myplitt'.
    !-------------------------------------------------------------------------
    ! argument
    real, intent(in) :: mygpp
    real, intent(in) :: mydrleaf
    real, intent(in) :: myplabl
    type( orgpool ), intent(inout) :: myproot
    real, intent(out) :: rroot
    real, intent(out) :: npp
    real, intent(out) :: cexu
    real, intent(in) :: dtemp
    type( orgpool ), intent(inout), optional :: myplitt
    
    ! local variables
    real :: croot_trgt
    real :: droot
    type( orgpool ) :: rm_turn

    real, parameter :: safety = 0.9999

    ! calculate target root mass
    croot_trgt = safety * ( mygpp - mydrleaf + myplabl) / ( params_plant%r_root + params_plant%exurate )
    droot      = ( 1.0 - croot_trgt / myproot%c%c12 )
    rm_turn    = orgfrac( droot, myproot )
    if (present(myplitt)) then
      call orgmv( rm_turn, myproot, myplitt )
    else
      myproot = orgminus( myproot, rm_turn )
    end if

    ! update fluxes based on corrected root mass
    rroot = calc_resp_maint( myproot%c%c12, params_plant%r_root, dtemp )
    npp   = mygpp - mydrleaf - rroot
    cexu  = calc_cexu( myproot%c%c12 , dtemp )     

  end subroutine deactivate_root


  function calc_resp_maint( cmass, rresp, dtemp ) result( resp_maint )
    !////////////////////////////////////////////////////////////////
    ! Returns maintenance respiration
    !----------------------------------------------------------------
    ! use md_gpp, only: calc_tempstress     ! same ramp as for GPP 

    ! arguments
    real, intent(in) :: cmass   ! N mass per unit area [gN/m2]
    real, intent(in) :: rresp   ! respiration coefficient [gC gC-1 d-1]
    real, intent(in) :: dtemp   ! temperature (soil or air, deg C)

    ! function return variable
    real :: resp_maint                    ! return value: maintenance respiration [gC/m2]

    ! resp_maint = cmass * rresp * calc_tempstress( dtemp )

    ! LPX-like temperature dependeneo of respiration rates
    resp_maint = cmass * rresp * ftemp( dtemp, "lloyd_and_taylor" )

  end function calc_resp_maint


  function calc_cexu( croot, dtemp ) result( cexu )
    !/////////////////////////////////////////////////////////////////
    ! Constant exudation rate
    !-----------------------------------------------------------------
    ! arguments
    real, intent(in)           :: croot
    real, intent(in), optional :: dtemp   ! temperature (soil or air, deg C)

    ! function return variable
    real :: cexu

    ! low-temperature ramp is included here to prevent negative C balance after exudation
    cexu = params_plant%exurate * croot

  end function calc_cexu

end module md_npp
