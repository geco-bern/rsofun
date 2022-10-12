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
    use md_turnover, only: turnover_leaf, turnover_root

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(climate_type), intent(in) :: climate

    ! local variables
    integer :: pft
    integer :: lu

    real, parameter :: dleaf_die = 0.012
    real, parameter :: droot_die = 0.012
    real, parameter :: dlabl_die = 0.0

    ! for CUE dampening, determining acceleration of turnover (tissue deactivation)
    logical, save :: firstcall = .true.
    integer, parameter :: len_cue_vec = 15
    real, dimension(nlu,npft,len_cue_vec), save :: cue_vec
    real :: f_deactivate, cue_damped

    ! xxx debug
    real :: tmp

    pftloop: do pft=1,npft

      lu = params_pft_plant(pft)%lu_category

      ! if (tile(lu)%plant(pft)%plabl%c%c12 < 0.0) stop 'before npp labile C is neg.'
      ! if (tile(lu)%plant(pft)%plabl%n%n14 < 0.0) stop 'before npp labile N is neg.'

      !/////////////////////////////////////////////////////////////////////////
      ! MAINTENANCE RESPIRATION
      ! use function 'resp_main'
      !-------------------------------------------------------------------------
      ! fine roots should have a higher repsiration coefficient than other tissues (Franklin et al., 2007).
      tile_fluxes(lu)%plant(pft)%drleaf = tile_fluxes(lu)%plant(pft)%drd  ! leaf respiration is given by dark respiration as calculated in P-model.       
      tile_fluxes(lu)%plant(pft)%drroot = calc_resp_maint(  tile(lu)%plant(pft)%proot%c%c12, &
                                                            params_plant%r_root, &
                                                            climate%dtemp &
                                                            )
      if (params_pft_plant(pft)%tree) then
        tile_fluxes(lu)%plant(pft)%drsapw = calc_resp_maint(  tile(lu)%plant(pft)%psapw%c%c12, &
                                                              params_plant%r_sapw, &
                                                              climate%dtemp &
                                                              )
      else
        tile_fluxes(lu)%plant(pft)%drsapw = 0.0
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
      tile_fluxes(lu)%plant(pft)%dnpp = carbon( tile_fluxes(lu)%plant(pft)%dgpp &
                                              - tile_fluxes(lu)%plant(pft)%drleaf &
                                              - tile_fluxes(lu)%plant(pft)%drroot &
                                              - tile_fluxes(lu)%plant(pft)%drsapw &
                                                )
      tile_fluxes(lu)%plant(pft)%dcex = calc_cexu( tile(lu)%plant(pft)%proot%c%c12 )   

      ! !/////////////////////////////////////////////////////////////////////////
      ! ! SAFETY AND DEATH (CUE-driven mortality)
      ! ! If negative C balance results from GPP - Rleaf - Rroot then ...
      ! ! ... increase tissue turnover (deactivation)
      ! !-------------------------------------------------------------------------      
      ! ! CUE dampening
      ! if (firstcall) then
      !   if (tile_fluxes(lu)%plant(pft)%dgpp < eps) then
      !     cue_vec(lu,pft,:) = 0.0
      !   else
      !     cue_vec(lu,pft,:) = tile_fluxes(lu)%plant(pft)%dnpp%c12 / tile_fluxes(lu)%plant(pft)%dgpp
      !   end if
      !   firstcall = .false.
      ! else
      !   cue_vec(lu,pft,1:(len_cue_vec-1)) = cue_vec(lu,pft,2:len_cue_vec)
      !   if (tile_fluxes(lu)%plant(pft)%dgpp < eps) then
      !     cue_vec(lu,pft,len_cue_vec) = 0.0
      !   else
      !     cue_vec(lu,pft,len_cue_vec) = tile_fluxes(lu)%plant(pft)%dnpp%c12 / tile_fluxes(lu)%plant(pft)%dgpp
      !   end if
      ! end if
      ! cue_damped = sum( cue_vec(lu,pft,:) ) / real( len_cue_vec )

      ! ! turnover due to CUE-driven deactivation: increase towards 1 when (damped) CUE gets negative.
      ! ! note that CUE can become negative when respiration is positive and GPP tending towards zero
      ! if (tile(lu)%plant(pft)%plabl%c%c12 < 1.0 * tile(lu)%plant(pft)%pleaf%c%c12) then
        
      !   f_deactivate = 0.5 * calc_cue_stress( cue_damped )   ! never deactivate 100%

      !   if (f_deactivate > 0.001) then  ! don't call it when removing only a negiglible fraction - computational costs!
      !     call turnover_leaf( f_deactivate, tile(lu), tile_fluxes(lu), pft )
      !     call turnover_root( f_deactivate, tile(lu), pft )
      !   else
      !     f_deactivate = 0.0
      !   end if
      ! else
      !   f_deactivate = 0.0
      ! end if

      ! ! print*,'gpp, cue, cue_damped, f_deactivate: ', &
      ! !   tile_fluxes(lu)%plant(pft)%dgpp, &
      ! !   tile_fluxes(lu)%plant(pft)%dnpp%c12 / tile_fluxes(lu)%plant(pft)%dgpp, &
      ! !   cue_damped, &
      ! !   f_deactivate   

      ! ! xxx test
      ! ! read into R with:
      ! ! read_fwf(
      ! !    "~/rsofun/test.txt",
      ! !    fwf_widths(c(19,18,18), c("cue", "cue_damped", "f_deactivate")),
      ! !    skip = 1,
      ! !    col_types = "nn")
      
      ! ! print*, tile_fluxes(lu)%plant(pft)%dnpp%c12 / tile_fluxes(lu)%plant(pft)%dgpp, cue_damped, f_deactivate

      ! !/////////////////////////////////////////////////////////////////////////
      ! ! SAFETY AND DEATH
      ! ! If negative C balance results from GPP - Rleaf - Rroot - Cex then ...
      ! ! ... first, change allocation to 100% leaves
      ! ! ... second, when this still leads to a complete depletion of the labile
      ! !     pool (negative values), shut down organism (zero GPP, NPP, etc., 
      ! !     but continuing turnover).
      ! !-------------------------------------------------------------------------
      ! ! ! This option (deactivate_root) leads to good results, the alternative leads to on-off growth. Unclear why.
      ! ! if ( (tile_fluxes(lu)%plant(pft)%dnpp%c12 - tile_fluxes(lu)%plant(pft)%dcex) < 0.0 ) then
      ! !   call deactivate_root( tile_fluxes(lu)%plant(pft)%dgpp, tile_fluxes(lu)%plant(pft)%drleaf, tile(lu)%plant(pft)%plabl%c%c12, tile(lu)%plant(pft)%proot, tile_fluxes(lu)%plant(pft)%drroot, tile_fluxes(lu)%plant(pft)%dnpp%c12, tile_fluxes(lu)%plant(pft)%dcex, dtemp, tile(lu)%plant(pft)%plitt_bg )
      ! ! end if

      ! ! -------------------------------------------------------------------------
      ! ! the alternative formulation with shutting all fluxes down and decaying
      ! ! -------------------------------------------------------------------------
      ! if ( (tile(lu)%plant(pft)%plabl%c%c12 + tile_fluxes(lu)%plant(pft)%dnpp%c12 - tile_fluxes(lu)%plant(pft)%dcex) < 0.0 ) then
      !   ! stop exuding
      !   tile_fluxes(lu)%plant(pft)%dcex = 0.0

      !   if ( ( tile(lu)%plant(pft)%plabl%c%c12 + tile_fluxes(lu)%plant(pft)%dnpp%c12 ) < 0.0 ) then

      !     ! ! after C balance has become negative wait until it gets positive again to trigger sprouting
      !     ! ! print*,'setting check_sprout = T ', doy
      !     ! check_sprout = .true.

      !     ! slow death
      !     ! print*,'slow death', doy
      !     tile_fluxes(lu)%plant(pft)%dgpp   = 0.0
      !     tile_fluxes(lu)%plant(pft)%drleaf = 0.0
      !     tile_fluxes(lu)%plant(pft)%drroot = 0.0
      !     tile_fluxes(lu)%plant(pft)%drd    = 0.0
      !     tile_fluxes(lu)%plant(pft)%dcex   = 0.0
      !     tile_fluxes(lu)%plant(pft)%dnpp   = carbon(0.0)

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


      !/////////////////////////////////////////////////////////////////////////
      ! TO LABILE POOL
      ! NPP available for growth first enters the labile pool ('plabl ').
      !-------------------------------------------------------------------------
      ! tmp = tile_fluxes(lu)%plant(pft)%dnpp%c12 - tile_fluxes(lu)%plant(pft)%dcex
      ! print*,'GPP, Rl, Rr, Cex, dC, Cl, LAI, Cb: ', tile_fluxes(lu)%plant(pft)%dgpp, tile_fluxes(lu)%plant(pft)%drleaf, tile_fluxes(lu)%plant(pft)%drroot, tile_fluxes(lu)%plant(pft)%dcex, tmp, tile(lu)%plant(pft)%pleaf, tile(lu)%plant(pft)%lai_ind, tile(lu)%plant(pft)%plabl

      ! call ccp( carbon( tile_fluxes(lu)%plant(pft)%dcex ), tile(lu)%soil%pexud )
      ! call ccp( cminus( tile_fluxes(lu)%plant(pft)%dnpp, carbon(tile_fluxes(lu)%plant(pft)%dcex) ), tile(lu)%plant(pft)%plabl%c )

      tile(lu)%soil%pexud%c12 = tile(lu)%soil%pexud%c12 + tile_fluxes(lu)%plant(pft)%dcex
      tile(lu)%plant(pft)%plabl%c%c12 = tile(lu)%plant(pft)%plabl%c%c12     &
                                      + tile_fluxes(lu)%plant(pft)%dnpp%c12 &
                                      - tile_fluxes(lu)%plant(pft)%dcex

      if (tile(lu)%plant(pft)%plabl%c%c12 < (-1)*eps) stop 'after npp labile C is neg.'
      if (tile(lu)%plant(pft)%plabl%n%n14 < (-1)*eps) stop 'after npp labile N is neg.'

      ! print*,'gpp, dclabl', doy, tile_fluxes(lu)%plant(pft)%dgpp, cminus( tile_fluxes(lu)%plant(pft)%dnpp, carbon(tile_fluxes(lu)%plant(pft)%dcex) )

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
    cexu  = calc_cexu( myproot%c%c12 )     

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

    ! LPX-like temperature dependeneo of respiration rates
    resp_maint = cmass * rresp * ftemp( dtemp, "lloyd_and_taylor" )

    ! ! xxx debug
    ! resp_maint = 0.0

  end function calc_resp_maint


  function calc_cexu( croot ) result( cexu )
    !/////////////////////////////////////////////////////////////////
    ! Constant exudation rate
    !-----------------------------------------------------------------
    ! arguments
    real, intent(in)           :: croot

    ! function return variable
    real :: cexu

    ! low-temperature ramp is included here to prevent negative C balance after exudation
    cexu = params_plant%exurate * croot

  end function calc_cexu


  !/////////////////////////////////////////////////////////////////////////
  ! Function to increase towards 1 when (damped) CUE approaches 0.
  !-------------------------------------------------------------------------    
  function calc_cue_stress( cue ) result( f_deactivate )
    ! arguments
    real, intent(in) :: cue

    ! function return variable
    real :: f_deactivate

    f_deactivate = 1.0 / (1.0 + exp(10.0 * (cue + 0.25)))
  
  end function

end module md_npp
