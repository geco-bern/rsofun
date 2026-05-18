module md_npp
  !////////////////////////////////////////////////////////////////
  ! Autotrophic respiration and labile C balance
  !----------------------------------------------------------------
  use md_classdefs
  use md_params_core, only: npft, ndayyear, eps, nlu
  use md_tile_cnmodel
  use md_plant_cnmodel
  use md_forcing_cnmodel, only: climate_type, vegcover_type
  use md_interface_cnmodel, only: myinterface

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
    real :: cavl, creq, frac_avl
    real, parameter :: buffer = 0.9
    type(orgpool) :: org_resv_to_labl ! organic mass moving from reserves to labile pool (g C[N] m-2 tstep-1)
    real :: f_resv_to_labl

    pftloop: do pft=1,npft

      lu = params_pft_plant(pft)%lu_category

      !/////////////////////////////////////////////////////////////////////////
      ! LABILE POOL UPDATE
      ! Gross CO2 assimilation (GPP) is first added to the labile pool. Then, 
      ! C required for maintenance is determined. If labile pool is insufficient
      ! to satisfy respiration, leaf and fine root mass is reduced accordingly.
      !-------------------------------------------------------------------------
      ! first add GPP to labile pool
      tile(lu)%plant(pft)%plabl%c%c12 = tile(lu)%plant(pft)%plabl%c%c12 &
                                      + tile_fluxes(lu)%plant(pft)%dgpp

      ! determine C required for respiration
      creq = tile_fluxes(lu)%plant(pft)%drd &
           + calc_resp_maint( tile(lu)%plant(pft)%proot%c%c12, &
                              params_plant%r_root, &
                              climate%dtemp &
                              ) &
           + calc_resp_maint( tile(lu)%plant(pft)%pwood%c%c12, &  ! will have to do this more clever, using Huiying's approach
                              params_plant%r_sapw, &
                              climate%dtemp &
                              ) &
           + calc_cexu( tile(lu)%plant(pft)%proot%c%c12 )

      ! available C in labile pool
      cavl = tile(lu)%plant(pft)%plabl%c%c12

      !-------------------------------------------------------------------------
      ! When insufficient C is available in labile pool refill labile pool using 
      ! reserves.
      !-------------------------------------------------------------------------
      if (cavl < creq) then

        ! Buffer is introduced for safety - to avoid "over-depletion" of labile pool.
        frac_avl = buffer * (cavl / creq)

        ! transfer required C from reserves to labile
        f_resv_to_labl = creq - cavl
        print*,'refilling labile, frac_avl ', frac_avl
        tile(lu)%plant(pft)%plabl%c%c12 = tile(lu)%plant(pft)%plabl%c%c12 + f_resv_to_labl
        if (.not. myinterface%steering%spinup_reserves) then
          tile(lu)%plant(pft)%presv%c%c12 = tile(lu)%plant(pft)%presv%c%c12 - f_resv_to_labl
        end if

      else

        ! if enough labile C is available, don't reduce leaf and root biomass
        frac_avl = 1.0

      end if

      !/////////////////////////////////////////////////////////////////////////
      ! MAINTENANCE RESPIRATION
      ! use function 'resp_main'
      !-------------------------------------------------------------------------
      ! leaf respiration is given by dark respiration as calculated in P-model.
      ! Simplification: is linearly reduced with frac_avl.
      tile_fluxes(lu)%plant(pft)%drleaf = frac_avl * tile_fluxes(lu)%plant(pft)%drd
      tile_fluxes(lu)%plant(pft)%drroot = frac_avl * &
                                            calc_resp_maint(  tile(lu)%plant(pft)%proot%c%c12, &
                                                              params_plant%r_root, &
                                                              climate%dtemp &
                                                              )
      if (params_pft_plant(pft)%tree) then
        tile_fluxes(lu)%plant(pft)%drsapw = frac_avl * &
                                              ! not distinguishing sapwood from wood
                                              calc_resp_maint(  tile(lu)%plant(pft)%pwood%c%c12, &
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
      tile_fluxes(lu)%plant(pft)%dcex = frac_avl * calc_cexu( tile(lu)%plant(pft)%proot%c%c12 )

      ! add root C export to respective (fast-decaying pool)
      tile(lu)%soil%pexud%c12 = tile(lu)%soil%pexud%c12 + tile_fluxes(lu)%plant(pft)%dcex

      !/////////////////////////////////////////////////////////////////////////
      ! NPP and LABILE POOL UPDATE
      ! After assimilated C is added above, C required for respiration is 
      ! immediately removed
      !-------------------------------------------------------------------------
      tile(lu)%plant(pft)%plabl%c%c12 = tile(lu)%plant(pft)%plabl%c%c12   &
                                        - tile_fluxes(lu)%plant(pft)%drleaf &
                                        - tile_fluxes(lu)%plant(pft)%drroot &
                                        - tile_fluxes(lu)%plant(pft)%drsapw & 
                                        - tile_fluxes(lu)%plant(pft)%dcex

      tile_fluxes(lu)%plant(pft)%dnpp = carbon( tile_fluxes(lu)%plant(pft)%dgpp &
                                              - tile_fluxes(lu)%plant(pft)%drleaf &
                                              - tile_fluxes(lu)%plant(pft)%drroot &
                                              - tile_fluxes(lu)%plant(pft)%drsapw &
                                                )

      if (tile(lu)%plant(pft)%plabl%c%c12 < (-1) * eps) stop 'after npp labile C is neg.'
      if (tile(lu)%plant(pft)%plabl%n%n14 < (-1) * eps) stop 'after npp labile N is neg.'

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
