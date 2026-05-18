module md_allocation_cnmodel
  !////////////////////////////////////////////////////////////////
  ! Allocation of labile C and N to different plant pools based on
  ! functional balance.
  !----------------------------------------------------------------
  use md_params_core
  use md_classdefs
  use md_tile_cnmodel
  use md_plant_cnmodel
  use md_interface_cnmodel, only: myinterface

  implicit none

  private 
  public allocation_daily, getpar_modl_allocation

  !----------------------------------------------------------------
  ! Module-specific, private variables
  !----------------------------------------------------------------
  type statetype
    type(orgpool) :: pleaf
    type(orgpool) :: proot
    type(orgpool) :: plabl
    real          :: actnv_unitfapar
    integer       :: usepft
    real          :: soiltemp
    real          :: fpc_grid
    real          :: vcmax25_unitfapar
    real          :: nh4
    real          :: no3
    real          :: luep
    real          :: cn
    real          :: rduf
    real          :: rrum
  end type statetype

  type(statetype) :: state

  !-----------------------------------------------------------------------
  ! Module-specific model parameters
  !-----------------------------------------------------------------------
  type params_allocation_type
    real :: frac_leaf
    real :: frac_wood
    real :: frac_avl_labl
  end type params_allocation_type

  type( params_allocation_type ) :: params_allocation

  real :: test

contains

  subroutine allocation_daily( tile, tile_fluxes, climate, init )
    !//////////////////////////////////////////////////////////////////
    ! Finds optimal shoot:root growth ratio to balance C:N stoichiometry
    ! of a grass (no wood allocation).
    !------------------------------------------------------------------
    use md_forcing_cnmodel, only: climate_type
    use md_sofunutils, only: dampen_variability, calc_reg_line

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(climate_type), intent(in) :: climate
    logical, intent(in) :: init

    ! local variables
    real :: dcleaf
    real :: dnleaf
    real :: dcroot
    real :: dcwood 
    real :: dnwood
    real :: dcseed
    real :: dnseed
    real :: dnroot
    real :: drgrow
    real :: dclabl
    integer, save :: count_increasing, count_declining
    logical, save :: firstcall0 = .true.
    logical, save :: firstcall1 = .true.
    logical, save :: firstcall2 = .true.
    logical, save :: firstcall3 = .true.
    logical, save :: firstcall4 = .true.
    logical, save :: firstcall_resv = .true.

    integer :: lu
    integer :: pft
    integer :: idx
    integer :: usemoy        ! MOY in climate vectors to use for allocation
    integer :: usedoy        ! DOY in climate vectors to use for allocation
  
    type(orgpool) :: avl

    integer, parameter :: len_resp_vec = 30
    real, dimension(nlu,npft,len_resp_vec), save :: resp_vec
    real :: frac_for_resp

    real    :: req, c_req, n_req, c_acq, n_acq
    logical, save :: firstcall_cnbal = .true.
    integer, parameter :: len_cnbal_vec = 365

    real, dimension(nlu,npft,len_cnbal_vec), save :: g_net_vec, r_rex_vec, n_acq_vec, c_a_l_vec, c_a_r_vec, n_con_vec, c_a_s_vec

    real :: psi_c           ! return on leaf investment
    real :: psi_n           ! return on root investment
    real :: c_con           ! sum of C consumed to satisfy all biomass production and respiration, including growth respiration
    real :: n_con           ! sum of N consumed to satisfy all biomass production of past N days
    real :: r_ntoc_con      ! consumed N:C ratio (considering C allocation and respiration over N allocation)
    real :: n_exc           ! excess N acquisition over the past N days
    real :: n_con_corr      ! corrected sum of N consumed, after accounting for excess uptake left over from the imbalance of acquisition and utilization over the preceeeding N days
    real, save :: frac_leaf ! fraction of C allocated to leaves

    integer, parameter :: len_luep_vec = ndayyear
    integer, parameter :: len_rrum_vec = ndayyear
    integer, parameter :: len_rduf_vec = ndayyear
    integer, parameter :: len_cn_vec   = ndayyear
    
    real, dimension(nlu,npft,len_luep_vec), save :: luep_vec
    real, dimension(nlu,npft,len_rduf_vec), save :: rduf_vec
    real, dimension(nlu,npft,len_rrum_vec), save :: rrum_vec
    real, dimension(nlu,npft,len_cn_vec),   save :: cn_vec

    real, parameter :: f_seed = 0.0
    real, parameter :: par_resv = 0.1   ! scales reserves pool (controlling risk avoidance)
    real, parameter :: par_labl = 0.1   ! scales labile pool (controlling risk avoidance)
    real :: c_labl_target     ! target size of labile pool (g C m-2)
    real :: c_resv_target     ! target size of reserves pool (g C m-2)
    real :: f_resv_to_labl    ! net C flux from reserves to labile pool (g C m-2 tstep-1)
    real :: cfrac_resv        ! fraction of C pool moving from reserves to labile
    real :: cfrac_labl        ! fraction of C pool moving from labile to reserves
    type(orgpool) :: org_resv_to_labl ! organic mass moving from reserves to labile pool (g C[N] m-2 tstep-1)
    type(orgpool) :: org_labl_to_resv ! organic mass moving from labile to reserves pool (g C[N] m-2 tstep-1)

    real    :: max_dcleaf_n_constraint
    real    :: max_dcroot_n_constraint

    ! xxx debug
    real :: tmp
    real :: frac_leaf_test

    ! xxx verbose
    logical, parameter :: verbose = .true.

    if ( init .or. .not. myinterface%steering%dofree_alloc ) then
      frac_leaf = params_allocation%frac_leaf
    end if

    pftloop: do pft=1,npft
      lu = params_pft_plant(pft)%lu_category

      if ( tile(lu)%plant(pft)%plabl%c%c12 > eps .and. tile(lu)%plant(pft)%plabl%n%n14 > eps ) then

        if (params_pft_plant(pft)%grass) then
          !------------------------------------------------------------------
          ! Update leaf traits
          !------------------------------------------------------------------
          ! update
          if (tile(lu)%plant(pft)%pleaf%c%c12 == 0.0) then
            call get_leaftraits_init( tile(lu)%plant(pft) )
          end if

          !------------------------------------------------------------------
          ! Calculate maximum C allocatable based on current labile pool size
          !------------------------------------------------------------------
          avl = orgfrac(  params_allocation%frac_avl_labl &
                          * tile(lu)%plant(pft)%pheno%level_coldacclim &
                          * tile(lu)%plant(pft)%pheno%level_veggrowth, &
                          tile(lu)%plant(pft)%plabl )

          ! ! xxx debug
          ! tile_fluxes(lu)%plant(pft)%debug1 = params_allocation%frac_avl_labl
          ! tile_fluxes(lu)%plant(pft)%debug2 = tile(lu)%plant(pft)%pheno%level_coldacclim
          ! tile_fluxes(lu)%plant(pft)%debug3 = tile(lu)%plant(pft)%pheno%level_veggrowth
          ! tile_fluxes(lu)%plant(pft)%debug4 = tile(lu)%plant(pft)%plabl%c%c12

          ! additionally constrain allocatable C by available N, given the lower C:N of leaves or roots
          if ( myinterface%steering%dofree_alloc .and. myinterface%steering%closed_nbal ) then
            max_dcleaf_n_constraint = tile(lu)%plant(pft)%plabl%n%n14 * tile(lu)%plant(pft)%r_cton_leaf
            max_dcroot_n_constraint = tile(lu)%plant(pft)%plabl%n%n14 * params_pft_plant(pft)%r_cton_root ! should be obsolete as generally r_ntoc_leaf > r_ntoc_root
            avl%c%c12 = min(avl%c%c12, max_dcleaf_n_constraint, max_dcroot_n_constraint)
          end if

          !------------------------------------------------------------------
          ! Set allocation fraction to wood
          !------------------------------------------------------------------
          dcwood = params_allocation%frac_wood * avl%c%c12
          dnwood = dcwood * params_pft_plant(pft)%r_ntoc_wood
          avl%c%c12 = (1.0 - params_allocation%frac_wood) * avl%c%c12

          !------------------------------------------------------------------
          ! Set allocation to seeds
          !------------------------------------------------------------------
          dcseed = f_seed * params_plant%growtheff * avl%c%c12
          dnseed = min(avl%n%n14, dcseed * params_pft_plant(pft)%r_ntoc_seed)

          !------------------------------------------------------------------
          ! Set remaining allocation fractions to leaves and roots (after wood)
          !------------------------------------------------------------------
          ! print*,'clabl, avl, frac_leaf: ', tile(lu)%plant(pft)%plabl%c%c12, avl%c%c12, frac_leaf

          ! leaf allocation fraction was determined in the previous time step (dcleaf is a "save variable")
          dcleaf = frac_leaf         * params_plant%growtheff * avl%c%c12
          dcroot = (1.0 - frac_leaf) * params_plant%growtheff * avl%c%c12
          dnroot = dcroot * params_pft_plant(pft)%r_ntoc_root

          ! print*,'dcleaf, dcroot ', dcleaf, dcroot

          tile_fluxes(lu)%plant(pft)%debug4 = tile(lu)%plant(pft)%pheno%level_veggrowth 

          !-------------------------------------------------------------------
          ! SEED ALLOCATION
          !-------------------------------------------------------------------
          call orgmv( orgpool( carbon( dcseed ), nitrogen( dnseed )  ) , &
                      tile(lu)%plant(pft)%plabl, &
                      tile(lu)%plant(pft)%pseed &
                      )
          
          ! ... and remove growth respiration from labile C, update growth respiration
          dclabl = (1.0 / params_plant%growtheff) * dcseed
          tile(lu)%plant(pft)%plabl%c%c12 = tile(lu)%plant(pft)%plabl%c%c12 - dclabl
          tile_fluxes(lu)%plant(pft)%drgrow = tile_fluxes(lu)%plant(pft)%drgrow + dclabl - dcseed

          !-------------------------------------------------------------------
          ! WOOD ALLOCATION
          !-------------------------------------------------------------------
          if (dcwood > 0.0) then

            call allocate_wood( &
              pft, &
              dcwood, &
              dnroot, &
              tile(lu)%plant(pft)%pwood%c%c12, &
              tile(lu)%plant(pft)%pwood%n%n14, &
              tile(lu)%plant(pft)%plabl%c%c12, &
              tile(lu)%plant(pft)%plabl%n%n14, &
              tile_fluxes(lu)%plant(pft)%drgrow, &
              myinterface%steering%closed_nbal, &
              tile_fluxes(lu)%plant(pft)%dnup_fix &
              )

          end if

          !-------------------------------------------------------------------
          ! LEAF ALLOCATION
          !-------------------------------------------------------------------
          if (dcleaf > 0.0) then

            call allocate_leaf( &
              pft, &
              dcleaf, &
              tile(lu)%plant(pft)%pleaf%c%c12, &
              tile(lu)%plant(pft)%pleaf%n%n14, &
              tile(lu)%plant(pft)%plabl%c%c12, &
              tile(lu)%plant(pft)%plabl%n%n14, &
              tile_fluxes(lu)%plant(pft)%drgrow, &
              tile(lu)%plant(pft)%actnv_unitfapar, &
              tile(lu)%plant(pft)%lai_ind, &
              dnleaf, &
              myinterface%steering%closed_nbal, &
              tile_fluxes(lu)%plant(pft)%dnup_fix &
              )

            !-------------------------------------------------------------------  
            ! Update leaf traits, given updated LAI and fAPAR (leaf N is consistent with plant%narea_canopy)
            !------------------------------------------------------------------- 
            tile(lu)%plant(pft)%fapar_ind = get_fapar( tile(lu)%plant(pft)%lai_ind )
            call update_leaftraits( tile(lu)%plant(pft) )

            !-------------------------------------------------------------------  
            ! If labile N gets negative, account gap as N fixation
            !-------------------------------------------------------------------  
            if ( tile(lu)%plant(pft)%plabl%n%n14 < 0.0 ) then
              ! stop 'labile N got negative'
              req = 2.0 * abs(tile(lu)%plant(pft)%plabl%n%n14) ! give it a bit more (factor 2)
              tile_fluxes(lu)%plant(pft)%dnup%n14 = tile_fluxes(lu)%plant(pft)%dnup%n14 + req
              tile_fluxes(lu)%plant(pft)%dnup_fix = tile_fluxes(lu)%plant(pft)%dnup_fix + req
              tile(lu)%plant(pft)%plabl%n%n14 = tile(lu)%plant(pft)%plabl%n%n14 + req
            end if

          end if

          !-------------------------------------------------------------------
          ! ROOT ALLOCATION
          !-------------------------------------------------------------------
          if (dcroot > 0.0) then

            call allocate_root( &
              pft, &
              dcroot, &
              dnroot, &
              tile(lu)%plant(pft)%proot%c%c12, &
              tile(lu)%plant(pft)%proot%n%n14, &
              tile(lu)%plant(pft)%plabl%c%c12, &
              tile(lu)%plant(pft)%plabl%n%n14, &
              tile_fluxes(lu)%plant(pft)%drgrow, &
              myinterface%steering%closed_nbal, &
              tile_fluxes(lu)%plant(pft)%dnup_fix &
              )

            !-------------------------------------------------------------------  
            ! If labile N gets negative, account gap as N fixation
            !-------------------------------------------------------------------  
            if ( tile(lu)%plant(pft)%plabl%n%n14 < 0.0 ) then
              ! stop 'labile N got negative'
              req = 2.0 * abs(tile(lu)%plant(pft)%plabl%n%n14) ! give it a bit more (factor 2)
              tile_fluxes(lu)%plant(pft)%dnup%n14 = tile_fluxes(lu)%plant(pft)%dnup%n14 + req
              tile_fluxes(lu)%plant(pft)%dnup_fix = tile_fluxes(lu)%plant(pft)%dnup_fix + req
              tile(lu)%plant(pft)%plabl%n%n14 = tile(lu)%plant(pft)%plabl%n%n14 + req
            end if

          end if

        else

          stop 'allocation_daily not implemented for trees'

        end if

      else

          dcseed = 0.0
          dnseed = 0.0
          dcleaf = 0.0
          dcroot = 0.0
          dnleaf = 0.0
          dnroot = 0.0
          drgrow = 0.0

      end if

      !-------------------------------------------------------------------
      ! Record acquired and required C and N
      !-------------------------------------------------------------------
      ! record values over preceeding len_cnbal_vec (365) days
      if (firstcall_cnbal) then

        g_net_vec(lu,pft,:) = tile_fluxes(lu)%plant(pft)%dgpp - tile_fluxes(lu)%plant(pft)%drd
        r_rex_vec(lu,pft,:) = tile_fluxes(lu)%plant(pft)%drroot &
                              + tile_fluxes(lu)%plant(pft)%drsapw &
                              + tile_fluxes(lu)%plant(pft)%dcex
        n_acq_vec(lu,pft,:) = tile_fluxes(lu)%plant(pft)%dnup%n14
        c_a_l_vec(lu,pft,:) = dcleaf
        c_a_r_vec(lu,pft,:) = dcroot
        c_a_s_vec(lu,pft,:) = dcseed
        n_con_vec(lu,pft,:) = dnleaf + dnroot ! + dnseed

        if (pft == npft .and. lu == nlu) firstcall_cnbal = .false.

      else

        g_net_vec(lu,pft,1:(len_cnbal_vec-1)) = g_net_vec(lu,pft,2:len_cnbal_vec)
        r_rex_vec(lu,pft,1:(len_cnbal_vec-1)) = r_rex_vec(lu,pft,2:len_cnbal_vec)
        n_acq_vec(lu,pft,1:(len_cnbal_vec-1)) = n_acq_vec(lu,pft,2:len_cnbal_vec)
        c_a_l_vec(lu,pft,1:(len_cnbal_vec-1)) = c_a_l_vec(lu,pft,2:len_cnbal_vec)
        c_a_r_vec(lu,pft,1:(len_cnbal_vec-1)) = c_a_r_vec(lu,pft,2:len_cnbal_vec)
        c_a_s_vec(lu,pft,1:(len_cnbal_vec-1)) = c_a_s_vec(lu,pft,2:len_cnbal_vec)
        n_con_vec(lu,pft,1:(len_cnbal_vec-1)) = n_con_vec(lu,pft,2:len_cnbal_vec)

        g_net_vec(lu,pft,len_cnbal_vec) = tile_fluxes(lu)%plant(pft)%dgpp - tile_fluxes(lu)%plant(pft)%drd
        r_rex_vec(lu,pft,len_cnbal_vec) = tile_fluxes(lu)%plant(pft)%drroot &
                                          + tile_fluxes(lu)%plant(pft)%drsapw &
                                          + tile_fluxes(lu)%plant(pft)%dcex
        n_acq_vec(lu,pft,len_cnbal_vec) = tile_fluxes(lu)%plant(pft)%dnup%n14
        c_a_l_vec(lu,pft,len_cnbal_vec) = dcleaf
        c_a_r_vec(lu,pft,len_cnbal_vec) = dcroot
        c_a_s_vec(lu,pft,len_cnbal_vec) = dcseed
        n_con_vec(lu,pft,len_cnbal_vec) = dnleaf + dnroot ! + dnseed

      end if


      ! return on leaf investment, defined as sum of C assimilated (after leaf dark respiration, but before exudation, root and other respiration) 
      ! divided by sum over C invested into leaf construction (ignoring growth respiration)
      psi_c = sum( g_net_vec(lu,pft,:) ) / sum( c_a_l_vec(lu,pft,:) )

      ! return on root investment, defined as sum N acquisition divided by sum of C invested into root construction (ignoring growth respiration)
      psi_n = sum( n_acq_vec(lu,pft,:) ) / sum( c_a_r_vec(lu,pft,:) )

      ! sum of C consumed to satisfy all biomass production and respiration, including growth respiration, of past N days
      c_con = (1.0 / params_plant%growtheff) * sum( c_a_l_vec(lu,pft,:) + c_a_r_vec(lu,pft,:) + c_a_s_vec(lu,pft,:) ) &
              + sum( r_rex_vec(lu,pft,:) )

      ! sum of N consumed to satisfy all biomass production of past N days minus excess N
      n_con = sum( n_con_vec(lu,pft,:) )

      ! consumed N:C ratio (considering C allocation and respiration over N allocation), mean over preceeding N days
      r_ntoc_con = n_con / c_con

      ! excess N acquisition over the past N days
      n_exc = sum( n_acq_vec(lu,pft,:) ) - sum( g_net_vec(lu,pft,:) ) * r_ntoc_con
      ! tile_fluxes(lu)%plant(pft)%debug3 = n_exc

      ! corrected sum of N consumed, after accounting for excess uptake left over from the imbalance of acquisition and utilization over the preceeeding N days
      n_con_corr = n_con - n_exc

      ! determine balance (fraction of allocation to leaves)
      ! psi_c * x * growtheff * c_avl / (psi_n * (1-x) * growtheff * c_avl) = c_consumed / (n_consumed - n_excess)
      ! => solve for x (frac_leaf below)
      ! c_consumed is c_req; n_consumed is n_con
      if ( myinterface%steering%dofree_alloc ) then
        ! frac_leaf = 1.0 / (psi_c * n_con_corr / (psi_n * c_con) + 1.0)

        ! with wood allocation:
        frac_leaf = psi_n * c_con * (1.0 - params_allocation%frac_wood) / (psi_c * n_con_corr + psi_n * c_con)
      end if
      ! tile_fluxes(lu)%plant(pft)%debug3 = 1.0 / (psi_c * n_con_corr / (psi_n * c_con) + 1.0)

      ! ! compare C:N aquired and required at the level of GPP (C required includes C for respiration)
      ! print*,'C:N acquired:', sum( g_net_vec(lu,pft,:) ) / sum( n_acq_vec(lu,pft,:) ), 'C:N required: ', c_con / n_con

      ! C:N acquired:
      ! tile_fluxes(lu)%plant(pft)%debug1 = sum( g_net_vec(lu,pft,:) ) / sum( n_acq_vec(lu,pft,:) )

      ! C:N required
      ! tile_fluxes(lu)%plant(pft)%debug2 = c_con / n_con

      ! XXX n_exc has a problem!
      ! tile_fluxes(lu)%plant(pft)%debug4 = n_exc

      !-------------------------------------------------------------------------
      ! RESERVES - LABILE POOL DYNAMICS
      ! Adopted from the QUINCY model (Thum et al., 2019 GMD; Eqs. S40-S42)
      ! See also vignettes/reserves_labile.Rmd
      !-------------------------------------------------------------------------
      ! The target labile pool size is determined by the C requirement for satisfying
      ! N days of respiration and exudation (averaged over len_cnbal_vec).
      c_labl_target = par_labl * sum( r_rex_vec(lu,pft,:) )

      ! The target reserves pool size is determined by the cumulative amount of C allocated
      ! to leaves, roots, and seeds
      c_resv_target = par_resv * (sum( c_a_l_vec(lu,pft,:) ) &
                      + sum( c_a_r_vec(lu,pft,:) ) &
                      + sum( c_a_s_vec(lu,pft,:) ) &
                      )

      ! ! during reserves spinup phase, keep them at their target value all the time
      ! if (.not. myinterface%steering%spinup_reserves) then
      !   tile(lu)%plant(pft)%presv%c%c12 = c_resv_target
      !   if (pft == npft .and. lu == nlu) firstcall_resv = .false.
      ! end if      

      ! net C flux from reserves to labile pool
      f_resv_to_labl = calc_f_reserves_labile(&
        tile(lu)%plant(pft)%plabl%c%c12, &
        tile(lu)%plant(pft)%presv%c%c12, &
        c_labl_target, &
        c_resv_target &
        )

      ! Assume that the N flux goes in proportion with the C flux, depending on the source pool
      ! If f_resv_to_labl is positive, the source pool is presv.
      ! If f_resv_to_labl is negative, the source pool is plabl.
      if (f_resv_to_labl > 0.0 .and. c_labl_target > 0.0) then

        ! transfer C and N to labile, source pool is presv
        org_resv_to_labl = orgfrac(f_resv_to_labl, tile(lu)%plant(pft)%presv)
        call orginit(org_labl_to_resv)

        ! warning: no isotopes treatable like this
        tile(lu)%plant(pft)%plabl%c%c12 = tile(lu)%plant(pft)%plabl%c%c12 + org_resv_to_labl%c%c12
        tile(lu)%plant(pft)%plabl%n%n14 = tile(lu)%plant(pft)%plabl%n%n14 + org_resv_to_labl%n%n14
        if (.not. myinterface%steering%spinup_reserves) then
          tile(lu)%plant(pft)%presv%c%c12 = tile(lu)%plant(pft)%presv%c%c12 - org_resv_to_labl%c%c12
          tile(lu)%plant(pft)%presv%n%n14 = tile(lu)%plant(pft)%presv%n%n14 - org_resv_to_labl%n%n14
        end if
        
      else if (c_resv_target > 0.0) then

        ! transfer C and N to reserves, source pool is plabl
        org_labl_to_resv = orgfrac((-1.0) * f_resv_to_labl, tile(lu)%plant(pft)%plabl)
        call orginit(org_resv_to_labl)

        ! warning: no isotopes treatable like this
        tile(lu)%plant(pft)%presv%c%c12 = tile(lu)%plant(pft)%presv%c%c12 + org_labl_to_resv%c%c12
        tile(lu)%plant(pft)%presv%n%n14 = tile(lu)%plant(pft)%presv%n%n14 + org_labl_to_resv%n%n14
        if (.not. myinterface%steering%spinup_reserves) then
          tile(lu)%plant(pft)%plabl%c%c12 = tile(lu)%plant(pft)%plabl%c%c12 - org_labl_to_resv%c%c12
          tile(lu)%plant(pft)%plabl%n%n14 = tile(lu)%plant(pft)%plabl%n%n14 - org_labl_to_resv%n%n14
        end if

      end if

      ! tile_fluxes(lu)%plant(pft)%debug3 = tile(lu)%plant(pft)%presv%c%c12

      !-------------------------------------------------------------------
      ! Adjust NPP for growth respiration
      !-------------------------------------------------------------------
      tile_fluxes(lu)%plant(pft)%dnpp = cminus( tile_fluxes(lu)%plant(pft)%dnpp, carbon( tile_fluxes(lu)%plant(pft)%drgrow ) )

      ! ! record for today
      ! tile_fluxes(lu)%plant(pft)%alloc_leaf = orgpool( carbon( dcleaf ), nitrogen( dnleaf ) )
      ! tile_fluxes(lu)%plant(pft)%alloc_root = orgpool( carbon( dcroot ), nitrogen( dnroot ) )
      ! tile_fluxes(lu)%plant(pft)%alloc_seed = orgpool( carbon( dcseed ), nitrogen( dnseed ) )
      ! call orginit( tile_fluxes(lu)%plant(pft)%alloc_sapw )
      ! call orginit( tile_fluxes(lu)%plant(pft)%alloc_wood )

      !-------------------------------------------------------------------
      ! Record for output
      !-------------------------------------------------------------------
      tile_fluxes(lu)%plant(pft)%npp_leaf = orgpool( carbon(dcleaf), nitrogen( dnleaf ) )
      tile_fluxes(lu)%plant(pft)%npp_root = orgpool( carbon(dcroot), nitrogen( dnroot ) )
      tile_fluxes(lu)%plant(pft)%npp_wood = orgpool( carbon(dcwood), nitrogen( dnwood ) )
      tile_fluxes(lu)%plant(pft)%npp_seed = orgpool( carbon(dcseed), nitrogen( dnseed ) )

      ! provisional BP
      tile_fluxes(lu)%plant(pft)%debug2 = dcleaf + dcroot + dcwood + dcseed

    end do pftloop

  end subroutine allocation_daily


  subroutine allocate_leaf( pft, mydcleaf, cleaf, nleaf, clabl, nlabl, rgrow, actnv_unitfapar, lai, mydnleaf, closed_nbal, nfix )
    !///////////////////////////////////////////////////////////////////
    ! LEAF ALLOCATION
    ! Sequence of steps:
    ! - increment foliage C pool
    ! - update LAI
    ! - calculate canopy-level foliage N as a function of LAI 
    ! - reduce labile pool by C and N increments
    !-------------------------------------------------------------------
    ! arguments
    integer, intent(in)  :: pft
    real, intent(in)     :: mydcleaf
    real, intent(inout)  :: cleaf, nleaf
    real, intent(inout)  :: clabl, nlabl
    real, intent(inout)  :: rgrow
    real, intent(in)     :: actnv_unitfapar
    real, intent(out)    :: lai
    real, intent(out)    :: mydnleaf
    logical, intent(in)  :: closed_nbal
    real, intent(inout)  :: nfix

    ! local variables
    real :: nleaf0
    real :: dclabl, dnlabl

    ! xxx debug
    real :: cleaf0

    cleaf0 = cleaf

    ! Calculate LAI as a function of leaf C
    cleaf  = cleaf + mydcleaf
    lai = get_lai( pft, cleaf, actnv_unitfapar )

    ! calculate canopy-level leaf N as a function of LAI
    nleaf0   = nleaf

    nleaf    = get_leaf_n_canopy( pft, lai, actnv_unitfapar )

    mydnleaf = nleaf - nleaf0

    ! depletion of labile C pool is enhanced by growth respiration
    dclabl = (1.0 / params_plant%growtheff) * mydcleaf

    ! growth respiration
    rgrow = rgrow + dclabl - mydcleaf

    ! substract from labile pools
    clabl  = clabl - dclabl
    nlabl  = nlabl - mydnleaf

    if ( clabl < -1.0 * eps ) then
      stop 'ALLOCATE_LEAF: trying to remove too much from labile pool: leaf C'
    else if ( clabl < 0.0 ) then
      ! numerical imprecision
      ! print*,'numerical imprecision?'
      ! print*,'clabl ', clabl
      ! stop 'allocate leaf'
      clabl = 0.0
    end if

    if (closed_nbal) then
      if ( nlabl < -1.0 * eps ) then
        print*,'dcleaf       ', mydcleaf
        print*,'cleaf before ', cleaf0
        print*,'cleaf after  ', cleaf
        print*,'nleaf before ', nleaf0
        print*,'nleaf after  ', nleaf
        print*,'C:N before   ', cleaf0 / nleaf0
        print*,'C:N after    ', cleaf / nleaf
        print*,'nlabl = ', nlabl
        stop 'ALLOCATE_LEAF: trying to remove too much from labile pool: leaf N'
      else if ( nlabl < 0.0 ) then
        ! more N used for leaf growth than available in labile N pool
        ! assume an implied (unspecified) N source, accounted as N fixation
        nfix = nfix - nlabl
        nlabl = 0.0
      end if
    end if  

  end subroutine allocate_leaf


  subroutine allocate_root( pft, mydcroot, mydnroot, croot, nroot, clabl, nlabl, rgrow, closed_nbal, nfix )
    !///////////////////////////////////////////////////////////////////
    ! ROOT ALLOCATION
    ! Sequence of steps:
    ! - determine root C and N increments based on remaining labile C and N
    ! - update root C and N
    ! - update labile C and N
    !-------------------------------------------------------------------
    ! arguments
    integer, intent(in) :: pft
    real, intent(in)   :: mydcroot
    real, intent(in)   :: mydnroot
    real, intent(inout) :: croot, nroot
    real, intent(inout) :: clabl, nlabl
    real, intent(inout)  :: rgrow
    logical, intent(in) :: closed_nbal
    real, intent(inout)  :: nfix

    ! local variables
    real :: dclabl

    ! update root pools
    croot = croot + mydcroot
    nroot = nroot + mydnroot

    ! depletion of labile C pool is enhanced by growth respiration
    dclabl = (1.0 / params_plant%growtheff) * mydcroot

    ! substract from labile pools
    clabl  = clabl - dclabl
    nlabl  = nlabl - mydnroot

    ! growth respiration
    rgrow = rgrow + dclabl - mydcroot

    if ( clabl < -1.0 * eps ) then
      ! print*,'clabl after root allocation: ', clabl
      stop 'ALLOCATE_ROOT: trying to remove too much from labile pool: root C'
    else if ( clabl < 0.0 ) then
      ! numerical imprecision
      ! print*,'numerical imprecision?'
      ! stop 'allocate root'
      clabl = 0.0
    end if

    if (closed_nbal) then
      if ( nlabl < -1.0 * eps ) then
        stop 'ALLOCATE_ROOT: trying to remove too much from labile pool: root N'
      else if ( nlabl < 0.0 ) then
        ! more N used for root growth than available in labile N pool
        ! assume an implied (unspecified) N source, accounted as N fixation
        nfix = nfix - nlabl
        nlabl = 0.0
      end if
    end if

  end subroutine allocate_root


  subroutine allocate_wood( pft, mydcwood, mydnwood, cwood, nwood, clabl, nlabl, rgrow, closed_nbal, nfix )
    !///////////////////////////////////////////////////////////////////
    ! WOOD ALLOCATION
    ! Sequence of steps:
    ! - determine wood C and N increments based on remaining labile C and N
    ! - update wood C and N
    ! - update labile C and N
    !-------------------------------------------------------------------
    ! arguments
    integer, intent(in) :: pft
    real, intent(in)   :: mydcwood
    real, intent(in)   :: mydnwood
    real, intent(inout) :: cwood, nwood
    real, intent(inout) :: clabl, nlabl
    real, intent(inout)  :: rgrow
    logical, intent(in) :: closed_nbal
    real, intent(inout)  :: nfix

    ! local variables
    real :: dclabl

    ! update wood pools
    cwood = cwood + mydcwood
    nwood = nwood + mydnwood

    ! depletion of labile C pool is enhanced by growth respiration
    dclabl = (1.0 / params_plant%growtheff) * mydcwood

    ! substract from labile pools
    clabl  = clabl - dclabl
    nlabl  = nlabl - mydnwood

    ! growth respiration
    rgrow = rgrow + dclabl - mydcwood

    if ( clabl < -1.0 * eps ) then
      stop 'ALLOCATE_WOOD: trying to remove too much from labile pool: wood C'
    else if ( clabl < 0.0 ) then
      ! numerical imprecision
      ! print*,'numerical imprecision?'
      ! stop 'allocate wood'
      clabl = 0.0
    end if

    if (closed_nbal) then
      if ( nlabl < -1.0 * eps ) then
        stop 'ALLOCATE_WOOD: trying to remove too much from labile pool: wood N'
      else if ( nlabl < 0.0 ) then
        ! more N used for wood growth than available in labile N pool
        ! assume an implied (unspecified) N source, accounted as N fixation
        nfix = nfix - nlabl
        nlabl = 0.0
      end if
    end if

  end subroutine allocate_wood


  function calc_ft_growth( xx ) result( yy )
    !////////////////////////////////////////////////////////////////
    ! Temperature limitation function to growth. Increases from around 
    ! 0 at 0 deg C to 1 at around 10 deg C. The factor scales the 
    ! amount of C that becomes available for growth.
    !----------------------------------------------------------------
    ! arguments
    real, intent(in) :: xx  ! air temperature (deg C)

    ! function return variable
    real :: yy

    yy = 1.0 / (1.0 + exp(-1.0 * (xx - 5.0)))

    ! ! xxx try turn off temperature limitation on growth
    ! yy = 1.0

  end function calc_ft_growth


  function calc_l2r(c_labl, c_resv, c_labl_target, c_resv_target, f_max) result( out )
    !///////////////////////////////////////////////////////////
    ! Function for trickling from labile to reserves
    ! Returns the fraction of the labile pool (is source pool) 
    ! moving from labile to reserves.
    !-----------------------------------------------------------
    real, intent(in) :: c_labl, c_resv, c_labl_target, c_resv_target, f_max
    real, parameter :: lambda_store = 10.0
    real :: out
    out = max(0.0, 1.0 - c_resv / c_resv_target) * f_max / (1.0 + exp(-lambda_store * (c_labl / c_labl_target - 1.5)))
  end function calc_l2r


  function calc_r2l(c_labl, c_resv, c_labl_target, c_resv_target, f_max) result( out )
    !///////////////////////////////////////////////////////////
    ! Function for refilling labile pool
    ! Returns the fraction of the reserves pool (is source pool)
    ! moving from reserves to labile
    !-----------------------------------------------------------
    real, intent(in) :: c_labl, c_resv, c_labl_target, c_resv_target, f_max
    real, parameter :: lambda_store = 10.0
    real, parameter :: k_store = 3.0
    real :: out
    out = c_resv / c_resv_target * f_max * exp(-(lambda_store * c_labl / c_labl_target)**k_store)
  end function calc_r2l


  function calc_f_reserves_labile(c_labl, c_resv, c_labl_target, c_resv_target) result( out )
    !///////////////////////////////////////////////////////////
    ! calculates the fraction of the source pool trickling to the other pool
    ! source pool is reserves if the net is positive, and labile if the net is negative
    !-----------------------------------------------------------
    real, intent(in) :: c_labl, c_resv, c_labl_target, c_resv_target
    real :: out, r2l, l2r
    real, parameter :: f_max = 0.02

    r2l = calc_r2l(c_labl, c_resv, c_labl_target, c_resv_target, f_max = f_max)
    l2r = calc_l2r(c_labl, c_resv, c_labl_target, c_resv_target, f_max = f_max)
    out = r2l - l2r

  end function calc_f_reserves_labile


  subroutine getpar_modl_allocation()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads module-specific parameters
    !----------------------------------------------------------------
    use md_interface_cnmodel, only: myinterface

    ! maximum nitrification rate
    params_allocation%frac_leaf     = myinterface%params_calib%frac_leaf
    params_allocation%frac_wood     = myinterface%params_calib%frac_wood
    params_allocation%frac_avl_labl = myinterface%params_calib%frac_avl_labl

  end subroutine getpar_modl_allocation


  ! function get_rcton_init( pft, meanmppfd, nv ) result( rcton )
  !   !////////////////////////////////////////////////////////////////
  !   ! Calculates initial guess based on Taylor approximation of 
  !   ! Cleaf and Nleaf function around cleaf=0.
  !   ! Cleaf = c_molmass * params_pft_plant(pft)%r_ctostructn_leaf * [ meanmppfd * (1-exp(-kbeer*LAI)) * nv * params_pft_plant(pft)%r_n_cw_v + LAI * params_pft_plant(pft)%ncw_min ]
  !   ! Nleaf = n_molmass * [ meanmppfd * (1-exp(-kbeer*LAI)) * nv * (params_pft_plant(pft)%r_n_cw_v + 1) + LAI * params_pft_plant(pft)%ncw_min ]
  !   ! linearization around LAI = 0 ==> (1-exp(-k*L)) ~= k*L
  !   ! ==> Cleaf ~= LAI * c_molmass * params_pft_plant(pft)%r_ctostructn_leaf * ( meanmppfd * kbeer * nv * params_pft_plant(pft)%r_n_cw_v + params_pft_plant(pft)%ncw_min )
  !   ! ==> Nleaf ~= LAI * n_molmass * ( meanmppfd * kbeer * nv * (params_pft_plant(pft)%r_n_cw_v + 1) + params_pft_plant(pft)%ncw_min )
  !   ! r_cton = Cleaf / Nleaf
  !   !----------------------------------------------------------------
  !   ! use md_params_core, only: nmonth

  !   ! arguments
  !   integer, intent(in)                 :: pft
  !   real, dimension(nmonth), intent(in) :: meanmppfd
  !   real, dimension(nmonth), intent(in) :: nv

  !   ! function return variable
  !   real :: rcton

  !   ! local variables
  !   real :: maxnv
  !   real :: tmp1, tmp2, tmp3

  !   ! Metabolic N is predicted and is optimised at a monthly time scale. 
  !   ! Leaf traits are calculated based on metabolic N => cellwall N => cellwall C / LMA
  !   ! Leaves get thinner at the bottom of the canopy => increasing LAI through the season comes at a declining C and N cost
  !   ! Monthly variations in metabolic N, determined by variations in meanmppfd and nv should not result in variations in leaf traits. 
  !   ! In order to prevent this, assume annual maximum metabolic N, part of which is deactivated during months with lower insolation (and Rd reduced.)
  !   maxnv = maxval( meanmppfd(:) * nv(:) )

  !   ! tmp1 = c_molmass * params_pft_plant(pft)%r_ctostructn_leaf
  !   ! tmp2 = maxnv * params_plant%kbeer * params_pft_plant(pft)%r_n_cw_v + params_pft_plant(pft)%ncw_min
  !   ! tmp3 = n_molmass * ( maxnv * params_plant%kbeer * ( params_pft_plant(pft)%r_n_cw_v + 1.0 ) + params_pft_plant(pft)%ncw_min )
  !   ! rcton = tmp1 * tmp2 / tmp3

  !   rcton = ( c_molmass * params_pft_plant(pft)%r_ctostructn_leaf * &
  !     ( maxnv * params_plant%kbeer * params_pft_plant(pft)%r_n_cw_v + params_pft_plant(pft)%ncw_min ) &
  !     ) / ( n_molmass * ( maxnv * params_plant%kbeer * ( params_pft_plant(pft)%r_n_cw_v + 1.0 ) + params_pft_plant(pft)%ncw_min ) )

  ! end function get_rcton_init


end module md_allocation_cnmodel
