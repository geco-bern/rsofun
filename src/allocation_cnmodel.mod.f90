module md_allocation_cnmodel
  !////////////////////////////////////////////////////////////////
  ! ALLOCATION MODULE
  ! Contains the "main" subroutine 'allocation_daily' and all 
  ! necessary subroutines for handling input/output, and auxiliary
  ! subroutines.
  ! Every module that implements 'allocation_daily' must contain 
  ! this list of subroutines (names that way).
  !   - allocation_daily
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  use md_params_core
  use md_classdefs
  use md_tile
  use md_plant
  use md_interface_pmodel, only: myinterface
  use md_gpp_pmodel, only: calc_dgpp, calc_drd, params_gpp
  use md_npp, only: calc_resp_maint, calc_cexu
  use md_photosynth, only: calc_ftemp_inst_rd

  implicit none

  private 
  public allocation_daily

  !----------------------------------------------------------------
  ! MODULE-SPECIFIC, PRIVATE VARIABLES
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

  ! logical, parameter :: write_logfile_eval_imbalance = .false.
  real :: test

contains

  subroutine allocation_daily( tile, tile_fluxes, climate, year )
    !//////////////////////////////////////////////////////////////////
    ! Finds optimal shoot:root growth ratio to balance C:N stoichiometry
    ! of a grass (no wood allocation).
    !------------------------------------------------------------------
    use md_findroot_fzeroin
    use md_interface_pmodel, only: myinterface
    use md_forcing_pmodel, only: climate_type
    use md_sofunutils, only: dampen_variability, calc_reg_line

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(climate_type), intent(in) :: climate
    integer, intent(in) :: year   ! xxx debug

    ! local variables
    real :: dcleaf
    real :: dnleaf
    real :: dcroot
    real :: dcseed
    real :: dnseed
    real :: dnroot
    real :: drgrow
    real :: dclabl
    real, save :: an_unitlai_prev
    real :: an_unitlai, an_unitlai_reldiff, an_unitlai_reldiff_damped, an_max
    real :: f_seed           ! fraction allocated to seeds (unitless)
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

    real, parameter :: freserve = 0.004 ! SwissFACE results are very sensitive to this parameter!

    real, parameter :: kdecay_labl = 0.1

    integer, parameter :: len_resp_vec = 30
    real, dimension(nlu,npft,len_resp_vec), save :: resp_vec
    real :: frac_for_resp

    integer, parameter :: len_an_vec = 15
    real, dimension(nlu,npft,len_an_vec), save :: an_vec
    real, dimension(len_an_vec) :: tmp_vec
    integer, dimension(len_an_vec) :: vec_idx
    real :: slope, intercept

    real    :: cavl, navl, req, c_req, n_req, c_acq, n_acq
    logical, save :: firstcall_cnbal = .true.
    integer, parameter :: len_cnbal_vec = 365

    real, dimension(nlu,npft,len_cnbal_vec), save :: g_net_vec, r_rex_vec, n_acq_vec, c_a_l_vec, c_a_r_vec, c_a_s_vec, n_con_vec

    real :: psi_c        ! return on leaf investment
    real :: psi_n        ! return on root investment
    real :: c_con        ! sum of C consumed to satisfy all biomass production and respiration, including growth respiration
    real :: n_con        ! sum of N consumed to satisfy all biomass production of past N days
    real :: r_ntoc_con   ! consumed N:C ratio (considering C allocation and respiration over N allocation)
    real :: n_exc        ! excess N acquisition over the past N days
    real :: n_con_corr   ! corrected sum of N consumed, after accounting for excess uptake left over from the imbalance of acquisition and utilization over the preceeeding N days
    real, save :: frac_leaf = 0.5 ! fraction of C allocated to leaves

    integer, parameter :: len_luep_vec = ndayyear
    integer, parameter :: len_rrum_vec = ndayyear
    integer, parameter :: len_rduf_vec = ndayyear
    integer, parameter :: len_navl_vec = ndayyear
    integer, parameter :: len_cn_vec   = ndayyear
    
    real, dimension(nlu,len_navl_vec),      save :: no3_vec
    real, dimension(nlu,len_navl_vec),      save :: nh4_vec
    real, dimension(nlu,npft,len_luep_vec), save :: luep_vec
    real, dimension(nlu,npft,len_rduf_vec), save :: rduf_vec
    real, dimension(nlu,npft,len_rrum_vec), save :: rrum_vec
    real, dimension(nlu,npft,len_cn_vec),   save :: cn_vec

    real, parameter :: par_resv = 0.1   ! scales reserves pool (controlling risk avoidance)
    real, parameter :: par_labl = 0.1   ! scales labile pool (controlling risk avoidance)
    real :: c_labl_target     ! target size of labile pool (g C m-2)
    real :: c_resv_target     ! target size of reserves pool (g C m-2)
    real :: f_resv_to_labl    ! net C flux from reserves to labile pool (g C m-2 tstep-1)
    real :: cfrac_resv        ! fraction of C pool moving from reserves to labile
    real :: cfrac_labl        ! fraction of C pool moving from labile to reserves
    type(orgpool) :: org_resv_to_labl ! organic mass moving from reserves to labile pool (g C[N] m-2 tstep-1)
    type(orgpool) :: org_labl_to_resv ! organic mass moving from labile to reserves pool (g C[N] m-2 tstep-1)

    logical :: cont          ! true if allocation to leaves (roots) is not 100% and not 0%
    real    :: max_dcleaf_n_constraint
    real    :: max_dcroot_n_constraint
    real    :: max_dc_buffr_constraint
    real    :: max_dc_n_constraint
    real    :: max_dc
    real    :: min_dc
    real    :: eval_allleaves
    real    :: eval_allroots
    real    :: abserr
    real    :: relerr
    real    :: nleaf0
    real    :: lai0, lai1
    integer, parameter :: nmax = 100
    logical :: findroot

    type(outtype_zeroin)  :: out_zeroin

    ! xxx debug
    real :: tmp

    ! xxx verbose
    logical, parameter :: verbose = .true.

    abserr = 100.0  * XMACHEPS !*10e5
    relerr = 1000.0 * XMACHEPS !*10e5

    pftloop: do pft=1,npft
      lu = params_pft_plant(pft)%lu_category

      if ( tile(lu)%plant(pft)%plabl%c%c12 > eps .and. tile(lu)%plant(pft)%plabl%n%n14 > eps ) then

        if (params_pft_plant(pft)%grass) then
          !-------------------------------------------------------------------------
          ! PHENOPHASE: SEED FILLING
          ! Determine day when "absorbed" top-of-atmosphere radiation per unit leaf 
          ! area starts declining
          !-------------------------------------------------------------------------
          ! Calculate absorbed top-of-atmosphere solar radiation per unit leaf area
          ! Using TOA radiation here to avoid effects by daily varying cloud cover,
          ! assuming the plant senses available radiation over the seasons based on day length.
          an_unitlai = tile_fluxes(lu)%canopy%dra
                       ! * tile(lu)%plant(pft)%fapar_ind &
                       ! / tile(lu)%plant(pft)%lai_ind

          ! Apply low-pass filter on an_unitlai
          if (firstcall1) then
            an_vec(lu,pft,:) = an_unitlai
            if (pft == npft .and. lu == nlu) firstcall1 = .false.
          else
            an_vec(lu,pft,1:(len_an_vec-1)) = an_vec(lu,pft,2:len_an_vec)
            an_vec(lu,pft,len_an_vec) = an_unitlai
          end if

          ! normalise by mean
          tmp_vec = an_vec(lu,pft,:) / (sum(an_vec(lu,pft,:)) / len_an_vec)

          ! get trend of an_unitlai over preceeding len_an_vec days
          vec_idx = (/ (idx, idx = 1, len_an_vec) /)
          call calc_reg_line( real(vec_idx(:)), tmp_vec, intercept, slope )

          ! calculate fraction allocated to seeds
          f_seed = calc_f_seed( slope )

          ! record trend for test output
          tile_fluxes(lu)%plant(pft)%debug1 = an_unitlai            
          tile_fluxes(lu)%plant(pft)%debug2 = slope            
          tile_fluxes(lu)%plant(pft)%debug3 = f_seed            

          ! Only start filling seeds if LAI > 1
          if (tile(lu)%plant(pft)%lai_ind < 1.0) then
            f_seed = 0.0
          end if

          ! XXX try
          ! if (year < 2004) then
            ! f_seed = 0.0
          ! end if

          ! initialise (to make sure)
          dcleaf = 0.0
          dcroot = 0.0
          dcseed = 0.0
          drgrow = 0.0


          ! if ( myinterface%steering%dofree_alloc ) then
          !   !==================================================================
          !   ! Free allocation
          !   !------------------------------------------------------------------

          !   !------------------------------------------------------------------
          !   ! At the start of growth, use approximation to calculate leaf N
          !   !------------------------------------------------------------------
          !   if (tile(lu)%plant(pft)%pleaf%c%c12 == 0.0) then
          !     call get_leaftraits_init( tile(lu)%plant(pft) )
          !   end if

            ! !------------------------------------------------------------------
            ! ! Determine allocatable C, given C and N availability (labile) constraint
            ! !------------------------------------------------------------------
            ! max_dcleaf_n_constraint = tile(lu)%plant(pft)%plabl%n%n14 * tile(lu)%plant(pft)%r_cton_leaf
            ! max_dcroot_n_constraint = tile(lu)%plant(pft)%plabl%n%n14 * params_pft_plant(pft)%r_cton_root ! should be obsolete as generally r_ntoc_leaf > r_ntoc_root

            ! ! XXX THIS MAKES A HUGE DIFFERENCE
            ! ! >>>> OPTION A (WORKS NICELY):
            ! max_dc_buffr_constraint = max(  0.0, &
            !                                 params_plant%growtheff &
            !                                 * ( tile(lu)%plant(pft)%plabl%c%c12 &
            !                                 - ( params_plant%r_root + params_plant%exurate ) * tile(lu)%plant(pft)%proot%c%c12 ) )

            ! ! print*,'option A: ', max_dc_buffr_constraint

            ! ! ! >>>> OPTION B (PRODUCES NON-SENSICAL ROOT RESULTS):
            ! ! max_dc_buffr_constraint = params_plant%growtheff * tile(lu)%plant(pft)%plabl%c%c12
            ! ! ! print*,'option B: ', max_dc_buffr_constraint

            ! ! ! >>>> OPTION C:
            ! ! ! works fine with freserve = 0.004
            ! ! max_dc_buffr_constraint = max( 0.0, params_plant%growtheff * ( tile(lu)%plant(pft)%plabl%c%c12 - freserve * ( tile(lu)%plant(pft)%proot%c%c12 ) ) )

            ! max_dc = min( max_dc_buffr_constraint, max_dcleaf_n_constraint, max_dcroot_n_constraint )
            ! min_dc = 0.0
            
          !   ! !------------------------------------------------------------------
          !   ! ! Binary decision: this is good for quickly depleting labile pool 
          !   ! ! imbalance but leads to overshoot 
          !   ! !------------------------------------------------------------------
          !   ! findroot = .false.
          !   ! if ( tile(lu)%plant(pft)%plabl%c%c12 < (tile(lu)%plant(pft)%plabl%n%n14 * leaftraits(pft)%r_cton_leaf) ) then
          !   !   ! print*,'C is limiting -> should put more to leaves'
          !   !   dcleaf = max_dc
          !   ! else if ( tile(lu)%plant(pft)%plabl%c%c12 > (tile(lu)%plant(pft)%plabl%n%n14 * params_pft_plant(pft)%r_cton_root) ) then
          !   !   ! print*,'N is limiting -> should put more to roots'
          !   !   dcleaf = 0.0
          !   ! else
          !   !   ! print*,'findroot ...'
          !   !   findroot = .true.
          !   ! end if

          !   ! !------------------------------------------------------------------
          !   ! ! Safety brakes: if massive imbalance in labile pool accumulates,
          !   ! ! do binary allocation as a safety measure to re-balance labile pool's
          !   ! ! C:N ratio.
          !   ! ! Otherwise (as long as no massive imbalance arises), find optimum
          !   ! ! allocation, defined by newly acquired C and N (NPP-Ra-Cex, Nuptake)
          !   ! ! are acquired in the same ratio as is needed for new tissue growth.
          !   ! !------------------------------------------------------------------
          !   ! if ( cton( tile(lu)%plant(pft)%plabl, default=0.0 ) > 10.0 * params_pft_plant(pft)%r_cton_root ) then
          !   !   ! print*,'1.1.1'
          !   !   !------------------------------------------------------------------
          !   !   ! massive imbalance: too much C -> put all to roots
          !   !   !------------------------------------------------------------------
          !   !   dcleaf = 0.0
          !   !   ! print*,'safety: all to roots', doy
            
          !   ! else if ( ntoc( tile(lu)%plant(pft)%plabl, default=9999.0 ) > 10.0 * leaftraits(pft)%r_ntoc_leaf ) then
          !   !   ! print*,'1.1.2'
          !   !   !------------------------------------------------------------------
          !   !   ! massive imbalance: too much N -> put all to leaves
          !   !   !------------------------------------------------------------------
          !   !   dcleaf = max_dc
          !   !   ! print*,'safety: all to leaves', doy
            
          !   ! else if (findroot) then
          !     ! ------------------------------------------------------------------
          !     ! No massive imbalance. determine allocation so that C:N of return is equal to C:N new tissue
          !     ! test: if flexible allocation returns 1 or 0 for frac_leaf, then test if this is consistent with what it's set to above
          !     ! ------------------------------------------------------------------

          !     !------------------------------------------------------------------
          !     ! Average over the preceeding N days:
          !     ! - NO3 and NH4 availability
          !     ! - (LUE * PPFD)
          !     ! - C:N stoichiometry of new production
          !     !------------------------------------------------------------------
          !     if (firstcall3) then
          !       ! initialise all vector elements with current value
          !       no3_vec(lu,:) = tile(lu)%soil%pno3%n14
          !       nh4_vec(lu,:) = tile(lu)%soil%pnh4%n14
          !       luep_vec(lu,pft,:) = tile_fluxes(lu)%plant(pft)%lue * climate%dppfd
          !       rduf_vec(lu,pft,:) = tile_fluxes(lu)%plant(pft)%vcmax25_unitfapar * &
          !                            params_gpp%rd_to_vcmax * &
          !                            calc_ftemp_inst_rd( climate%dtemp )
          !       rrum_vec(lu,pft,:) = calc_resp_maint( 1.0, params_plant%r_root, climate%dtemp )
          !       cn_vec(lu,pft,:) = cton( orgplus( tile_fluxes(lu)%plant(pft)%alloc_leaf, &
          !                                         tile_fluxes(lu)%plant(pft)%alloc_root, &
          !                                         tile_fluxes(lu)%plant(pft)%alloc_sapw, &
          !                                         tile_fluxes(lu)%plant(pft)%alloc_wood  &
          !                                         ), &
          !                                 default = params_pft_plant(pft)%r_cton_root )
          !       if (lu == nlu) firstcall3 = .false.
          !     else
          !       ! shift elements of vector forward
          !       no3_vec(lu,1:(len_navl_vec-1)) = no3_vec(lu,2:len_navl_vec)
          !       nh4_vec(lu,1:(len_navl_vec-1)) = nh4_vec(lu,2:len_navl_vec)
          !       luep_vec(lu,pft,1:(len_luep_vec-1)) = luep_vec(lu,pft,2:len_luep_vec)
          !       rduf_vec(lu,pft,1:(len_rduf_vec-1)) = rduf_vec(lu,pft,2:len_rduf_vec)
          !       rrum_vec(lu,pft,1:(len_rrum_vec-1)) = rrum_vec(lu,pft,2:len_rrum_vec)
          !       cn_vec(lu,pft,1:(len_cn_vec-1)) = cn_vec(lu,pft,2:len_cn_vec)

          !       ! put current value to end of vector
          !       no3_vec(lu,len_navl_vec) = tile(lu)%soil%pno3%n14
          !       nh4_vec(lu,len_navl_vec) = tile(lu)%soil%pnh4%n14
          !       luep_vec(lu,pft,len_luep_vec) = tile_fluxes(lu)%plant(pft)%lue * climate%dppfd
          !       rduf_vec(lu,pft,len_rduf_vec) = tile_fluxes(lu)%plant(pft)%vcmax25_unitfapar * &
          !                                       params_gpp%rd_to_vcmax * &
          !                                       calc_ftemp_inst_rd( climate%dtemp )
          !       rrum_vec(lu,pft,len_rrum_vec) = calc_resp_maint( 1.0, params_plant%r_root, climate%dtemp )
          !       cn_vec(lu,pft,len_cn_vec) = cton( orgplus(  tile_fluxes(lu)%plant(pft)%alloc_leaf, &
          !                                                   tile_fluxes(lu)%plant(pft)%alloc_root, &
          !                                                   tile_fluxes(lu)%plant(pft)%alloc_sapw, &
          !                                                   tile_fluxes(lu)%plant(pft)%alloc_wood  &
          !                                                   ), &
          !                                         default = params_pft_plant(pft)%r_cton_root )
          !     end if

          !     !------------------------------------------------------------------
          !     ! Store state variables for optimisation
          !     !------------------------------------------------------------------
          !     state%pleaf           = tile(lu)%plant(pft)%pleaf
          !     state%proot           = tile(lu)%plant(pft)%proot
          !     state%plabl           = tile(lu)%plant(pft)%plabl
          !     state%actnv_unitfapar = tile(lu)%plant(pft)%actnv_unitfapar
          !     state%usepft          = pft
          !     state%soiltemp        = tile(lu)%soil%phy%temp

          !     state%fpc_grid         = tile(lu)%plant(pft)%fpc_grid
          !     state%vcmax25_unitfapar = tile_fluxes(lu)%plant(pft)%vcmax25_unitfapar

          !     ! long-term average 
          !     state%cn               = sum(cn_vec(lu,pft,:)) / len_cn_vec
          !     state%luep             = sum(luep_vec(lu,pft,:)) / len_luep_vec
          !     state%rduf             = sum(rduf_vec(lu,pft,:)) / len_rduf_vec
          !     state%rrum             = sum(rrum_vec(lu,pft,:)) / len_rrum_vec
          !     state%luep             = sum(luep_vec(lu,pft,:)) / len_luep_vec
          !     state%nh4              = sum(nh4_vec(lu,:)) / len_navl_vec
          !     state%no3              = sum(no3_vec(lu,:)) / len_navl_vec

          !     !------------------------------------------------------------------
          !     ! Optimisation by balanced growth
          !     ! Test I: Evaluate balance if all is put to roots.
          !     ! If C:N ratio of return is still greater than whole-plant C:N 
          !     ! ratio, then put all to roots.
          !     !------------------------------------------------------------------
          !     cont = .true.
          !     if (verbose) print*, 'check allocation: all to roots'
          !     eval_allroots  = eval_imbalance( min_dc )
          !     if (verbose) print*, 'eval_allroots', eval_allroots  
          !     if (eval_allroots > 0.0) then
          !       dcleaf = 0.0
          !       cont = .false.
          !       if (verbose) print*, '* putting all to roots *'
          !     end if

          !     !------------------------------------------------------------------
          !     ! Test II: Evaluate balance if all is put to leaves.
          !     ! If C:N ratio of return is still lower than whole-plant C:N ratio, 
          !     ! then put all to leaves.
          !     !------------------------------------------------------------------
          !     if (cont) then
          !       if (verbose) print*, 'check allocation: all to leaves with dcleaf =', max_dc
          !       eval_allleaves = eval_imbalance( max_dc )
          !       if (verbose) print*, 'eval_allleaves', eval_allleaves
          !       if (eval_allleaves < 0.0) then
          !         dcleaf = max_dc
          !         cont = .false.
          !         if (verbose) print*, '* putting all to leaves *'
          !       end if
          !     end if

          !     !------------------------------------------------------------------
          !     ! Optimum is between 0.0 (=min_dc) and max_dc. Find root of function 
          !     ! 'eval_imbalance()' in the interval [0.0, max_dc].
          !     !------------------------------------------------------------------
          !     if (cont) then
          !       if (verbose) print*, '*** finding root of eval_imbalance ***'
          !       ! if (write_logfile_eval_imbalance) open(unit=666,file='eval_imbalance.log',status='unknown')
          !       out_zeroin = zeroin( eval_imbalance, abserr, relerr, nmax, min_dc, max_dc )
          !       if ( out_zeroin%error /= 0 ) then
          !         print*, 'error code ', out_zeroin%error
          !         stop 'zeroin for eval_imbalance() failed'
          !         dcleaf = 0.0
          !       else
          !         dcleaf = out_zeroin%root
          !       end if
          !       ! if (write_logfile_eval_imbalance) close(unit=666)
          !       if (verbose) print*, 'no. of iterations   ', out_zeroin%niter
          !       if (verbose) print*, 'dcleaf is root ', dcleaf
          !       test = eval_imbalance( dcleaf, .true. )
          !       if (verbose) print*, 'eval               =', test
          !       ! if (abs(test)>1e-4) stop 'failed finding a good root'
          !       if (verbose) print*, '----------------------------------'
          !       ! break_after_alloc = .true.
          !       ! stop 'after finding root'
          !     else
          !       ! break_after_alloc = .false.
          !     end if

          !   ! end if

          !   !-------------------------------------------------------------------
          !   ! LEAF ALLOCATION
          !   ! - increment foliage C pool
          !   ! - update LAI
          !   ! - calculate canopy-level foliage N as a function of LAI 
          !   ! - reduce labile pool by C and N increments
          !   !-------------------------------------------------------------------
          !   call allocate_leaf( &
          !     pft, &
          !     dcleaf, &
          !     tile(lu)%plant(pft)%pleaf%c%c12, &
          !     tile(lu)%plant(pft)%pleaf%n%n14, &
          !     tile(lu)%plant(pft)%plabl%c%c12, &
          !     tile(lu)%plant(pft)%plabl%n%n14, &
          !     tile(lu)%plant(pft)%actnv_unitfapar, &
          !     tile(lu)%plant(pft)%lai_ind, &
          !     dnleaf, &
          !     nignore = .false. &
          !     )

          !   !-------------------------------------------------------------------  
          !   ! Update leaf traits with updated LAI and fapar
          !   !-------------------------------------------------------------------
          !   tile(lu)%plant(pft)%fapar_ind = get_fapar( tile(lu)%plant(pft)%lai_ind )
          !   call update_leaftraits( tile(lu)%plant(pft) )

          !   !-------------------------------------------------------------------
          !   ! ROOT ALLOCATION
          !   ! - determine root C and N increments based on remaining labile C and N
          !   ! - update root C and N
          !   ! - update labile C and N
          !   !-------------------------------------------------------------------
          !   call allocate_root( &
          !     pft, &
          !     dcroot, &
          !     dnroot, &
          !     tile(lu)%plant(pft)%proot%c%c12, &
          !     tile(lu)%plant(pft)%proot%n%n14, &
          !     tile(lu)%plant(pft)%plabl%c%c12, &
          !     tile(lu)%plant(pft)%plabl%n%n14, &
          !     nignore = .false. &
          !     )


          ! else
            !==================================================================
            ! Fixed allocation 
            !------------------------------------------------------------------
            ! update
            if (tile(lu)%plant(pft)%pleaf%c%c12 == 0.0) then
              call get_leaftraits_init( tile(lu)%plant(pft) )
            end if

            !------------------------------------------------------------------
            ! record total respiration of preceeding 30 days. 
            ! Keep this amount in labile pool to satisfy demand.
            !------------------------------------------------------------------
            if (firstcall4) then 
              resp_vec(lu,pft,:) = tile_fluxes(lu)%plant(pft)%drleaf &
                                     + tile_fluxes(lu)%plant(pft)%drroot &
                                     + tile_fluxes(lu)%plant(pft)%drsapw &
                                     + tile_fluxes(lu)%plant(pft)%dcex
              if (pft == npft .and. lu == nlu) firstcall4 = .false.
            else
              resp_vec(lu,pft,1:(len_resp_vec-1)) = resp_vec(lu,pft,2:len_resp_vec)
              resp_vec(lu,pft,len_resp_vec) = tile_fluxes(lu)%plant(pft)%drleaf &
                                   + tile_fluxes(lu)%plant(pft)%drroot &
                                   + tile_fluxes(lu)%plant(pft)%drsapw &
                                   + tile_fluxes(lu)%plant(pft)%dcex
            end if
            frac_for_resp = sum(resp_vec(lu,pft,:)) / tile(lu)%plant(pft)%plabl%c%c12

            !------------------------------------------------------------------
            ! Calculate maximum C allocatable based on current labile pool size, 
            ! discounted by the yield factor.
            !------------------------------------------------------------------
            ! ! witholding C for respiration
            ! avl = orgfrac(  kdecay_labl * calc_ft_growth( climate%dtemp ), &
            !                 orgfrac( (1.0 - frac_for_resp), &
            !                           tile(lu)%plant(pft)%plabl ) )

            ! without witholding C for respiration
            avl = orgfrac(  kdecay_labl * calc_ft_growth( climate%dtemp ), &
                            tile(lu)%plant(pft)%plabl )

            ! additionally constrain allocatable C by available N, given the lower C:N of leaves or roots
            if (myinterface%steering%dofree_alloc .and. myinterface%steering%closed_nbal) then
              max_dcleaf_n_constraint = tile(lu)%plant(pft)%plabl%n%n14 * tile(lu)%plant(pft)%r_cton_leaf
              max_dcroot_n_constraint = tile(lu)%plant(pft)%plabl%n%n14 * params_pft_plant(pft)%r_cton_root ! should be obsolete as generally r_ntoc_leaf > r_ntoc_root
              avl%c%c12 = min(avl%c%c12, max_dcleaf_n_constraint, max_dcroot_n_constraint)
            end if

            ! amount to be allocated as real number
            dcseed = f_seed * params_plant%growtheff * avl%c%c12
            dnseed = min(avl%n%n14, dcseed * params_pft_plant(pft)%r_ntoc_seed)
            dcleaf = (1.0 - f_seed) * frac_leaf         * params_plant%growtheff * avl%c%c12
            dcroot = (1.0 - f_seed) * (1.0 - frac_leaf) * params_plant%growtheff * avl%c%c12
            dnroot = dcroot * params_pft_plant(pft)%r_ntoc_root

            ! !-------------------------------------------------------------------
            ! ! SEED ALLOCATION
            ! !-------------------------------------------------------------------
            ! call orgmv( orgpool( carbon( dcseed ), nitrogen( dnseed )  ) , &
            !             tile(lu)%plant(pft)%plabl, &
            !             tile(lu)%plant(pft)%pseed &
            !             )
            
            ! ! ... and remove growth respiration from labile C, update growth respiration
            ! dclabl = (1.0 / params_plant%growtheff) * dcseed
            ! tile(lu)%plant(pft)%plabl%c%c12 = tile(lu)%plant(pft)%plabl%c%c12 - dclabl
            ! tile_fluxes(lu)%plant(pft)%drgrow = tile_fluxes(lu)%plant(pft)%drgrow + dclabl - dcseed

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
                closed_nbal = myinterface%steering%closed_nbal &
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
                closed_nbal = myinterface%steering%closed_nbal &
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

          ! end if

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
        r_rex_vec(lu,pft,:) = tile_fluxes(lu)%plant(pft)%drroot + tile_fluxes(lu)%plant(pft)%drsapw &
                            + tile_fluxes(lu)%plant(pft)%dcex
        n_acq_vec(lu,pft,:) = tile_fluxes(lu)%plant(pft)%dnup%n14
        c_a_l_vec(lu,pft,:) = dcleaf
        c_a_r_vec(lu,pft,:) = dcroot
        c_a_s_vec(lu,pft,:) = dcseed
        n_con_vec(lu,pft,:) = dnleaf + dnroot + dnseed

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
        r_rex_vec(lu,pft,len_cnbal_vec) = tile_fluxes(lu)%plant(pft)%drroot + tile_fluxes(lu)%plant(pft)%drsapw &
                                        + tile_fluxes(lu)%plant(pft)%dcex
        n_acq_vec(lu,pft,len_cnbal_vec) = tile_fluxes(lu)%plant(pft)%dnup%n14
        c_a_l_vec(lu,pft,len_cnbal_vec) = dcleaf
        c_a_r_vec(lu,pft,len_cnbal_vec) = dcroot
        c_a_s_vec(lu,pft,len_cnbal_vec) = dcseed
        n_con_vec(lu,pft,len_cnbal_vec) = dnleaf + dnroot + dnseed

      end if

      if ( myinterface%steering%dofree_alloc ) then

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

        ! corrected sum of N consumed, after accounting for excess uptake left over from the imbalance of acquisition and utilization over the preceeeding N days
        n_con_corr = n_con - n_exc

        ! determine balance (fraction of allocation to leaves)
        ! psi_c * x * growtheff * c_avl / (psi_n * (1-x) * growtheff * c_avl) = c_consumed / (n_consumed - n_excess)
        ! => solve for x (frac_leaf below)
        ! c_consumed is c_req; n_consumed is n_con
        frac_leaf = 1.0 / (psi_c * n_con_corr / (psi_n * c_con) + 1.0)
        
        ! print*,'n_exc, n_con, c_con, psi_c, psi_n, r_ntoc_con, n_con_corr, frac_leaf ', &
        !         n_exc, n_con, c_con, psi_c, psi_n, r_ntoc_con, n_con_corr, frac_leaf

      end if


      !-------------------------------------------------------------------------
      ! RESERVES - LABILE POOL DYNAMICS
      ! Adopted from the QUINCY model (Thum et al., 2019 GMD; Eqs. S40-S42)
      ! See also vignettes/reserves_labile.Rmd
      !-------------------------------------------------------------------------
      ! The target labile pool size is determined by the C requirement for satisfying
      ! N days of respiration and exudation (averaged over len_cnbal_vec), where 
      ! N is the turnover time of the labile pool in days.
      c_labl_target = par_labl * sum( r_rex_vec(lu,pft,:) )

      ! The target reserves pool size is determined by the cumulative amount of C allocated
      ! to leaves, roots, and seeds
      c_resv_target = par_resv * (sum( c_a_l_vec(lu,pft,:) ) &
                      + sum( c_a_r_vec(lu,pft,:) ) &
                      + sum( c_a_s_vec(lu,pft,:) ) )

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
      if (f_resv_to_labl > 0.0) then

        ! transfer C to labile, source pool is presv
        org_resv_to_labl = orgfrac(f_resv_to_labl, tile(lu)%plant(pft)%presv)
        call orginit(org_labl_to_resv)

        tile(lu)%plant(pft)%plabl%c%c12 = tile(lu)%plant(pft)%plabl%c%c12 + org_resv_to_labl%c%c12
        if (.not. myinterface%steering%spinup_reserves) then
          tile(lu)%plant(pft)%presv%c%c12 = tile(lu)%plant(pft)%presv%c%c12 - org_resv_to_labl%c%c12
        end if
        
      else

        ! transfer C to reserves, source pool is plabl
        org_labl_to_resv = orgfrac((-1.0) * f_resv_to_labl, tile(lu)%plant(pft)%plabl)
        call orginit(org_resv_to_labl)

        tile(lu)%plant(pft)%presv%c%c12 = tile(lu)%plant(pft)%presv%c%c12 + org_labl_to_resv%c%c12
        if (.not. myinterface%steering%spinup_reserves) then
          tile(lu)%plant(pft)%plabl%c%c12 = tile(lu)%plant(pft)%plabl%c%c12 - org_labl_to_resv%c%c12
        end if

      end if

      ! ! xxx debug
      ! tile(lu)%plant(pft)%presv%c%c12 = tile(lu)%plant(pft)%presv%c%c12 &
      !                                   - (1.0/3650.0) * tile(lu)%plant(pft)%presv%c%c12

      ! ! record for experimental output
      ! tile_fluxes(lu)%plant(pft)%debug1 = f_resv_to_labl
      ! tile_fluxes(lu)%plant(pft)%debug2 = tile(lu)%plant(pft)%plabl%c%c12 / c_labl_target
      ! tile_fluxes(lu)%plant(pft)%debug3 = tile(lu)%plant(pft)%presv%c%c12 / c_resv_target
      ! tile_fluxes(lu)%plant(pft)%debug4 = tile(lu)%plant(pft)%presv%c%c12

      !-------------------------------------------------------------------
      ! Adjust NPP for growth respiration
      !-------------------------------------------------------------------
      tile_fluxes(lu)%plant(pft)%dnpp = cminus( tile_fluxes(lu)%plant(pft)%dnpp, carbon( tile_fluxes(lu)%plant(pft)%drgrow ) )

      ! record for today
      tile_fluxes(lu)%plant(pft)%alloc_leaf = orgpool( carbon( dcleaf ), nitrogen( dnleaf ) )
      tile_fluxes(lu)%plant(pft)%alloc_root = orgpool( carbon( dcroot ), nitrogen( dnroot ) )
      tile_fluxes(lu)%plant(pft)%alloc_seed = orgpool( carbon( dcseed ), nitrogen( dnseed ) )
      call orginit( tile_fluxes(lu)%plant(pft)%alloc_sapw )
      call orginit( tile_fluxes(lu)%plant(pft)%alloc_wood )

    end do pftloop

  end subroutine allocation_daily


  ! function eval_imbalance( mydcleaf, verbose ) result ( eval )
  !   !/////////////////////////////////////////////////////////
  !   ! Evaluates C:N ratio of new assimilation after allocation 
  !   ! versus whole-plant C:N ratio after allocation. Optimal 
  !   ! allocation is where the two are equal. 
  !   ! Returns positive value (eval) if C:N ratio of new acquisition
  !   ! is greater than C:N ratio of new growth => put more to roots
  !   ! Returns negative value (eval) if C:N ratio of new acquisition
  !   ! is smaller than C:N ratio of new growth => put more to leaves
  !   !---------------------------------------------------------
  !   use md_classdefs, only: orgpool, nitrogen
  !   use md_nuptake, only: calc_dnup, outtype_calc_dnup
  !   use md_findroot_fzeroin

  !   ! arguments
  !   real, intent(in)              :: mydcleaf
  !   logical, intent(in), optional :: verbose

  !   ! function return variable
  !   real :: eval

  !   ! local variables
  !   real    :: cleaf
  !   real    :: nleaf
  !   real    :: croot
  !   real    :: nroot
  !   real    :: clabl
  !   real    :: nlabl
  !   real    :: actnv_unitfapar
  !   integer :: usepft
  !   real    :: soiltemp
  !   real    :: fpc_grid
  !   real    :: luep
  !   real    :: nh4
  !   real    :: no3
  !   real    :: rduf
  !   real    :: rrum
  !   real    :: cn

  !   integer :: lu

  !   real :: mydcroot
  !   real :: mydnleaf
  !   real :: mydnroot
  !   real :: mylai
  !   real :: gpp
  !   real :: npp
  !   real :: rd
  !   real :: mresp_root
  !   real :: cexu
  !   real :: avl
  !   real :: dc
  !   real :: dn
  !   real :: kcleaf
  !   real :: knleaf
  !   real :: kcroot
  !   real :: knroot

  !   real :: nleaf0
  !   real :: lai0, lai1

  !   type( orgpool )           :: proot_tmp
  !   type( outtype_zeroin )    :: out_zeroin
  !   type( outtype_calc_dnup ) :: out_calc_dnup

  !   real :: myfapar

  !   ! write(0,*) '--- in eval_imbalance with mydcleaf=', mydcleaf

  !   ! Copy to local variables for shorter writing
  !   cleaf           = state%pleaf%c%c12
  !   nleaf           = state%pleaf%n%n14
  !   croot           = state%proot%c%c12
  !   nroot           = state%proot%n%n14
  !   clabl           = state%plabl%c%c12
  !   nlabl           = state%plabl%n%n14
  !   actnv_unitfapar = state%actnv_unitfapar
  !   usepft          = state%usepft
  !   soiltemp        = state%soiltemp
  !   fpc_grid        = state%fpc_grid

  !   ! xxx new
  !   rduf            = state%rduf
  !   rrum            = state%rrum
  !   luep            = state%luep
  !   cn              = state%cn
  !   nh4             = state%nh4
  !   no3             = state%no3

  !   !-------------------------------------------------------------------
  !   ! Calculate new LAI, new labile C and N, and canopy N increment after 
  !   ! adding mydcleaf to leaf pool.
  !   !-------------------------------------------------------------------
  !   call allocate_leaf( &
  !     usepft, &
  !     mydcleaf, &
  !     cleaf, &
  !     nleaf, &
  !     clabl, &
  !     nlabl, &
  !     actnv_unitfapar, &
  !     mylai, &          ! intent(out)
  !     mydnleaf, &       ! intent(out)
  !     nignore = .true. &
  !     )

  !   !-------------------------------------------------------------------  
  !   ! Update fAPAR for GPP and Rd calculations below
  !   !-------------------------------------------------------------------  
  !   myfapar = get_fapar( mylai )

  !   !-------------------------------------------------------------------
  !   ! Calculate new root and labile C and N pools after adding mydcroot
  !   ! too roots pool.
  !   !-------------------------------------------------------------------
  !   call allocate_root( &
  !     usepft,&
  !     mydcroot, &
  !     mydnroot, &
  !     croot, &
  !     nroot, &
  !     clabl, &
  !     nlabl, &
  !     nignore = .true. &
  !     )    

  !   !-------------------------------------------------------------------
  !   ! PROJECT NEXT DAY'S C AND N BALANCE:
  !   ! decay, GPP, respiration, N uptake
  !   !-------------------------------------------------------------------
  !   ! Calculate next day's C and N return after assumed allocation (tissue turnover happens before!)
  !   lu = params_pft_plant(usepft)%lu_category

  !   ! GPP is a linear function of fAPAR, thus a saturating function of LAI. 
  !   ! Here, consider the long-term average (luep = LUE * PPFD) for determining GPP with the light use efficiency
  !   ! model, implemented by calc_dgpp().
  !   gpp = calc_dgpp(  myfapar, &
  !                     fpc_grid, &
  !                     luep, &
  !                     1.0, &                             ! factored into 'luep' = LUE * PPFD
  !                     1.0, &                             ! no soil moisture stress considered
  !                     myinterface%params_siml%secs_per_tstep &
  !                     )
  !   print*,'gpp: ', gpp

  !   ! consider the long-term average Vcmax25_unitfapar(t) * (Rd25:Vcmax25) * ftemp_rd( temp(t) )
  !   rd = calc_drd(  myfapar, &
  !                   fpc_grid, &
  !                   1.0, &
  !                   rduf, &
  !                   25.0, &                                ! standard temperature, factor = 1.0
  !                   1.0, &                                 ! no soil moisture stress considered
  !                   myinterface%params_siml%secs_per_tstep &
  !                   )
  !   print*,'rd: ', rd

  !   mresp_root    = croot * rrum
  !   print*,'mresp_root: ', mresp_root

  !   npp           = gpp - rd - mresp_root
  !   print*,'npp: ', npp

  !   cexu          = calc_cexu( croot ) 
  !   print*,'cexu: ', cexu

  !   if ((clabl + npp - cexu) < 0.0 .or. (npp - cexu) < 0.0) then
  !     dc          = 0.0
  !   else
  !     dc          = npp - cexu
  !   end if

  !   out_calc_dnup = calc_dnup( cexu, nh4, no3, params_pft_plant(usepft)%nfixer, soiltemp )
  !   dn            = out_calc_dnup%fix + out_calc_dnup%act_nh4 + out_calc_dnup%act_no3
  !   print*,'dn: ', dn
  !   print*,'cn: ', cn

  !   !-------------------------------------------------------------------
  !   ! EVALUATION QUANTITY - IS MINIMISED BY OPTIMISATION
  !   ! Evaluation quantity is the difference between the 
  !   ! C:N ratio of new assimilates and the C:N ratio 
  !   ! of the whole plant after allocation.
  !   !-------------------------------------------------------------------
  !   ! ! >>>>>>>>>>>>>>>>>>>
  !   ! ! INITIAL IMPLEMENTATION: C:N OF ACQUISITION IS EQUAL TO C:N OF CURRENT WHOLE-PLANT
  !   ! if ((dn + nlabl) == 0.0) then
  !   !   eval = -999.0
  !   ! else if (( mydnleaf + mydnroot ) == 0.0) then
  !   !   eval = 999.0
  !   ! else
  !   !   !     |---------------------------------------------------|  |-------------------------------------------------|
  !   !   eval = params_plant%growtheff * (dc + clabl) / (dn + nlabl) - ( mydcleaf + mydcroot ) / ( mydnleaf + mydnroot )
  !   !   !     |---------------------------------------------------|  |-------------------------------------------------|
  !   !   !     |lab. pool C:N ratio after acq. nxt. day            |  | C:N ratio of new growth                         |
  !   !   !     |---------------------------------------------------|  |-------------------------------------------------|
  !   ! end if
  !   !=====================
  !   ! ALTERNATIVE IMPLEMENTATION: C:N OF ACQUISITION IS EQUAL TO C:N OF INVESTMENT
  !   if (dn == 0.0) then
  !     eval = 999.0
  !   else if (( mydnleaf + mydnroot ) == 0.0) then
  !     eval = -999.0
  !   else
  !     !     |----------------------------------------|   |--------------------------------------------|
  !     eval = params_plant%growtheff * dc / dn          -                      cn
  !     !     |----------------------------------------|   |--------------------------------------------|
  !     !     | projected C:N available for new growth |   | long-term average C:N ratio of past growth |
  !     !     |----------------------------------------|   |--------------------------------------------|
  !   end if
  !   !<<<<<<<<<<<<<<<<<<<

  !   ! if (write_logfile_eval_imbalance) write(666,*) mydcleaf, ",", eval

  ! end function eval_imbalance


  subroutine allocate_leaf( pft, mydcleaf, cleaf, nleaf, clabl, nlabl, rgrow, actnv_unitfapar, lai, mydnleaf, closed_nbal )
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
        ! numerical imprecision
        ! print*,'numerical imprecision?'
        ! print*,'nlabl ', nlabl
        ! stop 'allocate leaf'
        nlabl = 0.0
      end if
    end if  

  end subroutine allocate_leaf


  subroutine allocate_root( pft, mydcroot, mydnroot, croot, nroot, clabl, nlabl, rgrow, closed_nbal )
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
        ! numerical imprecision
        ! print*,'numerical imprecision?'
        ! stop 'allocate leaf'
        nlabl = 0.0
      end if
    end if

  end subroutine allocate_root


  function calc_f_seed( xx ) result( yy )
    !////////////////////////////////////////////////////////////////
    ! Calculates fraction of C allocated to seeds.
    ! Parameters are chosen so that yy is 1 when xx reaches the
    ! minimum value over the season. xx is the 15-day trend in relative
    ! changes in Ra * fAPAR / LAI, where Ra is the top-of-atmosphere
    ! solar radiation. When tested for one site (forcing from FR-Pue),
    ! xx varied between -0.01 and 0.01 over one year 
    ! (see analysis/example_cnmodel.R)
    !----------------------------------------------------------------
    ! arguments
    real, intent(in) :: xx  ! damped relative daily change net assimilation per unit leaf area

    ! function return variable
    real :: yy      ! fraction allocated to seeds

    yy = 1.0 / (1.0 + exp( 1000.0 * xx ))

  end function calc_f_seed


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


  ! function calc_phi_maint(c_labl, c_labl_target) result( out )
  !   !///////////////////////////////////////////////////////////
  !   ! Pull function into labile pool
  !   ! Adopted from the QUINCY model (Thum et al., 2019 GMD; Eqs. S40-S42)
  !   !-----------------------------------------------------------
  !   real, intent(in) :: c_labl, c_labl_target
  !   real, parameter :: lambda_maint = 4.0
  !   real, parameter :: k_maint = 1.6
  !   real :: out
  !   out = exp(-(lambda_maint * c_labl / c_labl_target)**k_maint)
  ! end function


  ! function calc_phi_store(c_resv, c_resv_target) result( out )
  !   !///////////////////////////////////////////////////////////
  !   ! Pull function into reserves
  !   ! Adopted from the QUINCY model (Thum et al., 2019 GMD; Eqs. S40-S42)
  !   !-----------------------------------------------------------
  !   real, intent(in) :: c_resv, c_resv_target
  !   real, parameter :: lambda_store = 2.0
  !   real, parameter :: k_store = 3.0
  !   real :: out
  !   out = exp(-(lambda_store * c_resv / c_resv_target)**k_store)
  ! end function calc_phi_store


  ! function calc_phi_store_corr(phi_maint, phi_store) result( out )
  !   !///////////////////////////////////////////////////////////
  !   ! Pull function into reserves, corrected for stress conditions (very low labile pool)
  !   ! Adopted from the QUINCY model (Thum et al., 2019 GMD; Eqs. S40-S42)
  !   !-----------------------------------------------------------
  !   real, intent(in) :: phi_maint, phi_store
  !   real, parameter :: k_inter = 0.75
  !   real :: out
  !   if (phi_maint > k_inter) then
  !     out = phi_store * (1.0 - phi_maint) / (1.0 - k_inter)
  !   else
  !     out = phi_store
  !   end if
  ! end function calc_phi_store_corr


  ! function calc_f_reserves_labile(c_labl, c_resv, c_labl_target, c_resv_target) result( out )
  !   !///////////////////////////////////////////////////////////
  !   ! Function to calculate the net flux from reserves to labile pool
  !   ! Adopted from the QUINCY model (Thum et al., 2019 GMD; Eqs. S40-S42)
  !   !-----------------------------------------------------------
  !   real, intent(in) :: c_labl, c_resv, c_labl_target, c_resv_target
  !   real, parameter :: tau_labl = 7.0
  !   real :: out, tmp, tmp2
  !   tmp  = calc_phi_maint(c_labl, c_labl_target)
  !   tmp2 = calc_phi_store(c_resv, c_resv_target)
  !   out = (1.0/tau_labl) * (tmp * c_resv - calc_phi_store_corr(tmp, tmp2) * c_labl)
  ! end function calc_f_reserves_labile


  function calc_l2r(c_labl, c_resv, c_labl_target, c_resv_target, f_max) result( out )
    !///////////////////////////////////////////////////////////
    ! Function for trickling from labile to reserves
    ! Returns the fraction of the labile pool (is source pool) 
    ! moving from labile to reserves.
    !-----------------------------------------------------------
    real, intent(in) :: c_labl, c_resv, c_labl_target, c_resv_target, f_max
    real :: out
    out = max(0.0, 1.0 - c_resv / c_resv_target) * f_max / (1.0 + exp(-10.0 * (c_labl / c_labl_target - 1.5)))
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


  ! old
  ! subroutine allocate_leaf( pft, mydcleaf, cleaf, nleaf, clabl, nlabl, meanmppfd,    nv,    lai,   mydnleaf )
  !   !///////////////////////////////////////////////////////////////////
  !   ! LEAF ALLOCATION
  !   ! Sequence of steps:
  !   ! - increment foliage C pool
  !   ! - update LAI
  !   ! - calculate canopy-level foliage N as a function of LAI 
  !   ! - reduce labile pool by C and N increments
  !   !-------------------------------------------------------------------
  !   use md_classdefs

  !   ! arguments
  !   integer, intent(in)                 :: pft
  !   real, intent(in)                    :: mydcleaf
  !   real, intent(inout)                 :: cleaf, nleaf
  !   real, intent(inout)                 :: clabl, nlabl
  !   real, dimension(nmonth), intent(in) :: meanmppfd
  !   real, dimension(nmonth), intent(in) :: nv
  !   real, intent(out)                   :: lai
  !   real, optional, intent(out)         :: mydnleaf

  !   ! local variables
  !   real :: nleaf0
  !   real :: dclabl, dnlabl

  !   ! xxx debug
  !   real :: lai_tmp

  !   ! find LAI, given new leaf mass. This is necessary to get leaf-N as 
  !   ! a function of LAI.
  !   if (mydcleaf>0.0) then

  !     cleaf  = cleaf + mydcleaf

  !     lai_tmp = lai 

  !     ! Calculate LAI as a function of leaf C
  !     lai = get_lai( pft, cleaf, meanmppfd(:), nv(:) )

  !     ! calculate canopy-level leaf N as a function of LAI
  !     nleaf0   = nleaf      
  !     nleaf    = get_leaf_n_canopy( pft, lai, meanmppfd(:), nv(:) )
  !     mydnleaf = nleaf - nleaf0

  !     ! subtract from labile pool, making sure pool does not get negative
  !     dclabl = min( clabl, 1.0 / params_plant%growtheff * mydcleaf )
  !     dnlabl = min( nlabl, mydnleaf )
  !     if ( (dclabl - clabl) > 1e-8 ) stop 'trying to remove too much from labile pool: leaf C'
  !     if ( (dnlabl - nlabl) > 1e-8 ) stop 'trying to remove too much from labile pool: leaf N'
  !     clabl  = clabl - dclabl
  !     nlabl  = nlabl - dnlabl

  !   else

  !     lai      = get_lai( pft, cleaf, meanmppfd(:), nv(:) )
  !     mydnleaf = 0.0

  !   end if

  ! end subroutine allocate_leaf


  ! old:
  ! subroutine allocate_root( croot, nroot, clabl, nlabl, pft, mydcroot, mydnroot )
  !   !-------------------------------------------------------------------
  !   ! ROOT ALLOCATION
  !   !-------------------------------------------------------------------
  !   use md_classdefs

  !   ! arguments
  !   real, intent(inout)         :: croot, nroot
  !   real, intent(inout)         :: clabl, nlabl
  !   integer, intent(in)         :: pft
  !   real, optional, intent(out) :: mydcroot
  !   real, optional, intent(out) :: mydnroot

  !   ! local variables
  !   real :: dclabl
  !   real :: dnlabl

  !   if (clabl>0.0 .and. nlabl>0.0) then
  !     ! use remainder for allocation to roots
  !     mydcroot = min( params_plant%growtheff * clabl, params_pft_plant(pft)%r_cton_root * nlabl )
  !     mydnroot = min( mydcroot * params_pft_plant(pft)%r_ntoc_root, nlabl )

  !     dclabl = min( clabl, 1.0 / params_plant%growtheff * mydcroot )
  !     dnlabl = min( nlabl, mydnroot )
  !     if ( (dnlabl - nlabl) > 1e-8 ) stop 'trying to remove too much from labile pool: root N'
  !     if ( (dclabl - clabl) > 1e-8 ) stop 'trying to remove too much from labile pool: root C'
  !     clabl  = clabl - dclabl
  !     nlabl  = nlabl - dnlabl

  !     if (mydcroot<0.0) stop 'root allocation neg.: C'
  !     if (mydnroot<0.0) stop 'root allocation neg.: N'

  !     croot = croot + mydcroot
  !     nroot = nroot + mydnroot
    
  !   else
  !     mydcroot = 0.0
  !     mydnroot = 0.0
  !   end if

  ! end subroutine allocate_root

end module md_allocation_cnmodel
