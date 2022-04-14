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
    real          :: useppfd
    real          :: airtemp
    real          :: soiltemp
    real          :: fpc_grid
    real          :: lue     
    real          :: vcmax25_unitiabs
    real          :: nh4
    real          :: no3
  end type statetype

  type(statetype) :: state

  ! logical, parameter :: write_logfile_eval_imbalance = .false.
  real :: test

contains

  subroutine allocation_daily( tile, tile_fluxes, climate, climate_memory )
    !//////////////////////////////////////////////////////////////////
    ! Finds optimal shoot:root growth ratio to balance C:N stoichiometry
    ! of a grass (no wood allocation).
    !------------------------------------------------------------------
    use md_findroot_fzeroin
    use md_interface_pmodel, only: myinterface
    use md_forcing_pmodel, only: climate_type
    use md_sofunutils, only: dampen_variability

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(climate_type), intent(in) :: climate, climate_memory

    ! local variables
    real :: dcleaf
    real :: dnleaf
    real :: dcroot
    real :: dnroot
    real :: drgrow
    real :: an_unitlai
    real, save :: an_unitlai_damped, an_unitlai_damped_prev, count
    integer, parameter :: count_wait = 15
    logical, save :: firstcall1 = .true.
    logical, save :: firstcall2 = .true.

    integer :: lu
    integer :: pft
    integer :: usemoy        ! MOY in climate vectors to use for allocation
    integer :: usedoy        ! DOY in climate vectors to use for allocation
  
    real    :: cavl, navl, avl, req
    real, parameter :: freserve = 0.004 ! SwissFACE results are very sensitive to this parameter!

    ! xxx try
    real, parameter :: kdecay_labl = 0.1
    real, parameter :: frac_leaf = 0.5

    real, dimension(nlu,npft,30) :: resp_vec
    real :: keep_for_resp

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

    ! xxx verbose
    logical, parameter :: verbose = .true.

    abserr = 100.0  * XMACHEPS !*10e5
    relerr = 1000.0 * XMACHEPS !*10e5


    ! !-------------------------------------------------------------------------
    ! ! Determine day of year (DOY) and month of year (MOY) to use in climate vectors
    ! !-------------------------------------------------------------------------
    ! if (dm==ndaymonth(moy)) then
    !   usemoy = moy + 1
    !   if (usemoy==13) usemoy = 1
    ! else
    !   usemoy = moy
    ! end if
    ! if (doy==ndayyear) then
    !   usedoy = 1
    ! else
    !   usedoy = doy + 1
    ! end if

    do pft=1,npft
      lu = params_pft_plant(pft)%lu_category

      if (params_pft_plant(pft)%grass) then
        !-------------------------------------------------------------------------
        ! Determine day when net C assimilation per unit leaf area starts declining
        !-------------------------------------------------------------------------
        an_unitlai = (tile_fluxes(lu)%plant(pft)%dgpp - tile_fluxes(lu)%plant(pft)%drd) / tile(lu)%plant(pft)%lai_ind
        if (firstcall1) then 
          an_unitlai_damped = an_unitlai
          an_unitlai_damped_prev = an_unitlai_damped
          count = 0
          if (pft == npft .and. lu == nlu) firstcall1 = .false.
        else
          an_unitlai_damped_prev = an_unitlai_damped
          an_unitlai_damped = an_unitlai   ! dampen_variability( an_unitlai, 15.0, an_unitlai_damped )
        end if

        ! after N consecutive days of declining (damped) net assimilation per unit leaf area,
        ! stop growing and allocate to seeds instead
        !-------------------------------------------------------------------------
        ! print*,'an_unitlai, an_unitlai_damped, an_unitlai_damped_prev, count', &
        !         an_unitlai, an_unitlai_damped, an_unitlai_damped_prev, count
                
        if (an_unitlai_damped_prev > an_unitlai_damped) then
          print*,'declining, count: ', count
          count = count + 1
        else
          count = 0
        end if

        if (count > count_wait) then
          tile(lu)%plant(pft)%fill_seeds = .true.
        end if

        ! after N consecutive days of increasing (damped) net assimilation per unit leaf area,
        ! re-start growing and no longer allocate to seeds
        !-------------------------------------------------------------------------
        if (an_unitlai_damped_prev < an_unitlai_damped) then
          count = count + 1
        else
          count = 0
        end if

        if (count > count_wait) then
          tile(lu)%plant(pft)%fill_seeds = .false.
        end if

        ! xxx debug
        print*,'fill_seeds: ', tile(lu)%plant(pft)%fill_seeds

      end if


      ! xxx debug
      tile(lu)%plant(pft)%fill_seeds = .false.



      if ( tile(lu)%plant(pft)%plabl%c%c12 > eps .and. tile(lu)%plant(pft)%plabl%n%n14 > eps ) then

        if (params_pft_plant(pft)%grass) then

          if ( myinterface%steering%dofree_alloc ) then
            !==================================================================
            ! Free allocation
            !------------------------------------------------------------------

            !------------------------------------------------------------------
            ! At the start of growth, use approximation to calculate leaf N
            !------------------------------------------------------------------
            if (tile(lu)%plant(pft)%pleaf%c%c12 == 0.0) then
              call get_leaftraits_init( tile(lu)%plant(pft) )
            end if

            !------------------------------------------------------------------
            ! Determine allocatable C, given C and N availability (labile) constraint
            !------------------------------------------------------------------
            max_dcleaf_n_constraint = tile(lu)%plant(pft)%plabl%n%n14 * tile(lu)%plant(pft)%r_cton_leaf
            max_dcroot_n_constraint = tile(lu)%plant(pft)%plabl%n%n14 * params_pft_plant(pft)%r_cton_root ! should be obsolete as generally r_ntoc_leaf > r_ntoc_root

            ! XXX THIS MAKES A HUGE DIFFERENCE
            ! >>>> OPTION A (WORKS NICELY):
            max_dc_buffr_constraint = max(  0.0, &
                                            params_plant%growtheff &
                                            * ( tile(lu)%plant(pft)%plabl%c%c12 &
                                            - ( params_plant%r_root + params_plant%exurate ) * tile(lu)%plant(pft)%proot%c%c12 ) )

            ! print*,'option A: ', max_dc_buffr_constraint

            ! ! >>>> OPTION B (PRODUCES NON-SENSICAL ROOT RESULTS):
            ! max_dc_buffr_constraint = params_plant%growtheff * tile(lu)%plant(pft)%plabl%c%c12
            ! ! print*,'option B: ', max_dc_buffr_constraint

            ! ! >>>> OPTION C:
            ! ! works fine with freserve = 0.004
            ! max_dc_buffr_constraint = max( 0.0, params_plant%growtheff * ( tile(lu)%plant(pft)%plabl%c%c12 - freserve * ( tile(lu)%plant(pft)%proot%c%c12 ) ) )

            max_dc = min( max_dc_buffr_constraint, max_dcleaf_n_constraint, max_dcroot_n_constraint )
            min_dc = 0.0
            
            ! !------------------------------------------------------------------
            ! ! Binary decision: this is good for quickly depleting labile pool 
            ! ! imbalance but leads to overshoot 
            ! !------------------------------------------------------------------
            ! findroot = .false.
            ! if ( tile(lu)%plant(pft)%plabl%c%c12 < (tile(lu)%plant(pft)%plabl%n%n14 * leaftraits(pft)%r_cton_leaf) ) then
            !   ! print*,'C is limiting -> should put more to leaves'
            !   dcleaf = max_dc
            ! else if ( tile(lu)%plant(pft)%plabl%c%c12 > (tile(lu)%plant(pft)%plabl%n%n14 * params_pft_plant(pft)%r_cton_root) ) then
            !   ! print*,'N is limiting -> should put more to roots'
            !   dcleaf = 0.0
            ! else
            !   ! print*,'findroot ...'
            !   findroot = .true.
            ! end if

            ! !------------------------------------------------------------------
            ! ! Safety brakes: if massive imbalance in labile pool accumulates,
            ! ! do binary allocation as a safety measure to re-balance labile pool's
            ! ! C:N ratio.
            ! ! Otherwise (as long as no massive imbalance arises), find optimum
            ! ! allocation, defined by newly acquired C and N (NPP-Ra-Cex, Nuptake)
            ! ! are acquired in the same ratio as is needed for new tissue growth.
            ! !------------------------------------------------------------------
            ! if ( cton( tile(lu)%plant(pft)%plabl, default=0.0 ) > 10.0 * params_pft_plant(pft)%r_cton_root ) then
            !   ! print*,'1.1.1'
            !   !------------------------------------------------------------------
            !   ! massive imbalance: too much C -> put all to roots
            !   !------------------------------------------------------------------
            !   dcleaf = 0.0
            !   ! print*,'safety: all to roots', doy
            
            ! else if ( ntoc( tile(lu)%plant(pft)%plabl, default=9999.0 ) > 10.0 * leaftraits(pft)%r_ntoc_leaf ) then
            !   ! print*,'1.1.2'
            !   !------------------------------------------------------------------
            !   ! massive imbalance: too much N -> put all to leaves
            !   !------------------------------------------------------------------
            !   dcleaf = max_dc
            !   ! print*,'safety: all to leaves', doy
            
            ! else if (findroot) then
              ! ------------------------------------------------------------------
              ! No massive imbalance. determine allocation so that C:N of return is equal to C:N new tissue
              ! test: if flexible allocation returns 1 or 0 for frac_leaf, then test if this is consistent with what it's set to above
              ! ------------------------------------------------------------------

              !------------------------------------------------------------------
              ! Store state variables for optimisation
              !------------------------------------------------------------------
              state%pleaf           = tile(lu)%plant(pft)%pleaf
              state%proot           = tile(lu)%plant(pft)%proot
              state%plabl           = tile(lu)%plant(pft)%plabl
              state%actnv_unitfapar = tile(lu)%plant(pft)%actnv_unitfapar
              state%usepft          = pft
              state%useppfd         = climate_memory%dppfd    ! use damped and lagged climate
              state%airtemp         = climate_memory%dtemp    ! use damped and lagged climate
              state%soiltemp        = tile(lu)%soil%phy%temp

              state%fpc_grid         = tile(lu)%plant(pft)%fpc_grid
              state%lue              = tile_fluxes(lu)%plant(pft)%lue
              state%vcmax25_unitiabs = tile_fluxes(lu)%plant(pft)%vcmax25_unitiabs
              state%nh4              = tile(lu)%soil%pnh4%n14
              state%no3              = tile(lu)%soil%pno3%n14

              !------------------------------------------------------------------
              ! Optimisation by balanced growth
              ! Test I: Evaluate balance if all is put to roots.
              ! If C:N ratio of return is still greater than whole-plant C:N 
              ! ratio, then put all to roots.
              !------------------------------------------------------------------
              cont = .true.
              if (verbose) print*, 'check alloation: all to roots'
              eval_allroots  = eval_imbalance( min_dc )
              if (verbose) print*, 'eval_allroots', eval_allroots  
              if (eval_allroots > 0.0) then
                dcleaf = 0.0
                cont = .false.
                if (verbose) print*, '* putting all to roots *'
              end if

              !------------------------------------------------------------------
              ! Test II: Evaluate balance if all is put to leaves.
              ! If C:N ratio of return is still lower than whole-plant C:N ratio, 
              ! then put all to leaves.
              !------------------------------------------------------------------
              if (cont) then
                if (verbose) print*, 'check alloation: all to leaves with dcleaf =', max_dc
                eval_allleaves = eval_imbalance( max_dc )
                if (verbose) print*, 'eval_allleaves', eval_allleaves  
                if (eval_allleaves < 0.0) then
                  dcleaf = max_dc
                  cont = .false.
                  if (verbose) print*, '* putting all to leaves *'
                end if
              end if

              !------------------------------------------------------------------
              ! Optimum is between 0.0 (=min_dc) and max_dc. Find root of function 
              ! 'eval_imbalance()' in the interval [0.0, max_dc].
              !------------------------------------------------------------------
              if (cont) then
                if (verbose) print*, '*** finding root of eval_imbalance ***'
                ! if (write_logfile_eval_imbalance) open(unit=666,file='eval_imbalance.log',status='unknown')
                out_zeroin = zeroin( eval_imbalance, abserr, relerr, nmax, min_dc, max_dc )
                if ( out_zeroin%error /= 0 ) then
                  print*, 'error code ', out_zeroin%error
                  stop 'zeroin for eval_imbalance() failed'
                  dcleaf = 0.0
                else
                  dcleaf = out_zeroin%root
                end if
                ! if (write_logfile_eval_imbalance) close(unit=666)
                if (verbose) print*, 'no. of iterations   ', out_zeroin%niter
                if (verbose) print*, 'dcleaf is root ', dcleaf
                test = eval_imbalance( dcleaf, .true. )
                if (verbose) print*, 'eval               =', test
                ! if (abs(test)>1e-4) stop 'failed finding a good root'
                if (verbose) print*, '----------------------------------'
                ! break_after_alloc = .true.
                ! stop 'after finding root'
              else
                ! break_after_alloc = .false.
              end if

            ! end if

            !-------------------------------------------------------------------
            ! LEAF ALLOCATION
            ! - increment foliage C pool
            ! - update LAI
            ! - calculate canopy-level foliage N as a function of LAI 
            ! - reduce labile pool by C and N increments
            !-------------------------------------------------------------------
            call allocate_leaf( &
              pft, &
              dcleaf, &
              tile(lu)%plant(pft)%pleaf%c%c12, &
              tile(lu)%plant(pft)%pleaf%n%n14, &
              tile(lu)%plant(pft)%plabl%c%c12, &
              tile(lu)%plant(pft)%plabl%n%n14, &
              tile(lu)%plant(pft)%actnv_unitfapar, &
              tile(lu)%plant(pft)%lai_ind, &
              dnleaf, &
              nignore = .false. &
              )

            !-------------------------------------------------------------------  
            ! Update leaf traits with updated LAI and fapar
            !-------------------------------------------------------------------
            tile(lu)%plant(pft)%fapar_ind = get_fapar( tile(lu)%plant(pft)%lai_ind )
            call update_leaftraits( tile(lu)%plant(pft) )

            !-------------------------------------------------------------------
            ! ROOT ALLOCATION
            ! - determine root C and N increments based on remaining labile C and N
            ! - update root C and N
            ! - update labile C and N
            !-------------------------------------------------------------------
            call allocate_root( &
              pft, &
              dcroot, &
              dnroot, &
              tile(lu)%plant(pft)%proot%c%c12, &
              tile(lu)%plant(pft)%proot%n%n14, &
              tile(lu)%plant(pft)%plabl%c%c12, &
              tile(lu)%plant(pft)%plabl%n%n14, &
              nignore = .false. &
              )

            !-------------------------------------------------------------------
            ! GROWTH RESPIRATION, NPP
            !-------------------------------------------------------------------
            ! add growth respiration to autotrophic respiration and substract from NPP
            ! (note that NPP is added to plabl in and growth resp. is implicitly removed
            ! from plabl above)
            drgrow = ( 1.0 - params_plant%growtheff ) * ( dcleaf + dcroot ) / params_plant%growtheff
            tile_fluxes(lu)%plant(pft)%dnpp = cminus( tile_fluxes(lu)%plant(pft)%dnpp, carbon(drgrow) )

          else
            !==================================================================
            ! Fixed allocation 
            !------------------------------------------------------------------

            ! record total respiration of preceeding 30 days. keep this amount in labile pool to satisfy demand
            if (firstcall2) then 
              resp_vec(lu,pft,:) = tile_fluxes(lu)%plant(pft)%drleaf &
                                     + tile_fluxes(lu)%plant(pft)%drroot &
                                     + tile_fluxes(lu)%plant(pft)%drsapw &
                                     + tile_fluxes(lu)%plant(pft)%dcex
              if (pft == npft .and. lu == nlu) firstcall2 = .false.
            else
              resp_vec(lu,pft,1:29) = resp_vec(lu,pft,2:30)
              resp_vec(lu,pft,30) = tile_fluxes(lu)%plant(pft)%drleaf &
                                   + tile_fluxes(lu)%plant(pft)%drroot &
                                   + tile_fluxes(lu)%plant(pft)%drsapw &
                                   + tile_fluxes(lu)%plant(pft)%dcex
            end if
            keep_for_resp = sum(resp_vec(lu,pft,:))

            !------------------------------------------------------------------
            ! Calculate maximum C allocatable based on current labile pool size.
            ! Maximum is the lower of all labile C and the C to be matched by all labile N,
            ! discounted by the yield factor.
            !------------------------------------------------------------------
            if (tile(lu)%plant(pft)%pleaf%c%c12 == 0.0) then
              call get_leaftraits_init( tile(lu)%plant(pft) )
            end if

            ! Determine allocation to roots and leaves, fraction given by 'frac_leaf'
            ! No limitation by low temperatures or dryness
            ! avl = max( 0.0, tile(lu)%plant(pft)%plabl%c%c12 - freserve * tile(lu)%plant(pft)%pleaf%c%c12 )

            ! xxx todo: 
            ! keep stock of labile C to satisfy some time respiration without assimilation

            avl = kdecay_labl * (tile(lu)%plant(pft)%plabl%c%c12 - keep_for_resp)
            dcleaf = frac_leaf         * params_plant%growtheff * avl
            dcroot = (1.0 - frac_leaf) * params_plant%growtheff * avl
            dnroot = dcroot * params_pft_plant(pft)%r_ntoc_root

            ! ! xxx debug
            ! avl = max( 0.0, tile(lu)%plant(pft)%plabl%c%c12 )
            ! dcleaf = frac_leaf         * params_plant%growtheff * avl
            ! dcroot = (1.0 - frac_leaf) * params_plant%growtheff * avl
            ! dnroot = dcroot * params_pft_plant(pft)%r_ntoc_root

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
                tile(lu)%plant(pft)%actnv_unitfapar, &
                tile(lu)%plant(pft)%lai_ind, &
                dnleaf, &
                nignore = .true. &
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
                req = 2.0 * abs(tile(lu)%plant(pft)%plabl%n%n14) ! give it a bit more (factor 2)
                ! print*,'Negative labile N. required to balance:', req
                tile_fluxes(lu)%plant(pft)%dnup%n14 = tile_fluxes(lu)%plant(pft)%dnup%n14 + req
                tile_fluxes(lu)%plant(pft)%dnup_fix = tile_fluxes(lu)%plant(pft)%dnup_fix + req
                tile(lu)%plant(pft)%plabl%n%n14 = tile(lu)%plant(pft)%plabl%n%n14 + req
              end if

            end if

            !-------------------------------------------------------------------
            ! ROOT ALLOCATION
            !-------------------------------------------------------------------
            ! xxx debug 
            print*,'dcroot ', dcroot
            print*,'croot before: ', tile(lu)%plant(pft)%proot%c%c12
            if (dcroot > 0.0) then

              call allocate_root( &
                pft, &
                dcroot, &
                dnroot, &
                tile(lu)%plant(pft)%proot%c%c12, &
                tile(lu)%plant(pft)%proot%n%n14, &
                tile(lu)%plant(pft)%plabl%c%c12, &
                tile(lu)%plant(pft)%plabl%n%n14, &
                nignore = .true. &
                )

              !-------------------------------------------------------------------  
              ! If labile N gets negative, account gap as N fixation
              !-------------------------------------------------------------------  
              if ( tile(lu)%plant(pft)%plabl%n%n14 < 0.0 ) then
                req = 2.0 * abs(tile(lu)%plant(pft)%plabl%n%n14) ! give it a bit more (factor 2)
                ! print*,'Negative labile N. required to balance:', req
                tile_fluxes(lu)%plant(pft)%dnup%n14 = tile_fluxes(lu)%plant(pft)%dnup%n14 + req
                tile_fluxes(lu)%plant(pft)%dnup_fix = tile_fluxes(lu)%plant(pft)%dnup_fix + req
                tile(lu)%plant(pft)%plabl%n%n14 = tile(lu)%plant(pft)%plabl%n%n14 + req
              end if

            end if

            print*,'croot after:  ', tile(lu)%plant(pft)%proot%c%c12

            !-------------------------------------------------------------------
            ! GROWTH RESPIRATION, NPP
            !-------------------------------------------------------------------
            ! add growth respiration to autotrophic respiration and substract from NPP
            ! (note that NPP is added to plabl in and growth resp. is implicitly removed
            ! from plabl above)
            drgrow   = ( 1.0 - params_plant%growtheff ) * ( dcleaf + dcroot ) / params_plant%growtheff
            tile_fluxes(lu)%plant(pft)%drgrow = drgrow
            tile_fluxes(lu)%plant(pft)%dnpp = cminus( tile_fluxes(lu)%plant(pft)%dnpp, carbon(drgrow) )

          end if

        else

          stop 'allocation_daily not implemented for trees'

        end if

      
      else

          dcleaf = 0.0
          dcroot = 0.0
          dnleaf = 0.0
          dnroot = 0.0
          drgrow = 0.0

      end if

    end do

    ! print*, '--- END allocation_daily:'

  end subroutine allocation_daily


  function eval_imbalance( mydcleaf, verbose ) result ( eval )
    !/////////////////////////////////////////////////////////
    ! Evaluates C:N ratio of new assimilation after allocation 
    ! versus whole-plant C:N ratio after allocation. Optimal 
    ! allocation is where the two are equal. 
    ! Returns positive value (eval) if C:N ratio of new acquisition
    ! is greater than C:N ratio of new growth => put more to roots
    ! Returns negative value (eval) if C:N ratio of new acquisition
    ! is smaller than C:N ratio of new growth => put more to leaves
    !---------------------------------------------------------
    use md_classdefs, only: orgpool, nitrogen
    use md_gpp_pmodel, only: calc_dgpp, calc_drd, params_gpp
    use md_nuptake, only: calc_dnup, outtype_calc_dnup
    use md_npp, only: calc_resp_maint, calc_cexu
    use md_findroot_fzeroin
    use md_photosynth, only: calc_ftemp_inst_rd

    ! arguments
    real, intent(in)              :: mydcleaf
    logical, intent(in), optional :: verbose

    ! function return variable
    real :: eval

    ! local variables
    real    :: cleaf
    real    :: nleaf
    real    :: croot
    real    :: nroot
    real    :: clabl
    real    :: nlabl
    real    :: actnv_unitfapar
    integer :: usepft
    real    :: useppfd
    real    :: airtemp
    real    :: soiltemp
    real    :: fpc_grid
    real    :: lue
    real    :: vcmax25_unitiabs
    real    :: nh4
    real    :: no3

    integer :: lu

    real :: mydcroot
    real :: mydnleaf
    real :: mydnroot
    real :: mylai
    real :: gpp
    real :: npp
    real :: rd
    real :: mresp_root
    real :: cexu
    real :: avl
    real :: dc
    real :: dn
    real :: kcleaf
    real :: knleaf
    real :: kcroot
    real :: knroot

    real :: nleaf0
    real :: lai0, lai1

    type( orgpool )           :: proot_tmp
    type( outtype_zeroin )    :: out_zeroin
    type( outtype_calc_dnup ) :: out_calc_dnup

    real :: myfapar

    ! write(0,*) '--- in eval_imbalance with mydcleaf=', mydcleaf

    ! Copy to local variables for shorter writing
    cleaf           = state%pleaf%c%c12
    nleaf           = state%pleaf%n%n14
    croot           = state%proot%c%c12
    nroot           = state%proot%n%n14
    clabl           = state%plabl%c%c12
    nlabl           = state%plabl%n%n14
    actnv_unitfapar = state%actnv_unitfapar
    usepft          = state%usepft
    useppfd         = state%useppfd
    airtemp         = state%airtemp
    soiltemp        = state%soiltemp
    fpc_grid        = state%fpc_grid
    lue             = state%lue     
    vcmax25_unitiabs= state%vcmax25_unitiabs
    nh4             = state%nh4
    no3             = state%no3

    !-------------------------------------------------------------------
    ! LEAF ALLOCATION
    !-------------------------------------------------------------------
    call allocate_leaf( &
      usepft, &
      mydcleaf, &
      cleaf, &
      nleaf, &
      clabl, &
      nlabl, &
      actnv_unitfapar, &
      mylai, &          ! intent(out)
      mydnleaf, &       ! intent(out)
      nignore = .true. &
      )

    !-------------------------------------------------------------------  
    ! Update fAPAR for GPP and Rd calculations below
    !-------------------------------------------------------------------  
    myfapar = get_fapar( mylai )

    !-------------------------------------------------------------------
    ! ROOT ALLOCATION
    !-------------------------------------------------------------------
    call allocate_root( &
      usepft,&
      mydcroot, &
      mydnroot, &
      croot, &
      nroot, &
      clabl, &
      nlabl, &
      nignore = .true. &
      )    

    !-------------------------------------------------------------------
    ! PROJECT NEXT DAY'S C AND N BALANCE:
    ! decay, GPP, respiration, N uptake
    !-------------------------------------------------------------------
    ! Calculate next day's C and N return after assumed allocation (tissue turnover happens before!)

    lu = params_pft_plant(usepft)%lu_category

    gpp = calc_dgpp(  myfapar, &
                      fpc_grid, &
                      useppfd, &
                      lue, &
                      1.0, &                             ! no soil moisture stress considered
                      real(myinterface%params_siml%secs_per_tstep) &
                      )

    rd = calc_drd(  myfapar, &
                    fpc_grid, &
                    useppfd, &
                    params_gpp%rd_to_vcmax * vcmax25_unitiabs * calc_ftemp_inst_rd( airtemp ), &
                    1.0, &                                 ! no soil moisture stress considered
                    real(myinterface%params_siml%secs_per_tstep) &
                    )


    mresp_root    = calc_resp_maint( croot, params_plant%r_root, airtemp )
    npp           = gpp - rd - mresp_root
    cexu          = calc_cexu( croot, airtemp ) 

    if ((clabl + npp - cexu) < 0.0 .or. (npp - cexu) < 0.0) then
      dc          = 0.0
    else
      dc          = npp - cexu
    end if

    out_calc_dnup = calc_dnup( cexu, nh4, no3, params_pft_plant(usepft)%nfixer, soiltemp )
    dn            = out_calc_dnup%fix + out_calc_dnup%act_nh4 + out_calc_dnup%act_no3

    !-------------------------------------------------------------------
    ! EVALUATION QUANTITY - IS MINIMISED BY OPTIMISATION
    ! Evaluation quantity is the difference between the 
    ! C:N ratio of new assimilates and the C:N ratio 
    ! of the whole plant after allocation.
    !-------------------------------------------------------------------
    ! >>>>>>>>>>>>>>>>>>>
    ! INITIAL IMPLEMENTATION: C:N OF ACQUISITION IS EQUAL TO C:N OF CURRENT WHOLE-PLANT
    if ((dn + nlabl) == 0.0) then
      eval = -999.0
    else if (( mydnleaf + mydnroot ) == 0.0) then
      eval = 999.0
    else
      !     |---------------------------------------------------|  |-------------------------------------------------|
      eval = params_plant%growtheff * (dc + clabl) / (dn + nlabl) - ( mydcleaf + mydcroot ) / ( mydnleaf + mydnroot )
      !     |---------------------------------------------------|  |-------------------------------------------------|
      !     |lab. pool C:N ratio after acq. nxt. day            |  | C:N ratio of new growth                         |
      !     |---------------------------------------------------|  |-------------------------------------------------|
    end if
    !=====================
    ! ! DOESN'T WORK PROPERLY: ALTERNATIVE IMPLEMENTATION: C:N OF ACQUISITION IS EQUAL TO C:N OF INVESTMENT
    ! if (dn==0.0) then
    !   eval = 999.0
    ! else if (( mydnleaf + mydnroot )==0.0) then
    !   eval = -999.0
    ! else
    !   !     |---------------------------------------------------|  |-------------------------------------------------|
    !   eval = params_plant%growtheff * (dc) / (dn)     - ( mydcleaf + mydcroot ) / ( mydnleaf + mydnroot )
    !   !     |---------------------------------------|   |-------------------------------------------------|
    !   !     |lab. pool C:N ratio after acq. nxt. day|   | C:N ratio of new growth                         |
    !   !     |---------------------------------------|   |-------------------------------------------------|
    ! end if
    !<<<<<<<<<<<<<<<<<<<

    ! if (write_logfile_eval_imbalance) write(666,*) mydcleaf, ",", eval

  end function eval_imbalance


  subroutine allocate_leaf( pft, mydcleaf, cleaf, nleaf, clabl, nlabl, actnv_unitfapar, lai, mydnleaf, nignore )
    !///////////////////////////////////////////////////////////////////
    ! LEAF ALLOCATION
    ! Sequence of steps:
    ! - increment foliage C pool
    ! - update LAI
    ! - calculate canopy-level foliage N as a function of LAI 
    ! - reduce labile pool by C and N increments
    !-------------------------------------------------------------------
    use md_classdefs
    use md_params_core, only: eps

    ! arguments
    integer, intent(in)  :: pft
    real, intent(in)     :: mydcleaf
    real, intent(inout)  :: cleaf, nleaf
    real, intent(inout)  :: clabl, nlabl
    real, intent(in)     :: actnv_unitfapar
    real, intent(out)    :: lai
    real, intent(out)    :: mydnleaf
    logical, intent(in)  :: nignore

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

    ! xxx debug
    ! nleaf    = get_leaf_n_canopy( pft, lai, actnv_unitfapar )
    nleaf = cleaf * r_ntoc_leaf

    mydnleaf = nleaf - nleaf0

    ! depletion of labile C pool is enhanced by growth respiration
    dclabl = 1.0 / params_plant%growtheff * mydcleaf

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

    if (.not. nignore) then
      if ( nlabl < -1.0*eps ) then
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


  subroutine allocate_root( pft, mydcroot, mydnroot, croot, nroot, clabl, nlabl, nignore )
    !///////////////////////////////////////////////////////////////////
    ! ROOT ALLOCATION
    ! Sequence of steps:
    ! - determine root C and N increments based on remaining labile C and N
    ! - update root C and N
    ! - update labile C and N
    !-------------------------------------------------------------------
    use md_classdefs
    use md_params_core, only: eps

    ! arguments
    integer, intent(in) :: pft
    real, intent(in)   :: mydcroot
    real, intent(in)   :: mydnroot
    real, intent(inout) :: croot, nroot
    real, intent(inout) :: clabl, nlabl
    logical, intent(in) :: nignore

    ! local variables
    real :: dclabl

    ! print*,'in allocate_root: clabl, nlabl: ', clabl, nlabl

    ! ! use remainder for allocation to roots
    ! mydcroot = min( mydcroot, params_plant%growtheff * clabl, params_pft_plant(pft)%r_cton_root * nlabl )
    ! mydnroot = min( mydcroot * params_pft_plant(pft)%r_ntoc_root, nlabl )

    ! update root pools
    croot = croot + mydcroot
    nroot = nroot + mydnroot

    ! depletion of labile C pool is enhanced by growth respiration
    dclabl = (1.0 / params_plant%growtheff) * mydcroot

    ! substract from labile pools
    clabl  = clabl - dclabl
    nlabl  = nlabl - mydnroot

    if ( clabl < -1.0 * eps ) then
      stop 'ALLOCATE_ROOT: trying to remove too much from labile pool: root C'
    else if ( clabl < 0.0 ) then
      ! numerical imprecision
      ! print*,'numerical imprecision?'
      ! stop 'allocate root'
      clabl = 0.0
    end if

    if (.not. nignore) then
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