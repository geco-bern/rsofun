module md_vegetation_processes_biomee
  !////////////////////////////////////////////////////////////////
  ! Implementation of vegetation processes for BiomeE.
  ! Code is adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !---------------------------------------------------------------  
  use md_vegetation_tile_biomee
  use md_soil_biomee
  use md_interface_in_biomee, only: inputs, init_lu_biomee

  implicit none
  private

  ! public subroutines
  public :: vegn_CNW_budget, vegn_phenology, vegn_growth_EW
  public :: vegn_reproduction, vegn_nat_mortality, vegn_annual_starvation
  public :: kill_old_grass

contains

  !========================================================================
  !============= Carbon, nitrogen and water budget    =====================
  !========================================================================

  subroutine vegn_CNW_budget( vegn, forcing )
    !////////////////////////////////////////////////////////////////
    ! Fast loop carbon, nitrogen, and water dynamics, Weng 2016-11-25
    ! include Nitrogen uptake and carbon budget
    ! C_growth is calculated here to drive plant growth and reproduciton
    !---------------------------------------------------------------
    use md_forcing_biomee, only: climate_type
    use md_gpp_biomee, only: gpp

    type(vegn_tile_type), intent(inout) :: vegn
    type(climate_type), intent(in) :: forcing

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_stack_item), pointer :: it

    ! Photosynsthesis
    call gpp( forcing, vegn )
    
    ! Update soil water
    call SoilWaterDynamicsLayer( forcing, vegn )
    
    ! Respiration and allocation for growth
    it => vegn%cohorts()
    do while (associated(it))
      cc => it%cohort
      ! increment the cohort age
      cc%age = cc%age + inputs%dt_fast_yr

      ! Maintenance respiration
      call plant_respiration( cc, forcing%tair ) ! get resp per tree per time step

      ! We add the growth respiration scaled from daily to timestap
      cc%fast_fluxes%resp = cc%fast_fluxes%resp + (cc%resg * inputs%step_seconds) / secs_per_day
      cc%fast_fluxes%resp = cc%fast_fluxes%resp * inputs%params_tile%tf_base          ! scaling for calibration

      ! detach photosynthesis model from plant growth
      cc%plabl = cc%plabl + orgpool(cc%fast_fluxes%npp(), cc%fast_fluxes%fixedN)

      it => it%next()
    end do ! all cohorts
    
    ! update soil C and N
    call SOMdecomposition(vegn)
    
    ! Nitrogen uptake
    call vegn_N_uptake(vegn)
    
  end subroutine vegn_CNW_budget

  !========================================================================
  !============= Plant physiology =========================================
  !========================================================================

  subroutine plant_respiration( cc, tairK )
    !//////////////////////////////////////////////////////////////////////
    ! Autotrophic respiration.
    ! Adopted from BiomeE-Allcation.
    !----------------------------------------------------------------------
    type(cohort_type), intent(inout) :: cc
    real, intent(in) :: tairK ! degK
    real :: tf, tfs ! thermal inhibition factors for above- and below-ground biomass
    real :: r_stem, r_root
    real :: Acambium  ! cambium area, m2/tree
    real :: fnsc,exp_acambium ! used to regulation respiration rate !NSCtarget
    real :: r_Nfix    ! respiration due to N fixation
    integer :: sp ! shorthand for cohort species

    associate (spdata => inputs%params_species)

      sp = cc%species

      ! temperature response function
      tf  = exp(9000.0 * (1.0/298.16 - 1.0/tairK))

      !  tfs = thermal_inhibition(tsoil)  ! original
      tfs = tf ! Rm_T_response_function(tsoil) ! Weng 2014-01-14

      ! With nitrogen model, leaf respiration is a function of leaf nitrogen
      !NSCtarget = 3.0 * (cc%bl_max + cc%br_max)
      fnsc = 1.0 ! min(max(0.0,cc%plabl%c12/NSCtarget),1.0)
      ! Acambium = PI * cc%dbh() * cc%height() * 1.2 ! see Weng et al. 2015: Acambium~D^1.5 -> H~D^0.5 and D*H is proportional to D^1.5
      exp_acambium = 1.5 !(1.5 - 2) Use this exponent to make Acambium~D^2. Ensheng suggested range 1.5 to 2.
      Acambium = PI * cc%dbh() ** exp_acambium * cc%height() * 1.2

      ! Facultive Nitrogen fixation
      !if (cc%plabl%n14 < cc%NSNmax() .and. cc%plabl%c12 > 0.5 * NSCtarget) then
      !   cc%fixedN = spdata(sp)%NfixRate0 * cc%proot%c12 * tf * inputs%dt_fast_yr ! kgN tree-1 step-1
      !else
      !   cc%fixedN = 0.0 ! spdata(sp)%NfixRate0 * cc%proot%c12 * tf * inputs%dt_fast_yr ! kgN tree-1 step-1
      !endif

      ! Obligate Nitrogen Fixation
      cc%fast_fluxes%fixedN = fnsc*spdata(sp)%NfixRate0 * cc%proot%c12 * tf * inputs%dt_fast_yr ! kgN tree-1 step-1
      r_Nfix    = spdata(sp)%NfixCost0 * cc%fast_fluxes%fixedN ! + 0.25*spdata(sp)%NfixCost0 * cc%N_uptake    ! tree-1 step-1

      ! LeafN    = spdata(sp)%LNA * cc%leafarea()  ! gamma_SW is sapwood respiration rate (kgC m-2 Acambium yr-1)
      r_stem   = fnsc*spdata(sp)%gamma_SW  * Acambium * tf * inputs%dt_fast_yr ! kgC tree-1 step-1
      r_root   = fnsc*spdata(sp)%gamma_FR  * cc%proot%n14 * tf * inputs%dt_fast_yr ! root respiration ~ root N
      cc%resr = r_root + r_Nfix ! tree-1 step-1
      cc%fast_fluxes%resp = cc%resl + r_stem + cc%resr   !kgC tree-1 step-1

    end associate

  end subroutine plant_respiration

  !========================================================================
  !========= Plant growth =================================================
  !========================================================================

  subroutine fetch_CN_for_growth( cc )
    !////////////////////////////////////////////////////////////////
    ! Fetch C from labile C pool according to the demand of leaves and fine roots,
    ! and the push of labile C pool
    ! DAILY call.
    ! added by Weng, 12-06-2016
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(cohort_type), intent(inout) :: cc
    
    ! local variables
    real :: NSCtarget
    real :: C_push, C_pull
    real :: N_push, N_pull
    real, parameter :: LFR_rate = 1.0  ! filling rate/day

    associate ( sp => cc%sp() )
      NSCtarget = 3.0 * (cc%bl_max + cc%br_max)      ! kgC/tree
      ! Fetch C from labile C pool if it is in the growing season
      if (cc%status == LEAF_ON) then ! growing season
        C_pull = LFR_rate * (Max(cc%bl_max - cc%pleaf%c12,0.0) +   &
          Max(cc%br_max - cc%proot%c12,0.0))
        N_pull = LFR_rate * (Max(cc%bl_max - cc%pleaf%c12,0.0)/sp%CNleaf0 +  &
          Max(cc%br_max - cc%proot%c12,0.0)/sp%CNroot0)
        C_push = cc%plabl%c12 / (ndayyear * sp%tauNSC) ! max(cc%plabl%c12-NSCtarget, 0.0)/(ndayyear*sp%tauNSC)
        N_push = cc%plabl%n14 / (ndayyear * sp%tauNSC) ! 4.0 * C_push/sp%CNsw0  !
        cc%N_growth = Min(max(0.02 * cc%plabl%n14,0.0), N_pull + N_push)
        cc%C_growth = Min(max(0.02 * cc%plabl%c12,0.0), C_pull + C_push) ! Max(0.0,MIN(0.02*(cc%plabl%c12-0.2*NSCtarget), C_pull+C_push))
        !!! cc%plabl%c12      = cc%plabl%c12 - cc%C_growth ! just an estimate, not out yet
      else ! non-growing season
        cc%C_growth = 0.0
        cc%N_growth = 0.0
        cc%resg     = 0.0
      endif
    end associate

  end subroutine fetch_CN_for_growth


  subroutine vegn_growth_EW( vegn )
    !////////////////////////////////////////////////////////////////
    ! updates cohort biomass pools, LAI, and height using accumulated
    ! C_growth and bHW_gain
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_stack_item), pointer :: it
    real :: CSAsw  ! Sapwood cross sectional area, m2
    real :: CSAwd  ! Heartwood cross sectional area, m2
    real :: DBHwd  ! diameter of heartwood at breast height, m
    real :: BSWmax ! max sapwood biomass, kg C/individual
    real :: G_LFR  ! amount of carbon spent on leaf and root growth
    real :: dSeed ! allocation to seeds, Weng, 2016-11-26
    real :: dBL, dBR ! tendencies of leaf and root biomass, kgC/individual
    real :: dBSW ! tendency of sapwood biomass, kgC/individual
    real :: dBHW ! tendency of wood biomass, kgC/individual
    real :: dNS    ! Nitrogen from SW to HW
    real :: BL_u, BL_c
    real :: LF_deficit, FR_deficit
    real :: N_demand,Nsupplyratio,extraN
    real :: r_N_SD
    type(orgpool) :: dHW_pool

    ! Turnover of leaves and fine roots
    call vegn_tissue_turnover( vegn )

    ! Allocate C_gain to tissues
    it => vegn%cohorts()
    do while (associated(it))
      cc => it%cohort

      ! call biomass_allocation( cc )
      associate (sp => cc%sp())

      if (cc%status == LEAF_ON) then

        !update leaf age 
        cc%leaf_age = cc%leaf_age + 1.0/365.0
        
        ! Get carbon from NSC pool. This sets cc%C_growth
        call fetch_CN_for_growth( cc )

        ! Allocate carbon to the plant pools
        ! calculate the carbon spent on growth of leaves and roots
        LF_deficit = max(0.0, cc%bl_max - cc%pleaf%c12)
        FR_deficit = max(0.0, cc%br_max - cc%proot%c12)
        G_LFR = max(min(LF_deficit + FR_deficit,  &
          f_LFR_max  * cc%C_growth), 0.0)

        ! and distribute it between roots and leaves
        dBL  = min(G_LFR, max(0.0, &
                (G_LFR*cc%bl_max + cc%bl_max*cc%proot%c12 - cc%br_max*cc%pleaf%c12)/(cc%bl_max + cc%br_max) &
                ))

        ! flexible allocation scheme
        if ((G_LFR-dBL) > FR_deficit) dBL = G_LFR - FR_deficit
        dBR = G_LFR - dBL

        ! calculate carbon spent on growth of sapwood growth
        if (cc%layer == 1 .AND. cc%age > sp%maturalage) then
          dSeed = sp%v_seed * (cc%C_growth - G_LFR)
          dBSW  = (1.0 - sp%v_seed) * (cc%C_growth - G_LFR)
        else
          dSeed= 0.0
          dBSW = cc%C_growth - G_LFR
        endif

        ! For grasses, temporary
        if (sp%lifeform == 0) then
          dSeed = dSeed + 0.15 * G_LFR
          G_LFR = 0.85 * G_LFR
          dBR   = 0.85 * dBR
          dBL   = 0.85 * dBL
        endif

        ! Nitrogen adjustment on allocations between wood and leaves+roots
        ! Nitrogen demand by leaves, roots, and seeds (Their C/N ratios are fixed.)
        N_demand = dBL / sp%CNleaf0 + dBR / sp%CNroot0 + dSeed / sp%CNseed0 + dBSW / sp%CNsw0

        !==================================
        ! Turn off N effects on allocation  (for running the simulations)
        !==================================

        ! Nitrogen available for all tisues, including wood
        if (cc%N_growth < N_demand) then

          ! a new method, Weng, 2019-05-21
          ! same ratio reduction for leaf, root, and seed if (cc%N_growth < N_demand)
          Nsupplyratio = MAX(0.0, MIN(1.0, cc%N_growth / N_demand))

          !r_N_SD = (cc%N_growth-cc%C_growth/sp%CNsw0)/(N_demand-cc%C_growth/sp%CNsw0) ! fixed wood CN

          r_N_SD = cc%N_growth / N_demand
          if (sp%lifeform > 0 ) then ! for trees
            if (r_N_SD<=1.0 .and. r_N_SD>0.0) then
              dBSW =  dBSW + (1.0-r_N_SD) * (dBL+dBR+dSeed)
              dBR  =  r_N_SD * dBR
              dBL  =  r_N_SD * dBL
              dSeed=  r_N_SD * dSeed
            elseif (r_N_SD <= 0.0) then
              dBSW = cc%N_growth/sp%CNsw0
              dBR  =  0.0
              dBL  =  0.0
              dSeed=  0.0
            endif
          else ! for grasses
            dBR  =  Nsupplyratio * dBR
            dBL  =  Nsupplyratio * dBL
            dSeed=  Nsupplyratio * dSeed
            dBSW =  Nsupplyratio * dBSW
          endif
        endif

        ! update carbon pools
        call cc%pleaf%add_c12(dBL)
        call cc%proot%add_c12(dBR)
        call cc%psapw%add_c12(dBSW)
        call cc%pseed%add_c12(dSeed)
        call cc%plabl%add_c12(- dBR - dBL - dSeed - dBSW)
        cc%leaf_age = (1.0 - dBL/cc%pleaf%c12) * cc%leaf_age !NEW
        cc%resg = 0.5 * (dBR + dBL + dSeed + dBSW) !  daily

        ! update nitrogen pools, Nitrogen allocation
        call cc%pleaf%add_n14(dBL / sp%CNleaf0)
        call cc%proot%add_n14(dBR / sp%CNroot0)
        call cc%pseed%add_n14(dSeed / sp%CNseed0)
        call cc%psapw%add_n14(inputs%params_tile%f_N_add * cc%plabl%n14 + &
        (cc%N_growth - dBL/sp%CNleaf0 - dBR/sp%CNroot0 - dSeed/sp%CNseed0))
        !extraN = max(0.0,cc%psapw%n14+cc%pwood%n14 - (cc%psapw%c12+cc%pwood%c12)/sp%CNsw0)
        extraN   = max(0.0,cc%psapw%n14 - cc%psapw%c12/sp%CNsw0)
        call cc%psapw%add_n14(- extraN)
        call cc%plabl%add_n14(extraN - inputs%params_tile%f_N_add * cc%plabl%n14 - cc%N_growth) !! update NSN
        cc%N_growth = 0.0

        ! accumulated C allocated to leaf, root, and wood
        cc%NPPleaf = cc%NPPleaf + dBL
        cc%NPProot = cc%NPProot + dBR
        cc%NPPwood = cc%NPPwood + dBSW

        ! convert sapwood to heartwood for woody plants ! Nitrogen from sapwood to heart wood
        if (sp%lifeform > 0) then
          CSAsw  = cc%bl_max/sp%LMA * sp%phiCSA * cc%height() ! with Plant hydraulics, Weng, 2016-11-30
          CSAwd  = max(0.0, cc%basal_area() - CSAsw)
          DBHwd  = 2.0 * sqrt(CSAwd/PI)
          BSWmax = sp%alphaBM * (cc%dbh()**sp%thetaBM - DBHwd**sp%thetaBM)
          dBHW   = max(cc%psapw%c12 - BSWmax, 0.0)
          dNS    = dBHW / cc%psapw%c12 * cc%psapw%n14

          ! update C and N of sapwood and wood
          dHW_pool = orgpool(dBHW, dNS)
          cc%pwood = cc%pwood + dHW_pool
          cc%psapw = cc%psapw - dHW_pool

        endif

        ! update bl_max and br_max daily
        BL_c = sp%LMA * sp%LAImax * cc%crownarea() * &
                (1.0-sp%internal_gap_frac) / cc%layer
        BL_u = sp%LMA*cc%crownarea()*(1.0-sp%internal_gap_frac) * &
                sp%underLAImax

        if (cc%layer == 1) cc%topyear = cc%topyear + 1.0 / 365.0

        if (cc%layer > 1 .and. cc%firstlayer == 0) then ! changed back, Weng 2014-01-23
          cc%bl_max = BL_u

          ! Keep understory tree's root low and constant
          cc%br_max = 1.8 * cc%bl_max / (sp%LMA * sp%SRA) ! sp%phiRL
          !cc%br_max = sp%phiRL*cc%bl_max/(sp%LMA*sp%SRA) ! sp%phiRL

        else

          cc%bl_max = BL_u + min(cc%topyear/5.0, 1.0) * (BL_c - BL_u)
          cc%br_max = sp%phiRL * cc%bl_max/(sp%LMA * sp%SRA)

        endif

        ! Grasses have the same bl_max regardless of their layer position
        if (sp%lifeform == 0) then
          cc%bl_max = BL_c
          cc%br_max = sp%phiRL * cc%bl_max/(sp%LMA * sp%SRA)
        endif ! for grasses

      elseif (cc%status == LEAF_OFF .and. cc%C_growth > 0.0) then
        call cc%plabl%add_c12(cc%C_growth)
        cc%resg = 0.0
      endif

      ! reset carbon acculmulation terms
      cc%C_growth = 0

      end associate

      it => it%next()
    end do

  end subroutine vegn_growth_EW

  subroutine vegn_phenology( vegn )
    !////////////////////////////////////////////////////////////////
    ! Determines phenology state (leaf on/off)
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_stack_item), pointer :: it
    real    :: ccNSC, ccNSN
    logical :: do_relayer ! flag to tell if we should call relayer_cohort()
    logical :: cc_firstday
    logical :: TURN_ON_life
    logical :: TURN_OFF_life

    do_relayer = .false.

    ! update vegn GDD and tc_pheno
    vegn%gdd      = vegn%gdd + max(0.0, vegn%tc_daily - 278.15)
    vegn%tc_pheno = vegn%tc_pheno * 0.8 + vegn%Tc_daily * 0.2

    ! ON and OFF of phenology: change the indicator of growing season for deciduous
    it => vegn%cohorts()
    do while (associated(it))
      cc => it%cohort

      ! update GDD for each cohort
      cc%gdd = cc%gdd + max(0.0, vegn%tc_daily - 278.15) ! GDD5

      associate (sp => cc%sp() )

      ! for evergreen
      if (sp%phenotype == 1 .and. cc%status == LEAF_OFF) cc%status=LEAF_ON

      ! for deciduous and grasses
      TURN_ON_life = (sp%phenotype == 0 .and. &
        cc%status    == LEAF_OFF       .and. &
        cc%gdd        > sp%gdd_crit    .and. &
        vegn%tc_pheno > sp%tc_crit_on) .and. &
        (sp%lifeform /= 0 .OR.(sp%lifeform == 0 .and. cc%layer == 1))

      cc_firstday = .false.
      if (TURN_ON_life) then
        cc%status = LEAF_ON ! Turn on a growing season
        cc_firstday = .True.
        do_relayer = .True.
      endif

      ! Reset grass density at the first day of a growing season
      if  (sp%lifeform == 0 .and. (cc_firstday .and. cc%age > 0.5)) then
        
        ! reset grass density and size for perenials
        ccNSC   = (cc%plabl%c12 + cc%pleaf%c12 + cc%psapw%c12 + &
          cc%pwood%c12 + cc%proot%c12 + cc%pseed%c12) * cc%density
        ccNSN   = (cc%plabl%n14 + cc%pleaf%n14 + cc%psapw%n14 + &
          cc%pwood%n14 + cc%proot%n14 + cc%pseed%n14) * cc%density
        
        ! reset
        cc%density = MIN(ccNSC /sp%seedlingsize, ccNSN/(sp%seedlingsize/sp%CNroot0))
        cc%psapw = orgpool(inputs%params_tile%f_initialBSW * sp%seedlingsize, cc%psapw%c12 / sp%CNsw0)  ! for setting up a initial size
        cc%proot = orgpool(0.25 * cc%psapw%c12, cc%proot%c12 / sp%CNroot0)
        cc%pleaf = orgpool()
        cc%pwood = orgpool()
        cc%pseed = orgpool()
        cc%plabl = orgpool(ccNSC, ccNSN) / cc%density - &
          (cc%pleaf + cc%psapw + cc%pwood + cc%proot + cc%pseed)

        ! beni: added NPP accounting (for output)
        cc%NPPleaf = cc%NPPleaf + cc%pleaf%c12
        cc%NPProot = cc%NPProot + cc%proot%c12
        cc%NPPwood = cc%NPPwood + cc%psapw%c12 + cc%pwood%c12


        call cc%init_bl_br()

      endif
      end associate

      it => it%next()
    end do

    if (do_relayer) call vegn%relayer()

    ! OFF of a growing season
    it => vegn%cohorts()
    do while (associated(it))
      cc => it%cohort

      associate (sp => cc%sp())
      TURN_OFF_life = (sp%phenotype  == 0 .and.     &
      cc%status == LEAF_ON .and.     &
      cc%gdd > sp%gdd_crit+600. .and. &
      vegn%tc_pheno < sp%tc_crit)
      end associate

      if (TURN_OFF_life) then
        cc%status = LEAF_OFF  ! Turn off a growing season
        cc%gdd   = 0.0        ! Start to counting a new cycle of GDD
        vegn%gdd = 0.0
      endif

      ! leaf fall
      call Seasonal_fall(cc,vegn)

      it => it%next()
    end do

  end subroutine vegn_phenology


  subroutine Seasonal_fall( cc, vegn )
    !////////////////////////////////////////////////////////////////
    ! Leaf and stem fall
    ! sum leaf and stem fall for deciduous plants, including deciduous trees and grasses
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    implicit none
    type(cohort_type), intent(inout) :: cc
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    real :: dBL, dBR, dNL, dNR, dBStem, dNStem      ! per day
    real, parameter :: leaf_fall_rate = 0.05    ! per day
    real, parameter :: root_mort_rate = 0.025   ! per day

    ! End a growing season: leaves fall for deciduous
    associate (sp => cc%sp() )
    
    if (cc%status == LEAF_OFF .AND. cc%pleaf%c12 > 0.0) then

      dBL = min(leaf_fall_rate * cc%bl_max, cc%pleaf%c12)
      dBR = min(root_mort_rate * cc%br_max, cc%proot%c12)  ! Just for test: keep roots
      dBStem = 0.0 ! trees
      dNStem = 0.0 ! trees

      if (sp%lifeform == 0) then  ! grasses
        dBStem = MIN(1.0,dBL/cc%pleaf%c12) * cc%psapw%c12
        dNStem = MIN(1.0,dBL/cc%pleaf%c12) * cc%psapw%n14
      endif

      ! Nitrogen out
      if (cc%pleaf%c12 > 0) then
        dNL = dBL / cc%pleaf%c12 * cc%pleaf%n14 !dBL/sp%CNleaf0
      else
        dNL = 0.0
      endif

      if (cc%proot%c12 > 0) then
        dNR = dBR / cc%proot%c12 * cc%proot%n14 !dBR/sp%CNroot0
      else
        dNR = 0.0
      endif

      call update_plant_pools(cc, vegn, orgpool(dBL, dNL), orgpool(dBR, dNR), orgpool(dBStem, dNStem))

    endif
    end associate
  
  end subroutine Seasonal_fall


  subroutine vegn_nat_mortality (vegn)
    !////////////////////////////////////////////////////////////////
    ! Determines mortality and updates tile
    !---------------------------------------------------------------
    use md_interface_in_biomee, only: inputs
    
    !   TODO: update background mortality rate as a function of wood density (Weng, Jan. 07 2017)
    type(vegn_tile_type), intent(inout) :: vegn

    ! ---- local vars
    type(cohort_type), pointer :: cc
    type(cohort_stack_item), pointer :: it

    real :: deathrate ! mortality rate, 1/year

    ! real, parameter :: min_density = 1e-5 ! 2e-15 ! 1/m. If density is less than this number,
    ! then the entire cohort is killed; 2e-15 is approximately 1 individual per Earth
    real :: cCAI ! Cumulative CAI
    real :: param_dbh_under 
    real :: param_nsc_under
    real :: param_dbh 
    real :: param_nsc
    real :: CAI_max
    real :: dbh ! cache variable

    cCAI = 0.0
    deathrate = 0.0

    if ((trim(inputs%params_siml%method_mortality) == "const_selfthin")) then
      ! Work in progress.

      ! Remove a big amount of very small trees first
      it => vegn%cohorts()
      do while (associated(it))
        cc => it%cohort

        if (cc%layer > 1) deathrate = 0.2 !sp%mortrate_d_u
        it => vegn%thin_cohort(it, deathrate)
      end do

      ! set calibratable mortality parameter
      CAI_max = inputs%params_tile%par_mort

      ! This thinning method depends on the order of the cohorts (oldest cohorts tends to die first)
      ! We sort the cohorts by increasing height
      call vegn%sort_cohorts_by_height(.true.)

      ! calculate cai_partial and the number of cohorts with cai_partial < CAI_max (keep them)
      it => vegn%cohorts()
      do while (associated(it))
        cc => it%cohort

        cCAI = cCAI + cc%layerfrac()
        if (cCAI > CAI_max) then
          ! Trees to delete
          deathrate = MIN((cCAI - CAI_max) / cc%layerfrac(), 1.0)

          ! Carbon and Nitrogen from dead plants to soil pools
          it => vegn%thin_cohort(it, deathrate)
        else
          it => it%next()
        end if
      enddo
 
    else

      it => vegn%cohorts()
      do while (associated(it))
        cc => it%cohort
        associate ( sp => cc%sp())

          dbh = cc%dbh()

        if ((trim(inputs%params_siml%method_mortality) == "cstarvation")) then

          ! set calibratable parameter
          param_nsc_under = inputs%params_tile%par_mort_under
          param_nsc       = inputs%params_tile%par_mort

          ! Understory mortality
          if (cc%layer > 1) then !
            deathrate = param_nsc_under * sp%mortrate_d_u * &
                     (1. + A_mort*exp(B_mort*dbh))/ &
                     (1. +        exp(B_mort*dbh))

          else  
            ! Canopy mortality
            if (cc%bl_max > 0) then
              deathrate = param_nsc * 0.05 * (exp(-3.5*(cc%plabl%c12/cc%bl_max))/(0.01+exp(-3.5*(cc%plabl%c12/cc%bl_max)))) ! -3.5,-2.5,-2
            endif
          endif

        else if ((trim(inputs%params_siml%method_mortality) == "dbh")) then

          ! set calibratable parameter
          param_dbh_under = inputs%params_tile%par_mort_under
          param_dbh       = inputs%params_tile%par_mort

          if (sp%lifeform == 0) then  ! for grasses
            if (cc%layer > 1) then
              deathrate = sp%mortrate_d_u
            else
              deathrate = sp%mortrate_d_c
            endif
          else                    ! for trees
            if (cc%layer > 1) then ! Understory layer mortality Weng 2015: deathrate = 0.075*(1+9*exp(-60*cc%dbh()))/(1+exp(-60*cc%dbh()))
              deathrate = param_dbh_under * sp%mortrate_d_u * &
                     (1.0 + A_mort*exp(B_mort*dbh))/ &
                     (1.0 +        exp(B_mort*dbh))

            else  ! First layer mortality Weng 2015: deathrate = 0.01*(1+5*exp(4*(cc%dbh()-2)))/(1+exp(4*(cc%dbh()-2)))
              if (inputs%params_siml%do_U_shaped_mortality) then
                ! deathrate = param_dbh * 0.1 *    &
                !            (1.*exp(2.*(cc%dbh()-1))/  &
                !            (1. + exp(2.*(cc%dbh()-1))))
                deathrate = min(1.0, param_dbh * dbh ** 1.5) ! 1.5, 2.5, 5
              else
                deathrate = sp%mortrate_d_c
              endif
            endif
          endif
         
        endif

        ! previous setup allowed death rates > 1 (hence negative ind)
        deathrate = min(1.0, deathrate + 0.01)

        it => vegn%thin_cohort(it, deathrate)

        end associate
      end do

    endif

    ! Remove the cohorts with very few individuals
    call vegn%kill_lowdensity_cohorts()

  end subroutine vegn_nat_mortality


  subroutine vegn_annual_starvation( vegn ) ! Annual
    !////////////////////////////////////////////////////////////////
    ! Mortality due to C starvation (NSC below threshold)
    ! Starvation due to low NSC and annual NPP
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables --------
    type(cohort_type), pointer :: cc
    type(cohort_stack_item), pointer :: it

    it => vegn%cohorts()
    do while (associated(it))
      cc => it%cohort
     
      if (cc%plabl%c12 < 0.01*cc%bl_max) then
        it => vegn%thin_cohort(it)
      else
        it => it%next()
      endif
    end do

  end subroutine vegn_annual_starvation

  subroutine vegn_reproduction( vegn )
    !////////////////////////////////////////////////////////////////
    ! Reproduction of each canopy cohort, yearly time step
    ! calculate the new cohorts added in this step and states:
    ! tree density, DBH, woddy and fine biomass
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_stack_item), pointer :: new ! new cohort
    type(cohort_stack_item), pointer :: it
    integer, dimension(NCohortMax) :: reproPFTs
    type(orgpool), dimension(NCohortMax) :: seed ! seed pool of productible PFTs
    integer :: nPFTs ! number of new cohorts to be created
    integer :: pft_idx ! PFT index
    integer :: k ! new cohort indices

    ! Looping through all reproductable cohorts and check if reproduction happens
    reproPFTs = -999 ! the code of reproductive PFT
    nPFTs = 0

    ! We loop through each cohort and add C and N to species-specific seed pools
    ! whenever a cohort can reproduce
    it => vegn%cohorts()
    do while (associated(it))
      cc => it%cohort

      if (cohort_can_reproduce( cc )) then
        pft_idx = 0
        do k = 1, nPFTs
          if (cc%species == reproPFTs(k)) then
            pft_idx = k
            exit
          endif
        enddo

        if (pft_idx == 0) then ! when it is a new PFT, put it to the next place
          nPFTs            = nPFTs + 1 ! update the number of reproducible PFTs
          reproPFTs(nPFTs) = cc%species ! PFT number
          pft_idx = nPFTs
        endif
          seed(pft_idx) = seed(pft_idx) + cc%pseed * cc%density
          vegn%totSeed = vegn%totSeed + cc%pseed  * cc%density

          ! reset parent's seed C and N
          cc%pseed = orgpool()
      endif ! cohort_can_reproduce

      it => it%next()
    end do

    ! We build new cohorts for seedlings
    do k = 1, nPFTs

      new => vegn%create_cohort()
      cc => new%cohort

      ! update child cohort parameters
      associate (sp => inputs%params_species(reproPFTs(k)))

      ! density
      cc%density = seed(k)%c12 / sp%seedlingsize

      cc%species    = reproPFTs(k)

      call cc%init_bl_br()

      ! Carbon pools
      cc%pleaf%c12 = 0.0 * sp%seedlingsize
      cc%proot%c12 = 0.1 * sp%seedlingsize
      cc%psapw%c12 = inputs%params_tile%f_initialBSW * sp%seedlingsize
      cc%pwood%c12 = 0.0 * sp%seedlingsize
      cc%pseed%c12 = 0.0
      cc%plabl%c12 = sp%seedlingsize - cc%psapw%c12 - cc%proot%c12

      ! beni: added seedling growth for NPP accounting (for output)
      cc%NPPleaf = cc%pleaf%c12
      cc%NPProot = cc%proot%c12
      cc%NPPwood = cc%psapw%c12 + cc%pwood%c12

      ! Nitrogen pools
      cc%pleaf%n14  = cc%pleaf%c12/sp%CNleaf0
      cc%proot%n14  = cc%proot%c12/sp%CNroot0
      cc%psapw%n14  = cc%psapw%c12/sp%CNsw0
      cc%pwood%n14  = cc%pwood%c12/sp%CNwood0
      cc%pseed%n14  = 0.0

      if (cc%density > 0.0) then
        cc%plabl%n14 = sp%seedlingsize * seed(k)%n14 / seed(k)%c12 -  &
          (cc%pleaf%n14 + cc%proot%n14 + cc%psapw%n14 + cc%pwood%n14)
      end if

      vegn%totNewC = vegn%totNewC + (cc%pleaf + cc%proot + &
              cc%psapw + cc%pwood + cc%plabl) * cc%density

      end associate
    enddo

  end subroutine vegn_reproduction


  function cohort_can_reproduce( cc ) result(can_reproduce)
    !////////////////////////////////////////////////////////////////
    ! Determine whether a cohort can reproduce, based on criteria:
    ! - is in top canopy layer
    ! - is actually present
    ! - has reached reproductive maturity (age)
    ! - C and N in seed pool is sufficiently large to satisfy mass of a new seedling
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    logical can_reproduce ! return value
    type(cohort_type), intent(in) :: cc

    associate (sp => cc%sp() )
      can_reproduce = (cc%layer == 1 .and. &
        cc%density > 0.0 .and. &
        cc%age > sp%maturalage.and. &
        cc%pseed%c12 > sp%seedlingsize .and. &
        cc%pseed%n14 > sp%seedlingsize/sp%CNseed0)
    end associate

  end function

  subroutine update_plant_pools( cc, vegn, dL, dR, dStem)
    !////////////////////////////////////////////////////////////////
    ! Update plant carbon and nitrogen
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn
    type(cohort_type), intent(inout) :: cc
    type(orgpool), intent(in) :: dL, dR, dStem  ! leaf and fine root pool tendencies

    ! local variables
    type(orgpool) :: loss_coarse, loss_fine, dtot, dAleaf_pool
    real :: dAleaf ! leaf area decrease due to dBL

    associate ( sp => cc%sp() )

      dAleaf = leaf_area_from_biomass(dL%c12, cc%species)

      ! Retranslocation to NSC and NSN
      dtot = dL + dR + dStem
      cc%plabl = cc%plabl + orgpool(dtot%c12 * inputs%params_tile%l_fract, dtot%n14 * inputs%params_tile%retransN)

      ! update plant pools
      cc%pleaf = cc%pleaf - dL
      cc%proot = cc%proot - dR
      cc%psapw = cc%psapw - dStem

      ! update NPP for leaves, fine roots, and wood
      cc%NPPleaf = cc%NPPleaf - inputs%params_tile%l_fract * dL%n14
      cc%NPProot = cc%NPProot - inputs%params_tile%l_fract * dR%n14
      cc%NPPwood = cc%NPPwood - inputs%params_tile%l_fract * dStem%n14

      ! put C and N into soil pools
      dAleaf_pool = orgpool(dAleaf * inputs%params_tile%LMAmin, &
            dAleaf * sp%LNbase)
      loss_coarse  = (dL - dAleaf_pool + dStem) * cc%density
      loss_fine    = (dR + dAleaf_pool) * cc%density

      loss_coarse = orgpool(loss_coarse%c12 * (1.0 - inputs%params_tile%l_fract), &
              loss_coarse%n14 * (1.0 - inputs%params_tile%retransN))

      loss_fine = orgpool(loss_fine%c12 * (1.0 - inputs%params_tile%l_fract), &
              loss_fine%n14 * (1.0 - inputs%params_tile%retransN))

      call vegn%plant2soil(loss_coarse, loss_fine)

    end associate

  end subroutine update_plant_pools

  subroutine vegn_tissue_turnover( vegn )
    !////////////////////////////////////////////////////////////////
    ! Tissue turnover and transfer to litter pools
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_stack_item), pointer :: it
    real :: alpha_L   ! turnover rate of leaves
    real :: alpha_S   ! turnover rate of stems
    real :: dBL, dBR, dBStem  ! leaf and fine root carbon tendencies
    real :: dNL, dNR, dNStem  ! leaf and fine root nitrogen tendencies

    ! update plant carbon and nitrogen for all cohorts
    it => vegn%cohorts()
    do while (associated(it))
      cc => it%cohort
      associate ( sp => cc%sp() )

      !    Turnover of leaves and roots regardless of the STATUS of leaf
      !    longevity. Deciduous: 0; Evergreen 0.035/LMa
      !    root turnover
      if (cc%status==LEAF_OFF) then
        alpha_L = sp%alpha_L ! 60.0 ! yr-1, for decuduous leaf fall
      else
        alpha_L = sp%alpha_L
      endif

      ! Stem turnover
      if (sp%lifeform == 0) then
        alpha_S = alpha_L
      else
        alpha_S = 0.0
      endif

      dBL    = cc%pleaf%c12 * alpha_L  / ndayyear
      dNL    = cc%pleaf%n14 * alpha_L  / ndayyear

      dBStem = cc%psapw%c12 * alpha_S  / ndayyear
      dNStem = cc%psapw%n14 * alpha_S  / ndayyear

      dBR    = cc%proot%c12 * sp%alpha_FR / ndayyear
      dNR    = cc%proot%n14 * sp%alpha_FR / ndayyear

      call update_plant_pools(cc, vegn, orgpool(dBL, dNL), orgpool(dBR, dNR), orgpool(dBStem, dNStem))

      ! record continuous biomass turnover (not linked to mortality)
      ! cc%m_turnover = cc%m_turnover + loss_coarse + loss_fine
      cc%m_turnover = cc%m_turnover + (1.0 - inputs%params_tile%l_fract) * dBStem

      end associate

      it => it%next()
    end do

  end subroutine vegn_tissue_turnover

  
  subroutine vegn_N_uptake(vegn)
    !//////////////////////////////////////////////////////////////////////
    ! Mineral N uptake from the soil
    ! Code from BiomeE-Allocation
    !----------------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_stack_item), pointer :: it

    real, parameter :: rho_N_up0 = 0.1 ! hourly N uptake rate, fraction of the total mineral N
    real, parameter :: N_roots0  = 0.4 ! root biomass at half max N-uptake rate, kg C m-2

    real    :: totNup    ! kgN m-2
    real    :: avgNup
    real    :: rho_N_up, N_roots   ! actual N uptake rate
    real :: tsoil ! Soil temp in K

    tsoil = vegn%tc_soil + kTkelvin

    ! We artificially refill N inorg
    if (inputs%params_siml%do_closedN_run) vegn%inorg%n14 = inputs%init_soil%init_Nmineral

    ! Nitrogen uptake parameter
    ! It considers competition here. How much N one can absorp depends on 
    ! how many roots it has and how many roots other individuals have.
    N_Roots  = 0.0

    if (vegn%inorg%n14 > 0.0) then

      it => vegn%cohorts()
      do while (associated(it))
        cc => it%cohort
        associate (sp => cc%sp())

          if (cc%plabl%n14 < cc%NSNmax()) N_Roots = N_Roots + cc%proot%c12 * cc%density

        end associate

        it => it%next()
      end do

      ! M-M equation for Nitrogen absoption, McMurtrie et al. 2012, Ecology & Evolution
      ! rate at given root biomass and period of time
      if (N_roots>0.0) then

        ! Add a temperature response equation herefor rho_N_up0 (Zhu Qing 2016)
        rho_N_up = rho_N_up0 * N_roots / (N_roots0 + N_roots) * hours_per_year * inputs%dt_fast_yr

        totNup = rho_N_up * vegn%inorg%n14 * exp(9000.0 * (1./298.16 - 1./tsoil)) ! kgN m-2 time step-1

        ! Below code is from BiomeE-Allocation
        avgNup = totNup / N_roots ! kgN time step-1 kg roots-1
        
        ! Nitrogen uptaken by each cohort (N_uptake) - proportional to cohort's root mass
        it => vegn%cohorts()
        do while (associated(it))
          cc => it%cohort
          if (cc%plabl%n14 < cc%NSNmax()) then

            cc%fast_fluxes%Nup = cc%proot%c12 * avgNup ! min(cc%proot%c12*avgNup, cc%NSNmax()-cc%plabl%n14)
            cc%plabl%n14 = cc%plabl%n14 + cc%fast_fluxes%Nup

            ! subtract N from mineral N
            vegn%inorg%n14 = vegn%inorg%n14 - cc%fast_fluxes%Nup * cc%density
          endif

          it => it%next()
        end do

      endif ! N_roots>0
    endif
  
  end subroutine vegn_N_uptake


  subroutine SOMdecomposition(vegn)
    !//////////////////////////////////////////////////////////////////////
    ! Soil organic matter decomposition and N mineralization
    !
    ! Code from BiomeE-Allocation
    !
    ! Nitrogen mineralization and immoblization with microbial C & N pools
    ! it's a new decomposition model with coupled C & N pools and variable 
    ! carbon use efficiency 
    !----------------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    real, parameter :: CUE0=0.4  ! default microbial CUE
    real, parameter :: phoMicrobial = 2.5 ! turnover rate of microbes (yr-1)
    real, parameter :: CNm = 10.0  ! Microbial C/N ratio
    real, parameter :: fNM=0.0  ! mineral N available for microbes
    real :: CUEfast,CUEslow
    real :: NforM
    real :: micr_C_loss, fast_L_loss, slow_L_loss
    real :: N_loss
    real :: DON_fast,DON_slow,DON_loss ! Dissolved organic N loss, kg N m-2 step-1
    real, parameter :: runoff = 0.2    ! kg m-2 /step
    real, parameter :: fDON = 0.25     ! fractio of DON production in decomposition
    real :: fast_N_free 
    real :: slow_N_free 
    real :: CNfast, CNslow
    real :: A  ! decomp rate reduction due to moisture and temperature
    real :: tsoil ! Soil temp in K
    real :: extra_turnover ! Extra turnover rate due to soil management (tillage, ...)
    real :: N_input, delta_N       ! Extra N input (N fertilization)
    type(init_lu_biomee) :: lu_props ! Land use properties

    tsoil = vegn%tc_soil + kTkelvin
    lu_props = vegn%lu_props()

    ! runoff = vegn%runoff
  
    ! CN ratios of soil C pools
    CNfast = vegn%psoil_fs%c12 / vegn%psoil_fs%n14
    CNslow = vegn%psoil_sl%c12 / vegn%psoil_sl%n14

    ! C decomposition
    A = A_function(tsoil, vegn%thetaS())
    extra_turnover = 1.0 / (1.0 + lu_props%extra_turnover_rate)
    micr_C_loss = vegn%pmicr%c12    * (1.0 - exp(-A * phoMicrobial * inputs%dt_fast_yr))
    fast_L_loss = vegn%psoil_fs%c12 * (1.0 - exp(-A * inputs%params_tile%K1 * extra_turnover * inputs%dt_fast_yr))
    slow_L_loss = vegn%psoil_sl%c12 * (1.0 - exp(-A * inputs%params_tile%K2 * extra_turnover * inputs%dt_fast_yr))

    ! Carbon use efficiencies of microbes
    NforM = fNM * vegn%inorg%n14

    ! Default CUE0 adopted from BiomeE-Allocation
    if (slow_L_loss > 0.0) then  
      CUEfast = MIN(CUE0, CNm * (fast_L_loss/CNfast + NforM)/fast_L_loss)
    else
      CUEfast = CUE0
    end if
    if (slow_L_loss > 0.0) then
      CUEslow = MIN(CUE0,CNm * (slow_L_loss/CNslow + NforM)/slow_L_loss)
    else
      CUEslow = CUE0
    end if

    ! update C and N pools
    ! Carbon pools
    vegn%pmicr%c12  = vegn%pmicr%c12 - micr_C_loss &
                      + fast_L_loss * CUEfast &
                      + slow_L_loss * CUEslow
    vegn%psoil_fs%c12 = vegn%psoil_fs%c12 - fast_L_loss
    vegn%psoil_sl%c12 = vegn%psoil_sl%c12 - slow_L_loss
    
    ! Assume it is proportional to decomposition rates
    ! Find some papers!!
    DON_fast    = fDON * fast_L_loss/CNfast * (inputs%params_tile%etaN*runoff)
    DON_slow    = fDON * slow_L_loss/CNslow * (inputs%params_tile%etaN*runoff)
    DON_loss    = DON_fast + DON_slow

    ! Update Nitrogen pools
    vegn%pmicr%n14= vegn%pmicr%c12/CNm
    vegn%psoil_fs%n14  = vegn%psoil_fs%n14  - fast_L_loss/CNfast - DON_fast
    vegn%psoil_sl%n14 = vegn%psoil_sl%n14 - slow_L_loss/CNslow - DON_slow
    
    ! Mixing of microbes to litters
    vegn%psoil_fs%c12 = vegn%psoil_fs%c12 + inputs%params_tile%MLmixRatio*fast_L_loss * CUEfast
    vegn%psoil_fs%n14 = vegn%psoil_fs%n14 + inputs%params_tile%MLmixRatio*fast_L_loss * CUEfast/CNm
    vegn%psoil_sl%c12 = vegn%psoil_sl%c12 + inputs%params_tile%MLmixRatio*slow_L_loss * CUEslow
    vegn%psoil_sl%n14 = vegn%psoil_sl%n14 + inputs%params_tile%MLmixRatio*slow_L_loss * CUEslow/CNm
    vegn%pmicr%c12 = vegn%pmicr%c12  - inputs%params_tile%MLmixRatio*(fast_L_loss*CUEfast+slow_L_loss*CUEslow)
    vegn%pmicr%n14  = vegn%pmicr%c12/CNm
      
    ! update mineral N pool (mineralN)
    fast_N_free = MAX(0.0, fast_L_loss*(1./CNfast - CUEfast/CNm))
    slow_N_free = MAX(0.0, slow_L_loss*(1./CNslow - CUEslow/CNm))


    N_loss = vegn%inorg%n14 * &
            MIN(0.25, &
                (A * inputs%params_tile%K_nitrogen * inputs%dt_fast_yr + inputs%params_tile%etaN*runoff))

    vegn%Nloss_yr = vegn%Nloss_yr + N_loss + DON_loss

    N_input = (inputs%init_soil%N_input + lu_props%extra_N_input) * inputs%dt_fast_yr

    delta_N = N_input  &
            + fast_N_free + slow_N_free  &
            + micr_C_loss/CNm &
            - N_loss

    vegn%inorg%n14 = vegn%inorg%n14 + delta_N
    vegn%annualN   = vegn%annualN   + delta_N

    ! Check if soil C/N is above CN0
    fast_N_free = MAX(0.0, vegn%psoil_fs%n14  - vegn%psoil_fs%c12/CN0metabolicL)
    slow_N_free = MAX(0.0, vegn%psoil_sl%n14 - vegn%psoil_sl%c12/CN0structuralL)

    vegn%psoil_fs%n14 = vegn%psoil_fs%n14 - fast_N_free
    vegn%psoil_sl%n14 = vegn%psoil_sl%n14 - slow_N_free
    vegn%inorg%n14    = vegn%inorg%n14 + fast_N_free + slow_N_free
    vegn%annualN      = vegn%annualN   + fast_N_free + slow_N_free
    
    ! Heterotrophic respiration: decomposition of litters and SOM, kgC m-2 step-1
    vegn%rh =  (micr_C_loss + fast_L_loss*(1.-CUEfast)+ slow_L_loss*(1.0-CUEslow))

  end subroutine SOMdecomposition


  function A_function(tsoil, thetaS) result(A)
    !////////////////////////////////////////////////////////////////
    ! The combined reduction in decomposition rate as a funciton of TEMP and MOIST
    ! Based on CENTURY Parton et al 1993 GBC 7(4):785-809 and Bolker's copy of
    ! CENTURY code
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    real :: A                 ! return value, resulting reduction in decomposition rate
    real, intent(in) :: tsoil ! effective temperature for soil carbon decomposition
    real, intent(in) :: thetaS

    real :: soil_temp ! temperature of the soil, deg C
    real :: Td        ! rate multiplier due to temp
    real :: Wd        ! rate reduction due to mositure

    ! coefficeints and terms used in temperaturex term
    real :: Topt,Tmax,t1,t2,tshl,tshr

    soil_temp = tsoil-273.16

    ! EFFECT OF TEMPERATURE , ! from Bolker's century code
    Tmax = 45.0 
    if (soil_temp > Tmax) soil_temp = Tmax 
    Topt = 35.0 
    tshr = 0.2 
    tshl = 2.63 
    t1 = (Tmax-soil_temp)/(Tmax-Topt) 
    t2 = exp((tshr/tshl)*(1.-t1**tshl)) 
    Td = t1**tshr*t2 
    if (soil_temp > -10) Td = Td+0.05 
    if (Td > 1.) Td = 1. 

    ! EFFECT OF MOISTURE
    ! Linn and Doran, 1984, Soil Sci. Amer. J. 48:1267-1272
    ! This differs from the Century Wd
    ! was modified by slm/ens based on the figures from the above paper 
    ! (not the reported function)
    if (thetaS <= 0.3) then
      Wd = 0.2 
    else if (thetaS <= 0.6) then
      Wd = 0.2+0.8*(thetaS-0.3)/0.3
    else 
      Wd = 1.0 ! exp(2.3*(0.6-thetaS)); ! Weng, 2016-11-26
    endif
    A = (Td*Wd)  ! the combined (multiplicative) effect of temp and water
    ! on decomposition rates
  end function A_function


  !=======================================================================
  !=================== Cohort management =================================
  !=======================================================================

  subroutine kill_old_grass(vegn)
    !////////////////////////////////////////////////////////////////
    ! Kill old grass cohorts
    ! Weng, 01/22/2023
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! ---- local vars
    logical :: OldGrass
    logical :: at_least_one_survivor
    type(cohort_stack_item), pointer :: it

    at_least_one_survivor = .FALSE.

    ! We check that there will be at least one survivor
    it => vegn%cohorts()
    do while (associated(it))
      associate(sp=>it%cohort%sp())
        OldGrass = (sp%lifeform == 0 .and. it%cohort%age > 3.0)
        if (.not. OldGrass) then
          at_least_one_survivor = .TRUE.
          exit
        end if
      end associate
      it => it%next()
    end do

    if (at_least_one_survivor) then
      it => vegn%cohorts()
      do while (associated(it))
        associate(sp=>it%cohort%sp())
          OldGrass = (sp%lifeform == 0 .and. it%cohort%age > 3.0)
          if (OldGrass) then
            it => vegn%thin_cohort(it)
          else
            it => it%next()
          end if
        end associate
      end do
    end if

  end subroutine kill_old_grass
  

  function leaf_area_from_biomass(bl,species) result (area)
    !////////////////////////////////////////////////////////////////
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    real :: area ! returned value
    real,    intent(in) :: bl      ! biomass of leaves, kg C/individual
    integer, intent(in) :: species ! species

    area = bl/inputs%params_species(species)%LMA

  end function

end module md_vegetation_processes_biomee
