module md_vegetation_biomee
  !////////////////////////////////////////////////////////////////
  ! Contains all vegetation-related subroutines for BiomeE.
  ! Code is adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !---------------------------------------------------------------  
  use datatypes
  use md_soil_biomee
  use md_interface_biomee, only: myinterface

  implicit none
  private

  ! public subroutines
  public :: initialize_cohort_from_biomass, initialize_vegn_tile
  public :: vegn_CNW_budget, vegn_phenology, vegn_growth_EW,update_layer_LAI ! , vegn_CNW_budget_daily
  public :: vegn_reproduction, vegn_annualLAImax_update !, annual_calls
  public :: vegn_nat_mortality, vegn_species_switch !, vegn_starvation
  public :: relayer_cohorts, vegn_mergecohorts, kill_lowdensity_cohorts
  public :: vegn_annual_starvation,Zero_diagnostics

contains

  !========================================================================
  !============= Carbon, nitrogen and water budget    =====================
  !========================================================================

  subroutine vegn_CNW_budget( vegn, forcing, init )
    !////////////////////////////////////////////////////////////////
    ! hourly carbon, nitrogen, and water dynamics, Weng 2016-11-25
    ! include Nitrogen uptake and carbon budget
    ! C_growth is calculated here to drive plant growth and reproduciton
    !---------------------------------------------------------------
    use md_forcing_biomee, only: climate_type
    use md_gpp_biomee, only: gpp

    type(vegn_tile_type), intent(inout) :: vegn
    type(climate_type), intent(in) :: forcing
    ! is true on the very first simulation day (first subroutine call of each gridcell)
    logical, intent(in) :: init

    ! local variables
    type(cohort_type), pointer :: cc  
    integer:: i
    real   :: tair, tsoil  ! temperature of soil, degC
    real   :: theta        ! soil wetness, unitless
    integer :: iyears

    ! Climatic variable
    tair   = forcing%Tair - 273.16   ! conversion to degC
    tsoil  = forcing%tsoil - 273.16  ! conversion to degC
    theta  = (vegn%wcl(2) - WILTPT) / (FLDCAP - WILTPT)

    ! Photosynsthesis
    call gpp( forcing, vegn, init )
    
    ! Update soil water
    call SoilWaterDynamicsLayer( forcing, vegn )
    
    ! Respiration and allocation for growth
    do i = 1, vegn%n_cohorts

      cc => vegn%cohorts(i)
      associate ( sp => spdata(cc%species) )

      ! increment the cohort age
      cc%age = cc%age + myinterface%dt_fast_yr

      ! Maintenance respiration
      call plant_respiration( cc, forcing%tair ) ! get resp per tree per time step

      cc%resp = cc%resp + (cc%resg * myinterface%step_seconds) / secs_per_day ! put growth respiration to tot resp
      cc%resp = cc%resp * myinterface%params_tile%tf_base          ! scaling for calibration
      cc%npp  = cc%gpp - cc%resp       ! kgC tree-1 step-1

      ! detach photosynthesis model from plant growth
      cc%plabl%c%c12 = cc%plabl%c%c12 + cc%npp
      cc%plabl%n%n14 = cc%plabl%n%n14 + cc%fixedN

      end associate
    enddo ! all cohorts
    
    ! update soil carbon
    call SOMdecomposition( vegn, forcing%tsoil, theta )
    
    ! Nitrogen uptake
    call vegn_N_uptake( vegn, forcing%tsoil )
    
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
    sp = cc%species
    ! tf_base = myinterface%params_tile%tf_base
    
    ! temperature response function
    ! tf  = tf_base * exp(9000.0 * (1.0/298.16 - 1.0/tairK))
    tf  = exp(9000.0 * (1.0/298.16 - 1.0/tairK))
    
    !  tfs = thermal_inhibition(tsoil)  ! original
    tfs = tf ! Rm_T_response_function(tsoil) ! Weng 2014-01-14

    ! With nitrogen model, leaf respiration is a function of leaf nitrogen
    !NSCtarget = 3.0 * (cc%bl_max + cc%br_max)
    fnsc = 1.0 ! min(max(0.0,cc%plabl%c%c12/NSCtarget),1.0)
    ! Acambium = PI * cc%DBH * cc%height * 1.2 ! see Weng et al. 2015: Acambium~D^1.5 -> H~D^0.5 and D*H is proportional to D^1.5
    exp_acambium = 1.5 !(1.5 - 2) Use this exponent to make Acambium~D^2. Ensheng suggested range 1.5 to 2.
    Acambium = PI * cc%DBH ** exp_acambium * cc%height * 1.2

    ! Facultive Nitrogen fixation
    !if (cc%plabl%n%n14 < cc%NSNmax .and. cc%plabl%c%c12 > 0.5 * NSCtarget) then
    !   cc%fixedN = spdata(sp)%NfixRate0 * cc%proot%c%c12 * tf * myinterface%dt_fast_yr ! kgN tree-1 step-1
    !else
    !   cc%fixedN = 0.0 ! spdata(sp)%NfixRate0 * cc%proot%c%c12 * tf * myinterface%dt_fast_yr ! kgN tree-1 step-1
    !endif

    ! Obligate Nitrogen Fixation
    cc%fixedN = fnsc*spdata(sp)%NfixRate0 * cc%proot%c%c12 * tf * myinterface%dt_fast_yr ! kgN tree-1 step-1
    r_Nfix    = spdata(sp)%NfixCost0 * cc%fixedN ! + 0.25*spdata(sp)%NfixCost0 * cc%N_uptake    ! tree-1 step-1
    
    ! LeafN    = spdata(sp)%LNA * cc%leafarea  ! gamma_SW is sapwood respiration rate (kgC m-2 Acambium yr-1)
    r_stem   = fnsc*spdata(sp)%gamma_SW  * Acambium * tf * myinterface%dt_fast_yr ! kgC tree-1 step-1
    r_root   = fnsc*spdata(sp)%gamma_FR  * cc%proot%n%n14 * tf * myinterface%dt_fast_yr ! root respiration ~ root N    
    cc%resp = cc%resl + r_stem + r_root + r_Nfix   !kgC tree-1 step-1
    cc%resr = r_root + r_Nfix ! tree-1 step-1

    ! print*, 'tf',  tf  ! xxx debug

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
    implicit none
    type(cohort_type), intent(inout) :: cc
    
    ! local variables
    real :: NSCtarget
    real :: C_push, C_pull
    real :: N_push, N_pull
    real :: LFR_rate ! make these two variables to PFT-specific parameters
    ! make these two variables to PFT-specific parameters
    LFR_rate = 1.0 ! 1.0/5.0 ! filling rate/day
    associate ( sp => spdata(cc%species) )
      NSCtarget = 3.0 * (cc%bl_max + cc%br_max)      ! kgC/tree
      ! Fetch C from labile C pool if it is in the growing season
      if (cc%status == LEAF_ON) then ! growing season
        C_pull = LFR_rate * (Max(cc%bl_max - cc%pleaf%c%c12,0.0) +   &
          Max(cc%br_max - cc%proot%c%c12,0.0))
        N_pull = LFR_rate * (Max(cc%bl_max - cc%pleaf%c%c12,0.0)/sp%CNleaf0 +  &
          Max(cc%br_max - cc%proot%c%c12,0.0)/sp%CNroot0)
        C_push = cc%plabl%c%c12 / (ndayyear * sp%tauNSC) ! max(cc%plabl%c%c12-NSCtarget, 0.0)/(ndayyear*sp%tauNSC)
        N_push = cc%plabl%n%n14 / (ndayyear * sp%tauNSC) ! 4.0 * C_push/sp%CNsw0  !
        cc%N_growth = Min(max(0.02 * cc%plabl%n%n14,0.0), N_pull + N_push)
        cc%C_growth = Min(max(0.02 * cc%plabl%c%c12,0.0), C_pull + C_push) ! Max(0.0,MIN(0.02*(cc%plabl%c%c12-0.2*NSCtarget), C_pull+C_push))
        !!! cc%plabl%c%c12      = cc%plabl%c%c12 - cc%C_growth ! just an estimate, not out yet
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
    type(cohort_type), pointer :: cc    ! current cohort
    real :: CSAtot ! total cross section area, m2
    real :: CSAsw  ! Sapwood cross sectional area, m2
    real :: CSAwd  ! Heartwood cross sectional area, m2
    real :: DBHwd  ! diameter of heartwood at breast height, m
    real :: BSWmax ! max sapwood biomass, kg C/individual
    real :: G_LFR  ! amount of carbon spent on leaf and root growth
    real :: dSeed ! allocation to seeds, Weng, 2016-11-26
    real :: dBL, dBR ! tendencies of leaf and root biomass, kgC/individual
    real :: dBSW ! tendency of sapwood biomass, kgC/individual
    real :: dBHW ! tendency of wood biomass, kgC/individual
    real :: dDBH ! tendency of breast height diameter, m
    real :: dCA ! tendency of crown area, m2/individual
    real :: dHeight ! tendency of vegetation height
    real :: dNS    ! Nitrogen from SW to HW
    ! real :: sw2nsc = 0.0 ! conversion of sapwood to non-structural carbon
    real :: BL_u, BL_c
    real :: LFR_deficit, LF_deficit, FR_deficit
    real :: N_demand,Nsupplyratio,extraN
    real :: r_N_SD
    logical :: do_editor_scheme = .False.
    integer :: i
    do_editor_scheme = .False. ! .True.

    ! Turnover of leaves and fine roots
    call vegn_tissue_turnover( vegn )

    ! Allocate C_gain to tissues
    vegn%LAI = 0.0    ! added here, otherwise LAI is the sum of current and new, Beni 24 Aug 2022
    do i = 1, vegn%n_cohorts   
      cc => vegn%cohorts(i)

      ! call biomass_allocation( cc )
      associate (sp => spdata(cc%species))

      if (cc%status == LEAF_ON) then
        
        ! Get carbon from NSC pool. This sets cc%C_growth
        call fetch_CN_for_growth( cc )

        ! Allocate carbon to the plant pools
        ! calculate the carbon spent on growth of leaves and roots
        LF_deficit = max(0.0, cc%bl_max - cc%pleaf%c%c12)
        FR_deficit = max(0.0, cc%br_max - cc%proot%c%c12)
        LFR_deficit = LF_deficit + FR_deficit
        G_LFR = max(min(LF_deficit + FR_deficit,  &
          f_LFR_max  * cc%C_growth), 0.0)

        ! and distribute it between roots and leaves
        dBL  = min(G_LFR, max(0.0, &
                (G_LFR*cc%bl_max + cc%bl_max*cc%proot%c%c12 - cc%br_max*cc%pleaf%c%c12)/(cc%bl_max + cc%br_max) &
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
        
        ! IF(cc%N_growth < N_demand)THEN
        !     ! a new method, Weng, 2019-05-21
        !     ! same ratio reduction for leaf, root, and seed if(cc%N_growth < N_demand)
        !     Nsupplyratio = MAX(0.0, MIN(1.0, cc%N_growth/N_demand))
        !     !r_N_SD = (cc%N_growth-cc%C_growth/sp%CNsw0)/(N_demand-cc%C_growth/sp%CNsw0) ! fixed wood CN
        !     r_N_SD = cc%N_growth/N_demand ! = Nsupplyratio
        !     if(sp%lifeform > 0 )then ! for trees
        !        if(r_N_SD<=1.0 .and. r_N_SD>0.0)then
        !         dBSW =  dBSW + (1.0-r_N_SD) * (dBL+dBR+dSeed)
        !         dBR  =  r_N_SD * dBR
        !         dBL  =  r_N_SD * dBL
        !         dSeed=  r_N_SD * dSeed
        !        elseif(r_N_SD <= 0.0)then
        !         dBSW = cc%N_growth/sp%CNsw0
        !         dBR  =  0.0
        !         dBL  =  0.0
        !         dSeed=  0.0
        !        endif
        !     else ! for grasses
        !        dBR  =  Nsupplyratio * dBR
        !        dBL  =  Nsupplyratio * dBL
        !        dSeed=  Nsupplyratio * dSeed
        !        dBSW =  Nsupplyratio * dBSW
        !     endif
        ! ENDIF

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
        cc%pleaf%c%c12    = cc%pleaf%c%c12 + dBL
        cc%proot%c%c12    = cc%proot%c%c12 + dBR
        cc%psapw%c%c12    = cc%psapw%c%c12 + dBSW
        cc%pseed%c%c12    = cc%pseed%c%c12 + dSeed
        cc%plabl%c%c12    = cc%plabl%c%c12 - dBR - dBL - dSeed - dBSW
        cc%resg = 0.5 * (dBR + dBL + dSeed + dBSW) !  daily

        ! update nitrogen pools, Nitrogen allocation
        cc%pleaf%n%n14 = cc%pleaf%n%n14 + dBL   /sp%CNleaf0
        cc%proot%n%n14 = cc%proot%n%n14 + dBR   /sp%CNroot0
        cc%pseed%n%n14 = cc%pseed%n%n14 + dSeed /sp%CNseed0
        cc%psapw%n%n14 = cc%psapw%n%n14 + f_N_add * cc%plabl%n%n14 + &
          (cc%N_growth - dBL/sp%CNleaf0 - dBR/sp%CNroot0 - dSeed/sp%CNseed0)
        !extraN = max(0.0,cc%psapw%n%n14+cc%pwood%n%n14 - (cc%psapw%c%c12+cc%pwood%c%c12)/sp%CNsw0)
        extraN   = max(0.0,cc%psapw%n%n14 - cc%psapw%c%c12/sp%CNsw0)
        cc%psapw%n%n14 = cc%psapw%n%n14 - extraN
        cc%plabl%n%n14 = cc%plabl%n%n14 + extraN - f_N_add * cc%plabl%n%n14 - cc%N_growth !! update NSN
        cc%N_growth = 0.0

        ! accumulated C allocated to leaf, root, and wood
        cc%NPPleaf = cc%NPPleaf + dBL
        cc%NPProot = cc%NPProot + dBR
        cc%NPPwood = cc%NPPwood + dBSW

        ! update breast height diameter given increase of bsw
        dDBH   = dBSW / (sp%thetaBM * sp%alphaBM * cc%DBH**(sp%thetaBM - 1.0))
        dHeight= sp%thetaHT * sp%alphaHT * cc%DBH**(sp%thetaHT - 1) * dDBH
        dCA    = sp%thetaCA * sp%alphaCA * cc%DBH**(sp%thetaCA - 1) * dDBH

        ! update plant architecture
        cc%DBH       = cc%DBH       + dDBH
        cc%height    = cc%height    + dHeight
        cc%crownarea = cc%crownarea + dCA
        cc%leafarea  = leaf_area_from_biomass(cc%pleaf%c%c12, cc%species)
        cc%lai       = cc%leafarea/cc%crownarea !(cc%crownarea *(1.0-sp%internal_gap_frac))
        vegn%LAI     = vegn%LAI + cc%leafarea  * cc%nindivs

        call rootarea_and_verticalprofile( cc )

        ! convert sapwood to heartwood for woody plants ! Nitrogen from sapwood to heart wood
        if (sp%lifeform > 0) then
          CSAsw  = cc%bl_max/sp%LMA * sp%phiCSA * cc%height ! with Plant hydraulics, Weng, 2016-11-30
          CSAtot = 0.25 * PI * cc%DBH**2
          CSAwd  = max(0.0, CSAtot - CSAsw)
          DBHwd  = 2.0 * sqrt(CSAwd/PI)
          BSWmax = sp%alphaBM * (cc%DBH**sp%thetaBM - DBHwd**sp%thetaBM)
          dBHW   = max(cc%psapw%c%c12 - BSWmax, 0.0)
          dNS    = dBHW / cc%psapw%c%c12 * cc%psapw%n%n14

          ! update C and N of sapwood and wood
          cc%pwood%c%c12 = cc%pwood%c%c12 + dBHW
          cc%psapw%c%c12 = cc%psapw%c%c12 - dBHW
          cc%psapw%n%n14 = cc%psapw%n%n14 - dNS
          cc%pwood%n%n14 = cc%pwood%n%n14 + dNS
        endif

        ! update bl_max and br_max daily
        BL_c = sp%LMA * sp%LAImax * cc%crownarea * &
                (1.0-sp%internal_gap_frac) /max(1,cc%layer)
        BL_u = sp%LMA*cc%crownarea*(1.0-sp%internal_gap_frac) * &
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

        ! Grasses have the saem bl_max regardless of their layer position
        if (sp%lifeform == 0) then
          cc%bl_max = BL_c
          cc%br_max = sp%phiRL * cc%bl_max/(sp%LMA * sp%SRA)
        endif ! for grasses

      elseif (cc%status == LEAF_OFF .and. cc%C_growth > 0.0) then
        cc%plabl%c%c12 = cc%plabl%c%c12 + cc%C_growth
        cc%resg = 0.0
      endif

      ! reset carbon acculmulation terms
      cc%C_growth = 0

      end associate
    enddo
    cc => null()

  end subroutine vegn_growth_EW


  subroutine update_layer_LAI( vegn )
    !////////////////////////////////////////////////////////////////
    ! Updates LAI per canopy layer
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc
    integer :: i, layer

    ! update accumulative LAI for each corwn layer
    vegn%LAI      = 0.0
    vegn%LAIlayer = 0.0
    do i = 1, vegn%n_cohorts
     cc => vegn%cohorts(i)
     layer = Max (1, Min(cc%layer,9)) ! between 1~9
     vegn%LAIlayer(layer) = vegn%LAIlayer(layer) + cc%leafarea * cc%nindivs !/(1.0-sp%internal_gap_frac)
    enddo
  
  end subroutine update_layer_LAI


  subroutine rootarea_and_verticalprofile( cc )
    !////////////////////////////////////////////////////////////////
    ! Weng: partioning root area into layers, 10-24-2017
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(cohort_type), intent(inout) :: cc
    
    ! local variables
    integer :: j

    associate (sp => spdata(cc%species) )
      cc%rootarea  = cc%proot%c%c12 * sp%SRA
      do j=1,max_lev
       cc%rootareaL(j) = cc%rootarea * sp%root_frac(j)
     enddo
    end associate
  
  end subroutine rootarea_and_verticalprofile


  subroutine vegn_phenology( vegn )
    !////////////////////////////////////////////////////////////////
    ! Determines phenology state (leaf on/off)
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc
    integer :: i
    ! real    :: grassdensity   ! for grasses only
    ! real    :: BL_u,BL_c
    real    :: ccNSC, ccNSN
    logical :: cc_firstday = .false.
    logical :: TURN_ON_life = .false., TURN_OFF_life

    vegn%litter = 0   ! daily litter

    ! update vegn GDD and tc_pheno
    vegn%gdd      = vegn%gdd + max(0.0, vegn%tc_daily - 278.15)
    vegn%tc_pheno = vegn%tc_pheno * 0.8 + vegn%Tc_daily * 0.2

    ! ON and OFF of phenology: change the indicator of growing season for deciduous
    cohortloop2: do i = 1,vegn%n_cohorts
      cc => vegn%cohorts(i)

      ! update GDD for each cohort
      cc%gdd = cc%gdd + max(0.0, vegn%tc_daily - 278.15) ! GDD5

      associate (sp => spdata(cc%species) )

      ! for evergreen
      if (sp%phenotype==1 .and. cc%status==LEAF_OFF) cc%status=LEAF_ON

      ! for deciduous and grasses
      TURN_ON_life = (sp%phenotype == 0 .and. &
        cc%status    == LEAF_OFF       .and. &
        cc%gdd        > sp%gdd_crit    .and. &
        vegn%tc_pheno > sp%tc_crit_on) .and. &
        (sp%lifeform .ne. 0 .OR.(sp%lifeform .eq. 0 .and. cc%layer==1))

      cc_firstday = .false.
      if (TURN_ON_life) then
        cc%status = LEAF_ON ! Turn on a growing season
        cc_firstday = .True.
      endif

      ! Reset grass density at the first day of a growing season
      if (cc_firstday .and. sp%lifeform == 0 .and. cc%age > 2.0) then
        
        ! reset grass density and size for perenials
        ccNSC   = (cc%plabl%c%c12 + cc%pleaf%c%c12 + cc%psapw%c%c12 + &
          cc%pwood%c%c12 + cc%proot%c%c12 + cc%pseed%c%c12) * cc%nindivs
        ccNSN   = (cc%plabl%n%n14 + cc%pleaf%n%n14 + cc%psapw%n%n14 + &
          cc%pwood%n%n14 + cc%proot%n%n14 + cc%pseed%n%n14) * cc%nindivs
        
        ! reset
        cc%nindivs = MIN(ccNSC /sp%seedlingsize, ccNSN/(sp%seedlingsize/sp%CNroot0))
        cc%psapw%c%c12 = f_initialBSW * sp%seedlingsize  ! for setting up a initial size
        cc%proot%c%c12 = 0.25 * cc%psapw%c%c12
        cc%pleaf%c%c12 = 0.0
        cc%pwood%c%c12 = 0.0
        cc%pseed%c%c12 = 0.0
        cc%plabl%c%c12 = ccNSC / cc%nindivs - &
          (cc%pleaf%c%c12 + cc%psapw%c%c12 + cc%pwood%c%c12 + cc%proot%c%c12 + cc%pseed%c%c12)

        ! beni: added NPP accounting (for output)
        cc%NPPleaf = cc%NPPleaf + cc%pleaf%c%c12
        cc%NPProot = cc%NPProot + cc%proot%c%c12
        cc%NPPwood = cc%NPPwood + cc%psapw%c%c12 + cc%pwood%c%c12

        ! nitrogen pools
        cc%psapw%n%n14 = cc%psapw%c%c12  /sp%CNsw0
        cc%proot%n%n14 = cc%proot%c%c12   /sp%CNroot0
        cc%pleaf%n%n14 = 0.0
        cc%pwood%n%n14 = 0.0
        cc%pseed%n%n14 = 0.0
        cc%plabl%n%n14 = ccNSN/cc%nindivs - &
          (cc%pleaf%n%n14 + cc%psapw%n%n14 + cc%pwood%n%n14 + cc%proot%n%n14 + cc%pseed%n%n14)

        call rootarea_and_verticalprofile( cc )
        call init_cohort_allometry( cc )
      endif
      end associate

    enddo cohortloop2

    if (TURN_ON_life) call relayer_cohorts( vegn )

    ! OFF of a growing season
    cohortloop3: do i = 1,vegn%n_cohorts

      cc => vegn%cohorts(i)
      associate (sp => spdata(cc%species) )
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
    
    enddo cohortloop3

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
    real    :: loss_coarse, loss_fine, lossN_coarse, lossN_fine
    real    :: dAleaf, dBL, dBR, dNL, dNR, dBStem, dNStem      ! per day
    real    :: leaf_fall_rate, root_mort_rate      ! per day

    leaf_fall_rate = 0.05; root_mort_rate = 0.025

    ! End a growing season: leaves fall for deciduous
    associate (sp => spdata(cc%species) )
    
    if (cc%status == LEAF_OFF .AND. cc%pleaf%c%c12 > 0.0) then

      dBL = min(leaf_fall_rate * cc%bl_max, cc%pleaf%c%c12)
      dBR = min(root_mort_rate * cc%br_max, cc%proot%c%c12)  ! Just for test: keep roots
      dBStem = 0.0 ! trees
      dNStem = 0.0 ! trees

      if (sp%lifeform == 0) then  ! grasses
        dBStem = MIN(1.0,dBL/cc%pleaf%c%c12) * cc%psapw%c%c12
        dNStem = MIN(1.0,dBL/cc%pleaf%c%c12) * cc%psapw%n%n14
      endif

      ! Nitrogen out
      if (cc%pleaf%c%c12 > 0) then
        dNL = dBL / cc%pleaf%c%c12 * cc%pleaf%n%n14 !dBL/sp%CNleaf0
      else
        dNL = 0.0
      endif

      if (cc%proot%c%c12 > 0) then
        dNR = dBR / cc%proot%c%c12 * cc%proot%n%n14 !dBR/sp%CNroot0
      else
        dNR = 0.0
      endif

      dAleaf = leaf_area_from_biomass(dBL, cc%species)

      ! Retranslocation to NSC and NSN
      cc%plabl%c%c12 = cc%plabl%c%c12 + l_fract  * (dBL + dBR + dBStem)
      cc%plabl%n%n14 = cc%plabl%n%n14 + retransN * (dNL + dNR + dNStem)

      ! update plant pools
      cc%pleaf%c%c12 = cc%pleaf%c%c12 - dBL
      cc%proot%c%c12 = cc%proot%c%c12 - dBR
      cc%psapw%c%c12 = cc%psapw%c%c12 - dBStem

      cc%pleaf%n%n14 = cc%pleaf%n%n14 - dNL
      cc%proot%n%n14 = cc%proot%n%n14 - dNR
      cc%psapw%n%n14 = cc%psapw%n%n14 - dNStem

      ! update NPP for leaves, fine roots, and wood
      cc%NPPleaf = cc%NPPleaf - l_fract * dBL
      cc%NPProot = cc%NPProot - l_fract * dBR
      cc%NPPwood = cc%NPPwood - l_fract * dBStem

      cc%leafarea= leaf_area_from_biomass(cc%pleaf%c%c12, cc%species)
      cc%lai     = cc%leafarea/(cc%crownarea * (1.0 - sp%internal_gap_frac))

      ! Update plant size (for grasses)
      !call init_cohort_allometry( cc )

      ! put C and N into soil pools:  Substraction of C and N from leaf and root pools
      loss_coarse  = (1.0 - l_fract)  * cc%nindivs * (dBStem + dBL - dAleaf * LMAmin)
      loss_fine    = (1.0 - l_fract)  * cc%nindivs * (dBR    + dAleaf * LMAmin)
      lossN_coarse = (1.0 - retransN) * cc%nindivs * (dNStem + dNL - dAleaf * sp%LNbase)
      lossN_fine   = (1.0 - retransN) * cc%nindivs * (dNR    + dAleaf * sp%LNbase)

      vegn%psoil_fs%c%c12 = vegn%psoil_fs%c%c12 +  &
        fsc_fine * loss_fine + fsc_wood * loss_coarse
      vegn%psoil_sl%c%c12 = vegn%psoil_sl%c%c12 +   &
        (1.0 - fsc_fine)*loss_fine + (1.0 - fsc_wood)*loss_coarse

      ! Nitrogen to soil SOMs
      vegn%psoil_fs%n%n14  = vegn%psoil_fs%n%n14 +    &
        fsc_fine * lossN_fine + fsc_wood * lossN_coarse
      vegn%psoil_sl%n%n14 = vegn%psoil_sl%n%n14 +   &
        (1.0 - fsc_fine) * lossN_fine + (1.0 - fsc_wood) * lossN_coarse

      ! annual N from plants to soil
      vegn%N_P2S_yr = vegn%N_P2S_yr + lossN_fine + lossN_coarse

    endif
    end associate
  
  end subroutine Seasonal_fall


  subroutine vegn_nat_mortality (vegn)
    !////////////////////////////////////////////////////////////////
    ! Determines mortality and updates tile
    !---------------------------------------------------------------
    use md_interface_biomee, only: myinterface
    
    !   TODO: update background mortality rate as a function of wood density (Weng, Jan. 07 2017)
    type(vegn_tile_type), intent(inout) :: vegn
    ! real, intent(in) :: deltat ! seconds since last mortality calculations, s

    ! ---- local vars
    type(cohort_type), pointer :: cc => null()
    ! type(spec_data_type),   pointer :: sp

    ! integer :: idx(vegn%n_cohorts)
    real :: deathrate = 0 ! mortality rate, 1/year
    real :: deadtrees ! number of trees that died over the time step
    integer :: totCC,i,k

    ! real :: nindivs_new, frac_new
    real, dimension(:), allocatable :: cai_partial != 0.0 !max_cohorts
    real, parameter :: min_nindivs = 1e-5 ! 2e-15 ! 1/m. If nindivs is less than this number, 

    ! then the entire cohort is killed; 2e-15 is approximately 1 individual per Earth 
    ! logical :: merged(vegn%n_cohorts) ! mask to skip cohorts that were already merged
    real :: cCAI
    real :: dn ! number of trees that died due to CAI_partial>CAI_max
    real :: param_dbh_under 
    real :: param_nsc_under 
    real :: param_gr_under
    real :: param_dbh 
    real :: param_nsc 
    real :: param_gr
    real :: CAI_max

    if ((trim(myinterface%params_siml%method_mortality) == "const_selfthin")) then

      ! Remove a big amount of very small trees first
      if (cc%layer > 1) deathrate = 0.2 !sp%mortrate_d_u
      deadtrees = cc%nindivs * deathrate
      call plant2soil(vegn, cc, deadtrees)

      ! Update plant density
      cc%nindivs = cc%nindivs - deadtrees

      ! set calibratable mortality parameter
      CAI_max = myinterface%params_tile%par_mort

      ! Calculate cumulative CAI from shortest trees
      totCC = vegn%n_cohorts
      allocate(cai_partial(totCC))

      ! calculate cai_partial and the number of cohorts with cai_partial < CAI_max (keep them)
      cai_partial = 0.0
      do i = totCC, 1, -1
        cc => vegn%cohorts(i)
        cCAI = cc%crownarea * cc%nindivs
        if (i==totCC) then
          cai_partial(i) = cCAI
        else
          cai_partial(i) = cai_partial(i+1) + cCAI
        end if
      enddo

      ! Kill the trees that lead to total CAI > CAI_max
      k = 0 ! for checking how many cohorts trimmed
      do i =1, totCC-1 ! at least keep the last cohort (totCC)
        cc => vegn%cohorts(i)
        if(cai_partial(i) > CAI_max)then
          dn = (cai_partial(i) - max(CAI_max, cai_partial(i + 1))) / cc%crownarea

          ! Carbon and Nitrogen from dead plants to soil pools
          call plant2soil(vegn, cc, dn)

          ! Update plant density
          cc%nindivs = cc%nindivs - dn
          k = k + 1
        else
         exit
        endif
      enddo

      ! Remove the cohorts with 0 individuals, (never used b/c k<2)
      if(k >= 2) call kill_lowdensity_cohorts(vegn)

      ! final check, can be removed if the model runs well
      cai_partial = 0.0
      do i = vegn%n_cohorts, 1, -1
        cc => vegn%cohorts(i)
        cCAI = cc%crownarea * cc%nindivs
        if (i == vegn%n_cohorts) then
          cai_partial(i) = cCAI
        else
          cai_partial(i) = cai_partial(i+1) + cCAI
        end if
      enddo

      deallocate(cai_partial)
 
    else

      do i = 1, vegn%n_cohorts
        cc => vegn%cohorts(i)
        associate ( sp => spdata(cc%species))

        if ((trim(myinterface%params_siml%method_mortality) == "cstarvation")) then
          
          ! set calibratable parameter
          param_nsc_under = myinterface%params_tile%par_mort_under
          param_nsc       = myinterface%params_tile%par_mort

          ! Understory mortality
          if (cc%layer > 1) then !
            deathrate = param_nsc_under * sp%mortrate_d_u * &
                     (1. + A_mort*exp(B_mort*cc%dbh))/ &
                     (1. +        exp(B_mort*cc%dbh)) 

          else  
            ! Canopy mortality
            if (cc%bl_max > 0) then
            deathrate = param_nsc * 0.05 * (exp(-3.5*(cc%plabl%c%c12/cc%bl_max))/(0.01+exp(-3.5*(cc%plabl%c%c12/cc%bl_max)))) ! -3.5,-2.5,-2
            endif
          endif

        else if ((trim(myinterface%params_siml%method_mortality) == "growthrate")) then
          
          ! set calibratable parameter
          param_gr_under = myinterface%params_tile%par_mort_under
          param_gr       = myinterface%params_tile%par_mort

          ! Understory mortality
          if (cc%layer > 1) then !
            deathrate = param_gr_under * sp%mortrate_d_u * &
                     (1. + A_mort*exp(B_mort*cc%dbh))/ &
                     (1. +        exp(B_mort*cc%dbh)) 
          else  
          ! Canopy mortality
          ! deathrate = param_gr * 0.05 *    &
          !                  (1.*exp(1*(cc%psapw%c%c12+cc%pwood%c%c12-cc%ABG_ys-6.0))/ &
          !                  (1. + exp(1*(cc%psapw%c%c12+cc%pwood%c%c12-cc%ABG_ys-6.0))))
          deathrate = min(1.0, param_dbh * 0.015 * cc%dbh ** 1.5) ! 1.5, 1.6, 1.7
          endif

        else if ((trim(myinterface%params_siml%method_mortality) == "dbh")) then 
     
          ! set calibratable parameter
          param_dbh_under = myinterface%params_tile%par_mort_under
          param_dbh       = myinterface%params_tile%par_mort

          if (sp%lifeform == 0)then  ! for grasses
            if (cc%layer > 1) then
              deathrate = sp%mortrate_d_u
            else
              deathrate = sp%mortrate_d_c
            endif
          else                    ! for trees
            if (cc%layer > 1) then ! Understory layer mortality Weng 2015: deathrate = 0.075*(1+9*exp(-60*cc%dbh))/(1+exp(-60*cc%dbh))
              deathrate = param_dbh_under * sp%mortrate_d_u * &
                     (1.0 + A_mort*exp(B_mort*cc%dbh))/ &
                     (1.0 +        exp(B_mort*cc%dbh)) 

            else  ! First layer mortality Weng 2015: deathrate = 0.01*(1+5*exp(4*(cc%dbh-2)))/(1+exp(4*(cc%dbh-2)))
              if(myinterface%params_siml%do_U_shaped_mortality)then
                ! deathrate = param_dbh * 0.1 *    &
                !            (1.*exp(2.*(cc%dbh-1))/  &
                !            (1. + exp(2.*(cc%dbh-1))))
                deathrate = min(1.0, param_dbh * cc%dbh ** 1.5) ! 1.5, 2.5, 5
              else
                deathrate = sp%mortrate_d_c !0.01
              endif
            endif
          endif
         
        endif

        ! previous setup allowed death rates > 1 (hence negative ind)
        deathrate = min(1.0, deathrate + 0.01) 
        deadtrees = cc%nindivs * deathrate

        ! record mortality rates at cohort level
        cc%deathratevalue = deathrate

        ! Carbon and Nitrogen from dead plants to soil pools
        call plant2soil(vegn, cc, deadtrees)

        ! Update plant density
        cc%nindivs = cc%nindivs - deadtrees
        ! vegn%n_deadtrees = deadtrees
        ! vegn%c_deadtrees = vegn%c_deadtrees + deadtrees*(cc%plabl%c%c12 + cc%pseed%c%c12 + cc%pleaf%c%c12 + cc%proot%c%c12 + cc%psapw%c%c12 + cc%pwood%c%c12)
        end associate
      enddo

      ! Remove the cohorts with very few individuals
      call kill_lowdensity_cohorts( vegn )

    endif

  end subroutine vegn_nat_mortality


  subroutine vegn_annual_starvation( vegn ) ! Annual
    !////////////////////////////////////////////////////////////////
    ! Mortality due to C starvation (NSC below threshold)
    ! Starvation due to low NSC and annual NPP
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables --------
    real :: deathrate ! mortality rate, 1/year
    real :: deadtrees ! number of trees that died over the time step
    integer :: i
    type(cohort_type), pointer :: cc
    ! type(cohort_type), dimension(:), pointer :: ccold, ccnew

    do i = 1, vegn%n_cohorts
      
      cc => vegn%cohorts(i)
      associate ( sp => spdata(cc%species)  )

      ! Mortality due to starvation
      deathrate = 0.0
     
      if (cc%plabl%c%c12 < 0.01*cc%bl_max) then

        deathrate = 1.0
        deadtrees = cc%nindivs * deathrate !individuals / m2

        ! Carbon and Nitrogen from plants to soil pools
        call plant2soil(vegn, cc, deadtrees)

        ! update cohort individuals
        cc%nindivs = 0.0 ! cc%nindivs*(1.0 - deathrate)

        ! print*,'i, deadtrees', i, deadtrees

      else
        deathrate = 0.0
      endif
      end associate
    enddo
    ! Remove the cohorts with 0 individuals
    call kill_lowdensity_cohorts( vegn )

  end subroutine vegn_annual_starvation


  subroutine plant2soil(vegn, cc, deadtrees)
    !////////////////////////////////////////////////////////////////
    ! Transfer of deat biomass to litter pools
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn
    type(cohort_type),    intent(inout) :: cc
    real,                 intent(in)    :: deadtrees ! dead trees/m2

    ! local variables --------
    real :: loss_fine,loss_coarse
    real :: lossN_fine,lossN_coarse

    associate (sp => spdata(cc%species))

    ! Carbon and Nitrogen from plants to soil pools
    loss_coarse  = deadtrees * (cc%pwood%c%c12 + cc%psapw%c%c12 + cc%pleaf%c%c12 - cc%leafarea * LMAmin)
    loss_fine    = deadtrees * (cc%plabl%c%c12 + cc%pseed%c%c12 + cc%proot%c%c12 + cc%leafarea * LMAmin)

    lossN_coarse = deadtrees * (cc%pwood%n%n14 + cc%psapw%n%n14 + cc%pleaf%n%n14 - cc%leafarea*sp%LNbase)
    lossN_fine   = deadtrees * (cc%proot%n%n14 + cc%pseed%n%n14 + cc%plabl%n%n14 + cc%leafarea*sp%LNbase)

    vegn%psoil_fs%c%c12 = vegn%psoil_fs%c%c12 + fsc_fine * loss_fine + fsc_wood * loss_coarse
    vegn%psoil_sl%c%c12 = vegn%psoil_sl%c%c12 + (1.0 - fsc_fine) * loss_fine + (1.0-fsc_wood) * loss_coarse

    vegn%psoil_fs%n%n14 = vegn%psoil_fs%n%n14 + fsc_fine * lossN_fine + fsc_wood * lossN_coarse
    vegn%psoil_sl%n%n14 = vegn%psoil_sl%n%n14 + (1.0 - fsc_fine) * lossN_fine + (1.-fsc_wood) * lossN_coarse

    ! annual N from plants to soil
    vegn%N_P2S_yr = vegn%N_P2S_yr + lossN_fine + lossN_coarse

    ! record mortality
    ! cohort level
    cc%n_deadtrees = deadtrees
    cc%c_deadtrees = loss_coarse + loss_fine 
    ! cc%m_turnover  = cc%m_turnover + loss_coarse + loss_fine
    cc%m_turnover  = cc%m_turnover + deadtrees * (cc%pwood%c%c12 + cc%psapw%c%c12)

    ! cc%c_deadtrees   = deadtrees * (cc%plabl%c%c12 + cc%pseed%c%c12 + cc%pleaf%c%c12 + cc%proot%c%c12 + cc%psapw%c%c12 + cc%pwood%c%c12) 
    
    ! vegn%n_deadtrees   = vegn%n_deadtrees + deadtrees
    ! vegn%c_deadtrees   = vegn%c_deadtrees + deadtrees * (cc%plabl%c%c12 + cc%pseed%c%c12 + cc%pleaf%c%c12 + cc%proot%c%c12 + cc%psapw%c%c12 + cc%pwood%c%c12)
    
    ! ! print*, "vegn%n_deadtrees", vegn%n_deadtrees
    ! print*, "deadtrees", deadtrees
    ! print*, "cc%n_deadtrees", cc%n_deadtrees
    ! print*, "vegn%n_deadtrees", vegn%n_deadtrees
    ! print*, "vegn%n_deadtrees", vegn%n_deadtrees
    ! print*, "vegn%c_deadtrees", vegn%c_deadtrees

    end associate

  end subroutine plant2soil


  subroutine vegn_reproduction( vegn )
    !////////////////////////////////////////////////////////////////
    ! Reproduction of each canopy cohort, yearly time step
    ! calculate the new cohorts added in this step and states:
    ! tree density, DBH, woddy and fine biomass
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc ! parent and child cohort pointers
    type(cohort_type), dimension(:), pointer :: ccold, ccnew   ! pointer to old cohort array
    integer, dimension(16) :: reproPFTs
    real,    dimension(16) :: seedC, seedN ! seed pool of productible PFTs
    ! real :: failed_seeds, N_failedseed !, prob_g, prob_e
    integer :: newcohorts, matchflag, nPFTs ! number of new cohorts to be created
    integer :: nCohorts, istat
    integer :: i, k ! cohort indices

    ! Looping through all reproductable cohorts and Check if reproduction happens
    reproPFTs = -999 ! the code of reproductive PFT
    vegn%totseedC = 0.0
    vegn%totseedN = 0.0
    vegn%totNewCC = 0.0
    vegn%totNewCN = 0.0
    seedC = 0.0
    seedN = 0.0
    nPFTs = 0

    cohortloop1: do k=1, vegn%n_cohorts
      cc => vegn%cohorts(k)

      if (cohort_can_reproduce( cc )) then
        matchflag = 0
        do i=1,nPFTs
          if (cc%species == reproPFTs(i)) then

            seedC(i) = seedC(i) + cc%pseed%c%c12  * cc%nindivs
            seedN(i) = seedN(i) + cc%pseed%n%n14  * cc%nindivs

            ! reset parent's seed C and N
            vegn%totSeedC = vegn%totSeedC + cc%pseed%c%c12  * cc%nindivs
            vegn%totSeedN = vegn%totSeedN + cc%pseed%n%n14  * cc%nindivs
            cc%pseed%c%c12 = 0.0
            cc%pseed%n%n14 = 0.0
            matchflag = 1
            exit
          endif
        enddo

        if (matchflag==0) then ! when it is a new PFT, put it to the next place

          nPFTs            = nPFTs + 1 ! update the number of reproducible PFTs
          reproPFTs(nPFTs) = cc%species ! PFT number
          seedC(nPFTs)     = cc%pseed%c%c12 * cc%nindivs ! seed carbon
          seedN(nPFTs)     = cc%pseed%n%n14 * cc%nindivs ! seed nitrogen
          vegn%totSeedC = vegn%totSeedC + cc%pseed%c%c12  * cc%nindivs
          vegn%totSeedN = vegn%totSeedN + cc%pseed%n%n14  * cc%nindivs

          ! reset parent's seed C and N
          cc%pseed%c%c12 = 0.0
          cc%pseed%n%n14 = 0.0
        endif
      endif ! cohort_can_reproduce
    enddo cohortloop1

    ! Generate new cohorts
    newcohorts = nPFTs

    if (newcohorts >= 1) then   ! build new cohorts for seedlings
      
      ccold => vegn%cohorts ! keep old cohort information
      nCohorts = vegn%n_cohorts + newcohorts
      allocate(ccnew(1:nCohorts), STAT = istat)
      ccnew(1:vegn%n_cohorts) = ccold(1:vegn%n_cohorts) ! copy old cohort information
      vegn%cohorts => ccnew

      deallocate (ccold)

      ! set up new cohorts
      k = vegn%n_cohorts
      do i = 1, newcohorts
        
        k = k + 1 ! increment new cohort index
        cc => vegn%cohorts(k)
        
        ! Give the new cohort an ID
        cc%ccID = MaxCohortID + i
        
        ! update child cohort parameters
        associate (sp => spdata(reproPFTs(i)))
        
        ! density
        cc%nindivs = seedC(i)/sp%seedlingsize

        cc%species    = reproPFTs(i)
        cc%status     = LEAF_OFF
        cc%firstlayer = 0
        cc%topyear    = 0.0
        cc%age        = 0.0

        ! Carbon pools
        cc%pleaf%c%c12 = 0.0 * sp%seedlingsize
        cc%proot%c%c12 = 0.1 * sp%seedlingsize
        cc%psapw%c%c12 = f_initialBSW * sp%seedlingsize
        cc%pwood%c%c12 = 0.0 * sp%seedlingsize
        cc%pseed%c%c12 = 0.0
        cc%plabl%c%c12 = sp%seedlingsize - cc%psapw%c%c12 - cc%proot%c%c12
        
        ! beni: added seedling growth for NPP accounting (for output)
        cc%NPPleaf = cc%NPPleaf + cc%pleaf%c%c12
        cc%NPProot = cc%NPProot + cc%proot%c%c12
        cc%NPPwood = cc%NPPwood + cc%psapw%c%c12 + cc%pwood%c%c12

        call rootarea_and_verticalprofile( cc )

        ! Nitrogen pools
        cc%pleaf%n%n14  = cc%pleaf%c%c12/sp%CNleaf0
        cc%proot%n%n14  = cc%proot%c%c12/sp%CNroot0
        cc%psapw%n%n14  = cc%psapw%c%c12/sp%CNsw0
        cc%pwood%n%n14  = cc%pwood%c%c12/sp%CNwood0
        cc%pseed%n%n14  = 0.0

        if (cc%nindivs>0.0) then
          cc%plabl%n%n14 = sp%seedlingsize * seedN(i) / seedC(i) -  &
            (cc%pleaf%n%n14 + cc%proot%n%n14 + cc%psapw%n%n14 + cc%pwood%n%n14)
        end if 

        vegn%totNewCC = vegn%totNewCC + cc%nindivs*(cc%pleaf%c%c12 + cc%proot%c%c12 + &
          cc%psapw%c%c12 + cc%pwood%c%c12 + cc%plabl%c%c12)
        vegn%totNewCN = vegn%totNewCN + cc%nindivs*(cc%pleaf%n%n14 + cc%proot%n%n14 + &
          cc%psapw%n%n14 + cc%pwood%n%n14 + cc%plabl%n%n14)

        call init_cohort_allometry( cc )
        !        ! seeds fail
        !cc%nindivs = cc%nindivs * sp%prob_g * sp%prob_e
        !       put failed seeds to soil carbon pools
        !        failed_seeds = 0.0 ! (1. - sp%prob_g*sp%prob_e) * seedC(i)!!

        !        vegn%litter = vegn%litter + failed_seeds
        !        vegn%psoil_fs%c%c12 = vegn%psoil_fs%c%c12 +        fsc_fine *failed_seeds
        !        vegn%psoil_sl%c%c12 = vegn%psoil_sl%c%c12 + (1.0 - fsc_fine)*failed_seeds

        !      Nitrogen of seeds to soil SOMs
        !        N_failedseed= 0.0 ! (1.-sp%prob_g*sp%prob_e)   * seedN(i)
        !        vegn%psoil_fs%n%n14  = vegn%psoil_fs%n%n14   +        fsc_fine * N_failedseed
        !        vegn%psoil_sl%n%n14 = vegn%psoil_sl%n%n14  + (1.0 - fsc_fine)* N_failedseed

        !       annual N from plants to soil
        !   vegn%N_P2S_yr = vegn%N_P2S_yr + N_failedseed

        end associate   ! F2003
      enddo

      MaxCohortID = MaxCohortID + newcohorts
      vegn%n_cohorts = k
      ccnew => null()
      
      call zero_diagnostics( vegn )
    
    endif ! set up new born cohorts

  end subroutine vegn_reproduction


  function cohort_can_reproduce( cc ); logical cohort_can_reproduce
    !////////////////////////////////////////////////////////////////
    ! Determine whether a cohort can reproduce, based on criteria:
    ! - is in top canopy layer
    ! - is actually present
    ! - has reached reproductive maturity (age)
    ! - C and N in seed pool is sufficiently large to satisfy mass of a new seedling
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(cohort_type), intent(in) :: cc

    associate (sp => spdata(cc%species) )! F2003
      cohort_can_reproduce = (cc%layer == 1 .and. &
        cc%nindivs > 0.0 .and. &
        cc%age > sp%maturalage.and. &
        cc%pseed%c%c12 > sp%seedlingsize .and. &
        cc%pseed%n%n14 > sp%seedlingsize/sp%CNseed0)
    end associate

  end function


  subroutine vegn_species_switch(vegn, N_SP, iyears, FREQ)
    !////////////////////////////////////////////////////////////////
    ! switch the species of the first cohort to another species
    ! bugs !!!!!!
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn
    integer, intent(in):: N_SP  ! total species in model run settings
    integer, intent(in):: iyears
    integer, intent(in):: FREQ  ! frequency of species switching

    ! local variables --------
    real :: loss_fine,loss_coarse
    real :: lossN_fine,lossN_coarse
    ! integer :: i, k
    type(cohort_type), pointer :: cc

    cc => vegn%cohorts(1)
    associate (sp => spdata(cc%species))

    if (cc%pleaf%c%c12 > 0.0) then 
      ! remove all leaves to keep mass balance
      loss_coarse  = cc%nindivs * (cc%pleaf%c%c12 - cc%leafarea * LMAmin)
      loss_fine    = cc%nindivs * cc%leafarea * LMAmin
      lossN_coarse = cc%nindivs * (cc%pleaf%n%n14 - cc%leafarea * sp%LNbase)
      lossN_fine   = cc%nindivs * cc%leafarea * sp%LNbase

      ! Carbon to soil pools
      vegn%psoil_fs%c%c12  = vegn%psoil_fs%c%c12  + fsc_fine * loss_fine + &
        fsc_wood * loss_coarse
      vegn%psoil_sl%c%c12 = vegn%psoil_sl%c%c12 + (1.0 - fsc_fine) * loss_fine + &
        (1.0 - fsc_wood) * loss_coarse

      ! Nitrogen to soil pools
      vegn%psoil_fs%n%n14 = vegn%psoil_fs%n%n14 + fsc_fine  * lossN_fine +   &
        fsc_wood * lossN_coarse
      vegn%psoil_sl%n%n14 = vegn%psoil_sl%n%n14 +(1.0 - fsc_fine) * lossN_fine +   &
        (1.0 - fsc_wood) * lossN_coarse

      ! annual N from plants to soil
      vegn%N_P2S_yr = vegn%N_P2S_yr + lossN_fine + lossN_coarse

      ! remove leaves
      cc%pleaf%c%c12 = 0.0

      ! record continuous biomass turnover (not linked to mortality)
      ! cc%m_turnover = cc%m_turnover + loss_coarse + loss_fine XXX don't add anything if only considering wood

    endif
    end associate

    ! Change species
    cc%species = mod(iyears / FREQ, N_SP) + 2

  end subroutine vegn_species_switch


  subroutine relayer_cohorts( vegn )
    !////////////////////////////////////////////////////////////////
    ! Arrange crowns into canopy layers according to their height and 
    ! crown areas.
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn ! input cohorts

    ! ---- local constants
    real, parameter :: tolerance = 1e-4
    real, parameter :: layer_vegn_cover = 1.0   

    ! local variables
    integer :: idx(vegn%n_cohorts) ! indices of cohorts in decreasing height order
    integer :: i ! new cohort index
    integer :: k ! old cohort index
    integer :: L ! layer index (top-down)
    integer :: N0, N1 ! initial and final number of cohorts 
    real    :: frac ! fraction of the layer covered so far by the canopies
    type(cohort_type), pointer :: cc(:)
    type(cohort_type), pointer :: new(:)
    real    :: nindivs

    !  rand_sorting = .TRUE. ! .False.

    ! rank cohorts in descending order by height. For now, assume that they are 
    ! in order
    N0 = vegn%n_cohorts; cc=>vegn%cohorts
    call rank_descending(cc(1:N0)%height,idx)

    ! calculate max possible number of new cohorts : it is equal to the number of
    ! old cohorts, plus the number of layers -- since the number of full layers is 
    ! equal to the maximum number of times an input cohort can be split by a layer 
    ! boundary.
    
    ! replace NaN with 0
    where(cc(1:N0)%crownarea /= cc(1:N0)%crownarea)
      cc(1:N0)%crownarea = 0
    end where
    
    where(cc(1:N0)%nindivs /= cc(1:N0)%nindivs)
      cc(1:N0)%nindivs = 0
    end where

    ! calculate size of the new cohorts, correctly dealing with the NaN
    ! values - if one ignores the NaN values these are treated as a large
    ! negative int()
    N1 = vegn%n_cohorts + int(sum(cc(1:N0)%nindivs * cc(1:N0)%crownarea))
    
    ! allocate the new cohort array using the above size
    allocate(new(N1))

    ! copy cohort information to the new cohorts, splitting the old cohorts that 
    ! stride the layer boundaries
    i = 1 
    k = 1 
    L = 1 
    frac = 0.0 
    nindivs = cc(idx(k))%nindivs
    
    ! loop over all original cohorts
    do
      new(i) = cc(idx(k))
      new(i)%nindivs = min(nindivs, (layer_vegn_cover - frac)/cc(idx(k))%crownarea)
      new(i)%layer   = L

      if (L == 1) then
        new(i)%firstlayer = 1
      endif

      !    if (L>1)  new(i)%firstlayer = 0  ! switch off "push-down effects"
      frac = frac + new(i)%nindivs * new(i)%crownarea
      nindivs = nindivs - new(i)%nindivs
      
      ! check for individuals less than 0
      if (nindivs < 0) then
        nindivs = 0
      endif

      if ((nindivs*cc(idx(k))%crownarea) < tolerance) then

        ! allocate the remainder of individuals to the last cohort
        new(i)%nindivs = new(i)%nindivs + nindivs
        
        if (k == N0) then
          exit ! end of loop
        else
          k = k + 1
          nindivs = cc(idx(k))%nindivs
        endif
        
      endif

      if (abs(layer_vegn_cover - frac) < tolerance) then
        L = L + 1
        frac = 0.0   ! start new layer
      endif

      i = i + 1
    
    enddo

    ! replace the array of cohorts
    deallocate(vegn%cohorts)
    vegn%cohorts => new 
    vegn%n_cohorts = i

    ! update layer fraction for each cohort
    do i=1, vegn%n_cohorts
      vegn%cohorts(i)%layerfrac = vegn%cohorts(i)%nindivs * vegn%cohorts(i)%crownarea
    enddo

  end subroutine relayer_cohorts


  subroutine vegn_tissue_turnover( vegn )
    !////////////////////////////////////////////////////////////////
    ! Tissue turnover and transfer to litter pools
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc    ! current cohort
    real :: loss_coarse, loss_fine, lossN_coarse, lossN_fine
    real :: alpha_L   ! turnover rate of leaves
    real :: alpha_S   ! turnover rate of stems
    real :: dBL, dBR, dBStem  ! leaf and fine root carbon tendencies
    real :: dNL, dNR, dNStem  ! leaf and fine root nitrogen tendencies
    real :: dAleaf ! leaf area decrease due to dBL
    integer :: i

    ! update plant carbon and nitrogen for all cohorts
    do i = 1, vegn%n_cohorts
      cc => vegn%cohorts(i)
      associate ( sp => spdata(cc%species) )

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

      dBL    = cc%pleaf%c%c12 * alpha_L  / ndayyear
      dNL    = cc%pleaf%n%n14 * alpha_L  / ndayyear

      dBStem = cc%psapw%c%c12 * alpha_S  / ndayyear
      dNStem = cc%psapw%n%n14 * alpha_S  / ndayyear

      dBR    = cc%proot%c%c12 * sp%alpha_FR / ndayyear
      dNR    = cc%proot%n%n14 * sp%alpha_FR / ndayyear

      dAleaf = leaf_area_from_biomass(dBL, cc%species)

      ! Retranslocation to NSC and NSN
      cc%plabl%c%c12 = cc%plabl%c%c12 + l_fract  * (dBL + dBR + dBStem)
      cc%plabl%n%n14 = cc%plabl%n%n14 + retransN * (dNL + dNR + dNStem)

      ! update plant pools
      cc%pleaf%c%c12 = cc%pleaf%c%c12 - dBL
      cc%psapw%c%c12 = cc%psapw%c%c12 - dBStem
      cc%proot%c%c12 = cc%proot%c%c12 - dBR

      cc%pleaf%n%n14 = cc%pleaf%n%n14 - dNL
      cc%psapw%n%n14 = cc%psapw%n%n14 - dNStem
      cc%proot%n%n14 = cc%proot%n%n14 - dNR

      ! update leaf area and LAI
      cc%leafarea= leaf_area_from_biomass(cc%pleaf%c%c12, cc%species)
      cc%lai     = cc%leafarea / (cc%crownarea *(1.0-sp%internal_gap_frac))

      ! update NPP for leaves, fine roots, and wood
      cc%NPPleaf = cc%NPPleaf - l_fract * dBL
      cc%NPProot = cc%NPProot - l_fract * dBR
      cc%NPPwood = cc%NPPwood - l_fract * dBStem

      ! put C and N into soil pools
      loss_coarse  = (1.0 - l_fract) * cc%nindivs * (dBL - dAleaf * LMAmin + dBStem)
      loss_fine    = (1.0 - l_fract) * cc%nindivs * (dBR + dAleaf * LMAmin)
      lossN_coarse = (1.0 - retransN)* cc%nindivs * (dNL - dAleaf * sp%LNbase + dNStem)
      lossN_fine   = (1.0 - retransN)* cc%nindivs * (dNR + dAleaf * sp%LNbase)

      ! add to soil
      vegn%psoil_fs%c%c12 = vegn%psoil_fs%c%c12 +  &
        fsc_fine * loss_fine + fsc_wood * loss_coarse

      vegn%psoil_sl%c%c12 = vegn%psoil_sl%c%c12 +  &
        ((1.0 - fsc_fine)*loss_fine + (1.0 - fsc_wood) * loss_coarse)

      ! Nitrogen to soil SOMs
      vegn%psoil_fs%n%n14  = vegn%psoil_fs%n%n14 +    &
        fsc_fine * lossN_fine + fsc_wood * lossN_coarse

      vegn%psoil_sl%n%n14 = vegn%psoil_sl%n%n14 + &
        (1.0 - fsc_fine) * lossN_fine + (1.0 - fsc_wood) * lossN_coarse

      ! annual N from plants to soil
      vegn%N_P2S_yr = vegn%N_P2S_yr + lossN_fine + lossN_coarse

      ! record continuous biomass turnover (not linked to mortality)
      ! cc%m_turnover = cc%m_turnover + loss_coarse + loss_fine
      cc%m_turnover = cc%m_turnover + (1.0 - l_fract) * cc%nindivs * dBStem

      end associate
    enddo

  end subroutine vegn_tissue_turnover

  
  subroutine vegn_N_uptake(vegn, tsoil)
    !//////////////////////////////////////////////////////////////////////
    ! Mineral N uptake from the soil
    ! Code from BiomeE-Allocation
    !----------------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn
    real, intent(in) :: tsoil ! average temperature of soil, deg K

    ! local variables
    type(cohort_type),pointer :: cc

    real    :: rho_N_up0 = 0.1 ! 0.05 ! hourly N uptake rate, fraction of the total mineral N
    real    :: N_roots0  = 0.4  ! root biomass at half max N-uptake rate,kg C m-2

    real    :: totNup    ! kgN m-2
    real    :: avgNup
    real    :: rho_N_up, N_roots   ! actual N uptake rate
    ! logical :: NSN_not_full
    integer :: i

    ! xxx try
    vegn%ninorg%n14 = 0.2

    ! Nitrogen uptake parameter
    ! It considers competition here. How much N one can absorp depends on 
    ! how many roots it has and how many roots other individuals have.
    N_Roots  = 0.0
    vegn%N_uptake = 0.0

    if (vegn%ninorg%n14 > 0.0) then
    
      do i = 1, vegn%n_cohorts
        cc => vegn%cohorts(i)
        associate (sp => spdata(cc%species))

        cc%NSNmax = sp%fNSNmax*(cc%bl_max/(sp%CNleaf0*sp%leafLS)+cc%br_max/sp%CNroot0) !5.0 * (cc%bl_max/sp%CNleaf0 + cc%br_max/sp%CNroot0)) !
        if (cc%plabl%n%n14 < cc%NSNmax) N_Roots = N_Roots + cc%proot%c%c12 * cc%nindivs

        end associate
      enddo
      
      ! M-M equation for Nitrogen absoption, McMurtrie et al. 2012, Ecology & Evolution
      ! rate at given root biomass and period of time
      if (N_roots>0.0) then

        ! Add a temperature response equation herefor rho_N_up0 (Zhu Qing 2016)
        rho_N_up = rho_N_up0 * N_roots / (N_roots0 + N_roots) * hours_per_year * myinterface%dt_fast_yr        

        totNup = rho_N_up * vegn%ninorg%n14 * exp(9000.0 * (1./298.16 - 1./tsoil)) ! kgN m-2 time step-1

        ! Below code is from BiomeE-Allocation
        avgNup = totNup / N_roots ! kgN time step-1 kg roots-1
        
        ! Nitrogen uptaken by each cohort (N_uptake) - proportional to cohort's root mass
        vegn%N_uptake = 0.0
        do i = 1, vegn%n_cohorts
          cc => vegn%cohorts(i)
          cc%N_uptake  = 0.0
          if (cc%plabl%n%n14 < cc%NSNmax) then

            cc%N_uptake    = cc%proot%c%c12 * avgNup ! min(cc%proot%c%c12*avgNup, cc%NSNmax-cc%plabl%n%n14)
            cc%plabl%n%n14 = cc%plabl%n%n14 + cc%N_uptake
            cc%annualNup   = cc%annualNup + cc%N_uptake !/cc%crownarea

            ! subtract N from mineral N
            vegn%ninorg%n14 = vegn%ninorg%n14 - cc%N_uptake * cc%nindivs
            vegn%N_uptake   = vegn%N_uptake + cc%N_uptake * cc%nindivs
          endif
        enddo
        cc =>null()

      endif ! N_roots>0
    endif
  
  end subroutine vegn_N_uptake


  subroutine SOMdecomposition(vegn, tsoil, thetaS)
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
    real                , intent(in)    :: tsoil ! soil temperature, deg K 
    real                , intent(in)    :: thetaS
    real :: CUE0=0.4  ! default microbial CUE
    real :: phoMicrobial = 2.5 ! turnover rate of microbes (yr-1)
    real :: CUEfast,CUEslow
    real :: CNm = 10.0  ! Microbial C/N ratio
    real :: NforM, fNM=0.0  ! mineral N available for microbes
    real :: micr_C_loss, fast_L_loss, slow_L_loss
    real :: runoff ! kg m-2 /step
    real :: N_loss
    real :: DON_fast,DON_slow,DON_loss ! Dissolved organic N loss, kg N m-2 step-1
    real :: fDON=0.0   ! 0.02     ! fractio of DON production in decomposition
    real :: fast_N_free 
    real :: slow_N_free 
    real :: CNfast, CNslow
    real :: A  ! decomp rate reduction due to moisture and temperature    

    runoff = vegn%runoff  !* myinterface%dt_fast_yr !kgH2O m-2 yr-1 ->kgH2O m-2/time step, weng 2017-10-15
  
    ! CN ratios of soil C pools
    CNfast = vegn%psoil_fs%c%c12 / vegn%psoil_fs%n%n14
    CNslow = vegn%psoil_sl%c%c12 / vegn%psoil_sl%n%n14

    ! C decomposition
    A = A_function(tsoil, thetaS)
    micr_C_loss = vegn%pmicr%c%c12    * (1.0 - exp(-A*phoMicrobial* myinterface%dt_fast_yr))
    fast_L_loss = vegn%psoil_fs%c%c12 * (1.0 - exp(-A*K1          * myinterface%dt_fast_yr))
    slow_L_loss = vegn%psoil_sl%c%c12 * (1.0 - exp(-A*K2          * myinterface%dt_fast_yr))

    ! Carbon use efficiencies of microbes
    NforM = fNM * vegn%ninorg%n14

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
    vegn%pmicr%c%c12  = vegn%pmicr%c%c12 - micr_C_loss &
                      + fast_L_loss * CUEfast &
                      + slow_L_loss * CUEslow
    vegn%psoil_fs%c%c12 = vegn%psoil_fs%c%c12 - fast_L_loss
    vegn%psoil_sl%c%c12 = vegn%psoil_sl%c%c12 - slow_L_loss

    fDON        = 0.25 ! 0.25 ! * myinterface%dt_fast_yr ! 0.05 !* myinterface%dt_fast_yr
    runoff      = 0.2 ! 0.2 ! mm day-1
    
    ! Assume it is proportional to decomposition rates
    ! Find some papers!!
    DON_fast    = fDON * fast_L_loss/CNfast * (etaN*runoff)
    DON_slow    = fDON * slow_L_loss/CNslow * (etaN*runoff)
    DON_loss    = DON_fast + DON_slow

    ! Update Nitrogen pools
    vegn%pmicr%n%n14= vegn%pmicr%c%c12/CNm
    vegn%psoil_fs%n%n14  = vegn%psoil_fs%n%n14  - fast_L_loss/CNfast - DON_fast
    vegn%psoil_sl%n%n14 = vegn%psoil_sl%n%n14 - slow_L_loss/CNslow - DON_slow
    
    ! Mixing of microbes to litters
    vegn%psoil_fs%c%c12 = vegn%psoil_fs%c%c12 + MLmixRatio*fast_L_loss * CUEfast
    vegn%psoil_fs%n%n14 = vegn%psoil_fs%n%n14 + MLmixRatio*fast_L_loss * CUEfast/CNm
    vegn%psoil_sl%c%c12 = vegn%psoil_sl%c%c12 + MLmixRatio*slow_L_loss * CUEslow
    vegn%psoil_sl%n%n14 = vegn%psoil_sl%n%n14 + MLmixRatio*slow_L_loss * CUEslow/CNm
    vegn%pmicr%c%c12 = vegn%pmicr%c%c12  - MLmixRatio*(fast_L_loss*CUEfast+slow_L_loss*CUEslow)
    vegn%pmicr%n%n14  = vegn%pmicr%c%c12/CNm
      
    ! update mineral N pool (mineralN)
    fast_N_free = MAX(0.0, fast_L_loss*(1./CNfast - CUEfast/CNm))
    slow_N_free = MAX(0.0, slow_L_loss*(1./CNslow - CUEslow/CNm))


    N_loss = vegn%ninorg%n14 * MIN(0.25, (A * K_nitrogen * myinterface%dt_fast_yr + etaN*runoff))

    vegn%Nloss_yr = vegn%Nloss_yr + N_loss + DON_loss

    vegn%ninorg%n14 = vegn%ninorg%n14 - N_loss     &
                    + vegn%N_input * myinterface%dt_fast_yr  &
                    + fast_N_free + slow_N_free  &
                    + micr_C_loss/CNm

    vegn%annualN   = vegn%annualN - N_loss     &
                    + vegn%N_input * myinterface%dt_fast_yr  &
                    + fast_N_free + slow_N_free  &
                    + micr_C_loss/CNm

    ! Check if soil C/N is above CN0
    fast_N_free = MAX(0.0, vegn%psoil_fs%n%n14  - vegn%psoil_fs%c%c12/CN0metabolicL)
    slow_N_free = MAX(0.0, vegn%psoil_sl%n%n14 - vegn%psoil_sl%c%c12/CN0structuralL)

    vegn%psoil_fs%n%n14 = vegn%psoil_fs%n%n14 - fast_N_free
    vegn%psoil_sl%n%n14 = vegn%psoil_sl%n%n14 - slow_N_free
    vegn%ninorg%n14    = vegn%ninorg%n14 + fast_N_free + slow_N_free
    vegn%annualN     = vegn%annualN  + fast_N_free + slow_N_free
    
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

  subroutine rank_descending(x, idx)
    !////////////////////////////////////////////////////////////////
    ! Ranks array x in descending order: on return, idx() contains indices
    ! of elements of array x in descending order of x values. These codes
    ! are from Sergey Malyshev (biomee, Weng et al. 2015 Biogeosciences)
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    real,    intent(in)  :: x(:)
    integer, intent(out) :: idx(:)
    integer :: i,n
    integer, allocatable :: t(:)
    n = size(x)
    do i = 1,n
      idx(i) = i
    enddo
    allocate(t((n+1)/2))
    call mergerank(x, idx, n, t)
    deallocate(t)
  end subroutine rank_descending


  subroutine merge(x, a, na, b, nb, c, nc)
    !////////////////////////////////////////////////////////////////
    ! based on:
    ! http://rosettacode.org/wiki/Sorting_algorithms/Merge_sort#Fortran
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    integer, intent(in) :: na,nb,nc ! Normal usage: NA+NB = NC
    real, intent(in)       :: x(*)
    integer, intent(in)    :: a(na)    ! B overlays C(NA+1:NC)
    integer, intent(in)    :: b(nb)
    integer, intent(inout) :: c(nc)
    integer :: i, j, k
    i = 1; j = 1; k = 1;
    do while(i <= na .and. j <= nb)
      if (x(a(i)) >= x(b(j))) then
        c(k) = a(i) ; i = i+1
      else
        c(k) = b(j) ; j = j+1
      endif
      k = k + 1
    enddo
    do while (i <= na)
      c(k) = a(i) ; i = i + 1 ; k = k + 1
    enddo
  
  end subroutine merge


  recursive subroutine mergerank(x, a, n, t)
    !////////////////////////////////////////////////////////////////
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    integer, intent(in) :: n
    real,    intent(in) :: x(*)
    integer, dimension(n), intent(inout) :: a
    integer, dimension((n+1)/2), intent (out) :: t
    integer :: na,nb
    integer :: v
    if (n < 2) return
    if (n == 2) then
      if ( x(a(1)) < x(a(2)) ) then
        v = a(1) ; a(1) = a(2) ; a(2) = v
      endif
      return
    endif  
    na=(n+1)/2
    nb=n-na
    call mergerank(x,a,na,t)
    call mergerank(x,a(na+1),nb,t)
    if (x(a(na)) < x(a(na+1))) then
      t(1:na) = a(1:na)
      call merge(x,t,na,a(na+1),nb,a,n)
    endif
  end subroutine mergerank


  subroutine vegn_mergecohorts( vegn )
    !////////////////////////////////////////////////////////////////
    ! Merge similar cohorts in a tile
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc(:) ! array to hold new cohorts
    logical :: merged(vegn%n_cohorts)        ! mask to skip cohorts that were already merged
    real, parameter :: mindensity = 1.0E-6
    integer :: i,j,k
    allocate(cc(vegn%n_cohorts))
    merged(:)=.FALSE. ; k = 0
    do i = 1, vegn%n_cohorts 
      if (merged(i)) cycle ! skip cohorts that were already merged
      k = k+1
      cc(k) = vegn%cohorts(i)
      ! try merging the rest of the cohorts into current one
      do j = i+1, vegn%n_cohorts
        if (merged(j)) cycle ! skip cohorts that are already merged
        if (cohorts_can_be_merged(vegn%cohorts(j),cc(k))) then
          call merge_cohorts(vegn%cohorts(j),cc(k))
          merged(j) = .TRUE.
        endif
      enddo
    enddo

    ! at this point, k is the number of new cohorts
    vegn%n_cohorts = k
    deallocate(vegn%cohorts)
    vegn%cohorts=>cc

  end subroutine vegn_mergecohorts


  subroutine kill_lowdensity_cohorts( vegn )
    !////////////////////////////////////////////////////////////////
    ! Remove cohorts that have (almost) fully died and update tile
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn
    ! local variables
    type(cohort_type), pointer :: cx, cc(:) ! array to hold new cohorts
    ! logical :: merged(vegn%n_cohorts)        ! mask to skip cohorts that were already merged
    real, parameter :: mindensity = 0.25E-4
    integer :: i,k

    ! calculate the number of cohorts with indivs>mindensity
    k = 0
    do i = 1, vegn%n_cohorts
      if (vegn%cohorts(i)%nindivs > mindensity) k = k + 1
    enddo

    ! https://github.com/geco-bern/rsofun/issues/24
    !if (k==0) then 
    !  print *, "cohort too small..."
    !  stop
    !endif
    
    ! exclude cohorts that have low individuals
    if (k > 0 .and. k < vegn%n_cohorts) then
      allocate(cc(k))
      k = 0
      do i = 1,vegn%n_cohorts
        cx => vegn%cohorts(i)
        associate(sp => spdata(cx%species))
        if (cx%nindivs > mindensity) then
          k = k + 1
          cc(k) = cx
        else
          ! Carbon and Nitrogen from plants to soil pools
          call plant2soil(vegn, cx, cx%nindivs)
        endif
        end associate
      enddo
      vegn%n_cohorts = k
      deallocate (vegn%cohorts)
      vegn%cohorts => cc
    endif
  end subroutine kill_lowdensity_cohorts


  subroutine merge_cohorts(c1, c2)
    !////////////////////////////////////////////////////////////////
    ! kill low density cohorts, a new function seperated from vegn_mergecohorts
    ! Weng, 2014-07-22
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(cohort_type), intent(in) :: c1
    type(cohort_type), intent(inout) :: c2
    real :: x1, x2 ! normalized relative weights
    if (c1%nindivs > 0.0 .or. c2%nindivs > 0.0) then
      
      x1 = c1%nindivs/(c1%nindivs+c2%nindivs)
      x2 = c2%nindivs/(c1%nindivs+c2%nindivs)
      !else
      !   x1 = 0.5
      !   x2 = 0.5
      !endif

      ! update number of individuals in merged cohort
      c2%nindivs = c1%nindivs + c2%nindivs

      ! Carbon
      c2%pleaf%c%c12 = x1*c1%pleaf%c%c12 + x2*c2%pleaf%c%c12
      c2%proot%c%c12 = x1*c1%proot%c%c12 + x2*c2%proot%c%c12
      c2%psapw%c%c12 = x1*c1%psapw%c%c12 + x2*c2%psapw%c%c12
      c2%pwood%c%c12 = x1*c1%pwood%c%c12 + x2*c2%pwood%c%c12
      c2%pseed%c%c12 = x1*c1%pseed%c%c12 + x2*c2%pseed%c%c12
      c2%plabl%c%c12 = x1*c1%plabl%c%c12 + x2*c2%plabl%c%c12

      ! Allometry
      c2%dbh = x1*c1%dbh + x2*c2%dbh
      c2%height = x1*c1%height + x2*c2%height
      c2%crownarea = x1*c1%crownarea + x2*c2%crownarea
      c2%age = x1*c1%age + x2*c2%age
      c2%C_growth = x1*c1%C_growth + x2*c2%C_growth
      c2%topyear = x1*c1%topyear + x2*c2%topyear

      ! Nitrogen
      c2%pleaf%n%n14 = x1*c1%pleaf%n%n14 + x2*c2%pleaf%n%n14
      c2%proot%n%n14 = x1*c1%proot%n%n14 + x2*c2%proot%n%n14
      c2%psapw%n%n14 = x1*c1%psapw%n%n14 + x2*c2%psapw%n%n14
      c2%pwood%n%n14 = x1*c1%pwood%n%n14 + x2*c2%pwood%n%n14
      c2%pseed%n%n14 = x1*c1%pseed%n%n14 + x2*c2%pseed%n%n14
      c2%plabl%n%n14 = x1*c1%plabl%n%n14 + x2*c2%plabl%n%n14
      
      !  calculate the resulting dry heat capacity
      c2%leafarea = leaf_area_from_biomass(c2%pleaf%c%c12, c2%species)
      call init_cohort_allometry(c2) !Enseng comments
    endif

  end subroutine merge_cohorts


  function cohorts_can_be_merged(c1, c2); logical cohorts_can_be_merged
    !////////////////////////////////////////////////////////////////
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(cohort_type), intent(in) :: c1,c2
    real, parameter :: mindensity = 1.0E-4
    logical :: sameSpecies, sameLayer, sameSize, sameSizeTree, sameSizeGrass, lowDensity
    sameSpecies  = c1%species == c2%species
    
    sameLayer    = (c1%layer == c2%layer) .or. & ! .and. (c1%firstlayer == c2%firstlayer)
      ((spdata(c1%species)%lifeform ==0) .and. &
       (spdata(c2%species)%lifeform ==0) .and. &
       (c1%layer>1 .and.c2%layer>1))
    
    sameSizeTree = (spdata(c1%species)%lifeform > 0).and.  &
      (spdata(c2%species)%lifeform > 0).and.  &
      ((abs(c1%DBH - c2%DBH)/c2%DBH < 0.2 ) .or.  &
      (abs(c1%DBH - c2%DBH)        < 0.001))  ! it'll be always true for grasses
    
    sameSizeGrass= (spdata(c1%species)%lifeform ==0) .and. &
      (spdata(c2%species)%lifeform ==0) .and. &
      ((c1%DBH == c2%DBH).and.c1%age> 2. .and. c2%age>2.)  ! it'll be always true for grasses
    
    sameSize = sameSizeTree .OR. sameSizeGrass
    lowDensity  = .FALSE. ! c1%nindivs < mindensity 
    
    ! Weng, 2014-01-27, turned off
    cohorts_can_be_merged = sameSpecies .and. sameLayer .and. sameSize
  end function


  subroutine initialize_cohort_from_biomass(cc, btot)
    !////////////////////////////////////////////////////////////////
    ! calculate tree height, DBH, height, and crown area by initial biomass
    ! The allometry equations are from Ray Dybzinski et al. 2011 and Forrior et al. in review
    !         HT = alphaHT * DBH ** (gamma-1)   ! DBH --> Height
    !         CA = alphaCA * DBH ** gamma       ! DBH --> Crown Area
    !         BM = alphaBM * DBH ** (gamma + 1) ! DBH --> tree biomass
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(cohort_type), intent(inout) :: cc
    real, intent(in) :: btot ! total biomass per individual, kg C
    associate(sp=>spdata(cc%species))
    
    cc%DBH        = (btot / sp%alphaBM) ** ( 1.0/sp%thetaBM )
    cc%height     = sp%alphaHT * cc%dbh ** sp%thetaHT
    cc%crownarea  = sp%alphaCA * cc%dbh ** sp%thetaCA
    cc%bl_max     = sp%LMA   * sp%LAImax        * cc%crownarea/max(1,cc%layer)
    cc%br_max     = sp%phiRL * sp%LAImax/sp%SRA * cc%crownarea/max(1,cc%layer)
    cc%NSNmax     = sp%fNSNmax*(cc%bl_max/(sp%CNleaf0*sp%leafLS)+cc%br_max/sp%CNroot0)
    cc%plabl%c%c12        = 2.0 * (cc%bl_max + cc%br_max)
    
    call rootarea_and_verticalprofile( cc )
    
    ! N pools
    cc%plabl%n%n14    = 5.0 * (cc%bl_max / sp%CNleaf0 + cc%br_max / sp%CNroot0)
    cc%pleaf%n%n14  = cc%pleaf%c%c12 / sp%CNleaf0
    cc%proot%n%n14  = cc%proot%c%c12 / sp%CNroot0
    cc%psapw%n%n14  = cc%psapw%c%c12 / sp%CNsw0
    cc%pwood%n%n14  = cc%pwood%c%c12 / sp%CNwood0
    end associate
  
  end subroutine initialize_cohort_from_biomass


  ! subroutine annual_calls( vegn )
  !   !////////////////////////////////////////////////////////////////
  !   ! Code from BiomeE-Allocation
  !   !---------------------------------------------------------------
  !   type(vegn_tile_type), intent(inout) :: vegn
  !   !---- annual call -------------
  !   ! update the LAImax of each PFT according to available N for next year
  !   if (update_annualLAImax) call vegn_annualLAImax_update( vegn )
  !   ! Reproduction and mortality
  !   !call vegn_starvation( vegn )  ! called daily
  !   !call vegn_annual_starvation( vegn )
  !   call vegn_reproduction( vegn )
  !   call vegn_nat_mortality(vegn, real(seconds_per_year))
  !   ! Re-organize cohorts
  !   call relayer_cohorts( vegn )
  !   call kill_lowdensity_cohorts( vegn )
  !   call vegn_mergecohorts( vegn )
  !   ! set annual variables zero
  !   call Zero_diagnostics( vegn )
  ! end subroutine annual_calls


  subroutine init_cohort_allometry( cc )
    !////////////////////////////////////////////////////////////////
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(cohort_type), intent(inout) :: cc
    ! ----- local var -----------
    integer :: layer
    real    :: btot ! total biomass per individual, kg C

    associate(sp=>spdata(cc%species))
      !if (sp%lifeform>0) then
      btot = max(0.0001, cc%pwood%c%c12 + cc%psapw%c%c12)
      layer = max(1, cc%layer)

      cc%DBH        = (btot / sp%alphaBM) ** ( 1.0/sp%thetaBM )
      cc%height     = sp%alphaHT * cc%dbh ** sp%thetaHT
      cc%crownarea  = sp%alphaCA * cc%dbh ** sp%thetaCA

      ! calculations of bl_max and br_max are here only for the sake of the
      ! diagnostics, because otherwise those fields are inherited from the 
      ! parent cohort and produce spike in the output, even though these spurious
      ! values are not used by the model
      cc%bl_max = sp%LMA   * sp%LAImax        * cc%crownarea/layer
      cc%br_max = sp%phiRL * sp%LAImax/sp%SRA * cc%crownarea/layer
      cc%NSNmax = sp%fNSNmax * (cc%bl_max / (sp%CNleaf0 * sp%leafLS) + cc%br_max / sp%CNroot0)
    end associate
  
  end subroutine init_cohort_allometry


  subroutine vegn_annualLAImax_update( vegn )
    !////////////////////////////////////////////////////////////////
    ! used for updating LAImax according to mineral N in soil
    ! Potential problems:
    !   1. All species LAImax are updated
    !   2. For evergreen, LAImax can be less than current LAI.
    !  Weng, 2017-08-02
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn
    ! local variables
    ! type(cohort_type), pointer :: cc
    real   :: LAImin, LAIfixedN, LAImineralN
    real   :: LAI_Nitrogen
    ! real   :: fixedN, rootN
    logical:: fixedN_based
    integer :: i
    ! Calculating LAI max based on mineral N or mineralN + fixed N
    fixedN_based =  .False. ! .True. !
    LAImin       = 0.5

    !fixedN = 0.0
    !do i = 1,vegn%n_cohorts
    !      cc => vegn%cohorts(i)
    !      fixedN = fixedN + cc%annualfixedN * cc%crownarea * cc%nindivs
    !enddo
    ! Mineral+fixed N-based LAImax
    ! LAI_fixedN = sp%Nfixrate0 * sp%LMA * sp%CNleaf0 * sp%leafLS / sp%LMA
    ! cc%br_max = sp%phiRL*cc%bl_max/(sp%LMA*sp%SRA)

    vegn%previousN = 0.8 * vegn%previousN + 0.2 * vegn%annualN
    do i=0,MSPECIES
      associate (sp => spdata(i) )
        LAIfixedN  = 0.5 * sp%Nfixrate0 * sp%CNleaf0 * sp%leafLS
        LAImineralN = 0.5*vegn%previousN*sp%CNleaf0*sp%leafLS/sp%LMA

        !LAImineralN = vegn%previousN/(sp%LMA/(sp%CNleaf0*sp%leafLS)+sp%phiRL*sp%alpha_FR/sp%SRA /sp%CNroot0)
        LAI_nitrogen = LAIfixedN + LAImineralN
        ! spdata(i)%LAImax = MAX(LAImin, MIN(LAI_nitrogen, sp%LAI_light))

        ! turn off N limitation
        spdata(i)%LAImax = MAX(LAImin, sp%LAI_light)

        spdata(i)%underLAImax = MIN(sp%LAImax, 1.2)

        ! print*,'sp%LAI_light', sp%LAI_light

      end associate
    enddo

    !  ! update the PFTs in the first layer based on fixed N
    !  if (fixedN_based) then ! based on "cc%annualfixedN + vegn%previousN"
    !!    Reset sp%LAImax
    !     do i = 1,vegn%n_cohorts
    !        cc => vegn%cohorts(i)
    !        associate (sp => spdata(cc%species) )
    !        sp%LAImax    = 0.0  ! max(sp%LAImax,ccLAImax)
    !        sp%layerfrac = 0.0
    !        sp%n_cc      = 0
    !        end associate
    !     enddo
    !!   Sum ccLAImax in the first layer
    !     do i = 1,vegn%n_cohorts
    !        cc => vegn%cohorts(i)
    !        associate ( sp => spdata(cc%species) )
    !        if (sp%LAImax < LAImin) then
    !           LAI_nitrogen = 0.5*(vegn%previousN+cc%annualfixedN)*sp%CNleaf0*sp%leafLS/sp%LMA
    !           if (sp%Nfixrate0 > 0.0)
    !           sp%LAImax    = MAX(LAImin, MIN(LAI_nitrogen,sp%LAI_light))
    !        endif
    !        end associate
    !     enddo
    !  endif
  
  end subroutine vegn_annualLAImax_update


  function leaf_area_from_biomass(bl,species) result (area)
    !////////////////////////////////////////////////////////////////
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    real :: area ! returned value
    real,    intent(in) :: bl      ! biomass of leaves, kg C/individual
    integer, intent(in) :: species ! species
    ! integer, intent(in) :: layer, firstlayer
    ! modified by Weng (2014-01-09), 07-18-2017
    area = bl/spdata(species)%LMA
    !if (layer > 1.AND. firstlayer == 0) then
    !   area = bl/(0.5*spdata(species)%LMA) ! half thickness for leaves in understory
    !else
    !   area = bl/spdata(species)%LMA
    !endif
  
  end function
  
  !=======================================================================
  !==================== Vegetation initializations =======================
  !=======================================================================

  subroutine initialize_vegn_tile( vegn, nCohorts )
    !////////////////////////////////////////////////////////////////
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    type(vegn_tile_type), intent(inout), pointer :: vegn
    integer, intent(in) :: nCohorts
    ! -local vars -------
    type(cohort_type), dimension(:), pointer :: cc
    type(cohort_type), pointer :: cx
    integer,parameter :: rand_seed = 86456
    real    :: r
    real    :: btotal
    integer :: i, istat
    ! integer :: io           ! i/o status for the namelist
    ! integer :: ierr         ! error code, returned by i/o routines
    ! integer :: nml_unit

    ! Take tile parameters from myinterface (they are read from the namelist file in initialize_PFT() otherwise)
    soiltype    = myinterface%params_tile%soiltype 
    FLDCAP      = myinterface%params_tile%FLDCAP 
    WILTPT      = myinterface%params_tile%WILTPT 
    K1          = myinterface%params_tile%K1  
    K2          = myinterface%params_tile%K2
    K_nitrogen  = myinterface%params_tile%K_nitrogen  
    MLmixRatio  = myinterface%params_tile%MLmixRatio    
    etaN        = myinterface%params_tile%etaN  
    LMAmin      = myinterface%params_tile%LMAmin   
    fsc_fine    = myinterface%params_tile%fsc_fine                
    fsc_wood    = myinterface%params_tile%fsc_wood                
    GR_factor   = myinterface%params_tile%GR_factor                
    l_fract     = myinterface%params_tile%l_fract      
    retransN    = myinterface%params_tile%retransN     
    f_initialBSW= myinterface%params_tile%f_initialBSW 
    f_N_add     = myinterface%params_tile%f_N_add  
    tf_base     = myinterface%params_tile%tf_base  !calibratable
    par_mort    = myinterface%params_tile%par_mort  !calibratable
    par_mort_under  = myinterface%params_tile%par_mort_under  !calibratable

    !  Read parameters from the parameter file (namelist)
    if (read_from_parameter_file) then

      ! Initialize plant cohorts
      init_n_cohorts = nCohorts ! Weng,2018-11-21
      allocate(cc(1:init_n_cohorts), STAT = istat)
      vegn%cohorts => cc
      vegn%n_cohorts = init_n_cohorts
      cc => null()

      do i=1,init_n_cohorts
        cx => vegn%cohorts(i)
        cx%status  = LEAF_OFF ! ON=1, OFF=0 ! ON
        cx%layer   = 1
        cx%species = INT(myinterface%init_cohort(i)%init_cohort_species)
        cx%ccID    =  i
        cx%plabl%c%c12     = myinterface%init_cohort(i)%init_cohort_nsc
        cx%nindivs = myinterface%init_cohort(i)%init_cohort_nindivs ! trees/m2
        cx%psapw%c%c12     = myinterface%init_cohort(i)%init_cohort_bsw
        cx%pwood%c%c12     = myinterface%init_cohort(i)%init_cohort_bHW
        btotal     = cx%psapw%c%c12 + cx%pwood%c%c12  ! kgC /tree
        call initialize_cohort_from_biomass(cx,btotal)
      enddo
      MaxCohortID = cx%ccID

      ! Sorting these cohorts
      call relayer_cohorts( vegn )

      ! Initial Soil pools and environmental conditions
      vegn%psoil_fs%c%c12   = myinterface%init_soil%init_fast_soil_C ! kgC m-2
      vegn%psoil_sl%c%c12  = myinterface%init_soil%init_slow_soil_C ! slow soil carbon pool, (kg C/m2)
      vegn%psoil_fs%n%n14   = vegn%psoil_fs%c%c12 / CN0metabolicL  ! fast soil nitrogen pool, (kg N/m2)
      vegn%psoil_sl%n%n14  = vegn%psoil_sl%c%c12 / CN0structuralL  ! slow soil nitrogen pool, (kg N/m2)
      vegn%N_input      = myinterface%init_soil%N_input   ! kgN m-2 yr-1, N input to soil
      vegn%ninorg%n14     = myinterface%init_soil%init_Nmineral  ! Mineral nitrogen pool, (kg N/m2)
      vegn%previousN    = vegn%ninorg%n14

      ! Soil water parameters
      vegn%soiltype = myinterface%params_tile%soiltype    
      vegn%FLDCAP = myinterface%params_tile%FLDCAP  
      vegn%WILTPT = myinterface%params_tile%WILTPT  

      ! Initialize soil volumetric water conent with field capacity (maximum soil moisture to start with)
      vegn%wcl = myinterface%params_tile%FLDCAP  
      ! Update soil water
      vegn%SoilWater = 0.0
      do i=1, max_lev
        vegn%SoilWater = vegn%SoilWater + vegn%wcl(i)*thksl(i)*1000.0
      enddo
      vegn%thetaS = 1.0
      ! tile
      call summarize_tile( vegn )
      vegn%initialN0 =  vegn%plabl%n%n14 + vegn%pseed%n%n14 + vegn%pleaf%n%n14 +      &
                        vegn%proot%n%n14 + vegn%psapw%n%n14 + vegn%pwood%n%n14 + &
                        vegn%pmicr%n%n14 + vegn%psoil_fs%n%n14 +       &
                        vegn%psoil_sl%n%n14 + vegn%ninorg%n14
      vegn%totN =  vegn%initialN0

    endif  ! initialization: random or pre-described
  
  end subroutine initialize_vegn_tile

end module md_vegetation_biomee
