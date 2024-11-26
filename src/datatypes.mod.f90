module datatypes
  !////////////////////////////////////////////////////////////////
  ! Module containing BiomeE state variable and parameter 
  ! definitions.
  ! Code adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !----------------------------------------------------------------
  use md_interface_biomee, only: myinterface, spec_data_type, MAX_LEVELS
  use md_params_core
  use md_classdefs

  ! define data types and constants
  implicit none
  !=============== Public types ===========================================================
  public :: spec_data_type, cohort_type, vegn_tile_type

  !=============== Public subroutines =====================================================
  public :: Zero_diagnostics, hourly_diagnostics, daily_diagnostics, &
            annual_diagnostics

  !=============== Public parameters =======================================================
  public :: MaxCohortID, K1, K2, K_nitrogen, etaN, MLmixRatio, &
            fsc_fine, fsc_wood, LMAmin, GR_factor, tf_base, par_mort, par_mort_under, l_fract, &
            retransN, f_initialBSW,f_N_add, A_mort, B_mort,DBHtp

  !=============== Constants =============================================================
  logical, public, parameter :: read_from_parameter_file = .TRUE.
  integer, public, parameter :: max_lev                  = MAX_LEVELS            ! Soil layers, for soil water dynamics
  integer, public, parameter :: num_l                    = MAX_LEVELS            ! Soil layers
  integer, public, parameter :: LEAF_ON                  = 1
  integer, public, parameter :: LEAF_OFF                 = 0
  integer, public, parameter :: PT_C3                    = 0                     ! physiology types
  integer, public, parameter :: PT_C4                    = 1                     ! physiology types

  !===== Soil water hydrualics
  real, public, parameter :: rzone                       = 2.0                   ! m
  real, public, parameter ::  thksl(max_lev)              = (/0.05, 0.45, 1.5/)   ! m, thickness of soil layers
  real, public, parameter :: psi_wilt                    = -150.0                ! matric head at wilting
  real, public, parameter :: K_rel_min                   = 1.e-12
  real, public, parameter :: rate_fc                     = 0.1/86400             ! 0.1 mm/d drainage rate at FC
  real, public, parameter :: ws0                         = 0.02                  ! hygroscopic point
  real, public, parameter :: Edepth                      = 0.05                  ! m, the depth of soil for surface evaporation
  integer, public, parameter :: & ! soil types
                    Sand        = 1, &
                    LoamySand   = 2, &
                    SandyLoam   = 3, &
                    SiltLoam    = 4, &
                    FrittedClay = 5, &
                    Loam        = 6, &
                    Clay        = 7
  integer, public, parameter :: PHEN_DECIDIOUS           = 0                     ! phenology type
  integer, public, parameter :: PHEN_EVERGREEN           = 1                     ! phenology type

  !===== Soil SOM reference C/N ratios
  real, public, parameter :: CN0metabolicL               = 15.0 
  real, public, parameter :: CN0structuralL              = 40.0

  !===== Hydrolics
  real, public, parameter :: psi_leaf                    = -2.31 *1.0e6 ! pa, Katul et al. 2003, for clay soil

  !=============== Cohort level data type =============================================================
  type :: cohort_type

    !===== Biological prognostic variables
    integer :: ccID       = 0            ! cohort ID
    integer :: species    = 1            ! vegetation species
    real    :: gdd        = 0.0          ! for phenology
    integer :: status     = 0            ! growth status of plant: 1 for ON, 0 for OFF
    integer :: layer      = 1            ! the layer of this cohort (numbered from top, top layer=1)
    integer :: firstlayer = 0            ! 0 = never been in the first layer; 1 = at least one year in first layer
    real    :: layerfrac  = 0.0          ! fraction of layer area occupied by this cohort
    real    :: leaf_age   = 0.0          ! leaf age (years)

    !===== Population structure
    real :: nindivs   = 1.0          ! density of vegetation, individuals/m2
    real :: age       = 0.0          ! age of cohort, years
    real :: dbh       = 0.0          ! diameter at breast height, m
    real :: height    = 0.0          ! vegetation height, m
    real :: crownarea = 1.0          ! crown area, m2/individual
    real :: leafarea  = 0.0          ! total area of leaves, m2/individual
    real :: lai       = 0.0          ! crown leaf area index, m2/m2
    real :: BA        = 0.0          ! tree basal area
    real :: BAL       = 0.0          ! basal area of larger trees
    real :: Volume    = 0.0

    !===== Organic pools
    type(orgpool) :: pleaf                       ! leaf biomass [kg C(N)/ind.]
    type(orgpool) :: proot                       ! root biomass [kg C(N)/ind.]
    type(orgpool) :: psapw                       ! sapwood biomass [kg C(N)/ind.]
    type(orgpool) :: pwood                       ! heartwood (non-living) biomass [kg C(N)/ind.]
    type(orgpool) :: pseed                       ! biomass put aside for future progeny [kg C(N)/ind.]
    type(orgpool) :: plabl                       ! labile pool, temporary storage of N and C [kg C(N)/ind.]

    !===== Carbon fluxes
    real    :: gpp                = 0.0          ! gross primary productivity kg C/timestep
    real    :: npp                = 0.0          ! net primary productivity kg C/timestep
    real    :: resp               = 0.0          ! plant respiration
    real    :: resl               = 0.0          ! leaf respiration
    real    :: resr               = 0.0          ! root respiration
    real    :: resg               = 0.0          ! growth respiration

    real    :: NPPleaf, NPProot, NPPwood         ! to record C allocated to leaf, root, and wood
    real    :: dailyTrsp
    real    :: dailyGPP                          ! kgC/tree day-1
    real    :: dailyNPP
    real    :: dailyResp
    real    :: dailyNup
    real    :: dailyfixedN 
    real    :: annualTrsp
    real    :: annualGPP                         ! C flux/tree
    real    :: annualNPP
    real    :: annualResp 
    real    :: n_deadtrees        = 0.0          ! plant to soil N flux due to mortality (kg N m-2 yr-1)
    real    :: c_deadtrees        = 0.0          ! plant to soil C flux due to mortality (kg C m-2 yr-1)
    real    :: m_turnover         = 0.0          ! C turnover due to mortality and tissue turnover (kg C m-2 yr-1)
    real    :: deathratevalue     = 0.0

    !===== Nitrogen model related parameters
    real    :: NSNmax             = 0.0
    real    :: N_uptake           = 0.0           ! N uptake at each step per tree, kg N/individual h-1
    real    :: annualNup          = 0.0           ! annual N uptake, kg N/individual yr-1
    real    :: fixedN             = 0.0           ! fixed N at each step per tree, kg N/individual h-1
    real    :: annualfixedN       = 0.0           ! annual N fixation, kg N/individual yr-1
    real    :: bl_max             = 0.0           ! Max. leaf biomass, kg C/individual
    real    :: br_max             = 0.0           ! Max. fine root biomass, kg C/individual
    real    :: CSAsw              = 0.0
    real    :: topyear            = 0.0           ! the years that a plant in top layer
    real    :: DBH_ys                             ! DBH at the begining of a year (growing season)
    real    :: BA_ys
    real    :: Vol_ys
    real    :: ABG_ys

    !===== Water uptake-related variables
    real    :: rootarea                           ! total fine root area per tree
    real    :: rootdepth                          ! maximum depth of fine roots
    real    :: rootareaL(max_lev) = 0.0           ! Root length per layer, m of root/m
    real    :: WupL(max_lev)      = 0.0           ! normalized vertical distribution of uptake
    real    :: W_supply                           ! potential water uptake rate per unit time per tree
    real    :: transp                             ! transpiration rate per tree per hour
    real    :: uptake_frac(max_lev)               ! for LM3 soil water uptake, Weng, 2017-10-28
    real    :: K_r, r_r
    real    :: root_zeta

    !===== Photosynthesis
    real    :: An_op              = 0.0           ! mol C/(m2 of leaf per year)
    real    :: An_cl              = 0.0           ! mol C/(m2 of leaf per year)
    real    :: w_scale            = dummy
    real    :: C_growth           = 0.0           ! carbon gain since last growth, kg C/individual
    real    :: N_growth           = 0.0           ! Nitrogen used for plant tissue growth
    real    :: extinct            = 0.75          ! light extinction coefficient in the canopy for photosynthesis

  end type cohort_type



  !=============== Tile level data type ============================================================
  type :: vegn_tile_type

    integer  :: n_cohorts         = 0.0

    !===== For reseting vegetation
    integer :: n_initialCC = 0
    type(cohort_type), pointer :: initialCC(:) => NULL()

    !===== Cohorts nested inside tile
    type(cohort_type), pointer :: cohorts(:) => NULL()

    ! !=====  Litter pools (SOFUN-structure, remain empty in original BiomeE)
    ! type(orgpool) :: plitt_af                     ! above-ground litter, fast turnover [kg C(N)/m2]
    ! type(orgpool) :: plitt_as                     ! above-ground litter, slow turnover [kg C(N)/m2]
    ! type(orgpool) :: plitt_bg                     ! below-ground litter [kg C(N)/m2]

    !=====  Soil organic pools (renamed: metabolicL, metabolicN -> psoil_fs; structuralL, structuralN -> psoil_sl; MicrobialC, MicrobialN -> pmicr)
    type(orgpool) :: psoil_fs        ! soil organic matter, fast turnover [kg C(N)/m2]
    type(orgpool) :: psoil_sl        ! soil organic matter, slow turnover [kg C(N)/m2]
    type(orgpool) :: pmicr           ! microbial biomass (kg C(N)/m2)

    !=====  Inorganic N pools
    type(nitrogen) :: ninorg                      ! Mineral nitrogen pool (kg N/m2)   
    ! type(nitrogen) :: pno3                        ! Soil nitrate pool (kg N/m2)   
    ! type(nitrogen) :: pnh4                        ! Soil ammonium pool (kg N/m2)   

    !===== Tile-level forest inventory information
    real    :: area                               ! m2
    real    :: age                = 0.0           ! tile age
    real    :: nindivs                            ! New varaibles at tile level 
    real    :: DBH                                ! New varaibles at tile level 
    real    :: nindivs12
    real    :: DBH12
    real    :: DBH12pow2
    real    :: QMD12
    real    :: MaxAge
    real    :: MaxVolume
    real    :: MaxDBH
    real    :: NPPL
    real    :: NPPW

    !===== Leaf area index
    real    :: LAI                                ! leaf area index
    real    :: CAI                                ! crown area index
    real    :: root_distance(max_lev)             ! characteristic half-distance between fine roots, m

    !=====  Averaged quantities for PPA phenology
    real    :: tc_daily           = 0.0
    real    :: gdd                = 0.0           ! growing degree-days
    real    :: tc_pheno           = 0.0           ! smoothed canopy air temperature for phenology

    !=====  Litter and soil carbon pools
    real    :: litter             = 0.0           ! litter flux
    real    :: c_deadtrees        = 0.0           ! plant to soil C flux due to mortality (kg C m-2 yr-1)
    real    :: m_turnover         = 0.0           ! C turnover due to mortality and tissue turnover (kg C m-2 yr-1)

    !=====  N-related fluxes
    real    :: totN               = 0.0
    real    :: N_input            = 0.0           ! annual N input (kg N m-2 yr-1)
    real    :: N_uptake           = 0.0           ! N uptake at each time step, kg N m-2 hour-1
    real    :: annualN            = 0.0           ! annual available N in a year
    real    :: Nloss_yr           = 0.0           ! annual N loss
    real    :: N_P2S_yr           = 0.0           ! N turnover (plant to soil) (kg N m-2 yr-1)
    real    :: n_deadtrees        = 0.0           ! plant to soil N flux due to mortality (kg N m-2 yr-1)
    real    :: previousN          = 0.0           ! weighted annual available N
    real    :: initialN0          = 0.0           ! initial available N (kg N m-2)

    !=====  Soil water
    integer :: soiltype                           ! lookup table for soil hydrologic parameters
    real    :: FLDCAP                             ! soil property: field capacity
    real    :: WILTPT                             ! soil property: wilting point 
    real    :: evap                               ! kg m-2 per unit fast time step (mm/hour)
    real    :: transp                             ! kg m-2 hour-1
    real    :: runoff                             ! Water runoff of the veg tile, unit?
    real    :: thetaS                             ! moisture index (ws - wiltpt)/(fldcap - wiltpt)
    real    :: wcl(max_lev)                       ! volumetric soil water content for each layer                    
    real    :: soilWater                          ! kg m-2 in root zone

    !=====  Water uptake-related variables
    real    :: RAI                                ! root area index
    real    :: RAIL(max_lev)      = 0.0           ! Root length per layer, m of root/m
    real    :: W_uptake                           ! water uptake rate per unit time per m2

    !=====  Carbon fluxes
    real    :: gpp                = 0.0           ! gross primary production, kgC m-2 h-1
    real    :: npp                = 0.0           ! net primary productivity, kgC m-2 h-1
    real    :: resp               = 0.0           ! auto-respiration of plants, kgC m-2 h-1
    real    :: nep                = 0.0           ! net ecosystem productivity
    real    :: rh                 = 0.0           ! soil carbon lost to the atmosphere

    !=====  Daily diagnostics
    real    :: dailyGPP     = 0.0
    real    :: dailyNPP     = 0.0
    real    :: dailyResp    = 0.0
    real    :: dailyRh      = 0.0
    real    :: dailyNup     = 0.0
    real    :: dailyfixedN  = 0.0

    !=====  Annual diagnostics
    real    :: dailyPrcp     = 0.0,  annualPrcp  = 0.0                            ! mm m-2 yr-1
    real    :: dailyTrsp     = 0.0,  dailyEvap   = 0.0,   dailyRoff  = 0.0        ! mm m-2 yr-1
    real    :: annualTrsp    = 0.0,  annualEvap  = 0.0,   annualRoff  = 0.0       ! mm m-2 yr-1
    real    :: annualGPP     = 0.0                                                ! kgC m-2 ground yr-1
    real    :: annualNPP     = 0.0
    real    :: annualResp    = 0.0
    real    :: annualRh      = 0.0
    real    :: annualNup     = 0.0                                                ! accumulated N uptake kgN m-2 yr-1
    real    :: annualfixedN  = 0.0                                                ! fixed N in a tile

    !===== Annual reporting at tile level
    type(orgpool) :: pleaf                       ! leaf biomass [kg C m-2]
    type(orgpool) :: proot                       ! root biomass [kg C m-2]
    type(orgpool) :: psapw                       ! sapwood biomass [kg C m-2]
    type(orgpool) :: pwood                       ! heartwood (non-living) biomass [kg C m-2]
    type(orgpool) :: pseed                       ! biomass put aside for future progeny [kg C m-2]
    type(orgpool) :: plabl                       ! labile pool, temporary storage of N and C [kg C m-2]

    real :: totSeedC = 0.0
    real :: totSeedN = 0.0
    real :: totNewCC = 0.0
    real :: totNewCN = 0.0

  end type vegn_tile_type

  !=============== Soil data type ============================================================
  type :: soil_pars_type
    real    :: GMD                                ! geometric mean partice diameter, mm
    real    :: GSD                                ! geometric standard deviation of particle size
    real    :: vwc_wilt
    real    :: vwc_fc
    real    :: vwc_sat
    real    :: vlc_min
    real    :: k_sat_ref                          ! hydraulic conductivity of saturated soil, kg/(m2 s)
    real    :: psi_sat_ref                        ! saturation soil water potential, m
    real    :: chb                                ! Soil texture parameter
    real    :: alpha                              ! vertical changes of soil property, 1: no change
    real    :: heat_capacity_dry
    real    :: tfreeze
  end type soil_pars_type

  type :: soil_prog_type
    real    :: wl
    real    :: ws
    real    :: T
  end type soil_prog_type

  type :: soil_tile_type
    integer                       :: tag          ! kind of the soil
    type(soil_pars_type)          :: pars
    type(soil_prog_type), pointer :: prog(:)
    real,                 pointer :: w_fc(:)
    real,                 pointer :: w_wilt(:)
    real                          :: Eg_part_ref
    real                          :: z0_scalar
    real, pointer                 :: heat_capacity_dry(:)
    real, pointer                 :: e(:),f(:)
    real                          :: uptake_T     ! update temperature from previous time step
    real, pointer                 :: psi(:)       ! soil water potential
  end type soil_tile_type  

  !=============== PFT-specific parameters ======================================================
  integer :: MaxCohortID = 0

  !=============== Params_tile in R =============================================================
  !===== Soil water properties
  real   :: soiltype                                !Sand = 1, LoamySand = 2, SandyLoam = 3, SiltLoam = 4, FrittedClay = 5, Loam = 6, Clay = 7
  real   :: FLDCAP                                  ! vol/vol 
  real   :: WILTPT                                  ! vol/vol

  !===== Carbon pools
  real   :: K1                                      ! Fast soil C decomposition rate (yr-1)
  real   :: K2                                      ! slow soil C decomposition rate (yr-1)
  real   :: K_nitrogen                              ! mineral Nitrogen turnover rate
  real   :: MLmixRatio                              ! the ratio of C and N returned to litters from microbes
  real   :: etaN                                    ! N loss through runoff (organic and mineral)
  real   :: LMAmin                                  ! minimum LMA, boundary condition
  real   :: fsc_fine                                ! fraction of fast turnover carbon in fine biomass
  real   :: fsc_wood                                ! fraction of fast turnover carbon in wood biomass
  real   :: GR_factor                               ! growth respiration factor
  real   :: l_fract                                 ! fraction of the carbon retained after leaf drop
  real   :: retransN                                ! retranslocation coefficient of Nitrogen
  real   :: f_initialBSW      
  real   :: f_N_add                                 ! re-fill of N for sapwood

  !===== Ensheng's growth parameters
  real   :: f_LFR_max  = 0.85                       ! max allocation to leaves and fine roots 

  !===== Leaf life span 
  real   :: c_LLS  = 28.57143                       ! yr/ (kg C m-2), c_LLS=1/LMAs, where LMAs = 0.035

  !===== Calibratable parameters
  real   :: tf_base                                 ! calibratable scalar for respiration
  real   :: par_mort                                ! generic calibratable parameter for mortality module
  real   :: par_mort_under                          ! generic calibratable parameter for mortality understory module

  !===== deathrate = mortrate_d_u * (1+A*exp(B*DBH))/(1+exp(B*DBH))
  real  :: A_mort     = 9.0    ! A coefficient in understory mortality rate correction, 1/year
  real  :: B_mort     = -60.0  ! B coefficient in understory mortality rate correction, 1/m
  real  :: DBHtp      = 2.0    !  m, for canopy tree's mortality rate

contains

  !==============for diagnostics============================================
  subroutine Zero_diagnostics(vegn)

    ! for annual update
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc
    integer :: i
    
    ! daily
    call zero_daily_diagnostics(vegn)

    ! annual
    vegn%annualfixedN = 0.0
    vegn%annualPrcp   = 0.0
    vegn%annualTrsp   = 0.0
    vegn%annualEvap   = 0.0
    vegn%annualRoff   = 0.0
    vegn%annualGPP    = 0.0
    vegn%annualNPP    = 0.0
    vegn%annualResp   = 0.0
    vegn%annualRh     = 0.0
    vegn%N_P2S_yr     = 0.0
    vegn%annualN      = 0.0
    vegn%Nloss_yr     = 0.0
    vegn%annualNup    = 0.0
    vegn%n_deadtrees  = 0.0
    vegn%c_deadtrees  = 0.0

    do i = 1, vegn%n_cohorts
      cc => vegn%cohorts(i)

      cc%C_growth     = 0.0
      cc%N_growth     = 0.0
      cc%gpp          = 0.0
      cc%npp          = 0.0
      cc%resp         = 0.0
      cc%resl         = 0.0
      cc%resr         = 0.0
      cc%resg         = 0.0
      cc%transp       = 0.0

      ! daily
      cc%dailyTrsp    = 0.0
      cc%dailyGPP     = 0.0
      cc%dailyNPP     = 0.0
      cc%dailyResp    = 0.0
      cc%dailyNup     = 0.0
      cc%dailyfixedN  = 0.0

      ! annual
      cc%annualTrsp   = 0.0
      cc%annualGPP    = 0.0
      cc%annualNPP    = 0.0
      cc%annualResp   = 0.0
      cc%annualNup    = 0.0
      cc%annualfixedN = 0.0
      cc%NPPleaf      = 0.0
      cc%NPProot      = 0.0
      cc%NPPwood      = 0.0
      cc%DBH_ys       = cc%dbh
      cc%BA_ys        = cc%BA
      cc%Vol_ys       = cc%Volume
      cc%ABG_ys       = cc%psapw%c%c12 + cc%pwood%c%c12
      cc%n_deadtrees  = 0.0
      cc%c_deadtrees  = 0.0
      cc%m_turnover   = 0.0
    enddo
  
  end subroutine Zero_diagnostics

  subroutine zero_daily_diagnostics( vegn )
    !////////////////////////////////////////////////////////////////////////
    ! Zero daily diagnostics
    !------------------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    vegn%dailyNup  = 0.0
    vegn%dailyGPP  = 0.0
    vegn%dailyNPP  = 0.0
    vegn%dailyResp = 0.0
    vegn%dailyRh   = 0.0
    vegn%dailyPrcp = 0.0
    vegn%dailyTrsp = 0.0
    vegn%dailyEvap = 0.0
    vegn%dailyRoff = 0.0
    vegn%dailyfixedN = 0.0

  end subroutine zero_daily_diagnostics

  subroutine summarize_tile( vegn ) 
    !////////////////////////////////////////////////////////////////////////
    ! for annual update
    !------------------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_type), pointer :: cc
    integer :: i

    ! State variables
    call orginit(vegn%plabl)
    call orginit(vegn%pleaf)
    call orginit(vegn%proot)
    call orginit(vegn%psapw)
    call orginit(vegn%pwood)
    call orginit(vegn%pseed)

    vegn%LAI        = 0.0
    vegn%CAI        = 0.0

    ! New tile outputs
    vegn%nindivs    = 0.0
    vegn%DBH        = 0.0
    vegn%nindivs12  = 0.0
    vegn%DBH12      = 0.0
    vegn%DBH12pow2  = 0.0
    vegn%MaxAge     = 0.0
    vegn%MaxVolume  = 0.0
    vegn%MaxDBH     = 0.0

    do i = 1, vegn%n_cohorts
      cc => vegn%cohorts(i)
      ! organic pools
      call orgcp(cc%plabl, vegn%plabl, cc%nindivs)
      call orgcp(cc%pleaf, vegn%pleaf, cc%nindivs)
      call orgcp(cc%proot, vegn%proot, cc%nindivs)
      call orgcp(cc%psapw, vegn%psapw, cc%nindivs)
      call orgcp(cc%pwood, vegn%pwood, cc%nindivs)
      call orgcp(cc%pseed, vegn%pseed, cc%nindivs)

      vegn%CAI          = vegn%CAI      + cc%crownarea * cc%nindivs
      vegn%LAI          = vegn%LAI      + cc%leafarea  * cc%nindivs
      
      ! New tile outputs
      vegn%DBH          = vegn%DBH      + cc%dbh       * cc%nindivs
      vegn%nindivs      = vegn%nindivs  + cc%nindivs

      if (cc%dbh > 0.12) then
        vegn%DBH12      = vegn%DBH12     + cc%dbh      * cc%nindivs 
        vegn%nindivs12  = vegn%nindivs12 + cc%nindivs
        vegn%DBH12pow2  = vegn%DBH12pow2 + cc%dbh      * cc%dbh     * cc%nindivs
      endif

      if (cc%age    > vegn%MaxAge)       vegn%MaxAge    = cc%age
      if (cc%Volume > vegn%MaxVolume)    vegn%MaxVolume = cc%Volume ! maxloc(cc%age)
      if (cc%dbh    > vegn%MaxDBH)       vegn%MaxDBH    = cc%dbh    ! maxloc(cc%age)

      ! vegn%NPPL      = vegn%NPPL   + fleaf * cc%nindivs
      ! vegn%NPPW      = vegn%NPPW   + fwood * cc%nindivs

      ! vegn%n_deadtrees = vegn%n_deadtrees + cc%n_deadtrees
      ! vegn%c_deadtrees = vegn%c_deadtrees + cc%c_deadtrees

    enddo

    if (vegn%nindivs>0.0)   vegn%DBH   = vegn%DBH / vegn%nindivs  
    if (vegn%nindivs12>0.0) vegn%DBH12 = vegn%DBH12 / vegn%nindivs12  ! vegn%nindivs12 could be zero if all dbh<0.12
    if (vegn%nindivs12>0.0) then
      vegn%QMD12   = sqrt(vegn%DBH12pow2 / vegn%nindivs12)
    else
      vegn%QMD12 = 0.0
    end if

  end subroutine summarize_tile


  subroutine hourly_diagnostics(vegn, forcing)  !, iyears, idoy, ihour, out_hourly_tile
    !////////////////////////////////////////////////////////////////////////
    ! Updates sub-daily tile-level variables and takes running daily sums
    !------------------------------------------------------------------------
    use md_forcing_biomee, only: climate_type
    use md_interface_biomee, only: outtype_hourly_tile, myinterface

    type(vegn_tile_type), intent(inout) :: vegn
    type(climate_type),intent(in):: forcing
    ! integer, intent(in) :: iyears, idoy, ihour
    ! type(outtype_hourly_tile),intent(out) :: out_hourly_tile 

    ! local variables
    type(cohort_type), pointer :: cc    ! current cohort
    integer :: i

    vegn%age = vegn%age + myinterface%dt_fast_yr

    ! Tile summary
    vegn%GPP    = 0
    vegn%NPP    = 0
    vegn%Resp   = 0

    do i = 1, vegn%n_cohorts
      cc => vegn%cohorts(i)

      ! cohort daily
      cc%dailyTrsp = cc%dailyTrsp + cc%transp ! kg day-1
      cc%dailyGPP  = cc%dailygpp  + cc%gpp ! kg day-1
      cc%dailyNPP  = cc%dailyNpp  + cc%Npp ! kg day-1
      cc%dailyResp = cc%dailyResp + cc%Resp ! kg day-1

      ! Tile hourly
      vegn%GPP    = vegn%GPP    + cc%gpp    * cc%nindivs
      vegn%NPP    = vegn%NPP    + cc%Npp    * cc%nindivs
      vegn%Resp   = vegn%Resp   + cc%Resp   * cc%nindivs
    enddo

    ! NEP is equal to NNP minus soil respiration
    vegn%nep = vegn%npp - vegn%rh

    !  if (.not. myinterface%steering%spinup) then
    !   out_hourly_tile%year      =  iyears    
    !   out_hourly_tile%doy       =  idoy   
    !   out_hourly_tile%hour      =  ihour    
    !   out_hourly_tile%rad       =  forcing%radiation    !forcingData 
    !   out_hourly_tile%Tair      =  forcing%Tair         !forcingData  
    !   out_hourly_tile%Prcp      =  forcing%rain         !forcingData 
    !   out_hourly_tile%GPP       =  vegn%GPP  
    !   out_hourly_tile%Resp      =  vegn%resp   
    !   out_hourly_tile%Transp    =  vegn%transp
    !   out_hourly_tile%Evap      =  vegn%evap   
    !   out_hourly_tile%Runoff    =  vegn%runoff   
    !   out_hourly_tile%Soilwater =  vegn%soilwater
    !   out_hourly_tile%wcl       =  vegn%wcl(1)    
    !   out_hourly_tile%FLDCAP    =  vegn%FLDCAP
    !   out_hourly_tile%WILTPT    =  vegn%WILTPT
    ! end if

    ! Daily summary:
    vegn%dailyNup  = vegn%dailyNup  + vegn%N_uptake
    vegn%dailyGPP  = vegn%dailyGPP  + vegn%gpp
    vegn%dailyNPP  = vegn%dailyNPP  + vegn%npp
    vegn%dailyResp = vegn%dailyResp + vegn%resp
    vegn%dailyRh   = vegn%dailyRh   + vegn%rh
    vegn%dailyTrsp = vegn%dailyTrsp + vegn%transp
    vegn%dailyEvap = vegn%dailyEvap + vegn%evap
    vegn%dailyRoff = vegn%dailyRoff + vegn%runoff
    vegn%dailyPrcp = vegn%dailyPrcp + forcing%rain * myinterface%step_seconds

  end subroutine hourly_diagnostics


  subroutine daily_diagnostics( vegn , iyears, idoy, out_daily_tile )  ! , out_daily_cohorts 
    !////////////////////////////////////////////////////////////////////////
    ! Updates daily tile-level variables and takes running annual sums
    !------------------------------------------------------------------------
    use md_forcing_biomee, only: climate_type
    use md_interface_biomee, only: outtype_daily_cohorts, outtype_daily_tile

    type(vegn_tile_type), intent(inout) :: vegn
    integer, intent(in) :: iyears, idoy
    ! type(outtype_daily_cohorts), dimension(out_max_cohorts), intent(out) :: out_daily_cohorts
    type(outtype_daily_tile), intent(out) :: out_daily_tile

    ! local variables
    type(cohort_type), pointer :: cc    ! current cohort
    integer :: i

    ! cohorts output
    do i = 1, vegn%n_cohorts
      cc => vegn%cohorts(i)

      ! running annual sum
      cc%annualGPP  = cc%annualGPP  + cc%dailyGPP
      cc%annualNPP  = cc%annualNPP  + cc%dailyNPP
      cc%annualResp = cc%annualResp + cc%dailyResp
      cc%annualTrsp = cc%annualTrsp + cc%dailyTrsp

      ! Zero Daily variables
      cc%dailyTrsp = 0.0
      cc%dailyGPP  = 0.0
      cc%dailyNPP  = 0.0
      cc%dailyResp = 0.0
    enddo

    ! Tile level, daily
    call summarize_tile(vegn)

    if (.not. myinterface%steering%spinup) then
      out_daily_tile%year      = iyears
      out_daily_tile%doy       = idoy
      out_daily_tile%Tc        = vegn%tc_daily
      out_daily_tile%Prcp      = vegn%dailyPrcp
      out_daily_tile%totWs     = vegn%soilwater
      out_daily_tile%Trsp      = vegn%dailyTrsp
      out_daily_tile%Evap      = vegn%dailyEvap
      out_daily_tile%Runoff    = vegn%dailyRoff
      out_daily_tile%ws1       = vegn%wcl(1)*thksl(1) * 1000
      out_daily_tile%ws2       = vegn%wcl(2)*thksl(2) * 1000
      out_daily_tile%ws3       = vegn%wcl(3)*thksl(3) * 1000
      out_daily_tile%LAI       = vegn%LAI
      out_daily_tile%GPP       = vegn%dailyGPP
      out_daily_tile%Rauto     = vegn%dailyResp
      out_daily_tile%Rh        = vegn%dailyRh
      out_daily_tile%NSC       = vegn%plabl%c%c12
      out_daily_tile%seedC     = vegn%pseed%c%c12
      out_daily_tile%leafC     = vegn%pleaf%c%c12
      out_daily_tile%rootC     = vegn%proot%c%c12
      out_daily_tile%SW_C      = vegn%psapw%c%c12
      out_daily_tile%HW_C      = vegn%pwood%c%c12
      out_daily_tile%NSN       = vegn%plabl%n%n14
      out_daily_tile%seedN     = vegn%pseed%n%n14
      out_daily_tile%leafN     = vegn%pleaf%n%n14
      out_daily_tile%rootN     = vegn%proot%n%n14
      out_daily_tile%SW_N      = vegn%psapw%n%n14
      out_daily_tile%HW_N      = vegn%pwood%n%n14
      out_daily_tile%McrbC     = vegn%pmicr%c%c12
      out_daily_tile%fastSOM   = vegn%psoil_fs%c%c12
      out_daily_tile%slowSOM   = vegn%psoil_sl%c%c12
      out_daily_tile%McrbN     = vegn%pmicr%n%n14
      out_daily_tile%fastSoilN = vegn%psoil_fs%n%n14
      out_daily_tile%slowSoilN = vegn%psoil_sl%n%n14
      out_daily_tile%mineralN  = vegn%ninorg%n14
      out_daily_tile%N_uptk    = vegn%dailyNup
    endif

    ! running annual sums
    vegn%annualNup  = vegn%annualNup  + vegn%dailyNup
    vegn%annualGPP  = vegn%annualGPP  + vegn%dailygpp
    vegn%annualNPP  = vegn%annualNPP  + vegn%dailynpp
    vegn%annualResp = vegn%annualResp + vegn%dailyresp
    vegn%annualRh   = vegn%annualRh   + vegn%dailyrh
    vegn%annualPrcp = vegn%annualPrcp + vegn%dailyPrcp
    vegn%annualTrsp = vegn%annualTrsp + vegn%dailytrsp
    vegn%annualEvap = vegn%annualEvap + vegn%dailyevap
    vegn%annualRoff = vegn%annualRoff + vegn%dailyRoff

    ! zero:
    call zero_daily_diagnostics(vegn)

  end subroutine daily_diagnostics


  subroutine annual_diagnostics(vegn, iyears, out_annual_cohorts, out_annual_tile)
    !////////////////////////////////////////////////////////////////////////
    ! Updates tile-level variables and populates annual output in once
    !------------------------------------------------------------------------
    use md_interface_biomee, only: outtype_annual_cohorts, outtype_annual_tile, myinterface

    type(vegn_tile_type), intent(inout) :: vegn
    integer, intent(in) :: iyears
    type(outtype_annual_cohorts), dimension(out_max_cohorts) :: out_annual_cohorts
    type(outtype_annual_tile) :: out_annual_tile
    ! type(spec_data_type) :: sp

    ! local variables
    type(cohort_type), pointer :: cc
    real :: treeG, fseed, fleaf=0, froot, fwood=0, dDBH, dBA, dVol
    real :: plantC, plantN, soilC, soilN
    integer :: i

    ! re-initialise to avoid elements not updated when number 
    ! of cohorts declines from one year to the next
    out_annual_cohorts(:)%year        = dummy
    out_annual_cohorts(:)%cID         = dummy
    out_annual_cohorts(:)%PFT         = dummy
    out_annual_cohorts(:)%layer       = dummy
    out_annual_cohorts(:)%density     = dummy
    out_annual_cohorts(:)%flayer      = dummy
    out_annual_cohorts(:)%DBH         = dummy
    out_annual_cohorts(:)%dDBH        = dummy
    out_annual_cohorts(:)%height      = dummy
    out_annual_cohorts(:)%age         = dummy
    out_annual_cohorts(:)%BA          = dummy
    out_annual_cohorts(:)%dBA         = dummy
    out_annual_cohorts(:)%Acrown      = dummy
    out_annual_cohorts(:)%Aleaf       = dummy
    out_annual_cohorts(:)%nsc         = dummy
    out_annual_cohorts(:)%nsn         = dummy
    out_annual_cohorts(:)%seedC       = dummy
    out_annual_cohorts(:)%leafC       = dummy
    out_annual_cohorts(:)%rootC       = dummy
    out_annual_cohorts(:)%sapwC       = dummy
    out_annual_cohorts(:)%woodC       = dummy
    out_annual_cohorts(:)%treeG       = dummy
    out_annual_cohorts(:)%fseed       = dummy
    out_annual_cohorts(:)%fleaf       = dummy
    out_annual_cohorts(:)%froot       = dummy
    out_annual_cohorts(:)%fwood       = dummy
    out_annual_cohorts(:)%GPP         = dummy
    out_annual_cohorts(:)%NPP         = dummy
    out_annual_cohorts(:)%Nupt        = dummy
    out_annual_cohorts(:)%Nfix        = dummy
    out_annual_cohorts(:)%n_deadtrees = dummy
    out_annual_cohorts(:)%c_deadtrees = dummy
    out_annual_cohorts(:)%deathrate   = dummy

    ! Cohorts ouput
    do i = 1, vegn%n_cohorts
      cc => vegn%cohorts(i)
      treeG     = cc%pseed%c%c12 + cc%NPPleaf + cc%NPProot + cc%NPPwood
      fseed     = cc%pseed%c%c12 / treeG
      fleaf     = cc%NPPleaf / treeG
      froot     = cc%NPProot / treeG
      fwood     = cc%NPPwood / treeG
      dDBH      = cc%dbh - cc%DBH_ys !in m
      cc%BA     = pi/4 * cc%dbh * cc%dbh
      dBA       = cc%BA - cc%BA_ys
      cc%Volume = (cc%psapw%c%c12 + cc%pwood%c%c12) / myinterface%params_species(cc%species)%rho_wood
      dVol      = (cc%Volume - cc%Vol_ys)

      out_annual_cohorts(i)%year        = iyears
      out_annual_cohorts(i)%cID         = cc%ccID
      out_annual_cohorts(i)%PFT         = cc%species
      out_annual_cohorts(i)%layer       = cc%layer
      out_annual_cohorts(i)%density     = cc%nindivs * 10000
      out_annual_cohorts(i)%flayer     = cc%layerfrac
      out_annual_cohorts(i)%dbh         = cc%dbh * 100   ! *100 to convert m in cm
      out_annual_cohorts(i)%dDBH        = dDBH * 100     ! *100 to convert m in cm
      out_annual_cohorts(i)%height      = cc%height
      out_annual_cohorts(i)%age         = cc%age
      out_annual_cohorts(i)%BA          = cc%BA
      out_annual_cohorts(i)%dBA         = dBA
      out_annual_cohorts(i)%Acrown      = cc%crownarea
      out_annual_cohorts(i)%Aleaf       = cc%leafarea
      out_annual_cohorts(i)%nsc         = cc%plabl%c%c12
      out_annual_cohorts(i)%nsn         = cc%plabl%n%n14
      out_annual_cohorts(i)%seedC       = cc%pseed%c%c12
      out_annual_cohorts(i)%leafC       = cc%pleaf%c%c12
      out_annual_cohorts(i)%rootC       = cc%proot%c%c12
      out_annual_cohorts(i)%sapwC       = cc%psapw%c%c12
      out_annual_cohorts(i)%woodC       = cc%pwood%c%c12
      out_annual_cohorts(i)%treeG       = treeG
      out_annual_cohorts(i)%fseed       = fseed 
      out_annual_cohorts(i)%fleaf       = fleaf
      out_annual_cohorts(i)%froot       = froot
      out_annual_cohorts(i)%fwood       = fwood
      out_annual_cohorts(i)%GPP         = cc%annualGPP
      out_annual_cohorts(i)%NPP         = cc%annualNPP
      out_annual_cohorts(i)%Rauto       = cc%annualResp
      out_annual_cohorts(i)%Nupt        = cc%annualNup
      out_annual_cohorts(i)%Nfix        = cc%annualfixedN
      out_annual_cohorts(i)%n_deadtrees = cc%n_deadtrees
      out_annual_cohorts(i)%c_deadtrees = cc%c_deadtrees
      out_annual_cohorts(i)%deathrate   = cc%deathratevalue

    enddo

    ! tile pools output
    call summarize_tile( vegn )
 
    vegn%NPPL        = 0.0
    vegn%NPPW        = 0.0
    vegn%n_deadtrees = 0
    vegn%c_deadtrees = 0
    vegn%m_turnover  = 0

    do i = 1, vegn%n_cohorts
      cc => vegn%cohorts(i)
      vegn%annualfixedN = vegn%annualfixedN  + cc%annualfixedN * cc%nindivs
      vegn%NPPL         = vegn%NPPL          + cc%NPPleaf * cc%nindivs
      vegn%NPPW         = vegn%NPPW          + cc%NPPwood * cc%nindivs 
      vegn%n_deadtrees  = vegn%n_deadtrees   + cc%n_deadtrees
      vegn%c_deadtrees  = vegn%c_deadtrees   + cc%c_deadtrees
      vegn%m_turnover   = vegn%m_turnover    + cc%m_turnover  
    enddo

    plantC    = vegn%plabl%c%c12 + vegn%pseed%c%c12 + vegn%pleaf%c%c12 + vegn%proot%c%c12 + vegn%psapw%c%c12 + vegn%pwood%c%c12
    plantN    = vegn%plabl%n%n14 + vegn%pseed%n%n14 + vegn%pleaf%n%n14 + vegn%proot%n%n14 + vegn%psapw%n%n14 + vegn%pwood%n%n14

    soilC     = vegn%pmicr%c%c12 + vegn%psoil_fs%c%c12 + vegn%psoil_sl%c%c12
    soilN     = vegn%pmicr%n%n14 + vegn%psoil_fs%n%n14 + vegn%psoil_sl%n%n14 + vegn%ninorg%n14
    vegn%totN = plantN + soilN

    out_annual_tile%year            = iyears
    out_annual_tile%CAI             = vegn%CAI
    out_annual_tile%LAI             = vegn%LAI
    out_annual_tile%density         = vegn%nindivs * 10000   !New tile out  * 10000 to convert in indivs/ha
    out_annual_tile%DBH             = vegn%DBH * 100         !xxx New tile out  * 100 to convert in cm
    out_annual_tile%density12       = vegn%nindivs12 * 10000 !xxx New tile out
    out_annual_tile%DBH12           = vegn%DBH12 * 100       !xxx New tile out  * 100 to convert in cm
    out_annual_tile%QMD12           = vegn%QMD12 * 100       !xxx New tile out  * 100 to convert in cm
    out_annual_tile%NPP             = vegn%annualNPP
    out_annual_tile%GPP             = vegn%annualGPP
    out_annual_tile%Rauto           = vegn%annualResp
    out_annual_tile%Rh              = vegn%annualRh
    out_annual_tile%rain            = vegn%annualPrcp
    out_annual_tile%SoilWater       = vegn%SoilWater
    out_annual_tile%Transp          = vegn%annualTrsp
    out_annual_tile%Evap            = vegn%annualEvap
    out_annual_tile%Runoff          = vegn%annualRoff
    out_annual_tile%plantC          = plantC ! kg C/m2/yr
    out_annual_tile%soilC           = soilC
    out_annual_tile%plantN          = plantN
    out_annual_tile%soilN           = soilN
    out_annual_tile%totN            = (plantN + soilN)
    out_annual_tile%NSC             = vegn%plabl%c%c12
    out_annual_tile%SeedC           = vegn%pseed%c%c12
    out_annual_tile%leafC           = vegn%pleaf%c%c12
    out_annual_tile%rootC           = vegn%proot%c%c12
    out_annual_tile%SapwoodC        = vegn%psapw%c%c12
    out_annual_tile%WoodC           = vegn%pwood%c%c12
    out_annual_tile%NSN             = vegn%plabl%n%n14
    out_annual_tile%SeedN           = vegn%pseed%n%n14
    out_annual_tile%leafN           = vegn%pleaf%n%n14
    out_annual_tile%rootN           = vegn%proot%n%n14
    out_annual_tile%SapwoodN        = vegn%psapw%n%n14
    out_annual_tile%WoodN           = vegn%pwood%n%n14
    out_annual_tile%McrbC           = vegn%pmicr%c%c12
    out_annual_tile%fastSOM         = vegn%psoil_fs%c%c12
    out_annual_tile%SlowSOM         = vegn%psoil_sl%c%c12
    out_annual_tile%McrbN           = vegn%pmicr%n%n14
    out_annual_tile%fastSoilN       = vegn%psoil_fs%n%n14
    out_annual_tile%slowSoilN       = vegn%psoil_sl%n%n14
    out_annual_tile%mineralN        = vegn%ninorg%n14
    out_annual_tile%N_fxed          = vegn%annualfixedN
    out_annual_tile%N_uptk          = vegn%annualNup
    out_annual_tile%N_yrMin         = vegn%annualN
    out_annual_tile%N_P2S           = vegn%N_P2S_yr
    out_annual_tile%N_loss          = vegn%Nloss_yr
    out_annual_tile%totseedC        = vegn%totseedC
    out_annual_tile%totseedN        = vegn%totseedN
    out_annual_tile%Seedling_C      = vegn%totNewCC
    out_annual_tile%Seedling_N      = vegn%totNewCN
    out_annual_tile%MaxAge          = vegn%MaxAge
    out_annual_tile%MaxVolume       = vegn%MaxVolume
    out_annual_tile%MaxDBH          = vegn%MaxDBH
    out_annual_tile%NPPL            = vegn%NPPL
    out_annual_tile%NPPW            = vegn%NPPW
    out_annual_tile%n_deadtrees     = vegn%n_deadtrees
    out_annual_tile%c_deadtrees     = vegn%c_deadtrees
    out_annual_tile%m_turnover      = vegn%m_turnover
    out_annual_tile%c_turnover_time = vegn%pwood%c%c12 / vegn%NPPW

    ! Rebalance N (to compensate for the adjunction in vegn_N_uptake)
    if (myinterface%params_siml%do_closedN_run) call Recover_N_balance(vegn)

  end subroutine annual_diagnostics

  subroutine annual_diagnostics_post_mortality(vegn, out_annual_cohorts, out_annual_tile)
    !////////////////////////////////////////////////////////////////////////
    ! Updates tile-level variables and populates annual output in once
    !------------------------------------------------------------------------
    use md_interface_biomee, only: outtype_annual_cohorts, outtype_annual_tile

    type(vegn_tile_type), intent(inout) :: vegn
    type(outtype_annual_cohorts), dimension(out_max_cohorts) :: out_annual_cohorts
    type(outtype_annual_tile) :: out_annual_tile

    ! local variables
    type(cohort_type), pointer :: cc
    integer :: i

    ! re-initialise to avoid elements not updated when number
    ! of cohorts declines from one year to the next
    out_annual_cohorts(:)%n_deadtrees = dummy
    out_annual_cohorts(:)%c_deadtrees = dummy
    out_annual_cohorts(:)%deathrate   = dummy

    ! Cohorts ouput
    do i = 1, vegn%n_cohorts
      cc => vegn%cohorts(i)
      out_annual_cohorts(i)%n_deadtrees = cc%n_deadtrees
      out_annual_cohorts(i)%c_deadtrees = cc%c_deadtrees
      out_annual_cohorts(i)%deathrate   = cc%deathratevalue

    enddo

    vegn%n_deadtrees = 0
    vegn%c_deadtrees = 0
    vegn%m_turnover  = 0

    do i = 1, vegn%n_cohorts
      cc => vegn%cohorts(i)
      vegn%n_deadtrees  = vegn%n_deadtrees   + cc%n_deadtrees
      vegn%c_deadtrees  = vegn%c_deadtrees   + cc%c_deadtrees
      vegn%m_turnover   = vegn%m_turnover    + cc%m_turnover
    enddo

    out_annual_tile%N_P2S           = vegn%N_P2S_yr
    out_annual_tile%n_deadtrees     = vegn%n_deadtrees
    out_annual_tile%c_deadtrees     = vegn%c_deadtrees
    out_annual_tile%m_turnover      = vegn%m_turnover

  end subroutine annual_diagnostics_post_mortality

  subroutine Recover_N_balance(vegn)
    !////////////////////////////////////////////////////////////////////////
    ! We scale the N pools to contrain the yearly N (soil + plant) to be constant.
    !------------------------------------------------------------------------
    type(vegn_tile_type), intent(inout) :: vegn
    real :: delta, scaling_factor

    delta = vegn%totN - vegn%initialN0

    if (abs(delta) > 1e-6) then
      scaling_factor = 1 - delta / vegn%totN

      vegn%psoil_sl%n%n14 = vegn%psoil_sl%n%n14 * scaling_factor
      vegn%psoil_fs%n%n14 = vegn%psoil_fs%n%n14 * scaling_factor
      vegn%pmicr%n%n14    = vegn%pmicr%n%n14    * scaling_factor
      vegn%ninorg%n14     = vegn%ninorg%n14     * scaling_factor
      vegn%plabl%n%n14    = vegn%plabl%n%n14    * scaling_factor
      vegn%pseed%n%n14    = vegn%pseed%n%n14    * scaling_factor
      vegn%pleaf%n%n14    = vegn%pleaf%n%n14    * scaling_factor
      vegn%proot%n%n14    = vegn%proot%n%n14    * scaling_factor
      vegn%psapw%n%n14    = vegn%psapw%n%n14    * scaling_factor
      vegn%pwood%n%n14    = vegn%pwood%n%n14    * scaling_factor
      vegn%totN = vegn%initialN0
    endif
  end subroutine Recover_N_balance

end module datatypes
