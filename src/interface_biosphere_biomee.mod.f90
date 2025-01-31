module md_interface_biomee
  !////////////////////////////////////////////////////////////////
  ! Module for handling I/O (forcing, parameters, output) from 
  ! biome_f to the biosphere() with the BiomeE implementation
  !----------------------------------------------------------------
  use md_forcing_biomee, only: climate_type
  use md_params_siml_biomee, only: paramstype_siml_biomee
  use md_params_core

  implicit none

  private
  public  myinterface, interfacetype_biosphere, spec_data_type

  integer, public, parameter :: MAX_LEVELS = 3  ! Soil layers, for soil water dynamics

  !===== Soil water hydrualics
  real, public, parameter ::  thksl(MAX_LEVELS) = (/0.05, 0.45, 1.5/)  ! m, thickness of soil layers

  !===== Leaf life span
  real, parameter  :: c_LLS = 28.57143    ! yr/ (kg C m-2), c_LLS=1/LMAs, where LMAs = 0.035

  type paramstype_tile
    integer:: soiltype
    real   :: FLDCAP
    real   :: WILTPT
    real   :: K1
    real   :: K2
    real   :: K_nitrogen
    real   :: MLmixRatio
    real   :: etaN
    real   :: LMAmin
    real   :: fsc_fine
    real   :: fsc_wood
    real   :: GR_factor
    real   :: l_fract
    real   :: retransN
    real   :: f_initialBSW
    real   :: f_N_add
    real   :: tf_base  ! calibratable
    real   :: par_mort ! calibratable
    real   :: par_mort_under ! calibratable
  end type paramstype_tile

  !=============== PFT data type =============================================================
  type spec_data_type

    integer :: lifeform                           ! 0 for grasses, 1 for trees
    integer :: phenotype                          ! phenology type: 0 for deciduous, 1 for evergreen
    integer :: pt                                 ! photosynthetic physiology of species

    !===== Leaf traits
    real    :: LMA                                ! leaf mass per unit area, kg C/m2
    real    :: leafLS                             ! leaf life span
    real    :: alpha_L                            ! leaf turn over rate, (leaf longevity as a function of LMA)
    real    :: LNA                                ! leaf Nitrogen per unit area, kg N/m2
    real    :: LNbase                             ! basal leaf Nitrogen per unit area, kg N/m2, (Rubisco)
    real    :: CNleafsupport                      ! leaf structural tissues, 175
    real    :: leaf_size                          ! characteristic leaf size
    real    :: alpha_phot                         ! photosynthesis efficiency
    real    :: m_cond                             ! factor of stomatal conductance
    real    :: Vmax                               ! max rubisco rate, mol m-2 s-1
    real    :: Vannual                            ! annual productivity per unit area at full fun (kgC m-2 yr-1)
    real    :: gamma_L                            ! leaf respiration coeficient (per yr)
    real    :: gamma_LN                           ! leaf respiration coeficient per unit N
    real    :: wet_leaf_dreg                      ! wet leaf photosynthesis down-regulation

    !===== Root traits
    real    :: rho_FR                             ! material density of fine roots (kgC m-3)
    real    :: root_r                             ! radius of the fine roots, m
    real    :: root_zeta                          ! e-folding parameter of root vertical distribution (m)
    real    :: root_frac(MAX_LEVELS)              ! root fraction
    real    :: SRA                                ! specific fine root area, m2/kg C
    real    :: gamma_FR                           ! Fine root respiration rate, kgC kgC-1 yr-1
    real    :: alpha_FR                           ! Turnover rate of Fine roots, fraction yr-1
    real    :: Kw_root                            ! fine root water donductivity mol m m-2 s−1 MPa−1 !
    real    :: NfixRate0                          ! Reference N fixation rate (kgN kgC-1 root)
    real    :: NfixCost0                          ! Carbon cost of N fixation (kgC kgN-1)

    !===== Wood traits
    real    :: rho_wood                           ! woody density, kg C m-3 wood
    real    :: gamma_SW                           ! sapwood respiration rate, kgC m-2 Acambium yr-1
    real    :: taperfactor

    !===== Allometry
    real    :: alphaHT, thetaHT                   ! height = alphaHT * DBH ** thetaHT
    real    :: alphaCA, thetaCA                   ! crown area = alphaCA * DBH ** thetaCA
    real    :: alphaBM, thetaBM                   ! biomass = alphaBM * DBH ** thetaBM
    real    :: kphio                              ! quantum yield efficiency calibratable
    real    :: phiRL                              ! ratio of fine root to leaf area calibratable
    real    :: phiCSA                             ! ratio of sapwood CSA to target leaf area
    real    :: tauNSC                             ! residence time of C in NSC (to define storage capacity)
    real    :: fNSNmax                            ! multilier for NSNmax

    !===== Default C/N ratios
    real    :: CNleaf0
    real    :: CNroot0
    real    :: CNsw0
    real    :: CNwood0
    real    :: CNseed0

    !===== Phenology
    real    :: tc_crit                            ! K, for turning OFF a growth season
    real    :: tc_crit_on                         ! K, for turning ON a growth season
    real    :: gdd_crit                           ! K, critical value of GDD5 for turning ON growth season
    real    :: betaON                             ! Critical soil moisture for PhenoON
    real    :: betaOFF                            ! Critical soil moisture for PhenoOFF

    !===== Vital rates
    real    :: maturalage                         ! the age that can reproduce
    real    :: v_seed                             ! fracton of G_SF to G_F
    real    :: seedlingsize                       ! size of the seedlings, kgC/indiv
    real    :: prob_g         = 1.0               ! germination probability
    real    :: prob_e         = 1.0               ! establishment probability
    real    :: mortrate_d_c                       ! yearly mortality rate in canopy
    real    :: mortrate_d_u                       ! yearly mortality rate in understory

    !===== Population level variables
    real    :: LAImax, underLAImax                ! max. LAI - Overridden
    real    :: LAI_light                          ! light controlled maximum LAI
    real    :: internal_gap_frac                  ! fraction of internal gaps in the canopy

    ! "internal" gaps are the gaps that are created within the canopy by the branch fall processes.

    contains

      procedure init_pft_data
      procedure init_derived_species_data

  end type spec_data_type

  type inittype_cohort
    integer :: init_cohort_species
    real :: init_cohort_nindivs
    real :: init_cohort_bl
    real :: init_cohort_br
    real :: init_cohort_bsw
    real :: init_cohort_bHW
    real :: init_cohort_seedC
    real :: init_cohort_nsc
  end type inittype_cohort

  type inittype_soil 
    real :: init_fast_soil_C
    real :: init_slow_soil_C
    real :: init_Nmineral
    real :: N_input
  end type inittype_soil

  type site_info_type
    real :: lon
    real :: lat
    real :: elv ! elevation
  endtype site_info_type

  type interfacetype_biosphere
    type(climate_type), dimension(:), allocatable         :: climate
    type(paramstype_siml_biomee)                          :: params_siml
    type(spec_data_type), dimension(:), allocatable       :: params_species
    type(paramstype_tile)                                 :: params_tile
    type(inittype_cohort), dimension(:), allocatable      :: init_cohort
    type(inittype_soil)                                   :: init_soil
    type(site_info_type)                                  :: site_info
    integer                                               :: steps_per_day
    real                                                  :: dt_fast_yr
    real                                                  :: step_seconds
  end type interfacetype_biosphere

  ! Data structure containing the parameters and forcing data.
  ! Should not be mutated (it is the case now for historical reasons)
  type(interfacetype_biosphere) :: myinterface

  !=============== Output mappings =============================================================

  integer, public, parameter :: DAILY_TILE_YEAR               =  1
  integer, public, parameter :: DAILY_TILE_DOY                =  2
  integer, public, parameter :: DAILY_TILE_TC                 =  3
  integer, public, parameter :: DAILY_TILE_PRCP               =  4
  integer, public, parameter :: DAILY_TILE_SOIL_W             =  5
  integer, public, parameter :: DAILY_TILE_TRSP               =  6
  integer, public, parameter :: DAILY_TILE_EVAP               =  7
  integer, public, parameter :: DAILY_TILE_RUNOFF             =  8
  integer, public, parameter :: DAILY_TILE_WS1                =  9
  integer, public, parameter :: DAILY_TILE_WS2                = 10
  integer, public, parameter :: DAILY_TILE_WS3                = 11
  integer, public, parameter :: DAILY_TILE_LAI                = 12
  integer, public, parameter :: DAILY_TILE_GPP                = 13
  integer, public, parameter :: DAILY_TILE_RESP               = 14
  integer, public, parameter :: DAILY_TILE_RH                 = 15
  integer, public, parameter :: DAILY_TILE_NSC                = 16
  integer, public, parameter :: DAILY_TILE_SEED_C             = 17
  integer, public, parameter :: DAILY_TILE_LEAF_C             = 18
  integer, public, parameter :: DAILY_TILE_ROOT_C             = 19
  integer, public, parameter :: DAILY_TILE_SWC                = 20
  integer, public, parameter :: DAILY_TILE_HWC                = 21
  integer, public, parameter :: DAILY_TILE_NSN                = 22
  integer, public, parameter :: DAILY_TILE_SEED_N             = 23
  integer, public, parameter :: DAILY_TILE_LEAF_N             = 24
  integer, public, parameter :: DAILY_TILE_ROOT_N             = 25
  integer, public, parameter :: DAILY_TILE_SW_N               = 26
  integer, public, parameter :: DAILY_TILE_HW_N               = 27
  integer, public, parameter :: DAILY_TILE_MCRB_C             = 28
  integer, public, parameter :: DAILY_TILE_FASTSOM            = 29
  integer, public, parameter :: DAILY_TILE_SLOWSOM            = 30
  integer, public, parameter :: DAILY_TILE_MCRB_N             = 31
  integer, public, parameter :: DAILY_TILE_FS_N               = 32
  integer, public, parameter :: DAILY_TILE_SL_N               = 33
  integer, public, parameter :: DAILY_TILE_INORG_N            = 34
  integer, public, parameter :: DAILY_TILE_N_UPTK             = 35

  integer, public, parameter :: ANNUAL_COHORTS_ID             =  1
  integer, public, parameter :: ANNUAL_COHORTS_YEAR           =  2
  integer, public, parameter :: ANNUAL_COHORTS_CID            =  3
  integer, public, parameter :: ANNUAL_COHORTS_PFT            =  4
  integer, public, parameter :: ANNUAL_COHORTS_LAYER          =  5
  integer, public, parameter :: ANNUAL_COHORTS_DENSITY        =  6
  integer, public, parameter :: ANNUAL_COHORTS_FLAYER         =  7
  integer, public, parameter :: ANNUAL_COHORTS_DBH            =  8
  integer, public, parameter :: ANNUAL_COHORTS_DDBH           =  9
  integer, public, parameter :: ANNUAL_COHORTS_HEIGHT         = 10
  integer, public, parameter :: ANNUAL_COHORTS_AGE            = 11
  integer, public, parameter :: ANNUAL_COHORTS_BA             = 12
  integer, public, parameter :: ANNUAL_COHORTS_DBA            = 13
  integer, public, parameter :: ANNUAL_COHORTS_ACROWN         = 14
  integer, public, parameter :: ANNUAL_COHORTS_ALEAF          = 15
  integer, public, parameter :: ANNUAL_COHORTS_NCS            = 16
  integer, public, parameter :: ANNUAL_COHORTS_NSN            = 17
  integer, public, parameter :: ANNUAL_COHORTS_SEED_C         = 18
  integer, public, parameter :: ANNUAL_COHORTS_LEAF_C         = 19
  integer, public, parameter :: ANNUAL_COHORTS_ROOT_C         = 20
  integer, public, parameter :: ANNUAL_COHORTS_SW_C           = 21
  integer, public, parameter :: ANNUAL_COHORTS_HW_C           = 22
  integer, public, parameter :: ANNUAL_COHORTS_TREEG          = 23
  integer, public, parameter :: ANNUAL_COHORTS_FSEED          = 24
  integer, public, parameter :: ANNUAL_COHORTS_FLEAF          = 25
  integer, public, parameter :: ANNUAL_COHORTS_FROOT          = 26
  integer, public, parameter :: ANNUAL_COHORTS_FWOOD          = 27
  integer, public, parameter :: ANNUAL_COHORTS_GPP            = 28
  integer, public, parameter :: ANNUAL_COHORTS_NPP            = 29
  integer, public, parameter :: ANNUAL_COHORTS_RESP           = 30
  integer, public, parameter :: ANNUAL_COHORTS_N_UPTK         = 31
  integer, public, parameter :: ANNUAL_COHORTS_N_FIX          = 32
  integer, public, parameter :: ANNUAL_COHORTS_DEATHRATE      = 33
  integer, public, parameter :: ANNUAL_COHORTS_N_LOSS         = 34
  integer, public, parameter :: ANNUAL_COHORTS_C_LOSS         = 35

  integer, public, parameter :: ANNUAL_TILE_YEAR              =  1
  integer, public, parameter :: ANNUAL_TILE_CAI               =  2
  integer, public, parameter :: ANNUAL_TILE_LAI               =  3
  integer, public, parameter :: ANNUAL_TILE_DENSITY           =  4
  integer, public, parameter :: ANNUAL_TILE_DBH               =  5
  integer, public, parameter :: ANNUAL_TILE_DENSITY12         =  6
  integer, public, parameter :: ANNUAL_TILE_DBH12             =  7
  integer, public, parameter :: ANNUAL_TILE_QMD12             =  8
  integer, public, parameter :: ANNUAL_TILE_NPP               =  9
  integer, public, parameter :: ANNUAL_TILE_GPP               = 10
  integer, public, parameter :: ANNUAL_TILE_RESP              = 11
  integer, public, parameter :: ANNUAL_TILE_RH                = 12
  integer, public, parameter :: ANNUAL_TILE_PRCP              = 13
  integer, public, parameter :: ANNUAL_TILE_SOIL_W            = 14
  integer, public, parameter :: ANNUAL_TILE_TRSP              = 15
  integer, public, parameter :: ANNUAL_TILE_EVAP              = 16
  integer, public, parameter :: ANNUAL_TILE_RUNOFF            = 17
  integer, public, parameter :: ANNUAL_TILE_PLANT_C           = 18
  integer, public, parameter :: ANNUAL_TILE_SOIL_C            = 19
  integer, public, parameter :: ANNUAL_TILE_PLANT_N           = 20
  integer, public, parameter :: ANNUAL_TILE_SOIL_N            = 21
  integer, public, parameter :: ANNUAL_TILE_TOT_N             = 22
  integer, public, parameter :: ANNUAL_TILE_NSC               = 23
  integer, public, parameter :: ANNUAL_TILE_SEED_C            = 24
  integer, public, parameter :: ANNUAL_TILE_LEAF_C            = 25
  integer, public, parameter :: ANNUAL_TILE_ROOT_C            = 26
  integer, public, parameter :: ANNUAL_TILE_SW_C              = 27
  integer, public, parameter :: ANNUAL_TILE_HW_C              = 28
  integer, public, parameter :: ANNUAL_TILE_NSN               = 29
  integer, public, parameter :: ANNUAL_TILE_SEED_N            = 30
  integer, public, parameter :: ANNUAL_TILE_LEAF_N            = 31
  integer, public, parameter :: ANNUAL_TILE_ROOT_N            = 32
  integer, public, parameter :: ANNUAL_TILE_SW_N              = 33
  integer, public, parameter :: ANNUAL_TILE_HW_N              = 34
  integer, public, parameter :: ANNUAL_TILE_MCRB_C            = 35
  integer, public, parameter :: ANNUAL_TILE_FASTSOM           = 36
  integer, public, parameter :: ANNUAL_TILE_SLOWSOM           = 37
  integer, public, parameter :: ANNUAL_TILE_MCRB_N            = 38
  integer, public, parameter :: ANNUAL_TILE_FS_N              = 39
  integer, public, parameter :: ANNUAL_TILE_SL_N              = 40
  integer, public, parameter :: ANNUAL_TILE_INORG_N           = 41
  integer, public, parameter :: ANNUAL_TILE_N_FIX             = 42
  integer, public, parameter :: ANNUAL_TILE_N_UPTK            = 43
  integer, public, parameter :: ANNUAL_TILE_NYRMIN            = 44
  integer, public, parameter :: ANNUAL_TILE_NP2S              = 45
  integer, public, parameter :: ANNUAL_TILE_NLOSS             = 46
  integer, public, parameter :: ANNUAL_TILE_TOTSEED_C         = 47
  integer, public, parameter :: ANNUAL_TILE_TOTSEED_N         = 48
  integer, public, parameter :: ANNUAL_TILE_SEEDLING_C        = 49
  integer, public, parameter :: ANNUAL_TILE_SEEDLING_N        = 50
  integer, public, parameter :: ANNUAL_TILE_MAX_AGE           = 51
  integer, public, parameter :: ANNUAL_TILE_MAX_VOULME        = 52
  integer, public, parameter :: ANNUAL_TILE_MAX_DBH           = 53
  integer, public, parameter :: ANNUAL_TILE_NPP_L             = 54
  integer, public, parameter :: ANNUAL_TILE_NPP_W             = 55
  integer, public, parameter :: ANNUAL_TILE_DEADTREES_N       = 56
  integer, public, parameter :: ANNUAL_TILE_DEADTREES_C       = 57
  integer, public, parameter :: ANNUAL_TILE_M_TURNOVER        = 58
  integer, public, parameter :: ANNUAL_TILE_C_TURNOVER_TIME   = 59

contains

  subroutine init_pft_data(self)
    class(spec_data_type), intent(inout) :: self

    self%LAImax = MAX(0.5, self%LAI_light)
    self%underLAImax = MIN(self%LAImax, 1.2)

    ! specific root area
    self%SRA           = 2.0/(self%root_r*self%rho_FR)

    ! calculate alphaBM parameter of allometry. note that rho_wood was re-introduced for this calculation
    self%alphaBM = self%rho_wood * self%taperfactor * PI/4. * self%alphaHT ! 5200

    ! Vmax as a function of LNbase
    self%Vmax = 0.02 * self%LNbase ! 0.03125 * sp%LNbase ! Vmax/LNbase= 25E-6/0.8E-3 = 0.03125 !

    ! CN0 of leaves
    self%LNA = self%LNbase +  self%LMA/self%CNleafsupport
    self%CNleaf0 = self%LMA/self%LNA
    ! Leaf life span as a function of LMA
    self%leafLS = c_LLS * self%LMA
    
    call self%init_derived_species_data()

  end subroutine init_pft_data


  subroutine init_derived_species_data(self)

    class(spec_data_type), intent(inout) :: self

    ! ---- local vars ------
    integer :: j
    real :: rdepth(0:MAX_LEVELS)
    real :: residual

    ! root vertical profile
    rdepth=0.0
    do j=1,MAX_LEVELS
      rdepth(j) = rdepth(j-1)+thksl(j)
      self%root_frac(j) = exp(-rdepth(j-1)/self%root_zeta)- &
              exp(-rdepth(j)  /self%root_zeta)
    enddo
    residual = exp(-rdepth(MAX_LEVELS)/self%root_zeta)
    do j=1,MAX_LEVELS
      self%root_frac(j) = self%root_frac(j) + residual*thksl(j)/rdepth(MAX_LEVELS)
    enddo

    if(self%leafLS>1.0)then
      self%phenotype = 1
    else
      self%phenotype = 0
    endif

    ! Leaf turnover rate, (leaf longevity as a function of LMA)
    self%alpha_L = 1.0/self%leafLS * self%phenotype

  end subroutine init_derived_species_data

end module md_interface_biomee
