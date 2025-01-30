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
  public  myinterface, interfacetype_biosphere, &
    outtype_annual_cohorts, outtype_daily_tile, outtype_annual_tile, spec_data_type

  integer, public, parameter :: MAX_LEVELS = 3  ! Soil layers, for soil water dynamics

  !===== Soil water hydrualics
  real, public, parameter ::  thksl(MAX_LEVELS)             = (/0.05, 0.45, 1.5/)  ! m, thickness of soil layers

  !===== Leaf life span
  real, parameter  :: c_LLS   = 28.57143    ! yr/ (kg C m-2), c_LLS=1/LMAs, where LMAs = 0.035

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

  !----------------------------------------------------------------
  ! Return variable of biosphere()
  !----------------------------------------------------------------

  type outtype_daily_tile   !fno4
    real :: year 
    real :: doy
    real :: Tc
    real :: Prcp
    real :: totWs
    real :: Trsp
    real :: Evap
    real :: Runoff
    real :: ws1
    real :: ws2
    real :: ws3
    real :: LAI
    real :: GPP
    real :: Rauto
    real :: Rh
    real :: NSC
    real :: seedC
    real :: leafC
    real :: rootC
    real :: SW_C
    real :: HW_C
    real :: NSN
    real :: seedN
    real :: leafN
    real :: rootN
    real :: SW_N
    real :: HW_N
    real :: McrbC
    real :: fastSOM
    real :: slowSOM
    real :: McrbN
    real :: fastSoilN
    real :: slowSoilN
    real :: mineralN
    real :: N_uptk
  end type outtype_daily_tile

  type outtype_annual_tile  !fno5
    real :: year
    real :: CAI
    real :: LAI
    real :: density
    real :: DBH
    real :: Density12
    real :: DBH12
    real :: QMD12
    real :: NPP
    real :: GPP
    real :: Rauto
    real :: Rh
    real :: rain
    real :: SoilWater
    real :: Transp
    real :: Evap
    real :: Runoff
    real :: plantC
    real :: soilC
    real :: plantN
    real :: soilN
    real :: totN
    real :: NSC
    real :: SeedC
    real :: leafC
    real :: rootC
    real :: SapwoodC
    real :: WoodC
    real :: NSN
    real :: SeedN
    real :: leafN
    real :: rootN
    real :: SapwoodN
    real :: WoodN
    real :: McrbC
    real :: fastSOM
    real :: SlowSOM
    real :: McrbN
    real :: fastSoilN
    real :: slowSoilN
    real :: mineralN
    real :: N_fxed
    real :: N_uptk
    real :: N_yrMin
    real :: N_P2S
    real :: N_loss
    real :: totseedC
    real :: totseedN
    real :: Seedling_C
    real :: Seedling_N
    real :: MaxAge
    real :: MaxVolume
    real :: MaxDBH
    real :: NPPL
    real :: NPPW
    real :: n_deadtrees
    real :: c_deadtrees
    real :: m_turnover
    real :: c_turnover_time
  end type outtype_annual_tile

  type outtype_annual_cohorts
    real :: year         = dummy
    real :: cID          = dummy
    real :: PFT          = dummy
    real :: layer        = dummy
    real :: density      = dummy
    real :: flayer       = dummy
    real :: DBH          = dummy
    real :: dDBH         = dummy
    real :: height       = dummy
    real :: age          = dummy
    real :: BA           = dummy
    real :: dBA          = dummy
    real :: Acrown       = dummy
    real :: Aleaf        = dummy
    real :: nsc          = dummy
    real :: nsn          = dummy
    real :: seedC        = dummy
    real :: leafC        = dummy
    real :: rootC        = dummy
    real :: sapwC        = dummy
    real :: woodC        = dummy
    real :: treeG        = dummy
    real :: fseed        = dummy
    real :: fleaf        = dummy
    real :: froot        = dummy
    real :: fwood        = dummy
    real :: GPP          = dummy
    real :: NPP          = dummy
    real :: Rauto        = dummy
    real :: Nupt         = dummy
    real :: Nfix         = dummy
    ! Important to initialise these to 0
    real :: n_deadtrees  = 0.0
    real :: c_deadtrees  = 0.0
    real :: deathrate    = 0.0
  end type outtype_annual_cohorts

contains

  subroutine init_pft_data(self)
    class(spec_data_type), intent(inout) :: self
    
    ! ---- local vars ------
    integer :: i

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
