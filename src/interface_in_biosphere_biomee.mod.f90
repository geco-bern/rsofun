module md_interface_in_biomee
  !////////////////////////////////////////////////////////////////
  ! Module for handling I/O (forcing, parameters, output) from 
  ! biome_f to the biosphere() with the BiomeE implementation
  !----------------------------------------------------------------
  use md_params_core
  use, intrinsic :: iso_c_binding, only: c_double

  implicit none

  private
  public  inputs, interface_in_biosphere_biomee, params_species_biomee, init_lu_biomee

  !===== Soil water hydraulics
  integer, public, parameter :: MAX_LEVELS = 3  ! Soil layers, for soil water dynamics
  real, public, parameter ::  thksl(MAX_LEVELS) = (/0.05, 0.45, 1.5/)  ! m, thickness of soil layers

  !===== Leaf life span
  real, parameter  :: c_LLS = 28.57143    ! yr/ (kg C m-2), c_LLS=1yr/LMAs, where LMAs = 0.035 kgC/m2, i.e. 
                                          ! leaves with 0.035 are approximated with a lifespan of 1 
                                          ! year (NOTE: can lead to non-compatible LMA and phenotype parameters)

  !===== Number of parameters
  integer, public, parameter :: nvars_params_siml    = 11
  integer, public, parameter :: nvars_site_info      = 4
  integer, public, parameter :: nvars_params_tile    = 20
  integer, public, parameter :: nvars_init_soil      = 14
  integer, public, parameter :: nvars_init_cohorts   = 24
  integer, public, parameter :: nvars_params_species = 65
  integer, public, parameter :: nvars_init_lu        = 5

  type init_lu_biomee

    real    :: fraction                   ! Area fraction
    real    :: extra_N_input              ! Additional inorg N supply (to account for N fertiliser application), in kg m-2 yr-1
    real    :: extra_turnover_rate        ! Additional soil turnover rate (to account for soil management such as tillage), dimensionless
    real    :: oxidized_litter_fraction   ! Fraction of above-ground turnover that is directly oxidized (crop and grass harvest), dimensionless
    logical :: vegetated                  ! Wehther this LU accept vegetation (i.e. cohorts)

    contains

    procedure populate_init_lu

  end type init_lu_biomee

  type params_siml_biomee

    type(steering_parameters) :: steering_input
    logical :: do_U_shaped_mortality
    logical :: do_closedN_run
    character(len=30) :: method_photosynth
    character(len=30) :: method_mortality

  contains

    procedure populate_params_siml

  end type params_siml_biomee

  type params_tile_biomee
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
    ! real   :: GR_factor       ! unused parameter
    real   :: l_fract
    real   :: retransN
    real   :: f_initialBSW
    real   :: f_N_add
    real   :: tf_base  ! calibratable
  
    !===== GPP P-model parameters (no effect in gs_leuning option)
    real   :: tau_acclim
    ! real   :: soilm_thetastar ! unused parameter
    ! real   :: soilm_betao     ! unused parameter
  
    real   :: CN0metabolicL
    real   :: CN0structuralL
  contains
          
    procedure populate_params_tile
    
  end type params_tile_biomee

  !=============== PFT data type =============================================================
  type params_species_biomee

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
    ! real    :: leaf_size             ! unused     ! characteristic leaf size
    real    :: alpha_phot                         ! photosynthesis efficiency
    real    :: m_cond                             ! factor of stomatal conductance
    real    :: Vmax                               ! max rubisco rate, mol m-2 s-1
    ! real    :: Vannual               ! unused     ! annual productivity per unit area at full fun (kgC m-2 yr-1)
    ! real    :: gamma_L               ! unused     ! leaf respiration coeficient (per yr)
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
    real    :: alphaBM, thetaBM                   ! biomass = alphaBM * DBH ** thetaBM (only of total woody biomass carbon (bole, branches, coarse roots))
    real    :: phiRL                              ! ratio of fine root to leaf area calibratable
    real    :: phiCSA                             ! ratio of sapwood CSA to target leaf area
    real    :: tauNSC                             ! residence time of C in NSC (to define storage capacity)
    real    :: fNSNmax                            ! multiplier for NSNmax

    !===== Default C/N ratios
    real    :: CNroot0                            ! C/N ratios for plant pools (roots), in kg C kg N\eqn{^{-1}}
    real    :: CNsw0                              ! C/N ratios for plant pools (sapwood), in kg C kg N\eqn{^{-1}}
    real    :: CNwood0                            ! C/N ratios for plant pools (heartwood), in kg C kg N\eqn{^{-1}}
    real    :: CNseed0                            ! C/N ratios for plant pools (seeds), in kg C kg N\eqn{^{-1}}
    real    :: CNleaf0                            ! C/N ratio (derived from: CNleafsupport, LNbase, LMA)

    !===== Phenology
    real    :: tk_crit                            ! K, for turning OFF a leaf phenology
    real    :: tk_crit_on                         ! K, for turning ON a leaf phenology
    real    :: gdd_crit                           ! K, critical value of GDD5 for turning ON leaf phenology
    real    :: betaON                ! unused     ! Critical soil moisture for PhenoON
    real    :: betaOFF               ! unused     ! Critical soil moisture for PhenoOFF

    !===== Vital rates
    real    :: maturalage                         ! the age that can reproduce
    real    :: v_seed                             ! fracton of G_SF to G_F
    real    :: seedlingsize                       ! size of the seedlings, kgC/indiv
    real    :: prob_g         = 1.0               ! germination probability
    real    :: prob_e         = 1.0               ! establishment probability
    real    :: mortrate_d_c                       ! yearly mortality rate in canopy
    real    :: mortrate_d_u                       ! yearly mortality rate in understory
    real    :: A_mort
    real    :: B_mort
    
    !===== Population level variables
    real    :: LAImax, underLAImax                ! max. LAI - Overridden
    real    :: LAI_light                          ! light controlled maximum LAI
    real    :: internal_gap_frac                  ! fraction of internal gaps in the canopy
    ! "internal" gaps are the gaps that are created within the canopy by the branch fall processes.
    real    :: kappa
    real    :: extinct
    real    :: f_LFR_max

    !===== GPP P-model parameters (no effect in gs_leuning option)
    real    :: beta            ! unit cost of carboxylation
    real    :: rd_to_vcmax     ! Ratio of Rdark to Vcmax25, number from Atkin et al., 2015 for C3 herbaceous
    real    :: kc_jmax         ! Jmax cost ratio
    real    :: kphio           ! quantum yield efficiency at optimal temperature, phi_0 (Stocker et al., 2020 GMD Eq. 10)
    real    :: kphio_par_a     ! shape parameter of temperature-dependency of quantum yield efficiency
    real    :: kphio_par_b     ! optimal temperature of quantum yield efficiency

    contains

      procedure init_pft_data
      procedure init_derived_species_data
      procedure populate_spec_data

  end type params_species_biomee

  type init_cohort_biomee
    integer :: init_cohort_species
    real    :: init_cohort_density
    real    :: init_cohort_age
    real    :: init_cohort_bl
    real    :: init_cohort_br
    real    :: init_cohort_bsw
    real    :: init_cohort_bHW
    real    :: init_cohort_seedC
    real    :: init_cohort_nsc
    real    :: init_cohort_bl_n14
    real    :: init_cohort_br_n14
    real    :: init_cohort_bsw_n14
    real    :: init_cohort_bHW_n14
    real    :: init_cohort_seedC_n14
    real    :: init_cohort_nsc_n14
    integer :: lu_index ! Which land use (LU) should this cohort be used for. Given as the index in 'init_lu' array.
    integer :: restart_status
    integer :: restart_layer
    integer :: restart_firstlayer
    real    :: restart_gdd
    real    :: restart_leaf_age
    real    :: restart_topyear
    real    :: restart_bl_max
    real    :: restart_br_max

  contains
    
    procedure populate_init_cohort
    
  end type init_cohort_biomee

  type init_soil_biomee
    real :: init_fast_soil_C
    real :: init_slow_soil_C
    real :: init_Nmineral
    real :: N_input
    real :: init_fast_soil_N
    real :: init_slow_soil_N
    real :: init_pmicr_C
    real :: init_pmicr_d13C
    real :: init_pmicr_N
    real :: init_wcl1
    real :: init_wcl2
    real :: init_wcl3
    real :: init_N0_ecosystem
    real :: restart_tk_pheno
    ! real :: restart_vegn_gdd
    
  contains
    
    procedure populate_init_soil
    
  end type init_soil_biomee

  type site_info_biomee
    real :: lon
    real :: lat
    real :: elv ! elevation
    real :: tc_home

  contains

    procedure populate_site_info
  endtype site_info_biomee

  type interface_in_biosphere_biomee
    type(params_siml_biomee)                               :: params_siml
    type(params_species_biomee), dimension(:), allocatable :: params_species
    type(params_tile_biomee)                               :: params_tile
    type(init_cohort_biomee), dimension(:), allocatable    :: init_cohort
    type(init_soil_biomee)                                 :: init_soil
    type(site_info_biomee)                                 :: site_info
    type(init_lu_biomee), dimension(:), allocatable        :: init_lu
    integer                                                :: steps_per_day ! Number of steps in 24h
    integer                                                :: ntstepsyear   ! Number of steps in 1 year
    real                                                   :: dt_fast_yr    ! Duration of one step in yr
    real                                                   :: step_seconds  ! Duration of one step in s

  contains

    procedure populate
    procedure shut_down
  end type interface_in_biosphere_biomee

  ! Data structure containing the parameters and forcing data.
  ! Should not be mutated (it is the case now for historical reasons)
  type(interface_in_biosphere_biomee) :: inputs

contains

  subroutine populate_init_lu(self, init_lu)
    class(init_lu_biomee), intent(inout) :: self
    real(kind=c_double), dimension(nvars_init_lu), intent(in) :: init_lu

    self%fraction                 = real(init_lu(1))
    self%vegetated                = int( init_lu(2)) /= 0
    self%extra_N_input            = real(init_lu(3))
    self%extra_turnover_rate      = real(init_lu(4))
    self%oxidized_litter_fraction = real(init_lu(5))

  end subroutine populate_init_lu

  subroutine populate_params_siml(self, params_siml)
    class(params_siml_biomee), intent(inout) :: self
    real(kind=c_double), dimension(nvars_params_siml), intent(in) :: params_siml

    self%steering_input%do_spinup        = int(params_siml(1)) /= 0
    self%steering_input%spinupyears      = int(params_siml(2))
    self%steering_input%recycle          = int(params_siml(3))
    self%steering_input%firstyeartrend   = int(params_siml(4))
    self%steering_input%nyeartrend       = int(params_siml(5))

    if (self%steering_input%do_spinup) then
      self%steering_input%runyears = self%steering_input%nyeartrend &
              + self%steering_input%spinupyears
    else
      self%steering_input%runyears    = self%steering_input%nyeartrend
      self%steering_input%spinupyears = 0
    endif

    ! Simulation parameters
    self%do_U_shaped_mortality = int(params_siml(7)) /= 0
    self%do_closedN_run        = int(params_siml(8)) /= 0

    ! this needs to be consistent with translation to code in run_biomee_f_bysite.R
    if (int(params_siml(9)) == 1) then
      self%method_photosynth = "gs_leuning"
    else
      self%method_photosynth = "pmodel"
    end if

    select case( int(params_siml(10)) )
    case (1)
      self%method_mortality = "cstarvation"
    case (2)
      self%method_mortality = "growthrate"
    case (3)
      self%method_mortality = "dbh"
    case (4)
      self%method_mortality = "const_selfthin"
    case (5)
      self%method_mortality = "bal"
    end select

    self%steering_input%do_daily_reporting = int(params_siml(11)) /= 0

  end subroutine populate_params_siml

  subroutine shut_down(self)
    class(interface_in_biosphere_biomee), intent(inout) :: self

    deallocate(self%params_species)
    deallocate(self%init_cohort)
    deallocate(self%init_lu)
  end subroutine shut_down

  subroutine populate(self, params_species, init_cohort, init_soil, params_tile, params_siml, site_info, init_lu)
    class(interface_in_biosphere_biomee), intent(inout) :: self
    real(kind=c_double), dimension(:,:), intent(in) :: params_species
    real(kind=c_double), dimension(:,:), intent(in) :: init_cohort
    real(kind=c_double), dimension(nvars_init_soil),   intent(in) :: init_soil
    real(kind=c_double), dimension(nvars_params_tile), intent(in) :: params_tile
    real(kind=c_double), dimension(nvars_params_siml), intent(in) :: params_siml
    real(kind=c_double), dimension(nvars_site_info),   intent(in) :: site_info
    real(kind=c_double), dimension(:,:), intent(in) :: init_lu

    ! ---- local vars ------
    integer :: i, n_init_cohort, n_params_species

    !----------------------------------------------------------------
    ! INTERPRET FORCING
    !----------------------------------------------------------------
    self%steps_per_day = int(params_siml(6)) ! Forcing resolution
    self%ntstepsyear = self%steps_per_day * ndayyear
    self%dt_fast_yr = 1.0 / self%ntstepsyear
    self%step_seconds = secs_per_day / self%steps_per_day ! seconds_per_year * dt_fast_yr

    call self%init_soil%populate_init_soil(init_soil)
    call self%site_info%populate_site_info(site_info)
    call self%params_tile%populate_params_tile(params_tile)
    call self%params_siml%populate_params_siml(params_siml)

    ! Initial cohort sizes
    n_init_cohort = size(init_cohort(:, 1))
    allocate(self%init_cohort(n_init_cohort))

    do i = 1, n_init_cohort
      call self%init_cohort(i)%populate_init_cohort(init_cohort(i, :))
    enddo

    ! Initialize PFT parameters
    n_params_species = size(params_species(:, 1))
    allocate(self%params_species(n_params_species))

    do i = 1, n_params_species
      call self%params_species(i)%populate_spec_data(params_species(i,:))
    enddo

    ! LULUC initializations
    allocate(self%init_lu(size(init_lu(:, 1))))
    do i = 1, size(self%init_lu)
      call self%init_lu(i)%populate_init_lu(init_lu(i, :))
    end do

  end subroutine populate

  subroutine populate_init_cohort(self, init_cohort)
    class(init_cohort_biomee), intent(inout) :: self
    real(kind=c_double), dimension(nvars_init_cohorts), intent(in) :: init_cohort

    self%init_cohort_species   = int( init_cohort(1))
    self%init_cohort_density   = real(init_cohort(2))
    self%init_cohort_age       = real(init_cohort(3))
    self%init_cohort_bl        = real(init_cohort(4))
    self%init_cohort_br        = real(init_cohort(5))
    self%init_cohort_bsw       = real(init_cohort(6))
    self%init_cohort_bHW       = real(init_cohort(7))
    self%init_cohort_seedC     = real(init_cohort(8))
    self%init_cohort_nsc       = real(init_cohort(9))
    self%init_cohort_bl_n14    = real(init_cohort(10))
    self%init_cohort_br_n14    = real(init_cohort(11))
    self%init_cohort_bsw_n14   = real(init_cohort(12))
    self%init_cohort_bHW_n14   = real(init_cohort(13))
    self%init_cohort_seedC_n14 = real(init_cohort(14))
    self%init_cohort_nsc_n14   = real(init_cohort(15))
    self%lu_index              = int( init_cohort(16))
    self%restart_status        = int( init_cohort(17))
    self%restart_layer         = int( init_cohort(18))
    self%restart_firstlayer    = int( init_cohort(19))
    self%restart_gdd           = real(init_cohort(20))
    self%restart_leaf_age      = real(init_cohort(21))
    self%restart_topyear       = real(init_cohort(22))
    self%restart_bl_max        = real(init_cohort(23))
    self%restart_br_max        = real(init_cohort(24))
  end subroutine populate_init_cohort
  
  subroutine populate_init_soil(self, init_soil)
    class(init_soil_biomee), intent(inout) :: self
    real(kind=c_double), dimension(nvars_init_soil), intent(in)  :: init_soil

    ! Initial soil pools
    self%init_fast_soil_C         = real( init_soil(1) )
    self%init_slow_soil_C         = real( init_soil(2) )
    self%init_Nmineral            = real( init_soil(3) )
    self%N_input                  = real( init_soil(4) )
    self%init_fast_soil_N         = real( init_soil(5) )
    self%init_slow_soil_N         = real( init_soil(6) )
    self%init_pmicr_C             = real( init_soil(7) )
    self%init_pmicr_d13C          = real( init_soil(8) )
    self%init_pmicr_N             = real( init_soil(9) )
    self%init_wcl1                = real( init_soil(10))
    self%init_wcl2                = real( init_soil(11))
    self%init_wcl3                = real( init_soil(12))
    self%init_N0_ecosystem        = real( init_soil(13))
    self%restart_tk_pheno         = real( init_soil(14))
    ! self%restart_vegn_gdd         = real( init_soil(15))
    
  end subroutine populate_init_soil
  
  subroutine populate_params_tile(self, params_tile)
    class(params_tile_biomee), intent(inout) :: self
    real(kind=c_double), dimension(nvars_params_tile), intent(in) :: params_tile
    
    ! Tile parameters
    self%soiltype                 = int(  params_tile(1)  ) ! Sand = 1, LoamySand = 2, SandyLoam = 3, SiltLoam = 4, FrittedClay = 5, Loam = 6, Clay = 7
    self%FLDCAP                   = real( params_tile(2)  ) ! vol / vol
    self%WILTPT                   = real( params_tile(3)  ) ! vol / vol
    self%K1                       = real( params_tile(4)  )
    self%K2                       = real( params_tile(5)  )
    self%K_nitrogen               = real( params_tile(6)  )
    self%MLmixRatio               = real( params_tile(7)  )
    self%etaN                     = real( params_tile(8)  )
    self%LMAmin                   = real( params_tile(9)  )
    self%fsc_fine                 = real( params_tile(10) )
    self%fsc_wood                 = real( params_tile(11) )
    ! self%GR_factor                = real( params_tile(12) )    ! unused
    self%l_fract                  = real( params_tile(13) )
    self%retransN                 = real( params_tile(14) )
    self%f_initialBSW             = real( params_tile(15) )
    self%f_N_add                  = real( params_tile(16) )
    self%tf_base                  = real( params_tile(17) )

    ! GPP P-model parameters (no effect in gs_leuning option)
    self%tau_acclim               = real( params_tile(18) )
    !self%soilm_thetastar         = 0.6 * 250 ! unused parameter (not even in PMODEL)
    !self%soilm_betao             = 0.0       ! unused parameter (not even in PMODEL)
    self%CN0metabolicL            = real( params_tile(19) )
    self%CN0structuralL           = real( params_tile(20) )

  end subroutine populate_params_tile  
  
  subroutine populate_site_info(self, site_info)
    class(site_info_biomee), intent(inout) :: self
    real(kind=c_double), dimension(nvars_site_info),   intent(in)  :: site_info

    ! Site info
    self%lon     = real( site_info(1) )
    self%lat     = real( site_info(2) )
    self%elv     = real( site_info(3) )
    self%tc_home = real( site_info(4) )
  end subroutine populate_site_info
  
  subroutine populate_spec_data(self, params_species)
    class(params_species_biomee), intent(inout) :: self
    real(kind=c_double), dimension(nvars_params_species), intent(in) :: params_species

    self%lifeform           = int(  params_species(1))
    self%phenotype          = int(  params_species(2))
    self%pt                 = int(  params_species(3))
    self%alpha_FR           = real( params_species(4))
    self%rho_FR             = real( params_species(5))
    self%root_r             = real( params_species(6))
    self%root_zeta          = real( params_species(7))
    self%Kw_root            = real( params_species(8))
    ! self%leaf_size          = real( params_species(9))    ! unused
    ! self%Vmax               = real( params_species(10)) ! overridden by 0.02 * self%LNbase
    ! self%Vannual            = real( params_species(11))   ! unused
    self%wet_leaf_dreg      = real( params_species(12))
    self%m_cond             = real( params_species(13))
    self%alpha_phot         = real( params_species(14))
    ! self%gamma_L            = real( params_species(15))   ! unused
    self%gamma_LN           = real( params_species(16))
    self%gamma_SW           = real( params_species(17))
    self%gamma_FR           = real( params_species(18))
    self%tk_crit            = real( params_species(19))
    self%tk_crit_on         = real( params_species(20))
    self%gdd_crit           = real( params_species(21))
    ! self%betaON             = real( params_species(22))   ! unused
    ! self%betaOFF            = real( params_species(23))   ! unused
    self%alphaHT            = real( params_species(24)) ! prescribed
    self%thetaHT            = real( params_species(25)) ! prescribed
    self%alphaCA            = real( params_species(26)) ! prescribed
    self%thetaCA            = real( params_species(27)) ! prescribed
    ! self%alphaBM            = real( params_species(28)) ! overridden by self%rho_wood * self%taperfactor * PI/4. * self%alphaHT
    self%thetaBM            = real( params_species(29)) ! prescribed
    self%seedlingsize       = real( params_species(30))
    self%maturalage         = real( params_species(31))
    self%v_seed             = real( params_species(32))
    self%mortrate_d_c       = real( params_species(33))
    self%mortrate_d_u       = real( params_species(34))
    self%LMA                = real( params_species(35)) ! prescribed
    ! self%leafLS             = real( params_species(36)) ! overridden by self%leafLS = c_LLS * self%LMA
    self%LNbase             = real( params_species(37))
    self%CNleafsupport      = real( params_species(38))
    self%rho_wood           = real( params_species(39)) ! prescribed
    self%taperfactor        = real( params_species(40))
    ! self%lAImax             = real( params_species(41)) ! overridden by MAX(0.5, self%LAI_light)
    self%tauNSC             = real( params_species(42))
    self%fNSNmax            = real( params_species(43))
    self%phiCSA             = real( params_species(44))
    ! self%CNleaf0            = real( params_species(45)) ! overridden by self%CNleaf0 = self%LMA/self%LNA
    self%CNsw0              = real( params_species(46))
    self%CNwood0            = real( params_species(47))
    self%CNroot0            = real( params_species(48))
    self%CNseed0            = real( params_species(49))
    self%Nfixrate0          = real( params_species(50))
    self%NfixCost0          = real( params_species(51))
    self%internal_gap_frac  = real( params_species(52))
    self%kphio              = real( params_species(53))
    self%phiRL              = real( params_species(54))
    self%LAI_light          = real( params_species(55))

    ! GPP P-model parameters (no effect in gs_leuning option)
    self%beta            = real( params_species(56))
    self%rd_to_vcmax     = real( params_species(57))
    self%kc_jmax         = real( params_species(58))

    self%kphio_par_a     = real( params_species(59))
    self%kphio_par_b     = real( params_species(60))

    self%extinct         = real( params_species(61))
    self%kappa           = real( params_species(62))
    self%A_mort          = real( params_species(63))
    self%B_mort          = real( params_species(64))
    self%f_LFR_max       = real( params_species(65))

    ! Following parameters are not yet populated and will be initialized with init_pft_data():
    !===== Population level variables
    ! real    :: LAImax, underLAImax                ! max. LAI - Overridden
    !===== Root traits
    ! real    :: root_frac(MAX_LEVELS)              ! root fraction
    ! real    :: SRA                                ! specific fine root area, m2/kg C
    !===== Leaf traits
    ! real    :: leafLS                             ! leaf life span
    ! real    :: alpha_L                            ! leaf turn over rate, (leaf longevity as a function of LMA)
    ! real    :: LNA                                ! leaf Nitrogen per unit area, kg N/m2
    ! real    :: Vmax                               ! max rubisco rate, mol m-2 s-1
    !===== Allometry
    ! real    :: alphaBM                            ! biomass = alphaBM * DBH ** thetaBM
    !===== Vital rates
    ! real    :: prob_g         = 1.0               ! germination probability
    ! real    :: prob_e         = 1.0               ! establishment probability
    !===== Default C/N ratios
    ! real    :: CNleaf0
    
    call self%init_pft_data()

  end subroutine populate_spec_data  

  subroutine init_pft_data(self)
    class(params_species_biomee), intent(inout) :: self

    self%LAImax = MAX(0.5, self%LAI_light)
    self%underLAImax = MIN(self%LAImax, 1.2)

    ! specific root area
    self%SRA           = 2.0/(self%root_r*self%rho_FR)

    ! calculate alphaBM parameter of allometry. note that rho_wood was re-introduced for this calculation ! TODO: note that this overwrites the parameter alphaBM
    self%alphaBM = self%rho_wood * self%taperfactor * PI/4. * self%alphaHT ! 5200
    !              (kgC/tree / m)* (-)              *       * m / m^thetaHT          => alphaBM is in kgC/tree / m^(thetaHT+2)
    ! NOTE: definition of taperfactor is the multiplicative factor to correct the cylindric volume/mass calculated with DBH.
    ! NOTE: e.g. for a cone the volume formula is: V = 1/3 Pi * r^2 * HT, i.e. for a cone the taperfactor is 1/3.
    ! TODO: wouldn't this also require that we assume thetaBM == thetaHT + 2 (eqA2, Weng et al. 2015)? Which is not enforced with current parameters.
    !       

    ! Vmax as a function of LNbase (max rubisco rate, mol m-2 s-1)
    self%Vmax = 0.02 * self%LNbase ! 0.03125 * sp%LNbase ! Vmax/LNbase= 25E-6/0.8E-3 = 0.03125 !

    ! CN0 of leaves
    self%LNA = self%LNbase +  self%LMA/self%CNleafsupport ! LNbase is metabolic (Rubisco) and support is structural
    self%CNleaf0 = self%LMA/self%LNA                      ! This is the total leaf (metabolic and structural)
    ! Leaf life span as a function of LMA
    self%leafLS = c_LLS * self%LMA
    
    call self%init_derived_species_data()

  end subroutine init_pft_data


  subroutine init_derived_species_data(self)

    class(params_species_biomee), intent(inout) :: self

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

    ! Leaf turnover rate
    self%alpha_L = 1.0/self%leafLS * self%phenotype ! unneeded for deciduous (phenotype=0)

  end subroutine init_derived_species_data

end module md_interface_in_biomee
