module md_interface_in_biomee
  !////////////////////////////////////////////////////////////////
  ! Module for handling I/O (forcing, parameters, output) from 
  ! biome_f to the biosphere() with the BiomeE implementation
  !----------------------------------------------------------------
  use md_params_siml_biomee
  use md_params_core
  use, intrinsic :: iso_c_binding, only: c_double

  implicit none

  private
  public  inputs, interface_in_biosphere_biomee, params_species_biomee

  integer, public, parameter :: MAX_LEVELS = 3  ! Soil layers, for soil water dynamics

  !===== Soil water hydrualics
  real, public, parameter ::  thksl(MAX_LEVELS) = (/0.05, 0.45, 1.5/)  ! m, thickness of soil layers

  !===== Leaf life span
  real, parameter  :: c_LLS = 28.57143    ! yr/ (kg C m-2), c_LLS=1/LMAs, where LMAs = 0.035

  !===== Number of parameters
  integer, public, parameter :: nvars_site_info      = 3
  integer, public, parameter :: nvars_params_tile    = 19
  integer, public, parameter :: nvars_init_soil      = 4
  integer, public, parameter :: nvars_init_cohorts   = 9
  integer, public, parameter :: nvars_params_species = 55
  integer, public, parameter :: nvars_init_lu        = 1

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
    real   :: GR_factor
    real   :: l_fract
    real   :: retransN
    real   :: f_initialBSW
    real   :: f_N_add
    real   :: tf_base  ! calibratable
    real   :: par_mort ! calibratable
    real   :: par_mort_under ! calibratable
  
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
      procedure populate_spec_data

  end type params_species_biomee

  type init_cohort_biomee
    integer :: init_cohort_species
    real :: init_cohort_nindivs
    real :: init_cohort_bl
    real :: init_cohort_br
    real :: init_cohort_bsw
    real :: init_cohort_bHW
    real :: init_cohort_seedC
    real :: init_cohort_nsc
    
  contains
    
    procedure populate_init_cohort
    
  end type init_cohort_biomee

  type init_soil_biomee
    real :: init_fast_soil_C
    real :: init_slow_soil_C
    real :: init_Nmineral
    real :: N_input
    
  contains
    
    procedure populate_init_soil
    
  end type init_soil_biomee

  type site_info_biomee
    real :: lon
    real :: lat
    real :: elv ! elevation

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

  subroutine shut_down(self)
    class(interface_in_biosphere_biomee), intent(inout) :: self

    deallocate(self%params_species)
    deallocate(self%init_cohort)
  end subroutine shut_down

  subroutine populate(self, params_species, init_cohort, init_soil, params_tile, params_siml, site_info)
    class(interface_in_biosphere_biomee), intent(inout) :: self
    real(kind=c_double), dimension(:,:), intent(in) :: params_species
    real(kind=c_double), dimension(:,:), intent(in)  :: init_cohort
    real(kind=c_double), dimension(nvars_init_soil),   intent(in)  :: init_soil
    real(kind=c_double), dimension(nvars_params_tile), intent(in) :: params_tile
    real(kind=c_double), dimension(nvars_params_siml), intent(in) :: params_siml
    real(kind=c_double), dimension(nvars_site_info),   intent(in)  :: site_info

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
    call self%params_siml%populate(params_siml)

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

  end subroutine populate

  subroutine populate_init_cohort(self, init_cohort)
    class(init_cohort_biomee), intent(inout) :: self
    real(kind=c_double), dimension(nvars_init_cohorts), intent(in) :: init_cohort

    self%init_cohort_species = int( init_cohort(1))
    self%init_cohort_nindivs = real(init_cohort(2))
    self%init_cohort_bl      = real(init_cohort(3))
    self%init_cohort_br      = real(init_cohort(4))
    self%init_cohort_bsw     = real(init_cohort(5))
    self%init_cohort_bHW     = real(init_cohort(6))
    self%init_cohort_seedC   = real(init_cohort(7))
    self%init_cohort_nsc     = real(init_cohort(8))
  end subroutine populate_init_cohort
  
  subroutine populate_init_soil(self, init_soil)
    class(init_soil_biomee), intent(inout) :: self
    real(kind=c_double), dimension(nvars_init_soil), intent(in)  :: init_soil

    ! Initial soil pools
    self%init_fast_soil_C         = real( init_soil(1) )
    self%init_slow_soil_C         = real( init_soil(2) )
    self%init_Nmineral            = real( init_soil(3) )
    self%N_input                  = real( init_soil(4) )
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
    self%GR_factor                = real( params_tile(12) )
    self%l_fract                  = real( params_tile(13) )
    self%retransN                 = real( params_tile(14) )
    self%f_initialBSW             = real( params_tile(15) )
    self%f_N_add                  = real( params_tile(16) )
    self%tf_base                  = real( params_tile(17) )
    self%par_mort                 = real( params_tile(18) )
    self%par_mort_under           = real( params_tile(19) )
  end subroutine populate_params_tile  
  
  subroutine populate_site_info(self, site_info)
    class(site_info_biomee), intent(inout) :: self
    real(kind=c_double), dimension(nvars_site_info),   intent(in)  :: site_info

    ! Site info
    self%lon = real( site_info(1) )
    self%lat = real( site_info(2) )
    self%elv = real( site_info(3) )
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
    self%leaf_size          = real( params_species(9))
    self%Vmax               = real( params_species(10))
    self%Vannual            = real( params_species(11))
    self%wet_leaf_dreg      = real( params_species(12))
    self%m_cond             = real( params_species(13))
    self%alpha_phot         = real( params_species(14))
    self%gamma_L            = real( params_species(15))
    self%gamma_LN           = real( params_species(16))
    self%gamma_SW           = real( params_species(17))
    self%gamma_FR           = real( params_species(18))
    self%tc_crit            = real( params_species(19))
    self%tc_crit_on         = real( params_species(20))
    self%gdd_crit           = real( params_species(21))
    self%betaON             = real( params_species(22))
    self%betaOFF            = real( params_species(23))
    self%alphaHT            = real( params_species(24)) ! prescribed
    self%thetaHT            = real( params_species(25)) ! prescribed
    self%alphaCA            = real( params_species(26)) ! prescribed
    self%thetaCA            = real( params_species(27)) ! prescribed
    self%alphaBM            = real( params_species(28)) ! prescribed
    self%thetaBM            = real( params_species(29)) ! prescribed
    self%seedlingsize       = real( params_species(30))
    self%maturalage         = real( params_species(31))
    self%v_seed             = real( params_species(32))
    self%mortrate_d_c       = real( params_species(33))
    self%mortrate_d_u       = real( params_species(34))
    self%LMA                = real( params_species(35)) ! prescribed
    self%leafLS             = real( params_species(36))
    self%LNbase             = real( params_species(37))
    self%CNleafsupport      = real( params_species(38))
    self%rho_wood           = real( params_species(39)) ! prescribed
    self%taperfactor        = real( params_species(40))
    ! self%lAImax             = real( params_species(41)) ! overriden
    self%tauNSC             = real( params_species(42))
    self%fNSNmax            = real( params_species(43))
    self%phiCSA             = real( params_species(44))
    self%CNleaf0            = real( params_species(45))
    self%CNsw0              = real( params_species(46))
    self%CNwood0            = real( params_species(47))
    self%CNroot0            = real( params_species(48))
    self%CNseed0            = real( params_species(49))
    self%Nfixrate0          = real( params_species(50))
    self%NfixCost0          = real( params_species(51))
    self%internal_gap_frac  = real( params_species(52))
    self%kphio              = real( params_species(53)) ! calibratable
    self%phiRL              = real( params_species(54)) ! calibratable
    self%LAI_light          = real( params_species(55)) ! calibratable

    call self%init_pft_data()

  end subroutine populate_spec_data  

  subroutine init_pft_data(self)
    class(params_species_biomee), intent(inout) :: self

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

    if(self%leafLS>1.0)then
      self%phenotype = 1
    else
      self%phenotype = 0
    endif

    ! Leaf turnover rate, (leaf longevity as a function of LMA)
    self%alpha_L = 1.0/self%leafLS * self%phenotype

  end subroutine init_derived_species_data

end module md_interface_in_biomee
