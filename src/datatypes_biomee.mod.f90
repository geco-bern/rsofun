module datatypes_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing BiomeE state variable and parameter 
  ! definitions.
  ! Code adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !----------------------------------------------------------------
  use md_interface_biomee, only: myinterface, spec_data_type, MAX_LEVELS
  use md_params_core
  use md_classdefs
  use md_cohort
  use md_cohort_linked_list

  ! define data types and constants
  implicit none
  !=============== Public types ===========================================================
  public :: spec_data_type, cohort_type, vegn_tile_type, dampended_forcing_type

  !=============== Public subroutines =====================================================
  public :: initialize_PFT_data

  !=============== Parameters ======================================================
  integer, public, parameter :: NCohortMax = 50 ! maximum number of cohorts

  !=============== Number of parameters (out) ==============================================
  integer, public, parameter :: nvars_daily_tile     = 35
  integer, public, parameter :: nvars_annual_tile    = 59
  integer, public, parameter :: nvars_annual_cohorts = 35
  integer, public, parameter :: nvars_lu_out         = 2
  integer, public, parameter :: out_max_cohorts      = NCohortMax

  !=============== Number of parameters (out) ==============================================
  integer, public, parameter :: nvars_forcing        = 7
  integer, public, parameter :: nvars_site_info      = 3
  integer, public, parameter :: nvars_params_siml    = 10
  integer, public, parameter :: nvars_params_tile    = 19
  integer, public, parameter :: nvars_init_soil      = 4
  integer, public, parameter :: nvars_init_cohorts   = 9
  integer, public, parameter :: nvars_params_species = 55
  integer, public, parameter :: nvars_init_lu        = 1

  !===== Model
  integer, public, parameter :: NLAYERS_MAX = 9     ! maximum number of canopy layers to be considered

  !===== Photosynthesis
  real, public, parameter  :: extinct = 0.75        ! light extinction coefficient in the canopy for photosynthesis

  !===== Soil water hydrualics
  real, public, parameter ::  thksl(MAX_LEVELS)             = (/0.05, 0.45, 1.5/)  ! m, thickness of soil layers

  !===== Soil SOM reference C/N ratios
  real, parameter :: CN0metabolicL                       = 15.0
  real, parameter :: CN0structuralL                      = 40.0

  !===== fraction = mortrate_d_u * (1+A*exp(B*DBH))/(1+exp(B*DBH))
  real, parameter  :: A_mort     = 9.0    ! A coefficient in understory mortality rate correction, 1/year
  real, parameter  :: B_mort     = -60.0  ! B coefficient in understory mortality rate correction, 1/m

  !===== Ensheng's growth parameters
  real, parameter  :: f_LFR_max  = 0.85    ! max allocation to leaves and fine roots

  !===== Leaf life span
  real, parameter  :: c_LLS   = 28.57143    ! yr/ (kg C m-2), c_LLS=1/LMAs, where LMAs = 0.035

  type :: dampended_forcing_type
    logical :: initialized = .true.
    real :: co2  = 0.0
    real :: vpd  = 0.0
    real :: temp = 0.0
    real :: patm = 0.0
    real :: par  = 0.0
  end type dampended_forcing_type

  !=============== Tile level data type ============================================================
  type :: vegn_tile_type

    ! Linked list of cohorts.
    ! Cohorts should not assumed to be ranked in any specific order.
    ! Use sort_cohorts_by_height() or implement a method following the same principle if needed.
    type(cohort_stack), private :: cohort_list
    ! Linked list of killed cohort fractions. It is empied at the end of each year.
    ! Contrary to cohort_list, this list may have duplicated uid as one cohort from cohort_stack may spawn multiple fractions placed in killed_fraction_list.
    ! Each fraction coming from the same initial cohort have the same uid as that cohort (which explains why they may not be unique).
    type(cohort_stack), private :: killed_fraction_list

    !===== Tile-level forest inventory information
    real    :: area                               ! m2
    real    :: age                = 0.0           ! tile age
    real    :: nindivs
    real    :: DBH
    real    :: nindivs12
    real    :: DBH12
    real    :: DBH12pow2
    real    :: QMD12
    real    :: MaxAge
    real    :: MaxVolume
    real    :: MaxDBH

    !===== Organic pools, kg m-2
    type(orgpool) :: pleaf                       ! leaf biomass, kg m-2
    type(orgpool) :: proot                       ! root biomass, kg m-2
    type(orgpool) :: psapw                       ! sapwood biomass, kg m-2
    type(orgpool) :: pwood                       ! heartwood (non-living) biomass, kg m-2
    type(orgpool) :: pseed                       ! biomass put aside for future progeny, kg m-2
    type(orgpool) :: plabl                       ! labile pool, temporary storage of N and C, kg m-2

    !===== Soil organic pools, kg m-2
    ! renamed: metabolicL, metabolicN -> psoil_fs; structuralL, structuralN -> psoil_sl; MicrobialC, MicrobialN -> pmicr
    type(orgpool) :: psoil_fs        ! soil organic matter, fast turnover [kg C(N)/m2]
    type(orgpool) :: psoil_sl        ! soil organic matter, slow turnover [kg C(N)/m2]
    type(orgpool) :: pmicr           ! microbial biomass (kg C(N)/m2)

    !===== Inorganic N pools, k m-2
    type(nitrogen) :: ninorg                      ! Mineral nitrogen pool (kg N/m2)

    !===== C fluxes, kg yr-1 m-2
    real    :: NPPL               = 0.0           ! NPP leaf, kg C m-2 yr-1
    real    :: NPPW               = 0.0           ! NPP wood, kg C m-2 yr-1

    !=====  Soil carbon fluxes due to death, kg yr-1 m-2
    real    :: c_deadtrees        = 0.0           ! plant to soil C flux due to mortality (kg C m-2 yr-1)
    real    :: m_turnover         = 0.0           ! C turnover due to mortality and tissue turnover (kg C m-2 yr-1)

    !===== Leaf area index
    real    :: LAI                                ! leaf area index (surface of leaves per m2 of ground/tile)
    real    :: CAI                                ! crown area index

    !=====  Averaged quantities for PPA phenology
    real    :: tc_daily           = 0.0           ! 24h average temperature (deg C)
    real    :: gdd                = 0.0           ! growing degree-days
    real    :: tc_pheno           = 0.0           ! smoothed canopy air temperature for phenology

    !=====  N-related fluxes
    real    :: totN               = 0.0
    real    :: N_input            = 0.0           ! annual N input (kg N m-2 yr-1)
    real    :: N_uptake           = 0.0           ! N uptake at each time step, kg N m-2 timestep-1
    real    :: fixedN             = 0.0           ! fixed N at each time step, kg N m-2 timestep-1
    real    :: annualN            = 0.0           ! annual available N in a year
    real    :: Nloss_yr           = 0.0           ! annual N loss
    real    :: N_P2S_yr           = 0.0           ! N turnover (plant to soil) (kg N m-2 yr-1)
    real    :: n_deadtrees        = 0.0           ! plant to soil N flux due to mortality (kg N m-2 yr-1)
    real    :: previousN          = 0.0           ! weighted annual available N
    real    :: initialN0          = 0.0           ! initial available N (kg N m-2)

    !=====  Soil water
    real    :: evap                               ! kg H2O m-2 timestep-1
    real    :: transp                             ! kg H2O m-2 timestep-1
    real    :: runoff                             ! Water runoff of the veg tile, unit?
    real    :: thetaS                             ! moisture index (ws - wiltpt)/(fldcap - wiltpt)
    real    :: wcl(MAX_LEVELS)                    ! volumetric soil water content for each layer
    real    :: soilWater                          ! kg m-2 in root zone

    !=====  Fast loop carbon fluxes, kg timestep-1 m-2
    real    :: gpp                = 0.0           ! gross primary production, kg C m-2 timestep-1
    real    :: npp                = 0.0           ! net primary productivity, kg C m-2 timestep-1
    real    :: resp               = 0.0           ! auto-respiration of plants, kg C m-2 timestep-1
    real    :: nep                = 0.0           ! net ecosystem productivity, kg C m-2 timestep-1
    real    :: rh                 = 0.0           ! soil carbon lost to the atmosphere, kg C m-2 timestep-1

    !=====  Daily fluxes, kg day-1 m-2
    real    :: dailyGPP           = 0.0
    real    :: dailyNPP           = 0.0
    real    :: dailyResp          = 0.0
    real    :: dailyRh            = 0.0
    real    :: dailyNup           = 0.0
    real    :: dailyfixedN        = 0.0
    real    :: dailyPrcp          = 0.0
    real    :: dailyTrsp          = 0.0
    real    :: dailyEvap          = 0.0
    real    :: dailyRoff          = 0.0

    !=====  Annual fluxes, kg yr-1 m-2
    real    :: annualGPP          = 0.0
    real    :: annualNPP          = 0.0
    real    :: annualResp         = 0.0
    real    :: annualRh           = 0.0
    real    :: annualNup          = 0.0
    real    :: annualfixedN       = 0.0
    real    :: annualPrcp         = 0.0
    real    :: annualTrsp         = 0.0
    real    :: annualEvap         = 0.0
    real    :: annualRoff         = 0.0

    !===== Annual C/N allocation to seed and non-seed, kg yr-1 m-2
    real    :: totSeedC           = 0.0           ! Total seed C, kg C yr-1 m-2
    real    :: totSeedN           = 0.0           ! Total seed N, kg N yr-1 m-2
    real    :: totNewCC           = 0.0           ! New cohort C (all compartments but seed), kg C yr-1 m-2
    real    :: totNewCN           = 0.0           ! New cohort N (all compartments but seed), kg N yr-1 m-2

    ! Scrap variable used by gpp()
    type(dampended_forcing_type) :: dampended_forcing

    contains

    procedure n_cohorts
    procedure cohorts
    procedure killed_cohort_fractions
    procedure new_cohort
    procedure sort_cohorts_by_height
    procedure sort_cohorts_by_uid
    procedure shut_down
    procedure thin_cohort
    procedure merge_cohorts
    procedure split_cohort
    procedure annual_diagnostics
    procedure annual_diagnostics_post_mortality
    procedure daily_diagnostics
    procedure hourly_diagnostics
    procedure summarize_tile
    procedure :: plant2soil
    procedure zero_diagnostics
    procedure, private :: zero_daily_diagnostics
    procedure, private :: kill_cohort

  end type vegn_tile_type

contains

  subroutine split_cohort(self, item, fraction)
    ! Split a cohort into two. The initial cohort gets scaled by (1 - fraction),
    ! while a newly created cohort, inserted in the cohort list, gets the complement (fraction).
    class(vegn_tile_type) :: self
    type(cohort_item), pointer :: item, new
    real :: fraction

    new => item%clone()
    new%cohort%nindivs = item%cohort%nindivs * fraction
    call self%cohort_list%insert_item(new)
    item%cohort%nindivs = item%cohort%nindivs - new%cohort%nindivs
  end subroutine split_cohort

  function merge_cohorts(self, c1, c2) result(next_item)
    ! Merge cohort c2 into c1 and return item following c2
    class(vegn_tile_type) :: self
    type(cohort_item), pointer :: c1, c2
    type(cohort_item), pointer :: next_item


    call c1%cohort%merge_in(c2%cohort)
    next_item => self%cohort_list%destroy_item(c2)
  end function merge_cohorts

  function cohorts(self) result(head_cohort)
    ! Return the head of the cohort list for iteration purpose.
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer :: head_cohort

    head_cohort => self%cohort_list%head()
  end function cohorts

  function killed_cohort_fractions(self) result(head_cohort)
    ! Return the head of the killed cohort list for iteration purpose.
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer :: head_cohort

    head_cohort => self%killed_fraction_list%head()
  end function killed_cohort_fractions

  function thin_cohort(self, item, fraction) result(next_item)
    ! Thin the provided cohort by applying a deathrate = 'fraction'.
    ! By default fraction = 1.0, which means that the whole cohort disappears.
    !
    ! Implementation details:
    ! This creates a new cohort in 'killed_fraction_list' with nindivis = cohort%nindivis * fraction
    ! And the original cohort is the complement: nindivis = cohort%nindivis * (1-fraction)
    ! If fraction is 0, nothing happens.
    ! If fraction is 1, the cohort is moved from cohorts to killed_cohort_fractions.
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer, intent(inout) :: item
    real, optional, intent(in) :: fraction
    type(cohort_item), pointer :: next_item

    ! Local variables
    type(cohort_item), pointer :: killed
    real :: frac

    frac = 1.0
    if (present(fraction)) frac = fraction

    if (frac <= 0.0) then
      next_item => null()
    elseif (frac >= 1.0) then
      next_item => self%kill_cohort(item)
    else
      killed => item%clone(.true.)
      killed%cohort%nindivs = item%cohort%nindivs * frac
      killed%cohort%deathrate = frac
      call self%killed_fraction_list%insert_item(killed)
      item%cohort%nindivs = item%cohort%nindivs - killed%cohort%nindivs
      next_item => item%next()
    end if
  end function thin_cohort

  subroutine shut_down(self)
    ! Free all allocated memory
    class(vegn_tile_type) :: self

    call self%cohort_list%destroy_all()
    call self%killed_fraction_list%destroy_all()
  end subroutine shut_down

  function get_height(item) result(res)
    type(cohort_item) :: item
    real :: res

    res = item%cohort%height()
  end function get_height

  subroutine sort_cohorts_by_height(self, increasing)
    ! Sort cohorts by height
    class(vegn_tile_type) :: self
    logical :: increasing

    call self%cohort_list%sort(increasing, get_height)

  end subroutine sort_cohorts_by_height

  function get_uid(item) result(res)
    type(cohort_item) :: item
    real :: res

    res = real(item%uid())
  end function get_uid

  subroutine sort_cohorts_by_uid(self, increasing)
    ! Sort cohorts by uid
    class(vegn_tile_type) :: self
    logical :: increasing

    call self%cohort_list%sort(increasing, get_uid)

  end subroutine sort_cohorts_by_uid

  function n_cohorts(self) result(res)
    ! Returns the current number of cohorts
    integer :: res
    class(vegn_tile_type) :: self

    res = self%cohort_list%length()
  end function n_cohorts

  function new_cohort(self) result(new_item)
    ! Insert a new cohort at the head of the list and return its pointer.
    type(cohort_item), pointer :: new_item
    class(vegn_tile_type) :: self

    new_item => create_cohort()
    call self%cohort_list%insert_item(new_item)

  end function new_cohort

  function kill_cohort(self, item) result(next_item)
    ! Move item to killed_cohort_fractions and return pointer to next
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer, intent(inout) :: item
    type(cohort_item), pointer :: next_item

    item%cohort%deathrate = 1.0
    next_item => self%cohort_list%detach_item(item)
    call self%killed_fraction_list%insert_item(item)
  end function kill_cohort

  !==============for diagnostics============================================
  subroutine zero_diagnostics(self)

    ! for annual update
    class(vegn_tile_type), intent(inout) :: self

    ! local variables
    type(cohort_item), pointer :: it !iterator

    ! daily
    call self%zero_daily_diagnostics()

    ! annual
    self%annualfixedN = 0.0
    self%annualPrcp   = 0.0
    self%annualTrsp   = 0.0
    self%annualEvap   = 0.0
    self%annualRoff   = 0.0
    self%annualGPP    = 0.0
    self%annualNPP    = 0.0
    self%annualResp   = 0.0
    self%annualRh     = 0.0
    self%N_P2S_yr     = 0.0
    self%annualN      = 0.0
    self%Nloss_yr     = 0.0
    self%annualNup    = 0.0
    self%NPPL         = 0.0
    self%NPPW         = 0.0
    self%n_deadtrees  = 0.0
    self%c_deadtrees  = 0.0
    self%m_turnover   = 0.0

    ! We reset the cohorts internal state
    it => self%cohorts()
    do while (associated(it))
      call it%cohort%reset_cohort()
      it => it%next()
    end do

    ! We clear the list of killed cohort fractions
    call self%killed_fraction_list%destroy_all()
  
  end subroutine zero_diagnostics

  subroutine zero_daily_diagnostics(self)
    !////////////////////////////////////////////////////////////////////////
    ! Zero daily diagnostics
    !------------------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self

    self%dailyNup  = 0.0
    self%dailyGPP  = 0.0
    self%dailyNPP  = 0.0
    self%dailyResp = 0.0
    self%dailyRh   = 0.0
    self%dailyPrcp = 0.0
    self%dailyTrsp = 0.0
    self%dailyEvap = 0.0
    self%dailyRoff = 0.0
    self%dailyfixedN = 0.0

  end subroutine zero_daily_diagnostics

  subroutine summarize_tile( self )
    !////////////////////////////////////////////////////////////////////////
    ! for annual update
    !------------------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it !iterator
    real :: dbh ! cache variable

    ! State variables
    call orginit(self%plabl)
    call orginit(self%pleaf)
    call orginit(self%proot)
    call orginit(self%psapw)
    call orginit(self%pwood)
    call orginit(self%pseed)

    self%LAI        = 0.0
    self%CAI        = 0.0

    ! New tile outputs
    self%nindivs    = 0.0
    self%DBH        = 0.0
    self%nindivs12  = 0.0
    self%DBH12      = 0.0
    self%DBH12pow2  = 0.0
    self%MaxAge     = 0.0
    self%MaxVolume  = 0.0
    self%MaxDBH     = 0.0

    it => self%cohorts()
    do while (associated(it))
      cc => it%cohort

      ! organic pools
      call orgcp(cc%plabl, self%plabl, cc%nindivs)
      call orgcp(cc%pleaf, self%pleaf, cc%nindivs)
      call orgcp(cc%proot, self%proot, cc%nindivs)
      call orgcp(cc%psapw, self%psapw, cc%nindivs)
      call orgcp(cc%pwood, self%pwood, cc%nindivs)
      call orgcp(cc%pseed, self%pseed, cc%nindivs)

      self%CAI          = self%CAI      + cc%crownarea() * cc%nindivs
      self%LAI          = self%LAI      + cc%leafarea()  * cc%nindivs
      
      ! New tile outputs
      dbh = cc%dbh()
      self%DBH          = self%DBH      + dbh            * cc%nindivs
      self%nindivs      = self%nindivs  + cc%nindivs

      if (dbh > 0.12) then
        self%DBH12      = self%DBH12     + dbh           * cc%nindivs
        self%nindivs12  = self%nindivs12 + cc%nindivs
        self%DBH12pow2  = self%DBH12pow2 + dbh ** 2      * cc%nindivs
      endif

      self%MaxAge    = MAX(cc%age, self%MaxAge)
      self%MaxVolume = MAX(cc%volume(), self%MaxVolume)
      self%MaxDBH    = MAX(dbh, self%MaxDBH)

      it => it%next()

    enddo

    if (self%nindivs>0.0)   self%DBH   = self%DBH / self%nindivs
    if (self%nindivs12>0.0) self%DBH12 = self%DBH12 / self%nindivs12  ! self%nindivs12 could be zero if all dbh<0.12
    if (self%nindivs12>0.0) then
      self%QMD12   = sqrt(self%DBH12pow2 / self%nindivs12)
    else
      self%QMD12 = 0.0
    end if

  end subroutine summarize_tile


  subroutine hourly_diagnostics(self, forcing)
    !////////////////////////////////////////////////////////////////////////
    ! Updates sub-daily tile-level variables and takes running daily sums
    !------------------------------------------------------------------------
    use md_forcing_biomee, only: climate_type
    use md_interface_biomee, only: myinterface

    class(vegn_tile_type), intent(inout) :: self
    type(climate_type), intent(in):: forcing

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it

    self%age = self%age + myinterface%dt_fast_yr

    ! Tile summary
    self%transp   = 0.0
    self%GPP      = 0.0
    self%NPP      = 0.0
    self%Resp     = 0.0
    self%N_uptake = 0.0
    self%fixedN   = 0.0

    it => self%cohorts()
    do while (associated(it))
      cc => it%cohort

      ! cohort daily
      call update_fluxes(cc%daily_fluxes, cc%fast_fluxes)

      ! Tile hourly
      self%transp   = self%transp   + cc%fast_fluxes%trsp     * cc%nindivs
      self%GPP      = self%GPP      + cc%fast_fluxes%gpp      * cc%nindivs
      self%NPP      = self%NPP      + cc%fast_fluxes%Npp      * cc%nindivs
      self%Resp     = self%Resp     + cc%fast_fluxes%Resp     * cc%nindivs
      self%N_uptake = self%N_uptake + cc%fast_fluxes%Nup      * cc%nindivs
      self%fixedN   = self%fixedN   + cc%fast_fluxes%fixedN   * cc%nindivs

      ! Reset fast fluxes
      cc%fast_fluxes = common_fluxes()

      it => it%next()
    enddo

    ! NEP is equal to NNP minus soil respiration
    self%nep = self%npp - self%rh

    ! Daily summary:
    self%dailyNup    = self%dailyNup     + self%N_uptake
    self%dailyfixedN = self%dailyfixedN  + self%fixedN
    self%dailyGPP    = self%dailyGPP     + self%gpp
    self%dailyNPP    = self%dailyNPP     + self%npp
    self%dailyResp   = self%dailyResp    + self%resp
    self%dailyRh     = self%dailyRh      + self%rh
    self%dailyTrsp   = self%dailyTrsp    + self%transp
    self%dailyEvap   = self%dailyEvap    + self%evap
    self%dailyRoff   = self%dailyRoff    + self%runoff
    self%dailyPrcp   = self%dailyPrcp    + forcing%rain * myinterface%step_seconds

  end subroutine hourly_diagnostics


  subroutine daily_diagnostics(self, iyears, idoy, state, out_daily_tile )
    !////////////////////////////////////////////////////////////////////////
    ! Updates daily tile-level variables and takes running annual sums
    !------------------------------------------------------------------------
    use md_forcing_biomee, only: climate_type
    use md_interface_biomee, only: outtype_daily_tile

    class(vegn_tile_type), intent(inout) :: self
    integer, intent(in) :: iyears, idoy
    type(outtype_steering), intent(in) :: state
    type(outtype_daily_tile), intent(out) :: out_daily_tile

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it

    it => self%cohorts()
    do while (associated(it))
      cc => it%cohort

      ! running annual sum
      call update_fluxes(cc%annual_fluxes, cc%daily_fluxes)

      ! Reset daily variables
      cc%daily_fluxes = common_fluxes()

      it => it%next()
    enddo

    ! Tile level, daily
    call self%summarize_tile()

    if (.not. state%spinup) then
      out_daily_tile%year      = iyears
      out_daily_tile%doy       = idoy
      out_daily_tile%Tc        = self%tc_daily
      out_daily_tile%Prcp      = self%dailyPrcp
      out_daily_tile%totWs     = self%soilwater
      out_daily_tile%Trsp      = self%dailyTrsp
      out_daily_tile%Evap      = self%dailyEvap
      out_daily_tile%Runoff    = self%dailyRoff
      out_daily_tile%ws1       = self%wcl(1)*thksl(1) * 1000
      out_daily_tile%ws2       = self%wcl(2)*thksl(2) * 1000
      out_daily_tile%ws3       = self%wcl(3)*thksl(3) * 1000
      out_daily_tile%LAI       = self%LAI
      out_daily_tile%GPP       = self%dailyGPP
      out_daily_tile%Rauto     = self%dailyResp
      out_daily_tile%Rh        = self%dailyRh
      out_daily_tile%NSC       = self%plabl%c%c12
      out_daily_tile%seedC     = self%pseed%c%c12
      out_daily_tile%leafC     = self%pleaf%c%c12
      out_daily_tile%rootC     = self%proot%c%c12
      out_daily_tile%SW_C      = self%psapw%c%c12
      out_daily_tile%HW_C      = self%pwood%c%c12
      out_daily_tile%NSN       = self%plabl%n%n14
      out_daily_tile%seedN     = self%pseed%n%n14
      out_daily_tile%leafN     = self%pleaf%n%n14
      out_daily_tile%rootN     = self%proot%n%n14
      out_daily_tile%SW_N      = self%psapw%n%n14
      out_daily_tile%HW_N      = self%pwood%n%n14
      out_daily_tile%McrbC     = self%pmicr%c%c12
      out_daily_tile%fastSOM   = self%psoil_fs%c%c12
      out_daily_tile%slowSOM   = self%psoil_sl%c%c12
      out_daily_tile%McrbN     = self%pmicr%n%n14
      out_daily_tile%fastSoilN = self%psoil_fs%n%n14
      out_daily_tile%slowSoilN = self%psoil_sl%n%n14
      out_daily_tile%mineralN  = self%ninorg%n14
      out_daily_tile%N_uptk    = self%dailyNup
    endif

    ! running annual sums
    self%annualNup  = self%annualNup  + self%dailyNup
    self%annualGPP  = self%annualGPP  + self%dailygpp
    self%annualNPP  = self%annualNPP  + self%dailynpp
    self%annualResp = self%annualResp + self%dailyresp
    self%annualRh   = self%annualRh   + self%dailyrh
    self%annualPrcp = self%annualPrcp + self%dailyPrcp
    self%annualTrsp = self%annualTrsp + self%dailytrsp
    self%annualEvap = self%annualEvap + self%dailyevap
    self%annualRoff = self%annualRoff + self%dailyRoff

    ! zero:
    call self%zero_daily_diagnostics()

  end subroutine daily_diagnostics


  subroutine annual_diagnostics(self, iyears, out_annual_cohorts, out_annual_tile)
    !////////////////////////////////////////////////////////////////////////
    ! Updates tile-level variables and populates annual output in once
    !------------------------------------------------------------------------
    use md_interface_biomee, only: outtype_annual_cohorts, outtype_annual_tile, myinterface

    class(vegn_tile_type), intent(inout) :: self
    integer, intent(in) :: iyears
    type(outtype_annual_cohorts), dimension(out_max_cohorts) :: out_annual_cohorts
    type(outtype_annual_tile) :: out_annual_tile

    ! local variables
    real :: treeG, fseed, fleaf, froot, fwood, dDBH, BA, dBA
    real :: plantC, plantN, soilC, soilN
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it
    integer :: i

    i = 0

    ! Cohorts ouput
    it => self%cohorts()
    do while (associated(it))
      cc => it%cohort

      i = i + 1

      treeG     = cc%pseed%c%c12 + cc%NPPleaf + cc%NPProot + cc%NPPwood
      fseed     = cc%pseed%c%c12 / treeG
      fleaf     = cc%NPPleaf / treeG
      froot     = cc%NPProot / treeG
      fwood     = cc%NPPwood / treeG
      dDBH      = cc%dbh() - cc%DBH_ys !in m
      BA        = cc%basal_area()
      dBA       = BA - cc%BA_ys

      if (i <= NCohortMax) then

        out_annual_cohorts(i)%year        = iyears
        out_annual_cohorts(i)%cID         = it%uid()
        out_annual_cohorts(i)%PFT         = cc%species
        out_annual_cohorts(i)%layer       = cc%layer
        out_annual_cohorts(i)%density     = cc%nindivs * 10000
        out_annual_cohorts(i)%flayer      = cc%layerfrac()
        out_annual_cohorts(i)%dbh         = cc%dbh() * 100   ! *100 to convert m in cm
        out_annual_cohorts(i)%dDBH        = dDBH * 100     ! *100 to convert m in cm
        out_annual_cohorts(i)%height      = cc%height()
        out_annual_cohorts(i)%age         = cc%age
        out_annual_cohorts(i)%BA          = BA
        out_annual_cohorts(i)%dBA         = dBA
        out_annual_cohorts(i)%Acrown      = cc%crownarea()
        out_annual_cohorts(i)%Aleaf       = cc%leafarea()
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
        out_annual_cohorts(i)%GPP         = cc%annual_fluxes%GPP
        out_annual_cohorts(i)%NPP         = cc%annual_fluxes%NPP
        out_annual_cohorts(i)%Rauto       = cc%annual_fluxes%Resp
        out_annual_cohorts(i)%Nupt        = cc%annual_fluxes%Nup
        out_annual_cohorts(i)%Nfix        = cc%annual_fluxes%fixedN

      end if

      it => it%next()

    enddo

    ! tile pools output
    call summarize_tile( self )

    it => self%cohorts()
    do while (associated(it))

      cc => it%cohort

      self%annualfixedN = self%annualfixedN  + cc%annual_fluxes%fixedN * cc%nindivs
      self%NPPL         = self%NPPL          + cc%NPPleaf    * cc%nindivs
      self%NPPW         = self%NPPW          + cc%NPPwood    * cc%nindivs
      self%m_turnover   = self%m_turnover    + cc%m_turnover * cc%nindivs

      it => it%next()

    enddo

    plantC    = self%plabl%c%c12 + self%pseed%c%c12 + self%pleaf%c%c12 + self%proot%c%c12 + self%psapw%c%c12 + self%pwood%c%c12
    plantN    = self%plabl%n%n14 + self%pseed%n%n14 + self%pleaf%n%n14 + self%proot%n%n14 + self%psapw%n%n14 + self%pwood%n%n14

    soilC     = self%pmicr%c%c12 + self%psoil_fs%c%c12 + self%psoil_sl%c%c12
    soilN     = self%pmicr%n%n14 + self%psoil_fs%n%n14 + self%psoil_sl%n%n14 + self%ninorg%n14
    self%totN = plantN + soilN

    out_annual_tile%year            = iyears
    out_annual_tile%CAI             = self%CAI
    out_annual_tile%LAI             = self%LAI
    out_annual_tile%density         = self%nindivs * 10000   ! * 10000 to convert in indivs/ha
    out_annual_tile%DBH             = self%DBH * 100         ! * 100 to convert in cm
    out_annual_tile%density12       = self%nindivs12 * 10000 ! * 10000 to convert in indivs/ha
    out_annual_tile%DBH12           = self%DBH12 * 100       ! * 100 to convert in cm
    out_annual_tile%QMD12           = self%QMD12 * 100       ! * 100 to convert in cm
    out_annual_tile%NPP             = self%annualNPP
    out_annual_tile%GPP             = self%annualGPP
    out_annual_tile%Rauto           = self%annualResp
    out_annual_tile%Rh              = self%annualRh
    out_annual_tile%rain            = self%annualPrcp
    out_annual_tile%SoilWater       = self%SoilWater
    out_annual_tile%Transp          = self%annualTrsp
    out_annual_tile%Evap            = self%annualEvap
    out_annual_tile%Runoff          = self%annualRoff
    out_annual_tile%plantC          = plantC ! kg C/m2/yr
    out_annual_tile%soilC           = soilC
    out_annual_tile%plantN          = plantN
    out_annual_tile%soilN           = soilN
    out_annual_tile%totN            = self%totN
    out_annual_tile%NSC             = self%plabl%c%c12
    out_annual_tile%SeedC           = self%pseed%c%c12
    out_annual_tile%leafC           = self%pleaf%c%c12
    out_annual_tile%rootC           = self%proot%c%c12
    out_annual_tile%SapwoodC        = self%psapw%c%c12
    out_annual_tile%WoodC           = self%pwood%c%c12
    out_annual_tile%NSN             = self%plabl%n%n14
    out_annual_tile%SeedN           = self%pseed%n%n14
    out_annual_tile%leafN           = self%pleaf%n%n14
    out_annual_tile%rootN           = self%proot%n%n14
    out_annual_tile%SapwoodN        = self%psapw%n%n14
    out_annual_tile%WoodN           = self%pwood%n%n14
    out_annual_tile%McrbC           = self%pmicr%c%c12
    out_annual_tile%fastSOM         = self%psoil_fs%c%c12
    out_annual_tile%SlowSOM         = self%psoil_sl%c%c12
    out_annual_tile%McrbN           = self%pmicr%n%n14
    out_annual_tile%fastSoilN       = self%psoil_fs%n%n14
    out_annual_tile%slowSoilN       = self%psoil_sl%n%n14
    out_annual_tile%mineralN        = self%ninorg%n14
    out_annual_tile%N_fxed          = self%annualfixedN
    out_annual_tile%N_uptk          = self%annualNup
    out_annual_tile%N_yrMin         = self%annualN
    out_annual_tile%N_P2S           = self%N_P2S_yr
    out_annual_tile%N_loss          = self%Nloss_yr
    out_annual_tile%totseedC        = self%totseedC
    out_annual_tile%totseedN        = self%totseedN
    out_annual_tile%Seedling_C      = self%totNewCC
    out_annual_tile%Seedling_N      = self%totNewCN
    out_annual_tile%MaxAge          = self%MaxAge
    out_annual_tile%MaxVolume       = self%MaxVolume
    out_annual_tile%MaxDBH          = self%MaxDBH
    out_annual_tile%NPPL            = self%NPPL
    out_annual_tile%NPPW            = self%NPPW
    out_annual_tile%n_deadtrees     = self%n_deadtrees
    out_annual_tile%c_deadtrees     = self%c_deadtrees
    out_annual_tile%m_turnover      = self%m_turnover
    out_annual_tile%c_turnover_time = self%pwood%c%c12 / self%NPPW

    ! Rebalance N (to compensate for the adjunction in vegn_N_uptake)
    if (myinterface%params_siml%do_closedN_run) call Recover_N_balance(self)

  end subroutine annual_diagnostics

  subroutine plant2soil(self, lossC_coarse, lossC_fine, lossN_coarse, lossN_fine)
    ! Redistribute C and N from dead plants to vegetation soil pools
    class(vegn_tile_type), intent(inout) :: self
    real, intent(in) :: lossC_coarse, lossC_fine, lossN_coarse, lossN_fine

      self%psoil_fs%c%c12 = self%psoil_fs%c%c12 + myinterface%params_tile%fsc_fine * lossC_fine + &
              myinterface%params_tile%fsc_wood * lossC_coarse
      self%psoil_sl%c%c12 = self%psoil_sl%c%c12 + (1.0 - myinterface%params_tile%fsc_fine) * lossC_fine + &
              (1.0-myinterface%params_tile%fsc_wood) * lossC_coarse

      self%psoil_fs%n%n14 = self%psoil_fs%n%n14 + myinterface%params_tile%fsc_fine * lossN_fine + &
              myinterface%params_tile%fsc_wood * lossN_coarse
      self%psoil_sl%n%n14 = self%psoil_sl%n%n14 + (1.0 - myinterface%params_tile%fsc_fine) * lossN_fine + &
              (1.0-myinterface%params_tile%fsc_wood) * lossN_coarse

      ! annual N from plants to soil and C turnover

      self%N_P2S_yr = self%N_P2S_yr + lossN_coarse + lossN_fine

  end subroutine

  subroutine annual_diagnostics_post_mortality(self, out_annual_cohorts, out_annual_tile)
    !////////////////////////////////////////////////////////////////////////
    ! Updates tile-level variables and populates annual output in once
    !------------------------------------------------------------------------
    use md_interface_biomee, only: outtype_annual_cohorts, outtype_annual_tile

    class(vegn_tile_type), intent(inout) :: self
    type(outtype_annual_cohorts), dimension(out_max_cohorts) :: out_annual_cohorts
    type(outtype_annual_tile) :: out_annual_tile

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it
    integer :: i
    real :: lossC_fine,lossC_coarse, lossC_total
    real :: lossN_fine,lossN_coarse, lossN_total

    ! We go through each killed fraction of cohort
    it => self%killed_cohort_fractions()
    do while (associated(it))
      cc => it%cohort
      do i = 1, NCohortMax
        if (int(out_annual_cohorts(i)%cID) == it%uid()) then
          associate (sp => cc%sp())

            ! Carbon and Nitrogen from plants to soil pools
            lossC_coarse = cc%nindivs * &
              (cc%pwood%c%c12 + cc%psapw%c%c12 + cc%pleaf%c%c12 - cc%leafarea() * myinterface%params_tile%LMAmin)
            lossC_fine   = cc%nindivs * &
              (cc%plabl%c%c12 + cc%pseed%c%c12 + cc%proot%c%c12 + cc%leafarea() * myinterface%params_tile%LMAmin)

            lossN_coarse = cc%nindivs * (cc%pwood%n%n14 + cc%psapw%n%n14 + cc%pleaf%n%n14 - cc%leafarea()*sp%LNbase)
            lossN_fine   = cc%nindivs * (cc%plabl%n%n14 + cc%pseed%n%n14 + cc%proot%n%n14 + cc%leafarea()*sp%LNbase)
          end associate

          call self%plant2soil(lossC_coarse, lossC_fine, lossN_coarse, lossN_fine)

          lossC_total = lossC_coarse + lossC_fine
          lossN_total = lossN_coarse + lossN_fine

          self%m_turnover   = self%m_turnover    + lossC_total ! We add C from dead trees to get the total turnover
          self%c_deadtrees  = self%c_deadtrees   + lossC_total
          self%n_deadtrees  = self%n_deadtrees   + lossN_total

          out_annual_cohorts(i)%n_deadtrees = out_annual_cohorts(i)%n_deadtrees + lossN_total
          out_annual_cohorts(i)%c_deadtrees = out_annual_cohorts(i)%c_deadtrees + lossC_total
          out_annual_cohorts(i)%deathrate   = out_annual_cohorts(i)%deathrate + lossC_total
          exit
        end if
      end do

      it => it%next()
    enddo

    out_annual_tile%N_P2S           = self%N_P2S_yr
    out_annual_tile%n_deadtrees     = self%n_deadtrees
    out_annual_tile%c_deadtrees     = self%c_deadtrees
    out_annual_tile%m_turnover      = self%m_turnover

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

  subroutine initialize_PFT_data()

    ! ---- local vars ------
    integer :: i

    associate(spdata => myinterface%params_species)

      spdata%prob_g        = 1.0
      spdata%prob_e        = 1.0

      spdata%LAImax = MAX(0.5, spdata%LAI_light)
      spdata%underLAImax = MIN(spdata%LAImax, 1.2)

      ! specific root area
      spdata%SRA           = 2.0/(spdata%root_r*spdata%rho_FR)

      ! calculate alphaBM parameter of allometry. note that rho_wood was re-introduced for this calculation
      spdata%alphaBM = spdata%rho_wood * spdata%taperfactor * PI/4. * spdata%alphaHT ! 5200

      ! Vmax as a function of LNbase
      spdata%Vmax = 0.02 * spdata%LNbase ! 0.03125 * sp%LNbase ! Vmax/LNbase= 25E-6/0.8E-3 = 0.03125 !

      ! CN0 of leaves
      spdata%LNA     = spdata%LNbase +  spdata%LMA/spdata%CNleafsupport
      spdata%CNleaf0 = spdata%LMA/spdata%LNA
      ! Leaf life span as a function of LMA
      spdata%leafLS = c_LLS * spdata%LMA

      do i = 1, size(spdata)
        call init_derived_species_data(spdata(i))
      enddo

    end associate

  end subroutine initialize_pft_data


  subroutine init_derived_species_data(sp)

    type(spec_data_type), intent(inout) :: sp

    ! ---- local vars ------
    integer :: j
    real :: rdepth(0:MAX_LEVELS)
    real :: residual

    ! root vertical profile
    rdepth=0.0
    do j=1,MAX_LEVELS
      rdepth(j) = rdepth(j-1)+thksl(j)
      sp%root_frac(j) = exp(-rdepth(j-1)/sp%root_zeta)- &
              exp(-rdepth(j)  /sp%root_zeta)
    enddo
    residual = exp(-rdepth(MAX_LEVELS)/sp%root_zeta)
    do j=1,MAX_LEVELS
      sp%root_frac(j) = sp%root_frac(j) + residual*thksl(j)/rdepth(MAX_LEVELS)
    enddo

    if(sp%leafLS>1.0)then
      sp%phenotype = 1
    else
      sp%phenotype = 0
    endif

    ! Leaf turnover rate, (leaf longevity as a function of LMA)
    sp%alpha_L = 1.0/sp%leafLS * sp%phenotype

  end subroutine init_derived_species_data

end module datatypes_biomee
