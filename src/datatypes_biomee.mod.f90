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
  public :: Zero_diagnostics, hourly_diagnostics, daily_diagnostics, &
            annual_diagnostics, initialize_PFT_data

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

  !===== deathrate = mortrate_d_u * (1+A*exp(B*DBH))/(1+exp(B*DBH))
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

    !===== Cohort heap
    ! Implemented as a linked list, 'heap' points to the first cohort of the heap
    ! Cohorts should not assumed to be ranked in any specific order.
    ! Use sort_cohorts_by_height() or implement a method following the same principle if needed.
    type(cohort_item), pointer :: heap => null() ! Important to nullify here!
    ! Heap of killed cohort for diagnostics purpose. It is empied at the end of each year.
    type(cohort_item), pointer :: heap_killed => null() ! Important to nullify here!

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
    real    :: NPPL               = dummy         ! NPP leaf, kg C m-2 yr-1
    real    :: NPPW               = dummy         ! NPP wood, kg C m-2 yr-1

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
    procedure new_cohort
    procedure sort_cohorts_by_height
    procedure sort_cohorts_by_uid
    procedure shut_down
    procedure kill_fraction
    procedure kill_cohort
    procedure merge_cohorts
    procedure split
    procedure, private :: plant2soil

  end type vegn_tile_type

contains

  subroutine plant2soil(self, cc, deadtrees)
    !////////////////////////////////////////////////////////////////
    ! Transfer of deat biomass to litter pools
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_type),    intent(inout) :: cc
    real,                 intent(in)    :: deadtrees ! dead trees/m2

    ! local variables --------
    real :: lossC_fine,lossC_coarse
    real :: lossN_fine,lossN_coarse

    associate (sp => cc%sp())

      ! Carbon and Nitrogen from plants to soil pools
      lossC_coarse  = deadtrees * &
              (cc%pwood%c%c12 + cc%psapw%c%c12 + cc%pleaf%c%c12 - cc%leafarea() * myinterface%params_tile%LMAmin)
      lossC_fine    = deadtrees * &
              (cc%plabl%c%c12 + cc%pseed%c%c12 + cc%proot%c%c12 + cc%leafarea() * myinterface%params_tile%LMAmin)

      lossN_coarse = deadtrees * (cc%pwood%n%n14 + cc%psapw%n%n14 + cc%pleaf%n%n14 - cc%leafarea()*sp%LNbase)
      lossN_fine   = deadtrees * (cc%plabl%n%n14 + cc%pseed%n%n14 + cc%proot%n%n14 + cc%leafarea()*sp%LNbase)

      self%psoil_fs%c%c12 = self%psoil_fs%c%c12 + myinterface%params_tile%fsc_fine * lossC_fine + &
              myinterface%params_tile%fsc_wood * lossC_coarse
      self%psoil_sl%c%c12 = self%psoil_sl%c%c12 + (1.0 - myinterface%params_tile%fsc_fine) * lossC_fine + &
              (1.0-myinterface%params_tile%fsc_wood) * lossC_coarse

      self%psoil_fs%n%n14 = self%psoil_fs%n%n14 + myinterface%params_tile%fsc_fine * lossN_fine + &
              myinterface%params_tile%fsc_wood * lossN_coarse
      self%psoil_sl%n%n14 = self%psoil_sl%n%n14 + (1.0 - myinterface%params_tile%fsc_fine) * lossN_fine + &
              (1.0-myinterface%params_tile%fsc_wood) * lossN_coarse

      ! annual N from plants to soil
      self%N_P2S_yr = self%N_P2S_yr + lossN_fine + lossN_coarse

      ! record mortality
      ! cohort level
      cc%n_deadtrees = lossN_coarse + lossN_fine
      cc%c_deadtrees = lossC_coarse + lossC_fine
      cc%m_turnover  = cc%m_turnover + cc%c_deadtrees

    end associate

  end subroutine plant2soil

  subroutine split(self, item, fraction)
    class(vegn_tile_type) :: self
    type(cohort_item), pointer :: item, new
    real :: fraction

    new => self%new_cohort()
    new%cohort = item%cohort
    new%cohort%nindivs = item%cohort%nindivs * fraction
    item%cohort%nindivs = item%cohort%nindivs - new%cohort%nindivs
  end subroutine split

  function merge_cohorts(self, c1, c2) result(next_item)
    ! Merge cohort c2 into c1 and return item following c2
    class(vegn_tile_type) :: self
    type(cohort_item), pointer :: c1, c2
    type(cohort_item), pointer :: next_item


    call c1%cohort%merge_in(c2%cohort)
    next_item => destroy_cohort(c2, self%heap)
  end function merge_cohorts

  function kill_fraction(self, item, deathrate) result(next_item)
    ! Kill fraction 'fraction' of cohort of uid 'uid'
    ! This creates a new cohort in 'heap_killed' with nindivis = cohort%nindivis * fraction
    ! And the original cohort is the complement: nindivis = cohort%nindivis * (1-fraction)
    ! If fraction is 0, nothing happens.
    ! If fraction is 1, the cohort is moved from heap to heap_killed.
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer, intent(inout) :: item
    real, intent(in) :: deathrate
    type(cohort_item), pointer :: next_item

    ! Local variables
    type(cohort_item), pointer :: killed

    if (deathrate <= 0.0) then
      next_item => null()
    elseif (deathrate >= 1.0) then
      next_item => self%kill_cohort(item)
    else

      killed => self%new_cohort(.true.)
      killed%cohort = item%cohort
      killed%cohort%nindivs = item%cohort%nindivs * deathrate
      item%cohort%nindivs = item%cohort%nindivs - killed%cohort%nindivs
      item%cohort%deathrate = deathrate ! deathrate having affected this cohort
      killed%cohort%nindivs = deathrate ! deathrate having spawned this killed_cohort
      next_item => item%next()

    end if
  end function kill_fraction

  subroutine shut_down(self)
    ! Free all allocated memory
    class(vegn_tile_type) :: self

    call destroy_all(self%heap)
    call destroy_all(self%heap_killed)
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

    call sort_cohorts(self%heap, increasing, get_height)

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

    call sort_cohorts(self%heap, increasing, get_uid)

  end subroutine sort_cohorts_by_uid

  function n_cohorts(self) result(res)
    ! Returns the current number of cohorts
    integer :: res
    class(vegn_tile_type) :: self

    res = length(self%heap)
  end function n_cohorts

  function new_cohort(self, killed) result(new_item)
    ! Insert a new cohort at the head of the list and return its pointer.
    ! If 'killed' is set, the cohort is inserted in 'heap_killed' rather than 'heap'.
    type(cohort_item), pointer :: new_item
    class(vegn_tile_type) :: self
    logical, optional :: killed
    logical :: killed_opt

    killed_opt = .false.

    if(present(killed)) killed_opt = killed

    new_item => create_cohort()
    if (killed_opt) then
      call insert_head(new_item, self%heap_killed)
    else
      call insert_head(new_item, self%heap)
    end if

  end function new_cohort

  function kill_cohort(self, item) result(next_item)
    ! Move item to heap_killed and return pointer to next
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer, intent(inout) :: item
    type(cohort_item), pointer :: next_item

    item%cohort%deathrate = 1.0
    next_item => detach_cohort(item, self%heap)
    call insert_head(item, self%heap_killed)
  end function kill_cohort

  !==============for diagnostics============================================
  subroutine Zero_diagnostics(vegn)

    ! for annual update
    type(vegn_tile_type), intent(inout) :: vegn

    ! local variables
    type(cohort_item), pointer :: it !iterator

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

    it => vegn%heap
    do while (associated(it))
      call it%cohort%reset_cohort()
      it => it%next()
    end do
  
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
    type(cohort_item), pointer :: it !iterator
    real :: dbh ! cache variable

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

    it => vegn%heap
    do while (associated(it))
      cc => it%cohort

      ! organic pools
      call orgcp(cc%plabl, vegn%plabl, cc%nindivs)
      call orgcp(cc%pleaf, vegn%pleaf, cc%nindivs)
      call orgcp(cc%proot, vegn%proot, cc%nindivs)
      call orgcp(cc%psapw, vegn%psapw, cc%nindivs)
      call orgcp(cc%pwood, vegn%pwood, cc%nindivs)
      call orgcp(cc%pseed, vegn%pseed, cc%nindivs)

      vegn%CAI          = vegn%CAI      + cc%crownarea() * cc%nindivs
      vegn%LAI          = vegn%LAI      + cc%leafarea()  * cc%nindivs
      
      ! New tile outputs
      dbh = cc%dbh()
      vegn%DBH          = vegn%DBH      + dbh            * cc%nindivs
      vegn%nindivs      = vegn%nindivs  + cc%nindivs

      if (dbh > 0.12) then
        vegn%DBH12      = vegn%DBH12     + dbh           * cc%nindivs
        vegn%nindivs12  = vegn%nindivs12 + cc%nindivs
        vegn%DBH12pow2  = vegn%DBH12pow2 + dbh ** 2      * cc%nindivs
      endif

      vegn%MaxAge    = MAX(cc%age, vegn%MaxAge)
      vegn%MaxVolume = MAX(cc%volume(), vegn%MaxVolume)
      vegn%MaxDBH    = MAX(dbh, vegn%MaxDBH)

      it => it%next()

    enddo

    if (vegn%nindivs>0.0)   vegn%DBH   = vegn%DBH / vegn%nindivs  
    if (vegn%nindivs12>0.0) vegn%DBH12 = vegn%DBH12 / vegn%nindivs12  ! vegn%nindivs12 could be zero if all dbh<0.12
    if (vegn%nindivs12>0.0) then
      vegn%QMD12   = sqrt(vegn%DBH12pow2 / vegn%nindivs12)
    else
      vegn%QMD12 = 0.0
    end if

  end subroutine summarize_tile


  subroutine hourly_diagnostics(vegn, forcing)
    !////////////////////////////////////////////////////////////////////////
    ! Updates sub-daily tile-level variables and takes running daily sums
    !------------------------------------------------------------------------
    use md_forcing_biomee, only: climate_type
    use md_interface_biomee, only: myinterface

    type(vegn_tile_type), intent(inout) :: vegn
    type(climate_type), intent(in):: forcing

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it

    vegn%age = vegn%age + myinterface%dt_fast_yr

    ! Tile summary
    vegn%transp   = 0.0
    vegn%GPP      = 0.0
    vegn%NPP      = 0.0
    vegn%Resp     = 0.0
    vegn%N_uptake = 0.0
    vegn%fixedN   = 0.0

    it => vegn%heap
    do while (associated(it))
      cc => it%cohort

      ! cohort daily
      call update_fluxes(cc%daily_fluxes, cc%fast_fluxes)

      ! Tile hourly
      vegn%transp   = vegn%transp   + cc%fast_fluxes%trsp     * cc%nindivs
      vegn%GPP      = vegn%GPP      + cc%fast_fluxes%gpp      * cc%nindivs
      vegn%NPP      = vegn%NPP      + cc%fast_fluxes%Npp      * cc%nindivs
      vegn%Resp     = vegn%Resp     + cc%fast_fluxes%Resp     * cc%nindivs
      vegn%N_uptake = vegn%N_uptake + cc%fast_fluxes%Nup      * cc%nindivs
      vegn%fixedN   = vegn%fixedN   + cc%fast_fluxes%fixedN   * cc%nindivs

      ! Reset fast fluxes
      cc%fast_fluxes = common_fluxes()

      it => it%next()
    enddo

    ! NEP is equal to NNP minus soil respiration
    vegn%nep = vegn%npp - vegn%rh

    ! Daily summary:
    vegn%dailyNup    = vegn%dailyNup     + vegn%N_uptake
    vegn%dailyfixedN = vegn%dailyfixedN  + vegn%fixedN
    vegn%dailyGPP    = vegn%dailyGPP     + vegn%gpp
    vegn%dailyNPP    = vegn%dailyNPP     + vegn%npp
    vegn%dailyResp   = vegn%dailyResp    + vegn%resp
    vegn%dailyRh     = vegn%dailyRh      + vegn%rh
    vegn%dailyTrsp   = vegn%dailyTrsp    + vegn%transp
    vegn%dailyEvap   = vegn%dailyEvap    + vegn%evap
    vegn%dailyRoff   = vegn%dailyRoff    + vegn%runoff
    vegn%dailyPrcp   = vegn%dailyPrcp    + forcing%rain * myinterface%step_seconds

  end subroutine hourly_diagnostics


  subroutine daily_diagnostics( vegn , iyears, idoy, state, out_daily_tile )
    !////////////////////////////////////////////////////////////////////////
    ! Updates daily tile-level variables and takes running annual sums
    !------------------------------------------------------------------------
    use md_forcing_biomee, only: climate_type
    use md_interface_biomee, only: outtype_daily_tile

    type(vegn_tile_type), intent(inout) :: vegn
    integer, intent(in) :: iyears, idoy
    type(outtype_steering), intent(in) :: state
    type(outtype_daily_tile), intent(out) :: out_daily_tile

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it

    it => vegn%heap
    do while (associated(it))
      cc => it%cohort

      ! running annual sum
      call update_fluxes(cc%annual_fluxes, cc%daily_fluxes)

      ! Reset daily variables
      cc%daily_fluxes = common_fluxes()

      it => it%next()
    enddo

    ! Tile level, daily
    call summarize_tile(vegn)

    if (.not. state%spinup) then
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

    ! local variables
    real :: treeG, fseed, fleaf, froot, fwood, dDBH, BA, dBA
    real :: plantC, plantN, soilC, soilN
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it
    integer :: i

    i = 0

    ! Cohorts ouput
    it => vegn%heap
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
    call summarize_tile( vegn )
 
    vegn%NPPL        = 0.0
    vegn%NPPW        = 0.0
    vegn%n_deadtrees = 0
    vegn%c_deadtrees = 0
    vegn%m_turnover  = 0

    it => vegn%heap
    do while (associated(it))

      cc => it%cohort

      vegn%annualfixedN = vegn%annualfixedN  + cc%annual_fluxes%fixedN * cc%nindivs
      vegn%NPPL         = vegn%NPPL          + cc%NPPleaf * cc%nindivs
      vegn%NPPW         = vegn%NPPW          + cc%NPPwood * cc%nindivs

      it => it%next()

    enddo

    plantC    = vegn%plabl%c%c12 + vegn%pseed%c%c12 + vegn%pleaf%c%c12 + vegn%proot%c%c12 + vegn%psapw%c%c12 + vegn%pwood%c%c12
    plantN    = vegn%plabl%n%n14 + vegn%pseed%n%n14 + vegn%pleaf%n%n14 + vegn%proot%n%n14 + vegn%psapw%n%n14 + vegn%pwood%n%n14

    soilC     = vegn%pmicr%c%c12 + vegn%psoil_fs%c%c12 + vegn%psoil_sl%c%c12
    soilN     = vegn%pmicr%n%n14 + vegn%psoil_fs%n%n14 + vegn%psoil_sl%n%n14 + vegn%ninorg%n14
    vegn%totN = plantN + soilN

    out_annual_tile%year            = iyears
    out_annual_tile%CAI             = vegn%CAI
    out_annual_tile%LAI             = vegn%LAI
    out_annual_tile%density         = vegn%nindivs * 10000   ! * 10000 to convert in indivs/ha
    out_annual_tile%DBH             = vegn%DBH * 100         ! * 100 to convert in cm
    out_annual_tile%density12       = vegn%nindivs12 * 10000 ! * 10000 to convert in indivs/ha
    out_annual_tile%DBH12           = vegn%DBH12 * 100       ! * 100 to convert in cm
    out_annual_tile%QMD12           = vegn%QMD12 * 100       ! * 100 to convert in cm
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
    out_annual_tile%totN            = vegn%totN
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
    type(cohort_item), pointer :: it
    integer :: i

    ! Cohorts ouput
    it => vegn%heap
    do while (associated(it))
      cc => it%cohort
      do i = 1, NCohortMax
        if (int(out_annual_cohorts(i)%cID) == it%uid()) then
          out_annual_cohorts(i)%n_deadtrees = cc%n_deadtrees
          out_annual_cohorts(i)%c_deadtrees = cc%c_deadtrees
          out_annual_cohorts(i)%deathrate   = cc%deathrate
          exit
        end if
      end do

      it => it%next()
    enddo

    vegn%n_deadtrees = 0
    vegn%c_deadtrees = 0
    vegn%m_turnover  = 0

    it => vegn%heap
    do while (associated(it))
      cc => it%cohort
      vegn%n_deadtrees  = vegn%n_deadtrees   + cc%n_deadtrees
      vegn%c_deadtrees  = vegn%c_deadtrees   + cc%c_deadtrees
      vegn%m_turnover   = vegn%m_turnover    + cc%m_turnover

      it => it%next()
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
