module vegetation_tile_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing BiomeE state variable and parameter 
  ! definitions.
  ! Code adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !----------------------------------------------------------------
  use md_interface_biomee, only: myinterface, spec_data_type, MAX_LEVELS, thksl
  use md_params_core
  use md_orgpool
  use md_common_fluxes
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

  !===== Soil SOM reference C/N ratios
  real, parameter :: CN0metabolicL                       = 15.0
  real, parameter :: CN0structuralL                      = 40.0

  !===== fraction = mortrate_d_u * (1+A*exp(B*DBH))/(1+exp(B*DBH))
  real, parameter  :: A_mort     = 9.0    ! A coefficient in understory mortality rate correction, 1/year
  real, parameter  :: B_mort     = -60.0  ! B coefficient in understory mortality rate correction, 1/m

  !===== Ensheng's growth parameters
  real, parameter  :: f_LFR_max  = 0.85    ! max allocation to leaves and fine roots

  !===== Minimum cohort density
  real, public, parameter :: mindensity = 0.25E-4 ! Minimum cohort density

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

    !===== Metadata
    real, private    :: age                = 0.0           ! tile age

    !========================= Cohort aggreation ===========================!
    ! Attention: variables aggregated from cohorts are only usable after having run aggregate_cohorts()
    real, private    :: nindivs                            ! density (tree/m2)
    real, private    :: LAI                                ! leaf area index (surface of leaves per m2 of ground/tile)
    real, private    :: CAI                                ! crown area index (surface of the projected crown mer m2 of ground/tile)
    real, private    :: DBH
    real, private    :: nindivs12
    real, private    :: DBH12
    real, private    :: QMD12
    real, private    :: MaxAge
    real, private    :: MaxVolume
    real, private    :: MaxDBH

    !===== Organic pools (cohort aggregation), kg m-2
    type(orgpool), private :: pleaf                       ! leaf biomass
    type(orgpool), private :: proot                       ! root biomass
    type(orgpool), private :: psapw                       ! sapwood biomass
    type(orgpool), private :: pwood                       ! heartwood (non-living) biomass
    type(orgpool), private :: pseed                       ! biomass put aside for future progeny
    type(orgpool), private :: plabl                       ! labile pool, temporary storage of N and C

    !===== C fluxes (cohort aggregation), kg yr-1 m-2
    real, private    :: NPPL                  ! NPP leaf
    real, private    :: NPPW                  ! NPP wood

    !=====  Soil carbon fluxes due to death (cohort aggregation), kg yr-1 m-2
    real, private    :: c_deadtrees           ! plant to soil C flux due to mortality
    real, private    :: n_deadtrees           ! plant to soil N flux due to mortality (kg N m-2 yr-1)
    real, private    :: m_turnover            ! C turnover due to mortality and tissue turnover

    !=====  Daily fluxes (cohort aggregation), kg day-1 m-2
    type(common_fluxes), private :: daily_fluxes

    !=====  Annual fluxes (cohort aggregation), kg yr-1 m-2
    type(common_fluxes), private :: annual_fluxes

    !========================= Tile level variables ===========================!
    !===== Soil organic pools, kg m-2
    ! renamed: metabolicL, metabolicN -> psoil_fs; structuralL, structuralN -> psoil_sl; MicrobialC, MicrobialN -> pmicr
    type(orgpool) :: psoil_fs        ! soil organic matter, fast turnover
    type(orgpool) :: psoil_sl        ! soil organic matter, slow turnover
    type(orgpool) :: pmicr           ! microbial biomass

    !===== Inorganic pools, kg m-2
    type(orgpool) :: inorg                      ! Mineral N/C pool

    !=====  Averaged quantities for PPA phenology
    ! Note, these fields do not follow the usual annual cycle (january-january)
    ! so they are not reset in zero_diagnostics
    real    :: tc_daily           = 0.0           ! 24h average temperature (deg C)
    real    :: gdd                = 0.0           ! growing degree-days
    real    :: tc_pheno           = 0.0           ! smoothed canopy air temperature for phenology
    real    :: tc_soil            = 0.0           ! Daily soil temperature in (deg C)

    !===== Annual C/N allocation to seed and non-seed, kg yr-1 m-2
    type(orgpool)    :: totSeed           ! Total seed
    type(orgpool)    :: totNewC           ! New cohort (all compartments but seed)

    !=====  N-related fluxes
    real    :: totN
    real    :: N_input            ! annual N input (kg N m-2 yr-1)
    real    :: annualN            ! annual available N in a year
    real    :: Nloss_yr           ! annual N loss
    real    :: N_P2S_yr           ! N turnover (plant to soil) (kg N m-2 yr-1)

    !=====  Memory
    real    :: previousN          ! weighted annual available N
    real    :: initialN0          ! initial available N (kg N m-2)

    !=====  Fast fluxes, kg m-2 timestep-1
    real    :: rh                                 ! soil carbon lost to the atmosphere
    !=====  Soil water, kg H2O m-2 timestep-1
    real    :: evap                               ! Evaporation
    real    :: runoff                             ! Water runoff of the tile
    real    :: precp                              ! Precipitation
    real    :: wcl(MAX_LEVELS)                    ! volumetric soil water content for each layer

    !=====  Daily fluxes, kg day-1 m-2
    real, private    :: dailyRh
    real, private    :: dailyPrcp
    real, private    :: dailyEvap
    real, private    :: dailyRoff

    !=====  Annual fluxes, kg yr-1 m-2
    real, private    :: annualRh
    real, private    :: annualPrcp
    real, private    :: annualEvap
    real, private    :: annualRoff

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
      procedure reduce
      procedure, private :: recover_N_balance
      procedure, private :: merge_cohorts
      procedure, private :: split_cohort
      procedure annual_diagnostics
      procedure annual_diagnostics_post_mortality
      procedure daily_diagnostics
      procedure hourly_diagnostics
      procedure zero_diagnostics
      procedure plant2soil
      procedure initialize_vegn_tile
      procedure :: relayer
      procedure, private :: aggregate_pools
      procedure, private :: aggregate_cohorts
      procedure, private :: zero_daily_diagnostics
      procedure, private :: kill_cohort

    !========= Derived variables
    ! Variables which are function of any state or temporary variable defined above.
    ! They can be used like any normal variable except that they are followed by parenthesis
    ! (indicating that they are being evaluated every time).
    ! The advantage is that they cannot be out-of-sync with the underlying data, at the cost of a negligeable
    ! increase in the number of operations.
    procedure thetaS
    procedure soilwater

  end type vegn_tile_type

contains

  pure real function soilwater(self)
    ! kg m-2 in root zone
    class(vegn_tile_type), intent(in) :: self

    soilwater  = SUM(self%wcl(:)*thksl(:)*1000.0)

  end function soilwater

  pure real function thetaS(self)
    ! moisture index (ws - wiltpt)/(fldcap - wiltpt)
    class(vegn_tile_type), intent(in) :: self

    thetaS  = (self%wcl(2) - myinterface%params_tile%WILTPT) &
            / (myinterface%params_tile%FLDCAP - myinterface%params_tile%WILTPT)

  end function thetaS

  subroutine split_cohort(self, item, fraction)
    ! Split a cohort into two. The initial cohort gets scaled by (1 - fraction),
    ! while a newly created cohort, inserted in the cohort list, gets the complement (fraction).
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer, intent(in) :: item
    real, intent(in) :: fraction

    ! Local variable
    type(cohort_item), pointer :: new

    new => item%clone()
    new%cohort%nindivs = item%cohort%nindivs * fraction
    call self%cohort_list%insert_item(new)
    item%cohort%nindivs = item%cohort%nindivs - new%cohort%nindivs
  end subroutine split_cohort

  function merge_cohorts(self, c1, c2) result(next_item)
    ! Merge cohort c2 into c1 and return item following c2
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer, intent(inout) :: c1, c2
    type(cohort_item), pointer :: next_item


    call c1%cohort%merge_in(c2%cohort)
    next_item => self%cohort_list%destroy_item(c2)
  end function merge_cohorts

  function cohorts(self) result(head_cohort)
    ! Return the head of the cohort list for iteration purpose.
    class(vegn_tile_type), intent(in) :: self
    type(cohort_item), pointer :: head_cohort

    head_cohort => self%cohort_list%head()
  end function cohorts

  function killed_cohort_fractions(self) result(head_cohort)
    ! Return the head of the killed cohort list for iteration purpose.
    class(vegn_tile_type), intent(in) :: self
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
    class(vegn_tile_type), intent(inout) :: self

    call self%cohort_list%destroy_all()
    call self%killed_fraction_list%destroy_all()
  end subroutine shut_down

  pure function get_height(item) result(res)
    type(cohort_item), intent(in) :: item
    real :: res

    res = item%cohort%height()
  end function get_height

  subroutine sort_cohorts_by_height(self, increasing)
    ! Sort cohorts by height
    class(vegn_tile_type), intent(inout) :: self
    logical, intent(in) :: increasing

    call self%cohort_list%sort(increasing, get_height)

  end subroutine sort_cohorts_by_height

  pure function get_uid(item) result(res)
    type(cohort_item), intent(in) :: item
    real :: res

    res = real(item%uid())
  end function get_uid

  subroutine sort_cohorts_by_uid(self, increasing)
    ! Sort cohorts by uid
    class(vegn_tile_type), intent(inout) :: self
    logical, intent(in) :: increasing

    call self%cohort_list%sort(increasing, get_uid)

  end subroutine sort_cohorts_by_uid

  function n_cohorts(self) result(res)
    ! Returns the current number of cohorts
    class(vegn_tile_type), intent(in) :: self
    integer :: res

    res = self%cohort_list%length()
  end function n_cohorts

  function new_cohort(self) result(new_item)
    ! Insert a new cohort at the head of the list and return its pointer.
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer :: new_item

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
    self%annual_fluxes = common_fluxes()
    self%annualPrcp   = 0.0
    self%annualEvap   = 0.0
    self%annualRoff   = 0.0
    self%annualRh     = 0.0
    self%N_P2S_yr     = 0.0
    self%annualN      = 0.0
    self%Nloss_yr     = 0.0
    self%n_deadtrees  = 0.0
    self%c_deadtrees  = 0.0
    self%m_turnover   = 0.0
    self%totseed      = orgpool()
    self%totNewC      = orgpool()


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

    self%daily_fluxes = common_fluxes()
    self%dailyRh   = 0.0
    self%dailyPrcp = 0.0
    self%dailyEvap = 0.0
    self%dailyRoff = 0.0

  end subroutine zero_daily_diagnostics

  subroutine aggregate_pools( self )
    !////////////////////////////////////////////////////////////////////////
    ! for annual update
    !------------------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it !iterator
    real :: dbh ! cache variable

    ! State variables
    self%plabl = orgpool()
    self%pleaf = orgpool()
    self%proot = orgpool()
    self%psapw = orgpool()
    self%pwood = orgpool()
    self%pseed = orgpool()

    it => self%cohorts()
    do while (associated(it))
      cc => it%cohort

      ! organic pools
      self%plabl = self%plabl + cc%plabl * cc%nindivs
      self%pleaf = self%pleaf + cc%pleaf * cc%nindivs
      self%proot = self%proot + cc%proot * cc%nindivs
      self%psapw = self%psapw + cc%psapw * cc%nindivs
      self%pwood = self%pwood + cc%pwood * cc%nindivs
      self%pseed = self%pseed + cc%pseed * cc%nindivs

      it => it%next()

    enddo

  end subroutine aggregate_pools

  subroutine aggregate_cohorts( self )
    !////////////////////////////////////////////////////////////////////////
    ! for annual update
    !------------------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it !iterator
    real :: dbh ! cache variable

    self%LAI          = 0.0
    self%CAI          = 0.0
    self%nindivs      = 0.0
    self%DBH          = 0.0
    self%nindivs12    = 0.0
    self%DBH12        = 0.0
    self%QMD12        = 0.0
    self%MaxAge       = 0.0
    self%MaxVolume    = 0.0
    self%MaxDBH       = 0.0
    self%NPPL         = 0.0
    self%NPPW         = 0.0
    self%m_turnover   = 0.0

    call self%aggregate_pools()

    it => self%cohorts()
    do while (associated(it))
      cc => it%cohort

      self%NPPL         = self%NPPL          + cc%NPPleaf    * cc%nindivs
      self%NPPW         = self%NPPW          + cc%NPPwood    * cc%nindivs
      self%m_turnover   = self%m_turnover    + cc%m_turnover * cc%nindivs

      self%CAI          = self%CAI      + cc%crownarea() * cc%nindivs
      self%LAI          = self%LAI      + cc%leafarea()  * cc%nindivs

      ! New tile outputs
      dbh = cc%dbh()
      self%DBH          = self%DBH      + dbh * cc%nindivs
      self%nindivs      = self%nindivs  + cc%nindivs

      if (dbh > 0.12) then
        self%DBH12      = self%DBH12     + dbh * cc%nindivs
        self%nindivs12  = self%nindivs12 + cc%nindivs
        self%QMD12  = self%QMD12 + dbh ** 2 * cc%nindivs
      endif

      self%MaxAge    = MAX(cc%age, self%MaxAge)
      self%MaxVolume = MAX(cc%volume(), self%MaxVolume)
      self%MaxDBH    = MAX(dbh, self%MaxDBH)

      it => it%next()

    enddo

    if (self%nindivs > 0.0) self%DBH   = self%DBH / self%nindivs
    if (self%nindivs12 > 0.0) self%DBH12 = self%DBH12 / self%nindivs12
    if (self%nindivs12 > 0.0) self%QMD12   = sqrt(self%QMD12 / self%nindivs12)

  end subroutine aggregate_cohorts


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

    it => self%cohorts()
    do while (associated(it))
      cc => it%cohort

      ! cohort daily
      call cc%daily_fluxes%add(cc%fast_fluxes)

      ! Reset fast fluxes
      cc%fast_fluxes = common_fluxes()

      it => it%next()
    enddo

    self%dailyRh     = self%dailyRh      + self%rh
    self%dailyEvap   = self%dailyEvap    + self%evap
    self%dailyRoff   = self%dailyRoff    + self%runoff
    self%dailyPrcp   = self%dailyPrcp    + self%precp

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
      call cc%annual_fluxes%add(cc%daily_fluxes)
      call self%daily_fluxes%add(cc%daily_fluxes, cc%nindivs)

      ! Reset daily variables
      cc%daily_fluxes = common_fluxes()

      it => it%next()
    enddo

    if (.not. state%spinup) then
      call self%aggregate_cohorts()

      out_daily_tile%year      = iyears
      out_daily_tile%doy       = idoy
      out_daily_tile%Tc        = self%tc_daily
      out_daily_tile%Prcp      = self%dailyPrcp
      out_daily_tile%totWs     = self%soilwater()
      out_daily_tile%Trsp      = self%daily_fluxes%trsp
      out_daily_tile%Evap      = self%dailyEvap
      out_daily_tile%Runoff    = self%dailyRoff
      out_daily_tile%ws1       = self%wcl(1)*thksl(1) * 1000
      out_daily_tile%ws2       = self%wcl(2)*thksl(2) * 1000
      out_daily_tile%ws3       = self%wcl(3)*thksl(3) * 1000
      out_daily_tile%LAI       = self%LAI
      out_daily_tile%GPP       = self%daily_fluxes%GPP
      out_daily_tile%Rauto     = self%daily_fluxes%Resp
      out_daily_tile%Rh        = self%dailyRh
      out_daily_tile%NSC       = self%plabl%c12
      out_daily_tile%seedC     = self%pseed%c12
      out_daily_tile%leafC     = self%pleaf%c12
      out_daily_tile%rootC     = self%proot%c12
      out_daily_tile%SW_C      = self%psapw%c12
      out_daily_tile%HW_C      = self%pwood%c12
      out_daily_tile%NSN       = self%plabl%n14
      out_daily_tile%seedN     = self%pseed%n14
      out_daily_tile%leafN     = self%pleaf%n14
      out_daily_tile%rootN     = self%proot%n14
      out_daily_tile%SW_N      = self%psapw%n14
      out_daily_tile%HW_N      = self%pwood%n14
      out_daily_tile%McrbC     = self%pmicr%c12
      out_daily_tile%fastSOM   = self%psoil_fs%c12
      out_daily_tile%slowSOM   = self%psoil_sl%c12
      out_daily_tile%McrbN     = self%pmicr%n14
      out_daily_tile%fastSoilN = self%psoil_fs%n14
      out_daily_tile%slowSoilN = self%psoil_sl%n14
      out_daily_tile%mineralN  = self%inorg%n14
      out_daily_tile%N_uptk    = self%daily_fluxes%Nup
    endif

    ! running annual sums
    self%annualRh   = self%annualRh   + self%dailyRh
    self%annualPrcp = self%annualPrcp + self%dailyPrcp
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

      treeG     = cc%pseed%c12 + cc%NPPleaf + cc%NPProot + cc%NPPwood
      fseed     = cc%pseed%c12 / treeG
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
        out_annual_cohorts(i)%nsc         = cc%plabl%c12
        out_annual_cohorts(i)%nsn         = cc%plabl%n14
        out_annual_cohorts(i)%seedC       = cc%pseed%c12
        out_annual_cohorts(i)%leafC       = cc%pleaf%c12
        out_annual_cohorts(i)%rootC       = cc%proot%c12
        out_annual_cohorts(i)%sapwC       = cc%psapw%c12
        out_annual_cohorts(i)%woodC       = cc%pwood%c12
        out_annual_cohorts(i)%treeG       = treeG
        out_annual_cohorts(i)%fseed       = fseed
        out_annual_cohorts(i)%fleaf       = fleaf
        out_annual_cohorts(i)%froot       = froot
        out_annual_cohorts(i)%fwood       = fwood
        out_annual_cohorts(i)%GPP         = cc%annual_fluxes%GPP
        out_annual_cohorts(i)%NPP         = cc%annual_fluxes%NPP()
        out_annual_cohorts(i)%Rauto       = cc%annual_fluxes%Resp
        out_annual_cohorts(i)%Nupt        = cc%annual_fluxes%Nup
        out_annual_cohorts(i)%Nfix        = cc%annual_fluxes%fixedN

      end if

      call self%annual_fluxes%add(cc%annual_fluxes, cc%nindivs)

      it => it%next()

    enddo

    call self%aggregate_cohorts()

    plantC    = self%plabl%c12 + self%pseed%c12 + self%pleaf%c12 + self%proot%c12 + self%psapw%c12 + self%pwood%c12
    plantN    = self%plabl%n14 + self%pseed%n14 + self%pleaf%n14 + self%proot%n14 + self%psapw%n14 + self%pwood%n14

    soilC     = self%pmicr%c12 + self%psoil_fs%c12 + self%psoil_sl%c12
    soilN     = self%pmicr%n14 + self%psoil_fs%n14 + self%psoil_sl%n14 + self%inorg%n14
    self%totN = plantN + soilN

    out_annual_tile%year            = iyears
    out_annual_tile%CAI             = self%CAI
    out_annual_tile%LAI             = self%LAI
    out_annual_tile%density         = self%nindivs * 10000   ! * 10000 to convert in indivs/ha
    out_annual_tile%DBH             = self%DBH * 100         ! * 100 to convert in cm
    out_annual_tile%density12       = self%nindivs12 * 10000 ! * 10000 to convert in indivs/ha
    out_annual_tile%DBH12           = self%DBH12 * 100       ! * 100 to convert in cm
    out_annual_tile%QMD12           = self%QMD12 * 100       ! * 100 to convert in cm
    out_annual_tile%NPP             = self%annual_fluxes%NPP()
    out_annual_tile%GPP             = self%annual_fluxes%GPP
    out_annual_tile%Rauto           = self%annual_fluxes%Resp
    out_annual_tile%Rh              = self%annualRh
    out_annual_tile%rain            = self%annualPrcp
    out_annual_tile%SoilWater       = self%SoilWater()
    out_annual_tile%Transp          = self%annual_fluxes%Trsp
    out_annual_tile%Evap            = self%annualEvap
    out_annual_tile%Runoff          = self%annualRoff
    out_annual_tile%plantC          = plantC ! kg C/m2/yr
    out_annual_tile%soilC           = soilC
    out_annual_tile%plantN          = plantN
    out_annual_tile%soilN           = soilN
    out_annual_tile%totN            = self%totN
    out_annual_tile%NSC             = self%plabl%c12
    out_annual_tile%SeedC           = self%pseed%c12
    out_annual_tile%leafC           = self%pleaf%c12
    out_annual_tile%rootC           = self%proot%c12
    out_annual_tile%SapwoodC        = self%psapw%c12
    out_annual_tile%WoodC           = self%pwood%c12
    out_annual_tile%NSN             = self%plabl%n14
    out_annual_tile%SeedN           = self%pseed%n14
    out_annual_tile%leafN           = self%pleaf%n14
    out_annual_tile%rootN           = self%proot%n14
    out_annual_tile%SapwoodN        = self%psapw%n14
    out_annual_tile%WoodN           = self%pwood%n14
    out_annual_tile%McrbC           = self%pmicr%c12
    out_annual_tile%fastSOM         = self%psoil_fs%c12
    out_annual_tile%SlowSOM         = self%psoil_sl%c12
    out_annual_tile%McrbN           = self%pmicr%n14
    out_annual_tile%fastSoilN       = self%psoil_fs%n14
    out_annual_tile%slowSoilN       = self%psoil_sl%n14
    out_annual_tile%mineralN        = self%inorg%n14
    out_annual_tile%N_fxed          = self%annual_fluxes%fixedN
    out_annual_tile%N_uptk          = self%annual_fluxes%Nup
    out_annual_tile%N_yrMin         = self%annualN
    out_annual_tile%N_P2S           = self%N_P2S_yr
    out_annual_tile%N_loss          = self%Nloss_yr
    out_annual_tile%MaxAge          = self%MaxAge
    out_annual_tile%MaxVolume       = self%MaxVolume
    out_annual_tile%MaxDBH          = self%MaxDBH
    out_annual_tile%NPPL            = self%NPPL
    out_annual_tile%NPPW            = self%NPPW
    out_annual_tile%n_deadtrees     = self%n_deadtrees
    out_annual_tile%c_deadtrees     = self%c_deadtrees
    out_annual_tile%m_turnover      = self%m_turnover
    out_annual_tile%c_turnover_time = self%pwood%c12 / self%NPPW

    ! Rebalance N (to compensate for the adjunction in vegn_N_uptake)
    if (myinterface%params_siml%do_closedN_run) call self%recover_N_balance()

  end subroutine annual_diagnostics

  subroutine plant2soil(self, loss_coarse, loss_fine)
    ! Redistribute C and N from dead plants to vegetation soil pools
    class(vegn_tile_type), intent(inout) :: self
    type(orgpool), intent(in) :: loss_coarse, loss_fine

    self%psoil_fs = self%psoil_fs + loss_fine * myinterface%params_tile%fsc_fine + &
            loss_coarse * myinterface%params_tile%fsc_wood
    self%psoil_sl = self%psoil_sl + loss_fine * (1.0 - myinterface%params_tile%fsc_fine) + &
            loss_coarse * (1.0 - myinterface%params_tile%fsc_wood)

      ! annual N from plants to soil and C turnover
      self%N_P2S_yr = self%N_P2S_yr + loss_coarse%n14 + loss_fine%n14

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
    type(orgpool) :: loss_fine,loss_coarse, loss_total

    ! We go through each killed fraction of cohort and we map the fraction to the cohort
    ! it originated from (they have the same uid).
    it => self%killed_cohort_fractions()
    do while (associated(it))
      cc => it%cohort
      do i = 1, NCohortMax
        if (int(out_annual_cohorts(i)%cID) == it%uid()) then
          associate (sp => cc%sp())

            ! Carbon and Nitrogen from plants to soil pools
            loss_coarse = (orgpool(- cc%leafarea(), cc%leafarea()) * myinterface%params_tile%LMAmin &
              + cc%pwood + cc%psapw + cc%pleaf) * cc%nindivs

            loss_fine = (orgpool(- cc%leafarea(), cc%leafarea()) * sp%LNbase &
              + cc%plabl + cc%pseed + cc%proot) * cc%nindivs

          end associate

          call self%plant2soil(loss_coarse, loss_fine)

          loss_total = loss_coarse + loss_fine

          self%m_turnover   = self%m_turnover    + loss_total%c12 ! We add C from dead trees to get the total turnover
          self%c_deadtrees  = self%c_deadtrees   + loss_total%c12
          self%n_deadtrees  = self%n_deadtrees   + loss_total%n14

          out_annual_cohorts(i)%n_deadtrees = out_annual_cohorts(i)%n_deadtrees + loss_total%n14
          out_annual_cohorts(i)%c_deadtrees = out_annual_cohorts(i)%c_deadtrees + loss_total%c12
          out_annual_cohorts(i)%deathrate   = out_annual_cohorts(i)%deathrate + cc%deathrate
          exit
        end if
      end do

      it => it%next()
    enddo

    out_annual_tile%N_P2S           = self%N_P2S_yr
    out_annual_tile%n_deadtrees     = self%n_deadtrees
    out_annual_tile%c_deadtrees     = self%c_deadtrees
    out_annual_tile%m_turnover      = self%m_turnover
    out_annual_tile%totseedC        = self%totseed%c12
    out_annual_tile%totseedN        = self%totseed%n14
    out_annual_tile%Seedling_C      = self%totNewC%c12
    out_annual_tile%Seedling_N      = self%totNewC%n14

  end subroutine annual_diagnostics_post_mortality

  subroutine recover_N_balance(self)
    !////////////////////////////////////////////////////////////////////////
    ! We scale the N pools to contrain the yearly N (soil + plant) to be constant.
    !------------------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    real :: delta, scaling_factor

    delta = self%totN - self%initialN0

    if (abs(delta) > 1e-6) then
      scaling_factor = 1 - delta / self%totN

      self%psoil_sl%n14 = self%psoil_sl%n14 * scaling_factor
      self%psoil_fs%n14 = self%psoil_fs%n14 * scaling_factor
      self%pmicr%n14 = self%pmicr%n14       * scaling_factor
      self%inorg%n14 = self%inorg%n14       * scaling_factor
      self%plabl%n14 = self%plabl%n14       * scaling_factor
      self%pseed%n14 = self%pseed%n14       * scaling_factor
      self%pleaf%n14 = self%pleaf%n14       * scaling_factor
      self%proot%n14 = self%proot%n14       * scaling_factor
      self%psapw%n14 = self%psapw%n14       * scaling_factor
      self%pwood%n14 = self%pwood%n14       * scaling_factor
      self%totN = self%initialN0
    endif
  end subroutine recover_N_balance

  subroutine initialize_PFT_data()

    ! ---- local vars ------
    integer :: i

    do i = 1, size(myinterface%params_species)
      call myinterface%params_species(i)%init_pft_data()
    enddo

  end subroutine initialize_pft_data


  subroutine initialize_vegn_tile( self )
    !////////////////////////////////////////////////////////////////
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self

    ! Local variables
    integer :: i, init_n_cohorts
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: new

    ! Initialize plant cohorts
    init_n_cohorts = size(myinterface%init_cohort)

    do i = 1, init_n_cohorts

      new => self%new_cohort()
      cc => new%cohort
      cc%species     = INT(myinterface%init_cohort(i)%init_cohort_species)
      cc%nindivs     = myinterface%init_cohort(i)%init_cohort_nindivs ! trees/m2
      cc%plabl%c12 = myinterface%init_cohort(i)%init_cohort_nsc
      cc%psapw%c12 = myinterface%init_cohort(i)%init_cohort_bsw
      cc%pwood%c12 = myinterface%init_cohort(i)%init_cohort_bHW
      cc%pleaf%c12 = myinterface%init_cohort(i)%init_cohort_bl
      cc%proot%c12 = myinterface%init_cohort(i)%init_cohort_br
      cc%pseed%c12 = myinterface%init_cohort(i)%init_cohort_seedC
      call cc%initialize_cohort_from_biomass()

    enddo

    ! Split initial layer in smaller layers (if it is full)
    call self%relayer()

    ! Initial Soil pools and environmental conditions
    self%psoil_fs%c12 = myinterface%init_soil%init_fast_soil_C  ! kgC m-2
    self%psoil_sl%c12 = myinterface%init_soil%init_slow_soil_C ! slow soil carbon pool, (kg C/m2)
    self%psoil_fs%n14 = self%psoil_fs%c12 / CN0metabolicL  ! fast soil nitrogen pool, (kg N/m2)
    self%psoil_sl%n14 = self%psoil_sl%c12 / CN0structuralL ! slow soil nitrogen pool, (kg N/m2)
    self%N_input      = myinterface%init_soil%N_input        ! kgN m-2 yr-1, N input to soil
    self%inorg%n14    = myinterface%init_soil%init_Nmineral  ! Mineral nitrogen pool, (kg N/m2)
    self%previousN    = self%inorg%n14

    ! debug: adding microbial biomass initialisation
    self%pmicr = orgpool() ! to do: add to: myinterface%init_soil%xxxxx

    ! Initialize soil volumetric water conent with field capacity (maximum soil moisture to start with)
    self%wcl = myinterface%params_tile%FLDCAP

    ! tile
    call aggregate_pools( self )

    self%initialN0 =  self%plabl%n14 + self%pseed%n14 + self%pleaf%n14 +      &
            self%proot%n14 + self%psapw%n14 + self%pwood%n14 + &
            self%pmicr%n14 + self%psoil_fs%n14 +       &
            self%psoil_sl%n14 + self%inorg%n14
    self%totN =  self%initialN0

  end subroutine initialize_vegn_tile

  subroutine relayer( self )
    !////////////////////////////////////////////////////////////////
    ! Arrange crowns into canopy layers according to their height and
    ! crown areas.
    ! We fill each layer until the maximum density is reached (layer_vegn_cover),
    ! before starting a new layer.
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self ! input cohorts

    ! ---- local constants
    real, parameter :: layer_vegn_cover = 1.0   ! i.e. max 1m2 vegetation per m2 ground

    ! local variables
    integer :: L        ! layer index (top-down)
    real    :: frac     ! fraction of the layer covered so far by the canopies
    real    :: fraction ! fraction to split off

    type(cohort_item), pointer :: it  ! iterator

    ! We sort the cohorts be decreasing height (important to do it here!)
    call self%sort_cohorts_by_height(.false.)

    L = 1
    frac = 0.0

    ! For each cohort present in the old list
    it => self%cohorts()
    do while (associated(it))

      ! We set the layer
      it%cohort%layer = L
      if (L == 1) then
        it%cohort%firstlayer = 1
      endif

      if ( &
              L < NLAYERS_MAX .and. &! We check if L < NLAYERS_MAX as we do not want to create more layers than the max specified amount.
                      it%cohort%layerfrac() > layer_vegn_cover - frac &
              ) then
        ! If the current cohort does not fit in the remaining fraction on the layer,
        ! we add a copy of the cohort to the new cohort list

        fraction = (layer_vegn_cover - frac) / it%cohort%layerfrac()
        call self%split_cohort(it, fraction)

        ! We keep it as we want to continue processing it at the next iteration
        ! Since the current layer is filled-up, we open-up a new fraction
        L = L + 1
        frac = 0.0
      else
        ! Otherwise, we have used-up the whole cohort, we update the current layer fraction and insert the current cohort
        frac = frac + it%cohort%layerfrac()
        it => it%next()
      end if

    end do

  end subroutine relayer

  subroutine reduce( self )
    !////////////////////////////////////////////////////////////////
    ! Merge similar cohorts in a tile
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self

    ! local variables
    type(cohort_item), pointer :: it1
    type(cohort_item), pointer :: it2

    ! This sort is not technically necessary, but is helpful for debugging
    !call self%sort_cohorts_by_uid(.true.)

    it1 => self%cohorts()
    do while (associated(it1))
      it2 => it1%next()
      do while (associated(it2))
        if (it1%cohort%can_be_merged_with(it2%cohort)) then
          it2 => self%merge_cohorts(it1, it2)
          call it1%cohort%init_bl_br()
        else
          it2 => it2%next()
        end if
      end do
      it1 => it1%next()
    end do

  end subroutine reduce


  subroutine kill_lowdensity_cohorts( self )
    !////////////////////////////////////////////////////////////////
    ! Remove cohorts that have (almost) fully died and update tile
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    ! local variables
    logical :: at_least_one_survivor

    type(cohort_item), pointer :: it

    at_least_one_survivor = .FALSE.

    ! We first check that we won't kill all the cohorts
    it => self%cohorts()
    do while (associated(it))
      if (it%cohort%nindivs > mindensity) then
        at_least_one_survivor = .TRUE.
        exit
      end if
      it => it%next()
    enddo

    ! If at least one cohort survives, we kill the low density ones
    ! Otherwise er do not kill anyone.
    if (at_least_one_survivor) then
      it => self%cohorts()
      do while (associated(it))
        if (it%cohort%nindivs > mindensity) then
          it => it%next()
        else
          ! if the density is below the threshold, we kill it.
          it => self%thin_cohort(it)
        end if
      end do
    end if
  end subroutine kill_lowdensity_cohorts

end module vegetation_tile_biomee
