module md_vegetation_tile_biomee
  !////////////////////////////////////////////////////////////////
  ! Definition of 'vegn_tile_type', the highest level type containing tile level variables and cohort list.
  ! Code adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !----------------------------------------------------------------
  use md_interface_in_biomee
  use md_interface_out_biomee
  use md_params_core
  use md_orgpool
  use md_common_fluxes
  use md_cohort
  use md_cohort_linked_list
  use, intrinsic :: iso_c_binding, only: c_double

  ! define data types and constants
  implicit none
  !=============== Public types ===========================================================
  public :: params_species_biomee, cohort_type, vegn_tile_type, dampended_forcing_type

  !=============== Parameters ======================================================
  integer, public, parameter :: NCohortMax = 50 ! maximum number of cohorts
  integer, public, parameter :: out_max_cohorts      = NCohortMax

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
    real    :: age = 0.0           ! tile age
    integer, private :: lu_index   ! Which land use (LU) this tile represents. Given as the index in 'init_lu' array.

    !========================= Cohort aggreation ===========================!
    ! Attention: variables aggregated from cohorts are only usable after having run aggregate_cohorts()
    real             :: density                            ! density (tree/m2)
    real, private    :: LAI                                ! leaf area index (surface of leaves per m2 of ground/tile)
    real, private    :: CAI                                ! crown area index (surface of the projected crown mer m2 of ground/tile)
    real, private    :: DBH
    real, private    :: density12
    real, private    :: DBH12
    real, private    :: QMD12
    real, private    :: MaxAge
    real, private    :: MaxVolume
    real, private    :: MaxDBH

    !===== Organic pools (cohort aggregation), kg m-2
    type(orgpool) :: pleaf                    ! leaf biomass
    type(orgpool) :: proot                    ! root biomass
    type(orgpool) :: psapw                    ! sapwood biomass
    type(orgpool) :: pwood                    ! heartwood (non-living) biomass
    type(orgpool) :: pseed                    ! biomass put aside for future progeny
    type(orgpool) :: plabl                    ! labile pool, temporary storage of N and C

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
    type(orgpool) :: psoil_fs        ! soil organic matter, fast turnover (= litter)
    type(orgpool) :: psoil_sl        ! soil organic matter, slow turnover
    type(orgpool) :: pmicr           ! microbial biomass

    !===== Inorganic pools, kg m-2
    type(orgpool) :: inorg                        ! Mineral N/C pool

    !===== Soil water pool, kg H2O m-2
    real    :: wcl(MAX_LEVELS)              ! volumetric soil water content for each layer

    !=====  Averaged quantities for PPA phenology
    ! Note, these fields do not follow the usual annual cycle (january-january)
    ! so they are not reset in zero_diagnostics
    real    :: tc_daily           = 0.0           ! 24h average temperature (deg C)
    real    :: gdd                = 0.0           ! growing degree-days
    real    :: tc_pheno           = 0.0           ! smoothed canopy air temperature for phenology
    real    :: tc_soil            = 0.0           ! Daily soil temperature in (deg C)

    !===== Annual C/N allocation to seed and non-seed, kg m-2 yr-1
    type(orgpool)    :: totSeed           ! Total seed
    type(orgpool)    :: totNewC           ! New cohort (all compartments but seed)

    !=====  N-related fluxes
    real    :: totN
    real    :: annualN            ! annual available N in a year
    real    :: Nloss_yr           ! annual N loss
    real    :: N_P2S_yr           ! N turnover (plant to soil) (kg N m-2 yr-1)

    !=====  Memory
    real    :: previousN          ! weighted annual available N
    real    :: initialN0          ! initial available N (kg N m-2)

    !=====  Fast fluxes, kg m-2 timestep-1
    real    :: rh                                 ! soil carbon lost to the atmosphere
    !=====  Soil water fluxes, kg H2O m-2 timestep-1
    real    :: evap                               ! Evaporation
    real    :: runoff                             ! Water runoff of the tile
    real    :: precp                              ! Precipitation

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
    ! Scrap variables used to compute tsoil
    real, dimension(:), allocatable   :: dtemp_pvy          ! daily temperature of previous year (deg C)
    real, dimension(:), allocatable   :: wscal_pvy          ! daily Cramer-Prentice-Alpha of previous year (unitless)
    real, dimension(ndayyear)         :: wscal_alldays

    !===== Reporting diagnostics
    real, dimension(nvars_annual_tile) :: out_annual_tile
    real, dimension(ndayyear, nvars_daily_tile) :: out_daily_tile
    real, dimension(out_max_cohorts, nvars_annual_cohorts) :: out_annual_cohorts

  contains

    !========= Derived variables
    ! Variables which are function of any state or temporary variable defined above.
    ! They can be used like any normal variable except that they are followed by parenthesis
    ! (indicating that they are being evaluated every time).
    ! The advantage is that they cannot be out-of-sync with the underlying data.
    ! One might think that this will increase the number of operations, but it is impossible to tell
    ! as the compiler may or may not optimize the compiled code in such a way that function calls are avoided.
    ! On the opposite, more structure and constrains in the code give the compiler more opportunities for optimizations and
    ! so far, the use of functions has actually sped up the execution time quite a lot.
    procedure thetaS
    procedure soilwater
    procedure pplant
    procedure psoil

    !========= Cohort management

    procedure n_cohorts
    procedure new_cohort
    procedure sort_cohorts_by_height
    procedure sort_cohorts_by_uid
    procedure shut_down
    procedure thin_cohort
    procedure reduce
    procedure relayer
    procedure kill_lowdensity_cohorts

    !========= Diagnostic methods

    procedure annual_diagnostics
    procedure annual_diagnostics_post_mortality
    procedure daily_diagnostics
    procedure hourly_diagnostics
    procedure zero_diagnostics

    !========= Cohort list accessors

    procedure cohorts
    procedure, private :: killed_cohort_fractions

    !========= Public helper methods

    procedure plant2soil
    procedure initialize_vegn_tile
    procedure :: aggregate_cohorts
    procedure :: lu_props

    !========= Private helper methods

    procedure, private :: recover_N_balance
    procedure, private :: merge_cohorts
    procedure, private :: split_cohort
    procedure, private :: aggregate_pools
    procedure, private :: zero_daily_diagnostics
    procedure, private :: kill_cohort

  end type vegn_tile_type

contains

  !----------------------------------------------------------------
  ! Derived variables
  !----------------------------------------------------------------

  pure real function soilwater(self)
    !////////////////////////////////////////////////////////////////
    ! Soil water, kg m-2 in root zone
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(in) :: self

    soilwater  = SUM(self%wcl(:)*thksl(:)*1000.0)

  end function soilwater

  pure real function thetaS(self)
    !////////////////////////////////////////////////////////////////
    ! Moisture index
    ! (ws - wiltpt)/(fldcap - wiltpt)
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(in) :: self

    thetaS  = (self%wcl(2) - inputs%params_tile%WILTPT) &
            / (inputs%params_tile%FLDCAP - inputs%params_tile%WILTPT)

  end function thetaS

  pure function pplant(self) result(res)
    !////////////////////////////////////////////////////////////////
    ! Plant pools
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(in) :: self
    type(orgpool) :: res

    res = self%plabl + self%pseed + self%pleaf + self%proot + self%psapw + self%pwood

  end function pplant

  pure function psoil(self) result(res)
    !////////////////////////////////////////////////////////////////
    ! Soil pools
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(in) :: self
    type(orgpool) :: res

    res = self%pmicr + self%psoil_fs + self%psoil_sl + self%inorg

  end function psoil

  !----------------------------------------------------------------
  ! Cohort management
  !----------------------------------------------------------------

  function n_cohorts(self) result(res)
    !////////////////////////////////////////////////////////////////
    ! Returns the current number of cohorts
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(in) :: self
    integer :: res

    res = self%cohort_list%length()
  end function n_cohorts

  subroutine split_cohort(self, item, fraction)
    !////////////////////////////////////////////////////////////////
    ! Split a cohort into two. The initial cohort gets scaled by (1 - fraction),
    ! while a newly created cohort, inserted in the cohort list, gets the complement (fraction).
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer, intent(in) :: item
    real, intent(in) :: fraction

    ! Local variable
    type(cohort_item), pointer :: new

    new => item%clone()
    new%cohort%density = item%cohort%density * fraction
    call self%cohort_list%insert_item(new)
    item%cohort%density = item%cohort%density - new%cohort%density
  end subroutine split_cohort

  function merge_cohorts(self, c1, c2) result(next_item)
    !////////////////////////////////////////////////////////////////
    ! Merge cohort c2 into c1 and return item following c2
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer, intent(inout) :: c1, c2
    type(cohort_item), pointer :: next_item


    call c1%cohort%merge_in(c2%cohort)
    next_item => self%cohort_list%destroy_item(c2)
  end function merge_cohorts

  function cohorts(self) result(head_cohort)
    !////////////////////////////////////////////////////////////////
    ! Return the head of the cohort list for iteration purpose.
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(in) :: self
    type(cohort_item), pointer :: head_cohort

    head_cohort => self%cohort_list%head()
  end function cohorts

  function killed_cohort_fractions(self) result(head_cohort)
    !////////////////////////////////////////////////////////////////
    ! Return the head of the killed cohort list for iteration purpose.
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(in) :: self
    type(cohort_item), pointer :: head_cohort

    head_cohort => self%killed_fraction_list%head()
  end function killed_cohort_fractions

  function thin_cohort(self, item, fraction) result(next_item)
    !////////////////////////////////////////////////////////////////
    ! Thin the provided cohort by applying a deathrate = 'fraction'.
    ! By default fraction = 1.0, which means that the whole cohort disappears.
    !
    ! Implementation details:
    ! This creates a new cohort in 'killed_fraction_list' with nindivis = cohort%nindivis * fraction
    ! And the original cohort is the complement: nindivis = cohort%nindivis * (1-fraction)
    ! If fraction is 0, nothing happens.
    ! If fraction is 1, the cohort is moved from cohorts to killed_cohort_fractions.
    !---------------------------------------------------------------
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
      killed%cohort%density = item%cohort%density * frac
      killed%cohort%deathrate = frac
      call self%killed_fraction_list%insert_item(killed)
      item%cohort%density = item%cohort%density - killed%cohort%density
      next_item => item%next()
    end if
  end function thin_cohort

  subroutine shut_down(self)
    !////////////////////////////////////////////////////////////////
    ! Free all allocated memory
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self

    call self%cohort_list%destroy_all()
    call self%killed_fraction_list%destroy_all()
    if (allocated(self%dtemp_pvy)) deallocate(self%dtemp_pvy)
    if (allocated(self%wscal_pvy)) deallocate(self%wscal_pvy)
  end subroutine shut_down

  pure function get_height(item) result(res)
    !////////////////////////////////////////////////////////////////
    ! Get height
    !---------------------------------------------------------------
    type(cohort_item), intent(in) :: item
    real :: res

    res = item%cohort%height()
  end function get_height

  subroutine sort_cohorts_by_height(self, increasing)
    !////////////////////////////////////////////////////////////////
    ! Sort cohorts by height
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    logical, intent(in) :: increasing

    call self%cohort_list%sort(increasing, get_height)

  end subroutine sort_cohorts_by_height

  pure function get_uid(item) result(res)
    !////////////////////////////////////////////////////////////////
    ! Get uid
    !---------------------------------------------------------------
    type(cohort_item), intent(in) :: item
    real :: res

    res = real(item%uid())
  end function get_uid

  subroutine sort_cohorts_by_uid(self, increasing)
    !////////////////////////////////////////////////////////////////
    ! Sorts cohorts by uid
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    logical, intent(in) :: increasing

    call self%cohort_list%sort(increasing, get_uid)

  end subroutine sort_cohorts_by_uid

  function new_cohort(self) result(new_item)
    !////////////////////////////////////////////////////////////////
    ! Create and insert a new cohort at the head of the list and return its pointer.
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer :: new_item

    new_item => create_cohort()
    call self%cohort_list%insert_item(new_item)

  end function new_cohort

  function kill_cohort(self, item) result(next_item)
    !////////////////////////////////////////////////////////////////
    ! Move item to killed_cohort_fractions and return pointer to next
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    type(cohort_item), pointer, intent(inout) :: item
    type(cohort_item), pointer :: next_item

    item%cohort%deathrate = 1.0
    next_item => self%cohort_list%detach_item(item)
    call self%killed_fraction_list%insert_item(item)
  end function kill_cohort

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
      if (it%cohort%density > mindensity) then
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
        if (it%cohort%density > mindensity) then
          it => it%next()
        else
          ! if the density is below the threshold, we kill it.
          it => self%thin_cohort(it)
        end if
      end do
    end if
  end subroutine kill_lowdensity_cohorts

  !----------------------------------------------------------------
  ! Diagnostic methods
  !----------------------------------------------------------------

  subroutine zero_diagnostics(self)
    !////////////////////////////////////////////////////////////////
    ! Reset annual all diagnostic variables
    !---------------------------------------------------------------

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
    ! Reset dauly diagnostic variables
    class(vegn_tile_type), intent(inout) :: self

    self%daily_fluxes = common_fluxes()
    self%dailyRh   = 0.0
    self%dailyPrcp = 0.0
    self%dailyEvap = 0.0
    self%dailyRoff = 0.0

  end subroutine zero_daily_diagnostics

  subroutine hourly_diagnostics(self)
    !////////////////////////////////////////////////////////////////
    ! Updates sub-daily cohort and tile-level variables and takes running daily sums
    !---------------------------------------------------------------
    use md_interface_in_biomee, only: inputs

    class(vegn_tile_type), intent(inout) :: self

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it

    self%age = self%age + inputs%dt_fast_yr

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


  subroutine daily_diagnostics(self, iyears, idoy, daily_reporting)
    !////////////////////////////////////////////////////////////////
    ! Updates daily tile-level variables and takes running annual sums
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    integer, intent(in) :: iyears, idoy
    logical, intent(in) :: daily_reporting

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it

    it => self%cohorts()
    do while (associated(it))
      cc => it%cohort

      ! running annual sum
      call cc%annual_fluxes%add(cc%daily_fluxes)
      call self%daily_fluxes%add(cc%daily_fluxes, cc%density)

      ! Reset daily variables
      cc%daily_fluxes = common_fluxes()

      it => it%next()
    enddo

    if (daily_reporting) then
      call self%aggregate_cohorts()

      self%out_daily_tile(idoy, DAILY_TILE_YEAR       ) = iyears
      self%out_daily_tile(idoy, DAILY_TILE_DOY        ) = idoy
      self%out_daily_tile(idoy, DAILY_TILE_TC         ) = self%tc_daily
      self%out_daily_tile(idoy, DAILY_TILE_PRCP       ) = self%dailyPrcp
      self%out_daily_tile(idoy, DAILY_TILE_SOIL_W     ) = self%soilwater()
      self%out_daily_tile(idoy, DAILY_TILE_TRSP       ) = self%daily_fluxes%trsp
      self%out_daily_tile(idoy, DAILY_TILE_EVAP       ) = self%dailyEvap
      self%out_daily_tile(idoy, DAILY_TILE_RUNOFF     ) = self%dailyRoff
      self%out_daily_tile(idoy, DAILY_TILE_WS1        ) = self%wcl(1)*thksl(1) * 1000
      self%out_daily_tile(idoy, DAILY_TILE_WS2        ) = self%wcl(2)*thksl(2) * 1000
      self%out_daily_tile(idoy, DAILY_TILE_WS3        ) = self%wcl(3)*thksl(3) * 1000
      self%out_daily_tile(idoy, DAILY_TILE_LAI        ) = self%LAI
      self%out_daily_tile(idoy, DAILY_TILE_GPP        ) = self%daily_fluxes%GPP
      self%out_daily_tile(idoy, DAILY_TILE_RESP       ) = self%daily_fluxes%Resp
      self%out_daily_tile(idoy, DAILY_TILE_RH         ) = self%dailyRh
      self%out_daily_tile(idoy, DAILY_TILE_NSC        ) = self%plabl%c12
      self%out_daily_tile(idoy, DAILY_TILE_SEED_C     ) = self%pseed%c12
      self%out_daily_tile(idoy, DAILY_TILE_LEAF_C     ) = self%pleaf%c12
      self%out_daily_tile(idoy, DAILY_TILE_ROOT_C     ) = self%proot%c12
      self%out_daily_tile(idoy, DAILY_TILE_SWC        ) = self%psapw%c12
      self%out_daily_tile(idoy, DAILY_TILE_HWC        ) = self%pwood%c12
      self%out_daily_tile(idoy, DAILY_TILE_NSN        ) = self%plabl%n14
      self%out_daily_tile(idoy, DAILY_TILE_SEED_N     ) = self%pseed%n14
      self%out_daily_tile(idoy, DAILY_TILE_LEAF_N     ) = self%pleaf%n14
      self%out_daily_tile(idoy, DAILY_TILE_ROOT_N     ) = self%proot%n14
      self%out_daily_tile(idoy, DAILY_TILE_SW_N       ) = self%psapw%n14
      self%out_daily_tile(idoy, DAILY_TILE_HW_N       ) = self%pwood%n14
      self%out_daily_tile(idoy, DAILY_TILE_MCRB_C     ) = self%pmicr%c12
      self%out_daily_tile(idoy, DAILY_TILE_FASTSOM    ) = self%psoil_fs%c12
      self%out_daily_tile(idoy, DAILY_TILE_SLOWSOM    ) = self%psoil_sl%c12
      self%out_daily_tile(idoy, DAILY_TILE_MCRB_N     ) = self%pmicr%n14
      self%out_daily_tile(idoy, DAILY_TILE_FS_N       ) = self%psoil_fs%n14
      self%out_daily_tile(idoy, DAILY_TILE_SL_N       ) = self%psoil_sl%n14
      self%out_daily_tile(idoy, DAILY_TILE_INORG_N    ) = self%inorg%n14
      self%out_daily_tile(idoy, DAILY_TILE_N_UPTK     ) = self%daily_fluxes%Nup
    endif

    ! running annual sums
    self%annualRh   = self%annualRh   + self%dailyRh
    self%annualPrcp = self%annualPrcp + self%dailyPrcp
    self%annualEvap = self%annualEvap + self%dailyevap
    self%annualRoff = self%annualRoff + self%dailyRoff

    ! zero:
    call self%zero_daily_diagnostics()

  end subroutine daily_diagnostics


  subroutine annual_diagnostics(self, iyears, cohort_reporting)
    !////////////////////////////////////////////////////////////////
    ! Updates tile-level variables and populates annual output
    !---------------------------------------------------------------
    use md_interface_in_biomee, only: inputs

    class(vegn_tile_type), intent(inout) :: self
    integer, intent(in) :: iyears
    logical, intent(in) :: cohort_reporting

    ! local variables
    real :: treeG, fseed, fleaf, froot, fwood, dDBH, BA, dBA
    real :: plantC, plantN, soilC, soilN
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it
    integer :: i
    type(orgpool) :: pool

    i = 0

      ! Cohorts ouput
      it => self%cohorts()
      do while (associated(it))
        cc => it%cohort

        i = i + 1

        if (cohort_reporting) then

        treeG     = cc%pseed%c12 + cc%NPPleaf + cc%NPProot + cc%NPPwood
        fseed     = cc%pseed%c12 / treeG
        fleaf     = cc%NPPleaf / treeG
        froot     = cc%NPProot / treeG
        fwood     = cc%NPPwood / treeG
        dDBH      = cc%dbh() - cc%DBH_ys !in m
        BA        = cc%basal_area()
        dBA       = BA - cc%BA_ys

        if (i <= NCohortMax) then

          self%out_annual_cohorts(i, ANNUAL_COHORTS_ID         ) = i
          self%out_annual_cohorts(i, ANNUAL_COHORTS_YEAR       ) = iyears
          self%out_annual_cohorts(i, ANNUAL_COHORTS_CID        ) = it%uid()
          self%out_annual_cohorts(i, ANNUAL_COHORTS_PFT        ) = cc%species
          self%out_annual_cohorts(i, ANNUAL_COHORTS_LAYER      ) = cc%layer
          self%out_annual_cohorts(i, ANNUAL_COHORTS_DENSITY    ) = cc%density * 10000   ! *10000 to convert in tree per ha
          self%out_annual_cohorts(i, ANNUAL_COHORTS_FLAYER     ) = cc%layerfrac()
          self%out_annual_cohorts(i, ANNUAL_COHORTS_DBH        ) = cc%dbh() * 100       ! *100 to convert m in cm
          self%out_annual_cohorts(i, ANNUAL_COHORTS_DDBH       ) = dDBH * 100           ! *100 to convert m in cm
          self%out_annual_cohorts(i, ANNUAL_COHORTS_HEIGHT     ) = cc%height()
          self%out_annual_cohorts(i, ANNUAL_COHORTS_AGE        ) = cc%age
          self%out_annual_cohorts(i, ANNUAL_COHORTS_BA         ) = BA
          self%out_annual_cohorts(i, ANNUAL_COHORTS_DBA        ) = dBA
          self%out_annual_cohorts(i, ANNUAL_COHORTS_ACROWN     ) = cc%crownarea()
          self%out_annual_cohorts(i, ANNUAL_COHORTS_ALEAF      ) = cc%leafarea()
          self%out_annual_cohorts(i, ANNUAL_COHORTS_NCS        ) = cc%plabl%c12
          self%out_annual_cohorts(i, ANNUAL_COHORTS_NSN        ) = cc%plabl%n14
          self%out_annual_cohorts(i, ANNUAL_COHORTS_SEED_C     ) = cc%pseed%c12
          self%out_annual_cohorts(i, ANNUAL_COHORTS_LEAF_C     ) = cc%pleaf%c12
          self%out_annual_cohorts(i, ANNUAL_COHORTS_ROOT_C     ) = cc%proot%c12
          self%out_annual_cohorts(i, ANNUAL_COHORTS_SW_C       ) = cc%psapw%c12
          self%out_annual_cohorts(i, ANNUAL_COHORTS_HW_C       ) = cc%pwood%c12
          self%out_annual_cohorts(i, ANNUAL_COHORTS_TREEG      ) = treeG
          self%out_annual_cohorts(i, ANNUAL_COHORTS_FSEED      ) = fseed
          self%out_annual_cohorts(i, ANNUAL_COHORTS_FLEAF      ) = fleaf
          self%out_annual_cohorts(i, ANNUAL_COHORTS_FROOT      ) = froot
          self%out_annual_cohorts(i, ANNUAL_COHORTS_FWOOD      ) = fwood
          self%out_annual_cohorts(i, ANNUAL_COHORTS_GPP        ) = cc%annual_fluxes%GPP
          self%out_annual_cohorts(i, ANNUAL_COHORTS_NPP        ) = cc%annual_fluxes%NPP()
          self%out_annual_cohorts(i, ANNUAL_COHORTS_RESP       ) = cc%annual_fluxes%Resp
          self%out_annual_cohorts(i, ANNUAL_COHORTS_N_UPTK     ) = cc%annual_fluxes%Nup
          self%out_annual_cohorts(i, ANNUAL_COHORTS_N_FIX      ) = cc%annual_fluxes%fixedN
          self%out_annual_cohorts(i, ANNUAL_COHORTS_DEATHRATE  ) = 0
          self%out_annual_cohorts(i, ANNUAL_COHORTS_N_LOSS     ) = 0
          self%out_annual_cohorts(i, ANNUAL_COHORTS_C_LOSS     ) = 0

        end if

        end if

        call self%annual_fluxes%add(cc%annual_fluxes, cc%density)

        it => it%next()

      enddo

    call self%aggregate_cohorts()

    pool = self%pplant()
    plantC = pool%c12
    plantN = pool%n14
    pool = self%psoil()
    soilC = pool%c12
    soilN = pool%n14

    self%out_annual_tile(ANNUAL_TILE_YEAR            ) = iyears
    self%out_annual_tile(ANNUAL_TILE_CAI             ) = self%CAI
    self%out_annual_tile(ANNUAL_TILE_LAI             ) = self%LAI
    self%out_annual_tile(ANNUAL_TILE_DENSITY         ) = self%density * 10000   ! * 10000 to convert in indivs/ha
    self%out_annual_tile(ANNUAL_TILE_DBH             ) = self%DBH * 100         ! * 100 to convert in cm
    self%out_annual_tile(ANNUAL_TILE_DENSITY12       ) = self%density12 * 10000 ! * 10000 to convert in indivs/ha
    self%out_annual_tile(ANNUAL_TILE_DBH12           ) = self%DBH12 * 100       ! * 100 to convert in cm
    self%out_annual_tile(ANNUAL_TILE_QMD12           ) = self%QMD12 * 100       ! * 100 to convert in cm
    self%out_annual_tile(ANNUAL_TILE_NPP             ) = self%annual_fluxes%NPP()
    self%out_annual_tile(ANNUAL_TILE_GPP             ) = self%annual_fluxes%GPP
    self%out_annual_tile(ANNUAL_TILE_RESP            ) = self%annual_fluxes%Resp
    self%out_annual_tile(ANNUAL_TILE_RH              ) = self%annualRh
    self%out_annual_tile(ANNUAL_TILE_PRCP            ) = self%annualPrcp
    self%out_annual_tile(ANNUAL_TILE_SOIL_W          ) = self%SoilWater()
    self%out_annual_tile(ANNUAL_TILE_TRSP            ) = self%annual_fluxes%Trsp
    self%out_annual_tile(ANNUAL_TILE_EVAP            ) = self%annualEvap
    self%out_annual_tile(ANNUAL_TILE_RUNOFF          ) = self%annualRoff
    self%out_annual_tile(ANNUAL_TILE_PLANT_C         ) = plantC
    self%out_annual_tile(ANNUAL_TILE_SOIL_C          ) = SoilC
    self%out_annual_tile(ANNUAL_TILE_PLANT_N         ) = plantN
    self%out_annual_tile(ANNUAL_TILE_SOIL_N          ) = SoilN
    self%out_annual_tile(ANNUAL_TILE_TOT_N           ) = self%totN
    self%out_annual_tile(ANNUAL_TILE_NS_C            ) = self%plabl%c12
    self%out_annual_tile(ANNUAL_TILE_SEED_C          ) = self%pseed%c12
    self%out_annual_tile(ANNUAL_TILE_LEAF_C          ) = self%pleaf%c12
    self%out_annual_tile(ANNUAL_TILE_ROOT_C          ) = self%proot%c12
    self%out_annual_tile(ANNUAL_TILE_SW_C            ) = self%psapw%c12
    self%out_annual_tile(ANNUAL_TILE_HW_C            ) = self%pwood%c12
    self%out_annual_tile(ANNUAL_TILE_NSN             ) = self%plabl%n14
    self%out_annual_tile(ANNUAL_TILE_SEED_N          ) = self%pseed%n14
    self%out_annual_tile(ANNUAL_TILE_LEAF_N          ) = self%pleaf%n14
    self%out_annual_tile(ANNUAL_TILE_ROOT_N          ) = self%proot%n14
    self%out_annual_tile(ANNUAL_TILE_SW_N            ) = self%psapw%n14
    self%out_annual_tile(ANNUAL_TILE_HW_N            ) = self%pwood%n14
    self%out_annual_tile(ANNUAL_TILE_MCRB_C          ) = self%pmicr%c12
    self%out_annual_tile(ANNUAL_TILE_FASTSOM         ) = self%psoil_fs%c12
    self%out_annual_tile(ANNUAL_TILE_SLOWSOM         ) = self%psoil_sl%c12
    self%out_annual_tile(ANNUAL_TILE_MCRB_N          ) = self%pmicr%n14
    self%out_annual_tile(ANNUAL_TILE_FS_N            ) = self%psoil_fs%n14
    self%out_annual_tile(ANNUAL_TILE_SL_N            ) = self%psoil_sl%n14
    self%out_annual_tile(ANNUAL_TILE_INORG_N         ) = self%inorg%n14
    self%out_annual_tile(ANNUAL_TILE_N_FIX           ) = self%annual_fluxes%fixedN
    self%out_annual_tile(ANNUAL_TILE_N_UPTK          ) = self%annual_fluxes%Nup
    self%out_annual_tile(ANNUAL_TILE_NYRMIN          ) = self%annualN
    self%out_annual_tile(ANNUAL_TILE_NP2S            ) = 0
    self%out_annual_tile(ANNUAL_TILE_NLOSS           ) = self%Nloss_yr
    self%out_annual_tile(ANNUAL_TILE_TOTSEED_C       ) = 0
    self%out_annual_tile(ANNUAL_TILE_TOTSEED_N       ) = 0
    self%out_annual_tile(ANNUAL_TILE_SEEDLING_C      ) = 0
    self%out_annual_tile(ANNUAL_TILE_SEEDLING_N      ) = 0
    self%out_annual_tile(ANNUAL_TILE_MAX_AGE         ) = self%MaxAge
    self%out_annual_tile(ANNUAL_TILE_MAX_VOLUME      ) = self%MaxVolume
    self%out_annual_tile(ANNUAL_TILE_MAX_DBH         ) = self%MaxDBH
    self%out_annual_tile(ANNUAL_TILE_NPP_L           ) = self%NPPL
    self%out_annual_tile(ANNUAL_TILE_NPP_W           ) = self%NPPW
    self%out_annual_tile(ANNUAL_TILE_DEADTREES_N     ) = 0
    self%out_annual_tile(ANNUAL_TILE_DEADTREES_C     ) = 0
    self%out_annual_tile(ANNUAL_TILE_M_TURNOVER      ) = 0
    self%out_annual_tile(ANNUAL_TILE_C_TURNOVER_TIME ) = self%pwood%c12 / self%NPPW

    ! Rebalance N (to compensate for the adjunction in vegn_N_uptake)
    if (inputs%params_siml%do_closedN_run) call self%recover_N_balance()

  end subroutine annual_diagnostics

  subroutine annual_diagnostics_post_mortality(self, cohort_reporting)
    !////////////////////////////////////////////////////////////////
    ! Updates tile-level variables and populates annual output after reproduction and mortality processes
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    logical, intent(in) :: cohort_reporting

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

      associate (sp => cc%sp())

        ! Carbon and Nitrogen from plants to soil pools
        loss_coarse = (orgpool(- cc%leafarea(), cc%leafarea()) * inputs%params_tile%LMAmin &
                + cc%pwood + cc%psapw + cc%pleaf) * cc%density

        loss_fine = (orgpool(- cc%leafarea(), cc%leafarea()) * sp%LNbase &
                + cc%plabl + cc%pseed + cc%proot) * cc%density

      end associate

      call self%plant2soil(loss_coarse, loss_fine)

      loss_total = loss_coarse + loss_fine

      self%m_turnover   = self%m_turnover    + loss_total%c12 ! We add C from dead trees to get the total turnover
      self%c_deadtrees  = self%c_deadtrees   + loss_total%c12
      self%n_deadtrees  = self%n_deadtrees   + loss_total%n14

      if (cohort_reporting) then
        do i = 1, NCohortMax
          if (int(self%out_annual_cohorts(i, ANNUAL_COHORTS_CID)) == it%uid()) then

            self%out_annual_cohorts(i, ANNUAL_COHORTS_DEATHRATE) = self%out_annual_cohorts(i, ANNUAL_COHORTS_DEATHRATE) &
                    + cc%deathrate
            self%out_annual_cohorts(i, ANNUAL_COHORTS_N_LOSS)    = self%out_annual_cohorts(i, ANNUAL_COHORTS_N_LOSS) &
                    + loss_total%n14
            self%out_annual_cohorts(i, ANNUAL_COHORTS_C_LOSS)    = self%out_annual_cohorts(i, ANNUAL_COHORTS_C_LOSS) &
                    + loss_total%c12

            exit
          end if
        end do
      end if

      it => it%next()
    enddo

    self%out_annual_tile(ANNUAL_TILE_NP2S       ) = self%N_P2S_yr
    self%out_annual_tile(ANNUAL_TILE_DEADTREES_N) = self%n_deadtrees
    self%out_annual_tile(ANNUAL_TILE_DEADTREES_C) = self%c_deadtrees
    self%out_annual_tile(ANNUAL_TILE_M_TURNOVER ) = self%m_turnover
    self%out_annual_tile(ANNUAL_TILE_TOTSEED_C  ) = self%totseed%c12
    self%out_annual_tile(ANNUAL_TILE_TOTSEED_N  ) = self%totseed%n14
    self%out_annual_tile(ANNUAL_TILE_SEEDLING_C ) = self%totNewC%c12
    self%out_annual_tile(ANNUAL_TILE_SEEDLING_N ) = self%totNewC%n14

  end subroutine annual_diagnostics_post_mortality

  !----------------------------------------------------------------
  ! Public helper methods
  !----------------------------------------------------------------

  subroutine plant2soil(self, loss_coarse, loss_fine)
    !////////////////////////////////////////////////////////////////
    ! Redistribute C and N from dead plants to vegetation soil pools
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    type(orgpool), intent(in) :: loss_coarse, loss_fine

    self%psoil_fs = self%psoil_fs + loss_fine * inputs%params_tile%fsc_fine + &
            loss_coarse * inputs%params_tile%fsc_wood
    self%psoil_sl = self%psoil_sl + loss_fine * (1.0 - inputs%params_tile%fsc_fine) + &
            loss_coarse * (1.0 - inputs%params_tile%fsc_wood)

    ! annual N from plants to soil and C turnover
    self%N_P2S_yr = self%N_P2S_yr + loss_coarse%n14 + loss_fine%n14

  end subroutine

  subroutine initialize_vegn_tile( self, lu_index )
    !////////////////////////////////////////////////////////////////////////
    ! Initialize vegetation tile and cohorts pools
    ! 'lu_index' parameter sets the eponym variable.
    !---------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    integer, intent(in) :: lu_index

    ! Local variables
    integer :: i, init_n_cohorts
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: new

    ! Initialize lu_index
    self%lu_index = lu_index

    ! If it is an LU acception vegetation (typically anything else than urban), we add the cohorts matching the lu_index
    if (inputs%init_lu(lu_index)%vegetated) then

      ! Initialize plant cohorts
      init_n_cohorts = size(inputs%init_cohort)

      do i = 1, init_n_cohorts

        ! If this cohorts init does not belong to the land use type of this vegetation tile, we move on to the next item.
        ! If the cohort's registered lu_index is negative, it belongs to all LU types (except urban)
        if (inputs%init_cohort(i)%lu_index > 0 .and. self%lu_index /= i) cycle

        new => self%new_cohort()
        cc => new%cohort
        cc%species   = inputs%init_cohort(i)%init_cohort_species
        cc%density   = inputs%init_cohort(i)%init_cohort_density ! trees/m2
        cc%plabl%c12 = inputs%init_cohort(i)%init_cohort_nsc
        cc%psapw%c12 = inputs%init_cohort(i)%init_cohort_bsw
        cc%pwood%c12 = inputs%init_cohort(i)%init_cohort_bHW
        cc%pleaf%c12 = inputs%init_cohort(i)%init_cohort_bl
        cc%proot%c12 = inputs%init_cohort(i)%init_cohort_br
        cc%pseed%c12 = inputs%init_cohort(i)%init_cohort_seedC
        call cc%initialize_cohort_from_biomass()

      enddo

      ! Split initial layer in smaller layers (if it is full)
      call self%relayer()

    end if

    ! Initial Soil pools and environmental conditions
    self%psoil_fs%c12 = inputs%init_soil%init_fast_soil_C  ! kgC m-2
    self%psoil_sl%c12 = inputs%init_soil%init_slow_soil_C  ! slow soil carbon pool, (kg C/m2)
    self%psoil_fs%n14 = self%psoil_fs%c12 / CN0metabolicL  ! fast soil nitrogen pool, (kg N/m2)
    self%psoil_sl%n14 = self%psoil_sl%c12 / CN0structuralL ! slow soil nitrogen pool, (kg N/m2)
    self%inorg%n14    = inputs%init_soil%init_Nmineral     ! Mineral nitrogen pool, (kg N/m2)
    self%previousN    = self%inorg%n14

    ! debug: adding microbial biomass initialisation
    self%pmicr = orgpool() ! to do: add to: inputs%init_soil%xxxxx

    ! Initialize soil volumetric water conent with field capacity (maximum soil moisture to start with)
    self%wcl = inputs%params_tile%FLDCAP

    call self%aggregate_cohorts()

    self%initialN0 =  self%totN

  end subroutine initialize_vegn_tile

  !----------------------------------------------------------------
  ! Private helper methods
  !----------------------------------------------------------------

  subroutine aggregate_pools( self )
    !////////////////////////////////////////////////////////////////////////
    ! Compute tile-level pools from aggregation of pools from all living cohorts.
    !------------------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it !iterator

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
      self%plabl = self%plabl + cc%plabl * cc%density
      self%pleaf = self%pleaf + cc%pleaf * cc%density
      self%proot = self%proot + cc%proot * cc%density
      self%psapw = self%psapw + cc%psapw * cc%density
      self%pwood = self%pwood + cc%pwood * cc%density
      self%pseed = self%pseed + cc%pseed * cc%density

      it => it%next()

    enddo

  end subroutine aggregate_pools

  subroutine aggregate_cohorts( self )
    !////////////////////////////////////////////////////////////////////////
    ! Update tile-level variables from aggration of living cohorts.
    !------------------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self

    ! local variables
    type(cohort_type), pointer :: cc
    type(cohort_item), pointer :: it !iterator
    real :: dbh ! cache variable
    type(orgpool) :: total_pool

    self%LAI          = 0.0
    self%CAI          = 0.0
    self%density      = 0.0
    self%DBH          = 0.0
    self%density12    = 0.0
    self%DBH12        = 0.0
    self%QMD12        = 0.0
    self%MaxAge       = 0.0
    self%MaxVolume    = 0.0
    self%MaxDBH       = 0.0
    self%NPPL         = 0.0
    self%NPPW         = 0.0
    self%m_turnover   = 0.0

    call self%aggregate_pools()

    total_pool = self%pplant() + self%psoil()
    self%totN = total_pool%n14

    it => self%cohorts()
    do while (associated(it))
      cc => it%cohort

      self%NPPL         = self%NPPL          + cc%NPPleaf    * cc%density
      self%NPPW         = self%NPPW          + cc%NPPwood    * cc%density
      self%m_turnover   = self%m_turnover    + cc%m_turnover * cc%density

      self%CAI          = self%CAI      + cc%crownarea() * cc%density
      self%LAI          = self%LAI      + cc%leafarea()  * cc%density

      ! New tile outputs
      dbh = cc%dbh()
      self%DBH          = self%DBH      + dbh * cc%density
      self%density      = self%density  + cc%density

      if (dbh > 0.12) then
        self%DBH12      = self%DBH12     + dbh * cc%density
        self%density12  = self%density12 + cc%density
        self%QMD12  = self%QMD12 + dbh ** 2 * cc%density
      endif

      self%MaxAge    = MAX(cc%age, self%MaxAge)
      self%MaxVolume = MAX(cc%volume(), self%MaxVolume)
      self%MaxDBH    = MAX(dbh, self%MaxDBH)

      it => it%next()

    enddo

    if (self%density > 0.0) self%DBH   = self%DBH / self%density
    if (self%density12 > 0.0) self%DBH12 = self%DBH12 / self%density12
    if (self%density12 > 0.0) self%QMD12   = sqrt(self%QMD12 / self%density12)

  end subroutine aggregate_cohorts

  function lu_props(self) result(res)
    !////////////////////////////////////////////////////////////////////////
    ! Return land use (LU) properties (from init_lu) for this tile
    !------------------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    type(init_lu_biomee) :: res

    res = inputs%init_lu(self%lu_index)
  end function lu_props

  subroutine recover_N_balance(self)
    !////////////////////////////////////////////////////////////////////////
    ! We scale the N pools to constrain the yearly N (soil + plant) to be constant.
    ! ATTENTION: this is a hack which leads to negative N pools...
    !------------------------------------------------------------------------
    class(vegn_tile_type), intent(inout) :: self
    real :: delta

    delta = self%totN - self%initialN0

    if (abs(delta) > 1e-6) then
      self%psoil_sl%n14 = self%psoil_sl%n14 - delta
      self%totN = self%initialN0
    endif
  end subroutine recover_N_balance

end module md_vegetation_tile_biomee
