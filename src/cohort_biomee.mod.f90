module md_cohort
  !////////////////////////////////////////////////////////////////
  ! Module containing BiomeE cohort definitions
  !----------------------------------------------------------------
  use md_interface_biomee, only: myinterface, spec_data_type, MAX_LEVELS
  use md_params_core, only: pi, eps
  use md_classdefs

  ! define data types and constants
  implicit none
  private
  !=============== Public types ===========================================================
  public :: cohort_type

  !=============== Public subroutines =====================================================


  integer, public, parameter :: LEAF_OFF                 = 0
  integer, public, parameter :: LEAF_ON                  = 1
  integer, public, parameter :: PT_C3                    = 0                     ! physiology types
  integer, public, parameter :: PT_C4                    = 1                     ! physiology types

  !=============== Cohort level data type =============================================================
  type :: cohort_type

    !======================== State
    ! The cohort state contains cohort properties which persist across the years

    !===== Metadata
    integer :: layer      = 1            ! the layer of this cohort (numbered from top, top layer=1)
    integer :: firstlayer = 0            ! 0 = never been in the first layer; 1 = at least one year in first layer
    integer :: species    = 1            ! vegetation species

    !===== Population structure
    real :: nindivs       = 0.0          ! density of vegetation, tree/m2
    real :: deathrate     = 0.0          ! Deathrate (0 to 1) of the original cohort. Only set for cohorts in the killed list.
    real :: age           = 0.0          ! age of cohort, years
    real :: topyear       = 0.0          ! number of years the cohort is in top layer

    !===== Biological prognostic variables
    real    :: gdd        = 0.0          ! growing degree-day (phenology)
    integer :: status     = LEAF_OFF     ! growth status of plant
    real    :: leaf_age   = 0.0          ! leaf age (years)

    !===== Organic pools, kg tree-1
    type(orgpool) :: pleaf               ! leaf biomass
    type(orgpool) :: proot               ! root biomass
    type(orgpool) :: psapw               ! sapwood biomass
    type(orgpool) :: pwood               ! heartwood (non-living) biomass
    type(orgpool) :: pseed               ! biomass put aside for future progeny
    type(orgpool) :: plabl               ! labile pool, temporary storage of N and C

    !===== Nitrogen model related variables (persistent from year to year), kg C tree-1
    real    :: bl_max     = 0.0          ! Max. leaf biomass
    real    :: br_max     = 0.0          ! Max. fine root biomass

    !=================== Temporary variables
    ! Contrary to the state vairbales, temporary variables are reset every step, day, or year (as appropriate).

    !===== Fast step fluxes, kg timestep-1 tree-1
    type(common_fluxes) :: fast_fluxes
    real    :: resl               = 0.0           ! leaf respiration
    real    :: resr               = 0.0           ! root respiration

    !===== Daily fluxes, kg day-1 tree-1
    type(common_fluxes) :: daily_fluxes
    real    :: C_growth           = 0.0           ! Carbon gain since last growth
    real    :: N_growth           = 0.0           ! Nitrogen used for plant tissue growth
    real    :: resg               = 0.0           ! growth respiration

    !===== Annual fluxes, kg yr-1 tree-1
    type(common_fluxes) :: annual_fluxes
    real    :: NPPleaf            = 0.0           ! C allocated to leaf
    real    :: NPProot            = 0.0           ! C allocated to root
    real    :: NPPwood            = 0.0           ! C allocated to wood
    real    :: m_turnover         = 0.0           ! C turnover due to tissue turnover

    !===== Water uptake-related variables
    real    :: WupL(MAX_LEVELS)   = 0.0           ! normalized vertical distribution of uptake

    !===== Memory variables used for computing deltas
    real    :: DBH_ys            = 0.0            ! DBH at the begining of a year (growing season)
    real    :: BA_ys             = 0.0            ! Basal area at the beginning og a year
    
    contains

      !========= Derived variables
      ! Variables which are function of any state or temporary variable defined above.
      ! They can be used like any normal variable except that they are followed by parenthesis
      ! (indicating that they are being evaluated every time).
      ! The advantage is that they cannot be out-of-sync with the underlying data, at the cost of a negligeable
      ! increase in the number of operations.

      procedure NSNmax
      procedure leafarea
      procedure crownarea
      procedure height
      procedure dbh
      procedure W_supply
      procedure rootareaL
      procedure lai
      procedure basal_area
      procedure volume
      procedure merge_in
      procedure layerfrac
      procedure sp

      !========== Other member procedures

      procedure reset_cohort
      procedure initialize_cohort_from_biomass
      procedure init_bl_br
      procedure can_be_merged_with

  end type cohort_type

contains

  pure function sp(self) result(res)
    type(spec_data_type) :: res
    class(cohort_type), intent(in) :: self

    res = myinterface%params_species(self%species)
  end function sp

  subroutine merge_in(self, other)
    !////////////////////////////////////////////////////////////////
    ! Merge other cohort into self
    !---------------------------------------------------------------
    class(cohort_type), intent(inout) :: self
    type(cohort_type), intent(in) :: other
    real :: x1, x2 ! normalized relative weights

    if (other%nindivs > 0.0 .or. self%nindivs > 0.0) then

      x1 = other%nindivs / (other%nindivs + self%nindivs)
      x2 = self%nindivs / (other%nindivs + self%nindivs)

      ! update number of individuals in merged cohort
      self%nindivs = other%nindivs + self%nindivs

      ! Carbon
      self%pleaf%c%c12 = x1*other%pleaf%c%c12 + x2*self%pleaf%c%c12
      self%proot%c%c12 = x1*other%proot%c%c12 + x2*self%proot%c%c12
      self%psapw%c%c12 = x1*other%psapw%c%c12 + x2*self%psapw%c%c12
      self%pwood%c%c12 = x1*other%pwood%c%c12 + x2*self%pwood%c%c12
      self%pseed%c%c12 = x1*other%pseed%c%c12 + x2*self%pseed%c%c12
      self%plabl%c%c12 = x1*other%plabl%c%c12 + x2*self%plabl%c%c12

      ! Nitrogen
      self%pleaf%n%n14 = x1*other%pleaf%n%n14 + x2*self%pleaf%n%n14
      self%proot%n%n14 = x1*other%proot%n%n14 + x2*self%proot%n%n14
      self%psapw%n%n14 = x1*other%psapw%n%n14 + x2*self%psapw%n%n14
      self%pwood%n%n14 = x1*other%pwood%n%n14 + x2*self%pwood%n%n14
      self%pseed%n%n14 = x1*other%pseed%n%n14 + x2*self%pseed%n%n14
      self%plabl%n%n14 = x1*other%plabl%n%n14 + x2*self%plabl%n%n14

      ! Cohort age
      self%age = x1*other%age + x2*self%age
      self%topyear = x1*other%topyear + x2*self%topyear
    endif

  end subroutine merge_in

  subroutine reset_cohort(self)
    ! Reset cohort temporary data (used yearly)
    class(cohort_type), intent(inout) :: self

    ! Save last year's values
    self%DBH_ys        = self%dbh()
    self%BA_ys         = basal_area(self)

    self%WupL(:)       = 0.0

    self%N_growth      = 0.0
    self%N_growth      = 0.0

    self%resl          = 0.0
    self%resr          = 0.0
    self%resg          = 0.0

    self%fast_fluxes   = common_fluxes()

    ! Reset daily
    self%daily_fluxes  = common_fluxes()

    ! annual
    self%annual_fluxes = common_fluxes()
    self%NPPleaf       = 0.0
    self%NPProot       = 0.0
    self%NPPwood       = 0.0

    self%m_turnover    = 0.0
    self%deathrate     = 0.0
  end subroutine reset_cohort

  pure function NSNmax(self) result(res)
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(spec_data_type) :: sp

    sp = self%sp()

    res = sp%fNSNmax * &
            (self%bl_max / (sp%CNleaf0 * sp%leafLS) + self%br_max / sp%CNroot0)
  end function NSNmax

  pure function dbh(self) result(res)
  ! diameter at breast height, m
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(spec_data_type) :: sp
    real :: btot

    btot = max(0.0001, self%pwood%c%c12 + self%psapw%c%c12)
    sp = self%sp()

    res = (btot / sp%alphaBM) ** ( 1.0/sp%thetaBM )
  end function dbh

  pure function crownarea(self) result(res)
  ! crown area, m2 tree-1
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(spec_data_type) :: sp

    sp = self%sp()

    res = sp%alphaCA * self%dbh() ** sp%thetaCA
  end function crownarea

  pure function leafarea(self) result(res)
  ! total area of leaves, m2 tree-1
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(spec_data_type) :: sp

    sp = self%sp()

    res = self%pleaf%c%c12 / sp%LMA
  end function leafarea

  pure function height(self) result(res)
    ! vegetation height, m
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(spec_data_type) :: sp

    sp = self%sp()

    res = sp%alphaHT * self%dbh() ** sp%thetaHT
  end function height

  pure function W_supply(self) result(res)
    ! potential water uptake rate per unit time per tree
    real :: res
    class(cohort_type), intent(in) :: self

    res = sum(self%WupL(:))
  end function W_supply

  pure function rootareaL(self, level) result(res)
    ! Root length per layer, m of root/m
    real :: res
    integer, intent(in) :: level
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(spec_data_type) :: sp

    sp = self%sp()

    res = rootarea(self) * sp%root_frac(level)
  end function rootareaL

  pure function rootarea(self) result(res)
    ! total fine root area per tree
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(spec_data_type) :: sp

    sp = self%sp()

    res  = self%proot%c%c12 * sp%SRA
  end function rootarea

  pure function lai(self) result(res)
    ! Leaf area index: surface of leaves per m2 of crown
    real :: res
    class(cohort_type), intent(in) :: self

    res = self%leafarea() / self%crownarea() !(cohort%crownarea() *(1.0-sp%internal_gap_frac))
  end function lai

  pure function basal_area(self) result(res)
    ! Tree basal area, m2 tree-1
    real :: res
    class(cohort_type), intent(in) :: self

    res = pi/4 * self%dbh() * self%dbh()
  end function basal_area

  pure function volume(self) result(res)
    ! Tree basal volume, m3 tree-1
    real :: res
    class(cohort_type), intent(in) :: self
    ! Local variable
    type(spec_data_type) :: sp

    sp = self%sp()

    res = (self%psapw%c%c12 + self%pwood%c%c12) / sp%rho_wood
  end function volume

  pure function layerfrac(self) result(res)
    ! Fraction of layer area occupied by this cohort
    real :: res
    class(cohort_type), intent(in) :: self

    res = self%nindivs * self%crownarea()
  end function layerfrac

  subroutine initialize_cohort_from_biomass(self)
    !////////////////////////////////////////////////////////////////
    ! calculate tree height, DBH, height, and crown area by initial biomass
    ! The allometry equations are from Ray Dybzinski et al. 2011 and Forrior et al. in review
    !         HT = alphaHT * DBH ** (gamma-1)   ! DBH --> Height
    !         CA = alphaCA * DBH ** gamma       ! DBH --> Crown Area
    !         BM = alphaBM * DBH ** (gamma + 1) ! DBH --> tree biomass
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    class(cohort_type), intent(inout) :: self

    ! Local variable
    type(spec_data_type) :: sp

    sp = self%sp()

    call self%init_bl_br()

    self%plabl%c%c12  = 2.0 * (self%bl_max + self%br_max)

    ! N pools
    self%plabl%n%n14  = 5.0 * (self%bl_max / sp%CNleaf0 + self%br_max / sp%CNroot0)
    self%pleaf%n%n14  = self%pleaf%c%c12 / sp%CNleaf0
    self%proot%n%n14  = self%proot%c%c12 / sp%CNroot0
    self%psapw%n%n14  = self%psapw%c%c12 / sp%CNsw0
    self%pwood%n%n14  = self%pwood%c%c12 / sp%CNwood0
    self%pseed%n%n14  = self%pseed%c%c12 / sp%CNseed0

  end subroutine initialize_cohort_from_biomass

  subroutine init_bl_br( self )
    !////////////////////////////////////////////////////////////////
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    class(cohort_type), intent(inout) :: self

    ! Local variable
    type(spec_data_type) :: sp
    real :: crownarea ! Cache variable

    sp = self%sp()

    crownarea = self%crownarea()

    ! calculations of bl_max and br_max are here only for the sake of the
    ! diagnostics, because otherwise those fields are inherited from the
    ! parent cohort and produce spike in the output, even though these spurious
    ! values are not used by the model
    self%bl_max = sp%LMA   * sp%LAImax        * crownarea / self%layer
    self%br_max = sp%phiRL * sp%LAImax/sp%SRA * crownarea / self%layer

  end subroutine init_bl_br

  function can_be_merged_with(self, other) result(res)
    !////////////////////////////////////////////////////////////////
    ! Code from BiomeE-Allocation
    !---------------------------------------------------------------
    class(cohort_type), intent(inout) :: self
    logical :: res
    type(cohort_type) :: other

    ! Local variables
    logical :: sameSpecies, sameLayer, sameSize, sameSizeTree, sameSizeGrass
    real :: self_dbh, other_dbh, dbh_diff

    self_dbh = self%dbh()
    other_dbh = other%dbh()
    dbh_diff = abs(self_dbh - other_dbh)

    associate (spdata => myinterface%params_species)

      sameSpecies  = self%species == other%species

      sameLayer    = (self%layer == other%layer) .or. &
              ((spdata(self%species)%lifeform == 0) .and. &
                      (spdata(other%species)%lifeform == 0) .and. &
                      (self%layer > 1 .and. other%layer > 1))

      sameSizeTree = (spdata(self%species)%lifeform > 0).and.  &
              (spdata(other%species)%lifeform > 0).and.  &
              ((dbh_diff/(self_dbh + other_dbh) < 0.1 ) .or.  &
                      (dbh_diff < 0.001))  ! it'll be always true for grasses

      sameSizeGrass= (spdata(self%species)%lifeform == 0) .and. &
              (spdata(other%species)%lifeform == 0) .and. &
              (dbh_diff < eps .and. self%age > 2. .and. other%age > 2.)  ! it'll be always true for grasses

      sameSize = sameSizeTree .or. sameSizeGrass

      res = sameSpecies .and. sameLayer .and. sameSize

    end associate

  end function

end module md_cohort
