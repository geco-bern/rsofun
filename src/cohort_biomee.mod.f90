module md_cohort
  !////////////////////////////////////////////////////////////////
  ! Derived type for modeling Biomee cohorts
  !----------------------------------------------------------------
  use md_interface_in_biomee, only: inputs, params_species_biomee, MAX_LEVELS
  use md_params_core, only: pi, eps
  use md_orgpool
  use md_common_fluxes

  ! define data types and constants
  implicit none
  private
  !=============== Public types ===========================================================
  public :: cohort_type

  !=============== Public subroutines =====================================================


  integer, public, parameter :: LEAF_OFF                 = 0   ! phenology
  integer, public, parameter :: LEAF_ON                  = 1   ! phenology
  integer, public, parameter :: PT_C3                    = 0   ! physiology types
  integer, public, parameter :: PT_C4                    = 1   ! physiology types

  !=============== Cohort level data type =============================================================
  type :: cohort_type

    !======================== State
    ! The cohort state contains cohort properties which persist from year to years

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
      ! The advantage is that they cannot be out-of-sync with the underlying data.
      ! One might think that this will increase the number of operations, but it is impossible to tell
      ! as the compiler may or may not optimize the compiled code in such a way that function calls are avoided.
      ! On the opposite, more structure and constrains in the code give the compiler more opportunities for optimizations and
      ! so far, the use of functions has actually sped up the execution time quite a lot.

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

  !----------------------------------------------------------------
  ! Derived variables
  !----------------------------------------------------------------

  pure function sp(self) result(res)
    ! Returns the species parameters
    type(params_species_biomee) :: res
    class(cohort_type), intent(in) :: self

    res = inputs%params_species(self%species)
  end function sp

  pure function NSNmax(self) result(res)
    ! NSNmax
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(params_species_biomee) :: sp

    sp = self%sp()

    res = sp%fNSNmax * &
            (self%bl_max / (sp%CNleaf0 * sp%leafLS) + self%br_max / sp%CNroot0)
  end function NSNmax

  pure function dbh(self) result(res)
    ! Diameter at breast height, m
    ! Allometry equation from Ray Dybzinski et al. 2011 and Forrior et al. in review
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(params_species_biomee) :: sp
    real :: btot

    btot = max(0.0001, self%pwood%c12 + self%psapw%c12)
    sp = self%sp()

    res = (btot / sp%alphaBM) ** ( 1.0/sp%thetaBM )
  end function dbh

  pure function height(self) result(res)
    ! Vegetation height, m
    ! Allometry equation from Ray Dybzinski et al. 2011 and Forrior et al. in review
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(params_species_biomee) :: sp

    sp = self%sp()

    res = sp%alphaHT * self%dbh() ** sp%thetaHT
  end function height

  pure function crownarea(self) result(res)
    ! Crown area, m2 tree-1
    ! Allometry equation from Ray Dybzinski et al. 2011 and Forrior et al. in review
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(params_species_biomee) :: sp

    sp = self%sp()

    res = sp%alphaCA * self%dbh() ** sp%thetaCA
  end function crownarea

  pure function leafarea(self) result(res)
    ! Total area of leaves, m2 tree-1
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(params_species_biomee) :: sp

    sp = self%sp()

    res = self%pleaf%c12 / sp%LMA
  end function leafarea

  pure function W_supply(self) result(res)
    ! Potential water uptake rate per unit time per tree
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
    type(params_species_biomee) :: sp

    sp = self%sp()

    res = rootarea(self) * sp%root_frac(level)
  end function rootareaL

  pure function rootarea(self) result(res)
    ! Total fine root area per tree
    real :: res
    class(cohort_type), intent(in) :: self

    ! Local variable
    type(params_species_biomee) :: sp

    sp = self%sp()

    res  = self%proot%c12 * sp%SRA
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
    type(params_species_biomee) :: sp

    sp = self%sp()

    res = (self%psapw%c12 + self%pwood%c12) / sp%rho_wood
  end function volume

  pure function layerfrac(self) result(res)
    ! Fraction of layer area occupied by this cohort
    real :: res
    class(cohort_type), intent(in) :: self

    res = self%nindivs * self%crownarea()
  end function layerfrac

  !----------------------------------------------------------------
  ! Cohort interaction procedures
  !----------------------------------------------------------------

  pure subroutine merge_in(self, other)
    !////////////////////////////////////////////////////////////////
    ! Merge cohort 'other' into this cohort ('sef')
    ! The pools and density of 'self' are updated with an average of both cohorts.
    ! 'other' is not modified and should be destroyed after calling this function.
    !---------------------------------------------------------------
    class(cohort_type), intent(inout) :: self
    type(cohort_type), intent(in) :: other
    real :: x1, x2 ! normalized relative weights

    if (other%nindivs > 0.0 .or. self%nindivs > 0.0) then

      x1 = other%nindivs / (other%nindivs + self%nindivs)
      x2 = self%nindivs / (other%nindivs + self%nindivs)

      ! update number of individuals in merged cohort
      self%nindivs = other%nindivs + self%nindivs

      ! Average pools
      self%pleaf = other%pleaf * x1 + self%pleaf * x2
      self%proot = other%proot * x1 + self%proot * x2
      self%psapw = other%psapw * x1 + self%psapw * x2
      self%pwood = other%pwood * x1 + self%pwood * x2
      self%pseed = other%pseed * x1 + self%pseed * x2
      self%plabl = other%plabl * x1 + self%plabl * x2

      ! Cohort age
      self%age = x1 * other%age + x2 * self%age
      self%topyear = x1 * other%topyear + x2 * self%topyear
    endif

  end subroutine merge_in

  pure subroutine reset_cohort(self)
    !////////////////////////////////////////////////////////////////
    ! Reset cohort temporary data (used yearly)
    !---------------------------------------------------------------
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

  pure function can_be_merged_with(self, other) result(res)
    !////////////////////////////////////////////////////////////////
    ! Check if the conditions for merging this cohort with cohort 'other' are met.
    ! To cohorts can be merged if they belong to the same species and layer, and have roughly the same size.
    !---------------------------------------------------------------
    class(cohort_type), intent(in) :: self
    type(cohort_type), intent(in) :: other
    logical :: res

    ! Local variables
    logical :: sameSpecies, sameLayer, sameSize, sameSizeTree, sameSizeGrass
    real :: self_dbh, other_dbh, dbh_diff

    self_dbh = self%dbh()
    other_dbh = other%dbh()
    dbh_diff = abs(self_dbh - other_dbh)

    associate (spdata => inputs%params_species)

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

  !----------------------------------------------------------------
  ! Other helper functions
  !----------------------------------------------------------------

  pure subroutine initialize_cohort_from_biomass(self)
    !////////////////////////////////////////////////////////////////
    ! Calculate initial biomass
    !---------------------------------------------------------------
    class(cohort_type), intent(inout) :: self

    ! Local variable
    type(params_species_biomee) :: sp

    sp = self%sp()

    call self%init_bl_br()

    self%plabl%c12 = 2.0 * (self%bl_max + self%br_max)

    ! N pools
    self%plabl%n14 = 5.0 * (self%bl_max / sp%CNleaf0 + self%br_max / sp%CNroot0)
    self%pleaf%n14 = self%pleaf%c12 / sp%CNleaf0
    self%proot%n14 = self%proot%c12 / sp%CNroot0
    self%psapw%n14 = self%psapw%c12 / sp%CNsw0
    self%pwood%n14 = self%pwood%c12 / sp%CNwood0
    self%pseed%n14 = self%pseed%c12 / sp%CNseed0

  end subroutine initialize_cohort_from_biomass

  pure subroutine init_bl_br( self )
    !////////////////////////////////////////////////////////////////
    ! Initialize bl_max and br_max
    !---------------------------------------------------------------
    class(cohort_type), intent(inout) :: self

    ! Local variable
    type(params_species_biomee) :: sp
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

end module md_cohort
