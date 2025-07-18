module md_params_core
  !////////////////////////////////////////////////////////////////
  ! Contains physical constants and other universal parameters used 
  ! for all model setups (BiomeE and P-model)
  !----------------------------------------------------------------
  implicit none

  integer, parameter :: ndayyear = 365           ! number of days in a year
  integer, parameter :: nhoursyear = 8760        ! number of days in a year
  integer, parameter :: nmonth = 12              ! number of months in a year
  real,    parameter :: secs_per_day = 86400.0   ! number of seconds in a day
  integer, parameter :: hours_per_year = ndayyear * 24
  real,    parameter :: seconds_per_year = ndayyear * secs_per_day

  ! From LM3-PPA
  integer, parameter :: nlayers_soil = 3         ! number of soil layers

  !===== Physical constants
  real, parameter :: mol_CO2  = 44.00995e-3           ! molar mass of CO2,kg
  real, parameter :: H2OLv0   = 2.501e6               ! latent heat H2O (J/kg)
  real, parameter :: DENS_H2O = 1000.                 ! kg m-3

  ! From SOFUN
  integer, parameter :: maxgrid = 1              ! number of spatial gridcells (dummy dimension for later code extension)
  integer, parameter :: nbucket = 2              ! number of buckets for soil water model
  integer, parameter :: npft = 1                 ! number of PFTs !3
  integer, parameter :: nlu = 1                  ! number of land units (tiles) !! ATTENTION: only for pmodel
  integer, parameter :: lunat = 1                ! ID of natural land unit
  integer, parameter :: lucrop = 2               ! ID of crop land unit

  integer, parameter, dimension(npft) :: pft_start = 1
  integer, parameter, dimension(npft) :: pft_end   = 1

  integer, parameter, dimension(nmonth)   :: ndaymonth = (/31,28,31,30,31,30,31,31,30,31,30,31/) ! number of days per month

  real, parameter :: pi = 3.14159265359          ! pi - what else?
  real, parameter :: c_molmass = 12.0107         ! g C / mol C
  real, parameter :: n_molmass = 14.0067         ! g N / mol N
  real, parameter :: h2o_molmass = 18.01528      ! g H2O / mol H2O
  real, parameter :: c_content_of_biomass = 0.46 ! gC / g-dry mass

  real, parameter :: kTkelvin = 273.15           ! freezing point in K (= 0 deg C) 
  real, parameter :: kTo = 298.15                ! base temperature, K (from P-model)
  real, parameter :: kR  = 8.31446262            ! universal gas constant, J/mol/K (Allen, 1973)
  real, parameter :: kMv = 18.02                 ! molecular weight of water vapor, g/mol (Tsilingiris, 2008)
  real, parameter :: kMa = 28.963                ! molecular weight of dry air, g/mol (Tsilingiris, 2008) XXX this was in SPLASH (WITH 1E-3 IN EQUATION) XXX
  real, parameter :: kfFEC = 2.04                ! from flux to energy conversion, umol/J (Meek et al., 1984)
  real, parameter :: kPo = 101325.0              ! standard atmosphere, Pa (Allen, 1973)
  real, parameter :: kL  = 0.0065                ! temperature lapse rate, K/m (Cavcar, 2000)
  real, parameter :: kG  = 9.80665               ! gravitational acceleration, m/s^2 (Allen, 1973)
  real, parameter :: k_karman = 0.41             ! Von Karman constant; from bigleaf R package
  real, parameter :: eps = 9.999e-6              ! numerical imprecision allowed in mass conservation tests
  real, parameter :: cp = 1.004834               ! specific heat of air for constant pressure (J K-1 g-1); from bigleaf R package
  real, parameter :: Rd = 287.0586               ! gas constant of dry air (J kg-1 K-1) (Foken 2008 p. 245; from bigleaf R package)
  real, parameter :: kGsc = 1360.8               ! solar constant (W m-2) (Kopp & Lean, 2011)

  ! needed here
  real, parameter :: dummy = -9999.0             ! arbitrary dummy value

  type outtype_steering
    integer :: year = 0           ! current simulation year
    integer :: climateyear        ! year AD for which climate is read in (recycling during spinup or when climate is held const.)
    integer :: climateyear_idx    ! year index for which climate is read in.
    integer :: forcingyear        ! year AD for which forcings are read in (=firstyeartrend during spinup)
    integer :: forcingyear_idx    ! year index for which forcings are read in (=1 during spinup)
    integer :: outyear            ! year AD written to output
    logical :: spinup             ! is true during spinup
    logical :: init     = .true.  ! is true in first simulation year
    logical :: finalize = .false. ! is true in the last simulation year
    logical :: daily_reporting    ! whether daily reporting should be done in the current year
    integer :: daily_report_idx   ! start_idx for output_annual_cohorts of current year
    logical :: cohort_reporting   ! whether cohort level reporting should be done in the current year
    integer :: cohort_report_idx  ! start_idx for output_daily_tile of current year
    ! Note: climateyear_idx == forcingyear_idx during transient phase. During spinup however, climateyear cycles, while forcing year is constant (= 1).
  end type outtype_steering

  type steering_parameters

    integer :: spinupyears      ! number of spinup years
    integer :: nyeartrend       ! number of transient years
    integer :: firstyeartrend   ! year AD of first transient year
    integer :: recycle          ! length of standard recycling period
    logical :: do_spinup        ! whether this simulation does spinup
    integer :: runyears         ! number of years of entire simulation (spinup+transient)
    logical :: do_daily_reporting ! whether this simulation reports daily values

  end type steering_parameters

contains

  pure function get_steering( year, steering_input ) result( curr_year_steering_state )
    !////////////////////////////////////////////////////////////////
    ! Gets variables used for steering simulation for each
    ! simulation year (setting booleans for opening files, doing
    ! spinup etc.)
    !----------------------------------------------------------------

    ! arguments
    integer, intent(in) :: year ! simulation year, starts counting from 1, starting at the beginning of spinup
    type(steering_parameters), intent(in) :: steering_input

    ! function return variable
    type(outtype_steering) :: curr_year_steering_state

    ! local variables
    integer :: cycleyear

    curr_year_steering_state%year = year

    if (steering_input%do_spinup) then
      if (year <= steering_input%spinupyears) then
        ! during spinup
        cycleyear = get_cycleyear( year, steering_input%spinupyears, steering_input%recycle )

        curr_year_steering_state%spinup          = .true.
        curr_year_steering_state%climateyear     = cycleyear + steering_input%firstyeartrend - 1
        curr_year_steering_state%climateyear_idx = cycleyear
        curr_year_steering_state%forcingyear     = steering_input%firstyeartrend
        curr_year_steering_state%forcingyear_idx = 1
      else
        ! during transient simulation
        curr_year_steering_state%spinup          = .false.
        curr_year_steering_state%climateyear_idx = year - steering_input%spinupyears
        curr_year_steering_state%climateyear     = curr_year_steering_state%climateyear_idx + steering_input%firstyeartrend - 1
        curr_year_steering_state%forcingyear     = curr_year_steering_state%climateyear
        curr_year_steering_state%forcingyear_idx = curr_year_steering_state%climateyear_idx
      endif
      curr_year_steering_state%outyear           = year + steering_input%firstyeartrend - steering_input%spinupyears - 1
    else
      curr_year_steering_state%spinup          = .false.
      curr_year_steering_state%climateyear     = year + steering_input%firstyeartrend - 1
      curr_year_steering_state%climateyear_idx = year
      curr_year_steering_state%forcingyear     = curr_year_steering_state%climateyear
      curr_year_steering_state%forcingyear_idx = curr_year_steering_state%climateyear_idx
      curr_year_steering_state%outyear         = curr_year_steering_state%climateyear
    endif

    if (curr_year_steering_state%spinup)  then
      curr_year_steering_state%daily_reporting  = .false.
      curr_year_steering_state%cohort_reporting = .false.
    else
      curr_year_steering_state%daily_reporting  = steering_input%do_daily_reporting
      curr_year_steering_state%cohort_reporting = .true.
    end if

    ! Indices for daily and cohort output
    ! Spinup years are not stored, which is why we offset by - spinupyears
    curr_year_steering_state%daily_report_idx = (curr_year_steering_state%year - steering_input%spinupyears - 1) * ndayyear + 1
    curr_year_steering_state%cohort_report_idx = curr_year_steering_state%year - steering_input%spinupyears
        
    if (year==1) then
      curr_year_steering_state%init = .true.
    else
      curr_year_steering_state%init = .false.
    endif

    if (year==steering_input%runyears) then
      curr_year_steering_state%finalize = .true.
    else
      curr_year_steering_state%finalize = .false.
    end if

  end function get_steering

  pure function get_cycleyear( year, spinupyears, recycle ) result( cycleyear )
  !////////////////////////////////////////////////////////////////
  ! Returns cycle year for climate recycling, given number of spinup
  ! years and recycle period length, so that in the last year of
  ! of the spinup, 'cycleyear' is equal to 'recycle'.
  !----------------------------------------------------------------
    ! arguments
    integer, intent(in) :: year
    integer, intent(in) :: spinupyears
    integer, intent(in) :: recycle

    ! local variables
    integer :: remainder
    integer :: first_cycleyear

    ! function return variable
    integer :: cycleyear

    remainder = mod( spinupyears, recycle )
    first_cycleyear = recycle - remainder - 1
    cycleyear = modulo( first_cycleyear + year, recycle ) + 1

  end function get_cycleyear

end module md_params_core