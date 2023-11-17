module md_params_core
  !////////////////////////////////////////////////////////////////
  ! Contains physical constants and other universal parameters used 
  ! for all model setups (BiomeE and P-model)
  !----------------------------------------------------------------
  implicit none

  integer :: ntstepsyear                         ! 365 when daily
  integer, parameter :: ndayyear = 365           ! number of days in a year
  integer, parameter :: nhoursyear = 8760        ! number of days in a year
  integer, parameter :: nmonth = 12              ! number of months in a year
  real,    parameter :: secs_per_day = 86400.0   ! number of seconds in a day
  integer, parameter :: hours_per_year = 365 * 24  
  real,    parameter :: seconds_per_year = 365. * 24. * 3600.

  ! From LM3-PPA
  integer, parameter :: nlayers_soil = 3         ! number of soil layers
  integer, parameter :: n_dim_soil_types = 9     ! number of soil types
  integer, parameter :: MSPECIES = 15            ! number of species
  integer, parameter :: MAX_INIT_COHORTS = 10    ! Number of initial cohorts
  integer, parameter :: out_max_cohorts = 50     ! Number of maximum cohorts

  integer, parameter :: nvars_hourly_tile = 15
  integer, parameter :: nvars_daily_tile = 35
  integer, parameter :: nvars_daily_cohorts = 27
  integer, parameter :: nvars_annual_tile = 59
  integer, parameter :: nvars_annual_cohorts = 29

  !===== Physical constants
  real, parameter :: mol_CO2  = 44.00995e-3           ! molar mass of CO2,kg
  real, parameter :: H2OLv0   = 2.501e6               ! latent heat H2O (J/kg)
  real, parameter :: DENS_H2O = 1000.                 ! kg m-3

  ! From SOFUN
  integer, parameter :: maxgrid = 1              ! number of spatial gridcells (dummy dimension for later code extension)
  integer, parameter :: nbucket = 2              ! number of buckets for soil water model
  integer, parameter :: npft = 1                 ! number of PFTs !3
  integer, parameter :: nlu = 1                  ! number of land units (tiles)
  integer, parameter :: lunat = 1                ! ID of natural land unit
  integer, parameter :: lucrop = 2               ! ID of crop land unit

  integer, parameter, dimension(npft) :: pft_start = 1
  integer, parameter, dimension(npft) :: pft_end   = 1

  integer, parameter, dimension(nmonth)   :: ndaymonth = (/31,28,31,30,31,30,31,31,30,31,30,31/) ! number of days per month
  integer, parameter, dimension(nmonth+1) :: middaymonth = (/16,44,75,105,136,166,197,228,258,289,319,350,381/) ! day of year of middle-month-day
  integer, parameter, dimension(nmonth)   :: cumdaymonth = (/31,59,90,120,151,181,212,243,273,304,334,365/)

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

end module md_params_core