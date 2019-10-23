module md_params_core
  !////////////////////////////////////////////////////////////////
  ! This module contains parameters that are not modified, but needed
  ! to define variables, dimension lengths, etc.
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  implicit none

  integer, parameter :: ndayyear = 365           ! number of days in a year
  integer, parameter :: nmonth = 12              ! number of months in a year
  real,    parameter :: secs_per_day = 86400.0   ! number of seconds in a day
  integer, parameter :: maxgrid = 1              ! number of spatial gridcells (dummy dimension for later code extension)
  integer, parameter :: nbucket = 2              ! number of buckets for soil water model
  integer, parameter :: npft = 1                 ! number of PFTs
  integer, parameter :: nlu = 1                  ! number of land units (tiles)
  integer, parameter :: lunat = 1                ! ID of natural land unit
  integer, parameter :: lucrop = 2               ! ID of crop land unit
  integer, parameter :: nlayers_soil = 2         ! number of soil layers

  integer, parameter, dimension(npft) :: pft_start = 1
  integer, parameter, dimension(npft) :: pft_end   = 1

  integer, parameter, dimension(nmonth)   :: ndaymonth = (/31,28,31,30,31,30,31,31,30,31,30,31/) ! number of days per month
  integer, parameter, dimension(nmonth+1) :: middaymonth = (/16,44,75,105,136,166,197,228,258,289,319,350,381/) ! day of year of middle-month-day
  integer, parameter, dimension(nmonth)   :: cumdaymonth = (/31,59,90,120,151,181,212,243,273,304,334,365/)

  real, parameter :: pi = 3.14159265359          ! pi - what else?
  real, parameter :: c_molmass = 12.0107         ! g C / mol C
  real, parameter :: n_molmass = 14.0067         ! g N / mol N
  real, parameter :: h2o_molmass = 44.013        ! g H2O / mol H2O
  real, parameter :: c_content_of_biomass = 0.46 ! gC / g-dry mass

  ! The following parameters are from SPLASH. 
  ! Defined here because parameters are used by climate input data unit conversions and
  ! because these are well-known parameters.
  ! XXX this was in SPLASH: real, parameter :: kTo = 288.15                ! base temperature, K (Prentice, unpublished)
  real, parameter :: kTo = 298.15                ! base temperature, K (from P-model)
  real, parameter :: kR  = 8.3145                ! universal gas constant, J/mol/K (Allen, 1973)
  real, parameter :: kMv = 18.02                 ! molecular weight of water vapor, g/mol (Tsilingiris, 2008)
  ! XXX this was in SPLASH (WITH 1E-3 IN EQUATION) real, parameter :: kMa = 28.963                ! molecular weight of dry air, g/mol (Tsilingiris, 2008)
  real, parameter :: kMa = 0.028963 ! molecular weight of dry air, kg/mol (Tsilingiris, 2008)
  real, parameter :: kfFEC = 2.04                ! from flux to energy conversion, umol/J (Meek et al., 1984)
  real, parameter :: kPo = 101325                ! standard atmosphere, Pa (Allen, 1973)
  real, parameter :: kL  = 0.0065                ! temperature lapse rate, K/m (Cavcar, 2000)
  real, parameter :: kG  = 9.80665               ! gravitational acceleration, m/s^2 (Allen, 1973)

  real, parameter :: eps = 9.999e-6              ! numerical imprecision allowed in mass conservation tests
  real, parameter :: dummy = -9999.0             ! arbitrary dummy value

end module md_params_core
