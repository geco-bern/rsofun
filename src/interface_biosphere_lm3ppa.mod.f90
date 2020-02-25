module md_interface_lm3ppa

  use, intrinsic :: iso_fortran_env, dp=>real64

  use md_forcing_lm3ppa, only: climate_type
  use md_params_soil_lm3ppa, only: paramtype_soil, getsoil
  use md_params_siml_lm3ppa, only: paramstype_siml, outtype_steering
  use md_params_core_lm3ppa, only: MSPECIES, ntstepsyear, ndayyear, MAX_INIT_COHORTS, out_max_cohorts
  use md_grid, only: gridtype !, domaininfo_type

  implicit none

  private
  public  myinterface, interfacetype_biosphere, outtype_biosphere, outtype_hourly_tile, &
    outtype_annual_cohorts, outtype_daily_cohorts, outtype_daily_tile, outtype_annual_tile

  type paramstype_tile
    integer:: soiltype
    real   :: FLDCAP
    real   :: WILTPT
    real   :: K1
    real   :: K2
    real   :: K_nitrogen
    real   :: etaN
    real   :: MLmixRatio
    real   :: l_fract
    real   :: retransN
    real   :: f_N_add
    real   :: f_initialBSW
  end type paramstype_tile
  
  type paramstype_species
    integer :: lifeform
    integer :: phenotype
    integer :: pt
    real :: seedlingsize
    real :: LMA
    real :: phiRL
    real :: LNbase
    real :: laimax
    real :: LAI_light
    real :: Nfixrate0
    real :: NfixCost0
    real :: phiCSA
    real :: mortrate_d_c
    real :: mortrate_d_u
    real :: maturalage
    real :: fNSNmax
  end type paramstype_species

  type inittype_cohort 
    real :: init_cohort_species
    real :: init_cohort_nindivs
    real :: init_cohort_bsw
    real :: init_cohort_bHW
    real :: init_cohort_nsc
  end type inittype_cohort

  type inittype_soil 
    real :: init_fast_soil_C
    real :: init_slow_soil_C
    real :: init_Nmineral
    real :: N_input
  end type inittype_soil

  type interfacetype_biosphere
    integer                                           :: year
    real, dimension(:), allocatable                   :: pco2
    type(gridtype)                                    :: grid
    type(climate_type), dimension(:), allocatable     :: climate
    type(outtype_steering)                            :: steering
    type(paramstype_siml)                             :: params_siml
    real, dimension(:), allocatable                   :: fpc_grid   ! allocatable because we don't know number of PFTs a priori
    ! type(paramstype_calib)                          :: params_calib    ! calibratable parameters
    type(paramstype_species), dimension(0:MSPECIES)   :: params_species
    type(paramtype_soil)                              :: params_soil
    type(paramstype_tile)                             :: params_tile
    type(inittype_cohort), dimension(MAX_INIT_COHORTS):: init_cohort
    type(inittype_soil)                               :: init_soil
    integer                                           :: datalines
    integer                                           :: steps_per_day
    real                                              :: dt_fast_yr
    real                                              :: step_seconds
  end type interfacetype_biosphere

  type(interfacetype_biosphere) :: myinterface

  !----------------------------------------------------------------
  ! Return variable of biosphere()
  !----------------------------------------------------------------
  ! This is the derived type-return variable of the function biosphere(),
  ! holding variables used for the cost function in sofun_calib.f90
  
  type outtype_hourly_tile ! corresponds to file fno1
    real :: year
    real :: doy
    real :: hour
    real :: rad
    real :: Tair
    real :: Prcp
    real :: GPP
    real :: Resp
    real :: Transp
    real :: Evap
    real :: Runoff
    real :: Soilwater
    real :: wcl
    real :: FLDCAP
    real :: WILTPT
  end type outtype_hourly_tile

  type outtype_daily_tile   !fno4
    real :: year 
    real :: doy
    real :: Tc
    real :: Prcp
    real :: totWs
    real :: Trsp
    real :: Evap
    real :: Runoff
    real :: ws1
    real :: ws2
    real :: ws3
    real :: LAI
    real :: GPP
    real :: Rauto
    real :: Rh
    real :: NSC
    real :: seedC
    real :: leafC
    real :: rootC
    real :: SW_C
    real :: HW_C
    real :: NSN
    real :: seedN
    real :: leafN
    real :: rootN
    real :: SW_N
    real :: HW_N
    real :: McrbC
    real :: fastSOM
    real :: slowSOM
    real :: McrbN
    real :: fastSoilN
    real :: slowSoilN
    real :: mineralN
    real :: N_uptk
  end type outtype_daily_tile  

  type outtype_daily_cohorts !fno3
    real :: year
    real :: doy
    real :: hour
    real :: cID
    real :: PFT
    real :: layer
    real :: density
    real :: f_layer
    real :: LAI
    real :: gpp
    real :: resp
    real :: transp
    real :: NPPleaf
    real :: NPProot
    real :: NPPwood    
    real :: NSC
    real :: seedC
    real :: leafC
    real :: rootC
    real :: SW_C
    real :: HW_C
    real :: NSN
    real :: seedN
    real :: leafN
    real :: rootN
    real :: SW_N
    real :: HW_N
  end type outtype_daily_cohorts

  type outtype_annual_tile  !fno5
    real :: year
    real :: CAI
    real :: LAI
    real :: GPP
    real :: Rauto
    real :: Rh
    real :: rain
    real :: SoilWater
    real :: Transp
    real :: Evap
    real :: Runoff
    real :: plantC
    real :: soilC
    real :: plantN
    real :: soilN
    real :: totN
    real :: NSC
    real :: SeedC
    real :: leafC
    real :: rootC
    real :: SapwoodC
    real :: WoodC
    real :: NSN
    real :: SeedN
    real :: leafN
    real :: rootN
    real :: SapwoodN
    real :: WoodN
    real :: McrbC
    real :: fastSOM
    real :: SlowSOM
    real :: McrbN
    real :: fastSoilN
    real :: slowSoilN
    real :: mineralN
    real :: N_fxed
    real :: N_uptk
    real :: N_yrMin
    real :: N_P2S
    real :: N_loss
    real :: totseedC
    real :: totseedN
    real :: Seedling_C
    real :: Seedling_N
  end type outtype_annual_tile  

  type outtype_annual_cohorts ! fno2
    real :: year
    real :: cID
    real :: PFT
    real :: layer
    real :: density
    real :: f_layer
    real :: dDBH
    real :: dbh
    real :: height
    real :: Acrown
    real :: wood
    real :: nsc
    real :: NSN
    real :: NPPtr
    real :: seed
    real :: NPPL
    real :: NPPR
    real :: NPPW
    real :: GPP
    real :: NPP
    real :: N_uptk
    real :: N_fix
    real :: maxLAI
  end type outtype_annual_cohorts

  type outtype_biosphere
    type(outtype_hourly_tile), dimension(:), allocatable              :: hourly_tile      !fn01
    type(outtype_daily_tile), dimension(ndayyear)                     :: daily_tile       !fno4
    type(outtype_daily_cohorts), dimension(ndayyear, out_max_cohorts) :: daily_cohorts    !fno3
    type(outtype_annual_tile)                                         :: annual_tile      !fno5
    type(outtype_annual_cohorts), dimension(out_max_cohorts)          :: annual_cohorts   !fno2
  end type outtype_biosphere

contains

end module md_interface_lm3ppa
