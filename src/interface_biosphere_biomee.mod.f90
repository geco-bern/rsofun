module md_interface_biomee
  !////////////////////////////////////////////////////////////////
  ! Module for handling I/O (forcing, parameters, output) from 
  ! biome_f to the biosphere() with the BiomeE implementation
  !----------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, dp=>real64
  use md_forcing_biomee, only: climate_type
  use md_params_soil_biomee, only: paramtype_soil, getsoil
  use md_params_siml_biomee, only: paramstype_siml, outtype_steering
  use md_params_core, only: MSPECIES, ntstepsyear, ndayyear, MAX_INIT_COHORTS, out_max_cohorts
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
    real   :: MLmixRatio
    real   :: etaN
    real   :: LMAmin
    real   :: fsc_fine
    real   :: fsc_wood
    real   :: GR_factor
    real   :: l_fract
    real   :: retransN
    real   :: f_initialBSW
    real   :: f_N_add
    real   :: tf_base  ! calibratable
    real   :: par_mort ! calibratable
    real   :: par_mort_under ! calibratable
  end type paramstype_tile
  
  type paramstype_species
    integer :: lifeform
    integer :: phenotype
    integer :: pt
    real    :: alpha_FR
    real    :: rho_FR
    real    :: root_r
    real    :: root_zeta
    real    :: Kw_root
    real    :: leaf_size
    real    :: Vmax
    real    :: Vannual
    real    :: wet_leaf_dreg
    real    :: m_cond
    real    :: alpha_phot
    real    :: gamma_L
    real    :: gamma_LN
    real    :: gamma_SW
    real    :: gamma_FR
    real    :: tc_crit
    real    :: tc_crit_on
    real    :: gdd_crit
    real    :: seedlingsize
    real    :: LNbase
    real    :: laimax
    real    :: Nfixrate0
    real    :: NfixCost0
    real    :: phiCSA
    real    :: mortrate_d_c
    real    :: mortrate_d_u
    real    :: maturalage
    real    :: fNSNmax
    real    :: LMA       ! prescribed by sps
    real    :: rho_wood  ! prescribed by sps
    real    :: alphaBM   ! prescribed by sps
    real    :: thetaBM   ! prescribed by sps
    real    :: kphio     ! calibratable
    real    :: phiRL     ! calibratable
    real    :: LAI_light ! calibratable
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
    integer                                               :: year
    real, dimension(:), allocatable                       :: pco2
    type(gridtype)                                        :: grid
    type(climate_type), dimension(:), allocatable         :: climate
    type(outtype_steering)                                :: steering
    type(paramstype_siml)                                 :: params_siml
    real, dimension(:), allocatable                       :: fpc_grid   ! allocatable because we don't know number of PFTs a priori
    type(paramstype_species), dimension(0:MSPECIES)       :: params_species
    type(paramtype_soil)                                  :: params_soil
    type(paramstype_tile)                                 :: params_tile
    type(inittype_cohort), dimension(MAX_INIT_COHORTS)    :: init_cohort
    type(inittype_soil)                                   :: init_soil
    integer                                               :: datalines
    integer                                               :: steps_per_day
    real                                                  :: dt_fast_yr
    real                                                  :: step_seconds
    ! type(paramstype_calib_species), dimension(0:MSPECIES) :: params_calib_species
    ! type(paramstype_calib_tile)                           :: params_calib_tile
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
    real :: density
    real :: DBH
    real :: Density12
    real :: DBH12
    real :: QMD
    real :: NPP
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
    real :: MaxAge
    real :: MaxVolume
    real :: MaxDBH
    real :: NPPL
    real :: NPPW
    real :: n_deadtrees !yyy
    real :: c_deadtrees
    real :: m_turnover
    real :: c_turnover_time
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
    real :: age
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
    real :: Rauto
    real :: N_uptk
    real :: N_fix
    real :: maxLAI
    real :: Volume
    real :: n_deadtrees
    real :: c_deadtrees
    real :: deathrate
  end type outtype_annual_cohorts

  type outtype_biosphere
    type(outtype_hourly_tile), dimension(:), allocatable              :: hourly_tile      !fn01
    type(outtype_daily_tile), dimension(ndayyear)                     :: daily_tile       !fno4
    type(outtype_daily_cohorts), dimension(ndayyear,out_max_cohorts)  :: daily_cohorts    !fno3
    type(outtype_annual_tile)                                         :: annual_tile      !fno5
    type(outtype_annual_cohorts), dimension(out_max_cohorts)          :: annual_cohorts   !fno2
  end type outtype_biosphere

contains

end module md_interface_biomee
