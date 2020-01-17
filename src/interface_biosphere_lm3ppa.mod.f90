module md_interface_lm3ppa

  use, intrinsic :: iso_fortran_env, dp=>real64

  use md_forcing_lm3ppa, only: climate_type, landuse_type, ninput_type, vegcover_type  
  use md_params_soil_lm3ppa, only: paramtype_soil
  use md_params_siml_lm3ppa, only: paramstype_siml, outtype_steering
  use md_params_core_lm3ppa, only: MSPECIES, ndayyear
  use md_grid, only: gridtype !, domaininfo_type

  implicit none

  private
  public interfacetype_biosphere, outtype_biosphere, myinterface  

  ! type paramstype_calib
  !   real :: kphio
  ! end type paramstype_calib  

  type paramstype_tile
    integer :: soiltype
    real :: FLDCAP
    real :: WILTPT
    real :: K1
    real :: K2
    real :: K_nitrogen
    real :: etaN
    real :: MLmixRatio
    real :: l_fract
    real :: retransN
    real :: fNSNmax
    real :: f_N_add
    real :: f_initialBSW
  end type paramstype_tile
  
  type paramstype_species
    real, dimension(MSPECIES) :: lifeform
    real, dimension(MSPECIES) :: phenotype
    real, dimension(MSPECIES) :: pt
    real, dimension(MSPECIES) :: seedlingsize
    real, dimension(MSPECIES) :: LMA
    real, dimension(MSPECIES) :: phiRL
    real, dimension(MSPECIES) :: LNbase
    real, dimension(MSPECIES) :: laimax
    real, dimension(MSPECIES) :: LAI_light
    real, dimension(MSPECIES) :: Nfixrate0
    real, dimension(MSPECIES) :: NfixCost0
    real, dimension(MSPECIES) :: phiCSA
    real, dimension(MSPECIES) :: mortrate_d_c
    real, dimension(MSPECIES) :: mortrate_d_u
    real, dimension(MSPECIES) :: maturalage
  end type paramstype_species

  type inittype_cohort 
    real, dimension(MAX_INIT_COHORTS) :: init_cohort_species
    real, dimension(MAX_INIT_COHORTS) :: init_cohort_nindivs
    real, dimension(MAX_INIT_COHORTS) :: init_cohort_bsw
    real, dimension(MAX_INIT_COHORTS) :: init_cohort_bHW
    real, dimension(MAX_INIT_COHORTS) :: init_cohort_nsc
  end type inittype_cohort

  type inittype_soil 
    real :: init_fast_soil_C
    real :: init_slow_soil_C
    real :: init_Nmineral
    real :: N_input
  end type inittype_soil

  type interfacetype_biosphere
    integer                         :: year
    real                            :: pco2
    type( gridtype )                :: grid
    type( paramtype_soil )          :: soilparams
    type( climate_type )            :: climate
    type( vegcover_type )           :: vegcover
    ! type( domaininfo_type )         :: domaininfo
    type( outtype_steering )        :: steering
    type( paramstype_siml )         :: params_siml
    real, dimension(:), allocatable :: fpc_grid   ! allocatable because we don't know number of PFTs a priori
    type( paramstype_calib )        :: params_calib    ! calibratable parameters
    type( paramstype_species )      :: params_species
    ! xxx todo add tile params, cohort, soil
  end type interfacetype_biosphere

  type(interfacetype_biosphere) :: myinterface

  !----------------------------------------------------------------
  ! Return variable of biosphere()
  !----------------------------------------------------------------
  ! This is the derived type-return variable of the function biosphere(),
  ! holding variables used for the cost function in sofun_calib.f90
  type outtype_biosphere
    real, dimension(ndayyear) :: gpp
    real, dimension(ndayyear) :: fapar
    real, dimension(ndayyear) :: transp
    real, dimension(ndayyear) :: latenth
  end type outtype_biosphere

contains


end module md_interface_lm3ppa
