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


  type paramstype_tile
    ! xxx todo: add tile parameters
  end type paramstype_tile

  type inittype_cohort ! xxx todo 

  type inittype_soil ! xxx tod 



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
