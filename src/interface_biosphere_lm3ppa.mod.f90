module md_interface_lm3ppa

  use, intrinsic :: iso_fortran_env, dp=>real64

  use md_forcing, only: climate_type, landuse_type, ninput_type, vegcover_type  
  use md_params_soil, only: paramtype_soil
  use md_params_siml, only: paramstype_siml, outtype_steering
  use md_params_core, only: nlayers_soil, ndayyear
  use md_grid, only: gridtype, domaininfo_type

  implicit none

  private
  public interfacetype_biosphere, outtype_biosphere, myinterface  

  type paramstype_calib
    ! real :: k_decay_tissue
    real :: kphio
    real :: soilm_par_a
    real :: soilm_par_b
    real :: vpdstress_par_a
    real :: vpdstress_par_b
    real :: vpdstress_par_m
  end type paramstype_calib  
  

  type interfacetype_biosphere
    integer                                             :: year
    real                                                :: pco2
    type( gridtype )      , dimension(:),   allocatable :: grid
    type( paramtype_soil ), dimension(:,:), allocatable :: soilparams
    ! type( landuse_type )  , dimension(:),   allocatable :: landuse
    type( climate_type )  , dimension(:),   allocatable :: climate
    ! type( ninput_type)    , dimension(:),   allocatable :: ninput_field
    type( vegcover_type ) , dimension(:), allocatable   :: vegcover
    type( domaininfo_type )                             :: domaininfo
    type( outtype_steering )                            :: steering
    type( paramstype_siml )                             :: params_siml
    real, dimension(:,:), allocatable                   :: fpc_grid
    type( paramstype_calib )                            :: params_calib    ! calibratable parameters
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
