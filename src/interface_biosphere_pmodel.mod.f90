module md_interface_pmodel
  !////////////////////////////////////////////////////////////////
  ! Module for handling I/O (forcing, parameters, output) from 
  ! pmodel_f to the biosphere() with the P-model implementation
  !----------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, dp=>real64

  use md_forcing_pmodel, only: climate_type, landuse_type, ninput_type, vegcover_type  
  ! use md_params_soil_pmodel, only: paramtype_soil
  use md_params_siml_pmodel, only: paramstype_siml, outtype_steering
  use md_params_core, only: nlayers_soil, ndayyear, npft
  use md_grid, only: gridtype !, domaininfo_type

  implicit none

  private
  public interfacetype_biosphere, outtype_biosphere, myinterface  

  type paramstype_calib
    real :: kphio
    real :: kphio_par_a
    real :: kphio_par_b
    real :: soilm_thetastar
    real :: soilm_betao
    real :: beta_unitcostratio
    real :: rd_to_vcmax
    real :: tau_acclim
    real :: kc_jmax
  end type paramstype_calib  


  type interfacetype_biosphere
    integer                                 :: year
    real                                    :: pco2
    type(gridtype)                          :: grid
    real                                    :: whc_prescr
    type(climate_type), dimension(ndayyear) :: climate
    type(vegcover_type), dimension(ndayyear):: vegcover
    ! type(domaininfo_type)                 :: domaininfo
    type(outtype_steering)                  :: steering
    type(paramstype_siml)                   :: params_siml
    real, dimension(npft)                   :: fpc_grid        ! allocatable because we don't know number of PFTs a priori
    type(paramstype_calib)                  :: params_calib    ! calibratable parameters
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
    real, dimension(ndayyear) :: pet
    real, dimension(ndayyear) :: vcmax
    real, dimension(ndayyear) :: jmax
    real, dimension(ndayyear) :: vcmax25
    real, dimension(ndayyear) :: jmax25
    real, dimension(ndayyear) :: gs_accl
    real, dimension(ndayyear) :: wscal
    real, dimension(ndayyear) :: chi
    real, dimension(ndayyear) :: iwue
    real, dimension(ndayyear) :: rd
    real, dimension(ndayyear) :: tsoil         ! soil temperature, deg C
    real, dimension(ndayyear) :: netrad
    real, dimension(ndayyear) :: wcont
    real, dimension(ndayyear) :: snow

  end type outtype_biosphere

end module md_interface_pmodel
