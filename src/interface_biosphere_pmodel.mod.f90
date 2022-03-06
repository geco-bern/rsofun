module md_interface_pmodel

  use, intrinsic :: iso_fortran_env, dp=>real64

  use md_forcing_pmodel, only: climate_type, landuse_type, &
    vegcover_type, landuse_type
  ! use md_params_soil_pmodel, only: paramtype_soil
  use md_params_siml_pmodel, only: paramstype_siml, outtype_steering
  use md_params_core, only: nlayers_soil, ndayyear, npft
  use md_grid, only: gridtype !, domaininfo_type

  implicit none

  private
  public interfacetype_biosphere, outtype_biosphere, myinterface  

  type paramstype_calib
    real :: kphio
    real :: soilm_par_a
    real :: soilm_par_b
    real :: tau_acclim_tempstress
    real :: par_shape_tempstress
    real :: kbeer
    real :: f_nretain
    real :: fpc_tree_max
    real :: growtheff
    real :: r_root
    real :: r_sapw
    real :: exurate
    real :: k_decay_tissue
    real :: k_decay_leaf_width
    real :: k_decay_sapw
    real :: k_decay_root
    real :: r_cton_root
    real :: r_ntoc_root
    real :: ncw_min
    real :: r_n_cw_v
    real :: r_ctostructn_leaf
  end type paramstype_calib  

  type interfacetype_biosphere
    integer                                 :: year
    real                                    :: pco2
    type(gridtype)                          :: grid
    real, dimension(4,nlayers_soil)         :: soiltexture   ! soil texture (rows: sand, clay, organic, gravel; columns: layers from top)
    real                                    :: whc_prescr
    type(climate_type), dimension(ndayyear) :: climate
    type(vegcover_type), dimension(ndayyear):: vegcover
    type(landuse_type), dimension(ndayyear) :: landuse
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

    ! new for cnmodel: 25 additional
    real, dimension(ndayyear) :: tsoil
    real, dimension(ndayyear) :: cleaf
    real, dimension(ndayyear) :: nleaf
    real, dimension(ndayyear) :: croot
    real, dimension(ndayyear) :: nroot
    real, dimension(ndayyear) :: clabl
    real, dimension(ndayyear) :: nlabl
    real, dimension(ndayyear) :: lai
    real, dimension(ndayyear) :: ninorg
    real, dimension(ndayyear) :: pno3
    real, dimension(ndayyear) :: pnh4
    real, dimension(ndayyear) :: en2o
    real, dimension(ndayyear) :: enleach
    real, dimension(ndayyear) :: tmp
    real, dimension(ndayyear) :: csoil
    real, dimension(ndayyear) :: nsoil
    real, dimension(ndayyear) :: clitt
    real, dimension(ndayyear) :: nlitt
    real, dimension(ndayyear) :: nfix
    real, dimension(ndayyear) :: nup
    real, dimension(ndayyear) :: cex
    real, dimension(ndayyear) :: dcharv
    real, dimension(ndayyear) :: dnharv

  end type outtype_biosphere

end module md_interface_pmodel
