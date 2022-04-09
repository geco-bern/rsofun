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
    real :: f_nretain
    real :: fpc_tree_max
    real :: growtheff
    real :: r_root
    real :: r_sapw
    real :: exurate
    real :: cton_soil
    real :: k_decay_leaf_base
    real :: k_decay_leaf_width
    real :: k_decay_root
    real :: k_decay_seed
    real :: k_decay_sapw
    real :: r_cton_root
    real :: r_cton_wood
    real :: ncw_min
    real :: r_n_cw_v
    real :: r_ctostructn_leaf
    real :: kbeer
    real :: gddbase
    real :: ramp
    real :: phentype
    real :: perc_k1
    real :: thdiff_wp
    real :: thdiff_whc15
    real :: thdiff_fc
    real :: forg
    real :: wbwp
    real :: por
    real :: fsand
    real :: fclay
    real :: fsilt
    real :: kA
    real :: kalb_sw
    real :: kalb_vis
    real :: kb
    real :: kc
    real :: kCw
    real :: kd
    real :: ke
    real :: keps
    real :: kWm
    real :: kw
    real :: komega
    real :: maxmeltrate
    real :: klitt_af10
    real :: klitt_as10
    real :: klitt_bg10
    real :: kexu10
    real :: ksoil_fs10
    real :: ksoil_sl10
    real :: ntoc_crit1
    real :: ntoc_crit2
    real :: cton_microb
    real :: tmppar
    real :: fastfrac
    real :: eff_nup
    real :: minimumcostfix
    real :: fixoptimum
    real :: a_param_fix
    real :: b_param_fix
    real :: maxnitr
    real :: non
    real :: n2on
    real :: kn
    real :: kdoc
    real :: docmax
    real :: dnitr2n2o
    real :: beta
    real :: rd_to_vcmax
    real :: tau_acclim
  end type paramstype_calib  

  type interfacetype_biosphere
    integer                                 :: year
    real                                    :: pco2
    type(gridtype)                          :: grid
    real, dimension(4,nlayers_soil)         :: soiltexture   ! soil texture (rows: sand, clay, organic, gravel; columns: layers from top)
    real                                    :: whc_prescr
    type(climate_type), dimension(ndayyear) :: climate
    type(climate_type), dimension(ndayyear) :: climate_memory
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
    real :: gpp
    real :: fapar
    real :: transp
    real :: latenth
    real :: pet
    real :: vcmax
    real :: jmax
    real :: vcmax25
    real :: jmax25
    real :: gs_accl
    real :: wscal
    real :: chi
    real :: iwue
    real :: tsoil
    real :: cleaf
    real :: nleaf
    real :: croot
    real :: nroot
    real :: clabl
    real :: nlabl
    real :: lai
    real :: ninorg
    real :: pno3
    real :: pnh4
    real :: en2o
    real :: enleach
    real :: npp
    real :: csoil
    real :: nsoil
    real :: clitt
    real :: nlitt
    real :: nfix
    real :: nup
    real :: cex
    real :: netmin
    real :: dcharv
    real :: dnharv
    real :: drd
  end type outtype_biosphere

end module md_interface_pmodel
