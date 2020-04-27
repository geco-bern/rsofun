module md_tile_pmodel
  !////////////////////////////////////////////////////////////////
  ! Holds all tile-specific variables and procedurs
  ! --------------------------------------------------------------
  use md_params_core_pmodel, only: npft, nlu
  use md_params_soil_pmodel, only: paramtype_soil
  use md_plant_pmodel, only: plant_type, plant_fluxes_type, initglobal_plant

  implicit none

  private
  public tile_type, tile_fluxes_type, initglobal_tile, psoilphystype, soil_type, &
    initdaily_tile_fluxes, getpar_modl_canopy

  !----------------------------------------------------------------
  ! physical soil state variables with memory from year to year (~pools)
  !----------------------------------------------------------------
  type psoilphystype
    real :: temp        ! soil temperature [deg C]
    real :: wcont       ! liquid soil water mass [mm = kg/m2]
    real :: wscal       ! relative soil water content, between 0 (PWP) and 1 (FC)
    real :: snow        ! snow depth in liquid-water-equivalents [mm = kg/m2]
    ! real :: rlmalpha    ! rolling mean of annual mean alpha (AET/PET)
  end type psoilphystype

  !----------------------------------------------------------------
  ! Soil type
  !----------------------------------------------------------------
  type soil_type
    type(psoilphystype)  :: phy      ! soil state variables
    type(paramtype_soil) :: params   ! soil parameters
  end type soil_type

  type paramtype_canopy
    real :: kbeer             ! canopy light extinction coefficient
  end type paramtype_canopy

  type(paramtype_canopy) :: params_canopy

  !----------------------------------------------------------------
  ! Canopy type
  ! Contains tile-level aggregated variables related to the canopy
  !----------------------------------------------------------------
  type canopy_type
    real :: lai            ! leaf area index 
    real :: fapar          ! fraction of absorbed photosynthetically active radiation (unitless)
    real :: height         ! canopy height (m)
    real :: dgc            ! canopy conductance, upscaled from leaf-level stomatal conductance (m s-1)
    real :: fpc_grid       ! fractional projective cover (sum of crownarea by canopy plants)
  end type canopy_type

  !----------------------------------------------------------------
  ! Tile type with year-to-year memory
  !----------------------------------------------------------------
  type tile_type
    integer                           :: luno       ! Index that goes along with this instance of 'tile'
    type(soil_type)                   :: soil       ! all organic, inorganic, and physical soil variables
    type(canopy_type)                 :: canopy     ! mean canopy
    type(plant_type), dimension(npft) :: plant
    real                              :: rlmalpha
  end type tile_type

  !----------------------------------------------------------------
  ! Variables with no memory
  !----------------------------------------------------------------
  type canopy_fluxes_type

    ! water
    real :: dro             ! daily runoff (mm d-1)
    real :: dfleach         ! daily fraction of soil water going to runoff (used for calculating leaching)
    real :: dwbal           ! daily water balance as precipitation and snow melt minus runoff and evapotranspiration (mm d-1)
    real :: econ            ! water-to-energy conversion factor (m^3/J)
    real :: drn             ! daily total net radiation (J/m2/d)
    real :: drnn            ! nighttime total net radiation (J m-1 d-1)
    real :: rnl             ! net longwave radiation (W m-2)
    real :: dcn             ! daily total condensation (mm d-1)
    real :: deet            ! daily total equilibrium evapotranspiration (mm d-1)
    real :: dpet            ! daily total potential evapotranspiration (mm d-1)
    real :: dpet_e          ! daily total potential evapotranspiration (J m-2 d-1)
    real :: daet            ! daily total (actual) evapotranspiration (mm d-1)
    real :: daet_e          ! daily total (actual) evapotranspiration (J m-2 d-1)
    real :: daet_soil       ! daily soil evaporation (mm d-1)
    real :: daet_e_soil     ! daily soil evaporation (J m-2 d-1)
    real :: daet_canop      ! daily canopy transpiration (mm d-1)
    real :: daet_e_canop    ! daily canopy transpiration (J m-2 d-1)
    real :: cpa             ! alpha = equilibrium ET over potential ET (EET/PET, unitless)

    real :: dtransp         ! work in progress

    ! biogeophysics
    real :: dgs             ! stomatal conductance
    real :: dgc             ! canopy conductance

    ! real :: rho_air         ! density of air (g m-3)
    ! real :: sat_slope       ! slope of saturation vap press temp curve, Pa/K 
    ! real :: lv              ! enthalpy of vaporization, J/kg
    ! real :: rho_water       ! density of water (g m-3)

    ! carbon 
    real :: dgpp
    real :: drd
    real :: assim             ! leaf-level assimilation rate

    ! radiation
    real :: ppfd_splash
    real :: dayl             ! day length (h)
    real :: dra              ! daily top-of-atmosphere solar radiation (J/m^2/d)

    ! real, dimension(ndayyear) :: dayl               ! day length (hours)
    ! real, dimension(ndayyear) :: dra                ! daily TOA solar irradiation (J/m2)
    ! real, dimension(ndayyear) :: dppfd_splash       ! daily total PPFD (mol m-2 d-1)
    ! real, dimension(nmonth)   :: mppfd_splash       ! monthly total PPFD (mol m-2 month-1)
    ! real, dimension(nmonth)   :: meanmppfd_splash   ! monthly mean PPFD, averaged over daylight seconds (mol m-2 s-1)

  end type canopy_fluxes_type

  type tile_fluxes_type
    type(canopy_fluxes_type) :: canopy
    type(plant_fluxes_type), dimension(npft) :: plant
  end type tile_fluxes_type

contains


  ! function get_fapar( tile, params_ca ) result( fapar )
  !   !////////////////////////////////////////////////////////////////
  !   ! FOLIAGE PROJECTIVE COVER 
  !   ! = Fraction of Absorbed Photosynthetically Active Radiation
  !   ! Function returns fractional plant cover an individual
  !   ! Eq. 7 in Sitch et al., 2003
  !   !----------------------------------------------------------------
  !   ! arguments
  !   type(tile_type), intent(in) :: tile

  !   ! function return variable
  !   real :: fapar

  !   fapar = ( 1.0 - exp( -1.0 * tile%canopy%params%kbeer * tile%canopy%lai) )

  ! end function get_fapar


  subroutine initglobal_tile( tile )
    !////////////////////////////////////////////////////////////////
    !  Initialisation of all _pools on all gridcells at the beginning
    !  of the simulation.
    !  June 2014
    !  b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    use md_interface_pmodel, only: myinterface

    ! argument
    type( tile_type ), dimension(nlu), intent(inout) :: tile

    ! local variables
    integer :: lu

    !-----------------------------------------------------------------------------
    ! derive which PFTs are present from fpc_grid (which is prescribed)
    !-----------------------------------------------------------------------------
    do lu=1,nlu
      
      tile(lu)%luno = lu

      ! initialise soil variables
      call initglobal_soil( tile(lu)%soil )

      ! Copy soil parameters
      tile(lu)%soil%params = myinterface%soilparams

      ! initialise canopy variables
      call initglobal_canopy( tile(lu)%canopy )

      ! initialise plant variables
      call initglobal_plant( tile(lu)%plant(:) )

    end do

  end subroutine initglobal_tile


  subroutine initglobal_canopy( canopy )
    !////////////////////////////////////////////////////////////////
    !  Initialisation of specified PFT on specified gridcell
    !  June 2014
    !  b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    ! argument
    type(canopy_type), intent(inout) :: canopy

    canopy%lai         = 0.0
    canopy%fapar       = 0.0
    canopy%height      = 0.0
    canopy%dgc         = 0.0
    canopy%fpc_grid    = 0.0

  end subroutine initglobal_canopy


  subroutine initglobal_soil( soil )
    !////////////////////////////////////////////////////////////////
    ! initialise soil variables globally
    !----------------------------------------------------------------
    ! argument
    type(soil_type), intent(inout) :: soil

    call initglobal_soil_phy( soil%phy )

  end subroutine initglobal_soil


  subroutine initglobal_soil_phy( phy )
    !////////////////////////////////////////////////////////////////
    ! initialise physical soil variables globally
    !----------------------------------------------------------------
    ! argument
    type(psoilphystype), intent(inout) :: phy

    ! initialise physical soil variables
    phy%wcont    = 50.0
    phy%temp     = 10.0
    phy%snow     = 0.0
    ! phy%rlmalpha = 0.0

  end subroutine initglobal_soil_phy


  subroutine initdaily_tile_fluxes( tile_fluxes )
    !////////////////////////////////////////////////////////////////
    ! Initialises all daily variables within derived type 'soilphys'.
    !----------------------------------------------------------------
    ! arguments
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes

    ! local
    integer :: pft

    tile_fluxes(:)%canopy%dro = 0.0             
    tile_fluxes(:)%canopy%dfleach = 0.0         
    tile_fluxes(:)%canopy%dwbal = 0.0           
    tile_fluxes(:)%canopy%econ = 0.0            
    tile_fluxes(:)%canopy%drn = 0.0              
    tile_fluxes(:)%canopy%drnn = 0.0             
    tile_fluxes(:)%canopy%rnl = 0.0             
    tile_fluxes(:)%canopy%dcn = 0.0              
    tile_fluxes(:)%canopy%daet = 0.0            
    tile_fluxes(:)%canopy%daet_e = 0.0          
    tile_fluxes(:)%canopy%daet_soil = 0.0       
    tile_fluxes(:)%canopy%daet_e_soil = 0.0     
    tile_fluxes(:)%canopy%daet_canop = 0.0      
    tile_fluxes(:)%canopy%daet_e_canop = 0.0    
    tile_fluxes(:)%canopy%dgpp = 0.0
    tile_fluxes(:)%canopy%drd = 0.0
    tile_fluxes(:)%canopy%ppfd_splash = 0.0
    tile_fluxes(:)%canopy%dayl = 0.0
    tile_fluxes(:)%canopy%dra = 0.0
    ! tile_fluxes(:)%canopy%nu = 0.0
    ! tile_fluxes(:)%canopy%lambda = 0.0

    do pft = 1,npft
      tile_fluxes(:)%plant(npft)%dgpp     = 0.0
      tile_fluxes(:)%plant(npft)%drd      = 0.0
      tile_fluxes(:)%plant(npft)%dtransp  = 0.0
      tile_fluxes(:)%plant(npft)%dlatenth = 0.0
    end do

    ! call initdaily_plant( tile_fluxes(:)%plant(:) )

  end subroutine initdaily_tile_fluxes


  subroutine getpar_modl_canopy()
    !////////////////////////////////////////////////////////////////
    !  Subroutine reads model parameters from input file.
    !  It was necessary to separate this SR from module md_plant
    !  because this SR uses module md_waterbal, which also uses
    !  _plant.
    ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
    ! contact: b.stocker@imperial.ac.uk
    !----------------------------------------------------------------    
    ! local variable
    integer :: lu

    !----------------------------------------------------------------
    ! NON-PFT DEPENDENT PARAMETERS
    !----------------------------------------------------------------
    ! canopy light extinction coefficient for Beer's Law
    params_canopy%kbeer = 0.5 ! hard-coded

  end subroutine getpar_modl_canopy


end module md_tile_pmodel
