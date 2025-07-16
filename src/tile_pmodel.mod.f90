module md_tile_pmodel
  !////////////////////////////////////////////////////////////////
  ! Defines how a tile looks like for P-model simulations.
  !---------------------------------------------------------------
  use md_params_core, only: npft, nlu
  use md_plant_pmodel, only: plant_type, plant_fluxes_type, initglobal_plant

  implicit none

  private
  public tile_type, tile_fluxes_type, initglobal_tile, psoilphystype, soil_type, &
    initdaily_tile_fluxes, getpar_modl_tile, diag_daily, initglobal_soil !, init_annual, diag_annual

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
  ! soil parameters. need to be stored for each gridcell (-> part of 'tile')
  !----------------------------------------------------------------
  type paramtype_soil
    real :: fsand
    real :: fclay
    real :: forg
    real :: fgravel
    real :: fc
    real :: pwp
    real :: whc_dz      ! water holding capacity per unit soil depth
    real :: whc
    real :: ksat
    real :: thdiff_wp
    real :: thdiff_whc15
    real :: thdiff_fc
  end type

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

    ! daily
    !----------------------------------------------------------------
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

    real :: vcmax25           ! acclimated Vcmax, normalised to 25 deg C (mol CO2 m-2 s-1)
    real :: jmax25            ! acclimated Jmax, normalised to 25 deg C (mol CO2 m-2 s-1)
    real :: vcmax             ! daily varying Vcmax (mol CO2 m-2 s-1)
    real :: jmax              ! daily varying Jmax (mol CO2 m-2 s-1)
    real :: gs_accl           ! acclimated stomatal conductance (xxx)
    real :: chi               ! ci:ca ratio (unitless)
    real :: iwue              ! intrinsic water use efficiency (A/gs = ca*(1-chi))

    ! radiation
    real :: ppfd_splash
    real :: dra              ! daily top-of-atmosphere solar radiation (J/m^2/d)

    ! ! annual
    ! !----------------------------------------------------------------
    ! ! carbon 
    ! real :: agpp
    ! real :: avcmax25_mean         ! annual Vcmax, normalised to 25 deg C, GPP-weighted mean
    ! real :: avcmax25_max          ! annual Vcmax, normalised to 25 deg C, annual maximum

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
    !----------------------------------------------------------------
    ! use md_interface_pmodel, only: myinterface

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

      ! initialise canopy variables
      call initglobal_canopy( tile(lu)%canopy )

      ! initialise plant variables
      call initglobal_plant( tile(lu)%plant(:) )

    end do

  end subroutine initglobal_tile


  subroutine initglobal_canopy( canopy )
    !////////////////////////////////////////////////////////////////
    !  Initialisation of specified PFT on specified gridcell
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
    call initglobal_soil_params( soil%params )

    soil%phy%wscal = soil%phy%wcont / soil%params%whc


  end subroutine initglobal_soil


  subroutine initglobal_soil_phy( phy )
    !////////////////////////////////////////////////////////////////
    ! initialise physical soil variables globally
    !----------------------------------------------------------------
    ! argument
    type(psoilphystype), intent(inout) :: phy

    ! initialise physical soil variables
    phy%wcont = 150.0
    phy%temp  = 10.0
    phy%snow  = 0.0
    ! phy%rlmalpha = 0.0

  end subroutine initglobal_soil_phy


  subroutine initglobal_soil_params( params )
    !////////////////////////////////////////////////////////////////
    ! Function to calculate soil parameters from texture info.
    !----------------------------------------------------------------
    use md_interface_pmodel, only: myinterface

    ! arguments
    type(paramtype_soil), intent(inout) :: params

    ! ! function return variable
    ! type(paramtype_soil) :: params_soil

    ! ADOPTED FROM David Sandoval (https://github.com/dsval/rsplash/blob/master/R/splash.point.R)
    ! ************************************************************************
    ! Name:     soil_hydro
    ! Input:    - float, fsand, (percent)
    !           - float, fclay, (percent)
    !           - float, OM Organic Matter (percent)
    !           - float,fgravel, (percent-volumetric)
    ! Output:   list:
    !           - float, FC, (volumetric fraction)
    !           - float, WP (volumetric fraction)
    !           - float,SAT, (volumetric fraction)
    !           - float, AWC (volumetric fraction)
    !           - float,Ksat, Saturate hydraulic conductivity/infiltration capacity(mm/hr)
    !           - float, A (Coefficient)
    !           - float, B (Clapp and Hornberger (1978) pore-size distribution index)
    ! Features: calculate some soil hydrophysic characteristics
    ! Ref:      Balland et al. 
    ! ************************************************************************

    ! local variables
    real :: fsand, fclay, forg, fgravel, fsand_forg, fclay_forg, &
      fsand_fclay, dp, bd, sat, fc, pwp, L_10_Ksat, ksat, whc_dz

    ! from David's code
    real, parameter :: depth    =  30.0
    ! real, parameter :: topsoil  =  1.0

    ! calibrated paramters according to David Sandoval's PhD project report 
    real, parameter :: c_wp = 0.2018 ! 0.1437904
    real, parameter :: d_wp = 0.7809 ! 0.8398534

    real, parameter :: a_fc = -0.0547 ! 0.03320495
    real, parameter :: b_fc = -0.001  ! 0.2755312
    real, parameter :: c_fc = 0.4760  ! 0.3366685
    real, parameter :: d_fc = 0.9402  ! 1.417544

    real, parameter :: a_ks = -2.6539  ! -2.793574
    real, parameter :: b_ks = 3.0924   ! 3.12048
    real, parameter :: c_ks = 4.2146   ! 4.358185

    ! do idx = 1, nlayers_soil

      ! TOP LAYER
      fsand   = 0.4 ! myinterface%soiltexture(1,1)
      fclay   = 0.3 ! myinterface%soiltexture(2,1)
      forg    = 0.1 ! myinterface%soiltexture(3,1)
      fgravel = 0.1 ! myinterface%soiltexture(4,1)

      fsand_forg  = fsand * forg
      fclay_forg  = fclay * forg
      fsand_fclay = fsand * fclay

      ! particle density
      dp = 1.0/((forg/1.3)+((1.0-forg)/2.65))  
      
      ! bulk density
      bd = (1.5 + (dp-1.5-1.10*(1.0 - fclay))*(1.0-exp(-0.022*depth)))/(1.0+6.27*forg) 
      
      ! water storage at saturation?
      sat = 1.0 - (bd/dp)  
    
      ! ! field capacity, interpretation of equation from David's report Eq. 18:
      ! fc = (sat/bd) * (c_fc + (d_fc - c_fc)) * fclay**0.5 * exp(-(a_fc * fsand - b_fc * forg)/(sat/bd))

      ! david's code: 
      fc = (sat/bd) * (c_fc + (d_fc - c_fc) * fclay**0.5) * exp(-(a_fc * fsand - b_fc * forg)/(sat/bd))  
      fc = max(fc, 0.0)
      fc = min(fc, 1.0)

      ! ! david's code:
      ! fc = (sat/bd)*(0.3366685 + (1.417544 - 0.3366685)*clay^0.5)*exp(-1*(0.03320495*sand - 0.2755312* OM)/(sat/bd))

      ! ! mct
      ! dplyr::mutate( fc = (sat/bd)*(0.3366685 + (1.417544 - 0.3366685)*fclay^0.5)*exp(-(0.03320495*fsand - 0.2755312* forg)/(sat/bd)) ) %>% 
      ! dplyr::mutate( fc = ifelse(fc<0, -0.1, fc) ) %>%
      ! dplyr::mutate( fc = ifelse(fc>1, 1, fc)) %>% 

      
      ! wilting point
      pwp = fc*(c_wp + (d_wp - c_wp)*fclay**0.5)  

      ! ! david's code:
      ! wp<- fc*(0.1437904 + (0.8398534 - 0.1437904)*clay^0.5) 

      ! ! mct:
      ! dplyr::mutate( pwp = fc*(0.1437904 + (0.8398534 - 0.1437904)*fclay^0.5) ) %>% 


      ! conductivity at saturation
      L_10_Ksat = a_ks + b_ks * log10(dp-bd) + c_ks * fsand  
      ksat = 10**L_10_Ksat 
      ksat = ksat * 10.0   ! to mm/h

      ! ! david's code: 
      ! L_10_Ksat = -2.793574+3.12048*log10(dp-bd)+4.358185*sand
      ! ksat = 10^L_10_Ksat
      ! ! to mm/h
      ! ksat<-ksat*10

      ! ! 
      ! moist_fvol33init = 0.278*fsand+0.034*fclay+0.022*forg-0.018*(fsand_forg)-0.027*(fclay_forg)-0.584*(fsand_fclay)+0.078 
      ! moist_fvol33 = moist_fvol33init+(0.636*moist_fvol33init-0.107)  
            
      ! ! get parameters for BC eqn form SAxton 2006
      ! coef_B = (log(1500)-log(33))/(log(fc)-log(pwp))  
      ! coef_A = exp(log(33)+coef_B*log(fc))  
      ! coef_lambda = 1.0/coef_B  
      
      ! ! Ksat = 1930*(SAT_fvol-FC_fvol)**(3-coef_lambda)
      
      ! bub_init = -21.6*fsand-27.93*fclay-81.97*moist_fvol33+71.12*(fsand*moist_fvol33)+8.29*(fclay*moist_fvol33)+14.05*(fsand_fclay)+27.16  
      ! bubbling_p = bub_init+(0.02*bub_init**2-0.113*bub_init-0.7)  
      
      ! ! 101.97162129779 converts from KPa to mmH2O
      ! bubbling_p = bubbling_p * -101.97162129779  
      
      ! ! error in empirical fitting, not possible matric potential positive
      ! if (bubbling_p > 0) then
      !   bubbling_p = bubbling_p * -1.0
      ! end if

      ! ! residual water content for BC eqn, Rawls, 1985
      ! fsand = fsand * 100, 
      ! fclay = fclay * 100,
      ! silt = 100 - fsand - fclay,
      ! forg = forg * 100 

      ! ! Ksat = 10*2.54*10**(-0.6+0.012*fsand-0.0064*fclay)
      ! RES = -0.018+0.0009*fsand+0.005*fclay+0.029*sat -0.0002*fclay**2-0.001*fsand*sat-0.0002*fclay**2*sat**2+0.0003*fclay**2*sat -0.002*sat**2*fclay  

      ! ! parameters for van Genutchen eqn
      ! VG_alpha = exp(-14.96 + 0.03135*fclay + 0.0351*silt + 0.646*forg +15.29*dp - 0.192*topsoil -4.671*dp**2- 0.000781*fclay**2 - 0.00687*forg**2 + 0.0449/forg + 0.0663*log(silt) + 0.1482*log(forg) - 0.04546*dp *silt - 0.4852*dp*forg + 0.00673*topsoil*fclay)  
      ! VG_n = 1.0+exp(-25.23 - 0.02195*fclay + 0.0074*silt - 0.1940*forg + 45.5*dp - 7.24*dp**2 +0.0003658*fclay**2 + 0.002885*forg**2 -12.81/dp - 0.1524/silt - 0.01958/forg - 0.2876*log(silt) - 0.0709*log(forg) -44.6*log(dp) - 0.02264*dp*fclay + 0.0896*dp*forg +0.00718*topsoil*fclay)  
      ! VG_m = 1-(VG_n)  
      
      ! water holding capacity
      whc_dz = (fc - pwp) * (1.0 - fgravel)

      ! add to soil paramters type
      params%fsand        = fsand
      params%fclay        = fclay
      params%forg         = forg
      params%fgravel      = fgravel
      params%fc           = fc
      params%pwp          = pwp
      params%whc_dz       = whc_dz
      params%ksat         = ksat
      params%thdiff_wp    = 0.2 ! value chosen from LPX (most soil codes have 0.2)
      params%thdiff_whc15 = 0.6 ! value chosen from LPX (most soil codes have 0.2)
      params%thdiff_fc    = 0.4
      
      ! overwrite
      params%whc = myinterface%whc_prescr

    ! end do

  end subroutine initglobal_soil_params  


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


  subroutine getpar_modl_tile()
    !////////////////////////////////////////////////////////////////
    !  Subroutine reads model parameters from input file.
    !  It was necessary to separate this SR from module md_plant
    !  because this SR uses module md_waterbal, which also uses
    !  _plant.
    !----------------------------------------------------------------    
    call getpar_modl_canopy()

  end subroutine getpar_modl_tile


  subroutine getpar_modl_canopy()
    !////////////////////////////////////////////////////////////////
    !  Subroutine reads model parameters from input file.
    !  It was necessary to separate this SR from module md_plant
    !  because this SR uses module md_waterbal, which also uses
    !  _plant.
    !----------------------------------------------------------------    

    !----------------------------------------------------------------
    ! NON-PFT DEPENDENT PARAMETERS
    !----------------------------------------------------------------
    ! canopy light extinction coefficient for Beer's Law
    params_canopy%kbeer = 0.5 ! hard-coded

  end subroutine getpar_modl_canopy


  ! subroutine init_annual( tile_fluxes )
  !   !////////////////////////////////////////////////////////////////
  !   ! Set (iterative) annual sums to zero
  !   !----------------------------------------------------------------
  !   ! arguments
  !   type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes

  !   ! local
  !   integer :: pft

  !   ! ! canopy-level
  !   ! tile_fluxes(:)%canopy%agpp          = 0.0
  !   ! tile_fluxes(:)%canopy%avcmax25_mean = 0.0
  !   ! tile_fluxes(:)%canopy%avcmax25_max  = 0.0
    
  !   ! ! pft-level
  !   ! do pft = 1,npft
  !   !   tile_fluxes(:)%plant(pft)%agpp          = 0.0
  !   !   tile_fluxes(:)%plant(pft)%avcmax25_mean = 0.0
  !   !   tile_fluxes(:)%plant(pft)%avcmax25_max  = 0.0
  !   ! end do

  ! end subroutine init_annual


  subroutine diag_daily( tile, tile_fluxes )
    !////////////////////////////////////////////////////////////////
    ! Daily diagnostics
    ! - sum over PFTs (plant) within LU (canopy) 
    ! - iterative sum over days
    !----------------------------------------------------------------
    ! arguments
    type(tile_type), dimension(nlu), intent(in) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes

    ! local
    integer :: lu

    !----------------------------------------------------------------
    ! Sum over PFTs to get canopy-level quantities
    !----------------------------------------------------------------
    do lu=1,nlu
      tile_fluxes(lu)%canopy%dgpp    = sum(tile_fluxes(lu)%plant(:)%dgpp)
      tile_fluxes(lu)%canopy%drd     = sum(tile_fluxes(lu)%plant(:)%drd)
      tile_fluxes(lu)%canopy%vcmax25 = sum(tile_fluxes(lu)%plant(:)%vcmax25 * tile(lu)%plant(:)%fpc_grid)
      tile_fluxes(lu)%canopy%jmax25  = sum(tile_fluxes(lu)%plant(:)%jmax25  * tile(lu)%plant(:)%fpc_grid)
      tile_fluxes(lu)%canopy%vcmax   = sum(tile_fluxes(lu)%plant(:)%vcmax   * tile(lu)%plant(:)%fpc_grid)
      tile_fluxes(lu)%canopy%jmax    = sum(tile_fluxes(lu)%plant(:)%jmax    * tile(lu)%plant(:)%fpc_grid)
      tile_fluxes(lu)%canopy%gs_accl = sum(tile_fluxes(lu)%plant(:)%gs_accl * tile(lu)%plant(:)%fpc_grid)
      tile_fluxes(lu)%canopy%chi     = sum(tile_fluxes(lu)%plant(:)%chi     * tile(lu)%plant(:)%fpc_grid)
      tile_fluxes(lu)%canopy%iwue    = sum(tile_fluxes(lu)%plant(:)%iwue    * tile(lu)%plant(:)%fpc_grid)
    end do

    ! !----------------------------------------------------------------
    ! ! Annual variables
    ! !----------------------------------------------------------------
    ! ! Canopy-level
    ! !----------------------------------------------------------------
    ! ! Annual sum
    ! tile_fluxes(:)%canopy%agpp = tile_fluxes(:)%canopy%agpp + tile_fluxes(:)%canopy%dgpp

    ! ! GPP-weighted annual mean
    ! tile_fluxes(lu)%canopy%avcmax25_mean = tile_fluxes(lu)%canopy%avcmax25_mean + tile_fluxes(lu)%canopy%vcmax25 * tile_fluxes(lu)%canopy%dgpp

    ! ! Annual maximum
    ! if (tile_fluxes(lu)%canopy%vcmax25 > tile_fluxes(lu)%canopy%avcmax25_max) tile_fluxes(lu)%canopy%avcmax25_max = tile_fluxes(lu)%canopy%vcmax25

    ! !----------------------------------------------------------------
    ! ! PFT-level
    ! !----------------------------------------------------------------
    ! do lu=1,nlu

    !   ! Annual sum
    !   tile_fluxes(lu)%plant(:)%agpp = tile_fluxes(lu)%plant(:)%agpp + tile_fluxes(lu)%plant(:)%dgpp

    !   ! GPP-weighted annual mean
    !   tile_fluxes(lu)%plant(:)%avcmax25_mean = tile_fluxes(lu)%plant(:)%avcmax25_mean + tile_fluxes(lu)%plant(:)%vcmax25 * tile_fluxes(lu)%plant(:)%dgpp

    !   ! Annual maximum
    !   do pft=1,npft
    !     if (tile_fluxes(lu)%plant(pft)%vcmax25 > tile_fluxes(lu)%plant(pft)%avcmax25_max) tile_fluxes(lu)%plant(pft)%avcmax25_max = tile_fluxes(lu)%plant(pft)%vcmax25
    !   end do

    ! end do

  end subroutine diag_daily


  ! subroutine diag_annual( tile, tile_fluxes )
  !   !////////////////////////////////////////////////////////////////
  !   ! Annual diagnostics
  !   !----------------------------------------------------------------
  !   ! arguments
  !   type(tile_type), dimension(nlu), intent(inout) :: tile
  !   type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes

  !   !----------------------------------------------------------------
  !   ! Store plant traits required for next year's allocation
  !   !----------------------------------------------------------------
  !   ! pft-level
  !   !do pft = 1,npft
  !     !tile(:)%plant(pft)%vcmax25 = tile_fluxes(:)%plant(pft)%avcmax25
  !   !end do

  !   ! ! for weighted-mean vcmax25 at canopy level
  !   !tile_fluxes(lu)%canopy%avcmax25 = tile_fluxes(lu)%canopy%avcmax25 / tile_fluxes(lu)%canopy%agpp
  !   ! ! for weighted-mean vcmax25 at pft-level

  !   ! !----------------------------------------------------------------
  !   ! ! Divide by annual total GPP for GPP-weighted sums 
  !   ! !----------------------------------------------------------------
  !   ! tile_fluxes(:)%canopy%avcmax25_mean = tile_fluxes(:)%canopy%avcmax25_mean / tile_fluxes(:)%canopy%agpp

  !   ! do pft = 1,npft
  !   !   tile_fluxes(:)%plant(pft)%avcmax25_mean = tile_fluxes(:)%plant(pft)%avcmax25_mean / tile_fluxes(:)%plant(pft)%agpp
  !   ! end do

  ! end subroutine diag_annual

end module md_tile_pmodel
