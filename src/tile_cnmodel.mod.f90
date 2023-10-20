module md_tile_cnmodel
  !////////////////////////////////////////////////////////////////
  ! Defines how a tile looks like and behaves for CN-model simulations.
  !---------------------------------------------------------------
  use md_params_core, only: npft, nlu
  use md_plant_cnmodel
  use md_classdefs

  implicit none

  private
  public tile_type, tile_fluxes_type, init_tile, psoilphystype, soil_type, &
    init_tile_fluxes, getpar_modl_tile, diag_daily, diag_annual, finalize_tile

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

    type(orgpool)  :: psoil_fs
    type(orgpool)  :: psoil_sl
    type(orgpool)  :: plitt_af
    type(orgpool)  :: plitt_as
    type(orgpool)  :: plitt_bg

    type(carbon)   :: pexud

    type(nitrogen) :: pno3
    type(nitrogen) :: pnh4

    real :: pno2             ! NO2 pool [gN/m2]
    real :: no_w             ! NO in wet microsites (split done in ntransform) [gN/m2]
    real :: no_d             ! NO in dry microsites (split done in ntransform) [gN/m2]
    real :: n2o_w            ! N2O in wet microsites (split done in ntransform) [gN/m2]
    real :: n2o_d            ! N2O in dry microsites (split done in ntransform) [gN/m2]
    real :: n2_w             ! N2 in wet microsites (split done in ntransform) [gN/m2]

    type(psoilphystype)  :: phy      ! soil physical state variables
    
    type(paramtype_soil) :: params   ! soil structure parameters

  end type soil_type

  !----------------------------------------------------------------
  ! Canopy type
  ! Contains tile-level aggregated variables related to the canopy
  !----------------------------------------------------------------
  type canopy_type
    
    real :: fapar          ! fraction of absorbed photosynthetically active radiation (unitless)
    real :: height         ! canopy height (m)
    real :: dgc            ! canopy conductance, upscaled from leaf-level stomatal conductance (m s-1)
    real :: fpc_grid       ! fractional plant coverage, remainder is bare ground (unitless)

    ! remaining elements, corresponding to plant_type
    real :: nind                ! number of individuals (m-2)
    real :: lai                 ! leaf area index 
    real :: acrown              ! crown area
    real :: narea               ! total leaf N per unit leaf area (gN m-2)
    real :: narea_metabolic     ! metabolic leaf N per unit leaf area (gN m-2)
    real :: narea_structural    ! structural leaf N per unit leaf area (gN m-2)
    real :: lma                 ! leaf mass per area (gC m-2)
    real :: sla                 ! specific leaf area (m2 gC-1)
    real :: nmass               ! leaf N per unit leaf mass, g N / g-dry mass
    real :: r_cton_leaf         ! leaf C:N ratio [gC/gN] 
    real :: r_ntoc_leaf         ! leaf N:C ratio [gN/gC]

    type(orgpool) :: pleaf     ! leaf biomass [gC/ind.] (=lm_ind)
    type(orgpool) :: proot     ! root biomass [gC/ind.] (=rm_ind)
    type(orgpool) :: psapw     ! sapwood biomass [gC/ind.] (=sm_ind)
    type(orgpool) :: pwood     ! heartwood (non-living) biomass [gC/ind.] (=hm_ind)
    type(orgpool) :: plabl     ! labile pool, temporary storage of N and C [gC/ind.] (=bm_inc but contains also N) 

  end type canopy_type

  !----------------------------------------------------------------
  ! Canopy-level parameters
  !----------------------------------------------------------------
  type paramtype_canopy
    real :: kbeer             ! canopy light extinction coefficient
  end type paramtype_canopy

  type(paramtype_canopy) :: params_canopy

  !----------------------------------------------------------------
  ! Tile type with year-to-year memory
  !----------------------------------------------------------------
  type tile_type
    integer                           :: luno       ! Index that goes along with this instance of 'tile'
    type(soil_type)                   :: soil       ! all organic, inorganic, and physical soil variables
    type(canopy_type)                 :: canopy     ! mean canopy
    type(plant_type), dimension(npft) :: plant
    real                              :: gdd        ! growing-degree days, cumulative tempearture above base temperature (deg C)
    real                              :: rlmalpha
  end type tile_type



  !----------------------------------------------------------------
  ! Soil fluxes
  !----------------------------------------------------------------
  type soil_fluxes_type
  
    type(carbon)   :: drhet             ! heterotrophic respiration [gC/m2/d]
    type(nitrogen) :: dnetmin           ! daily net mineralisation (gN m-2 d-1)

    real :: dnfix_free                  ! free-living N fixation (balance term to satisfy soil C:N ratio)
    real :: dn2o                        ! soil N2O emissions (gaseous escape) [gN/m2/d]
    real :: dnleach                     ! daily N leaching [gN/m2/d]
    real :: dnloss                      ! total N loss (gaseous + leaching, where gaseous loss is N w.r.t. NH4 and NO3 pool decline, not gaseous escape) [gN/m2/d]

  end type soil_fluxes_type

  !----------------------------------------------------------------
  ! Canopy-level fluxes
  !----------------------------------------------------------------
  type canopy_fluxes_type

    ! waterbal-related variables
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
    real :: ppfd_splash     ! photosynthetic photon flux density (PPFD) calculated by SPLASH (waterbal_splash.mod.f90) based on TOA solar radiation and cloud cover

    ! biogeophysics
    real :: dgs             ! stomatal conductance
    real :: dgc             ! canopy conductance

    ! radiation
    real :: ppfd_memory       ! damped photosynthetic photon flux density (PPFD) 
    real :: dra               ! daily top-of-atmosphere solar radiation (J/m^2/d)
    real :: dayl              ! day length (s)

    ! remaining elements, corresponding to plant_fluxes_type
    real :: dgpp              ! daily gross primary production [gC/m2/d]           
    real :: drd               ! daily dark respiration [gC/m2/d]
    real :: assim             ! daily assimilation (mol CO2 m-2 s-1)
    real :: dtransp           ! daily transpiration [mm]
    real :: dlatenth          ! daily latent heat flux [J m-2 d-1]

    real :: drleaf   ! daily total leaf respiration, no explicit isotopic signature as it is identical to the signature of GPP [gC/m2/d]
    real :: drroot   ! root maintenance respiration, no explicit isotopic signature as it is identical to the signature of GPP [gC/m2/d]
    real :: drsapw   ! sapwood maintenance respiration, no explicit isotopic signature as it is identical to the signature of GPP [gC/m2/d]
    real :: drgrow   ! growth respiration (growth+maintenance resp. of all compartments), no explicit isotopic signature as it is identical to the signature of GPP [gC/m2/d]
    real :: dcex     ! labile C exudation for N uptake, no explicit isotopic signature as it is identical to the signature of GPP [gC/m2/d]
    
    type(carbon)   :: dnpp     ! daily net primary production (gpp-ra, npp=bp+cex) [gC/m2/d]
    type(nitrogen) :: dnup     ! daily N uptake [gN/m2/d]

    real :: dnup_pas          ! daily N uptake by passsive uptake (transpiration) [gN/m2/d]
    real :: dnup_act          ! daily N uptake by active uptake [gN/m2/d]
    real :: dnup_fix          ! daily N uptake by plant symbiotic N fixation [gN/m2/d]
    real :: dnup_ret          ! daily N "uptake" by plant symbiotic N fixation [gN/m2/d]

    real :: vcmax25           ! acclimated Vcmax, normalised to 25 deg C (mol CO2 m-2 s-1)
    real :: jmax25            ! acclimated Jmax, normalised to 25 deg C (mol CO2 m-2 s-1)
    real :: vcmax             ! daily varying Vcmax (mol CO2 m-2 s-1)
    real :: jmax              ! daily varying Jmax (mol CO2 m-2 s-1)
    real :: gs_accl           ! acclimated stomatal conductance (xxx)
    real :: chi               ! ci:ca ratio (unitless)
    real :: iwue              ! intrinsic water use efficiency (A/gs = ca*(1-chi))
    real :: asat              ! light-saturated assimilation rate (mol CO2 m-2 s-1)
    real :: actnv_unitiabs    ! metabolic leaf N per unit absorbed light (g N m-2 mol-1)

    real :: npp_leaf          ! carbon allocated to leaves (g C m-2 d-1)
    real :: npp_root          ! carbon allocated to roots (g C m-2 d-1)
    real :: npp_wood          ! carbon allocated to wood (sapwood (g C m-2 d-1))

    type(orgpool) :: dharv    ! daily total biomass harvest (g m-2 d-1)

  end type canopy_fluxes_type

  !----------------------------------------------------------------
  ! Tile-level fluxes
  !----------------------------------------------------------------
  type tile_fluxes_type
    type(soil_fluxes_type) :: soil
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


  subroutine init_tile( tile )
    !////////////////////////////////////////////////////////////////
    !  Initialisation of all _pools on all gridcells at the beginning
    !  of the simulation.
    !----------------------------------------------------------------
    ! argument
    type( tile_type ), dimension(nlu), intent(inout) :: tile

    ! local variables
    integer :: lu
    character(len=256) :: prefix
    character(len=256) :: filnam

    !-----------------------------------------------------------------------------
    ! derive which PFTs are present from fpc_grid (which is prescribed)
    !-----------------------------------------------------------------------------
    do lu=1,nlu
      
      tile(lu)%luno = lu

      ! initialise soil variables
      call init_tile_soil( tile(lu)%soil )

      ! initialise canopy variables
      call init_tile_canopy( tile(lu)%canopy )

      ! initialise plant variables
      call init_plant( tile(lu)%plant(:) )

    end do

    tile(:)%gdd = 0.0

    !-----------------------------------------------------------------------------
    ! open files for experimental output
    !-----------------------------------------------------------------------------
    prefix = "./out/out_rsofun"

    filnam = trim(prefix)//'.a.csoil.txt'
    open(unit = 101, file = filnam, err = 999, status = 'unknown')

    filnam = trim(prefix)//'.a.nsoil.txt'
    open(unit = 102, file = filnam, err = 999, status = 'unknown')

    return
    999 stop 'init_tile(): error opening output files'

  end subroutine init_tile


  subroutine finalize_tile()
    !////////////////////////////////////////////////////////////////
    ! Closing files
    !----------------------------------------------------------------
    close(unit = 101)
    close(unit = 102)

  end subroutine finalize_tile


  subroutine init_tile_canopy( canopy )
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

    ! orgpools
    call orginit( canopy%pleaf )
    call orginit( canopy%proot )
    call orginit( canopy%psapw )
    call orginit( canopy%pwood )
    call orginit( canopy%plabl )

  end subroutine init_tile_canopy


  subroutine init_tile_soil( soil )
    !////////////////////////////////////////////////////////////////
    ! initialise soil variables globally
    !----------------------------------------------------------------
    ! argument
    type(soil_type), intent(inout) :: soil

    call ninit( soil%pno3 )
    call ninit( soil%pnh4 )

    call orginit( soil%psoil_fs )
    call orginit( soil%psoil_sl )
    call orginit( soil%plitt_af )
    call orginit( soil%plitt_as )
    call orginit( soil%plitt_bg )
    
    call cinit( soil%pexud )

    soil%pno2  = 0.0
    soil%no_w  = 0.0
    soil%no_d  = 0.0
    soil%n2o_w = 0.0
    soil%n2o_d = 0.0
    soil%n2_w  = 0.0

    call init_tile_soil_phy( soil%phy )
    call init_tile_soil_params( soil%params )

    soil%phy%wscal = soil%phy%wcont / soil%params%whc

  end subroutine init_tile_soil


  subroutine init_tile_soil_phy( phy )
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

  end subroutine init_tile_soil_phy


  subroutine init_tile_soil_params( params )
    !////////////////////////////////////////////////////////////////
    ! Function to calculate soil parameters from texture info.
    !----------------------------------------------------------------
    use md_interface_cnmodel, only: myinterface

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
    real, parameter :: topsoil  =  1.0

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
      whc_dz = (fc-pwp)*(1-fgravel)

      ! add to soil paramters type
      params%fsand        = fsand
      params%fclay        = fclay
      params%forg         = forg
      params%fgravel      = fgravel
      params%fc           = fc
      params%pwp          = pwp
      params%whc_dz       = whc_dz
      params%ksat         = ksat
      params%thdiff_wp    = 0.2    ! value chosen from LPX (most soil codes have 0.2)
      params%thdiff_whc15 = 0.6 ! value chosen from LPX (most soil codes have 0.2)
      params%thdiff_fc    = 0.4

      ! overwrite
      params%whc = myinterface%whc_prescr

    ! end do

  end subroutine init_tile_soil_params


  subroutine init_tile_fluxes( tile_fluxes )
    !////////////////////////////////////////////////////////////////
    ! Initialises all flux variables at tile level
    !----------------------------------------------------------------
    ! arguments
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes

    ! local
    integer :: lu

    ! canopy
    tile_fluxes(:)%canopy%dro          = 0.0
    tile_fluxes(:)%canopy%dfleach      = 0.0
    tile_fluxes(:)%canopy%dwbal        = 0.0
    tile_fluxes(:)%canopy%econ         = 0.0
    tile_fluxes(:)%canopy%drn          = 0.0
    tile_fluxes(:)%canopy%drnn         = 0.0
    tile_fluxes(:)%canopy%rnl          = 0.0
    tile_fluxes(:)%canopy%dcn          = 0.0
    tile_fluxes(:)%canopy%deet         = 0.0
    tile_fluxes(:)%canopy%dpet         = 0.0
    tile_fluxes(:)%canopy%dpet_e       = 0.0
    tile_fluxes(:)%canopy%daet         = 0.0
    tile_fluxes(:)%canopy%daet_e       = 0.0
    tile_fluxes(:)%canopy%daet_soil    = 0.0
    tile_fluxes(:)%canopy%daet_e_soil  = 0.0
    tile_fluxes(:)%canopy%daet_canop   = 0.0
    tile_fluxes(:)%canopy%daet_e_canop = 0.0
    tile_fluxes(:)%canopy%cpa          = 0.0
    tile_fluxes(:)%canopy%dtransp      = 0.0
    tile_fluxes(:)%canopy%dgs          = 0.0
    tile_fluxes(:)%canopy%dgc          = 0.0
    tile_fluxes(:)%canopy%dgpp         = 0.0
    tile_fluxes(:)%canopy%drd          = 0.0
    tile_fluxes(:)%canopy%assim        = 0.0
    tile_fluxes(:)%canopy%vcmax25      = 0.0
    tile_fluxes(:)%canopy%jmax25       = 0.0
    tile_fluxes(:)%canopy%vcmax        = 0.0
    tile_fluxes(:)%canopy%jmax         = 0.0
    tile_fluxes(:)%canopy%gs_accl      = 0.0
    tile_fluxes(:)%canopy%chi          = 0.0
    tile_fluxes(:)%canopy%iwue         = 0.0
    tile_fluxes(:)%canopy%asat         = 0.0
    tile_fluxes(:)%canopy%ppfd_splash  = 0.0
    tile_fluxes(:)%canopy%ppfd_memory  = 0.0
    tile_fluxes(:)%canopy%dra          = 0.0
    tile_fluxes(:)%canopy%npp_leaf     = 0.0
    tile_fluxes(:)%canopy%npp_root     = 0.0
    tile_fluxes(:)%canopy%npp_wood     = 0.0

    ! soil
    do lu=1,nlu

      ! derived types
      call ninit(tile_fluxes(lu)%soil%dnetmin)
      
      call cinit(tile_fluxes(lu)%soil%drhet)

      call orginit(tile_fluxes(lu)%canopy%dharv)

      ! plant
      call init_plant_fluxes( tile_fluxes(lu)%plant(:) )

      tile_fluxes(lu)%soil%dnfix_free = 0.0
      tile_fluxes(lu)%soil%dn2o       = 0.0
      tile_fluxes(lu)%soil%dnleach    = 0.0
      tile_fluxes(lu)%soil%dnloss     = 0.0

    end do

  end subroutine init_tile_fluxes


  subroutine getpar_modl_tile()
    !////////////////////////////////////////////////////////////////
    !  Subroutine reads model parameters from input file.
    !  It was necessary to separate this SR from module md_plant
    !  because this SR uses module md_waterbal, which also uses
    !  _plant.
    ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
    ! contact: b.stocker@imperial.ac.uk
    !----------------------------------------------------------------    
    call getpar_modl_canopy()

  end subroutine getpar_modl_tile


  subroutine getpar_modl_canopy()
    !////////////////////////////////////////////////////////////////
    !  Subroutine reads model parameters from input file.
    !  It was necessary to separate this SR from module md_plant
    !  because this SR uses module md_waterbal, which also uses
    !  _plant.
    ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
    ! contact: b.stocker@imperial.ac.uk
    !----------------------------------------------------------------    
    use md_interface_cnmodel, only: myinterface

    !----------------------------------------------------------------
    ! NON-PFT DEPENDENT PARAMETERS
    !----------------------------------------------------------------
    ! canopy light extinction coefficient for Beer's Law
    params_canopy%kbeer = myinterface%params_calib%kbeer

  end subroutine getpar_modl_canopy


  subroutine diag_daily( tile, tile_fluxes, out_biosphere )
    !////////////////////////////////////////////////////////////////
    ! Daily diagnostics
    ! - sum over PFTs (plant) within LU (canopy) 
    ! - iterative sum over days
    !----------------------------------------------------------------
    use md_interface_cnmodel, only: outtype_biosphere

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(outtype_biosphere), intent(inout) :: out_biosphere

    ! local
    integer :: lu, pft
    real :: tmp

    !----------------------------------------------------------------
    ! Initialise all canopy-level quantities that are later summed over plants
    !----------------------------------------------------------------
    ! fluxes
    tile_fluxes(:)%canopy%dgpp = 0.0
    tile_fluxes(:)%canopy%drd = 0.0
    tile_fluxes(:)%canopy%assim = 0.0
    tile_fluxes(:)%canopy%dtransp = 0.0
    tile_fluxes(:)%canopy%dlatenth = 0.0
    tile_fluxes(:)%canopy%drleaf = 0.0
    tile_fluxes(:)%canopy%drroot = 0.0
    tile_fluxes(:)%canopy%drsapw = 0.0
    tile_fluxes(:)%canopy%drgrow = 0.0
    tile_fluxes(:)%canopy%dcex = 0.0
    tile_fluxes(:)%canopy%dnup_pas = 0.0
    tile_fluxes(:)%canopy%dnup_act = 0.0
    tile_fluxes(:)%canopy%dnup_fix = 0.0
    tile_fluxes(:)%canopy%dnup_ret = 0.0
    tile_fluxes(:)%canopy%vcmax25 = 0.0
    tile_fluxes(:)%canopy%jmax25 = 0.0
    tile_fluxes(:)%canopy%vcmax = 0.0
    tile_fluxes(:)%canopy%jmax = 0.0
    tile_fluxes(:)%canopy%gs_accl = 0.0
    tile_fluxes(:)%canopy%chi = 0.0
    tile_fluxes(:)%canopy%iwue = 0.0
    tile_fluxes(:)%canopy%asat = 0.0
    tile_fluxes(:)%canopy%actnv_unitiabs = 0.0

    do lu=1,nlu
      call orginit( tile_fluxes(lu)%canopy%dharv )
      call cinit( tile_fluxes(lu)%canopy%dnpp )
      call ninit( tile_fluxes(lu)%canopy%dnup )
    end do

    ! pools
    tile(:)%canopy%nind = 0.0 
    tile(:)%canopy%fpc_grid = 0.0 
    tile(:)%canopy%lai = 0.0 
    tile(:)%canopy%fapar = 0.0 
    tile(:)%canopy%acrown = 0.0 
    tile(:)%canopy%narea = 0.0 
    tile(:)%canopy%narea_metabolic = 0.0 
    tile(:)%canopy%narea_structural = 0.0 
    tile(:)%canopy%lma = 0.0 
    tile(:)%canopy%sla = 0.0 
    tile(:)%canopy%nmass = 0.0 
    tile(:)%canopy%r_cton_leaf = 0.0 
    tile(:)%canopy%r_ntoc_leaf = 0.0

    do lu=1,nlu
      call orginit( tile(lu)%canopy%pleaf )
      call orginit( tile(lu)%canopy%proot )
      call orginit( tile(lu)%canopy%psapw )
      call orginit( tile(lu)%canopy%pwood )
      call orginit( tile(lu)%canopy%plabl )
    end do


    !----------------------------------------------------------------
    ! Sum over plants to get canopy-level quantities
    !----------------------------------------------------------------
    do pft=1,npft
      
      lu = params_pft_plant(pft)%lu_category

      !----------------------------------------------------------------
      ! fluxes
      !----------------------------------------------------------------
      ! canopy-level quantities as sums
      tile_fluxes(lu)%canopy%dgpp = tile_fluxes(lu)%canopy%dgpp + tile_fluxes(lu)%plant(pft)%dgpp
      tile_fluxes(lu)%canopy%drd = tile_fluxes(lu)%canopy%drd + tile_fluxes(lu)%plant(pft)%drd
      tile_fluxes(lu)%canopy%assim = tile_fluxes(lu)%canopy%assim + tile_fluxes(lu)%plant(pft)%assim
      tile_fluxes(lu)%canopy%dtransp = tile_fluxes(lu)%canopy%dtransp + tile_fluxes(lu)%plant(pft)%dtransp
      tile_fluxes(lu)%canopy%dlatenth = tile_fluxes(lu)%canopy%dlatenth + tile_fluxes(lu)%plant(pft)%dlatenth
      tile_fluxes(lu)%canopy%drleaf = tile_fluxes(lu)%canopy%drleaf + tile_fluxes(lu)%plant(pft)%drleaf
      tile_fluxes(lu)%canopy%drroot = tile_fluxes(lu)%canopy%drroot + tile_fluxes(lu)%plant(pft)%drroot
      tile_fluxes(lu)%canopy%drsapw = tile_fluxes(lu)%canopy%drsapw + tile_fluxes(lu)%plant(pft)%drsapw
      tile_fluxes(lu)%canopy%drgrow = tile_fluxes(lu)%canopy%drgrow + tile_fluxes(lu)%plant(pft)%drgrow
      tile_fluxes(lu)%canopy%dcex = tile_fluxes(lu)%canopy%dcex + tile_fluxes(lu)%plant(pft)%dcex
      tile_fluxes(lu)%canopy%dnup_pas = tile_fluxes(lu)%canopy%dnup_pas + tile_fluxes(lu)%plant(pft)%dnup_pas
      tile_fluxes(lu)%canopy%dnup_act = tile_fluxes(lu)%canopy%dnup_act + tile_fluxes(lu)%plant(pft)%dnup_act
      tile_fluxes(lu)%canopy%dnup_fix = tile_fluxes(lu)%canopy%dnup_fix + tile_fluxes(lu)%plant(pft)%dnup_fix
      tile_fluxes(lu)%canopy%dnup_ret = tile_fluxes(lu)%canopy%dnup_ret + tile_fluxes(lu)%plant(pft)%dnup_ret

      ! canopy-level quantities as FPC-weighted mean
      tile_fluxes(lu)%canopy%vcmax25 = tile_fluxes(lu)%canopy%vcmax25 + &
        tile_fluxes(lu)%plant(pft)%vcmax25 * tile(lu)%plant(pft)%fpc_grid
      tile_fluxes(lu)%canopy%jmax25 = tile_fluxes(lu)%canopy%jmax25 + &
        tile_fluxes(lu)%plant(pft)%jmax25 * tile(lu)%plant(pft)%fpc_grid
      tile_fluxes(lu)%canopy%vcmax = tile_fluxes(lu)%canopy%vcmax + &
        tile_fluxes(lu)%plant(pft)%vcmax * tile(lu)%plant(pft)%fpc_grid
      tile_fluxes(lu)%canopy%jmax = tile_fluxes(lu)%canopy%jmax + &
        tile_fluxes(lu)%plant(pft)%jmax * tile(lu)%plant(pft)%fpc_grid
      tile_fluxes(lu)%canopy%gs_accl = tile_fluxes(lu)%canopy%gs_accl + &
        tile_fluxes(lu)%plant(pft)%gs_accl * tile(lu)%plant(pft)%fpc_grid
      tile_fluxes(lu)%canopy%chi = tile_fluxes(lu)%canopy%chi + &
        tile_fluxes(lu)%plant(pft)%chi * tile(lu)%plant(pft)%fpc_grid
      tile_fluxes(lu)%canopy%iwue = tile_fluxes(lu)%canopy%iwue + &
        tile_fluxes(lu)%plant(pft)%iwue * tile(lu)%plant(pft)%fpc_grid
      tile_fluxes(lu)%canopy%asat = tile_fluxes(lu)%canopy%asat + &
        tile_fluxes(lu)%plant(pft)%asat * tile(lu)%plant(pft)%fpc_grid

      tile_fluxes(lu)%canopy%npp_leaf = tile_fluxes(lu)%canopy%npp_leaf + &
        tile_fluxes(lu)%plant(pft)%npp_leaf * tile(lu)%plant(pft)%fpc_grid
      tile_fluxes(lu)%canopy%npp_root = tile_fluxes(lu)%canopy%npp_root + &
        tile_fluxes(lu)%plant(pft)%npp_root * tile(lu)%plant(pft)%fpc_grid
      tile_fluxes(lu)%canopy%npp_wood = tile_fluxes(lu)%canopy%npp_wood + &
        tile_fluxes(lu)%plant(pft)%npp_wood * tile(lu)%plant(pft)%fpc_grid

      ! derived types canopy-level quantities as sums
      tile_fluxes(lu)%canopy%dnpp  = cplus( tile_fluxes(lu)%canopy%dnpp, tile_fluxes(lu)%plant(pft)%dnpp )
      tile_fluxes(lu)%canopy%dnup  = nplus( tile_fluxes(lu)%canopy%dnup, tile_fluxes(lu)%plant(pft)%dnup )
      tile_fluxes(lu)%canopy%dharv = orgplus( tile_fluxes(lu)%canopy%dharv, tile_fluxes(lu)%plant(pft)%dharv )

      !----------------------------------------------------------------
      ! pools
      !----------------------------------------------------------------
      tile(lu)%canopy%nind = tile(lu)%canopy%nind + tile(lu)%plant(pft)%nind 
      tile(lu)%canopy%fpc_grid = tile(lu)%canopy%fpc_grid + tile(lu)%plant(pft)%fpc_grid 
      tile(lu)%canopy%lai = tile(lu)%plant(pft)%lai_ind 
      tile(lu)%canopy%fapar = tile(lu)%canopy%fapar + tile(lu)%plant(pft)%fapar_ind * tile(lu)%plant(pft)%fpc_grid
      tile(lu)%canopy%acrown = tile(lu)%canopy%acrown + tile(lu)%plant(pft)%acrown 
      tile(lu)%canopy%narea = tile(lu)%canopy%narea + tile(lu)%plant(pft)%narea 
      tile(lu)%canopy%narea_metabolic = tile(lu)%canopy%narea_metabolic + tile(lu)%plant(pft)%narea_metabolic 
      tile(lu)%canopy%narea_structural = tile(lu)%canopy%narea_structural + tile(lu)%plant(pft)%narea_structural 
      tile(lu)%canopy%lma = tile(lu)%canopy%lma + tile(lu)%plant(pft)%lma 
      tile(lu)%canopy%sla = tile(lu)%canopy%sla + tile(lu)%plant(pft)%sla 
      tile(lu)%canopy%nmass = tile(lu)%canopy%nmass + tile(lu)%plant(pft)%nmass 
      tile(lu)%canopy%r_cton_leaf = tile(lu)%canopy%r_cton_leaf + tile(lu)%plant(pft)%r_cton_leaf 
      tile(lu)%canopy%r_ntoc_leaf = tile(lu)%canopy%r_ntoc_leaf + tile(lu)%plant(pft)%r_ntoc_leaf

      tile(lu)%canopy%pleaf = orgplus( tile(lu)%canopy%pleaf, tile(lu)%plant(pft)%pleaf )
      tile(lu)%canopy%proot = orgplus( tile(lu)%canopy%proot, tile(lu)%plant(pft)%proot )
      tile(lu)%canopy%psapw = orgplus( tile(lu)%canopy%psapw, tile(lu)%plant(pft)%psapw )
      tile(lu)%canopy%pwood = orgplus( tile(lu)%canopy%pwood, tile(lu)%plant(pft)%pwood )
      tile(lu)%canopy%plabl = orgplus( tile(lu)%canopy%plabl, tile(lu)%plant(pft)%plabl )

    end do

    !----------------------------------------------------------------
    ! populate function return variable
    !----------------------------------------------------------------
    ! if (nlu>1) stop 'think about nlu > 1'
    ! lu = 1
    ! out_biosphere%fapar   = tile(lu)%canopy%fapar
    ! out_biosphere%gpp     = tile_fluxes(lu)%canopy%dgpp
    ! out_biosphere%transp  = tile_fluxes(lu)%canopy%daet
    ! out_biosphere%latenth = tile_fluxes(lu)%canopy%daet_e
    ! out_biosphere%pet     = tile_fluxes(lu)%canopy%dpet
    ! out_biosphere%vcmax   = tile_fluxes(lu)%canopy%vcmax
    ! out_biosphere%jmax    = tile_fluxes(lu)%canopy%jmax
    ! out_biosphere%vcmax25 = tile_fluxes(lu)%canopy%vcmax25
    ! out_biosphere%jmax25  = tile_fluxes(lu)%canopy%jmax25
    ! out_biosphere%gs_accl = tile_fluxes(lu)%canopy%gs_accl
    ! out_biosphere%chi     = tile_fluxes(lu)%canopy%chi
    ! out_biosphere%iwue    = tile_fluxes(lu)%canopy%iwue
    ! out_biosphere%wscal   = tile(lu)%soil%phy%wscal
    ! out_biosphere%tsoil   = tile(lu)%soil%phy%temp
    ! out_biosphere%cleaf   = tile(lu)%canopy%pleaf%c%c12
    ! out_biosphere%nleaf   = tile(lu)%canopy%pleaf%n%n14
    ! out_biosphere%croot   = tile(lu)%canopy%proot%c%c12
    ! out_biosphere%nroot   = tile(lu)%canopy%proot%n%n14
    ! out_biosphere%clabl   = tile(lu)%canopy%plabl%c%c12
    ! out_biosphere%nlabl   = tile(lu)%canopy%plabl%n%n14
    ! out_biosphere%lai     = tile(lu)%canopy%lai
    ! out_biosphere%ninorg  = tile(lu)%soil%pno3%n14 + tile(lu)%soil%pnh4%n14
    ! out_biosphere%pno3    = tile(lu)%soil%pno3%n14
    ! out_biosphere%pnh4    = tile(lu)%soil%pnh4%n14
    ! out_biosphere%en2o    = 0.0
    ! out_biosphere%enleach = 0.0
    ! out_biosphere%csoil   = tile(lu)%soil%psoil_sl%c%c12 + tile(lu)%soil%psoil_fs%c%c12
    ! out_biosphere%nsoil   = tile(lu)%soil%psoil_sl%n%n14 + tile(lu)%soil%psoil_fs%n%n14 
    ! out_biosphere%clitt   = tile(lu)%soil%plitt_af%c%c12 + tile(lu)%soil%plitt_as%c%c12 + tile(lu)%soil%plitt_bg%c%c12
    ! out_biosphere%nlitt   = tile(lu)%soil%plitt_af%n%n14 + tile(lu)%soil%plitt_as%n%n14 + tile(lu)%soil%plitt_bg%n%n14
    ! out_biosphere%nfix    = 0.0
    ! out_biosphere%nup     = 0.0
    ! out_biosphere%cex     = 0.0
    ! out_biosphere%netmin  = tile_fluxes(lu)%soil%dnetmin%n14
    ! out_biosphere%dcharv  = tile_fluxes(lu)%canopy%dharv%c%c12
    ! out_biosphere%dnharv  = tile_fluxes(lu)%canopy%dharv%n%n14
    ! out_biosphere%npp     = tile_fluxes(lu)%canopy%dnpp%c12
    ! out_biosphere%drd     = tile_fluxes(lu)%canopy%drd

    ! xxx debug: writing PFT-level to output
    lu = 1
    pft = 1
    out_biosphere%fapar   = tile(lu)%plant(pft)%fapar_ind
    out_biosphere%gpp     = tile_fluxes(lu)%plant(pft)%dgpp
    out_biosphere%transp  = tile_fluxes(lu)%canopy%daet
    out_biosphere%latenth = tile_fluxes(lu)%canopy%daet_e
    out_biosphere%pet     = tile_fluxes(lu)%canopy%dpet
    out_biosphere%vcmax   = tile_fluxes(lu)%plant(pft)%vcmax
    out_biosphere%jmax    = tile_fluxes(lu)%plant(pft)%jmax
    out_biosphere%vcmax25 = tile_fluxes(lu)%plant(pft)%vcmax25
    out_biosphere%jmax25  = tile_fluxes(lu)%plant(pft)%jmax25
    out_biosphere%gs_accl = tile_fluxes(lu)%plant(pft)%gs_accl
    out_biosphere%chi     = tile_fluxes(lu)%plant(pft)%chi
    out_biosphere%iwue    = tile_fluxes(lu)%plant(pft)%iwue
    out_biosphere%asat    = tile_fluxes(lu)%canopy%asat
    out_biosphere%wscal   = tile(lu)%soil%phy%wscal
    out_biosphere%tsoil   = tile(lu)%soil%phy%temp
    out_biosphere%cleaf   = tile(lu)%plant(pft)%pleaf%c%c12
    out_biosphere%nleaf   = tile(lu)%plant(pft)%pleaf%n%n14
    out_biosphere%croot   = tile(lu)%plant(pft)%proot%c%c12
    out_biosphere%nroot   = tile(lu)%plant(pft)%proot%n%n14
    out_biosphere%clabl   = tile(lu)%plant(pft)%plabl%c%c12
    out_biosphere%nlabl   = tile(lu)%plant(pft)%plabl%n%n14
    out_biosphere%lai     = tile(lu)%plant(pft)%lai_ind
    out_biosphere%ninorg  = tile(lu)%soil%pno3%n14 + tile(lu)%soil%pnh4%n14
    out_biosphere%pno3    = tile(lu)%soil%pno3%n14
    out_biosphere%pnh4    = tile(lu)%soil%pnh4%n14
    out_biosphere%dn2o    = tile_fluxes(lu)%soil%dn2o
    out_biosphere%dnleach = tile_fluxes(lu)%soil%dnleach
    out_biosphere%csoil   = tile(lu)%soil%psoil_sl%c%c12 + tile(lu)%soil%psoil_fs%c%c12
    out_biosphere%nsoil   = tile(lu)%soil%psoil_sl%n%n14 + tile(lu)%soil%psoil_fs%n%n14 
    out_biosphere%clitt   = tile(lu)%soil%plitt_af%c%c12 + tile(lu)%soil%plitt_as%c%c12 + tile(lu)%soil%plitt_bg%c%c12
    out_biosphere%nlitt   = tile(lu)%soil%plitt_af%n%n14 + tile(lu)%soil%plitt_as%n%n14 + tile(lu)%soil%plitt_bg%n%n14
    out_biosphere%nfix    = tile_fluxes(lu)%plant(pft)%dnup_fix
    out_biosphere%nup     = tile_fluxes(lu)%plant(pft)%dnup%n14
    out_biosphere%cex     = tile_fluxes(lu)%plant(pft)%dcex
    out_biosphere%netmin  = tile_fluxes(lu)%soil%dnetmin%n14
    out_biosphere%dcharv  = tile_fluxes(lu)%plant(pft)%dharv%c%c12
    out_biosphere%dnharv  = tile_fluxes(lu)%plant(pft)%dharv%n%n14
    out_biosphere%npp     = tile_fluxes(lu)%plant(pft)%dnpp%c12
    out_biosphere%drd     = tile_fluxes(lu)%plant(pft)%drd    
    out_biosphere%lma     = tile(lu)%plant(pft)%lma
    out_biosphere%narea   = tile(lu)%plant(pft)%narea
    out_biosphere%narea_v = tile(lu)%plant(pft)%narea_metabolic
    out_biosphere%nloss   = tile_fluxes(lu)%soil%dnloss
    out_biosphere%seedc   = tile(lu)%plant(pft)%pseed%c%c12
    out_biosphere%seedn   = tile(lu)%plant(pft)%pseed%n%n14
    out_biosphere%npp_leaf= tile_fluxes(lu)%canopy%npp_leaf
    out_biosphere%npp_root= tile_fluxes(lu)%canopy%npp_root
    out_biosphere%npp_wood= tile_fluxes(lu)%canopy%npp_wood

    ! for debugging purposes
    out_biosphere%x1      = tile_fluxes(lu)%plant(pft)%debug1
    out_biosphere%x2      = tile_fluxes(lu)%plant(pft)%debug2
    out_biosphere%x3      = tile_fluxes(lu)%plant(pft)%debug3
    out_biosphere%x4      = tile_fluxes(lu)%plant(pft)%debug4

  end subroutine diag_daily


  subroutine diag_annual( tile, tile_fluxes )
    !////////////////////////////////////////////////////////////////
    ! Annual diagnostics
    ! Write to (experimental) files
    !----------------------------------------------------------------
    use md_interface_cnmodel, only: myinterface

    ! arguments
    type(tile_type), dimension(nlu), intent(in) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(in) :: tile_fluxes

    ! local
    integer :: lu, pft

    !////////////////////////////////////////////////////////////////
    ! Annual output to file
    !----------------------------------------------------------------
    lu = 1
    pft = 1

    ! soil C
    write(101, 999) myinterface%steering%outyear, (tile(lu)%soil%psoil_sl%c%c12 + tile(lu)%soil%psoil_fs%c%c12)

    ! soil N
    write(102, 999) myinterface%steering%outyear, (tile(lu)%soil%psoil_sl%n%n14 + tile(lu)%soil%psoil_fs%n%n14)

    return
    999 format (I4.4, F20.8)

  end subroutine diag_annual

end module md_tile_cnmodel
