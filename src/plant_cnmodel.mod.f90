module md_plant_cnmodel
  !////////////////////////////////////////////////////////////////
  ! Module specifying how a plant looks like and how it behaves
  ! for CN-model simulations
  !----------------------------------------------------------------
  use md_classdefs
  use md_params_core, only: ndayyear, npft, nlu, lunat
  use md_interface_cnmodel, only: myinterface

  implicit none

  private
  public plant_type, plant_fluxes_type, getpar_modl_plant, &
    init_plant, params_plant, params_pft_plant, ftemp, &
    fmoist, add_seed, update_leaftraits, get_leaftraits_init, &
    get_lai, get_fapar, init_plant_fluxes, get_leaf_n_canopy, &

    ! debug
    r_cton_leaf, r_ntoc_leaf

  !----------------------------------------------------------------
  ! NON PFT-DEPENDENT GLOBAL PARAMETERS
  !----------------------------------------------------------------
  type params_plant_type
    real :: kbeer             ! canopy light extinction coefficient
    real :: r_root            ! Fine root-specific respiration rate (gC gC-1 d-1)
    real :: r_sapw            ! Sapwood-specific respiration rate (gC gC-1 d-1)
    real :: exurate           ! Fine root-specific C export rate (gC gC-1 d-1)
    real :: f_nretain         ! fraction of N retained at leaf abscission 
    real :: fpc_tree_max      ! maximum fractional plant coverage of trees
    real :: growtheff         ! growth respiration coefficient = yield factor [unitless]
    ! real :: cton_soil         ! C:N ratio of soil organic matter (consider this to be equal to that of microbial biomass)
    real :: frac_leaf         ! fraction of allocatable C to leaf 
  end type params_plant_type

  type( params_plant_type ) :: params_plant

  !----------------------------------------------------------------
  ! PFT-DEPENDENT PARAMETERS
  !----------------------------------------------------------------
  type params_pft_plant_type

    character(len=4) :: pftname    ! standard PFT name with 4 characters length
    integer :: lu_category         ! land use category associated with PFT
    logical, dimension(nlu) :: islu! islu(ipft,ilu) is true if ipft belongs to ilu
    logical :: grass               ! boolean for growth form 'grass'
    logical :: tree                ! boolean for growth form 'tree'
    logical :: nfixer              ! whether plant is capable of symbiotically fixing N
    logical :: c3                  ! whether plant follows C3 photosynthesis
    logical :: c4                  ! whether plant follows C4 photosynthesis
    real    :: sla                 ! specific leaf area (m2 gC-1)
    real    :: lma                 ! leaf mass per area (gC m-2)
    real    :: r_ntolma            ! constant ratio of structural N to C (LMA) (gN/gC)

    ! new for cnmodel
    real    :: k_decay_leaf        ! base leaf decay constant [year-1]
    real    :: k_decay_sapw        ! (sap)wood decay constant [year-1]
    real    :: k_decay_root        ! root decay constant [year-1]
    real    :: k_decay_labl        ! labile pool decay constant [year-1]
    real    :: r_cton_root         ! C:N ratio in roots (gC/gN)
    real    :: r_ntoc_root         ! N:C ratio in roots (inverse of 'r_cton_root', gN/gC)
    real    :: r_cton_wood         ! C:N ratio in wood (gC/gN)
    real    :: r_ntoc_wood         ! N:C ratio in wood (gC/gN)
    real    :: r_cton_seed         ! C:N ratio in seeds (gC/gN)
    real    :: r_ntoc_seed         ! N:C ratio in seeds (gN/gC)
    real    :: nv_vcmax25          ! relationship between Vcmax25 and metabolic leaf N (including N in other enzymes - assumed to scale with Vcmax25)
    real    :: ncw_min             ! y-axis intersection in the relationship of non-metabolic versus metabolic N per leaf area    
    real    :: r_n_cw_v            ! slope in the relationship of non-metabolic versus metabolic N per leaf area              
    real    :: r_ctostructn_leaf   ! constant ratio of C to structural N (mol C / mol N)
    
  end type params_pft_plant_type

  type(params_pft_plant_type), dimension(npft) :: params_pft_plant


  !----------------------------------------------------------------
  ! Daily phenology dates for each PFT
  !----------------------------------------------------------------
  type pheno_ann_type
    real    :: dtphen            ! daily temperature-driven phenology (=dphen_t in LPX)
    logical :: sprout            ! boolean when PFT is sprouting
    logical :: shedleaves        ! boolean when PFT is shedding leaves
  end type pheno_ann_type

  type pheno_type
    real :: level_coldacclim  ! alternative temperature-driven phenology, governing cold acclimation of photosynthesis (hardening) and sprouting of grasses (value between 0 and 1)
    real :: level_veggrowth   ! phenophase of vegetative growth vs. seed filling (implemented as withholding C from allocation and keeping it in the labile pool)
  end type pheno_type

  !----------------------------------------------------------------
  ! Pools and other variables with year-to-year memory
  !----------------------------------------------------------------
  type plant_type

    ! PFT index that goes along with this instance of 'plant'
    integer :: pftno

    ! canopy at individual-level
    integer :: nind             ! number of individuals (m-2)
    real :: fpc_grid            ! fractional projective cover
    real :: lai_ind             ! fraction of absorbed photosynthetically active radiation
    real :: fapar_ind           ! fraction of absorbed photosynthetically active radiation
    real :: acrown              ! crown area

    ! leaf traits, whole plant-level, in units of gN / m2-ground 
    real :: narea_metabolic_canopy  ! canopy total metabolic leaf N per unit ground area (gN m-2)
    real :: narea_structural_canopy ! canopy total structural leaf N per unit ground area (gN m-2)
    real :: narea_canopy            ! canopy total leaf N per unit ground area (gN m-2)
    real :: leafc_canopy            ! canopy total leaf C per unit ground area (gC m-2)

    ! leaf traits, leaf-level, in units of gN / m2-leaf 
    real :: narea               ! total leaf N per unit leaf area (gN m-2)
    real :: narea_metabolic     ! metabolic leaf N per unit leaf area (gN m-2)
    real :: narea_structural    ! structural leaf N per unit leaf area (gN m-2)
    real :: lma                 ! leaf mass per area (gC m-2)
    real :: sla                 ! specific leaf area (m2 gC-1)
    real :: nmass               ! leaf N per unit leaf mass, g N / g-dry mass
    real :: r_cton_leaf         ! leaf C:N ratio [gC/gN] 
    real :: r_ntoc_leaf         ! leaf N:C ratio [gN/gC]
    real :: actnv_unitfapar     ! metabolic leaf N per unit fAPAR (g N m-2 mol-1)

    ! pheno phases
    logical :: fill_seeds       ! switch to change from seed filling to growth period

    ! pools
    type(orgpool) :: pleaf     ! leaf biomass
    type(orgpool) :: proot     ! root biomass
    type(orgpool) :: pwood     ! woody biomass
    type(orgpool) :: plabl     ! labile pool
    type(orgpool) :: presv     ! reserves pool
    type(orgpool) :: pseed     ! seed pool

    ! phenology
    type(pheno_type) :: pheno  ! phenology state, daily updated
    type(pheno_ann_type), dimension(ndayyear) :: pheno_ann  ! phenology state, calculated a priori (containing all days of year)

  end type plant_type


  !----------------------------------------------------------------
  ! Fluxes and other variables with no memory
  !----------------------------------------------------------------
  type plant_fluxes_type

    ! daily updated variables
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

    type(orgpool) :: dlabl     ! labile turnover, doesn't appear as NPP, therefore needs to be accounted for separately [gC/m2/d]
    
    type(carbon)   :: dnpp     ! daily net primary production (gpp-ra, npp=bp+cex) [gC/m2/d]
    type(nitrogen) :: dnup     ! daily N uptake [gN/m2/d]

    real :: dnup_pas          ! daily N uptake by passsive uptake (transpiration) [gN/m2/d]
    real :: dnup_act          ! daily N uptake by active uptake [gN/m2/d]
    real :: dnup_fix          ! daily N uptake by plant symbiotic N fixation [gN/m2/d]
    real :: dnup_res          ! daily N resorption [gN/m2/d]

    real :: vcmax25           ! acclimated Vcmax, normalised to 25 deg C (mol CO2 m-2 s-1)
    real :: jmax25            ! acclimated Jmax, normalised to 25 deg C (mol CO2 m-2 s-1)
    real :: vcmax             ! daily varying Vcmax (mol CO2 m-2 s-1)
    real :: jmax              ! daily varying Jmax (mol CO2 m-2 s-1)
    real :: gs_accl           ! acclimated stomatal conductance (xxx)
    real :: chi               ! ci:ca ratio (unitless)
    real :: iwue              ! intrinsic water use efficiency (A/gs = ca*(1-chi))
    real :: asat              ! light-saturated assimilation rate (mol CO2 m-2 s-1)
    real :: lue               ! light use efficiency (gC m-2 mol-1)
    real :: vcmax25_unitfapar ! acclimated Vcmax per unit fAPAR, normalised to 25 deg C (mol CO2 m-2 s-1)

    type(orgpool) :: npp_leaf          ! carbon and nitrogen allocated to leaves (g m-2 d-1)
    type(orgpool) :: npp_root          ! carbon and nitrogen allocated to roots (g m-2 d-1)
    type(orgpool) :: npp_wood          ! carbon and nitrogen allocated to wood (sapwood (g m-2 d-1))
    type(orgpool) :: npp_seed          ! carbon and nitrogen allocated to seeds (g m-2 d-1)

    real :: debug1             ! write anything into this
    real :: debug2             ! write anything into this
    real :: debug3             ! write anything into this
    real :: debug4             ! write anything into this

    type(orgpool) :: dharv    ! daily total biomass harvest (g m-2 d-1)

    ! type(orgpool) :: alloc_leaf, alloc_root, alloc_sapw, alloc_wood, alloc_seed

  end type plant_fluxes_type

  !-----------------------------------------------------------------------
  ! Fixed parameters
  !-----------------------------------------------------------------------
  ! type( orgpool ), parameter :: seed = orgpool( carbon(5.0), nitrogen(0.0) )
  type( orgpool ), parameter :: seed = orgpool( carbon(5.0), nitrogen(0.12) )
  ! type( orgpool ), parameter :: seed = orgpool( carbon(100.0), nitrogen(1 .0) )

  ! xxx debug
  real, parameter :: r_cton_leaf = 20.0
  real, parameter :: r_ntoc_leaf = 0.05

contains

  ! function get_canopy( lai ) result( out_canopy )
  !   !//////////////////////////////////////////////////////////////////
  !   ! Returs canopy variables as a function of LAI
  !   !------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in) :: lai

  !   ! function return value
  !   type( canopy_type ) :: out_canopy

  !   out_canopy%fapar_ind = get_fapar( lai )

  ! end function get_canopy


  function get_fapar( lai ) result( fapar )
    !////////////////////////////////////////////////////////////////
    ! FOLIAGE PROJECTIVE COVER 
    ! = Fraction of Absorbed Photosynthetically Active Radiation
    ! Function returns fractional plant cover an individual
    ! Eq. 7 in Sitch et al., 2003
    !----------------------------------------------------------------
    ! arguments
    real, intent(in) :: lai

    ! function return variable
    real :: fapar

    fapar = ( 1.0 - exp( -1.0 * params_plant%kbeer * lai) )

  end function get_fapar


  function get_lai( pft, cleaf, actnv_unitfapar ) result( lai )
    !////////////////////////////////////////////////////////////////
    ! Calculates LAI as a function of leaf-C. This is not so straight
    ! forward due to the dependency of canopy-metabolic leaf N on LAI,
    ! and the dependence of canopy-structural leaf N and C on canopy-
    ! metabolic leaf N.
    !----------------------------------------------------------------
    use md_params_core, only: nmonth, c_molmass
    use md_lambertw, only: calc_wapr

    ! arguments
    integer, intent(in) :: pft
    real, intent(in) :: cleaf
    real, intent(in) :: actnv_unitfapar 

    ! function return variable
    real :: lai

    ! local variables
    real    :: alpha, beta, gamma ! variable substitutes
    real    :: arg_to_lambertw
    integer :: nerror


    if (cleaf > 0.0) then

      ! Monthly variations in metabolic N, determined by variations in meanmppfd and nv should not result in variations in leaf traits. 
      ! In order to prevent this, assume annual maximum metabolic N, part of which is deactivated during months with lower insolation (and Rd reduced.)
      ! maxnv = actnv_unitfapar (as done before)

      alpha = actnv_unitfapar * params_pft_plant(pft)%r_n_cw_v
      beta  = params_pft_plant(pft)%ncw_min
      gamma = cleaf / ( c_molmass * params_pft_plant(pft)%r_ctostructn_leaf ) 
      arg_to_lambertw = alpha * params_plant%kbeer / beta * exp( (alpha - gamma) * params_plant%kbeer / beta )
      lai = 1.0 / (beta * params_plant%kbeer ) * &
        ( -alpha * params_plant%kbeer + &
          gamma * params_plant%kbeer + &
          beta * calc_wapr( arg_to_lambertw, 0, nerror, 9999 ) &
        )

    else

      lai = 0.0

    end if
    
  end function get_lai


  function get_leaf_n_metabolic_canopy( fapar, actnv_unitfapar ) result( mynleaf_metabolic )
    !////////////////////////////////////////////////////////////////
    ! Calculates metabolic leaf N at canopy-level, determined by 
    ! light conditions (meanmppfd) and the Rubisco-N per unit absorbed
    ! light.
    !----------------------------------------------------------------
    use md_params_core, only: nmonth

    ! arguments
    real, intent(in) :: fapar, actnv_unitfapar

    ! function return variable
    real :: mynleaf_metabolic  ! mol N m-2-ground

    ! Metabolic N is predicted and is optimised at a monthly time scale. 
    ! Leaf traits are calculated based on metabolic N => cellwall N => cellwall C / LMA
    ! Leaves get thinner at the bottom of the canopy => increasing LAI through the season comes at a declining C and N cost
    ! Monthly variations in metabolic N, determined by variations in meanmppfd and nv should not result in variations in leaf traits. 
    ! In order to prevent this, assume annual maximum metabolic N, part of which is deactivated during months with lower insolation (and Rd reduced.)

    ! if (present(myfapar)) then
    !   mynleaf_metabolic = meanmppfd * nv * myfapar
    ! else
    !   mynleaf_metabolic = meanmppfd * nv * get_fapar( mylai )
    ! end if

    mynleaf_metabolic = fapar * actnv_unitfapar

  end function get_leaf_n_metabolic_canopy


  function get_leaf_n_structural_canopy( pft, lai, mynleaf_metabolic ) result( mynleaf_structural )
    !////////////////////////////////////////////////////////////////
    ! Calculates structural leaf N at canopy-level, determined by 
    ! metabolic leaf N (linear relationship)
    !----------------------------------------------------------------
    ! arguments
    integer, intent(in) :: pft
    real, intent(in)    :: lai
    real, intent(in)    :: mynleaf_metabolic

    ! function return variable
    real :: mynleaf_structural  ! mol N m-2-ground

    mynleaf_structural = mynleaf_metabolic * params_pft_plant(pft)%r_n_cw_v + lai * params_pft_plant(pft)%ncw_min

  end function get_leaf_n_structural_canopy


  function get_leaf_n_canopy( pft, lai, mynleaf_metabolic ) result( mynleaf )
    !////////////////////////////////////////////////////////////////
    ! Calculates total leaf N at canopy-level, determined by 
    ! metabolic leaf N (linear relationship)
    ! Caution: this returns g N m-2-ground (not mol N m-2-ground)!
    !----------------------------------------------------------------
    use md_params_core, only: nmonth, n_molmass

    ! arguments
    integer, intent(in) :: pft
    real, intent(in)    :: lai
    real, intent(in)    :: mynleaf_metabolic

    ! function return variable
    real :: mynleaf ! g N m-2-ground

    ! local variables
    real :: fapar
    real :: nleaf_metabolic   ! mol N m-2
    real :: nleaf_structural  ! mol N m-2

    fapar            = get_fapar( lai )
    nleaf_metabolic  = get_leaf_n_metabolic_canopy( fapar, mynleaf_metabolic )
    nleaf_structural = get_leaf_n_structural_canopy( pft, lai, nleaf_metabolic )
    mynleaf          = n_molmass * ( nleaf_metabolic + nleaf_structural )

  end function get_leaf_n_canopy


  subroutine update_leaftraits( plant )
    !////////////////////////////////////////////////////////////////
    ! Calculates leaf traits, given updated fAPAR and LAI 
    ! based on (predicted) metabolic Narea and
    ! (prescribed) parameters that relate structural to metabolic
    ! Narea and Carea to structural Narea:
    ! Narea_metabolic  = predicted
    ! Narea_structural = a + b * Narea_metabolic
    ! Carea            = c * Narea_structural
    !----------------------------------------------------------------
    use md_params_core, only: c_content_of_biomass, nmonth, n_molmass, c_molmass

    ! arguments
    type( plant_type ), intent(inout) :: plant

    ! local variables
    real :: mynarea_metabolic_canop   ! mol N m-2-ground
    real :: mynarea_structural_canop  ! mol N m-2-ground

    ! calculate quantities in units of mol N
    mynarea_metabolic_canop  = get_leaf_n_metabolic_canopy( plant%fapar_ind, plant%actnv_unitfapar )               ! mol N m-2-ground    
    mynarea_structural_canop = get_leaf_n_structural_canopy( plant%pftno, plant%lai_ind, mynarea_metabolic_canop ) ! mol N m-2-ground

    ! canopy-level, in units of gN / m2-ground 
    plant%narea_metabolic_canopy  = n_molmass * mynarea_metabolic_canop ! g N m-2-ground 
    plant%narea_structural_canopy = n_molmass * mynarea_structural_canop ! g N m-2-ground
    plant%narea_canopy            = n_molmass * (mynarea_metabolic_canop + mynarea_structural_canop)  ! g N m-2-ground
    plant%leafc_canopy            = c_molmass * params_pft_plant(plant%pftno)%r_ctostructn_leaf * mynarea_structural_canop ! g C m-2-ground

    ! xxx debug: resolve this. should be identical.
    ! print*,'plant%leafc_canopy, plant%pleaf%c%c12 ', plant%leafc_canopy, plant%pleaf%c%c12

    ! leaf-level, in units of gN / m2-leaf 
    plant%narea_metabolic  = plant%narea_metabolic_canopy / plant%lai_ind   ! g N m-2-leaf
    plant%narea_structural = plant%narea_structural_canopy / plant%lai_ind  ! g N m-2-leaf
    plant%narea            = plant%narea_canopy / plant%lai_ind ! g N m-2-leaf
    plant%lma              = plant%leafc_canopy / plant%lai_ind ! g C m-2-leaf

    ! additional traits
    plant%nmass            = plant%narea / ( plant%lma / c_content_of_biomass )
    plant%r_cton_leaf      = plant%lma / plant%narea
    plant%r_ntoc_leaf      = 1.0 / plant%r_cton_leaf

  end subroutine update_leaftraits


  subroutine get_leaftraits_init( plant )
    !////////////////////////////////////////////////////////////////
    ! Calculates initial leaf traits (Taylor approximation for LAI -> 0)
    !----------------------------------------------------------------
    use md_params_core, only: c_content_of_biomass, nmonth, n_molmass, c_molmass

    ! arguments
    type( plant_type ), intent(inout) :: plant

    ! local variables
    real :: narea_metabolic   ! mol N m-2-ground
    real :: narea_structural  ! mol N m-2-ground

    narea_metabolic  = plant%actnv_unitfapar * params_plant%kbeer
    narea_structural = params_pft_plant(plant%pftno)%r_n_cw_v * plant%actnv_unitfapar &
      * params_plant%kbeer + params_pft_plant(plant%pftno)%ncw_min

    ! leaf-level, in units of gN / m2-leaf 
    plant%narea_metabolic  = n_molmass * narea_metabolic  ! g N m-2-leaf
    plant%narea_structural = n_molmass * narea_structural ! g N m-2-leaf
    plant%narea            = n_molmass * ( narea_metabolic +  narea_structural ) ! g N m-2-leaf
    plant%lma              = c_molmass * &
      params_pft_plant(plant%pftno)%r_ctostructn_leaf * narea_structural

    ! additional traits
    plant%nmass            = plant%narea / ( plant%lma / c_content_of_biomass )
    plant%r_cton_leaf      = plant%lma / plant%narea
    plant%r_ntoc_leaf      = 1.0 / plant%r_cton_leaf

    ! ! canopy-level, in units of gN / m2-ground 
    ! plant%narea_metabolic_canopy  = 0.0
    ! plant%narea_structural_canopy = 0.0
    ! plant%narea_canopy            = 0.0
    ! plant%leafc_canopy            = 0.0

  end subroutine get_leaftraits_init


  subroutine getpar_modl_plant()
    !////////////////////////////////////////////////////////////////
    !  Subroutine reads model parameters from input file.
    !  It was necessary to separate this SR from module md_plant_cnmodel
    !  because this SR uses module md_waterbal, which also uses
    !  _plant.
    ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
    ! contact: b.stocker@imperial.ac.uk
    !----------------------------------------------------------------    
    ! local variables
    integer :: pft
    integer :: npft_site

    !----------------------------------------------------------------
    ! NON-PFT DEPENDENT PARAMETERS
    !----------------------------------------------------------------
    ! canopy light extinction coefficient for Beer's Law
    params_plant%kbeer        = myinterface%params_calib%kbeer

    ! fraction of N retained at leaf abscission 
    params_plant%f_nretain    = myinterface%params_calib%f_nretain
    
    ! maximum fractional plant coverage of trees (sum of all tree PFTs)
    params_plant%fpc_tree_max = myinterface%params_calib%fpc_tree_max

    ! growth efficiency       = yield factor, central value: 0.6, range: 0.5-0.7; Zhang et al. (2009), see Li et al., 2014
    params_plant%growtheff    = myinterface%params_calib%growtheff

    ! Fine-root mass specific respiration rate (gC gC-1 year-1)
    ! Central value           : 0.913 year-1 (Yan and Zhao (2007); see Li et al., 2014)
    params_plant%r_root       = myinterface%params_calib%r_root / ndayyear

    ! Sapwood specific respiration rate (gC gC-1 year-1)
    ! Central value           : 0.044 year-1 (Yan and Zhao (2007); see Li et al., 2014)
    ! (                       = 0.044 nmol mol-1 s-1; range: 0.5–10, 20 nmol mol-1 s-1 (Landsberg and Sands (2010))
    params_plant%r_sapw       = myinterface%params_calib%r_sapw / ndayyear

    ! C export rate per unit root mass
    params_plant%exurate      = myinterface%params_calib%exurate

    ! ! C:N ratio of soil organic matter [1]
    ! params_plant%cton_soil = myinterface%params_calib%cton_soil


    !----------------------------------------------------------------
    ! PFT DEPENDENT PARAMETERS
    ! read parameter input file and store values in single array
    ! important: Keep this order of reading PFT parameters fixed.
    !----------------------------------------------------------------
    pft = 0
    if ( myinterface%params_siml%lTrE ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'tre' )
    end if

    if ( myinterface%params_siml%lTNE ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'tne' )
    end if

    if ( myinterface%params_siml%lTrD ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'trd' )
    end if

    if ( myinterface%params_siml%lTND ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'tnd' )
    end if

    if ( myinterface%params_siml%lGr3 ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'gr3' )
    end if

    if ( myinterface%params_siml%lGN3 ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'gn3' )
    end if

    if ( myinterface%params_siml%lGr4 ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'gr4' )
    end if

    npft_site = pft
    ! if (npft_site==0) stop 'PLANT:GETPAR_MODL_PLANT: PFT name not valid. See run/<simulationname>.sofun.parameter'

  end subroutine getpar_modl_plant


  function getpftparams( pftname ) result( out_getpftparams )
    !----------------------------------------------------------------
    ! Read PFT parameters from respective file, given the PFT name
    !----------------------------------------------------------------
    ! arguments
    character(len=*), intent(in) :: pftname

    ! local variables
    real :: lu_category_prov = 0   ! land use category associated with PFT (provisional)

    ! function return variable
    type( params_pft_plant_type ) :: out_getpftparams

    ! standard PFT name
    out_getpftparams%pftname = pftname

    ! PFT names
    ! Gr3 : C3 grass                          
    ! Gr4 : C4 grass     
    if (trim(pftname)=='gr3') then
      out_getpftparams%grass   = .true.
      out_getpftparams%tree    = .false.
      out_getpftparams%c3      = .true.
      out_getpftparams%c4      = .false.
      out_getpftparams%nfixer  = .false.
    else if (trim(pftname)=='gn3') then
      out_getpftparams%grass   = .true.
      out_getpftparams%tree    = .false.
      out_getpftparams%c3      = .true.
      out_getpftparams%c4      = .false.
      out_getpftparams%nfixer  = .true.
    else if (trim(pftname)=='gr4') then
      out_getpftparams%grass   = .true.
      out_getpftparams%tree    = .false.
      out_getpftparams%c3      = .false.
      out_getpftparams%c4      = .true.
      out_getpftparams%nfixer  = .false.
    else if (trim(pftname)=='tre') then
      out_getpftparams%grass   = .false.
      out_getpftparams%tree    = .true.
      out_getpftparams%c3      = .true.
      out_getpftparams%c4      = .false.
      out_getpftparams%nfixer  = .false.
    else if (trim(pftname)=='tne') then
      out_getpftparams%grass   = .false.
      out_getpftparams%tree    = .true.
      out_getpftparams%c3      = .true.
      out_getpftparams%c4      = .false.
      out_getpftparams%nfixer  = .true.
    else if (trim(pftname)=='tnd') then
      out_getpftparams%grass   = .false.
      out_getpftparams%tree    = .true.
      out_getpftparams%c3      = .true.
      out_getpftparams%c4      = .false.
      out_getpftparams%nfixer  = .true.
    end if      

    out_getpftparams%lu_category = lunat

    ! ! land use category associated with PFT (provisional) 
    ! if (lu_category_prov==1) then
    !   out_getpftparams%lu_category = lunat
    !   out_getpftparams%islu(lunat) = .true.
    ! else
    !   out_getpftparams%islu(lunat) = .false.
    ! end if

    ! ! leaf mass per area (gC m-2)
    ! out_getpftparams%lma = getparreal( trim('params/params_plant_'//pftname//'.dat'), 'lma' )
    ! out_getpftparams%sla = 1.0 / out_getpftparams%lma

    ! ! constant ratio of leaf structural N to LMA
    ! out_getpftparams%r_ntolma = getparreal( trim('params/params_plant_'//pftname//'.dat'), 'r_ntolma' )

    ! leaf decay constant, read in as [years-1], central value: 0.0 yr-1 for deciduous plants
    out_getpftparams%k_decay_leaf = myinterface%params_calib%k_decay_leaf / ndayyear 

    ! sapwood decay constant [days], read in as [years-1], central value: xxx
    out_getpftparams%k_decay_sapw =  myinterface%params_calib%k_decay_sapw / ndayyear 

    ! root decay constant [days], read in as [years-1], central value: 1.04 (Shan et al., 1993; see Li et al., 2014)  
    out_getpftparams%k_decay_root = myinterface%params_calib%k_decay_root / ndayyear 

    ! wood decay constant [days], read in as [years-1], 1.0/100 
    out_getpftparams%k_decay_sapw = myinterface%params_calib%k_decay_sapw / ndayyear 

    ! root decay constant [days], read in as [years-1], central value: 1.04 (Shan et al., 1993; see Li et al., 2014)  
    out_getpftparams%k_decay_labl = myinterface%params_calib%k_decay_labl / ndayyear 

    ! wood C:N and N:C ratio (gC/gN and gN/gC)
    out_getpftparams%r_cton_wood = myinterface%params_calib%r_cton_wood
    out_getpftparams%r_ntoc_wood = 1.0 / out_getpftparams%r_cton_wood

    ! root C:N and N:C ratio (gC/gN and gN/gC)
    out_getpftparams%r_cton_root = myinterface%params_calib%r_cton_root
    out_getpftparams%r_ntoc_root = 1.0 / out_getpftparams%r_cton_root

    ! seed C:N and N:C ratio (gC/gN and gN/gC)
    out_getpftparams%r_cton_seed = myinterface%params_calib%r_cton_seed
    out_getpftparams%r_ntoc_seed = 1.0 / out_getpftparams%r_cton_seed

    ! relationship between Vcmax25 and metabolic leaf N (including N in other enzymes - assumed to scale with Vcmax25)
    out_getpftparams%nv_vcmax25 = myinterface%params_calib%nv_vcmax25

    ! y-axis intersection in the relationship of non-metabolic versus metabolic N per leaf area
    out_getpftparams%ncw_min = myinterface%params_calib%ncw_min

    ! slope in the relationship of non-metabolic versus metabolic N per leaf area
    out_getpftparams%r_n_cw_v = myinterface%params_calib%r_n_cw_v

    ! constant ratio of C to structural N
    out_getpftparams%r_ctostructn_leaf = myinterface%params_calib%r_ctostructn_leaf

  end function getpftparams


  subroutine add_seed( plant )
    !//////////////////////////////////////////////////////////////////
    ! To initialise plant pools, add "sapling" mass
    !------------------------------------------------------------------
    use md_classdefs

    ! arguments
    type(plant_type), intent(inout) :: plant

    plant%plabl = orgplus( plant%plabl, seed )

  end subroutine add_seed


  subroutine init_plant( plant )
    !////////////////////////////////////////////////////////////////
    !  Initialisation of all _pools on all gridcells at the beginning
    !  of the simulation.
    !  June 2014
    !  b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    ! argument
    type( plant_type ), dimension(npft), intent(inout) :: plant

    ! local variables
    integer :: pft
      
    plant(:)%nind      = 1.0
    plant(:)%fpc_grid  = 1.0
    plant(:)%lai_ind   = 0.0
    plant(:)%fapar_ind = 0.0
    plant(:)%acrown    = 0.0

    ! canpopy state variables
    plant(:)%narea_metabolic_canopy  = 0.0
    plant(:)%narea_structural_canopy = 0.0
    plant(:)%narea_canopy            = 0.0
    plant(:)%leafc_canopy            = 0.0
    plant(:)%narea                   = 0.0
    plant(:)%narea_metabolic         = 0.0
    plant(:)%narea_structural        = 0.0
    plant(:)%lma                     = 0.0
    plant(:)%sla                     = 0.0
    plant(:)%nmass                   = 0.0
    plant(:)%r_cton_leaf             = 0.0
    plant(:)%r_ntoc_leaf             = 0.0
    plant(:)%actnv_unitfapar         = 0.0

    do pft=1,npft
      plant(pft)%pftno = pft
      call orginit( plant(pft)%pleaf )
      call orginit( plant(pft)%proot )
      call orginit( plant(pft)%pwood )
      call orginit( plant(pft)%plabl )
      call orginit( plant(pft)%pseed )
      call orginit( plant(pft)%presv )
      call init_pheno(plant(pft)%pheno )
      call init_pheno_ann(plant(pft)%pheno_ann(:))
    end do

    plant(:)%fill_seeds = .false.

  end subroutine init_plant


  subroutine init_pheno( pheno )
    !////////////////////////////////////////////////////////////////
    !  Initialisation of phenology variables
    !----------------------------------------------------------------
    ! arguments
    type(pheno_type), intent(inout) :: pheno

    pheno%level_coldacclim = 0.0

  end subroutine init_pheno

  
  subroutine init_pheno_ann( pheno )
    !////////////////////////////////////////////////////////////////
    !  Initialisation of phenology variables
    !----------------------------------------------------------------
    ! arguments
    type(pheno_ann_type), dimension(ndayyear), intent(inout) :: pheno

    pheno(:)%dtphen     = 0.0
    pheno(:)%sprout     = .false.
    pheno(:)%shedleaves = .false.

  end subroutine init_pheno_ann


  subroutine init_plant_fluxes( plant_fluxes )
    !////////////////////////////////////////////////////////////////
    ! Initialises all flux variables at plant level
    !----------------------------------------------------------------
    ! arguments
    type(plant_fluxes_type), dimension(npft), intent(inout) :: plant_fluxes

    ! local
    integer :: pft

    ! daily updated variables
    plant_fluxes(:)%dgpp = 0.0
    plant_fluxes(:)%drd = 0.0
    plant_fluxes(:)%assim = 0.0
    plant_fluxes(:)%dtransp = 0.0
    plant_fluxes(:)%dlatenth = 0.0
    plant_fluxes(:)%drleaf = 0.0
    plant_fluxes(:)%drroot = 0.0
    plant_fluxes(:)%drsapw = 0.0
    plant_fluxes(:)%drgrow = 0.0
    plant_fluxes(:)%dcex = 0.0
    plant_fluxes(:)%dnup_pas = 0.0
    plant_fluxes(:)%dnup_act = 0.0
    plant_fluxes(:)%dnup_fix = 0.0
    plant_fluxes(:)%dnup_res = 0.0
    plant_fluxes(:)%vcmax25 = 0.0
    plant_fluxes(:)%jmax25 = 0.0
    plant_fluxes(:)%vcmax = 0.0
    plant_fluxes(:)%jmax = 0.0
    plant_fluxes(:)%gs_accl = 0.0
    plant_fluxes(:)%chi = 0.0
    plant_fluxes(:)%iwue = 0.0
    plant_fluxes(:)%asat = 0.0
    plant_fluxes(:)%lue = 0.0
    plant_fluxes(:)%vcmax25_unitfapar = 0.0

    do pft=1,npft
      call orginit( plant_fluxes(pft)%dharv )
      ! call orginit( plant_fluxes(pft)%alloc_leaf )
      ! call orginit( plant_fluxes(pft)%alloc_root )
      ! call orginit( plant_fluxes(pft)%alloc_sapw )
      ! call orginit( plant_fluxes(pft)%alloc_wood )
      call cinit(   plant_fluxes(pft)%dnpp )
      call ninit(   plant_fluxes(pft)%dnup )
      call orginit( plant_fluxes(pft)%npp_leaf )
      call orginit( plant_fluxes(pft)%npp_root )
      call orginit( plant_fluxes(pft)%npp_wood )
      call orginit( plant_fluxes(pft)%npp_seed )
    end do

  end subroutine init_plant_fluxes


  function ftemp( temp, method, ref_temp )
    !////////////////////////////////////////////////////////////////
    ! Generic temperature response function
    !----------------------------------------------------------------
    ! arguments
    real, intent(in)             :: temp ! temperature [in Degrees Celsius]
    character(len=*), intent(in) :: method
    real, intent(in), optional   :: ref_temp

    ! local variables
    real                         :: ref_temp_local  ! local copy of ref_temp

    ! for lloyd and taylor method
    real, parameter :: E0 = 308.56      ! Activation Energy [J]
    real, parameter :: T0 = 227.13      ! calibration temperature [K]
    real, parameter :: Tzero = 273.15   ! 0°C = 273.15 K 

    ! function return variable
    real :: ftemp

    ! set default reference temperature to 10 deg C
    if (present(ref_temp)) then
     ref_temp_local = ref_temp
    else
     ref_temp_local = 10.0
    endif

    select case (method)

      case ("lloyd_and_taylor")
        !----------------------------------------------------------------
        ! LLOYD AND TAYLOR
        ! Temperature response function is a modified Q10 relationship
        ! (Lloyd & Taylor 1994)
        !----------------------------------------------------------------
        if (temp >= -40.0) then 
          ! avoid numerical errors
          ftemp = exp(E0 * ((1.0 / (ref_temp_local + Tzero - T0)) - (1.0/(temp + Tzero - T0))))
        else
          ! set temperature response to a constant at value of -40°C
          ftemp = exp(E0 * ((1.0 / (ref_temp_local + Tzero - T0)) - (1.0/(-40.0 + Tzero - T0))))
        end if

      case default

        stop 'FTEMP: select valid method'

    end select

    return

  end function ftemp


  function fmoist( moist, method )
    !////////////////////////////////////////////////////////////////
    ! Generic moisture response function
    !----------------------------------------------------------------
    ! arguments
    real, intent(in)             :: moist ! temperature [in Degrees Celsius]
    character(len=*), intent(in) :: method

    ! function return variable
    real :: fmoist

    select case (method)

      case ("foley")
        !----------------------------------------------------------------
        ! FOLEY
        ! Calculates decomposition rate modifier for a given water fraction
        ! according to Foley 1995
        !----------------------------------------------------------------
        fmoist = 0.25 + 0.75 * moist

      case default

        stop 'FMOIST: select valid method'

    end select

    return

  end function fmoist

end module md_plant_cnmodel