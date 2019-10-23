module md_plant
  !////////////////////////////////////////////////////////////////
  !  Module contains (constrainable) model parameters.
  !  Model parameters adopted here are from LPX C3 grass PFT
  !  Litter and soil turnover parameters are divided by 365 to 
  !  convert from [1/yr] to [1/d].
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  use md_params_core

  implicit none

  private
  public plant_type, plant_fluxes_type, getpar_modl_plant, params_pft_plant, &
    initdaily_plant, maxdoy, initglobal_plant, get_leaftraits

  !----------------------------------------------------------------
  ! Public, module-specific state variables
  !----------------------------------------------------------------
  ! Pools and other variables with year-to-year memory
  !----------------------------------------------------------------
  type plant_type

    ! PFT index that goes along with this instance of 'plant'
    integer :: pftno

    ! canopy
    real :: fpc_grid            ! fractional projective cover
    real :: lai_ind             ! fraction of absorbed photosynthetically active radiation
    real :: fapar_ind           ! fraction of absorbed photosynthetically active radiation
    real :: acrown              ! crown area

    ! leaf traits
    real :: narea               ! total leaf N per unit leaf area (gN m-2)
    real :: narea_metabolic     ! metabolic leaf N per unit leaf area (gN m-2)
    real :: narea_structural    ! structural leaf N per unit leaf area (gN m-2)
    real :: lma                 ! leaf mass per area (gC m-2)
    real :: sla                 ! specific leaf area (m2 gC-1)
    real :: nmass               ! leaf N per unit leaf mass, g N / g-dry mass
    real :: r_cton_leaf         ! leaf C:N ratio [gC/gN] 
    real :: r_ntoc_leaf         ! leaf N:C ratio [gN/gC]

  end type plant_type


  !----------------------------------------------------------------
  ! Fluxes and other variables with no memory
  !----------------------------------------------------------------
  type plant_fluxes_type

    real :: dgpp     ! daily gross primary production [gC/m2/d]           
    real :: drd      ! daily dark respiration [gC/m2/d]
    real :: dtransp  ! daily transpiration [mm]
    real :: dlatenth ! daily latent heat flux [J m-2 d-1]

  end type plant_fluxes_type

  !-----------------------------------------------------------------------
  ! Parameters. Runtime read-in
  !-----------------------------------------------------------------------
  ! NON PFT-DEPENDENT PARAMETERS
  type params_plant_type
    real :: kbeer             ! canopy light extinction coefficient
  end type params_plant_type

  type( params_plant_type ) :: params_plant

  ! PFT-DEPENDENT PARAMETERS
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
  end type params_pft_plant_type

  type( params_pft_plant_type ), dimension(npft) :: params_pft_plant

  ! required for outputting leaf trait variables in other modules
  integer, dimension(npft) :: maxdoy  ! DOY of maximum LAI

contains

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


  function get_leaf_n_metabolic_canopy( mylai, meanmppfd, nv, myfapar ) result( mynleaf_metabolic )
    !////////////////////////////////////////////////////////////////
    ! Calculates metabolic leaf N at canopy-level, determined by 
    ! light conditions (meanmppfd) and the Rubisco-N per unit absorbed
    ! light.
    !----------------------------------------------------------------
    use md_params_core, only: nmonth

    ! arguments
    real, intent(in)                    :: mylai
    real, dimension(nmonth), intent(in) :: meanmppfd
    real, dimension(nmonth), intent(in) :: nv
    real, intent(in), optional          :: myfapar

    ! function return variable
    real :: mynleaf_metabolic  ! mol N m-2-ground

    ! local variables
    real :: maxnv

    ! Metabolic N is predicted and is optimised at a monthly time scale. 
    ! Leaf traits are calculated based on metabolic N => cellwall N => cellwall C / LMA
    ! Leaves get thinner at the bottom of the canopy => increasing LAI through the season comes at a declining C and N cost
    ! Monthly variations in metabolic N, determined by variations in meanmppfd and nv should not result in variations in leaf traits. 
    ! In order to prevent this, assume annual maximum metabolic N, part of which is deactivated during months with lower insolation (and Rd reduced.)
    maxnv = maxval( meanmppfd(:) * nv(:) )

    if (present(myfapar)) then
      mynleaf_metabolic = maxnv * myfapar
    else
      mynleaf_metabolic = maxnv * get_fapar( mylai )
    end if

  end function get_leaf_n_metabolic_canopy


  subroutine get_leaftraits( plant, meanmppfd, nv )
    !////////////////////////////////////////////////////////////////
    ! Calculates leaf traits based on (predicted) metabolic Narea and
    ! (prescribed) parameters that relate structural to metabolic
    ! Narea and Carea to structural Narea:
    ! Narea_metabolic  = predicted
    ! Narea_structural = rN:C_struct * LMA
    !----------------------------------------------------------------
    use md_params_core, only: c_content_of_biomass, nmonth, n_molmass, c_molmass

    ! arguments
    type( plant_type ), intent(inout)   :: plant
    real, dimension(nmonth), intent(in) :: meanmppfd
    real, dimension(nmonth), intent(in) :: nv

    ! local variables
    real :: narea_metabolic_canopy   ! g N m-2-ground

    ! canopy-level, in units of gN / m2-ground 
    narea_metabolic_canopy  = n_molmass * get_leaf_n_metabolic_canopy(  -9999.9, meanmppfd(:), nv(:), plant%fapar_ind )

    ! leaf-level, in units of gN / m2-leaf 
    ! assume narea_metabolic is representative of the outer canopy, therefore divide by 1.0 (or just leave)
    plant%narea_metabolic  = narea_metabolic_canopy / 1.0
    plant%narea_structural = params_pft_plant(plant%pftno)%r_ntolma * params_pft_plant(plant%pftno)%lma
    plant%narea            = plant%narea_metabolic + plant%narea_structural
    plant%lma              = params_pft_plant(plant%pftno)%lma

    ! additional traits
    plant%nmass            = plant%narea / ( plant%lma / c_content_of_biomass )
    plant%r_cton_leaf      = params_pft_plant(plant%pftno)%lma / plant%narea
    plant%r_ntoc_leaf      = 1.0 / plant%r_cton_leaf

  end subroutine get_leaftraits


  subroutine getpar_modl_plant()
    !////////////////////////////////////////////////////////////////
    !  Subroutine reads model parameters from input file.
    !  It was necessary to separate this SR from module md_plant
    !  because this SR uses module md_waterbal, which also uses
    !  _plant.
    ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
    ! contact: b.stocker@imperial.ac.uk
    !----------------------------------------------------------------    
    use md_sofunutils, only: getparreal
    use md_interface

    ! local variables
    integer :: pft
    integer :: npft_site

    !----------------------------------------------------------------
    ! NON-PFT DEPENDENT PARAMETERS
    ! Hard-coded ones
    !----------------------------------------------------------------
    ! canopy light extinction coefficient for Beer's Law
    params_plant%kbeer = 0.5

    !----------------------------------------------------------------
    ! PFT DEPENDENT PARAMETERS
    ! read parameter input file and store values in single array
    ! important: Keep this order of reading PFT parameters fixed.
    !----------------------------------------------------------------
    pft = 0
    if ( myinterface%params_siml%ltre ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'tre' )
    end if

    if ( myinterface%params_siml%ltne ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'tne' )
    end if

    if ( myinterface%params_siml%ltrd ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'trd' )
    end if

    if ( myinterface%params_siml%ltnd ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'tnd' )
    end if

    if ( myinterface%params_siml%lgr3 ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'gr3' )
    end if

    if ( myinterface%params_siml%lgn3 ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'gn3' )
    end if

    if ( myinterface%params_siml%lgr4 ) then
      pft = pft + 1
      params_pft_plant(pft) = getpftparams( 'gr4' )
    end if

    npft_site = pft
    if (npft_site==0) stop 'PLANT:GETPAR_MODL_PLANT: PFT name not valid. See run/<simulationname>.sofun.parameter'

  end subroutine getpar_modl_plant


  function getpftparams( pftname ) result( out_getpftparams )
    !----------------------------------------------------------------
    ! Read PFT parameters from respective file, given the PFT name
    !----------------------------------------------------------------
    use md_sofunutils, only: getparreal
    use md_params_core, only: lunat

    ! arguments
    character(len=*), intent(in) :: pftname

    ! local variables
    integer :: lu_category_prov    ! land use category associated with PFT (provisional)
    ! real :: code_growthform
    ! real :: code_nfixer

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
      out_getpftparams%lma     = 50.0
      out_getpftparams%sla     = 1.0 / out_getpftparams%lma
      out_getpftparams%r_ntolma= 35.0000
      lu_category_prov         = 1
    else if (trim(pftname)=='gn3') then
      out_getpftparams%grass   = .true.
      out_getpftparams%tree    = .false.
      out_getpftparams%c3      = .true.
      out_getpftparams%c4      = .false.
      out_getpftparams%nfixer  = .true.
      out_getpftparams%lma     = 50.0
      out_getpftparams%sla     = 1.0 / out_getpftparams%lma
      out_getpftparams%r_ntolma= 35.0000
      lu_category_prov         = 1
    else if (trim(pftname)=='gr4') then
      out_getpftparams%grass   = .true.
      out_getpftparams%tree    = .false.
      out_getpftparams%c3      = .false.
      out_getpftparams%c4      = .true.
      out_getpftparams%nfixer  = .false.
      out_getpftparams%lma     = 50.0
      out_getpftparams%sla     = 1.0 / out_getpftparams%lma
      out_getpftparams%r_ntolma= 35.0000
      lu_category_prov         = 1
    else if (trim(pftname)=='tre') then
      out_getpftparams%grass   = .false.
      out_getpftparams%tree    = .true.
      out_getpftparams%c3      = .true.
      out_getpftparams%c4      = .false.
      out_getpftparams%nfixer  = .false.
      out_getpftparams%lma     = 50.0
      out_getpftparams%sla     = 1.0 / out_getpftparams%lma
      out_getpftparams%r_ntolma= 35.0000
      lu_category_prov         = 1
    else if (trim(pftname)=='tne') then
      out_getpftparams%grass   = .false.
      out_getpftparams%tree    = .true.
      out_getpftparams%c3      = .true.
      out_getpftparams%c4      = .false.
      out_getpftparams%nfixer  = .true.
      out_getpftparams%lma     = 50.0
      out_getpftparams%sla     = 1.0 / out_getpftparams%lma
      out_getpftparams%r_ntolma= 35.0000
      lu_category_prov         = 1
    else if (trim(pftname)=='tnd') then
      out_getpftparams%grass   = .false.
      out_getpftparams%tree    = .true.
      out_getpftparams%c3      = .true.
      out_getpftparams%c4      = .false.
      out_getpftparams%nfixer  = .true.
      out_getpftparams%lma     = 50.0
      out_getpftparams%sla     = 1.0 / out_getpftparams%lma
      out_getpftparams%r_ntolma= 35.0000
      lu_category_prov         = 1
    end if      

    ! land use category associated with PFT (provisional) 
    if (lu_category_prov==1) then
      out_getpftparams%lu_category = lunat
      out_getpftparams%islu(lunat) = .true.
    else
      out_getpftparams%islu(lunat) = .false.
    end if

  end function getpftparams


  subroutine initglobal_plant( plant, ngridcells )
    !////////////////////////////////////////////////////////////////
    !  Initialisation of all _pools on all gridcells at the beginning
    !  of the simulation.
    !  June 2014
    !  b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    use md_params_core, only: npft

    ! argument
    type( plant_type ), dimension(npft,ngridcells), intent(inout) :: plant
    integer, intent(in) :: ngridcells

    ! local variables
    integer :: pft
    integer :: jpngr

    !-----------------------------------------------------------------------------
    ! derive which PFTs are present from fpc_grid (which is prescribed)
    !-----------------------------------------------------------------------------
    do jpngr=1,ngridcells
      do pft=1,npft
        call initpft( plant(pft,jpngr) )
        plant(pft,jpngr)%pftno = pft
      end do
    end do

  end subroutine initglobal_plant


  subroutine initpft( plant )
    !////////////////////////////////////////////////////////////////
    !  Initialisation of specified PFT on specified gridcell
    !  June 2014
    !  b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    ! argument
    type( plant_type ), intent(inout) :: plant

    plant%fpc_grid  = 0.0
    plant%lai_ind   = 0.0
    plant%fapar_ind = 0.0
    plant%acrown    = 0.0

    ! canpopy state variables
    plant%narea            = 0.0
    plant%narea_metabolic  = 0.0
    plant%narea_structural = 0.0
    plant%lma              = 0.0
    plant%sla              = 0.0
    plant%nmass            = 0.0
    plant%r_cton_leaf      = 0.0
    plant%r_ntoc_leaf      = 0.0

  end subroutine initpft


  subroutine initdaily_plant( plant_fluxes )

    !////////////////////////////////////////////////////////////////
    ! Initialises all daily variables with zero.
    !----------------------------------------------------------------
    ! arguments
    type( plant_fluxes_type ), dimension(npft), intent(inout) :: plant_fluxes

    plant_fluxes(:)%dgpp    = 0.0
    plant_fluxes(:)%drd     = 0.0
    plant_fluxes(:)%dtransp = 0.0

  end subroutine initdaily_plant


end module md_plant
