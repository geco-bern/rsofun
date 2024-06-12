program main
  !////////////////////////////////////////////////////////////////
  ! Main program for SOFUN, used for single-site simulations 
  ! Receives simulation parameters, site parameters, and the full 
  ! simulation's forcing as time series
  ! test xxx
  !----------------------------------------------------------------
  use md_params_siml_biomee, only: getsteering
  use md_forcing_biomee, only: getclimate, &
    getco2, &
    climate_type, forcingData
  use md_interface_biomee, only: interfacetype_biosphere, &
    myinterface, &
    outtype_biosphere
    !outtype_daily_tile, &
    !outtype_daily_cohorts, &
    !outtype_annual_tile, &
    !outtype_annual_cohorts
  use md_params_core
  use md_biosphere_biomee, only: biosphere_annual
  use datatypes

  implicit none

  ! Simulation parameters
  logical :: spinup
  integer :: spinupyears
  integer :: recycle
  integer :: firstyeartrend
  integer :: nyeartrend
  integer :: runyears

  !logical :: outputhourly
  !logical :: outputdaily
  !logical :: do_U_shaped_mortality
  !logical :: update_annualLAImax
  !logical :: do_closedN_run
  !logical :: do_reset_veg
  !integer :: dist_frequency
  !integer :: code_method_photosynth
  !integer :: code_method_mortality

  ! site information
  real :: longitude
  real :: latitude
  real :: altitude
  
  integer :: idx
  integer :: idx_hourly_start
  integer :: idx_hourly_end
  integer :: idx_daily_start
  integer :: idx_daily_end

  ! Tile parameters
  !integer :: soiltype
  !real :: FLDCAP
  !real :: WILTPT
  !real :: K1
  !real :: K2
  !real :: K_nitrogen
  !real :: MLmixRatio
  !real :: etaN
  !real :: LMAmin
  !real :: fsc_fine
  !real :: fsc_wood
  !real :: GR_factor
  !real :: l_fract
  !real :: retransN
  !real :: f_initialBSW
  !real :: f_N_add
  !real :: tf_base
  !real :: par_mort
  !real :: par_mort_under

  ! naked arrays
  real, dimension(0:MSPECIES,55)      :: params_species
  real, dimension(n_dim_soil_types,8) :: params_soil
  real, dimension(MAX_INIT_COHORTS,9) :: init_cohort

  ! initial soil pool size
  !real :: init_fast_soil_C
  !real :: init_slow_soil_C
  !real :: init_Nmineral
  !real :: N_input

  integer :: nt
  integer :: nt_daily
  integer :: nt_annual
  integer :: nt_annual_cohorts

  !----------------------------------------------------------------
  ! LOCAL VARIABLES READ FROM/TO FILE/OUTPUT
  !----------------------------------------------------------------
  ! local variables
  integer :: datalines
  integer :: yr_data   ! Years of the forcing data
  integer :: totyears
  integer :: totdays
  integer :: days_data ! days of the forcing data
  real    :: timestep, timestep_d  ! hour, Time step of forcing data, usually hourly (1.0)
  integer :: ntstepsyear_forcing                 ! 365*48 when half-hourly inputs, 365*24 when hourly inputs
  type(outtype_biosphere) :: out_biosphere  ! holds all the output used for calculating the cost or maximum likelihood function 
  integer :: yr
  logical, parameter :: verbose = .false.
  integer :: iday

  character(len=100) :: namelistfile = '/home/laura/rsofun/inst/extdata/parameters_Allocation.nml'

  ! output arrays (naked) to be passed back to C/R
  real, dimension(:,:), allocatable  :: out_hourly_tile 
  real, dimension(:,:), allocatable  :: out_daily_tile       !fno4
  real, dimension(:,:,:), allocatable:: out_daily_cohorts    !fno3
  real, dimension(:,:), allocatable  :: out_annual_tile      !fno5
  real, dimension(:,:,:), allocatable:: out_annual_cohorts   !fno2

  ! whether fast time step processes are simulated. If .false., then C, N, and W balance is simulated daily.
  logical, parameter :: daily = .true.

! xxxx !!!!!!!!!!!!!!!!!!!
  ! input and output arrays (naked) to be passed back to C/R
  !real(kind=c_double), dimension(nt,13), intent(in) :: forcing

  ! real(kind=c_double), dimension(nt,nvars_hourly_tile), intent(out) :: output_hourly_tile ! nvars_hourly_tile = 15
  !real(kind=c_double), dimension(nt_daily,nvars_daily_tile), intent(out) :: output_daily_tile ! nvars_daily_tile = 35    

  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_year
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_doy
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_hour
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_cID
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_PFT
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_layer
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_density
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_f_layer
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_LAI
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_gpp
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_resp
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_transp
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_NPPleaf
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_NPProot
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_NPPwood
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_NSC
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_seedC
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_leafC
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_rootC
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_SW_C
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_HW_C
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_NSN
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_seedN
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_leafN
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_rootN
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_SW_N
  ! real(kind=c_double), dimension(nt_daily,out_max_cohorts), intent(out) :: output_daily_cohorts_HW_N

  !real(kind=c_double), dimension(nt_annual,nvars_annual_tile), intent(out) :: output_annual_tile ! nvars_annual_tile = 51
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_year
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_cID
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_PFT
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_layer
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_density
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_flayer
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_DBH
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_dDBH
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_height
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_age
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_BA
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_dBA
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_Acrown
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_Aleaf
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_nsc
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_nsn
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_seedC
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_leafC
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_rootC
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_sapwC
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_woodC
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_treeG
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_fseed
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_fleaf
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_froot
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_fwood
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_GPP
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_NPP
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_Rauto
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_Nupt
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_Nfix
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_n_deadtrees
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_c_deadtrees
  !real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_deathrate

  ! local variables
  ! type(outtype_biosphere) :: out_biosphere  ! holds all the output used for calculating the cost or maximum likelihood function 
  !type(outtype_daily_tile),     dimension(ndayyear)                 :: out_biosphere_daily_tile
  ! type(outtype_daily_cohorts),  dimension(ndayyear,out_max_cohorts) :: out_biosphere_daily_cohorts
  !type(outtype_annual_tile)                                         :: out_biosphere_annual_tile
  !type(outtype_annual_cohorts), dimension(out_max_cohorts)          :: out_biosphere_annual_cohorts

  !----------------------------------------------------------------
  ! DECLARATIONS TO READ FROM NAMELIST FILE
  !----------------------------------------------------------------
  integer :: io           ! i/o status for the namelist
  integer :: ierr         ! error code, returned by i/o routines
  integer :: i
  integer :: nml_unit
  integer :: j

  namelist /vegn_parameters_nml/  &
  soiltype        ,&
  FLDCAP          ,&
  WILTPT          ,&
  K1              ,&
  K2              ,&
  K_nitrogen      ,&
  MLmixRatio      ,&
  etaN            ,&
  LMAmin          ,&
  fsc_fine        ,&
  fsc_wood        ,&
  GR_factor       ,&
  l_fract         ,&
  retransN        ,&
  f_initialBSW    ,&
  f_N_add         ,& 
  tf_base         ,&
  par_mort        ,&
  par_mort_under  ,&
  lifeform        ,&
  phenotype       ,&
  pt              ,&
  alpha_FR        ,&
  rho_FR          ,&
  root_r          ,&
  root_zeta       ,&
  Kw_root         ,&
  leaf_size       ,&
  Vmax            ,&
  Vannual         ,&
  wet_leaf_dreg   ,&
  m_cond          ,&
  alpha_phot      ,&
  gamma_L         ,&
  gamma_LN        ,&
  gamma_SW        ,&
  gamma_FR        ,&
  tc_crit         ,&
  tc_crit_on      ,&
  gdd_crit        ,&
  betaON          ,&
  betaOFF         ,&
  alphaHT         ,&       
  thetaHT         ,&        
  alphaCA         ,&        
  thetaCA         ,&        
  alphaBM         ,&         
  thetaBM         ,&
  seedlingsize    ,&
  maturalage      ,&
  v_seed          ,&
  mortrate_d_c    ,&     
  mortrate_d_u    ,&
  LMA             ,&
  leafLS          ,&
  LNbase          ,&
  CNleafsupport   ,&
  rho_wood        ,&
  taperfactor     ,&
  lAImax          ,&
  tauNSC          ,&
  fNSNmax         ,&     
  phiCSA          ,&
  CNleaf0         ,&
  CNsw0           ,&
  CNwood0         ,&
  CNroot0         ,&
  CNseed0         ,&
  Nfixrate0       ,&
  NfixCost0       ,&
  internal_gap_frac,&     
  kphio           ,&
  phiRL           ,&
  LAI_light     

  namelist /soil_data_nml/ &
  GMD, &
  GSD, &
  vwc_sat,&
  chb, &
  psi_sat_ref, &
  k_sat_ref, &
  alphaSoil, &
  heat_capacity_dry

  namelist /initial_state_nml/ &
  init_n_cohorts,      &
  init_cohort_species, &
  init_cohort_nindivs, &
  init_cohort_bl,      &
  init_cohort_br, &
  init_cohort_bsw, &
  init_cohort_bHW, &
  init_cohort_seedC, &
  init_cohort_nsc, &
  init_fast_soil_C, &
  init_slow_soil_C,    & 
  init_Nmineral, &
  N_input,  &
  filepath_in, &
  climfile, &
  sitename, &
  longitude, &
  latitude, &
  altitude, &
  year_start, &
  year_end, &
  classid,&
  c4, &
  whc, &
  koeppen_code, &
  igbp_land_use, &
  plant_functional_type, &
  spinup, &
  spinupyears, &
  recycle, &
  firstyeartrend, &
  nyeartrend, &
  outputhourly, &
  outputdaily, &
  do_U_shaped_mortality, &
  update_annualLAImax, &
  do_closedN_run, &
  do_reset_veg,  &
  dist_frequency,  &
  method_photosynth,  &
  method_mortality

  !----------------------------------------------------------------
  ! READ FROM NAMELIST FILE
  !----------------------------------------------------------------
  ! Vegetation parameters: tile and species
  if(read_from_parameter_file)then
    nml_unit = 999
    open(nml_unit, file=trim(namelistfile), form='formatted', action='read', status='old')
    read (nml_unit, nml=vegn_parameters_nml, iostat=io, end=10)
  10    close (nml_unit)
  endif
  write(*,nml=vegn_parameters_nml)

  ! Soil parameters
  if(read_from_parameter_file)then
    nml_unit = 999
    open(nml_unit, file=trim(namelistfile), form='formatted', action='read', status='old')
    read (nml_unit, nml=soil_data_nml, iostat=io, end=20)
  20   close (nml_unit)
    write (*, nml=soil_data_nml)
  endif

  ! Initial soil and cohort states
  ! xxx todo: take info from myinterface instead of from namelist
  ! --- Generate cohorts according to "initial_state_nml" ---
  nml_unit = 999
  open(nml_unit, file=trim(namelistfile), form='formatted', action='read', status='old')
  read (nml_unit, nml=initial_state_nml, iostat=io, end=30)
  30    close (nml_unit)
  write(*,nml=initial_state_nml)

  !----------------------------------------------------------------
  ! POPULATE MYINTERFACE WITH ARGUMENTS FROM R
  !----------------------------------------------------------------
  myinterface%params_siml%do_spinup        = spinup
  myinterface%params_siml%spinupyears      = spinupyears
  myinterface%params_siml%recycle          = recycle
  myinterface%params_siml%firstyeartrend   = firstyeartrend
  myinterface%params_siml%nyeartrend       = nyeartrend

  if (myinterface%params_siml%do_spinup) then
    myinterface%params_siml%runyears = myinterface%params_siml%nyeartrend + myinterface%params_siml%spinupyears
  else
    myinterface%params_siml%runyears = myinterface%params_siml%nyeartrend
    myinterface%params_siml%spinupyears = 0
  endif

  ! Simulation parameters
  myinterface%params_siml%outputhourly          = outputhourly
  myinterface%params_siml%outputdaily           = outputdaily
  myinterface%params_siml%do_U_shaped_mortality = do_U_shaped_mortality
  myinterface%params_siml%update_annualLAImax   = update_annualLAImax      
  myinterface%params_siml%do_closedN_run        = do_closedN_run 
  myinterface%params_siml%do_reset_veg          = do_reset_veg  
  myinterface%params_siml%dist_frequency        = dist_frequency           
  
  myinterface%grid%lon = longitude
  myinterface%grid%lat = latitude
  myinterface%grid%elv = altitude

  ! Tile parameters
  myinterface%params_tile%soiltype        = soiltype 
  myinterface%params_tile%FLDCAP          = FLDCAP 
  myinterface%params_tile%WILTPT          = WILTPT 
  myinterface%params_tile%K1              = K1 
  myinterface%params_tile%K2              = K2 
  myinterface%params_tile%K_nitrogen      = K_nitrogen 
  myinterface%params_tile%MLmixRatio      = MLmixRatio 
  myinterface%params_tile%etaN            = etaN 
  myinterface%params_tile%LMAmin          = LMAmin 
  myinterface%params_tile%fsc_fine        = fsc_fine 
  myinterface%params_tile%fsc_wood        = fsc_wood 
  myinterface%params_tile%GR_factor       = GR_factor 
  myinterface%params_tile%l_fract         = l_fract 
  myinterface%params_tile%retransN        = retransN 
  myinterface%params_tile%f_initialBSW    = f_initialBSW 
  myinterface%params_tile%f_N_add         = f_N_add
  myinterface%params_tile%tf_base         = tf_base 
  myinterface%params_tile%par_mort        = par_mort 
  myinterface%params_tile%par_mort_under  = par_mort_under 

  ! Species parameters
  myinterface%params_species(:)%lifeform          = lifeform
  myinterface%params_species(:)%phenotype         = phenotype
  myinterface%params_species(:)%pt                = pt
  myinterface%params_species(:)%alpha_FR          = alpha_FR
  myinterface%params_species(:)%rho_FR            = rho_FR
  myinterface%params_species(:)%root_r            = root_r
  myinterface%params_species(:)%root_zeta         = root_zeta
  myinterface%params_species(:)%Kw_root           = Kw_root
  myinterface%params_species(:)%leaf_size         = leaf_size
  myinterface%params_species(:)%Vmax              = Vmax
  myinterface%params_species(:)%Vannual           = Vannual
  myinterface%params_species(:)%wet_leaf_dreg     = wet_leaf_dreg
  myinterface%params_species(:)%m_cond            = m_cond
  myinterface%params_species(:)%alpha_phot        = alpha_phot
  myinterface%params_species(:)%gamma_L           = gamma_L
  myinterface%params_species(:)%gamma_LN          = gamma_LN
  myinterface%params_species(:)%gamma_SW          = gamma_SW
  myinterface%params_species(:)%gamma_FR          = gamma_FR
  myinterface%params_species(:)%tc_crit           = tc_crit
  myinterface%params_species(:)%tc_crit_on        = tc_crit_on
  myinterface%params_species(:)%gdd_crit          = gdd_crit
  myinterface%params_species(:)%betaON            = betaON
  myinterface%params_species(:)%betaOFF           = betaOFF
  myinterface%params_species(:)%alphaHT           = alphaHT
  myinterface%params_species(:)%thetaHT           = thetaHT
  myinterface%params_species(:)%alphaCA           = alphaCA
  myinterface%params_species(:)%thetaCA           = thetaCA
  myinterface%params_species(:)%alphaBM           = alphaBM
  myinterface%params_species(:)%thetaBM           = thetaBM
  myinterface%params_species(:)%seedlingsize      = seedlingsize
  myinterface%params_species(:)%maturalage        = maturalage
  myinterface%params_species(:)%v_seed            = v_seed
  myinterface%params_species(:)%mortrate_d_c      = mortrate_d_c
  myinterface%params_species(:)%mortrate_d_u      = mortrate_d_u
  myinterface%params_species(:)%LMA               = LMA
  myinterface%params_species(:)%leafLS            = leafLS
  myinterface%params_species(:)%LNbase            = LNbase
  myinterface%params_species(:)%CNleafsupport     = CNleafsupport
  myinterface%params_species(:)%rho_wood          = rho_wood
  myinterface%params_species(:)%taperfactor       = taperfactor
  myinterface%params_species(:)%lAImax            = lAImax
  myinterface%params_species(:)%tauNSC            = tauNSC
  myinterface%params_species(:)%fNSNmax           = fNSNmax
  myinterface%params_species(:)%phiCSA            = phiCSA
  myinterface%params_species(:)%CNleaf0           = CNleaf0
  myinterface%params_species(:)%CNsw0             = CNsw0
  myinterface%params_species(:)%CNwood0           = CNwood0
  myinterface%params_species(:)%CNroot0           = CNroot0
  myinterface%params_species(:)%CNseed0           = CNseed0
  myinterface%params_species(:)%Nfixrate0         = Nfixrate0
  myinterface%params_species(:)%NfixCost0         = NfixCost0
  myinterface%params_species(:)%internal_gap_frac = internal_gap_frac
  myinterface%params_species(:)%kphio             = kphio
  myinterface%params_species(:)%phiRL             = phiRL
  myinterface%params_species(:)%LAI_light         = LAI_light

  ! Initial cohort sizes
  myinterface%init_cohort(:)%init_n_cohorts      = init_n_cohorts
  myinterface%init_cohort(:)%init_cohort_species = init_cohort_species
  myinterface%init_cohort(:)%init_cohort_nindivs = init_cohort_nindivs
  myinterface%init_cohort(:)%init_cohort_bl      = init_cohort_bl
  myinterface%init_cohort(:)%init_cohort_br      = init_cohort_br
  myinterface%init_cohort(:)%init_cohort_bsw     = init_cohort_bsw
  myinterface%init_cohort(:)%init_cohort_bHW     = init_cohort_bHW
  myinterface%init_cohort(:)%init_cohort_seedC   = init_cohort_seedC
  myinterface%init_cohort(:)%init_cohort_nsc     = init_cohort_nsc

  ! Initial soil pools
  myinterface%init_soil%init_fast_soil_C = init_fast_soil_C 
  myinterface%init_soil%init_slow_soil_C = init_slow_soil_C 
  myinterface%init_soil%init_Nmineral    = init_Nmineral 
  myinterface%init_soil%N_input          = N_input 

  !----------------------------------------------------------------
  ! GET SOIL PARAMETERS
  !----------------------------------------------------------------
  !myinterface%params_soil = getsoil( params_soil )

  myinterface%params_soil%GMD               = GMD
  myinterface%params_soil%GSD               = GSD
  myinterface%params_soil%vwc_sat           = vwc_sat
  myinterface%params_soil%chb               = chb
  myinterface%params_soil%psi_sat_ref       = psi_sat_ref
  myinterface%params_soil%k_sat_ref         = k_sat_ref
  myinterface%params_soil%alphaSoil         = alphaSoil
  myinterface%params_soil%heat_capacity_dry = heat_capacity_dry

  !----------------------------------------------------------------
  ! READ FORCING FILE
  !----------------------------------------------------------------
  call read_FACEforcing( forcingData, datalines, days_data, yr_data, timestep ) !! ORNL

  print*,'runyears  ', myinterface%params_siml%runyears
  print*,'params_tile%soiltype  ', myinterface%params_tile%soiltype
  print*,'params_species%LMA  ', myinterface%params_species%LMA
  print*,'spinupyears  ', myinterface%params_siml%spinupyears

  !----------------------------------------------------------------
  ! INTERPRET FORCING
  !----------------------------------------------------------------
  !timestep   = real(forcing(2,3)) - real(forcing(1,3))  ! This takes the hour of day (a numeric) from the forcing file
  !timestep_d = real(forcing(2,2)) - real(forcing(1,2))  ! This takes the day of year (a numeric) from the forcing file
  
  !if (abs(timestep) < eps .and. abs(timestep_d - 1.0) < eps) then
    ! forcing is daily
    !timestep = 24.0
  !end if
  if (daily) timestep = 24.0
  myinterface%steps_per_day = int(24.0/timestep)
  myinterface%dt_fast_yr = 1.0/(365.0 * myinterface%steps_per_day)
  myinterface%step_seconds = 24.0*3600.0/myinterface%steps_per_day ! seconds_per_year * dt_fast_yr
  ntstepsyear = myinterface%steps_per_day * 365

  allocate(myinterface%climate(ntstepsyear))
  allocate(myinterface%pco2(ntstepsyear))
  !allocate(out_biosphere%hourly_tile(ntstepsyear))

  allocate(out_hourly_tile(     ntstepsyear * myinterface%params_siml%nyeartrend,  nvars_hourly_tile                        ))
  allocate(out_daily_cohorts(   ndayyear * myinterface%params_siml%nyeartrend,     out_max_cohorts,    nvars_daily_cohorts  ))
  allocate(out_daily_tile(      ndayyear * myinterface%params_siml%nyeartrend,     nvars_daily_tile                         ))
  allocate(out_annual_cohorts(  myinterface%params_siml%runyears,                  out_max_cohorts,    nvars_annual_cohorts ))
  allocate(out_annual_tile(     myinterface%params_siml%runyears,                  nvars_annual_tile                        ))
  
  yearloop: do yr=1, myinterface%params_siml%runyears
    !----------------------------------------------------------------
    ! Define simulations "steering" variables (forcingyear, etc.)
    !----------------------------------------------------------------
    myinterface%steering = getsteering( yr, myinterface%params_siml )

    !----------------------------------------------------------------
    ! Get external (environmental) forcing (for biomee, co2 is in myinterface%climate)
    !----------------------------------------------------------------
    ! Get climate variables for this year (full fields and 365 daily values for each variable)
    myinterface%climate(:) = getclimate( &
                                          nt, &
                                          ntstepsyear, &
                                          forcingData, &
                                          myinterface%steering%climateyear_idx &
                                          !myinterface%steering%climateyear &
                                          )

    !----------------------------------------------------------------
    ! Call biosphere (wrapper for all modules, contains time loops)
    !----------------------------------------------------------------
    call biosphere_annual( &
       out_biosphere &
      !out_biosphere_daily_tile, &
      ! out_biosphere_daily_cohorts, &
      !out_biosphere_annual_tile, &
      !out_biosphere_annual_cohorts &
      )

    !----------------------------------------------------------------
    ! Populate big output arrays
    !----------------------------------------------------------------

    !----------------------------------------------------------------
    ! Output out_hourly_tile (calling subroutine)
    !----------------------------------------------------------------
     if (.not. myinterface%steering%spinup) then  
       idx_hourly_start = (yr - myinterface%params_siml%spinupyears - 1) * ntstepsyear + 1    ! To exclude the spinup years and include only the transient years
       idx_hourly_end   = idx_hourly_start + ntstepsyear - 1
    !   ! call populate_outarray_hourly_tile( out_biosphere_hourly_tile(:), output_hourly_tile(idx_hourly_start:idx_hourly_end,:)) !xxx commented out for calibration!
        call populate_outarray_hourly_tile( out_biosphere%hourly_tile(:), out_hourly_tile(idx_hourly_start:idx_hourly_end, :) )
     end if

    !----------------------------------------------------------------
    ! Output out_daily_tile (calling subroutine)
    !----------------------------------------------------------------
    ! Output only for transient years
    if (.not. myinterface%steering%spinup) then  

      idx_daily_start = (yr - myinterface%params_siml%spinupyears - 1) * ndayyear + 1  
      idx_daily_end   = idx_daily_start + ndayyear - 1

      !call populate_outarray_daily_tile( out_biosphere_daily_tile(:), output_daily_tile(idx_daily_start:idx_daily_end,:))
      call populate_outarray_daily_tile( out_biosphere%daily_tile(:), out_daily_tile(idx_daily_start:idx_daily_end, :) )

      !----------------------------------------------------------------
      ! Output out_daily_cohorts (without subroutine)
      !----------------------------------------------------------------
      ! output_daily_cohorts_year(idx_daily_start:idx_daily_end,:)    = dble(out_biosphere_daily_cohorts(:,:)%year)
      ! output_daily_cohorts_doy(idx_daily_start:idx_daily_end,:)     = dble(out_biosphere_daily_cohorts(:,:)%doy)
      ! output_daily_cohorts_hour(idx_daily_start:idx_daily_end,:)    = dble(out_biosphere_daily_cohorts(:,:)%hour)
      ! output_daily_cohorts_cID(idx_daily_start:idx_daily_end,:)     = dble(out_biosphere_daily_cohorts(:,:)%cID)
      ! output_daily_cohorts_PFT(idx_daily_start:idx_daily_end,:)     = dble(out_biosphere_daily_cohorts(:,:)%PFT)
      ! output_daily_cohorts_layer(idx_daily_start:idx_daily_end,:)   = dble(out_biosphere_daily_cohorts(:,:)%layer)
      ! output_daily_cohorts_density(idx_daily_start:idx_daily_end,:) = dble(out_biosphere_daily_cohorts(:,:)%density)
      ! output_daily_cohorts_f_layer(idx_daily_start:idx_daily_end,:) = dble(out_biosphere_daily_cohorts(:,:)%f_layer)
      ! output_daily_cohorts_LAI(idx_daily_start:idx_daily_end,:)     = dble(out_biosphere_daily_cohorts(:,:)%LAI)
      ! output_daily_cohorts_gpp(idx_daily_start:idx_daily_end,:)     = dble(out_biosphere_daily_cohorts(:,:)%gpp)
      ! output_daily_cohorts_resp(idx_daily_start:idx_daily_end,:)    = dble(out_biosphere_daily_cohorts(:,:)%resp)
      ! output_daily_cohorts_transp(idx_daily_start:idx_daily_end,:)  = dble(out_biosphere_daily_cohorts(:,:)%transp)
      ! output_daily_cohorts_NPPleaf(idx_daily_start:idx_daily_end,:) = dble(out_biosphere_daily_cohorts(:,:)%NPPleaf)
      ! output_daily_cohorts_NPProot(idx_daily_start:idx_daily_end,:) = dble(out_biosphere_daily_cohorts(:,:)%NPProot)
      ! output_daily_cohorts_NPPwood(idx_daily_start:idx_daily_end,:) = dble(out_biosphere_daily_cohorts(:,:)%NPPwood)    
      ! output_daily_cohorts_NSC(idx_daily_start:idx_daily_end,:)     = dble(out_biosphere_daily_cohorts(:,:)%NSC)
      ! output_daily_cohorts_seedC(idx_daily_start:idx_daily_end,:)   = dble(out_biosphere_daily_cohorts(:,:)%seedC)
      ! output_daily_cohorts_leafC(idx_daily_start:idx_daily_end,:)   = dble(out_biosphere_daily_cohorts(:,:)%leafC)
      ! output_daily_cohorts_rootC(idx_daily_start:idx_daily_end,:)   = dble(out_biosphere_daily_cohorts(:,:)%rootC)
      ! output_daily_cohorts_SW_C(idx_daily_start:idx_daily_end,:)    = dble(out_biosphere_daily_cohorts(:,:)%SW_C)
      ! output_daily_cohorts_HW_C(idx_daily_start:idx_daily_end,:)    = dble(out_biosphere_daily_cohorts(:,:)%HW_C)
      ! output_daily_cohorts_NSN(idx_daily_start:idx_daily_end,:)     = dble(out_biosphere_daily_cohorts(:,:)%NSN)
      ! output_daily_cohorts_seedN(idx_daily_start:idx_daily_end,:)   = dble(out_biosphere_daily_cohorts(:,:)%seedN)
      ! output_daily_cohorts_leafN(idx_daily_start:idx_daily_end,:)   = dble(out_biosphere_daily_cohorts(:,:)%leafN)
      ! output_daily_cohorts_rootN(idx_daily_start:idx_daily_end,:)   = dble(out_biosphere_daily_cohorts(:,:)%rootN)
      ! output_daily_cohorts_SW_N(idx_daily_start:idx_daily_end,:)    = dble(out_biosphere_daily_cohorts(:,:)%SW_N)
      ! output_daily_cohorts_HW_N(idx_daily_start:idx_daily_end,:)    = dble(out_biosphere_daily_cohorts(:,:)%HW_N)

    end if

    if (.not. myinterface%steering%spinup) then  
      call populate_outarray_daily_cohorts( out_biosphere%daily_cohorts(:,:), out_daily_cohorts(idx_daily_start:idx_daily_end,:,:) )
    end if

    !----------------------------------------------------------------
    ! Output out_annual_tile (calling subroutine)
    !----------------------------------------------------------------
    !call populate_outarray_annual_tile( out_biosphere_annual_tile, output_annual_tile(yr,:) )
    call populate_outarray_annual_tile( out_biosphere%annual_tile, out_annual_tile(yr,:) )

    !----------------------------------------------------------------
    ! Output output_annual_cohorts (without subroutine)
    !----------------------------------------------------------------
    ! To get outputs only after spinupyears make if below and 
    ! also in run_biomee_f_bysite.R make n_annual_cohorts = as.integer(params_siml$nyeartrend)

    call populate_outarray_annual_cohorts( out_biosphere%annual_cohorts(:), out_annual_cohorts(yr,:,:) )

    !if (.not. myinterface%steering%spinup) then  

      !idx =  yr - myinterface%params_siml%spinupyears

      !output_annual_cohorts_year(idx, :)        = dble(out_biosphere_annual_cohorts(:)%year)
      !output_annual_cohorts_cID(idx, :)         = dble(out_biosphere_annual_cohorts(:)%cID)
      !output_annual_cohorts_PFT(idx, :)         = dble(out_biosphere_annual_cohorts(:)%PFT)
      !output_annual_cohorts_layer(idx, :)       = dble(out_biosphere_annual_cohorts(:)%layer)
      !output_annual_cohorts_density(idx, :)     = dble(out_biosphere_annual_cohorts(:)%density)
      !output_annual_cohorts_flayer(idx, :)      = dble(out_biosphere_annual_cohorts(:)%flayer)
      !output_annual_cohorts_dbh(idx, :)         = dble(out_biosphere_annual_cohorts(:)%DBH)
      !output_annual_cohorts_dDBH(idx, :)        = dble(out_biosphere_annual_cohorts(:)%dDBH)
      !output_annual_cohorts_height(idx, :)      = dble(out_biosphere_annual_cohorts(:)%height)
      !output_annual_cohorts_age(idx, :)         = dble(out_biosphere_annual_cohorts(:)%age)
      !output_annual_cohorts_BA(idx, :)          = dble(out_biosphere_annual_cohorts(:)%BA)
      !output_annual_cohorts_dBA(idx, :)         = dble(out_biosphere_annual_cohorts(:)%dBA)
      !output_annual_cohorts_Acrown(idx, :)      = dble(out_biosphere_annual_cohorts(:)%Acrown)
      !output_annual_cohorts_Aleaf(idx, :)       = dble(out_biosphere_annual_cohorts(:)%Aleaf)
      !output_annual_cohorts_nsc(idx, :)         = dble(out_biosphere_annual_cohorts(:)%nsc)
      !output_annual_cohorts_nsn(idx, :)         = dble(out_biosphere_annual_cohorts(:)%nsn)
      !output_annual_cohorts_seedC(idx, :)       = dble(out_biosphere_annual_cohorts(:)%seedC)
      !output_annual_cohorts_leafC(idx, :)       = dble(out_biosphere_annual_cohorts(:)%leafC)
      !output_annual_cohorts_rootC(idx, :)       = dble(out_biosphere_annual_cohorts(:)%rootC)
      !output_annual_cohorts_sapwC(idx, :)       = dble(out_biosphere_annual_cohorts(:)%sapwC)
      !output_annual_cohorts_woodC(idx, :)       = dble(out_biosphere_annual_cohorts(:)%woodC)
      !output_annual_cohorts_treeG(idx, :)       = dble(out_biosphere_annual_cohorts(:)%treeG)
      !output_annual_cohorts_fseed(idx, :)       = dble(out_biosphere_annual_cohorts(:)%fseed)
      !output_annual_cohorts_fleaf(idx, :)       = dble(out_biosphere_annual_cohorts(:)%fleaf)
      !output_annual_cohorts_froot(idx, :)       = dble(out_biosphere_annual_cohorts(:)%froot)
      !output_annual_cohorts_fwood(idx, :)       = dble(out_biosphere_annual_cohorts(:)%fwood)
      !output_annual_cohorts_GPP(idx, :)         = dble(out_biosphere_annual_cohorts(:)%GPP)
      !output_annual_cohorts_NPP(idx, :)         = dble(out_biosphere_annual_cohorts(:)%NPP)
      !output_annual_cohorts_Rauto(idx, :)       = dble(out_biosphere_annual_cohorts(:)%Rauto)
      !output_annual_cohorts_Nupt(idx, :)        = dble(out_biosphere_annual_cohorts(:)%Nupt)
      !output_annual_cohorts_Nfix(idx, :)        = dble(out_biosphere_annual_cohorts(:)%Nfix)
      !output_annual_cohorts_deathrate(idx, :)   = dble(out_biosphere_annual_cohorts(:)%deathrate)
      !output_annual_cohorts_n_deadtrees(idx, :) = dble(out_biosphere_annual_cohorts(:)%n_deadtrees)
      !output_annual_cohorts_c_deadtrees(idx, :) = dble(out_biosphere_annual_cohorts(:)%c_deadtrees)

     !end if

  end do yearloop

  deallocate(myinterface%climate)
  deallocate(myinterface%pco2)
  deallocate(out_hourly_tile)
  deallocate(out_daily_cohorts)
  deallocate(out_daily_tile)
  deallocate(out_annual_cohorts)
  deallocate(out_annual_tile)

  100  format (A,I6,I6,F8.2)
  777  format (F20.8,F20.8)
  999  format (I4.4)

contains

  ! !////////////////////////////////////////////////////////////////
  ! ! Populates hourly tile-level output array passed back to C and R.
  ! !----------------------------------------------------------------
   subroutine populate_outarray_hourly_tile( hourly_tile, out_hourly_tile ) !, idx_daily_start, idx_daily_end

     use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32, in=>int32
     use md_interface_biomee, only: outtype_hourly_tile
     use md_params_core

     ! arguments
     type(outtype_hourly_tile), dimension(ntstepsyear), intent(in) :: hourly_tile    ! dimension(ntstepsyear)
     real, dimension(ntstepsyear, nvars_hourly_tile), intent(inout) :: out_hourly_tile

     out_hourly_tile(:, 1)  = dble(hourly_tile(:)%year)
     out_hourly_tile(:, 2)  = dble(hourly_tile(:)%doy)
     out_hourly_tile(:, 3)  = dble(hourly_tile(:)%hour)
     out_hourly_tile(:, 4)  = dble(hourly_tile(:)%rad)
     out_hourly_tile(:, 5)  = dble(hourly_tile(:)%Tair)
     out_hourly_tile(:, 6)  = dble(hourly_tile(:)%Prcp)
     out_hourly_tile(:, 7)  = dble(hourly_tile(:)%GPP)
     out_hourly_tile(:, 8)  = dble(hourly_tile(:)%Resp)
     out_hourly_tile(:, 9)  = dble(hourly_tile(:)%Transp)
     out_hourly_tile(:, 10) = dble(hourly_tile(:)%Evap)
     out_hourly_tile(:, 11) = dble(hourly_tile(:)%Runoff)
     out_hourly_tile(:, 12) = dble(hourly_tile(:)%Soilwater)
     out_hourly_tile(:, 13) = dble(hourly_tile(:)%wcl)
     out_hourly_tile(:, 14) = dble(hourly_tile(:)%FLDCAP)
     out_hourly_tile(:, 15) = dble(hourly_tile(:)%WILTPT)

   end subroutine populate_outarray_hourly_tile


  !////////////////////////////////////////////////////////////////
  ! Populates daily tile-level output array passed back to C and R.
  !----------------------------------------------------------------
  subroutine populate_outarray_daily_tile( daily_tile, out_daily_tile ) !, idx_daily_start, idx_daily_end

    use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32, in=>int32
    use md_interface_biomee, only: outtype_daily_tile
    use md_params_core

    ! arguments
    type(outtype_daily_tile), dimension(ndayyear), intent(in) :: daily_tile
    real, dimension(ndayyear, nvars_daily_tile), intent(inout) :: out_daily_tile

    out_daily_tile(:, 1)  = dble(daily_tile(:)%year) 
    out_daily_tile(:, 2)  = dble(daily_tile(:)%doy)
    out_daily_tile(:, 3)  = dble(daily_tile(:)%Tc)
    out_daily_tile(:, 4)  = dble(daily_tile(:)%Prcp)
    out_daily_tile(:, 5)  = dble(daily_tile(:)%totWs)
    out_daily_tile(:, 6)  = dble(daily_tile(:)%Trsp)
    out_daily_tile(:, 7)  = dble(daily_tile(:)%Evap)
    out_daily_tile(:, 8)  = dble(daily_tile(:)%Runoff)
    out_daily_tile(:, 9)  = dble(daily_tile(:)%ws1)
    out_daily_tile(:, 10) = dble(daily_tile(:)%ws2)
    out_daily_tile(:, 11) = dble(daily_tile(:)%ws3)
    out_daily_tile(:, 12) = dble(daily_tile(:)%LAI)
    out_daily_tile(:, 13) = dble(daily_tile(:)%GPP)
    out_daily_tile(:, 14) = dble(daily_tile(:)%Rauto)
    out_daily_tile(:, 15) = dble(daily_tile(:)%Rh)
    out_daily_tile(:, 16) = dble(daily_tile(:)%NSC)
    out_daily_tile(:, 17) = dble(daily_tile(:)%seedC)
    out_daily_tile(:, 18) = dble(daily_tile(:)%leafC)
    out_daily_tile(:, 19) = dble(daily_tile(:)%rootC)
    out_daily_tile(:, 20) = dble(daily_tile(:)%SW_C)
    out_daily_tile(:, 21) = dble(daily_tile(:)%HW_C)
    out_daily_tile(:, 22) = dble(daily_tile(:)%NSN)
    out_daily_tile(:, 23) = dble(daily_tile(:)%seedN)
    out_daily_tile(:, 24) = dble(daily_tile(:)%leafN)
    out_daily_tile(:, 25) = dble(daily_tile(:)%rootN)
    out_daily_tile(:, 26) = dble(daily_tile(:)%SW_N)
    out_daily_tile(:, 27) = dble(daily_tile(:)%HW_N)
    out_daily_tile(:, 28) = dble(daily_tile(:)%McrbC)
    out_daily_tile(:, 29) = dble(daily_tile(:)%fastSOM)
    out_daily_tile(:, 30) = dble(daily_tile(:)%slowSOM)
    out_daily_tile(:, 31) = dble(daily_tile(:)%McrbN)
    out_daily_tile(:, 32) = dble(daily_tile(:)%fastSoilN)
    out_daily_tile(:, 33) = dble(daily_tile(:)%slowSoilN)
    out_daily_tile(:, 34) = dble(daily_tile(:)%mineralN)
    out_daily_tile(:, 35) = dble(daily_tile(:)%N_uptk)

  end subroutine populate_outarray_daily_tile

    subroutine populate_outarray_daily_cohorts( daily_cohorts, out_daily_cohorts ) 

    use md_interface_biomee, only: outtype_daily_cohorts

    ! arguments
    type(outtype_daily_cohorts), dimension(ndayyear, out_max_cohorts), intent(in) :: daily_cohorts
    real, dimension(ndayyear, out_max_cohorts,nvars_daily_cohorts), intent(inout) :: out_daily_cohorts
    !real, dimension(:,:,:), allocatable, intent(inout) :: out_daily_cohorts

    out_daily_cohorts(:,:, 1)  = daily_cohorts(:,:)%year
    out_daily_cohorts(:,:, 2)  = daily_cohorts(:,:)%doy
    out_daily_cohorts(:,:, 3)  = daily_cohorts(:,:)%hour
    out_daily_cohorts(:,:, 4)  = daily_cohorts(:,:)%cID
    out_daily_cohorts(:,:, 5)  = daily_cohorts(:,:)%PFT
    out_daily_cohorts(:,:, 6)  = daily_cohorts(:,:)%layer
    out_daily_cohorts(:,:, 7)  = daily_cohorts(:,:)%density
    out_daily_cohorts(:,:, 8)  = daily_cohorts(:,:)%f_layer
    out_daily_cohorts(:,:, 9)  = daily_cohorts(:,:)%LAI
    out_daily_cohorts(:,:, 10) = daily_cohorts(:,:)%gpp
    out_daily_cohorts(:,:, 11) = daily_cohorts(:,:)%resp
    out_daily_cohorts(:,:, 12) = daily_cohorts(:,:)%transp
    out_daily_cohorts(:,:, 13) = daily_cohorts(:,:)%NPPleaf
    out_daily_cohorts(:,:, 14) = daily_cohorts(:,:)%NPProot
    out_daily_cohorts(:,:, 15) = daily_cohorts(:,:)%NPPwood    
    out_daily_cohorts(:,:, 16) = daily_cohorts(:,:)%NSC
    out_daily_cohorts(:,:, 17) = daily_cohorts(:,:)%seedC
    out_daily_cohorts(:,:, 18) = daily_cohorts(:,:)%leafC
    out_daily_cohorts(:,:, 19) = daily_cohorts(:,:)%rootC
    out_daily_cohorts(:,:, 20) = daily_cohorts(:,:)%SW_C
    out_daily_cohorts(:,:, 21) = daily_cohorts(:,:)%HW_C
    out_daily_cohorts(:,:, 22) = daily_cohorts(:,:)%NSN
    out_daily_cohorts(:,:, 23) = daily_cohorts(:,:)%seedN
    out_daily_cohorts(:,:, 24) = daily_cohorts(:,:)%leafN
    out_daily_cohorts(:,:, 25) = daily_cohorts(:,:)%rootN
    out_daily_cohorts(:,:, 26) = daily_cohorts(:,:)%SW_N
    out_daily_cohorts(:,:, 27) = daily_cohorts(:,:)%HW_N

  end subroutine populate_outarray_daily_cohorts


  !////////////////////////////////////////////////////////////////
  ! Populates annual output tile-level array passed back to C and R.
  !----------------------------------------------------------------
  subroutine populate_outarray_annual_tile( annual_tile, out_annual_tile )

    use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32, in=>int32
    use md_interface_biomee, only: outtype_annual_tile
    use md_params_core

    ! arguments
    type(outtype_annual_tile), intent(in) :: annual_tile
    real, dimension(nvars_annual_tile), intent(inout) :: out_annual_tile

    out_annual_tile(1)  = dble(annual_tile%year)
    out_annual_tile(2)  = dble(annual_tile%CAI)
    out_annual_tile(3)  = dble(annual_tile%LAI)
    out_annual_tile(4)  = dble(annual_tile%density)
    out_annual_tile(5)  = dble(annual_tile%DBH)
    out_annual_tile(6)  = dble(annual_tile%density12)
    out_annual_tile(7)  = dble(annual_tile%DBH12)
    out_annual_tile(8)  = dble(annual_tile%QMD)
    out_annual_tile(9)  = dble(annual_tile%NPP)
    out_annual_tile(10) = dble(annual_tile%GPP)
    out_annual_tile(11) = dble(annual_tile%Rauto)
    out_annual_tile(12) = dble(annual_tile%Rh)
    out_annual_tile(13) = dble(annual_tile%rain)
    out_annual_tile(14) = dble(annual_tile%SoilWater)
    out_annual_tile(15) = dble(annual_tile%Transp)
    out_annual_tile(16) = dble(annual_tile%Evap)
    out_annual_tile(17) = dble(annual_tile%Runoff)
    out_annual_tile(18) = dble(annual_tile%plantC)
    out_annual_tile(19) = dble(annual_tile%soilC)
    out_annual_tile(20) = dble(annual_tile%plantN)
    out_annual_tile(21) = dble(annual_tile%soilN)
    out_annual_tile(22) = dble(annual_tile%totN)
    out_annual_tile(23) = dble(annual_tile%NSC)
    out_annual_tile(24) = dble(annual_tile%SeedC)
    out_annual_tile(25) = dble(annual_tile%leafC)
    out_annual_tile(26) = dble(annual_tile%rootC)
    out_annual_tile(27) = dble(annual_tile%SapwoodC)
    out_annual_tile(28) = dble(annual_tile%WoodC)
    out_annual_tile(29) = dble(annual_tile%NSN)
    out_annual_tile(30) = dble(annual_tile%SeedN)
    out_annual_tile(31) = dble(annual_tile%leafN)
    out_annual_tile(32) = dble(annual_tile%rootN)
    out_annual_tile(33) = dble(annual_tile%SapwoodN)
    out_annual_tile(34) = dble(annual_tile%WoodN)
    out_annual_tile(35) = dble(annual_tile%McrbC)
    out_annual_tile(36) = dble(annual_tile%fastSOM)
    out_annual_tile(37) = dble(annual_tile%SlowSOM)
    out_annual_tile(38) = dble(annual_tile%McrbN)
    out_annual_tile(39) = dble(annual_tile%fastSoilN)
    out_annual_tile(40) = dble(annual_tile%slowSoilN)
    out_annual_tile(41) = dble(annual_tile%mineralN)
    out_annual_tile(42) = dble(annual_tile%N_fxed)
    out_annual_tile(43) = dble(annual_tile%N_uptk)
    out_annual_tile(44) = dble(annual_tile%N_yrMin)
    out_annual_tile(45) = dble(annual_tile%N_P2S)
    out_annual_tile(46) = dble(annual_tile%N_loss)
    out_annual_tile(47) = dble(annual_tile%totseedC)
    out_annual_tile(48) = dble(annual_tile%totseedN)
    out_annual_tile(49) = dble(annual_tile%Seedling_C)
    out_annual_tile(50) = dble(annual_tile%Seedling_N)
    out_annual_tile(51) = dble(annual_tile%MaxAge)
    out_annual_tile(52) = dble(annual_tile%MaxVolume)
    out_annual_tile(53) = dble(annual_tile%MaxDBH)
    out_annual_tile(54) = dble(annual_tile%NPPL)
    out_annual_tile(55) = dble(annual_tile%NPPW)
    out_annual_tile(56) = dble(annual_tile%n_deadtrees)
    out_annual_tile(57) = dble(annual_tile%c_deadtrees)
    out_annual_tile(58) = dble(annual_tile%m_turnover)
    out_annual_tile(59) = dble(annual_tile%c_turnover_time)

  end subroutine populate_outarray_annual_tile

  subroutine populate_outarray_annual_cohorts( annual_cohorts, out_annual_cohorts ) 

    use md_interface_biomee, only: outtype_annual_cohorts

    ! arguments
    type(outtype_annual_cohorts), dimension(out_max_cohorts), intent(in) :: annual_cohorts
    real, dimension(out_max_cohorts,nvars_annual_cohorts), intent(inout) :: out_annual_cohorts    

    out_annual_cohorts(:, 1)  = annual_cohorts(:)%year
    out_annual_cohorts(:, 2)  = annual_cohorts(:)%cID
    out_annual_cohorts(:, 3)  = annual_cohorts(:)%PFT
    out_annual_cohorts(:, 4)  = annual_cohorts(:)%layer
    out_annual_cohorts(:, 5)  = annual_cohorts(:)%density
    out_annual_cohorts(:, 6)  = annual_cohorts(:)%flayer
    out_annual_cohorts(:, 7)  = annual_cohorts(:)%DBH
    out_annual_cohorts(:, 8)  = annual_cohorts(:)%dDBH
    out_annual_cohorts(:, 9)  = annual_cohorts(:)%height
    out_annual_cohorts(:, 10)  = annual_cohorts(:)%age
    out_annual_cohorts(:, 11)  = annual_cohorts(:)%BA
    out_annual_cohorts(:, 12)  = annual_cohorts(:)%dBA
    out_annual_cohorts(:, 13)  = annual_cohorts(:)%Acrown
    out_annual_cohorts(:, 14)  = annual_cohorts(:)%Aleaf
    out_annual_cohorts(:, 15)  = annual_cohorts(:)%nsc
    out_annual_cohorts(:, 16)  = annual_cohorts(:)%nsn
    out_annual_cohorts(:, 17)  = annual_cohorts(:)%seedC
    out_annual_cohorts(:, 18)  = annual_cohorts(:)%leafC
    out_annual_cohorts(:, 19)  = annual_cohorts(:)%rootC
    out_annual_cohorts(:, 20)  = annual_cohorts(:)%sapwC
    out_annual_cohorts(:, 21)  = annual_cohorts(:)%woodC
    out_annual_cohorts(:, 22)  = annual_cohorts(:)%treeG
    out_annual_cohorts(:, 23)  = annual_cohorts(:)%fseed
    out_annual_cohorts(:, 24)  = annual_cohorts(:)%fleaf
    out_annual_cohorts(:, 25)  = annual_cohorts(:)%froot
    out_annual_cohorts(:, 26)  = annual_cohorts(:)%fwood
    out_annual_cohorts(:, 27)  = annual_cohorts(:)%GPP
    out_annual_cohorts(:, 28)  = annual_cohorts(:)%NPP
    out_annual_cohorts(:, 29)  = annual_cohorts(:)%Rauto
    out_annual_cohorts(:, 30)  = annual_cohorts(:)%Nupt
    out_annual_cohorts(:, 31)  = annual_cohorts(:)%Nfix
    out_annual_cohorts(:, 32)  = annual_cohorts(:)%n_deadtrees
    out_annual_cohorts(:, 33)  = annual_cohorts(:)%c_deadtrees
    out_annual_cohorts(:, 34)  = annual_cohorts(:)%deathrate

  end subroutine populate_outarray_annual_cohorts

  !========================================================================
  ! read in forcing data (Users need to write their own data input procedure)
  subroutine read_FACEforcing(forcingData,datalines,days_data,yr_data,timestep)
    type(climate_type),pointer,intent(inout) :: forcingData(:)
    integer,intent(inout) :: datalines,days_data,yr_data
    real, intent(inout)   :: timestep
    !------------local var -------------------
    type(climate_type), pointer :: climateData(:)
    character(len=80)  commts
    integer, parameter :: niterms=9       ! MDK data for Oak Ridge input
    integer, parameter :: ilines=22*366*24 ! the maxmum records of Oak Ridge FACE, 1999~2007
    integer,dimension(ilines) :: year_data
    real,   dimension(ilines) :: doy_data,hour_data
    real input_data(niterms,ilines)
    real inputstep
    integer :: istat1,istat2,istat3
    integer :: doy,idays
    integer :: i,j,k
    integer :: m,n
    integer :: idx_climatedata

    character(len=80) :: filepath_in = '/home/laura/rsofun/data-raw/'
    character(len=80) :: climfile    = 'ORNL_forcing.txt'

    climfile=trim(filepath_in)//trim(climfile)
    write(*,*)'inputfile: ',climfile

    ! open forcing data
    open(11,file=climfile,status='old',ACTION='read',IOSTAT=istat2)
    write(*,*)istat2
    ! skip 2 lines of input met data file
    read(11,'(a160)') commts
    ! read(11,'(a160)') commts ! MDK data only has one line comments
    m       = 0  ! to record the lines in a file
    idays   = 1  ! the total days in a data file
    yr_data = 0 ! to record years of a dataset
    do    ! read forcing files
      m=m+1
      read(11,*,IOSTAT=istat3)year_data(m),doy_data(m),hour_data(m),   &
      (input_data(n,m),n=1,niterms)
      if(istat3<0)exit
      if(m == 1) then
        doy = doy_data(m)
      else
        doy = doy_data(m-1)
      endif
      if(doy /= doy_data(m)) idays = idays + 1
      write(*,*)year_data(m),doy_data(m),hour_data(m)
    enddo ! end of reading the forcing file

    timestep = hour_data(2) - hour_data(1)
    write(*,*)"forcing", datalines, yr_data, timestep, myinterface%dt_fast_yr

    if (timestep==1.0)then
      write(*,*)"the data frequency is hourly"
    elseif(timestep==0.5)then
      write(*,*)"the data frequency is half hourly"
    else
      write(*,*)"Please check time step!"
      stop
    endif
    close(11)    ! close forcing file

    print*,'1'
    print*,'input par', input_data(1,10)

    ! Put the data into forcing 
    datalines = m - 1
    days_data = idays
    yr_data  = year_data(datalines-1) - year_data(1) + 1

    ! print*,'2'  
    allocate(climateData(datalines - 72))  !3*24
    days_data = days_data - 3

    ! print*,'3'
    ! print*,'datalines', datalines
    ! print*,'size(input_data)', shape(input_data)
    ! print*,'length(climateData)', size(climateData)
    idx_climatedata = 0
    do i=1,datalines
      ! print*,'i, doy_data(i), year_data(i)', i, doy_data(i), year_data(i)
     if (.not. (doy_data(i)==60 .and. (year_data(i)==2000 .or. year_data(i)==2004 .or. year_data(i)==2008))) then
       
       idx_climatedata = idx_climatedata + 1 !xxx debug 

       climateData(idx_climatedata)%year      = year_data(i)          ! Year
       climateData(idx_climatedata)%doy       = doy_data(i)           ! day of the year
       climateData(idx_climatedata)%hod       = hour_data(i)          ! hour of the day
       climateData(idx_climatedata)%PAR       = input_data(1,i)       ! umol/m2/s
       climateData(idx_climatedata)%radiation = input_data(2,i)       ! W/m2
       climateData(idx_climatedata)%Tair      = input_data(3,i) + 273.16  ! air temperature, K
       climateData(idx_climatedata)%Tsoil     = input_data(4,i) + 273.16  ! soil temperature, K
       climateData(idx_climatedata)%RH        = input_data(5,i) * 0.01    ! relative humidity (0.xx)
       climateData(idx_climatedata)%rain      = input_data(6,i)/(timestep * 3600)! ! kgH2O m-2 s-1
       climateData(idx_climatedata)%windU     = input_data(7,i)        ! wind velocity (m s-1)
       climateData(idx_climatedata)%P_air     = input_data(8,i)        ! pa
       climateData(idx_climatedata)%CO2       = input_data(9,i) * 1.0e-6       ! mol/mol
       climateData(idx_climatedata)%soilwater = 0.8    ! soil moisture, vol/vol
      else
     end if
   enddo
   forcingData => climateData

   ! print*,'4'
   datalines = datalines - 72  !3*24

   write(*,*)"forcing", datalines,days_data,yr_data

  end subroutine read_FACEforcing

end program main