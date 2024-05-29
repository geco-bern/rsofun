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
    climate_type
  use md_interface_biomee, only: interfacetype_biosphere, &
    myinterface, &
    outtype_biosphere, &
    outtype_daily_tile, &
    outtype_daily_cohorts, &
    outtype_annual_tile, &
    outtype_annual_cohorts
  use md_params_core
  use md_biosphere_biomee, only: biosphere_annual
  use datatypes

  implicit none

  ! Simulation parameters
  logical(kind=c_bool), intent(in) :: spinup
  integer(kind=c_int),  intent(in) :: spinupyears
  integer(kind=c_int),  intent(in) :: recycle
  integer(kind=c_int),  intent(in) :: firstyeartrend
  integer(kind=c_int),  intent(in) :: nyeartrend
  integer(kind=c_int),  intent(in) :: runyears

  logical(kind=c_bool), intent(in) :: outputhourly
  logical(kind=c_bool), intent(in) :: outputdaily
  logical(kind=c_bool), intent(in) :: do_U_shaped_mortality
  logical(kind=c_bool), intent(in) :: update_annualLAImax
  logical(kind=c_bool), intent(in) :: do_closedN_run
  logical(kind=c_bool), intent(in) :: do_reset_veg
  integer(kind=c_int),  intent(in) :: dist_frequency
  integer(kind=c_int),  intent(in) :: code_method_photosynth
  integer(kind=c_int),  intent(in) :: code_method_mortality

  ! site information
  real(kind=c_double),  intent(in) :: longitude
  real(kind=c_double),  intent(in) :: latitude
  real(kind=c_double),  intent(in) :: altitude

  ! Tile parameters
  integer(kind=c_int), intent(in) :: soiltype
  real(kind=c_double), intent(in) :: FLDCAP
  real(kind=c_double), intent(in) :: WILTPT
  real(kind=c_double), intent(in) :: K1
  real(kind=c_double), intent(in) :: K2
  real(kind=c_double), intent(in) :: K_nitrogen
  real(kind=c_double), intent(in) :: MLmixRatio
  real(kind=c_double), intent(in) :: etaN
  real(kind=c_double), intent(in) :: LMAmin
  real(kind=c_double), intent(in) :: fsc_fine
  real(kind=c_double), intent(in) :: fsc_wood
  real(kind=c_double), intent(in) :: GR_factor
  real(kind=c_double), intent(in) :: l_fract
  real(kind=c_double), intent(in) :: retransN
  real(kind=c_double), intent(in) :: f_initialBSW
  real(kind=c_double), intent(in) :: f_N_add
  real(kind=c_double), intent(in) :: tf_base
  real(kind=c_double), intent(in) :: par_mort
  real(kind=c_double), intent(in) :: par_mort_under

  ! naked arrays
  real(kind=c_double), dimension(0:MSPECIES,55), intent(in)       :: params_species
  real(kind=c_double), dimension(n_dim_soil_types,8), intent(in)  :: params_soil
  real(kind=c_double), dimension(MAX_INIT_COHORTS,9),  intent(in) :: init_cohort

  ! initial soil pool size
  real(kind=c_double), intent(in) :: init_fast_soil_C
  real(kind=c_double), intent(in) :: init_slow_soil_C
  real(kind=c_double), intent(in) :: init_Nmineral
  real(kind=c_double), intent(in) :: N_input

  integer(kind=c_int), intent(in) :: nt
  integer(kind=c_int), intent(in) :: nt_daily
  integer(kind=c_int), intent(in) :: nt_annual
  integer(kind=c_int), intent(in) :: nt_annual_cohorts

  !----------------------------------------------------------------
  ! LOCAL VARIABLES READ FROM/TO FILE/OUTPUT
  !----------------------------------------------------------------
  ! local variables
  integer :: datalines
  integer :: yr_data   ! Years of the forcing data
  integer :: totyears
  integer :: totdays
  integer :: days_data ! days of the forcing data
  real    :: timestep  ! hour, Time step of forcing data, usually hourly (1.0)
  integer :: ntstepsyear_forcing                 ! 365*48 when half-hourly inputs, 365*24 when hourly inputs
  type(outtype_biosphere) :: out_biosphere  ! holds all the output used for calculating the cost or maximum likelihood function 
  integer :: yr
  logical, parameter :: verbose = .false.
  integer :: iday

  character(len=100) :: namelistfile = '/home/laura/rsofun/inst/extdata/parameters_Allocation.nml'

  ! output arrays (naked) to be passed back to C/R
  !real, dimension(:,:), allocatable  :: out_hourly_tile 
  !real, dimension(:,:), allocatable  :: out_daily_tile       !fno4
  !real, dimension(:,:,:), allocatable:: out_daily_cohorts    !fno3
  real, dimension(:,:), allocatable  :: out_annual_tile      !fno5
  real, dimension(:,:,:), allocatable:: out_annual_cohorts   !fno2

  ! whether fast time step processes are simulated. If .false., then C, N, and W balance is simulated daily.
  logical, parameter :: daily = .true.

! xxxx !!!!!!!!!!!!!!!!!!!
  ! input and output arrays (naked) to be passed back to C/R
  real(kind=c_double), dimension(nt,13), intent(in) :: forcing

  ! real(kind=c_double), dimension(nt,nvars_hourly_tile), intent(out) :: output_hourly_tile ! nvars_hourly_tile = 15
  real(kind=c_double), dimension(nt_daily,nvars_daily_tile), intent(out) :: output_daily_tile ! nvars_daily_tile = 35    

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

  real(kind=c_double), dimension(nt_annual,nvars_annual_tile), intent(out) :: output_annual_tile ! nvars_annual_tile = 51

  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_year
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_cID
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_PFT
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_layer
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_density
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_flayer
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_DBH
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_dDBH
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_height
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_age
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_BA
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_dBA
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_Acrown
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_Aleaf
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_nsc
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_nsn
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_seedC
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_leafC
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_rootC
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_sapwC
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_woodC
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_treeG
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_fseed
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_fleaf
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_froot
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_fwood
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_GPP
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_NPP
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_Rauto
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_Nupt
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_Nfix
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_n_deadtrees
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_c_deadtrees
  real(kind=c_double), dimension(nt_annual_cohorts,out_max_cohorts), intent(out) :: output_annual_cohorts_deathrate

  ! local variables
  ! type(outtype_biosphere) :: out_biosphere  ! holds all the output used for calculating the cost or maximum likelihood function 
  type(outtype_daily_tile),     dimension(ndayyear)                 :: out_biosphere_daily_tile
  ! type(outtype_daily_cohorts),  dimension(ndayyear,out_max_cohorts) :: out_biosphere_daily_cohorts
  type(outtype_annual_tile)                                         :: out_biosphere_annual_tile
  type(outtype_annual_cohorts), dimension(out_max_cohorts)          :: out_biosphere_annual_cohorts

  real                    :: timestep, timestep_d
  integer                 :: yr
  
  integer :: idx
  ! integer :: idx_hourly_start
  ! integer :: idx_hourly_end
  integer :: idx_daily_start
  integer :: idx_daily_end

  !----------------------------------------------------------------
  ! DECLARATIONS TO READ FROM NAMELIST FILE
  !----------------------------------------------------------------
  integer :: io           ! i/o status for the namelist
  integer :: ierr         ! error code, returned by i/o routines
  integer :: i
  integer :: nml_unit

  integer :: j

  namelist /vegn_parameters_nml/  &
  soiltype, FLDCAP, WILTPT, &
  pt, phenotype, lifeform, &
  Vmax, Vannual,wet_leaf_dreg,   &
  gamma_L, gamma_LN, gamma_SW, gamma_FR,  &
  rho_FR, root_r, root_zeta,Kw_root, &
  !rho_N_up0, N_roots0, &
  leaf_size, leafLS, LAImax, LAI_light,   &
  LMA, LNbase, CNleafsupport, c_LLS,      &
  K1,K2, K_nitrogen, etaN, MLmixRatio,    &
  LMAmin, fsc_fine, fsc_wood, &
  GR_factor, l_fract, retransN,f_N_add,   &
  f_initialBSW,f_LFR_max,  &
  gdd_crit,tc_crit, tc_crit_on, &
  alphaHT, thetaHT, alphaCA, thetaCA, alphaBM, thetaBM, &
  maturalage, v_seed, seedlingsize, prob_g,prob_e,      &
  mortrate_d_c, mortrate_d_u, A_mort, B_mort,DBHtp,     &
  phiRL, phiCSA, rho_wood, taperfactor, &
  tauNSC, fNSNmax, understory_lai_factor, &
  CNleaf0,CNsw0,CNwood0,CNroot0,CNseed0, &
  NfixRate0, NfixCost0,  &
  internal_gap_frac

  namelist /soil_data_nml/ &
  GMD, GSD, vwc_sat,k_sat_ref, psi_sat_ref, &
  chb, alphaSoil,heat_capacity_dry

  namelist /initial_state_nml/ &
  init_n_cohorts, init_cohort_species, init_cohort_nindivs, &
  init_cohort_bl, init_cohort_br, init_cohort_bsw, &
  init_cohort_bHW, init_cohort_seedC, init_cohort_nsc, &
  init_fast_soil_C, init_slow_soil_C,    & 
  init_Nmineral, N_input,  &
  filepath_in,climfile, &
  equi_days, &
  outputhourly, &
  outputdaily, &
  do_U_shaped_mortality, &
  update_annualLAImax, &
  do_closedN_run, &
  spinup, spinupyears, recycle, firstyeartrend, nyeartrend, &
  longitude, latitude, altitude

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
  
  ! this needs to be consistent with translation to code in run_biomee_f_bysite.R
  if (code_method_photosynth == 1) then
    myinterface%params_siml%method_photosynth = "gs_leuning"
  else if (code_method_photosynth == 2) then
    myinterface%params_siml%method_photosynth = "pmodel"
  end if

  ! this needs to be consistent with translation to code in run_biomee_f_bysite.R
  if (code_method_mortality == 1) then
    myinterface%params_siml%method_mortality = "cstarvation"
  else if (code_method_mortality == 2) then
    myinterface%params_siml%method_mortality = "growthrate"
  else if (code_method_mortality == 3) then
    myinterface%params_siml%method_mortality = "dbh"
  else if (code_method_mortality == 4) then
    myinterface%params_siml%method_mortality = "const_selfthin"
  else if (code_method_mortality == 5) then
    myinterface%params_siml%method_mortality = "bal"
  end if

  myinterface%grid%lon = longitude
  myinterface%grid%lat = latitude
  myinterface%grid%elv = altitude

  ! Tile parameters
  myinterface%params_tile%soiltype     = int(soiltype)
  myinterface%params_tile%FLDCAP       = real( FLDCAP )
  myinterface%params_tile%WILTPT       = real( WILTPT )
  myinterface%params_tile%K1           = real( K1 )
  myinterface%params_tile%K2           = real( K2 )
  myinterface%params_tile%K_nitrogen   = real( K_nitrogen )
  myinterface%params_tile%MLmixRatio   = real( MLmixRatio )
  myinterface%params_tile%etaN         = real( etaN )
  myinterface%params_tile%LMAmin       = real( LMAmin )
  myinterface%params_tile%fsc_fine     = real( fsc_fine )
  myinterface%params_tile%fsc_wood     = real( fsc_wood )
  myinterface%params_tile%GR_factor    = real( GR_factor )
  myinterface%params_tile%l_fract      = real( l_fract )
  myinterface%params_tile%retransN     = real( retransN )
  myinterface%params_tile%f_initialBSW = real( f_initialBSW )
  myinterface%params_tile%f_N_add      = real( f_N_add )
  myinterface%params_tile%tf_base      = real( tf_base )
  myinterface%params_tile%par_mort     = real( par_mort )
  myinterface%params_tile%par_mort_under  = real( par_mort_under )

  ! Species parameters
  myinterface%params_species%lifeform(:)          = lifeform
  myinterface%params_species%phenotype(:)         = phenotype
  myinterface%params_species%pt(:)                = pt
  myinterface%params_species%alpha_FR(:)          = ralpha_FR
  myinterface%params_species%rho_FR(:)            = rho_FR
  myinterface%params_species%root_r(:)            = root_r
  myinterface%params_species%root_zeta(:)         = root_zeta
  myinterface%params_species%Kw_root(:)           = Kw_root
  myinterface%params_species%leaf_size(:)         = leaf_size
  myinterface%params_species%Vmax(:)              = Vmax
  myinterface%params_species%Vannual(:)           = Vannual
  myinterface%params_species%wet_leaf_dreg(:)     = wet_leaf_dreg
  myinterface%params_species%m_cond(:)            = m_cond
  myinterface%params_species%alpha_phot(:)        = alpha_phot
  myinterface%params_species%gamma_L(:)           = gamma_L
  myinterface%params_species%gamma_LN(:)          = gamma_LN
  myinterface%params_species%gamma_SW(:)          = gamma_SW
  myinterface%params_species%gamma_FR(:)          = gamma_FR
  myinterface%params_species%tc_crit(:)           = tc_crit
  myinterface%params_species%tc_crit_on(:)        = tc_crit_on
  myinterface%params_species%gdd_crit(:)          = gdd_crit
  myinterface%params_species%betaON(:)            = betaON
  myinterface%params_species%betaOFF(:)           = betaOFF
  myinterface%params_species%alphaHT(:)           = alphaHT
  myinterface%params_species%thetaHT(:)           = thetaHT
  myinterface%params_species%alphaCA(:)           = alphaCA
  myinterface%params_species%thetaCA(:)           = thetaCA
  myinterface%params_species%alphaBM(:)           = alphaBM
  myinterface%params_species%thetaBM(:)           = thetaBM
  myinterface%params_species%seedlingsize(:)      = seedlingsize
  myinterface%params_species%maturalage(:)        = maturalage
  myinterface%params_species%v_seed(:)            = v_seed
  myinterface%params_species%mortrate_d_c(:)      = mortrate_d_c
  myinterface%params_species%mortrate_d_u(:)      = mortrate_d_u
  myinterface%params_species%LMA(:)               = LMA
  myinterface%params_species%leafLS(:)            = leafLS
  myinterface%params_species%LNbase(:)            = LNbase
  myinterface%params_species%CNleafsupport(:)     = CNleafsupport
  myinterface%params_species%rho_wood(:)          = rho_wood
  myinterface%params_species%taperfactor(:)       = taperfactor
  myinterface%params_species%lAImax(:)            = lAImax
  myinterface%params_species%tauNSC(:)            = tauNSC
  myinterface%params_species%fNSNmax(:)           = fNSNmax
  myinterface%params_species%phiCSA(:)            = phiCSA
  myinterface%params_species%CNleaf0(:)           = CNleaf0
  myinterface%params_species%CNsw0(:)             = CNsw0
  myinterface%params_species%CNwood0(:)           = CNwood0
  myinterface%params_species%CNroot0(:)           = CNroot0
  myinterface%params_species%CNseed0(:)           = CNseed0
  myinterface%params_species%Nfixrate0(:)         = Nfixrate0
  myinterface%params_species%NfixCost0(:)         = NfixCost0
  myinterface%params_species%internal_gap_frac(:) = internal_gap_frac
  myinterface%params_species%kphio(:)             = kphio
  myinterface%params_species%phiRL(:)             = phiRL
  myinterface%params_species%LAI_light(:)         = LAI_light

  ! Initial cohort sizes
  myinterface%init_cohort%init_n_cohorts(:)      = int(init_cohort(:,1))
  myinterface%init_cohort%init_cohort_species(:) = int(init_cohort(:,2))
  myinterface%init_cohort%init_cohort_nindivs(:) = real(init_cohort(:,3))
  myinterface%init_cohort%init_cohort_bl(:)      = real(init_cohort(:,4))
  myinterface%init_cohort%init_cohort_br(:)      = real(init_cohort(:,5))
  myinterface%init_cohort%init_cohort_bsw(:)     = real(init_cohort(:,6))
  myinterface%init_cohort%init_cohort_bHW(:)     = real(init_cohort(:,7))
  myinterface%init_cohort%init_cohort_seedC(:)   = real(init_cohort(:,8))
  myinterface%init_cohort%init_cohort_nsc(:)     = real(init_cohort(:,9))

  ! Initial soil pools
  myinterface%init_soil%init_fast_soil_C = real( init_fast_soil_C )
  myinterface%init_soil%init_slow_soil_C = real( init_slow_soil_C )
  myinterface%init_soil%init_Nmineral    = real( init_Nmineral )
  myinterface%init_soil%N_input          = real( N_input )

  !----------------------------------------------------------------
  ! GET SOIL PARAMETERS
  !----------------------------------------------------------------
  !myinterface%params_soil = getsoil( params_soil )

  myinterface%params_soil%GMD(:)               = real(params_soil(:,1))
  myinterface%params_soil%GSD(:)               = real(params_soil(:,2))
  myinterface%params_soil%vwc_sat(:)           = real(params_soil(:,3))
  myinterface%params_soil%chb(:)               = real(params_soil(:,4))
  myinterface%params_soil%psi_sat_ref(:)       = real(params_soil(:,5))
  myinterface%params_soil%k_sat_ref(:)         = real(params_soil(:,6))
  myinterface%params_soil%alphaSoil(:)         = real(params_soil(:,7))
  myinterface%params_soil%heat_capacity_dry(:) = real(params_soil(:,8))

  !----------------------------------------------------------------
  ! READ FORCING FILE
  !----------------------------------------------------------------
  call read_FACEforcing( forcingData, datalines, days_data, yr_data, timestep ) !! ORNL

  !----------------------------------------------------------------
  ! INTERPRET FORCING
  !----------------------------------------------------------------
  timestep   = real(forcing(2,3)) - real(forcing(1,3))  ! This takes the hour of day (a numeric) from the forcing file
  timestep_d = real(forcing(2,2)) - real(forcing(1,2))  ! This takes the day of year (a numeric) from the forcing file
  if (abs(timestep) < eps .and. abs(timestep_d - 1.0) < eps) then
    ! forcing is daily
    timestep = 24.0
  end if
  myinterface%steps_per_day = int(24.0/timestep)
  myinterface%dt_fast_yr = 1.0/(365.0 * myinterface%steps_per_day)
  myinterface%step_seconds = 24.0*3600.0/myinterface%steps_per_day ! seconds_per_year * dt_fast_yr
  ntstepsyear = myinterface%steps_per_day * 365

  allocate(myinterface%climate(ntstepsyear))
  allocate(myinterface%pco2(ntstepsyear))
  allocate(out_biosphere%hourly_tile(ntstepsyear))

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
                                          forcing, &
                                          myinterface%steering%climateyear_idx &
                                          ! myinterface%steering%climateyear &
                                          )

    !----------------------------------------------------------------
    ! Call biosphere (wrapper for all modules, contains time loops)
    !----------------------------------------------------------------
    call biosphere_annual( &
      out_biosphere_daily_tile, &
      ! out_biosphere_daily_cohorts, &
      out_biosphere_annual_tile, &
      out_biosphere_annual_cohorts &
      )

    !----------------------------------------------------------------
    ! Populate big output arrays
    !----------------------------------------------------------------

    !----------------------------------------------------------------
    ! Output out_hourly_tile (calling subroutine)
    !----------------------------------------------------------------
    ! if (.not. myinterface%steering%spinup) then  
    !   idx_hourly_start = (yr - myinterface%params_siml%spinupyears - 1) * ntstepsyear + 1    ! To exclude the spinup years and include only the transient years
    !   idx_hourly_end   = idx_hourly_start + ntstepsyear - 1
    !   ! call populate_outarray_hourly_tile( out_biosphere_hourly_tile(:), output_hourly_tile(idx_hourly_start:idx_hourly_end,:)) !xxx commented out for calibration!
    ! end if

    !----------------------------------------------------------------
    ! Output out_daily_tile (calling subroutine)
    !----------------------------------------------------------------
    ! Output only for transient years
    if (.not. myinterface%steering%spinup) then  

      idx_daily_start = (yr - myinterface%params_siml%spinupyears - 1) * ndayyear + 1  
      idx_daily_end   = idx_daily_start + ndayyear - 1

      call populate_outarray_daily_tile( out_biosphere_daily_tile(:), output_daily_tile(idx_daily_start:idx_daily_end,:))

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

    !----------------------------------------------------------------
    ! Output out_annual_tile (calling subroutine)
    !----------------------------------------------------------------
    call populate_outarray_annual_tile( out_biosphere_annual_tile, output_annual_tile(yr,:) )

    !----------------------------------------------------------------
    ! Output output_annual_cohorts (without subroutine)
    !----------------------------------------------------------------
    ! To get outputs only after spinupyears make if below and 
    ! also in run_biomee_f_bysite.R make n_annual_cohorts = as.integer(params_siml$nyeartrend)

    if (.not. myinterface%steering%spinup) then  

      idx =  yr - myinterface%params_siml%spinupyears

      output_annual_cohorts_year(idx, :)        = dble(out_biosphere_annual_cohorts(:)%year)
      output_annual_cohorts_cID(idx, :)         = dble(out_biosphere_annual_cohorts(:)%cID)
      output_annual_cohorts_PFT(idx, :)         = dble(out_biosphere_annual_cohorts(:)%PFT)
      output_annual_cohorts_layer(idx, :)       = dble(out_biosphere_annual_cohorts(:)%layer)
      output_annual_cohorts_density(idx, :)     = dble(out_biosphere_annual_cohorts(:)%density)
      output_annual_cohorts_flayer(idx, :)      = dble(out_biosphere_annual_cohorts(:)%flayer)
      output_annual_cohorts_dbh(idx, :)         = dble(out_biosphere_annual_cohorts(:)%DBH)
      output_annual_cohorts_dDBH(idx, :)        = dble(out_biosphere_annual_cohorts(:)%dDBH)
      output_annual_cohorts_height(idx, :)      = dble(out_biosphere_annual_cohorts(:)%height)
      output_annual_cohorts_age(idx, :)         = dble(out_biosphere_annual_cohorts(:)%age)
      output_annual_cohorts_BA(idx, :)          = dble(out_biosphere_annual_cohorts(:)%BA)
      output_annual_cohorts_dBA(idx, :)         = dble(out_biosphere_annual_cohorts(:)%dBA)
      output_annual_cohorts_Acrown(idx, :)      = dble(out_biosphere_annual_cohorts(:)%Acrown)
      output_annual_cohorts_Aleaf(idx, :)       = dble(out_biosphere_annual_cohorts(:)%Aleaf)
      output_annual_cohorts_nsc(idx, :)         = dble(out_biosphere_annual_cohorts(:)%nsc)
      output_annual_cohorts_nsn(idx, :)         = dble(out_biosphere_annual_cohorts(:)%nsn)
      output_annual_cohorts_seedC(idx, :)       = dble(out_biosphere_annual_cohorts(:)%seedC)
      output_annual_cohorts_leafC(idx, :)       = dble(out_biosphere_annual_cohorts(:)%leafC)
      output_annual_cohorts_rootC(idx, :)       = dble(out_biosphere_annual_cohorts(:)%rootC)
      output_annual_cohorts_sapwC(idx, :)       = dble(out_biosphere_annual_cohorts(:)%sapwC)
      output_annual_cohorts_woodC(idx, :)       = dble(out_biosphere_annual_cohorts(:)%woodC)
      output_annual_cohorts_treeG(idx, :)       = dble(out_biosphere_annual_cohorts(:)%treeG)
      output_annual_cohorts_fseed(idx, :)       = dble(out_biosphere_annual_cohorts(:)%fseed)
      output_annual_cohorts_fleaf(idx, :)       = dble(out_biosphere_annual_cohorts(:)%fleaf)
      output_annual_cohorts_froot(idx, :)       = dble(out_biosphere_annual_cohorts(:)%froot)
      output_annual_cohorts_fwood(idx, :)       = dble(out_biosphere_annual_cohorts(:)%fwood)
      output_annual_cohorts_GPP(idx, :)         = dble(out_biosphere_annual_cohorts(:)%GPP)
      output_annual_cohorts_NPP(idx, :)         = dble(out_biosphere_annual_cohorts(:)%NPP)
      output_annual_cohorts_Rauto(idx, :)       = dble(out_biosphere_annual_cohorts(:)%Rauto)
      output_annual_cohorts_Nupt(idx, :)        = dble(out_biosphere_annual_cohorts(:)%Nupt)
      output_annual_cohorts_Nfix(idx, :)        = dble(out_biosphere_annual_cohorts(:)%Nfix)
      output_annual_cohorts_deathrate(idx, :)   = dble(out_biosphere_annual_cohorts(:)%deathrate)
      output_annual_cohorts_n_deadtrees(idx, :) = dble(out_biosphere_annual_cohorts(:)%n_deadtrees)
      output_annual_cohorts_c_deadtrees(idx, :) = dble(out_biosphere_annual_cohorts(:)%c_deadtrees)

     end if

  end do yearloop

  deallocate(myinterface%climate)
  deallocate(myinterface%pco2)
  deallocate(out_biosphere%hourly_tile)
  deallocate(out_hourly_tile)
  deallocate(out_daily_cohorts)
  deallocate(out_daily_tile)
  deallocate(out_annual_cohorts)
  deallocate(out_annual_tile)

  100  format (A,I6,I6,F8.2)
  777  format (F20.8,F20.8)
  999  format (I4.4)

contains

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
    character(len=80) :: climfile    = 'CHLAE_forcing.txt'

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
      !write(*,*)year_data(m),doy_data(m),hour_data(m)
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

    ! print*,'1'
    ! print*,'input par', input_data(1,10)

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