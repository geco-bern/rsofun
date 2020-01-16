module sofun_r_mod
  !////////////////////////////////////////////////////////////////
  ! Contains main subroutine handling I/O with C and R.
  !----------------------------------------------------------------
  use, intrinsic :: iso_c_binding, only: c_double, c_int, c_char, c_bool

  implicit none

  private
  public :: pmodel_f, lm3ppa_f

contains

  subroutine pmodel_f(          &
    spinup,                    &   
    spinupyears,               &        
    recycle,                   &    
    firstyeartrend,            &           
    nyeartrend,                &       
    soilmstress,               &        
    tempstress,                &       
    calc_aet_fapar_vpd,        &       
    in_ppfd,                   &    
    in_netrad,                 &      
    outdt,                     &  
    ltre,                      & 
    ltne,                      & 
    ltrd,                      & 
    ltnd,                      & 
    lgr3,                      & 
    lgn3,                      & 
    lgr4,                      & 
    longitude,                 &      
    latitude,                  &     
    altitude,                  &     
    whc,                       &
    soiltexture,               &
    nt,                        &
    par,                       &
    forcing,                   &
    output                     &
    ) bind(C, name = "pmodel_f_")

    !////////////////////////////////////////////////////////////////
    ! Main subroutine to handle I/O with C and R. 
    ! Receives simulation parameters, site parameters, and the full 
    ! simulation's forcing as time series
    !----------------------------------------------------------------
    use md_params_siml_pmodel, only: getsteering
    ! use md_grid, only: get_domaininfo, getgrid, type_params_domain
    use md_params_soil_pmodel, only: getsoil
    use md_forcing_pmodel, only: getclimate, getco2, getfapar, get_fpc_grid
    use md_interface_pmodel, only: interfacetype_biosphere, outtype_biosphere, myinterface
    use md_params_core_pmodel, only: nlayers_soil, ndayyear, npft
    use md_biosphere_pmodel, only: biosphere_annual

    implicit none

    ! arguments
    logical(kind=c_bool), intent(in) :: spinup
    integer(kind=c_int),  intent(in) :: spinupyears
    integer(kind=c_int),  intent(in) :: recycle
    integer(kind=c_int),  intent(in) :: firstyeartrend
    integer(kind=c_int),  intent(in) :: nyeartrend
    logical(kind=c_bool), intent(in) :: soilmstress
    logical(kind=c_bool), intent(in) :: tempstress
    logical(kind=c_bool), intent(in) :: calc_aet_fapar_vpd
    logical(kind=c_bool), intent(in) :: in_ppfd
    logical(kind=c_bool), intent(in) :: in_netrad
    integer(kind=c_int),  intent(in) :: outdt
    logical(kind=c_bool), intent(in) :: ltre
    logical(kind=c_bool), intent(in) :: ltne
    logical(kind=c_bool), intent(in) :: ltrd
    logical(kind=c_bool), intent(in) :: ltnd
    logical(kind=c_bool), intent(in) :: lgr3
    logical(kind=c_bool), intent(in) :: lgn3
    logical(kind=c_bool), intent(in) :: lgr4
    real(kind=c_double),  intent(in) :: longitude
    real(kind=c_double),  intent(in) :: latitude
    real(kind=c_double),  intent(in) :: altitude
    real(kind=c_double),  intent(in) :: whc
    real(kind=c_double),  dimension(4,nlayers_soil), intent(in) :: soiltexture   ! soil texture (rows: sand, clay, organic, gravel; columns: layers from top)
    integer(kind=c_int),  intent(in) :: nt ! number of time steps
    real(kind=c_double),  dimension(6), intent(in) :: par  ! free (calibratable) model parameters
    real(kind=c_double),  dimension(nt,13), intent(in) :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition, 10=fapar) 
    real(kind=c_double),  dimension(nt,5), intent(out) :: output

    ! local variables
    type(outtype_biosphere) :: out_biosphere  ! holds all the output used for calculating the cost or maximum likelihood function 
    type(type_params_domain) :: params_domain
    integer :: npft_local, yr, ncells, idx_start, idx_end

    !----------------------------------------------------------------
    ! GET SIMULATION PARAMETERS
    !----------------------------------------------------------------
    myinterface%params_siml%do_spinup      = spinup
    myinterface%params_siml%spinupyears    = spinupyears
    myinterface%params_siml%recycle        = recycle
    myinterface%params_siml%firstyeartrend = firstyeartrend
    myinterface%params_siml%nyeartrend     = nyeartrend

    if (myinterface%params_siml%do_spinup) then
      myinterface%params_siml%runyears = myinterface%params_siml%nyeartrend + myinterface%params_siml%spinupyears
    else
      myinterface%params_siml%runyears = myinterface%params_siml%nyeartrend
      myinterface%params_siml%spinupyears = 0
    endif
    
    myinterface%params_siml%soilmstress        = soilmstress
    myinterface%params_siml%tempstress         = tempstress
    myinterface%params_siml%calc_aet_fapar_vpd = calc_aet_fapar_vpd
    myinterface%params_siml%in_ppfd            = in_ppfd
    myinterface%params_siml%in_netrad          = in_netrad
    myinterface%params_siml%outdt              = outdt
    myinterface%params_siml%ltre               = ltre
    myinterface%params_siml%ltne               = ltne
    myinterface%params_siml%ltrd               = ltrd
    myinterface%params_siml%ltnd               = ltnd
    myinterface%params_siml%lgr3               = lgr3
    myinterface%params_siml%lgn3               = lgn3
    myinterface%params_siml%lgr4               = lgr4

    ! Count PFTs to be simulated
    npft_local = 0
    if (myinterface%params_siml%ltre) npft_local = npft_local + 1
    if (myinterface%params_siml%ltne) npft_local = npft_local + 1
    if (myinterface%params_siml%ltrd) npft_local = npft_local + 1
    if (myinterface%params_siml%ltnd) npft_local = npft_local + 1
    if (myinterface%params_siml%lgr3) npft_local = npft_local + 1
    if (myinterface%params_siml%lgr4) npft_local = npft_local + 1
    if (myinterface%params_siml%lgn3) npft_local = npft_local + 1

    ! allocate variable size arrays
    if (.not. allocated(myinterface%fpc_grid)) allocate( myinterface%fpc_grid( npft_local ) )

    ! set parameter to define that this is not a calibration run (otherwise sofun.f90 would not have been compiled, but sofun_simsuite.f90)
    myinterface%params_siml%is_calib = .true.  ! treat paramters passed through R/C-interface the same way as calibratable parameters

    !----------------------------------------------------------------
    ! GET GRID INFORMATION
    !----------------------------------------------------------------
    myinterface%grid%lon = real( longitude )
    myinterface%grid%lat = real( latitude )
    myinterface%grid%elv = real( altitude )
    myinterface%grid%dogridcell = .true.  ! xxx todo remove all dogridcell statemetns
    myinterface%grid%landfrac = 1.0
    myinterface%grid%area     = 1.0

    !----------------------------------------------------------------
    ! GET SOIL PARAMETERS
    !----------------------------------------------------------------
    myinterface%soilparams = getsoil( soiltexture )

    ! Overwrite whc
    myinterface%soilparams%whc(:) = whc

    !----------------------------------------------------------------
    ! GET CALIBRATABLE MODEL PARAMETERS (so far a small list)
    !----------------------------------------------------------------
    ! XXX warning: converting from double to single may cause a problem
    ! when calibrating and parameters are varied in their Nth digit after
    ! the comma!  
    myinterface%params_calib%kphio           = real(par(1))
    myinterface%params_calib%soilm_par_a     = real(par(2))
    myinterface%params_calib%soilm_par_b     = real(par(3))
    myinterface%params_calib%vpdstress_par_a = real(par(4))
    myinterface%params_calib%vpdstress_par_b = real(par(5))
    myinterface%params_calib%vpdstress_par_m = real(par(6))

    !----------------------------------------------------------------
    ! GET VEGETATION COVER (fractional projective cover by PFT)
    !----------------------------------------------------------------
    myinterface%fpc_grid(:) = get_fpc_grid( myinterface%domaininfo, myinterface%params_siml )


    ! LOOP THROUGH YEARS
    ! print*, '-------------------START OF SIMULATION--------------------'


    do yr=1,myinterface%params_siml%runyears

      !----------------------------------------------------------------
      ! Define simulations "steering" variables (forcingyear, etc.)
      !----------------------------------------------------------------
      myinterface%steering = getsteering( yr, myinterface%params_siml )

      ! if (yr == myinterface%params_siml%spinupyears+1 ) then
      !   print*, '------------------TRANSIENT SIMULATION--------------------'
      ! endif

      !----------------------------------------------------------------
      ! Get external (environmental) forcing
      !----------------------------------------------------------------
      ! Get climate variables for this year (full fields and 365 daily values for each variable)
      myinterface%climate = getclimate(  &
                                          nt, &
                                          forcing, &
                                          myinterface%domaininfo, &
                                          myinterface%grid, &
                                          myinterface%steering%init, &
                                          myinterface%steering%climateyear_idx, &
                                          myinterface%params_siml%in_ppfd,  &
                                          myinterface%params_siml%in_netrad &
                                          )

      ! Get annual, gobally uniform CO2
      myinterface%pco2 = getco2(  &
                                  nt, &
                                  forcing, &
                                  myinterface%domaininfo, &
                                  myinterface%steering%forcingyear, &
                                  myinterface%params_siml%const_co2_year, &
                                  myinterface%params_siml%firstyeartrend &
                                  )

      !----------------------------------------------------------------
      ! Get prescribed fAPAR if required (otherwise set to dummy value)
      !----------------------------------------------------------------
      myinterface%vegcover = getfapar( &
                                        nt, &
                                        forcing, &
                                        myinterface%domaininfo, &
                                        myinterface%grid, &
                                        myinterface%steering%forcingyear_idx &
                                        )

      !----------------------------------------------------------------
      ! Call SR biosphere at an annual time step but with vectors 
      ! containing data for each day of this year.
      !----------------------------------------------------------------
      ! print*,'--------------------------------------------------------'
      ! print*,'Simulation year: ', myinterface%steering%year, ' - Real year: ', myinterface%steering%outyear
      ! print*,'--------------------------------------------------------'
      
      !----------------------------------------------------------------
      ! Call biosphere (wrapper for all modules, contains gridcell loop)
      !----------------------------------------------------------------
      out_biosphere = biosphere_annual() 
      !----------------------------------------------------------------

      !----------------------------------------------------------------
      ! Populate Fortran output array which is passed back to C/R
      !----------------------------------------------------------------
      if (yr > myinterface%params_siml%spinupyears ) then

        idx_start = (myinterface%steering%forcingyear_idx - 1) * ndayyear + 1
        idx_end   = idx_start + ndayyear - 1

        output(idx_start:idx_end,1) = dble(out_biosphere%fapar(:))  
        output(idx_start:idx_end,2) = dble(out_biosphere%gpp(:))    
        output(idx_start:idx_end,3) = dble(out_biosphere%transp(:)) 
        output(idx_start:idx_end,4) = dble(out_biosphere%latenth(:))
        output(idx_start:idx_end,5) = 0.d0

      end if

    enddo

    ! print*, '--------------------END OF SIMULATION---------------------'

    ! clean up
    deallocate(myinterface%fpc_grid)

  end subroutine pmodel_f


  subroutine lm3ppa_f(    &
    spinup,                    &   
    spinupyears,               &        
    recycle,                   &    
    firstyeartrend,            &           
    nyeartrend,                &       
    ! model_run_years,      &             
    equi_days,            &       
    outputhourly,         &          
    outputdaily,          &         
    do_U_shaped_mortality,&                   
    update_annaulLAImax,  &                 
    do_closedN_run,       &            
    longitude,            &      
    latitude,             &     
    altitude,             &     
    soiltype,             &      
    FLDCAP,               &    
    WILTPT,               &    
    K1,                   &
    K2,                   &
    K_nitrogen,           &        
    etaN,                 &  
    MLmixRatio,           &        
    l_fract,              &     
    retransN,             &      
    fNSNmax,              &     
    f_N_add,              &     
    f_initialBSW,         &          
    params_species,       &            
    params_soil,          &         
    init_cohort,          &         
    init_fast_soil_C,     &              
    init_slow_soil_C,     &              
    init_Nmineral,        &           
    N_input,              &     
    n,                    &    
    forcing,              &     
    output                &
    ) bind(C, n&ame = "lm3ppa_f_")

    !////////////////////////////////////////////////////////////////
    ! Main subroutine to handle I/O with C and R. 
    ! Receives simulation parameters, site parameters, and the full 
    ! simulation's forcing as time series
    ! test xxx
    !----------------------------------------------------------------
    use md_params_siml_lm3ppa, only: getsteering
    ! use md_grid, only: get_domaininfo, getgrid, type_params_domain
    use md_params_soil_lm3ppa, only: getsoil
    use md_forcing_lm3ppa, only: getclimate, getco2, getfapar, get_fpc_grid
    use md_interface_lm3ppa, only: interfacetype_biosphere, outtype_biosphere, myinterface
    use md_params_core_lm3ppa, only: n_dim_soil_types, MSPECIES, MAX_INIT_COHORTS
    use md_biosphere_lm3ppa, only: biosphere_annual

    implicit none

    ! Simulation parameters
    logical(kind=c_bool), intent(in) :: spinup
    integer(kind=c_int),  intent(in) :: spinupyears
    integer(kind=c_int),  intent(in) :: recycle
    integer(kind=c_int),  intent(in) :: firstyeartrend
    integer(kind=c_int),  intent(in) :: nyeartrend

    ! integer(kind=c_int),  intent(in) :: model_run_years
    integer(kind=c_int),  intent(in) :: equi_days
    logical(kind=c_bool), intent(in) :: outputhourly
    logical(kind=c_bool), intent(in) :: outputdaily
    logical(kind=c_bool), intent(in) :: do_U_shaped_mortality
    logical(kind=c_bool), intent(in) :: update_annaulLAImax
    logical(kind=c_bool), intent(in) :: do_closedN_run

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
    real(kind=c_double), intent(in) :: etaN
    real(kind=c_double), intent(in) :: MLmixRatio
    real(kind=c_double), intent(in) :: l_fract
    real(kind=c_double), intent(in) :: retransN
    real(kind=c_double), intent(in) :: fNSNmax
    real(kind=c_double), intent(in) :: f_N_add
    real(kind=c_double), intent(in) :: f_initialBSW

    ! naked arrays
    real(kind=c_double), dimension(MSPECIES,15), intent(in) :: params_species
    real(kind=c_double), dimension(n_dim_soil_types,8), intent(in) :: params_soil
    real(kind=c_double), dimension(MAX_INIT_COHORTS,5),  intent(in) :: init_cohort

    ! initial soil pool size
    real(kind=c_double), intent(in) :: init_fast_soil_C
    real(kind=c_double), intent(in) :: init_slow_soil_C
    real(kind=c_double), intent(in) :: init_Nmineral
    real(kind=c_double), intent(in) :: N_input

    integer(kind=c_int), intent(in) :: nt
    real(kind=c_double), dimension(nt,13), intent(in) :: forcing
    real(kind=c_double), intent(in) :: output

    ! local variables
    type(outtype_biosphere) :: out_biosphere  ! holds all the output used for calculating the cost or maximum likelihood function 
    type(type_params_domain) :: params_domain
    integer :: npft_local, yr, ncells, idx_start, idx_end


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
    ! myinterface%params_siml%model_run_years       = myinterface%params_siml%runyears    ! xxx delete model_run_years from arguments
    myinterface%params_siml%equi_days             = 0   ! to always write output; xxx todo: remove once output is passed back to R
    myinterface%params_siml%outputhourly          = outputhourly
    myinterface%params_siml%outputdaily           = outputdaily
    myinterface%params_siml%do_U_shaped_mortality = do_U_shaped_mortality
    myinterface%params_siml%update_annaulLAImax   = update_annaulLAImax      ! xxx actually not used at all. todo: remove from arguments etc.
    myinterface%params_siml%do_closedN_run        = do_closedN_run           ! xxx actually not used at all. todo: remove from arguments etc.

    ! Tile parameters
    myinterface%params_tile%soiltype     = soiltype
    myinterface%params_tile%FLDCAP       = FLDCAP
    myinterface%params_tile%WILTPT       = WILTPT
    myinterface%params_tile%K1           = K1
    myinterface%params_tile%K2           = K2
    myinterface%params_tile%K_nitrogen   = K_nitrogen
    myinterface%params_tile%etaN         = etaN
    myinterface%params_tile%MLmixRatio   = MLmixRatio
    myinterface%params_tile%l_fract      = l_fract
    myinterface%params_tile%retransN     = retransN
    myinterface%params_tile%fNSNmax      = fNSNmax
    myinterface%params_tile%f_N_add      = f_N_add
    myinterface%params_tile%f_initialBSW = f_initialBSW

    ! Species parameters
    myinterface%params_species%lifeform(:)     = params_species(:,1)
    myinterface%params_species%phenotype(:)    = params_species(:,2)
    myinterface%params_species%pt(:)           = params_species(:,3)
    myinterface%params_species%seedlingsize(:) = params_species(:,4)
    myinterface%params_species%LMA(:)          = params_species(:,5)
    myinterface%params_species%phiRL(:)        = params_species(:,6)
    myinterface%params_species%LNbase(:)       = params_species(:,7)
    myinterface%params_species%laimax(:)       = params_species(:,8)
    myinterface%params_species%LAI_light(:)    = params_species(:,9)
    myinterface%params_species%Nfixrate0(:)    = params_species(:,10)
    myinterface%params_species%NfixCost0(:)    = params_species(:,11)
    myinterface%params_species%phiCSA(:)       = params_species(:,12)
    myinterface%params_species%mortrate_d_c(:) = params_species(:,13)
    myinterface%params_species%mortrate_d_u(:) = params_species(:,14)
    myinterface%params_species%maturalage(:)   = params_species(:,15)

    ! Initial cohort sizes
    myinterface%init_cohort%init_cohort_species(:) = init_cohort(:,1)
    myinterface%init_cohort%init_cohort_nindivs(:) = init_cohort(:,2)
    myinterface%init_cohort%init_cohort_bsw(:)     = init_cohort(:,3)
    myinterface%init_cohort%init_cohort_bHW(:)     = init_cohort(:,4)
    myinterface%init_cohort%init_cohort_nsc(:)     = init_cohort(:,5)

    ! Initial soil pools
    myinterface%init_soil%init_fast_soil_C = init_fast_soil_C
    myinterface%init_soil%init_slow_soil_C = init_slow_soil_C
    myinterface%init_soil%init_Nmineral    = init_Nmineral
    myinterface%init_soil%N_input          = N_input

    
    ! myinterface%params_siml%soilmstress        = soilmstress
    ! myinterface%params_siml%tempstress         = tempstress
    ! myinterface%params_siml%calc_aet_fapar_vpd = calc_aet_fapar_vpd
    ! myinterface%params_siml%in_ppfd            = in_ppfd
    ! myinterface%params_siml%in_netrad          = in_netrad
    ! myinterface%params_siml%const_clim_year    = const_clim_year
    ! myinterface%params_siml%const_lu_year      = const_lu_year
    ! myinterface%params_siml%const_co2_year     = const_co2_year
    ! myinterface%params_siml%const_ndep_year    = const_ndep_year
    ! myinterface%params_siml%const_nfert_year   = const_nfert_year
    ! myinterface%params_siml%outdt              = outdt
    ! myinterface%params_siml%ltre               = ltre
    ! myinterface%params_siml%ltne               = ltne
    ! myinterface%params_siml%ltrd               = ltrd
    ! myinterface%params_siml%ltnd               = ltnd
    ! myinterface%params_siml%lgr3               = lgr3
    ! myinterface%params_siml%lgn3               = lgn3
    ! myinterface%params_siml%lgr4               = lgr4

    ! npft_local = 0
    ! if (myinterface%params_siml%ltre) npft_local = npft_local + 1
    ! if (myinterface%params_siml%ltne) npft_local = npft_local + 1
    ! if (myinterface%params_siml%ltrd) npft_local = npft_local + 1
    ! if (myinterface%params_siml%ltnd) npft_local = npft_local + 1
    ! if (myinterface%params_siml%lgr3) npft_local = npft_local + 1
    ! if (myinterface%params_siml%lgr4) npft_local = npft_local + 1
    ! if (myinterface%params_siml%lgn3) npft_local = npft_local + 1

    ! ! allocate variable size arrays
    ! if (.not. allocated(myinterface%fpc_grid)) allocate( myinterface%fpc_grid( npft_local ) )

    ! ! set parameter to define that this is not a calibration run (otherwise sofun.f90 would not have been compiled, but sofun_simsuite.f90)
    ! myinterface%params_siml%is_calib = .true.  ! treat paramters passed through R/C-interface the same way as calibratable parameters

    !----------------------------------------------------------------
    ! GET GRID INFORMATION
    !----------------------------------------------------------------
    myinterface%grid%lon = real( longitude )
    myinterface%grid%lat = real( latitude )
    myinterface%grid%elv = real( altitude )
    myinterface%grid%dogridcell = .true.  ! xxx todo remove all dogridcell statemetns
    myinterface%grid%landfrac = 1.0
    myinterface%grid%area     = 1.0

    !----------------------------------------------------------------
    ! GET SOIL PARAMETERS
    !----------------------------------------------------------------
    myinterface%soilparams = getsoil( params_soil )

    !----------------------------------------------------------------
    ! GET CALIBRATABLE MODEL PARAMETERS (so far a small list)
    !----------------------------------------------------------------
    ! XXX warning: converting from double to single may cause a problem
    ! when calibrating and parameters are varied in their Nth digit after
    ! the comma!  
    ! myinterface%params_calib%kphio = real(par(1))

    ! !----------------------------------------------------------------
    ! ! GET VEGETATION COVER (fractional projective cover by PFT)
    ! !----------------------------------------------------------------
    ! myinterface%fpc_grid(:) = get_fpc_grid( myinterface%domaininfo, myinterface%params_siml )


    ! LOOP THROUGH YEARS
    ! print*, '-------------------START OF SIMULATION--------------------'


    do yr=1,myinterface%params_siml%runyears

      !----------------------------------------------------------------
      ! Define simulations "steering" variables (forcingyear, etc.)
      !----------------------------------------------------------------
      myinterface%steering = getsteering( yr, myinterface%params_siml )

      ! if (yr == myinterface%params_siml%spinupyears+1 ) then
      !   print*, '------------------TRANSIENT SIMULATION--------------------'
      ! endif


      !----------------------------------------------------------------
      ! Get external (environmental) forcing
      !----------------------------------------------------------------
      ! Get climate variables for this year (full fields and 365 daily values for each variable)
      myinterface%climate = getclimate( &
                                        nt, &
                                        forcing, &
                                        myinterface%steering%climateyear_idx, &
                                        myinterface%steering%climateyear &
                                        )

      ! Get annual, gobally uniform CO2
      myinterface%pco2 = getco2(  &
                                  nt, &
                                  forcing, &
                                  myinterface%steering%climateyear_idx, &  ! to make it equivalent to BiomeE
                                  myinterface%steering%climateyear &
                                  ! myinterface%steering%forcingyear_idx, &
                                  ! myinterface%steering%forcingyear &
                                  )


      !----------------------------------------------------------------
      ! Call SR biosphere at an annual time step but with vectors 
      ! containing data for each day of this year.
      !----------------------------------------------------------------
      ! print*,'--------------------------------------------------------'
      ! print*,'Simulation year: ', myinterface%steering%year, ' - Real year: ', myinterface%steering%outyear
      ! print*,'--------------------------------------------------------'
      
      !----------------------------------------------------------------
      ! Call biosphere (wrapper for all modules, contains gridcell loop)
      !----------------------------------------------------------------
      out_biosphere = biosphere_annual() 
      !----------------------------------------------------------------

      !----------------------------------------------------------------
      ! Populate Fortran output array which is passed back to C/R
      !----------------------------------------------------------------
      if (yr > myinterface%params_siml%spinupyears ) then

        idx_start = (myinterface%steering%forcingyear_idx - 1) * ndayyear + 1
        idx_end   = idx_start + ndayyear - 1

        output(idx_start:idx_end,1) = dble(out_biosphere%fapar(:))  
        output(idx_start:idx_end,2) = dble(out_biosphere%gpp(:))    
        output(idx_start:idx_end,3) = dble(out_biosphere%transp(:)) 
        output(idx_start:idx_end,4) = dble(out_biosphere%latenth(:))
        output(idx_start:idx_end,5) = 0.d0

      end if

    enddo

    ! print*, '--------------------END OF SIMULATION---------------------'

    ! clean up
    ! deallocate(myinterface%fpc_grid)

  end subroutine lm3ppa_f

end module sofun_r_mod
