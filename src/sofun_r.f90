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
    !if (.not. allocated(myinterface%fpc_grid)) allocate( myinterface%fpc_grid( npft_local ) )

    ! set parameter to define that this is not a calibration run (otherwise sofun.f90 would not have been compiled, but sofun_simsuite.f90)
    myinterface%params_siml%is_calib = .true.  ! treat paramters passed through R/C-interface the same way as calibratable parameters

    !----------------------------------------------------------------
    ! GET GRID INFORMATION
    !----------------------------------------------------------------
    myinterface%grid%lon = real( longitude )
    myinterface%grid%lat = real( latitude )
    myinterface%grid%elv = real( altitude )

    !----------------------------------------------------------------
    ! GET SOIL PARAMETERS
    !----------------------------------------------------------------
    myinterface%soilparams = getsoil( soiltexture )

    ! Overwrite whc
    myinterface%soilparams%whc = whc

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
    myinterface%fpc_grid(:) = get_fpc_grid( myinterface%params_siml )


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
                                          myinterface%steering%init, &
                                          myinterface%steering%climateyear_idx, &
                                          myinterface%params_siml%in_ppfd,  &
                                          myinterface%params_siml%in_netrad &
                                          )

      ! Get annual, gobally uniform CO2
      myinterface%pco2 = getco2(  &
                                  nt, &
                                  forcing, &
                                  myinterface%steering%forcingyear, &
                                  myinterface%params_siml%firstyeartrend &
                                  )

      !----------------------------------------------------------------
      ! Get prescribed fAPAR if required (otherwise set to dummy value)
      !----------------------------------------------------------------
      myinterface%vegcover = getfapar( &
                                        nt, &
                                        forcing, &
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
    !deallocate(myinterface%fpc_grid)

  end subroutine pmodel_f


  subroutine lm3ppa_f(    &
    spinup,               &   
    spinupyears,          &        
    recycle,              &    
    firstyeartrend,       &           
    nyeartrend,           &       
    outputhourly,         &          
    outputdaily,          &         
    do_U_shaped_mortality,&                   
    update_annualLAImax,  &                 
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
    f_N_add,              &     
    f_initialBSW,         &          
    params_species,       &            
    params_soil,          &         
    init_cohort,          &         
    init_fast_soil_C,     &              
    init_slow_soil_C,     &              
    init_Nmineral,        &           
    N_input,              &     
    nt,                   &  
    nt_daily,             &    
    nt_annual,            &    
    forcing,              &     
    output_hourly_tile,   &
    output_daily_tile,    &
    output_daily_cohorts, &
    output_annual_tile,   &
    output_annual_cohorts &
    ) bind(C, name = "lm3ppa_f_")

    !////////////////////////////////////////////////////////////////
    ! Main subroutine to handle I/O with C and R. 
    ! Receives simulation parameters, site parameters, and the full 
    ! simulation's forcing as time series
    ! test xxx
    !----------------------------------------------------------------
    use md_params_siml_lm3ppa, only: getsteering
    use md_params_soil_lm3ppa, only: getsoil
    use md_forcing_lm3ppa, only: getclimate, getco2, climate_type, forcingData
    use md_interface_lm3ppa, only: interfacetype_biosphere, outtype_biosphere, myinterface
    use md_params_core_lm3ppa, only: n_dim_soil_types, MSPECIES, MAX_INIT_COHORTS, ntstepsyear, out_max_cohorts, &
      ndayyear, nvars_daily_tile, nvars_hourly_tile, nvars_daily_cohorts, nvars_annual_cohorts, nvars_annual_tile
    use md_biosphere_lm3ppa, only: biosphere_annual

    implicit none

    ! Simulation parameters
    logical(kind=c_bool), intent(in) :: spinup
    integer(kind=c_int),  intent(in) :: spinupyears
    integer(kind=c_int),  intent(in) :: recycle
    integer(kind=c_int),  intent(in) :: firstyeartrend
    integer(kind=c_int),  intent(in) :: nyeartrend

    logical(kind=c_bool), intent(in) :: outputhourly
    logical(kind=c_bool), intent(in) :: outputdaily
    logical(kind=c_bool), intent(in) :: do_U_shaped_mortality
    logical(kind=c_bool), intent(in) :: update_annualLAImax
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
    real(kind=c_double), intent(in) :: f_N_add
    real(kind=c_double), intent(in) :: f_initialBSW

    ! naked arrays
    real(kind=c_double), dimension(0:MSPECIES,16), intent(in) :: params_species
    real(kind=c_double), dimension(n_dim_soil_types,8), intent(in) :: params_soil
    real(kind=c_double), dimension(MAX_INIT_COHORTS,5),  intent(in) :: init_cohort

    ! initial soil pool size
    real(kind=c_double), intent(in) :: init_fast_soil_C
    real(kind=c_double), intent(in) :: init_slow_soil_C
    real(kind=c_double), intent(in) :: init_Nmineral
    real(kind=c_double), intent(in) :: N_input

    integer(kind=c_int), intent(in) :: nt
    integer(kind=c_int), intent(in) :: nt_daily
    integer(kind=c_int), intent(in) :: nt_annual

   ! output arrays (naked) to be passed back to C/R
    real(kind=c_double), dimension(nt,13), intent(in) :: forcing
    real(kind=c_double), dimension(nt,15), intent(out) :: output_hourly_tile ! nvars_hourly_tile = 15
    real(kind=c_double), dimension(nt_daily,35), intent(out) :: output_daily_tile ! nvars_daily_tile = 35
    real(kind=c_double), dimension(nt_daily,out_max_cohorts,27), intent(out) :: output_daily_cohorts !nvars_daily_cohorts = 27
    real(kind=c_double), dimension(nt_annual,44), intent(out) :: output_annual_tile ! nvars_annual_tile = 44
    real(kind=c_double), dimension(nt_annual,out_max_cohorts,23), intent(out) :: output_annual_cohorts ! nvars_annual_cohorts + 23

    ! local variables
    !integer(kind=c_int), intent(in) :: datalines
    integer(kind=c_int)     :: datalines
    integer(kind=c_int)     :: yr_data
    integer(kind=c_int)     :: totyears
    integer(kind=c_int)     :: totdays
    integer(kind=c_int)     :: days_data
    real(kind=c_double)     :: timestep
    type(outtype_biosphere) :: out_biosphere  ! holds all the output used for calculating the cost or maximum likelihood function 
    integer(kind=c_int)     :: npft_local, yr, iday, ncells, idx_start, idx_end
    
    integer(kind=c_int) :: idx_hourly_start
    integer(kind=c_int) :: idx_hourly_end
    integer(kind=c_int) :: idx_daily_start
    integer(kind=c_int) :: idx_daily_end


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
    myinterface%params_siml%update_annualLAImax   = update_annualLAImax      ! xxx actually not used at all. todo: remove from arguments etc.
    myinterface%params_siml%do_closedN_run        = do_closedN_run           ! xxx actually not used at all. todo: remove from arguments etc.

    ! Tile parameters
    myinterface%params_tile%soiltype     = int(soiltype)
    myinterface%params_tile%FLDCAP       = real( FLDCAP )
    myinterface%params_tile%WILTPT       = real( WILTPT )
    myinterface%params_tile%K1           = real( K1 )
    myinterface%params_tile%K2           = real( K2 )
    myinterface%params_tile%K_nitrogen   = real( K_nitrogen )
    myinterface%params_tile%etaN         = real( etaN )
    myinterface%params_tile%MLmixRatio   = real( MLmixRatio )
    myinterface%params_tile%l_fract      = real( l_fract )
    myinterface%params_tile%retransN     = real( retransN )
    myinterface%params_tile%f_initialBSW = real( f_initialBSW )
    myinterface%params_tile%f_N_add      = real( f_N_add )

    ! Species parameters
    myinterface%params_species(:)%lifeform     = real( params_species(:,1) )
    myinterface%params_species(:)%phenotype    = real( params_species(:,2) )
    myinterface%params_species(:)%pt           = real( params_species(:,3) )
    myinterface%params_species(:)%seedlingsize = real( params_species(:,4) )
    myinterface%params_species(:)%LMA          = real( params_species(:,5) )
    myinterface%params_species(:)%phiRL        = real( params_species(:,6) )
    myinterface%params_species(:)%LNbase       = real( params_species(:,7) )
    myinterface%params_species(:)%laimax       = real( params_species(:,8) )
    myinterface%params_species(:)%LAI_light    = real( params_species(:,9) )
    myinterface%params_species(:)%Nfixrate0    = real( params_species(:,10) )
    myinterface%params_species(:)%NfixCost0    = real( params_species(:,11) )
    myinterface%params_species(:)%phiCSA       = real( params_species(:,12) )
    myinterface%params_species(:)%mortrate_d_c = real( params_species(:,13) )
    myinterface%params_species(:)%mortrate_d_u = real( params_species(:,14) )
    myinterface%params_species(:)%maturalage   = real( params_species(:,15) )
    myinterface%params_species(:)%fNSNmax      = real( params_species(:,16) )

    ! Initial cohort sizes
    myinterface%init_cohort(:)%init_cohort_species = real(init_cohort(:,1))
    myinterface%init_cohort(:)%init_cohort_nindivs = real(init_cohort(:,2))
    myinterface%init_cohort(:)%init_cohort_bsw     = real(init_cohort(:,3))
    myinterface%init_cohort(:)%init_cohort_bHW     = real(init_cohort(:,4))
    myinterface%init_cohort(:)%init_cohort_nsc     = real(init_cohort(:,5))

    ! Initial soil pools
    myinterface%init_soil%init_fast_soil_C = real( init_fast_soil_C )
    myinterface%init_soil%init_slow_soil_C = real( init_slow_soil_C )
    myinterface%init_soil%init_Nmineral    = real( init_Nmineral )
    myinterface%init_soil%N_input          = real( N_input )

    
    !----------------------------------------------------------------
    ! GET GRID INFORMATION
    !----------------------------------------------------------------
    myinterface%grid%lon = real( longitude )
    myinterface%grid%lat = real( latitude )
    myinterface%grid%elv = real( altitude )

    !----------------------------------------------------------------
    ! GET SOIL PARAMETERS
    !----------------------------------------------------------------
    myinterface%params_soil = getsoil( params_soil )

    myinterface%steps_per_day = int(24.0/timestep)
    myinterface%dt_fast_yr = 1.0/(365.0 * myinterface%steps_per_day)
    myinterface%step_seconds = 24.0*3600.0/myinterface%steps_per_day ! seconds_per_year * dt_fast_yr
    ntstepsyear = myinterface%steps_per_day * 365
    !print*,'ntstepsyear ', ntstepsyear
    write(*,*) myinterface%steps_per_day, myinterface%dt_fast_yr, myinterface%step_seconds
  
    totyears = myinterface%params_siml%runyears
    totdays  = int(totyears/yr_data+1)*days_data
    myinterface%datalines = datalines

    allocate(myinterface%climate(ntstepsyear))
    allocate(myinterface%pco2(ntstepsyear))
    allocate(out_biosphere%hourly_tile(ntstepsyear))

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
      ! if (yr > myinterface%params_siml%spinupyears ) then

      !   idx_start = (myinterface%steering%forcingyear_idx - 1) * ndayyear + 1
      !   idx_end   = idx_start + ndayyear - 1

      !   output(idx_start:idx_end,1) = dble(out_biosphere%fapar(:))  
      !   output(idx_start:idx_end,2) = dble(out_biosphere%gpp(:))    
      !   output(idx_start:idx_end,3) = dble(out_biosphere%transp(:)) 
      !   output(idx_start:idx_end,4) = dble(out_biosphere%latenth(:))
      !   output(idx_start:idx_end,5) = 0.d0

      ! end if

    enddo

    deallocate(myinterface%climate)
    deallocate(myinterface%pco2)

    ! print*, '--------------------END OF SIMULATION---------------------'


  end subroutine lm3ppa_f

  subroutine populate_outarray_hourly_tile( hourly_tile, out_hourly_tile ) !, idx_daily_start, idx_daily_end

    use md_interface_lm3ppa, only: outtype_hourly_tile
    use md_params_core_lm3ppa

    ! arguments
    type(outtype_hourly_tile), dimension(ntstepsyear), intent(in) :: hourly_tile    ! dimension(ntstepsyear)
    real, dimension(ntstepsyear, nvars_hourly_tile), intent(inout) :: out_hourly_tile

    out_hourly_tile(:, 1)  = hourly_tile(:)%year
    out_hourly_tile(:, 2)  = hourly_tile(:)%doy
    out_hourly_tile(:, 3)  = hourly_tile(:)%hour
    out_hourly_tile(:, 4)  = hourly_tile(:)%rad
    out_hourly_tile(:, 5)  = hourly_tile(:)%Tair
    out_hourly_tile(:, 6)  = hourly_tile(:)%Prcp
    out_hourly_tile(:, 7)  = hourly_tile(:)%GPP
    out_hourly_tile(:, 8)  = hourly_tile(:)%Resp
    out_hourly_tile(:, 9)  = hourly_tile(:)%Transp
    out_hourly_tile(:, 10) = hourly_tile(:)%Evap
    out_hourly_tile(:, 11) = hourly_tile(:)%Runoff
    out_hourly_tile(:, 12) = hourly_tile(:)%Soilwater
    out_hourly_tile(:, 13) = hourly_tile(:)%wcl
    out_hourly_tile(:, 14) = hourly_tile(:)%FLDCAP
    out_hourly_tile(:, 15) = hourly_tile(:)%WILTPT

  end subroutine populate_outarray_hourly_tile

  subroutine populate_outarray_daily_tile( daily_tile, out_daily_tile ) !, idx_daily_start, idx_daily_end

    use md_interface_lm3ppa, only: outtype_daily_tile
    use md_params_core_lm3ppa

    ! arguments
    type(outtype_daily_tile), dimension(ndayyear), intent(in) :: daily_tile
    ! integer, intent(in) :: idx_daily_start, idx_daily_end
    real, dimension(ndayyear, nvars_daily_tile), intent(inout) :: out_daily_tile

    out_daily_tile(:, 1)  = daily_tile(:)%year 
    out_daily_tile(:, 2)  = daily_tile(:)%doy
    out_daily_tile(:, 3)  = daily_tile(:)%Tc
    out_daily_tile(:, 4)  = daily_tile(:)%Prcp
    out_daily_tile(:, 5)  = daily_tile(:)%totWs
    out_daily_tile(:, 6)  = daily_tile(:)%Trsp
    out_daily_tile(:, 7)  = daily_tile(:)%Evap
    out_daily_tile(:, 8)  = daily_tile(:)%Runoff
    out_daily_tile(:, 9)  = daily_tile(:)%ws1
    out_daily_tile(:, 10) = daily_tile(:)%ws2
    out_daily_tile(:, 11) = daily_tile(:)%ws3
    out_daily_tile(:, 12) = daily_tile(:)%LAI
    out_daily_tile(:, 13) = daily_tile(:)%GPP
    out_daily_tile(:, 14) = daily_tile(:)%Rauto
    out_daily_tile(:, 15) = daily_tile(:)%Rh
    out_daily_tile(:, 16) = daily_tile(:)%NSC
    out_daily_tile(:, 17) = daily_tile(:)%seedC
    out_daily_tile(:, 18) = daily_tile(:)%leafC
    out_daily_tile(:, 19) = daily_tile(:)%rootC
    out_daily_tile(:, 20) = daily_tile(:)%SW_C
    out_daily_tile(:, 21) = daily_tile(:)%HW_C
    out_daily_tile(:, 22) = daily_tile(:)%NSN
    out_daily_tile(:, 23) = daily_tile(:)%seedN
    out_daily_tile(:, 24) = daily_tile(:)%leafN
    out_daily_tile(:, 25) = daily_tile(:)%rootN
    out_daily_tile(:, 26) = daily_tile(:)%SW_N
    out_daily_tile(:, 27) = daily_tile(:)%HW_N
    out_daily_tile(:, 28) = daily_tile(:)%McrbC
    out_daily_tile(:, 29) = daily_tile(:)%fastSOM
    out_daily_tile(:, 30) = daily_tile(:)%slowSOM
    out_daily_tile(:, 31) = daily_tile(:)%McrbN
    out_daily_tile(:, 32) = daily_tile(:)%fastSoilN
    out_daily_tile(:, 33) = daily_tile(:)%slowSoilN
    out_daily_tile(:, 34) = daily_tile(:)%mineralN
    out_daily_tile(:, 35) = daily_tile(:)%N_uptk

  end subroutine populate_outarray_daily_tile


  subroutine populate_outarray_daily_cohorts( daily_cohorts, out_daily_cohorts ) 

    use md_interface_lm3ppa, only: outtype_daily_cohorts
    use md_params_core_lm3ppa

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


  subroutine populate_outarray_annual_tile( annual_tile, out_annual_tile )

    use md_interface_lm3ppa, only: outtype_annual_tile
    use md_params_core_lm3ppa

    ! arguments
    type(outtype_annual_tile), intent(in) :: annual_tile
    real, dimension(nvars_annual_tile), intent(inout) :: out_annual_tile

    out_annual_tile(1)  = annual_tile%year
    out_annual_tile(2)  = annual_tile%CAI
    out_annual_tile(3)  = annual_tile%LAI
    out_annual_tile(4)  = annual_tile%GPP
    out_annual_tile(5)  = annual_tile%Rauto
    out_annual_tile(6)  = annual_tile%Rh
    out_annual_tile(7)  = annual_tile%rain
    out_annual_tile(8)  = annual_tile%SoilWater
    out_annual_tile(9)  = annual_tile%Transp
    out_annual_tile(10) = annual_tile%Evap
    out_annual_tile(11) = annual_tile%Runoff
    out_annual_tile(12) = annual_tile%plantC
    out_annual_tile(13) = annual_tile%soilC
    out_annual_tile(14) = annual_tile%plantN
    out_annual_tile(15) = annual_tile%soilN
    out_annual_tile(16) = annual_tile%totN
    out_annual_tile(17) = annual_tile%NSC
    out_annual_tile(18) = annual_tile%SeedC
    out_annual_tile(19) = annual_tile%leafC
    out_annual_tile(20) = annual_tile%rootC
    out_annual_tile(21) = annual_tile%SapwoodC
    out_annual_tile(22) = annual_tile%WoodC
    out_annual_tile(23) = annual_tile%NSN
    out_annual_tile(24) = annual_tile%SeedN
    out_annual_tile(25) = annual_tile%leafN
    out_annual_tile(26) = annual_tile%rootN
    out_annual_tile(27) = annual_tile%SapwoodN
    out_annual_tile(28) = annual_tile%WoodN
    out_annual_tile(29) = annual_tile%McrbC
    out_annual_tile(30) = annual_tile%fastSOM
    out_annual_tile(31) = annual_tile%SlowSOM
    out_annual_tile(32) = annual_tile%McrbN
    out_annual_tile(33) = annual_tile%fastSoilN
    out_annual_tile(34) = annual_tile%slowSoilN
    out_annual_tile(35) = annual_tile%mineralN
    out_annual_tile(36) = annual_tile%N_fxed
    out_annual_tile(37) = annual_tile%N_uptk
    out_annual_tile(38) = annual_tile%N_yrMin
    out_annual_tile(39) = annual_tile%N_P2S
    out_annual_tile(40) = annual_tile%N_loss
    out_annual_tile(41) = annual_tile%totseedC
    out_annual_tile(42) = annual_tile%totseedN
    out_annual_tile(43) = annual_tile%Seedling_C
    out_annual_tile(44) = annual_tile%Seedling_N

  end subroutine populate_outarray_annual_tile


  subroutine populate_outarray_annual_cohorts( annual_cohorts, out_annual_cohorts ) 

    use md_interface_lm3ppa, only: outtype_annual_cohorts
    use md_params_core_lm3ppa

    ! arguments
    type(outtype_annual_cohorts), dimension(out_max_cohorts), intent(in) :: annual_cohorts
    real, dimension(out_max_cohorts,nvars_annual_cohorts), intent(inout) :: out_annual_cohorts    

    out_annual_cohorts(:, 1)  = annual_cohorts(:)%year
    out_annual_cohorts(:, 2)  = annual_cohorts(:)%cID
    out_annual_cohorts(:, 3)  = annual_cohorts(:)%PFT
    out_annual_cohorts(:, 4)  = annual_cohorts(:)%layer
    out_annual_cohorts(:, 5)  = annual_cohorts(:)%density
    out_annual_cohorts(:, 6)  = annual_cohorts(:)%f_layer
    out_annual_cohorts(:, 7)  = annual_cohorts(:)%dDBH
    out_annual_cohorts(:, 8)  = annual_cohorts(:)%dbh
    out_annual_cohorts(:, 9)  = annual_cohorts(:)%height
    out_annual_cohorts(:, 10) = annual_cohorts(:)%Acrown
    out_annual_cohorts(:, 11) = annual_cohorts(:)%wood
    out_annual_cohorts(:, 12) = annual_cohorts(:)%nsc
    out_annual_cohorts(:, 13) = annual_cohorts(:)%NSN
    out_annual_cohorts(:, 14) = annual_cohorts(:)%NPPtr
    out_annual_cohorts(:, 15) = annual_cohorts(:)%seed
    out_annual_cohorts(:, 16) = annual_cohorts(:)%NPPL
    out_annual_cohorts(:, 17) = annual_cohorts(:)%NPPR
    out_annual_cohorts(:, 19) = annual_cohorts(:)%NPPW
    out_annual_cohorts(:, 20) = annual_cohorts(:)%GPP
    out_annual_cohorts(:, 21) = annual_cohorts(:)%NPP
    out_annual_cohorts(:, 22) = annual_cohorts(:)%N_uptk
    out_annual_cohorts(:, 23) = annual_cohorts(:)%N_fix
    out_annual_cohorts(:, 24) = annual_cohorts(:)%maxLAI

  end subroutine populate_outarray_annual_cohorts

end module sofun_r_mod
