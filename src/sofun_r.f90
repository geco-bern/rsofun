module sofun_r_mod
  !////////////////////////////////////////////////////////////////
  ! Contains main subroutine handling I/O with C and R.
  !----------------------------------------------------------------
  use, intrinsic :: iso_c_binding, only: c_double, c_int, c_char, c_bool

  implicit none

  private
  public :: pmodel_f, biomee_f

contains

  subroutine pmodel_f(         &
    spinup,                    &   
    spinupyears,               &        
    recycle,                   &    
    firstyeartrend,            &           
    nyeartrend,                &  
    secs_per_tstep,            &     
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
    use md_forcing_pmodel, only: getclimate, getco2, getfapar, get_fpc_grid
    use md_interface_pmodel, only: interfacetype_biosphere, outtype_biosphere, myinterface
    use md_params_core
    use md_biosphere_pmodel, only: biosphere_annual

    implicit none

    ! arguments
    integer(kind=c_int),  intent(in) :: spinup         ! logical type is not supported in the C interface (LTO)
    integer(kind=c_int),  intent(in) :: spinupyears
    integer(kind=c_int),  intent(in) :: recycle
    integer(kind=c_int),  intent(in) :: firstyeartrend
    integer(kind=c_int),  intent(in) :: nyeartrend
    integer(kind=c_int),  intent(in) :: secs_per_tstep
    integer(kind=c_int),  intent(in) :: in_ppfd        ! logical
    integer(kind=c_int),  intent(in) :: in_netrad      ! logical
    integer(kind=c_int),  intent(in) :: outdt
    integer(kind=c_int), intent(in) :: ltre            ! logical
    integer(kind=c_int), intent(in) :: ltne            ! logical
    integer(kind=c_int), intent(in) :: ltrd            ! logical
    integer(kind=c_int), intent(in) :: ltnd            ! logical
    integer(kind=c_int), intent(in) :: lgr3            ! logical
    integer(kind=c_int), intent(in) :: lgn3            ! logical
    integer(kind=c_int), intent(in) :: lgr4            ! logical
    real(kind=c_double),  intent(in) :: longitude
    real(kind=c_double),  intent(in) :: latitude
    real(kind=c_double),  intent(in) :: altitude
    real(kind=c_double),  intent(in) :: whc
    integer(kind=c_int),  intent(in) :: nt ! number of time steps
    real(kind=c_double),  dimension(9), intent(in) :: par  ! free (calibratable) model parameters
    real(kind=c_double),  dimension(nt,12), intent(in) :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=fapar, 10=patm, 11=tmin, 12=tmax) 
    real(kind=c_double),  dimension(nt,19), intent(out) :: output

    ! local variables
    type(outtype_biosphere) :: out_biosphere  ! holds all the output used for calculating the cost or maximum likelihood function 
    integer :: yr, idx_start, idx_end ! npft_local

    !----------------------------------------------------------------
    ! GET SIMULATION PARAMETERS
    !----------------------------------------------------------------
    myinterface%params_siml%steering%do_spinup      = spinup /= 0
    myinterface%params_siml%steering%spinupyears    = spinupyears
    myinterface%params_siml%steering%recycle        = recycle
    myinterface%params_siml%steering%firstyeartrend = firstyeartrend
    myinterface%params_siml%steering%nyeartrend     = nyeartrend

    if (myinterface%params_siml%steering%do_spinup) then
      myinterface%params_siml%steering%runyears = myinterface%params_siml%steering%nyeartrend &
              + myinterface%params_siml%steering%spinupyears
    else
      myinterface%params_siml%steering%runyears = myinterface%params_siml%steering%nyeartrend
      myinterface%params_siml%steering%spinupyears = 0
    endif
    
    myinterface%params_siml%in_ppfd            = in_ppfd /= 0
    myinterface%params_siml%in_netrad          = in_netrad /= 0
    myinterface%params_siml%outdt              = outdt
    myinterface%params_siml%ltre               = ltre /= 0
    myinterface%params_siml%ltne               = ltne /= 0
    myinterface%params_siml%ltrd               = ltrd /= 0
    myinterface%params_siml%ltnd               = ltnd /= 0
    myinterface%params_siml%lgr3               = lgr3 /= 0
    myinterface%params_siml%lgn3               = lgn3 /= 0
    myinterface%params_siml%lgr4               = lgr4 /= 0
    myinterface%params_siml%secs_per_tstep     = secs_per_tstep

    !----------------------------------------------------------------
    ! GET GRID INFORMATION
    !----------------------------------------------------------------
    myinterface%grid%lon = real( longitude )
    myinterface%grid%lat = real( latitude )
    myinterface%grid%elv = real( altitude )

    !----------------------------------------------------------------
    ! GET SOIL PARAMETERS
    !----------------------------------------------------------------
    myinterface%whc_prescr = real( whc )
    
    !----------------------------------------------------------------
    ! GET CALIBRATABLE MODEL PARAMETERS (so far a small list)
    !----------------------------------------------------------------
    myinterface%params_calib%kphio              = real(par(1))
    myinterface%params_calib%kphio_par_a        = real(par(2))
    myinterface%params_calib%kphio_par_b        = real(par(3))
    myinterface%params_calib%soilm_thetastar    = real(par(4))
    myinterface%params_calib%soilm_betao        = real(par(5))
    myinterface%params_calib%beta_unitcostratio = real(par(6))
    myinterface%params_calib%rd_to_vcmax        = real(par(7))
    myinterface%params_calib%tau_acclim         = real(par(8))
    myinterface%params_calib%kc_jmax            = real(par(9))

    !----------------------------------------------------------------
    ! GET VEGETATION COVER (fractional projective cover by PFT)
    !----------------------------------------------------------------
    myinterface%fpc_grid(:) = get_fpc_grid( myinterface%params_siml )
    
    do yr=1,myinterface%params_siml%steering%runyears

      !----------------------------------------------------------------
      ! Define simulations "steering" variables (forcingyear, etc.)
      !----------------------------------------------------------------
      myinterface%steering = get_steering( yr, myinterface%params_siml%steering )

      !----------------------------------------------------------------
      ! Get external (environmental) forcing
      !----------------------------------------------------------------
      ! Get climate variables for this year (full fields and 365 daily values for each variable)
      myinterface%climate(:) = getclimate(nt, &
                                          forcing, &
                                          myinterface%steering%climateyear_idx, &
                                          myinterface%params_siml%in_ppfd,  &
                                          myinterface%params_siml%in_netrad &
                                          )

      ! Get annual, gobally uniform CO2
      myinterface%pco2 = getco2(  nt, &
                                  forcing, &
                                  myinterface%steering%forcingyear, &
                                  myinterface%params_siml%steering%firstyeartrend &
                                  )

      !----------------------------------------------------------------
      ! Get prescribed fAPAR if required (otherwise set to dummy value)
      !----------------------------------------------------------------
      myinterface%vegcover(:) = getfapar( &
                                        nt, &
                                        forcing, &
                                        myinterface%steering%forcingyear_idx &
                                        )

      !----------------------------------------------------------------
      ! Call biosphere (wrapper for all modules, contains gridcell loop)
      !----------------------------------------------------------------
      out_biosphere = biosphere_annual() 
      !----------------------------------------------------------------

      !----------------------------------------------------------------
      ! Populate Fortran output array which is passed back to C/R
      !----------------------------------------------------------------
      if (yr > myinterface%params_siml%steering%spinupyears ) then

        idx_start = (myinterface%steering%forcingyear_idx - 1) * ndayyear + 1
        idx_end   = idx_start + ndayyear - 1

        output(idx_start:idx_end,1)  = dble(out_biosphere%fapar(:))  
        output(idx_start:idx_end,2)  = dble(out_biosphere%gpp(:))    
        output(idx_start:idx_end,3)  = dble(out_biosphere%transp(:)) 
        output(idx_start:idx_end,4)  = dble(out_biosphere%latenth(:))
        output(idx_start:idx_end,5)  = dble(out_biosphere%pet(:))
        output(idx_start:idx_end,6)  = dble(out_biosphere%vcmax(:))  
        output(idx_start:idx_end,7)  = dble(out_biosphere%jmax(:))    
        output(idx_start:idx_end,8)  = dble(out_biosphere%vcmax25(:)) 
        output(idx_start:idx_end,9)  = dble(out_biosphere%jmax25(:))
        output(idx_start:idx_end,10) = dble(out_biosphere%gs_accl(:))
        output(idx_start:idx_end,11) = dble(out_biosphere%wscal(:))
        output(idx_start:idx_end,12) = dble(out_biosphere%chi(:))
        output(idx_start:idx_end,13) = dble(out_biosphere%iwue(:))
        output(idx_start:idx_end,14) = dble(out_biosphere%rd(:))
        output(idx_start:idx_end,15) = dble(out_biosphere%tsoil(:))
        output(idx_start:idx_end,16) = dble(out_biosphere%netrad(:))
        output(idx_start:idx_end,17) = dble(out_biosphere%wcont(:))
        output(idx_start:idx_end,18) = dble(out_biosphere%snow(:))
        output(idx_start:idx_end,19) = dble(out_biosphere%cond(:))

      end if

    enddo

  end subroutine pmodel_f

  !//////////////////////////////////////////////////////////////////////////

  subroutine biomee_f(            &
    spinup,                       &   
    spinupyears,                  &        
    recycle,                      &    
    firstyeartrend,               &           
    nyeartrend,                   &
    do_U_shaped_mortality,        &                   
    update_annualLAImax,          &                 
    do_closedN_run,               &
    code_method_photosynth,       &
    code_method_mortality,        &             
    longitude,                    &      
    latitude,                     &     
    altitude,                     &             
    soiltype,                     &      
    FLDCAP,                       &    
    WILTPT,                       &    
    K1,                           &
    K2,                           &
    K_nitrogen,                   &        
    MLmixRatio,                   &  
    etaN,                         &        
    LMAmin,                       &     
    fsc_fine,                     &      
    fsc_wood,                     &     
    GR_factor,                    & 
    l_fract,                      & 
    retransN,                     & 
    f_initialBSW,                 & 
    f_N_add,                      & 
    tf_base,                      &     
    par_mort,                     &
    par_mort_under,               &
    n_params_species,             &
    params_species,               &
    n_init_cohort,                &
    init_cohort,                  &
    init_fast_soil_C,             &
    init_slow_soil_C,             &              
    init_Nmineral,                &           
    N_input,                      &     
    nt,                           &  
    nt_daily,                     &    
    nt_annual,                    &    
    nt_annual_cohorts,            &    
    forcing,                      &     
    steps_per_day,                &
    output_daily_tile,            &
    output_annual_tile,           &
    output_annual_cohorts_year,   &
    output_annual_cohorts_cID,    &
    output_annual_cohorts_PFT,    &
    output_annual_cohorts_layer,  &
    output_annual_cohorts_density,&
    output_annual_cohorts_flayer, &
    output_annual_cohorts_DBH,    &
    output_annual_cohorts_dDBH,   &
    output_annual_cohorts_height, &
    output_annual_cohorts_age,    &
    output_annual_cohorts_BA,     &
    output_annual_cohorts_dBA,    &
    output_annual_cohorts_Acrown, &
    output_annual_cohorts_Aleaf,  &
    output_annual_cohorts_nsc,    &
    output_annual_cohorts_nsn,    &
    output_annual_cohorts_seedC,  &
    output_annual_cohorts_leafC,  &
    output_annual_cohorts_rootC,  &
    output_annual_cohorts_sapwC,  &
    output_annual_cohorts_woodC,  &
    output_annual_cohorts_treeG,  &
    output_annual_cohorts_fseed,  &
    output_annual_cohorts_fleaf,  &
    output_annual_cohorts_froot,  &
    output_annual_cohorts_fwood,  &
    output_annual_cohorts_GPP,    &
    output_annual_cohorts_NPP,    &
    output_annual_cohorts_Rauto,  &
    output_annual_cohorts_Nupt,   &
    output_annual_cohorts_Nfix,   &
    output_annual_cohorts_n_deadtrees,  &
    output_annual_cohorts_c_deadtrees,  &
    output_annual_cohorts_deathrate  &
    ) bind(C, name = "biomee_f_")
     
    !////////////////////////////////////////////////////////////////
    ! Main subroutine to handle I/O with C and R. 
    ! Receives simulation parameters, site parameters, and the full 
    ! simulation's forcing as time series
    ! test xxx
    !----------------------------------------------------------------
    ! use md_params_soil_biomee, only: getsoil
    use md_forcing_biomee, only: getclimate, &
      climate_type
    use md_interface_biomee
    use md_params_core
    use md_biosphere_biomee, only: biosphere_annual

    implicit none

    ! Simulation parameters
    integer(kind=c_int), intent(in) :: spinup                 ! logical type is not supported in the C interface (LTO)
    integer(kind=c_int),  intent(in) :: spinupyears
    integer(kind=c_int),  intent(in) :: recycle
    integer(kind=c_int),  intent(in) :: firstyeartrend
    integer(kind=c_int),  intent(in) :: nyeartrend

    integer(kind=c_int), intent(in) :: do_U_shaped_mortality  ! logical
    integer(kind=c_int), intent(in) :: update_annualLAImax    ! logical
    integer(kind=c_int), intent(in) :: do_closedN_run         ! logical
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
    integer(kind=c_int),  intent(in) :: n_params_species
    real(kind=c_double), dimension(n_params_species,55), intent(in) :: params_species
    integer(kind=c_int),  intent(in) :: n_init_cohort
    real(kind=c_double), dimension(n_init_cohort,9),  intent(in) :: init_cohort

    ! initial soil pool size
    real(kind=c_double), intent(in) :: init_fast_soil_C
    real(kind=c_double), intent(in) :: init_slow_soil_C
    real(kind=c_double), intent(in) :: init_Nmineral
    real(kind=c_double), intent(in) :: N_input

    integer(kind=c_int), intent(in) :: nt
    integer(kind=c_int), intent(in) :: nt_daily
    integer(kind=c_int), intent(in) :: nt_annual
    integer(kind=c_int), intent(in) :: nt_annual_cohorts

    ! input and output arrays (naked) to be passed back to C/R
    real(kind=c_double), dimension(nt,7), intent(in) :: forcing

    integer(kind=c_int), intent(in) :: steps_per_day  ! Forcing resolution

    real(kind=c_double), dimension(nt_daily,nvars_daily_tile), intent(out) :: output_daily_tile ! nvars_daily_tile = 35

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

    integer                 :: yr
    
    integer :: idx
    integer :: idx_daily_start
    integer :: idx_daily_end

    !----------------------------------------------------------------
    ! POPULATE MYINTERFACE WITH ARGUMENTS FROM R
    !----------------------------------------------------------------
    myinterface%params_siml%steering%do_spinup        = spinup /= 0
    myinterface%params_siml%steering%spinupyears      = spinupyears
    myinterface%params_siml%steering%recycle          = recycle
    myinterface%params_siml%steering%firstyeartrend   = firstyeartrend
    myinterface%params_siml%steering%nyeartrend       = nyeartrend

    if (myinterface%params_siml%steering%do_spinup) then
      myinterface%params_siml%steering%runyears = myinterface%params_siml%steering%nyeartrend &
              + myinterface%params_siml%steering%spinupyears
    else
      myinterface%params_siml%steering%runyears = myinterface%params_siml%steering%nyeartrend
      myinterface%params_siml%steering%spinupyears = 0
    endif

    ! Simulation parameters
    myinterface%params_siml%do_U_shaped_mortality = do_U_shaped_mortality /= 0
    myinterface%params_siml%update_annualLAImax   = update_annualLAImax /= 0
    myinterface%params_siml%do_closedN_run        = do_closedN_run /= 0

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

    !----------------------------------------------------------------
    ! GET GRID INFORMATION
    !----------------------------------------------------------------
    myinterface%grid%lon = real( longitude )
    myinterface%grid%lat = real( latitude )
    myinterface%grid%elv = real( altitude )   

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
    allocate(myinterface%params_species(n_params_species))
    myinterface%params_species(:)%lifeform      = int(  params_species(:,1))
    myinterface%params_species(:)%phenotype     = int(  params_species(:,2))
    myinterface%params_species(:)%pt            = int(  params_species(:,3))
    myinterface%params_species(:)%alpha_FR      = real( params_species(:,4))
    myinterface%params_species(:)%rho_FR        = real( params_species(:,5))
    myinterface%params_species(:)%root_r        = real( params_species(:,6))
    myinterface%params_species(:)%root_zeta     = real( params_species(:,7))
    myinterface%params_species(:)%Kw_root       = real( params_species(:,8))
    myinterface%params_species(:)%leaf_size     = real( params_species(:,9))
    myinterface%params_species(:)%Vmax          = real( params_species(:,10))
    myinterface%params_species(:)%Vannual       = real( params_species(:,11))
    myinterface%params_species(:)%wet_leaf_dreg = real( params_species(:,12))
    myinterface%params_species(:)%m_cond        = real( params_species(:,13))
    myinterface%params_species(:)%alpha_phot    = real( params_species(:,14))
    myinterface%params_species(:)%gamma_L       = real( params_species(:,15))
    myinterface%params_species(:)%gamma_LN      = real( params_species(:,16))
    myinterface%params_species(:)%gamma_SW      = real( params_species(:,17))
    myinterface%params_species(:)%gamma_FR      = real( params_species(:,18))
    myinterface%params_species(:)%tc_crit       = real( params_species(:,19))
    myinterface%params_species(:)%tc_crit_on    = real( params_species(:,20))
    myinterface%params_species(:)%gdd_crit      = real( params_species(:,21))
    myinterface%params_species(:)%betaON        = real( params_species(:,22))
    myinterface%params_species(:)%betaOFF       = real( params_species(:,23))
    myinterface%params_species(:)%alphaHT       = real( params_species(:,24)) ! prescribed
    myinterface%params_species(:)%thetaHT       = real( params_species(:,25)) ! prescribed
    myinterface%params_species(:)%alphaCA       = real( params_species(:,26)) ! prescribed
    myinterface%params_species(:)%thetaCA       = real( params_species(:,27)) ! prescribed
    myinterface%params_species(:)%alphaBM       = real( params_species(:,28)) ! prescribed
    myinterface%params_species(:)%thetaBM       = real( params_species(:,29)) ! prescribed
    myinterface%params_species(:)%seedlingsize  = real( params_species(:,30))
    myinterface%params_species(:)%maturalage    = real( params_species(:,31))
    myinterface%params_species(:)%v_seed        = real( params_species(:,32))
    myinterface%params_species(:)%mortrate_d_c  = real( params_species(:,33))
    myinterface%params_species(:)%mortrate_d_u  = real( params_species(:,34))
    myinterface%params_species(:)%LMA           = real( params_species(:,35)) ! prescribed
    myinterface%params_species(:)%leafLS        = real( params_species(:,36))
    myinterface%params_species(:)%LNbase        = real( params_species(:,37))
    myinterface%params_species(:)%CNleafsupport = real( params_species(:,38))
    myinterface%params_species(:)%rho_wood      = real( params_species(:,39)) ! prescribed
    myinterface%params_species(:)%taperfactor   = real( params_species(:,40))
    myinterface%params_species(:)%lAImax        = real( params_species(:,41))
    myinterface%params_species(:)%tauNSC        = real( params_species(:,42))
    myinterface%params_species(:)%fNSNmax       = real( params_species(:,43))
    myinterface%params_species(:)%phiCSA        = real( params_species(:,44))
    myinterface%params_species(:)%CNleaf0       = real( params_species(:,45))
    myinterface%params_species(:)%CNsw0         = real( params_species(:,46))
    myinterface%params_species(:)%CNwood0       = real( params_species(:,47))
    myinterface%params_species(:)%CNroot0       = real( params_species(:,48))
    myinterface%params_species(:)%CNseed0       = real( params_species(:,49))
    myinterface%params_species(:)%Nfixrate0     = real( params_species(:,50))
    myinterface%params_species(:)%NfixCost0     = real( params_species(:,51))
    myinterface%params_species(:)%internal_gap_frac  = real( params_species(:,52))
    myinterface%params_species(:)%kphio         = real( params_species(:,53)) ! calibratable
    myinterface%params_species(:)%phiRL         = real( params_species(:,54)) ! calibratable
    myinterface%params_species(:)%LAI_light     = real( params_species(:,55)) ! calibratable

    ! Initial cohort sizes
    allocate(myinterface%init_cohort(n_init_cohort))
    myinterface%init_cohort(:)%init_n_cohorts      = int(init_cohort(:,1))
    myinterface%init_cohort(:)%init_cohort_species = int(init_cohort(:,2))
    myinterface%init_cohort(:)%init_cohort_nindivs = real(init_cohort(:,3))
    myinterface%init_cohort(:)%init_cohort_bl      = real(init_cohort(:,4))
    myinterface%init_cohort(:)%init_cohort_br      = real(init_cohort(:,5))
    myinterface%init_cohort(:)%init_cohort_bsw     = real(init_cohort(:,6))
    myinterface%init_cohort(:)%init_cohort_bHW     = real(init_cohort(:,7))
    myinterface%init_cohort(:)%init_cohort_seedC   = real(init_cohort(:,8))
    myinterface%init_cohort(:)%init_cohort_nsc     = real(init_cohort(:,9))

    ! Initial soil pools
    myinterface%init_soil%init_fast_soil_C = real( init_fast_soil_C )
    myinterface%init_soil%init_slow_soil_C = real( init_slow_soil_C )
    myinterface%init_soil%init_Nmineral    = real( init_Nmineral )
    myinterface%init_soil%N_input          = real( N_input )

    !----------------------------------------------------------------
    ! INTERPRET FORCING
    !----------------------------------------------------------------
    myinterface%steps_per_day = steps_per_day
    ntstepsyear = myinterface%steps_per_day * ndayyear
    myinterface%dt_fast_yr = 1.0 / ntstepsyear
    myinterface%step_seconds = secs_per_day / myinterface%steps_per_day ! seconds_per_year * dt_fast_yr

    allocate(myinterface%climate(ntstepsyear))
    allocate(myinterface%pco2(ntstepsyear))

    yearloop: do yr=1, myinterface%params_siml%steering%runyears
      !----------------------------------------------------------------
      ! Define simulations "steering" variables (forcingyear, etc.)
      !----------------------------------------------------------------
      myinterface%steering = get_steering( yr, myinterface%params_siml%steering )

      !----------------------------------------------------------------
      ! Get external (environmental) forcing (for biomee, co2 is in myinterface%climate)
      !----------------------------------------------------------------
      ! Get climate variables for this year (full fields and 365 daily values for each variable)
      myinterface%climate(:) = getclimate( &
                                            nt, &
                                            ntstepsyear, &
                                            forcing, &
                                            myinterface%steering%climateyear_idx &
                                            )

      !----------------------------------------------------------------
      ! Call biosphere (wrapper for all modules, contains time loops)
      !----------------------------------------------------------------
      call biosphere_annual( &
        out_biosphere_daily_tile, &
        out_biosphere_annual_tile, &
        out_biosphere_annual_cohorts &
        )

      !----------------------------------------------------------------
      ! Output out_daily_tile (calling subroutine)
      !----------------------------------------------------------------
      ! Output only for transient years
      if (.not. myinterface%steering%spinup) then  

        idx_daily_start = (yr - myinterface%params_siml%steering%spinupyears - 1) * ndayyear + 1
        idx_daily_end   = idx_daily_start + ndayyear - 1

        call populate_outarray_daily_tile( out_biosphere_daily_tile(:), output_daily_tile(idx_daily_start:idx_daily_end,:))

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

        idx =  yr - myinterface%params_siml%steering%spinupyears

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
    deallocate(myinterface%params_species)
    deallocate(myinterface%init_cohort)
 
  end subroutine biomee_f

  !////////////////////////////////////////////////////////////////
  ! Populates daily tile-level output array passed back to C and R.
  !----------------------------------------------------------------
  subroutine populate_outarray_daily_tile( daily_tile, out_daily_tile ) !, idx_daily_start, idx_daily_end

    use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32, in=>int32
    use md_interface_biomee, only: outtype_daily_tile
    use md_params_core

    ! arguments
    type(outtype_daily_tile), dimension(ndayyear), intent(in) :: daily_tile
    real(kind=dp), dimension(ndayyear, nvars_daily_tile), intent(inout) :: out_daily_tile

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


  !////////////////////////////////////////////////////////////////
  ! Populates annual output tile-level array passed back to C and R.
  !----------------------------------------------------------------
  subroutine populate_outarray_annual_tile( annual_tile, out_annual_tile )

    use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32, in=>int32
    use md_interface_biomee, only: outtype_annual_tile
    use md_params_core

    ! arguments
    type(outtype_annual_tile), intent(in) :: annual_tile
    real(kind=dp), dimension(nvars_annual_tile), intent(inout) :: out_annual_tile(:)

    out_annual_tile(1)  = dble(annual_tile%year)
    out_annual_tile(2)  = dble(annual_tile%CAI)
    out_annual_tile(3)  = dble(annual_tile%LAI)
    out_annual_tile(4)  = dble(annual_tile%density)
    out_annual_tile(5)  = dble(annual_tile%DBH)
    out_annual_tile(6)  = dble(annual_tile%Density12)
    out_annual_tile(7)  = dble(annual_tile%DBH12)
    out_annual_tile(8)  = dble(annual_tile%QMD12)
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

end module sofun_r_mod
