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
    params_siml,                  &
    site_info,                    &
    params_tile,                  &
    n_params_species,             &
    params_species,               &
    n_init_cohort,                &
    init_cohort,                  &
    init_soil,                    &
    nt,                           &  
    nt_daily,                     &    
    nt_annual,                    &    
    nt_annual_cohorts,            &    
    forcing,                      &
    n_lu,                         &
    init_lu,                      &
    n_lu_tr_years,                &
    luc_forcing,                  &
    output_daily_tile,            &
    output_annual_tile,           &
    output_annual_cohorts         &
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

    ! naked arrays
    integer(kind=c_int), intent(in) :: n_params_species
    real(kind=c_double), dimension(n_params_species,55), intent(in) :: params_species
    integer(kind=c_int), intent(in) :: n_init_cohort
    real(kind=c_double), dimension(n_init_cohort,9),  intent(in) :: init_cohort
    real(kind=c_double), dimension(4),  intent(in) :: init_soil
    real(kind=c_double), dimension(19), intent(in) :: params_tile
    real(kind=c_double), dimension(11), intent(in) :: params_siml
    real(kind=c_double), dimension(3),  intent(in) :: site_info

    ! LULUC
    integer(kind=c_int), intent(in) :: n_lu
    real(kind=c_double), dimension(n_lu,1), intent(in) :: init_lu
    integer(kind=c_int), intent(in) :: n_lu_tr_years
    real(kind=c_double), dimension(n_lu,n_lu,n_lu_tr_years), intent(in) :: luc_forcing

    integer(kind=c_int), intent(in) :: nt
    integer(kind=c_int), intent(in) :: nt_daily
    integer(kind=c_int), intent(in) :: nt_annual
    integer(kind=c_int), intent(in) :: nt_annual_cohorts

    ! input and output arrays (naked) to be passed back to C/R
    real(kind=c_double), dimension(nt,7), intent(in) :: forcing

    real(kind=c_double), dimension(nt_daily,nvars_daily_tile), intent(out) :: output_daily_tile

    real(kind=c_double), dimension(nt_annual,nvars_annual_tile), intent(out) :: output_annual_tile

    real(kind=c_double), dimension(out_max_cohorts, nt_annual_cohorts, nvars_annual_cohorts), intent(out) :: output_annual_cohorts

    ! local variables
    type(outtype_daily_tile),     dimension(ndayyear)                 :: out_biosphere_daily_tile
    type(outtype_annual_tile)                                         :: out_biosphere_annual_tile
    type(outtype_annual_cohorts), dimension(out_max_cohorts)          :: out_biosphere_annual_cohorts

    integer :: yr, idx, idx_daily_start, idx_daily_end

    !----------------------------------------------------------------
    ! POPULATE MYINTERFACE WITH ARGUMENTS FROM R
    !----------------------------------------------------------------
    myinterface%params_siml%steering%do_spinup        = int(params_siml(1)) /= 0
    myinterface%params_siml%steering%spinupyears      = int(params_siml(2))
    myinterface%params_siml%steering%recycle          = int(params_siml(3))
    myinterface%params_siml%steering%firstyeartrend   = int(params_siml(4))
    myinterface%params_siml%steering%nyeartrend       = int(params_siml(5))

    if (myinterface%params_siml%steering%do_spinup) then
      myinterface%params_siml%steering%runyears = myinterface%params_siml%steering%nyeartrend &
              + myinterface%params_siml%steering%spinupyears
    else
      myinterface%params_siml%steering%runyears = myinterface%params_siml%steering%nyeartrend
      myinterface%params_siml%steering%spinupyears = 0
    endif

    ! Simulation parameters
    myinterface%params_siml%do_U_shaped_mortality = int(params_siml(7)) /= 0
    myinterface%params_siml%update_annualLAImax   = int(params_siml(8)) /= 0
    myinterface%params_siml%do_closedN_run        = int(params_siml(9)) /= 0

    ! this needs to be consistent with translation to code in run_biomee_f_bysite.R
    if (int(params_siml(10)) == 1) then
      myinterface%params_siml%method_photosynth = "gs_leuning"
    else
      myinterface%params_siml%method_photosynth = "pmodel"
    end if

    select case( int(params_siml(11)) )
      case (1)
      myinterface%params_siml%method_mortality = "cstarvation"
      case (2)
      myinterface%params_siml%method_mortality = "growthrate"
      case (3)
      myinterface%params_siml%method_mortality = "dbh"
      case (4)
      myinterface%params_siml%method_mortality = "const_selfthin"
      case (5)
      myinterface%params_siml%method_mortality = "bal"
    end select

    !----------------------------------------------------------------
    ! GET GRID INFORMATION
    !----------------------------------------------------------------
    myinterface%grid%lon = real( site_info(1) )
    myinterface%grid%lat = real( site_info(2) )
    myinterface%grid%elv = real( site_info(3) )

    ! Tile parameters
    myinterface%params_tile%soiltype     = int( params_tile(1) )
    myinterface%params_tile%FLDCAP       = real( params_tile(2) )
    myinterface%params_tile%WILTPT       = real( params_tile(3) )
    myinterface%params_tile%K1           = real( params_tile(4) )
    myinterface%params_tile%K2           = real( params_tile(5) )
    myinterface%params_tile%K_nitrogen   = real( params_tile(6) )
    myinterface%params_tile%MLmixRatio   = real( params_tile(7) )
    myinterface%params_tile%etaN         = real( params_tile(8) )
    myinterface%params_tile%LMAmin       = real( params_tile(9) )
    myinterface%params_tile%fsc_fine     = real( params_tile(10) )
    myinterface%params_tile%fsc_wood     = real( params_tile(11) )
    myinterface%params_tile%GR_factor    = real( params_tile(12) )
    myinterface%params_tile%l_fract      = real( params_tile(13) )
    myinterface%params_tile%retransN     = real( params_tile(14) )
    myinterface%params_tile%f_initialBSW = real( params_tile(15) )
    myinterface%params_tile%f_N_add      = real( params_tile(16) )
    myinterface%params_tile%tf_base      = real( params_tile(17) )
    myinterface%params_tile%par_mort     = real( params_tile(18) )
    myinterface%params_tile%par_mort_under  = real( params_tile(19) )

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
    myinterface%init_cohort(:)%init_cohort_species = int(init_cohort(:,1))
    myinterface%init_cohort(:)%init_cohort_nindivs = real(init_cohort(:,2))
    myinterface%init_cohort(:)%init_cohort_bl      = real(init_cohort(:,3))
    myinterface%init_cohort(:)%init_cohort_br      = real(init_cohort(:,4))
    myinterface%init_cohort(:)%init_cohort_bsw     = real(init_cohort(:,5))
    myinterface%init_cohort(:)%init_cohort_bHW     = real(init_cohort(:,6))
    myinterface%init_cohort(:)%init_cohort_seedC   = real(init_cohort(:,7))
    myinterface%init_cohort(:)%init_cohort_nsc     = real(init_cohort(:,8))

    ! Initial soil pools
    myinterface%init_soil%init_fast_soil_C = real( init_soil(1) )
    myinterface%init_soil%init_slow_soil_C = real( init_soil(2) )
    myinterface%init_soil%init_Nmineral    = real( init_soil(3) )
    myinterface%init_soil%N_input          = real( init_soil(4) )

    ! Initial LU and LUC
    allocate(myinterface%lu_states(n_lu))
    myinterface%lu_states = real(init_lu(:,1))
    !allocate(myinterface%lu_transitions(n_lu, n_lu))
    !myinterface%lu_transitions = 0.0

    !----------------------------------------------------------------
    ! INTERPRET FORCING
    !----------------------------------------------------------------
    myinterface%steps_per_day = params_siml(6) ! Forcing resolution
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
      out_biosphere_annual_tile%lu_fraction = myinterface%lu_states(1)
      call populate_outarray_annual_tile( out_biosphere_annual_tile, output_annual_tile(yr,:) )

      !----------------------------------------------------------------
      ! Output output_annual_cohorts (without subroutine)
      !----------------------------------------------------------------
      ! To get outputs only after spinupyears make if below and 
      ! also in run_biomee_f_bysite.R make n_annual_cohorts = as.integer(params_siml$nyeartrend)

      if (.not. myinterface%steering%spinup) then  

        idx =  yr - myinterface%params_siml%steering%spinupyears

        call populate_outarray_annual_cohort(out_biosphere_annual_cohorts, output_annual_cohorts(:, idx,:))

       end if

    end do yearloop

    deallocate(myinterface%climate)
    deallocate(myinterface%pco2)
    deallocate(myinterface%params_species)
    deallocate(myinterface%init_cohort)
    deallocate(myinterface%lu_states)
    !deallocate(myinterface%lu_transitions)

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
    out_annual_tile(60) = dble(annual_tile%lu_fraction)

  end subroutine populate_outarray_annual_tile


  subroutine populate_outarray_annual_cohort( annual_cohorts, output_annual_cohorts )

    use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32, in=>int32
    use md_interface_biomee, only: outtype_annual_cohorts
    use md_params_core

    ! arguments
    type(outtype_annual_cohorts), dimension(out_max_cohorts), intent(in) :: annual_cohorts
    real(kind=dp), dimension(out_max_cohorts, nvars_annual_cohorts), intent(inout) :: output_annual_cohorts
    integer :: i

    output_annual_cohorts(:, 1)  = dble((/ (i, i = 1, out_max_cohorts) /))
    output_annual_cohorts(:, 2)  = dble(annual_cohorts(:)%year)
    output_annual_cohorts(:, 3)  = dble(annual_cohorts(:)%cID)
    output_annual_cohorts(:, 4)  = dble(annual_cohorts(:)%PFT)
    output_annual_cohorts(:, 5)  = dble(annual_cohorts(:)%layer)
    output_annual_cohorts(:, 6)  = dble(annual_cohorts(:)%density)
    output_annual_cohorts(:, 7)  = dble(annual_cohorts(:)%flayer)
    output_annual_cohorts(:, 8)  = dble(annual_cohorts(:)%DBH)
    output_annual_cohorts(:, 9)  = dble(annual_cohorts(:)%dDBH)
    output_annual_cohorts(:, 10) = dble(annual_cohorts(:)%height)
    output_annual_cohorts(:, 11) = dble(annual_cohorts(:)%age)
    output_annual_cohorts(:, 12) = dble(annual_cohorts(:)%BA)
    output_annual_cohorts(:, 13) = dble(annual_cohorts(:)%dBA)
    output_annual_cohorts(:, 14) = dble(annual_cohorts(:)%Acrown)
    output_annual_cohorts(:, 15) = dble(annual_cohorts(:)%Aleaf)
    output_annual_cohorts(:, 16) = dble(annual_cohorts(:)%nsc)
    output_annual_cohorts(:, 17) = dble(annual_cohorts(:)%nsn)
    output_annual_cohorts(:, 18) = dble(annual_cohorts(:)%seedC)
    output_annual_cohorts(:, 19) = dble(annual_cohorts(:)%leafC)
    output_annual_cohorts(:, 20) = dble(annual_cohorts(:)%rootC)
    output_annual_cohorts(:, 21) = dble(annual_cohorts(:)%sapwC)
    output_annual_cohorts(:, 22) = dble(annual_cohorts(:)%woodC)
    output_annual_cohorts(:, 23) = dble(annual_cohorts(:)%treeG)
    output_annual_cohorts(:, 24) = dble(annual_cohorts(:)%fseed)
    output_annual_cohorts(:, 25) = dble(annual_cohorts(:)%fleaf)
    output_annual_cohorts(:, 26) = dble(annual_cohorts(:)%froot)
    output_annual_cohorts(:, 27) = dble(annual_cohorts(:)%fwood)
    output_annual_cohorts(:, 28) = dble(annual_cohorts(:)%GPP)
    output_annual_cohorts(:, 29) = dble(annual_cohorts(:)%NPP)
    output_annual_cohorts(:, 30) = dble(annual_cohorts(:)%Rauto)
    output_annual_cohorts(:, 31) = dble(annual_cohorts(:)%Nupt)
    output_annual_cohorts(:, 32) = dble(annual_cohorts(:)%Nfix)
    output_annual_cohorts(:, 33) = dble(annual_cohorts(:)%deathrate)
    output_annual_cohorts(:, 34) = dble(annual_cohorts(:)%n_deadtrees)
    output_annual_cohorts(:, 35) = dble(annual_cohorts(:)%c_deadtrees)

  end subroutine populate_outarray_annual_cohort

end module sofun_r_mod
