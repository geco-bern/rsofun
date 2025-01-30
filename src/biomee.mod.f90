module biomee_mod
  !////////////////////////////////////////////////////////////////
  ! Contains main subroutine handling I/O with C and R.
  !----------------------------------------------------------------
  use, intrinsic :: iso_c_binding, only: c_double, c_int, c_char, c_bool
  use, intrinsic :: ieee_arithmetic

  implicit none

  private
  public :: biomee_f

contains

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
    nt_annual_trans,              &
    forcing,                      &
    n_lu,                         &
    init_lu,                      &
    n_lu_tr_years,                &
    luc_forcing,                  &
    output_daily_tile,            &
    output_annual_tile,           &
    output_annual_cohorts,        &
    output_annual_luluc_tile      &
  ) bind(C, name = "biomee_f_")
     
    !////////////////////////////////////////////////////////////////
    ! Main subroutine to handle I/O with C and R. 
    ! Receives simulation parameters, site parameters, and the full 
    ! simulation's forcing as time series
    ! test xxx
    !----------------------------------------------------------------
    use md_forcing_biomee, only: getclimate, &
      climate_type
    use md_interface_biomee
    use vegetation_tile_biomee
    use md_biosphere_biomee, only: biosphere_annual
    use md_luluc, only: update_lu_state, populate_outarray_annual_land_use

    implicit none

    ! mutble state keeping track of simulation state
    type(outtype_steering) :: state

    ! Array dimensions
    integer(kind=c_int), intent(in) :: nt                ! Forcing array dimension
    integer(kind=c_int), intent(in) :: nt_daily          ! Number of simulated days
    integer(kind=c_int), intent(in) :: nt_annual         ! Number of years (spinup + transient)
    integer(kind=c_int), intent(in) :: nt_annual_trans   ! Number of transient years

    ! Naked arrays
    integer(kind=c_int), intent(in) :: n_params_species
    real(kind=c_double), dimension(n_params_species, nvars_params_species), intent(in) :: params_species
    integer(kind=c_int), intent(in) :: n_init_cohort
    real(kind=c_double), dimension(n_init_cohort,nvars_init_cohorts),  intent(in)  :: init_cohort
    real(kind=c_double), dimension(nvars_init_soil),  intent(in)  :: init_soil
    real(kind=c_double), dimension(nvars_params_tile), intent(in) :: params_tile
    real(kind=c_double), dimension(nvars_params_siml), intent(in) :: params_siml
    real(kind=c_double), dimension(nvars_site_info),  intent(in)  :: site_info
    real(kind=c_double), dimension(nt,nvars_forcing), intent(in)  :: forcing

    ! LULUC
    integer(kind=c_int), intent(in) :: n_lu                           ! Number of land use types
    real(kind=c_double), dimension(n_lu,nvars_init_lu), intent(in) :: init_lu          ! Initial LU state
    integer(kind=c_int), intent(in) :: n_lu_tr_years                  ! Number of LU transitions
    real(kind=c_double), dimension(n_lu,n_lu,n_lu_tr_years), intent(in) :: luc_forcing ! LU transitions

    ! Output arrays (naked) to be passed back to C/R
    real(kind=c_double), dimension(nt_daily,nvars_daily_tile, n_lu), intent(out) :: output_daily_tile
    real(kind=c_double), dimension(nt_annual,nvars_annual_tile, n_lu), intent(out) :: output_annual_tile
    real(kind=c_double), dimension(out_max_cohorts, nt_annual_trans, nvars_annual_cohorts, n_lu), &
            intent(out) :: output_annual_cohorts
    real(kind=c_double), dimension(nt_annual,nvars_lu_out, n_lu), intent(out) :: output_annual_luluc_tile

    ! Local variables
    type(vegn_tile_type), dimension(n_lu)                       :: vegn_tiles ! One tile per LU
    real :: lu_state(n_lu) ! Current LU fractions
    real(kind=c_double) :: nan

    integer :: yr, idx, idx_daily_start, idx_daily_end, lu_idx

    ! Initialize outputs to NaN
    nan = ieee_value(nan, ieee_quiet_nan)
    output_daily_tile = nan
    output_annual_tile = nan
    output_annual_cohorts = nan

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
    myinterface%params_siml%do_closedN_run        = int(params_siml(8)) /= 0

    ! this needs to be consistent with translation to code in run_biomee_f_bysite.R
    if (int(params_siml(9)) == 1) then
      myinterface%params_siml%method_photosynth = "gs_leuning"
    else
      myinterface%params_siml%method_photosynth = "pmodel"
    end if

    select case( int(params_siml(10)) )
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

    ! Site info
    myinterface%site_info%lon = real( site_info(1) )
    myinterface%site_info%lat = real( site_info(2) )
    myinterface%site_info%elv = real( site_info(3) )

    ! Tile parameters
    myinterface%params_tile%soiltype                 = int( params_tile(1) )  ! Sand = 1, LoamySand = 2, SandyLoam = 3, SiltLoam = 4, FrittedClay = 5, Loam = 6, Clay = 7
    myinterface%params_tile%FLDCAP                   = real( params_tile(2) ) ! vol / vol
    myinterface%params_tile%WILTPT                   = real( params_tile(3) ) ! vol / vol
    myinterface%params_tile%K1                       = real( params_tile(4) )
    myinterface%params_tile%K2                       = real( params_tile(5) )
    myinterface%params_tile%K_nitrogen               = real( params_tile(6) )
    myinterface%params_tile%MLmixRatio               = real( params_tile(7) )
    myinterface%params_tile%etaN                     = real( params_tile(8) )
    myinterface%params_tile%LMAmin                   = real( params_tile(9) )
    myinterface%params_tile%fsc_fine                 = real( params_tile(10) )
    myinterface%params_tile%fsc_wood                 = real( params_tile(11) )
    myinterface%params_tile%GR_factor                = real( params_tile(12) )
    myinterface%params_tile%l_fract                  = real( params_tile(13) )
    myinterface%params_tile%retransN                 = real( params_tile(14) )
    myinterface%params_tile%f_initialBSW             = real( params_tile(15) )
    myinterface%params_tile%f_N_add                  = real( params_tile(16) )
    myinterface%params_tile%tf_base                  = real( params_tile(17) )
    myinterface%params_tile%par_mort                 = real( params_tile(18) )
    myinterface%params_tile%par_mort_under           = real( params_tile(19) )

    ! Species parameters
    allocate(myinterface%params_species(n_params_species))
    myinterface%params_species(:)%lifeform           = int(  params_species(:,1))
    myinterface%params_species(:)%phenotype          = int(  params_species(:,2))
    myinterface%params_species(:)%pt                 = int(  params_species(:,3))
    myinterface%params_species(:)%alpha_FR           = real( params_species(:,4))
    myinterface%params_species(:)%rho_FR             = real( params_species(:,5))
    myinterface%params_species(:)%root_r             = real( params_species(:,6))
    myinterface%params_species(:)%root_zeta          = real( params_species(:,7))
    myinterface%params_species(:)%Kw_root            = real( params_species(:,8))
    myinterface%params_species(:)%leaf_size          = real( params_species(:,9))
    myinterface%params_species(:)%Vmax               = real( params_species(:,10))
    myinterface%params_species(:)%Vannual            = real( params_species(:,11))
    myinterface%params_species(:)%wet_leaf_dreg      = real( params_species(:,12))
    myinterface%params_species(:)%m_cond             = real( params_species(:,13))
    myinterface%params_species(:)%alpha_phot         = real( params_species(:,14))
    myinterface%params_species(:)%gamma_L            = real( params_species(:,15))
    myinterface%params_species(:)%gamma_LN           = real( params_species(:,16))
    myinterface%params_species(:)%gamma_SW           = real( params_species(:,17))
    myinterface%params_species(:)%gamma_FR           = real( params_species(:,18))
    myinterface%params_species(:)%tc_crit            = real( params_species(:,19))
    myinterface%params_species(:)%tc_crit_on         = real( params_species(:,20))
    myinterface%params_species(:)%gdd_crit           = real( params_species(:,21))
    myinterface%params_species(:)%betaON             = real( params_species(:,22))
    myinterface%params_species(:)%betaOFF            = real( params_species(:,23))
    myinterface%params_species(:)%alphaHT            = real( params_species(:,24)) ! prescribed
    myinterface%params_species(:)%thetaHT            = real( params_species(:,25)) ! prescribed
    myinterface%params_species(:)%alphaCA            = real( params_species(:,26)) ! prescribed
    myinterface%params_species(:)%thetaCA            = real( params_species(:,27)) ! prescribed
    myinterface%params_species(:)%alphaBM            = real( params_species(:,28)) ! prescribed
    myinterface%params_species(:)%thetaBM            = real( params_species(:,29)) ! prescribed
    myinterface%params_species(:)%seedlingsize       = real( params_species(:,30))
    myinterface%params_species(:)%maturalage         = real( params_species(:,31))
    myinterface%params_species(:)%v_seed             = real( params_species(:,32))
    myinterface%params_species(:)%mortrate_d_c       = real( params_species(:,33))
    myinterface%params_species(:)%mortrate_d_u       = real( params_species(:,34))
    myinterface%params_species(:)%LMA                = real( params_species(:,35)) ! prescribed
    myinterface%params_species(:)%leafLS             = real( params_species(:,36))
    myinterface%params_species(:)%LNbase             = real( params_species(:,37))
    myinterface%params_species(:)%CNleafsupport      = real( params_species(:,38))
    myinterface%params_species(:)%rho_wood           = real( params_species(:,39)) ! prescribed
    myinterface%params_species(:)%taperfactor        = real( params_species(:,40))
    ! myinterface%params_species(:)%lAImax             = real( params_species(:,41)) ! overriden
    myinterface%params_species(:)%tauNSC             = real( params_species(:,42))
    myinterface%params_species(:)%fNSNmax            = real( params_species(:,43))
    myinterface%params_species(:)%phiCSA             = real( params_species(:,44))
    myinterface%params_species(:)%CNleaf0            = real( params_species(:,45))
    myinterface%params_species(:)%CNsw0              = real( params_species(:,46))
    myinterface%params_species(:)%CNwood0            = real( params_species(:,47))
    myinterface%params_species(:)%CNroot0            = real( params_species(:,48))
    myinterface%params_species(:)%CNseed0            = real( params_species(:,49))
    myinterface%params_species(:)%Nfixrate0          = real( params_species(:,50))
    myinterface%params_species(:)%NfixCost0          = real( params_species(:,51))
    myinterface%params_species(:)%internal_gap_frac  = real( params_species(:,52))
    myinterface%params_species(:)%kphio              = real( params_species(:,53)) ! calibratable
    myinterface%params_species(:)%phiRL              = real( params_species(:,54)) ! calibratable
    myinterface%params_species(:)%LAI_light          = real( params_species(:,55)) ! calibratable

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
    myinterface%init_soil%init_fast_soil_C         = real( init_soil(1) )
    myinterface%init_soil%init_slow_soil_C         = real( init_soil(2) )
    myinterface%init_soil%init_Nmineral            = real( init_soil(3) )
    myinterface%init_soil%N_input                  = real( init_soil(4) )

    ! LULUC initializations
    lu_state = real(init_lu(:, 1))
    output_annual_luluc_tile = 0

    !----------------------------------------------------------------
    ! INTERPRET FORCING
    !----------------------------------------------------------------
    myinterface%steps_per_day = int(params_siml(6)) ! Forcing resolution
    ntstepsyear = myinterface%steps_per_day * ndayyear
    myinterface%dt_fast_yr = 1.0 / ntstepsyear
    myinterface%step_seconds = secs_per_day / myinterface%steps_per_day ! seconds_per_year * dt_fast_yr

    allocate(myinterface%climate(ntstepsyear))

    ! Initialize PFT parameters
    call initialize_PFT_data()

    yearloop: do yr=1, myinterface%params_siml%steering%runyears
      !----------------------------------------------------------------
      ! Define simulations "steering" variables (forcingyear, etc.)
      !----------------------------------------------------------------
      state = get_steering( yr, myinterface%params_siml%steering )

      !----------------------------------------------------------------
      ! Get external (environmental) forcing (for biomee, co2 is in myinterface%climate)
      !----------------------------------------------------------------
      ! Get climate variables for this year (full fields and 365 daily values for each variable)
      myinterface%climate(:) = getclimate( &
         nt, &
         ntstepsyear, &
         forcing, &
         state%climateyear_idx &
      )

      idx_daily_start = (state%year - myinterface%params_siml%steering%spinupyears - 1) * ndayyear + 1
      idx_daily_end   = idx_daily_start + ndayyear - 1

      ! For each LU (land unit) whose fraction is > 0
      do lu_idx = 1, n_lu
        if (lu_state(lu_idx) > 0) then

          !----------------------------------------------------------------
          ! Call biosphere (wrapper for all modules, contains time loops)
          !----------------------------------------------------------------
          if (state%spinup) then
            call biosphere_annual( &
              state, &
              vegn_tiles(lu_idx), &
              output_annual_tile(state%year, :, lu_idx) &
            )
          else
            idx =  state%year - myinterface%params_siml%steering%spinupyears
            call biosphere_annual( &
                    state, &
                    vegn_tiles(lu_idx), &
                    output_annual_tile(state%year, :, lu_idx), &
                    output_daily_tile(idx_daily_start:idx_daily_end, :, lu_idx), &
                    output_annual_cohorts(:, idx,:, lu_idx) &
                    )
          end if

        end if

      end do

      !----------------------------------------------------------------
      ! Update LULUC state and fill output
      !----------------------------------------------------------------
      if ((.not.state%spinup) .and. (state%forcingyear_idx <= n_lu_tr_years)) then

        call update_lu_state(lu_state, real(luc_forcing(:,:,state%forcingyear_idx)), n_lu)

      end if
      call populate_outarray_annual_land_use(state%year, lu_state, output_annual_luluc_tile(state%year,:,:))

    end do yearloop

    ! Clean-up allocated memory
    deallocate(myinterface%climate)
    deallocate(myinterface%params_species)
    deallocate(myinterface%init_cohort)
    do idx = 1, n_lu
      call vegn_tiles(idx)%shut_down()
    end do


  end subroutine biomee_f

end module biomee_mod
