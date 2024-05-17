module md_biosphere_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing loop through time steps within a year and 
  ! calls to SRs for individual processes.
  ! Does not contain any input/output; this is done in SR sofun.
  ! Code adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !----------------------------------------------------------------
  use datatypes
  use md_vegetation_biomee
  use md_soil_biomee
  use md_params_core
  
  implicit none
  private
  public biosphere_annual

  type(vegn_tile_type), pointer :: vegn   
  ! type(soil_tile_type),  pointer :: soil
  ! type(cohort_type),     pointer :: cx, cc

contains

  subroutine biosphere_annual( &
    out_biosphere_daily_tile, &
    ! out_biosphere_daily_cohorts, &
    out_biosphere_annual_tile, &
    out_biosphere_annual_cohorts &
    )
    !////////////////////////////////////////////////////////////////
    ! Calculates one year of vegetation dynamics. 
    !----------------------------------------------------------------
    use md_interface_biomee, only: myinterface, &
      outtype_daily_tile, &
      ! outtype_daily_cohorts, &
      outtype_annual_tile, &
      outtype_annual_cohorts
    use md_gpp_biomee, only: getpar_modl_gpp

    ! return variables
    type(outtype_daily_tile),     dimension(ndayyear)                , intent(out) :: out_biosphere_daily_tile
    ! type(outtype_daily_cohorts),  dimension(ndayyear,out_max_cohorts), intent(out) :: out_biosphere_daily_cohorts
    type(outtype_annual_tile)                                        , intent(out) :: out_biosphere_annual_tile
    type(outtype_annual_cohorts), dimension(out_max_cohorts)         , intent(out) :: out_biosphere_annual_cohorts

    ! ! local variables
    integer :: dm, moy, doy
    logical, save :: init = .true.   ! is true only on the first day of the simulation 
    ! logical, parameter :: verbose = .false.       ! change by hand for debugging etc.

    !----------------------------------------------------------------
    ! Biome-E stuff
    !----------------------------------------------------------------
    real    :: tsoil, soil_theta
    integer :: year0
    integer :: i
    integer :: idata
    !integer :: nfrequency ! disturbances
    integer, save :: simu_steps !, datalines
    integer, save :: iyears
    integer, save :: idays
    integer, save :: idoy

    !----------------------------------------------------------------
    ! INITIALISATIONS
    !----------------------------------------------------------------
    if (myinterface%steering%init) then ! is true for the first year

      ! Parameter initialization: Initialize PFT parameters
      call initialize_PFT_data()

      ! Initialize vegetation tile and plant cohorts
      allocate( vegn )
      call initialize_vegn_tile( vegn )
      
      ! Sort and relayer cohorts
      call relayer_cohorts( vegn )

      ! initialise outputs 
      call Zero_diagnostics( vegn )

      ! module-specific parameter specification
      call getpar_modl_gpp()

      year0  = myinterface%climate(1)%year  ! forcingData(1)%year

      iyears = 1
      idoy   = 0
      idays  = 0

    endif

    simu_steps = 0

    !----------------------------------------------------------------
    ! LOOP THROUGH MONTHS
    !----------------------------------------------------------------
    doy = 0
    monthloop: do moy=1,nmonth

      !----------------------------------------------------------------
      ! LOOP THROUGH DAYS
      !----------------------------------------------------------------
      dayloop: do dm=1,ndaymonth(moy)
        
        doy = doy + 1
        idoy = idoy + 1

        ! print*,'----------------------'
        ! print*,'YEAR, DOY ', myinterface%steering%year, doy
        ! print*,'----------------------'

        !----------------------------------------------------------------
        ! FAST TIME STEP
        !----------------------------------------------------------------
        ! get daily mean temperature from hourly/half-hourly data
        vegn%Tc_daily = 0.0
        tsoil         = 0.0
        fastloop: do i = 1,myinterface%steps_per_day

          idata         = simu_steps + 1
          year0         = myinterface%climate(idata)%year  ! Current year
          vegn%Tc_daily = vegn%Tc_daily + myinterface%climate(idata)%Tair
          tsoil         = myinterface%climate(idata)%tsoil
          simu_steps    = simu_steps + 1

          !----------------------------------------------------------------
          ! Sub-daily time step at resolution given by forcing (can be 1 = daily)
          !----------------------------------------------------------------
          call vegn_CNW_budget( vegn, myinterface%climate(idata), init )
         
          call hourly_diagnostics( vegn, myinterface%climate(idata) )
         
          init = .false.
         
        enddo fastloop ! hourly or half-hourly

        ! print*,'-----------day-------------'
        
        !-------------------------------------------------
        ! Daily calls after fast loop
        !-------------------------------------------------
        vegn%Tc_daily = vegn%Tc_daily / myinterface%steps_per_day
        tsoil         = tsoil / myinterface%steps_per_day
        soil_theta    = vegn%thetaS

        ! sum over fast time steps and cohorts
        call daily_diagnostics( vegn, iyears, idoy, out_biosphere_daily_tile(doy)  )  ! , out_biosphere_daily_cohorts(doy,:)
        
        ! Determine start and end of season and maximum leaf (root) mass
        call vegn_phenology( vegn )
        
        ! Produce new biomass from 'carbon_gain' (is zero afterwards) and continous biomass turnover
        call vegn_growth_EW( vegn )

      end do dayloop

    end do monthloop

    !----------------------------------------------------------------
    ! Annual calls
    !----------------------------------------------------------------
    idoy = 0

    if ( myinterface%params_siml%update_annualLAImax ) call vegn_annualLAImax_update( vegn )
    
    !---------------------------------------------
    ! Get annual diagnostics and outputs in once. 
    ! Needs to be called here 
    ! because mortality and reproduction re-organize
    ! cohorts again and we want annual output and daily
    ! output to be consistent with cohort identities.
    !---------------------------------------------
    call annual_diagnostics( vegn, iyears, out_biosphere_annual_cohorts(:), out_biosphere_annual_tile )

    !---------------------------------------------
    ! Reproduction and mortality
    !---------------------------------------------        
    ! Kill all individuals in a cohort if NSC falls below critical point
    call vegn_annual_starvation( vegn )
    
    ! Natural mortality (reducing number of individuals 'nindivs')
    ! (~Eq. 2 in Weng et al., 2015 BG)

    call vegn_nat_mortality( vegn )
    
    ! seed C and germination probability (~Eq. 1 in Weng et al., 2015 BG)
    call vegn_reproduction( vegn )
    
    !---------------------------------------------
    ! Re-organize cohorts
    !---------------------------------------------
    call kill_lowdensity_cohorts( vegn )

    call kill_old_grass( vegn ) 
    
    call relayer_cohorts( vegn )
    
    call vegn_mergecohorts( vegn )

    !---------------------------------------------
    ! Set annual variables zero
    !---------------------------------------------
    call Zero_diagnostics( vegn )

    ! update the years of model run
    iyears = iyears + 1

    ! !---------------------------------------------
    ! ! Reset vegetation to initial conditions
    ! !---------------------------------------------

    ! !if (iyears > myinterface%params_siml%spinupyears+31 .and. rand(0)<0.40) &
    ! !     call reset_vegn_initial(vegn) ! 0.01, 0.02, 0.04, 0.08, 0.20, 0.40

    ! !if (iyears == 700 .or. iyears == 800) &
    ! !     call reset_vegn_initial(vegn) 

    ! if(myinterface%params_siml%do_reset_veg) then

    ! if (iyears==myinterface%params_siml%spinupyears + 31)  then
    !   call reset_vegn_initial(vegn)
    ! endif

    ! ! nfrequency = 50 ! 100,75,50,25,15,10 

    ! if(myinterface%params_siml%dist_frequency > 0) then
    !     do i = myinterface%params_siml%spinupyears + 31 + myinterface%params_siml%dist_frequency, &
    !     myinterface%params_siml%spinupyears + myinterface%params_siml%nyeartrend, &
    !     myinterface%params_siml%dist_frequency
    !   if (iyears == i) call reset_vegn_initial(vegn)
    ! enddo
    ! endif

    ! endif

    !----------------------------------------------------------------
    ! Finalize run: deallocating memory
    !----------------------------------------------------------------
    if (myinterface%steering%finalize) then  
      deallocate(vegn)
    end if
    
  end subroutine biosphere_annual

end module md_biosphere_biomee
