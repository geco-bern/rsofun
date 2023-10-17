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

  subroutine biosphere_annual(out_biosphere)
    !////////////////////////////////////////////////////////////////
    ! Calculates one year of vegetation dynamics. 
    !----------------------------------------------------------------
    use md_interface_biomee, only: myinterface, outtype_biosphere  
    use md_gpp_biomee, only: getpar_modl_gpp

    ! return variable
    type(outtype_biosphere) :: out_biosphere

    ! ! local variables
    integer :: dm, moy, doy
    logical, save :: init = .true.   ! is true only on the first day of the simulation 
    logical, parameter :: verbose = .false.       ! change by hand for debugging etc.

    !----------------------------------------------------------------
    ! Biome-E stuff
    !----------------------------------------------------------------
    integer, parameter :: rand_seed = 86456
    integer, parameter :: totalyears = 10
    integer, parameter :: nCohorts = 1
    real    :: tsoil, soil_theta
    integer :: year0
    integer :: i
    integer :: idata
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
      call initialize_vegn_tile( vegn, nCohorts)
      
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
         
          call hourly_diagnostics( vegn, myinterface%climate(idata), iyears, idoy, i , out_biosphere%hourly_tile(idata))
         
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
        call daily_diagnostics( vegn, iyears, idoy, out_biosphere%daily_cohorts(doy,:), out_biosphere%daily_tile(doy)  )
        
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
    call annual_diagnostics( vegn, iyears, out_biosphere%annual_cohorts(:), out_biosphere%annual_tile )

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
    
    call relayer_cohorts( vegn )
    
    call vegn_mergecohorts( vegn )

    !---------------------------------------------
    ! Set annual variables zero
    !---------------------------------------------
    call Zero_diagnostics( vegn )

    ! update the years of model run
    iyears = iyears + 1

    if (myinterface%steering%finalize) then
      !----------------------------------------------------------------
      ! Finazlize run: deallocating memory
      !----------------------------------------------------------------
      deallocate(vegn)

    end if
    
  end subroutine biosphere_annual

end module md_biosphere_biomee
