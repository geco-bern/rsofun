module md_biosphere_biomee
  
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
    ! function BIOSPHERE_annual calculates net ecosystem exchange (nee)
    ! in response to environmental boundary conditions (atmospheric 
    ! CO2, temperature, Nitrogen deposition. This SR "replaces" 
    ! LPJ, also formulated as subroutine.
    ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
    ! contact: b.stocker@imperial.ac.uk
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
         
          ! call getout_hourly( vegn, myinterface%climate(idata), iyears, idoy, i, out_biosphere%hourly_tile(idata) )

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
        ! print*,'1. vegn%annualGPP ', vegn%annualGPP
        
        ! Determine start and end of season and maximum leaf (root) mass
        call vegn_phenology( vegn )
        
        ! Produce new biomass from 'carbon_gain' (is zero afterwards) and continous biomass turnover
        call vegn_growth_EW( vegn )
        
        ! get daily outputs
        ! call getout_daily( vegn, iyears, idoy, out_biosphere%daily_cohorts(doy,:), out_biosphere%daily_tile(doy) )

      end do dayloop

    end do monthloop

    !----------------------------------------------------------------
    ! Annual calls
    !----------------------------------------------------------------
    idoy = 0

    ! print*,'sim. year  ', iyears
    ! print*,'real year: ', year0
    ! print*,'sim. year 2 ', myinterface%steering%year

    if ( myinterface%params_siml%update_annualLAImax ) call vegn_annualLAImax_update( vegn )
    
    !---------------------------------------------
    ! Get annual diagnostics and outputs in once. 
    ! Needs to be called here 
    ! because mortality and reproduction re-organize
    ! cohorts again and we want annual output and daily
    ! output to be consistent with cohort identities.
    !---------------------------------------------

    !print*,'A: vegn%cohorts(:)%nindivs', vegn%cohorts(:)%nindivs
    call annual_diagnostics( vegn, iyears, out_biosphere%annual_cohorts(:), out_biosphere%annual_tile )

    !---------------------------------------------
    ! Reproduction and mortality
    !---------------------------------------------        
    ! Kill all individuals in a cohort if NSC falls below critical point
    ! print*,'B: vegn%cohorts(:)%nindivs', vegn%cohorts(:)%nindivs 
    call vegn_annual_starvation( vegn )
    
    ! Natural mortality (reducing number of individuals 'nindivs')
    ! (~Eq. 2 in Weng et al., 2015 BG)

    ! print*,'C: vegn%cohorts(:)%nindivs', vegn%cohorts(:)%nindivs
    call vegn_nat_mortality( vegn )
    
    ! seed C and germination probability (~Eq. 1 in Weng et al., 2015 BG)
    ! print*,'D: vegn%cohorts(:)%nindivs', vegn%cohorts(:)%nindivs
    call vegn_reproduction( vegn )
    
    !---------------------------------------------
    ! Re-organize cohorts
    !---------------------------------------------

    ! print*,'E: vegn%cohorts(:)%nindivs', vegn%cohorts(:)%nindivs
    call kill_lowdensity_cohorts( vegn )
    
    ! print*,'F: vegn%cohorts(:)%nindivs', vegn%cohorts(:)%nindivs
    call relayer_cohorts( vegn )
    
    ! print*,'G: vegn%cohorts(:)%nindivs', vegn%cohorts(:)%nindivs
    call vegn_mergecohorts( vegn )

    ! call getout_annual( vegn, iyears, out_biosphere%annual_cohorts(:), out_biosphere%annual_tile)

    !---------------------------------------------
    ! Set annual variables zero
    !---------------------------------------------

    !print*,'H: vegn%cohorts(:)%nindivs', vegn%cohorts(:)%nindivs
    call Zero_diagnostics( vegn )
    !print*,'I: vegn%cohorts(:)%nindivs', vegn%cohorts(:)%nindivs

    ! update the years of model run
    iyears = iyears + 1

    ! stop after year 44 for fixed cut-out
    !if (iyears == 45) then
    !  stop
    !end if

    if (myinterface%steering%finalize) then
      !----------------------------------------------------------------
      ! Finazlize run: deallocating memory
      !----------------------------------------------------------------
      deallocate(vegn)

    end if
    
    ! stop
    ! print*,'Done with biosphere for this year. Guete Rutsch!'

  end subroutine biosphere_annual

end module md_biosphere_biomee
