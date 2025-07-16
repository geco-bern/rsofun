module md_biosphere_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing loop through time steps within a year and 
  ! calls to SRs for individual processes.
  ! Does not contain any input/output; this is done in SR sofun.
  ! Code adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !----------------------------------------------------------------
  use md_vegetation_tile_biomee
  use md_vegetation_processes_biomee
  use md_soil_biomee
  use md_forcing_biomee
  use md_soiltemp, only: air_to_soil_temp
  use, intrinsic :: iso_c_binding, only: c_double
  
  implicit none
  private
  public biosphere_annual

contains

  subroutine biosphere_annual( &
    steering_state, &
    climate, &
    vegn &
  )
    !////////////////////////////////////////////////////////////////
    ! Calculates one year of vegetation dynamics. 
    !----------------------------------------------------------------
    use md_interface_in_biomee, only: inputs
    use md_sofunutils, only: aggregate
    use, intrinsic :: iso_c_binding, only: c_double

    ! Input vairables
    type(outtype_steering), intent(in)  :: steering_state
    type(climate_type), intent(in), dimension(:) :: climate ! Dimension is ndayyear * steps_per_day
    type(vegn_tile_type), intent(inout) :: vegn

    ! Local variables
    integer :: doy         ! Day of year
    integer :: dayloop_idx, fastloop_idx, simu_steps
    real, dimension(ndayyear) :: daily_temp  ! Daily temperatures (average) in Kelvin
    real, dimension(size(climate)) :: tair

    tair = climate(:)%TairK

    !----------------------------------------------------------------
    ! INITIALISATIONS
    !----------------------------------------------------------------
    ! Compute averaged daily temperatures
    call aggregate(daily_temp, tair, inputs%steps_per_day)

    !===== Reset diagnostics and counters
    simu_steps = 0 ! fast loop
    doy = 0
    call vegn%zero_diagnostics()

    !----------------------------------------------------------------
    ! LOOP THROUGH DAYS
    !----------------------------------------------------------------
    dayloop: do dayloop_idx=1,ndayyear

      doy = doy + 1

      ! Compute daily air and soil temperature
      vegn%tk_daily = daily_temp(doy)
      vegn%tc_soil  = air_to_soil_temp( &
              vegn%thetaS(), &
              daily_temp - kTkelvin, &
              doy, &
              vegn%dtemp_pvy, &
              vegn%wscal_pvy, &
              vegn%wscal_alldays &
      )

      !----------------------------------------------------------------
      ! FAST TIME STEP
      !----------------------------------------------------------------
      ! get daily mean temperature from hourly/half-hourly data
      fastloop: do fastloop_idx = 1,inputs%steps_per_day

        simu_steps   = simu_steps + 1

        call vegn_CNW_budget( vegn, climate(simu_steps))

        call vegn%hourly_diagnostics()

      enddo fastloop ! hourly or half-hourly

      !-------------------------------------------------
      ! Daily calls after fast loop
      !-------------------------------------------------

      ! sum over fast time steps and cohorts
      call daily_diagnostics( vegn, steering_state%year, doy, steering_state%daily_reporting)

      ! Determine start and end of season and maximum leaf (root) mass
      call vegn_phenology( vegn )

      ! Produce new biomass from 'carbon_gain' (is zero afterwards) and continous biomass turnover
      call vegn_growth_EW( vegn )

    end do dayloop

    !----------------------------------------------------------------
    ! Annual calls
    !----------------------------------------------------------------

    !===== Get annual diagnostics
    call vegn%annual_diagnostics(steering_state%year, steering_state%cohort_reporting)

    !===== Reproduction and mortality
    ! Kill all individuals in a cohort if NSC falls below critical point
    call vegn_annual_starvation( vegn )
    
    ! Natural mortality (reducing number of individuals 'density')
    ! (~Eq. 2 in Weng et al., 2015 BG)
    call vegn_nat_mortality( vegn )
    call kill_old_grass( vegn )
    
    ! seed C and germination probability (~Eq. 1 in Weng et al., 2015 BG)
    call vegn_reproduction( vegn )

    !===== Re-organize cohorts
    call vegn%relayer()
    call vegn%reduce()

    !===== Update post-mortality metrics
    call vegn%annual_diagnostics_post_mortality(steering_state%cohort_reporting)

  end subroutine biosphere_annual

end module md_biosphere_biomee

