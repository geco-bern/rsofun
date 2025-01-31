module md_biosphere_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing loop through time steps within a year and 
  ! calls to SRs for individual processes.
  ! Does not contain any input/output; this is done in SR sofun.
  ! Code adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !----------------------------------------------------------------
  use vegetation_tile_biomee
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
    state, &
    climate, &
    vegn, &
    output_annual_tile, &
    output_daily_tile, &
    output_annual_cohorts &
  )
    !////////////////////////////////////////////////////////////////
    ! Calculates one year of vegetation dynamics. 
    !----------------------------------------------------------------
    use md_interface_in_biomee, only: inputs
    use md_sofunutils, only: aggregate
    use, intrinsic :: iso_c_binding, only: c_double

    ! Input vairables
    type(outtype_steering), intent(in)  :: state
    type(climate_type), dimension(:), allocatable :: climate
    type(vegn_tile_type), intent(inout) :: vegn

    ! Return variables
    real(kind=c_double), dimension(nvars_annual_tile), optional, intent(out) :: output_annual_tile
    real(kind=c_double), dimension(ndayyear, nvars_daily_tile), optional, intent(out) :: output_daily_tile
    real(kind=c_double), dimension(out_max_cohorts, nvars_annual_cohorts), optional, intent(out) :: output_annual_cohorts

    ! Local variables
    integer :: moy         ! Month of year
    integer :: doy         ! Day of year
    integer :: dayloop_idx, fastloop_idx, simu_steps
    real, dimension(ndayyear) :: daily_temp  ! Daily temperatures (average)

    !----------------------------------------------------------------
    ! INITIALISATIONS
    !----------------------------------------------------------------
    if (state%init) then ! is true for the first year

      call vegn%initialize_vegn_tile()

    endif

    !---------------------------------------------
    ! Reset diagnostics and counters
    !---------------------------------------------
    simu_steps = 0 ! fast loop
    doy = 0
    call vegn%zero_diagnostics()

    ! Compute averaged daily temperatures
    call aggregate(daily_temp, climate(:)%Tair, inputs%steps_per_day)

    !----------------------------------------------------------------
    ! LOOP THROUGH MONTHS
    !----------------------------------------------------------------
    monthloop: do moy=1,nmonth

      !----------------------------------------------------------------
      ! LOOP THROUGH DAYS
      !----------------------------------------------------------------
      dayloop: do dayloop_idx=1,ndaymonth(moy)

        doy = doy + 1

        vegn%tc_daily = daily_temp(doy)
        vegn%tc_soil  = air_to_soil_temp(vegn%thetaS(), &
                daily_temp - kTkelvin, &
                doy, &
                state%init, &
                state%finalize &
                )

        !----------------------------------------------------------------
        ! FAST TIME STEP
        !----------------------------------------------------------------
        ! get daily mean temperature from hourly/half-hourly data
        fastloop: do fastloop_idx = 1,inputs%steps_per_day

          simu_steps   = simu_steps + 1

          !----------------------------------------------------------------
          ! Sub-daily time step at resolution given by forcing (can be 1 = daily)
          !----------------------------------------------------------------
          call vegn_CNW_budget( vegn, climate(simu_steps))
         
          call vegn%hourly_diagnostics()
         
        enddo fastloop ! hourly or half-hourly

        ! print*,'-----------day-------------'
        
        !-------------------------------------------------
        ! Daily calls after fast loop
        !-------------------------------------------------

        ! sum over fast time steps and cohorts
        if (present(output_daily_tile)) then
          call daily_diagnostics( vegn, state%year, doy, output_daily_tile(doy,:)  )
        else
          call daily_diagnostics( vegn, state%year, doy  )
        end if

        ! Determine start and end of season and maximum leaf (root) mass
        call vegn_phenology( vegn )
        
        ! Produce new biomass from 'carbon_gain' (is zero afterwards) and continous biomass turnover
        call vegn_growth_EW( vegn )

      end do dayloop

    end do monthloop

    !----------------------------------------------------------------
    ! Annual calls
    !----------------------------------------------------------------

    !---------------------------------------------
    ! Get annual diagnostics and outputs in once. 
    ! Needs to be called here 
    ! because mortality and reproduction re-organize
    ! cohorts again and we want annual output and daily
    ! output to be consistent with cohort identities.
    ! Note: Relayering happens in phenology leading to a reshuffling of the cohorts and affecting cohort identities.
    !---------------------------------------------
    call vegn%annual_diagnostics(state%year, output_annual_tile, output_annual_cohorts )

    !---------------------------------------------
    ! Reproduction and mortality
    !---------------------------------------------        
    ! Kill all individuals in a cohort if NSC falls below critical point
    call vegn_annual_starvation( vegn )
    
    ! Natural mortality (reducing number of individuals 'nindivs')
    ! (~Eq. 2 in Weng et al., 2015 BG)

    call vegn_nat_mortality( vegn )

    call kill_old_grass( vegn )
    
    ! seed C and germination probability (~Eq. 1 in Weng et al., 2015 BG)
    call vegn_reproduction( vegn )
    
    !---------------------------------------------
    ! Re-organize cohorts
    !---------------------------------------------
    call vegn%relayer()

    call vegn%reduce()

    !---------------------------------------------
    ! Update post-mortality metrics
    !---------------------------------------------
    call vegn%annual_diagnostics_post_mortality(output_annual_tile, output_annual_cohorts )

  end subroutine biosphere_annual

end module md_biosphere_biomee

