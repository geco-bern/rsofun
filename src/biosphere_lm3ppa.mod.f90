module md_biosphere_lm3ppa

  use datatypes
  use md_vegetation_lm3ppa
  use md_soil_lm3ppa
  use md_params_core_lm3ppa

  implicit none

  private
  public biosphere_annual

   type(vegn_tile_type),  pointer :: vegn   
   type(soil_tile_type),  pointer :: soil
   type(cohort_type),     pointer :: cx, cc

contains

  ! function biosphere_annual() result( out_biosphere ) ! in the original it was a function
  subroutine biosphere_annual(out_biosphere)
    !////////////////////////////////////////////////////////////////
    ! function BIOSPHERE_annual calculates net ecosystem exchange (nee)
    ! in response to environmental boundary conditions (atmospheric 
    ! CO2, temperature, Nitrogen deposition. This SR "replaces" 
    ! LPJ, also formulated as subroutine.
    ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
    ! contact: b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    use md_interface_lm3ppa, only: myinterface, outtype_biosphere
    use md_params_core_lm3ppa, only: ntstepsyear
  
    ! return variable
    type(outtype_biosphere) :: out_biosphere

    ! ! local variables
    integer :: dm, moy, doy
    ! logical, save           :: init_daily = .true.   ! is true only on the first day of the simulation 
    logical, parameter :: verbose = .false.       ! change by hand for debugging etc.

    !----------------------------------------------------------------
    ! Biome-E stuff
    !----------------------------------------------------------------
    integer, parameter :: rand_seed = 86456
    integer, parameter :: totalyears = 10
    integer, parameter :: nCohorts = 1
    real    :: tsoil, soil_theta
    integer :: year0
    integer :: i, j
    integer :: idata
    integer, save :: simu_steps !, datalines
    integer, save :: iyears
    integer, save :: idays
    integer, save :: idoy
    ! character(len=50) :: namelistfile = '~/sofun/params/parameters_Allocation.nml' !'parameters_WC_biodiversity.nml' ! 'parameters_CN.nml'

    ! print*,'year0: ', myinterface%climate(1)%year
    ! print*,'climateyear_idx climateyear: ', myinterface%steering%climateyear_idx, myinterface%steering%climateyear

    !----------------------------------------------------------------
    ! INITIALISATIONS
    !----------------------------------------------------------------
    if (myinterface%steering%init) then

      ! allocate(out_biosphere%hourly_tile(ntstepsyear)) !xxxdebug

      ! Parameter initialization: Initialize PFT parameters

      call initialize_PFT_data()

      ! Initialize vegetation tile and plant cohorts
      allocate(vegn)

      ! print*,'2: vegn%cohorts, vegn%CAI, vegn%LAI ', vegn%n_cohorts, vegn%CAI, vegn%LAI 
      ! print*, '0.2 vegn%totNewCC, vegn%totNewCN', vegn%totNewCC, vegn%totNewCN

      call initialize_vegn_tile(vegn,nCohorts)
      
      ! Sort and relayer cohorts

      ! print*,'3: vegn%cohorts, vegn%CAI, vegn%LAI ', vegn%n_cohorts, vegn%CAI, vegn%LAI 
      ! print*, '0.3 vegn%totNewCC, vegn%totNewCN', vegn%totNewCC, vegn%totNewCN

      call relayer_cohorts(vegn)

      ! print*,'4: vegn%cohorts, vegn%CAI, vegn%LAI ', vegn%n_cohorts, vegn%CAI, vegn%LAI 
      ! print*, '0.4 vegn%totNewCC, vegn%totNewCN', vegn%totNewCC, vegn%totNewCN

      call Zero_diagnostics(vegn)

      ! print*, '0.5 vegn%totNewCC, vegn%totNewCN', vegn%totNewCC, vegn%totNewCN

      year0 = myinterface%climate(1)%year  ! forcingData(1)%year
      iyears = 1
      idoy = 0
      idays  = 0

    endif 

    ! print*, 'year', myinterface%climate(1)%year
    ! print*, 'doy',myinterface%climate(1)%doy
    ! print*, 'hour',myinterface%climate(1)%hod
    ! print*, 'PAR',myinterface%climate(1)%PAR
    ! print*, 'radiation',myinterface%climate(1)%radiation
    ! print*, 'Tair', myinterface%climate(1)%Tair
    ! print*, 'Tsoil', myinterface%climate(1)%Tsoil
    ! print*, 'windU', myinterface%climate(1)%windU
    ! print*, 'P_air', myinterface%climate(1)%P_air
    ! print*, 'CO2', myinterface%climate(1)%CO2
    ! print*, 'soilwater', myinterface%climate(1)%soilwater

    simu_steps = 0

    ! print*,'year0: ', myinterface%climate(1)%year

    !----------------------------------------------------------------
    ! LOOP THROUGH MONTHS
    !----------------------------------------------------------------
    doy = 0
    monthloop: do moy=1,nmonth

      !----------------------------------------------------------------
      ! LOOP THROUGH DAYS
      !----------------------------------------------------------------
      ! print*,'5: vegn%cohorts , vegn%CAI, vegn%LAI', vegn%n_cohorts, vegn%CAI, vegn%LAI
      dayloop: do dm=1,ndaymonth(moy)
        
        doy = doy + 1

        if (verbose) print*,'----------------------'
        if (verbose) print*,'YEAR, DOY ', myinterface%steering%year, doy
        if (verbose) print*,'----------------------'

        idoy = idoy + 1

        !----------------------------------------------------------------
        ! Get daily mean air and soil temperature
        !----------------------------------------------------------------

        ! get daily mean temperature from hourly/half-hourly data
        vegn%Tc_daily = 0.0
        tsoil         = 0.0

        fastloop: do i=1,myinterface%steps_per_day

          ! idata = MOD(simu_steps, myinterface%datalines)+1
          idata = simu_steps + 1
          year0 =  myinterface%climate(idata)%year  ! Current year
          vegn%Tc_daily = vegn%Tc_daily +  myinterface%climate(idata)%Tair

          tsoil         = myinterface%climate(idata)%tsoil
          simu_steps    = simu_steps + 1

          call vegn_CNW_budget_fast(vegn, myinterface%climate(idata))
        
          !print*,'idata, size', idata, size(out_biosphere%hourly_tile)
          call hourly_diagnostics(vegn, myinterface%climate(idata), iyears, idoy, i, out_biosphere%hourly_tile(idata) )

        enddo fastloop ! hourly or half-hourly

        vegn%Tc_daily = vegn%Tc_daily/myinterface%steps_per_day
        tsoil         = tsoil/myinterface%steps_per_day
        soil_theta    = vegn%thetaS

        !-------------------------------------------------
        ! Daily calls
        !-------------------------------------------------
        ! print*,'5: vegn%cohorts , vegn%CAI, vegn%LAI', vegn%n_cohorts, vegn%CAI, vegn%LAI
        call daily_diagnostics(vegn, myinterface%climate(idata), iyears, idoy, out_biosphere%daily_cohorts(doy,:), out_biosphere%daily_tile(doy) )

        ! Determine start and end of season and maximum leaf (root) mass
        ! print*,'6: vegn%cohorts , vegn%CAI, vegn%LAI', vegn%n_cohorts, vegn%CAI, vegn%LAI
        call vegn_phenology(vegn, j)

        ! Produce new biomass from 'carbon_gain' (is zero afterwards)
        ! print*,'7: vegn%cohorts , vegn%CAI, vegn%LAI', vegn%n_cohorts, vegn%CAI, vegn%LAI
        call vegn_growth_EW(vegn)

        !----------------------------------------------------------------
        ! populate function return variable
        !----------------------------------------------------------------

      end do dayloop

    end do monthloop


    !----------------------------------------------------------------
    ! Annual calls
    !----------------------------------------------------------------
     idoy = 0

    print*,'sim. year  ', iyears
    print*,'real year: ', year0

    ! call vegn_annualLAImax_update(vegn) ! Before without conditional
    if ( myinterface%params_siml%update_annualLAImax ) call vegn_annualLAImax_update(vegn) !xxx debugging

    ! print*,'8: vegn%cohorts , vegn%CAI, vegn%LAI', vegn%n_cohorts, vegn%CAI, vegn%LAI
    call annual_diagnostics(vegn, iyears, out_biosphere%annual_cohorts(:), out_biosphere%annual_tile)

    !---------------------------------------------
    ! Reproduction and mortality
    !---------------------------------------------        
    ! Kill all individuals in a cohort if NSC falls below critical point
    call vegn_annual_starvation(vegn)
     ! print*,'5: ', vegn%n_cohorts

    ! Natural mortality (reducing number of individuals 'nindivs')
    ! (~Eq. 2 in Weng et al., 2015 BG)
    call vegn_nat_mortality(vegn, real( seconds_per_year ))
     ! print*,'6: ', vegn%n_cohorts

    ! seed C and germination probability (~Eq. 1 in Weng et al., 2015 BG)
    call vegn_reproduction(vegn)
     ! print*,'7: ', vegn%n_cohorts

    !---------------------------------------------
    ! Re-organize cohorts
    !---------------------------------------------
    call kill_lowdensity_cohorts(vegn)
    ! print*,'8: ', vegn%n_cohorts
    call relayer_cohorts(vegn)
    ! print*,'9: ', vegn%n_cohorts
    call vegn_mergecohorts(vegn)
    ! print*,'10: ', vegn%n_cohorts

    ! !---------------------------------------------
    ! ! Set annual variables zero
    ! !---------------------------------------------
    call Zero_diagnostics(vegn)
    ! print*,'11: ', vegn%n_cohorts

    ! update the years of model run
    iyears = iyears + 1

    if (myinterface%steering%finalize) then
      !----------------------------------------------------------------
      ! Finazlize run: deallocating memory
      !----------------------------------------------------------------
      deallocate(vegn%cohorts)

    end if

    if (verbose) print*,'Done with biosphere for this year. Guete Rutsch!'

  end subroutine biosphere_annual

end module md_biosphere_lm3ppa
