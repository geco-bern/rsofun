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
      ! print*,'1: vegn%cohorts ', vegn%n_cohorts
      call initialize_PFT_data()

      ! print

      ! Initialize vegetation tile and plant cohorts
      allocate(vegn)

      ! print*,'2: vegn%cohorts, vegn%CAI, vegn%LAI ', vegn%n_cohorts, vegn%CAI, vegn%LAI 
      call initialize_vegn_tile(vegn,nCohorts)
      
      ! Sort and relayer cohorts

      ! print*,'3: vegn%cohorts, vegn%CAI, vegn%LAI ', vegn%n_cohorts, vegn%CAI, vegn%LAI 
      call relayer_cohorts(vegn)

      ! print*,'4: vegn%cohorts, vegn%CAI, vegn%LAI ', vegn%n_cohorts, vegn%CAI, vegn%LAI 
      call Zero_diagnostics(vegn)

      year0 = myinterface%climate(1)%year  ! forcingData(1)%year
      iyears = 1
      idoy = 0
      idays  = 0

    endif 

    simu_steps = 0

    ! print*,'year0: ', myinterface%climate(1)%year

    !----------------------------------------------------------------
    ! INITIALISATIONS OF OUTPUT 
    ! important because maximum cohorts may be bigger than active cohorts
    !----------------------------------------------------------------
      ! out_biosphere%daily_cohorts(:,:)%year    = 0.0
      ! out_biosphere%daily_cohorts(:,:)%doy     = 0.0
      ! out_biosphere%daily_cohorts(:,:)%hour    = 0.0
      ! out_biosphere%daily_cohorts(:,:)%cID     = 0.0
      ! out_biosphere%daily_cohorts(:,:)%PFT     = 0.0
      ! out_biosphere%daily_cohorts(:,:)%layer   = 0.0
      ! out_biosphere%daily_cohorts(:,:)%density = 0.0
      ! out_biosphere%daily_cohorts(:,:)%f_layer = 0.0
      ! out_biosphere%daily_cohorts(:,:)%LAI     = 0.0
      ! out_biosphere%daily_cohorts(:,:)%gpp     = 0.0
      ! out_biosphere%daily_cohorts(:,:)%resp    = 0.0
      ! out_biosphere%daily_cohorts(:,:)%transp  = 0.0
      ! out_biosphere%daily_cohorts(:,:)%NPPleaf = 0.0
      ! out_biosphere%daily_cohorts(:,:)%NPProot = 0.0
      ! out_biosphere%daily_cohorts(:,:)%NPPwood = 0.0
      ! out_biosphere%daily_cohorts(:,:)%NSC     = 0.0
      ! out_biosphere%daily_cohorts(:,:)%seedC   = 0.0
      ! out_biosphere%daily_cohorts(:,:)%leafC   = 0.0
      ! out_biosphere%daily_cohorts(:,:)%rootC   = 0.0
      ! out_biosphere%daily_cohorts(:,:)%SW_C    = 0.0
      ! out_biosphere%daily_cohorts(:,:)%HW_C    = 0.0
      ! out_biosphere%daily_cohorts(:,:)%NSN     = 0.0
      ! out_biosphere%daily_cohorts(:,:)%seedN   = 0.0
      ! out_biosphere%daily_cohorts(:,:)%leafN   = 0.0
      ! out_biosphere%daily_cohorts(:,:)%rootN   = 0.0
      ! out_biosphere%daily_cohorts(:,:)%SW_N    = 0.0
      ! out_biosphere%daily_cohorts(:,:)%HW_N    = 0.0

      ! out_biosphere%annual_cohorts(:)%year)    = 0.0
      ! out_biosphere%annual_cohorts(:)%cID)     = 0.0
      ! out_biosphere%annual_cohorts(:)%PFT)     = 0.0
      ! out_biosphere%annual_cohorts(:)%layer)   = 0.0
      ! out_biosphere%annual_cohorts(:)%density) = 0.0
      ! out_biosphere%annual_cohorts(:)%f_layer) = 0.0
      ! out_biosphere%annual_cohorts(:)%dDBH)    = 0.0
      ! out_biosphere%annual_cohorts(:)%dbh)     = 0.0
      ! out_biosphere%annual_cohorts(:)%height)  = 0.0
      ! out_biosphere%annual_cohorts(:)%Acrown)  = 0.0
      ! out_biosphere%annual_cohorts(:)%wood)    = 0.0
      ! out_biosphere%annual_cohorts(:)%nsc)     = 0.0
      ! out_biosphere%annual_cohorts(:)%NSN)     = 0.0
      ! out_biosphere%annual_cohorts(:)%NPPtr)   = 0.0
      ! out_biosphere%annual_cohorts(:)%seed)    = 0.0
      ! out_biosphere%annual_cohorts(:)%NPPL)    = 0.0
      ! out_biosphere%annual_cohorts(:)%NPPR)    = 0.0
      ! out_biosphere%annual_cohorts(:)%NPPW)    = 0.0
      ! out_biosphere%annual_cohorts(:)%GPP)     = 0.0
      ! out_biosphere%annual_cohorts(:)%NPP)     = 0.0
      ! out_biosphere%annual_cohorts(:)%N_uptk)  = 0.0
      ! out_biosphere%annual_cohorts(:)%N_fix)   = 0.0
      ! out_biosphere%annual_cohorts(:)%maxLAI)  = 0.0
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
