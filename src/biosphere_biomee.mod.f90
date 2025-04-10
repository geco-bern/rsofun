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
  use md_soiltemp, only: air_to_soil_temp
  
  implicit none
  private
  public biosphere_annual

  type(vegn_tile_type), pointer :: vegn

contains

  subroutine biosphere_annual( &
    out_biosphere_daily_tile, &
    out_biosphere_annual_tile, &
    out_biosphere_annual_cohorts &
    )
    !////////////////////////////////////////////////////////////////
    ! Calculates one year of vegetation dynamics. 
    !----------------------------------------------------------------
    use md_interface_biomee, only: myinterface, &
      outtype_daily_tile, &
      outtype_annual_tile, &
      outtype_annual_cohorts
    use md_gpp_biomee, only: getpar_modl_gpp
    use md_sofunutils, only: aggregate

    ! return variables
    type(outtype_daily_tile),     dimension(ndayyear)                , intent(out) :: out_biosphere_daily_tile
    type(outtype_annual_tile)                                        , intent(out) :: out_biosphere_annual_tile
    type(outtype_annual_cohorts), dimension(out_max_cohorts)         , intent(out) :: out_biosphere_annual_cohorts

    ! ! local variables
    integer :: moy         ! Month of year
    integer :: doy         ! Day of year
    integer :: dayloop_idx, fastloop_idx, simu_steps
    logical, save :: init  ! is true only on the first step of the simulation
    real, dimension(ndayyear) :: daily_temp  ! Daily temperatures (average)
    real    :: tsoil
    integer, save :: iyears, idoy

    !----------------------------------------------------------------
    ! INITIALISATIONS
    !----------------------------------------------------------------
    if (myinterface%steering%init) then ! is true for the first year

      ! Parameter initialization: Initialize PFT parameters
      call initialize_PFT_data()

      ! Initialize vegetation tile and plant cohorts
      allocate( vegn )
      call initialize_vegn_tile( vegn )

      ! module-specific parameter specification
      call getpar_modl_gpp()

      iyears = 1
      idoy   = 0
      init = .true.

    endif

    !---------------------------------------------
    ! Reset diagnostics and counters
    !---------------------------------------------
    simu_steps = 0
    doy = 0
    call Zero_diagnostics( vegn )

    ! Compute averaged daily temperatures
    call aggregate(daily_temp, myinterface%climate(:)%Tair, myinterface%steps_per_day)

    !----------------------------------------------------------------
    ! LOOP THROUGH MONTHS
    !----------------------------------------------------------------
    monthloop: do moy=1,nmonth

      !----------------------------------------------------------------
      ! LOOP THROUGH DAYS
      !----------------------------------------------------------------
      dayloop: do dayloop_idx=1,ndaymonth(moy)

        doy = doy + 1
        idoy = idoy + 1

        ! print*,'----------------------'
        ! print*,'YEAR, DOY ', myinterface%steering%year, doy
        ! print*,'----------------------'

        ! The algorithm for computing soil temp from air temp works with a daily period.
        ! vegn%wcl(2) is updated in the fast loop, but not much so it is ok to use
        ! the last value of the previous day for computing the daily soil temperature.
        vegn%thetaS  = (vegn%wcl(2) - WILTPT) / (FLDCAP - WILTPT)
        tsoil = air_to_soil_temp(vegn%thetaS, &
                daily_temp - kTkelvin, &
                doy, &
                myinterface%steering%init, &
                myinterface%steering%finalize &
                ) + kTkelvin

        !----------------------------------------------------------------
        ! FAST TIME STEP
        !----------------------------------------------------------------
        ! get daily mean temperature from hourly/half-hourly data
        fastloop: do fastloop_idx = 1,myinterface%steps_per_day

          simu_steps    = simu_steps + 1
          vegn%thetaS  = (vegn%wcl(2) - WILTPT) / (FLDCAP - WILTPT)

          !----------------------------------------------------------------
          ! Sub-daily time step at resolution given by forcing (can be 1 = daily)
          !----------------------------------------------------------------
          call vegn_CNW_budget( vegn, myinterface%climate(simu_steps), init, tsoil )
         
          call hourly_diagnostics( vegn, myinterface%climate(simu_steps) )
         
          init = .false.
         
        enddo fastloop ! hourly or half-hourly

        ! print*,'-----------day-------------'
        
        !-------------------------------------------------
        ! Daily calls after fast loop
        !-------------------------------------------------
        vegn%Tc_daily = daily_temp(doy)

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
    ! Note: Relayering happens in phenology leading to a reshuffling of the cohorts and affecting cohort identities.
    !---------------------------------------------
    call annual_diagnostics( vegn, iyears, out_biosphere_annual_cohorts, out_biosphere_annual_tile )

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
    ! Update post-mortality metrics
    !---------------------------------------------
    call annual_diagnostics_post_mortality( vegn, out_biosphere_annual_cohorts, out_biosphere_annual_tile )

    ! update the years of model run
    iyears = iyears + 1

    !----------------------------------------------------------------
    ! Finalize run: deallocating memory
    !----------------------------------------------------------------
    if (myinterface%steering%finalize) then
      deallocate(vegn)
    end if
    
  end subroutine biosphere_annual

  subroutine initialize_PFT_data()

    ! ---- local vars ------
    integer :: i

    associate(spdata => myinterface%params_species)

      spdata%prob_g        = 1.0
      spdata%prob_e        = 1.0

      spdata%underLAImax = spdata%LAImax

      ! specific root area
      spdata%SRA           = 2.0/(spdata%root_r*spdata%rho_FR)

      ! calculate alphaBM parameter of allometry. note that rho_wood was re-introduced for this calculation
      spdata%alphaBM = spdata%rho_wood * spdata%taperfactor * PI/4. * spdata%alphaHT ! 5200

      ! Vmax as a function of LNbase
      spdata%Vmax = 0.02 * spdata%LNbase ! 0.03125 * sp%LNbase ! Vmax/LNbase= 25E-6/0.8E-3 = 0.03125 !

      ! CN0 of leaves
      spdata%LNA     = spdata%LNbase +  spdata%LMA/spdata%CNleafsupport
      spdata%CNleaf0 = spdata%LMA/spdata%LNA
      ! Leaf life span as a function of LMA
      spdata%leafLS = c_LLS * spdata%LMA

      do i = 1, size(spdata)
        call init_derived_species_data(spdata(i))
      enddo

    end associate

  end subroutine initialize_pft_data


  subroutine init_derived_species_data(sp)

    type(spec_data_type), intent(inout) :: sp

    ! ---- local vars ------
    integer :: j
    real :: rdepth(0:MAX_LEVELS)
    real :: residual

    ! root vertical profile
    rdepth=0.0
    do j=1,MAX_LEVELS
      rdepth(j) = rdepth(j-1)+thksl(j)
      sp%root_frac(j) = exp(-rdepth(j-1)/sp%root_zeta)- &
              exp(-rdepth(j)  /sp%root_zeta)
    enddo
    residual = exp(-rdepth(MAX_LEVELS)/sp%root_zeta)
    do j=1,MAX_LEVELS
      sp%root_frac(j) = sp%root_frac(j) + residual*thksl(j)/rdepth(MAX_LEVELS)
    enddo

    if(sp%leafLS>1.0)then
      sp%phenotype = 1
    else
      sp%phenotype = 0
    endif

    ! Leaf turnover rate, (leaf longevity as a function of LMA)
    sp%alpha_L = 1.0/sp%leafLS * sp%phenotype

  end subroutine init_derived_species_data

end module md_biosphere_biomee

