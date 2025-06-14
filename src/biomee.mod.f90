module biomee_mod
  !////////////////////////////////////////////////////////////////
  ! Module containing the entrypoint subroutine in Fortran for Biomee simulation
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
    output_annual_aggregated      &
  ) bind(C, name = "biomee_f_")
     
    !////////////////////////////////////////////////////////////////
    ! Entrypoint in Fortran for Biomee simulation
    ! Receives simulation parameters and forcing, run the simulation year by year (biosphere_annual()).
    ! The C output arrays are being written at the end of each yearly loop
    !----------------------------------------------------------------
    use md_forcing_biomee
    use md_interface_in_biomee
    use md_biosphere_biomee
    use md_aggregated_tile_biomee
    use md_params_core
    use md_vegetation_tile_biomee

    implicit none

    ! mutable state keeping track of simulation steering and climate
    type(outtype_steering) :: steering_state
    type(climate_type), dimension(:), allocatable :: climate

    ! Array dimensions
    integer(kind=c_int), intent(in) :: nt                ! Forcing array dimension
    integer(kind=c_int), intent(in) :: nt_daily          ! Number of simulated days (0 for no daily output)
    integer(kind=c_int), intent(in) :: nt_annual         ! Number of years (spinup + transient)
    integer(kind=c_int), intent(in) :: nt_annual_trans   ! Number of transient years

    ! Naked arrays
    integer(kind=c_int), intent(in) :: n_params_species
    real(kind=c_double), dimension(n_params_species, nvars_params_species), intent(in) :: params_species
    integer(kind=c_int), intent(in) :: n_init_cohort
    real(kind=c_double), dimension(n_init_cohort,nvars_init_cohorts),  intent(in)  :: init_cohort
    real(kind=c_double), dimension(nvars_init_soil),   intent(in)  :: init_soil
    real(kind=c_double), dimension(nvars_params_tile), intent(in) :: params_tile
    real(kind=c_double), dimension(nvars_params_siml), intent(in) :: params_siml
    real(kind=c_double), dimension(nvars_site_info),   intent(in)  :: site_info
    real(kind=c_double), dimension(nt,nvars_forcing),  intent(in)  :: forcing

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
    real(kind=c_double), dimension(nt_annual,nvars_aggregated_out), intent(out) :: output_annual_aggregated

    ! Local state
    type(aggregated_tile) :: aggregat

    ! Local variables
    real(kind=c_double) :: nan
    integer :: yr, idx, idx_daily_start, idx_daily_end, lu_idx
    type(orgpool) :: export

    !----------------------------------------------------------------
    ! Initialize outputs to NaN / 0
    !----------------------------------------------------------------

    ! Initialize outputs to NaN
    nan = ieee_value(nan, ieee_quiet_nan)
    output_daily_tile = nan
    output_annual_tile = nan
    output_annual_cohorts = nan
    output_annual_aggregated = nan

    ! Allocate climate array
    allocate(climate(inputs%ntstepsyear))

    !----------------------------------------------------------------
    ! Populate interface with arguments from R
    !----------------------------------------------------------------

    call inputs%populate(params_species, init_cohort, init_soil, params_tile, params_siml, site_info, init_lu)

    ! Initialize tile
    call aggregat%initialize(inputs%init_lu(:)%fraction)

    !----------------------------------------------------------------
    ! Run simulation
    !----------------------------------------------------------------
    yearloop: do yr=1, inputs%params_siml%steering_input%runyears
      !----------------------------------------------------------------
      ! Define simulations "steering" variables (indices for forcing, but also output flags)
      !----------------------------------------------------------------
      steering_state = get_steering( yr, inputs%params_siml%steering_input )

      !----------------------------------------------------------------
      ! Get external (environmental) forcing (for biomee, co2 is in inputs%climate)
      !----------------------------------------------------------------
      ! Get climate variables for this year (full fields and 365 daily values for each variable)
      climate = getclimate( &
         nt, &
         inputs%ntstepsyear, &
         forcing, &
         steering_state%climateyear_idx &
      )

      !----------------------------------------------------------------
      ! Update LU state using LUC forcing if we are in transient simulation step
      !----------------------------------------------------------------
      if ((.not.steering_state%spinup) .and. (steering_state%forcingyear_idx <= n_lu_tr_years)) then
        export = aggregat%update_lu_fractions(real(luc_forcing(:,:,steering_state%forcingyear_idx)))
        ! 'export' is kg C / m2 of grid cell (i.e landscape)
      else
        export = orgpool()
      end if
      ! Update product pools
      call aggregat%prod_pools%update(export)

      ! For each non-empty LU (land unit)
      foreach_lu: do lu_idx = 1, n_lu
        associate (lu => aggregat%tiles(lu_idx))
          if (lu%non_empty()) then

            !----------------------------------------------------------------
            ! Call biosphere (wrapper for all modules, contains time loops)
            !----------------------------------------------------------------
            call biosphere_annual(steering_state, climate, lu%vegn)

          end if
        end associate

      end do foreach_lu

      !----------------------------------------------------------------
      ! Fill outputs
      ! We conditionally pass daily and cohorts arrays
      !----------------------------------------------------------------
      call aggregat%populate_outarrays( &
          output_annual_aggregated(steering_state%year,:), &
          output_annual_tile(steering_state%year, :, :))

      if (steering_state%cohort_reporting) then
        idx = steering_state%cohort_report_idx
        call aggregat%populate_outcohorts(output_annual_cohorts(:, idx, :, :))
      end if
      if (steering_state%daily_reporting) then
        idx_daily_start = steering_state%daily_report_idx
        idx_daily_end   = idx_daily_start + ndayyear - 1
        call aggregat%populate_outdaily(output_daily_tile(idx_daily_start:idx_daily_end, :, :))
      end if

    end do yearloop

    !----------------------------------------------------------------
    ! Clean-up allocated memory
    !----------------------------------------------------------------
    deallocate(climate)
    call inputs%shut_down()
    call aggregat%shut_down()

  end subroutine biomee_f

end module biomee_mod
