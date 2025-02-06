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
    output_annual_luluc_tile      &
  ) bind(C, name = "biomee_f_")
     
    !////////////////////////////////////////////////////////////////
    ! Entrypoint in Fortran for Biomee simulation
    ! Receives simulation parameters and forcing, run the simulation year by year (biosphere_annual()).
    ! The C output arrays are being written on as a side effect in biosphere_annual()
    !----------------------------------------------------------------
    use md_forcing_biomee
    use md_interface_in_biomee
    use vegetation_tile_biomee
    use md_biosphere_biomee
    use md_luluc

    implicit none

    ! mutble state keeping track of simulation state and climate
    type(outtype_steering) :: state
    type(climate_type), dimension(:), allocatable :: climate

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
    real(kind=c_double), dimension(nt_annual,nvars_lu_out, n_lu), intent(out) :: output_annual_luluc_tile

    ! Local variables
    type(vegn_tile_type), dimension(n_lu) :: vegn_tiles ! One tile per LU
    real :: lu_state(n_lu) ! Current LU fractions
    real(kind=c_double) :: nan

    integer :: yr, idx, idx_daily_start, idx_daily_end, lu_idx

    !----------------------------------------------------------------
    ! Initialize outputs to NaN / 0
    !----------------------------------------------------------------

    ! Initialize outputs to NaN
    nan = ieee_value(nan, ieee_quiet_nan)
    output_daily_tile = nan
    output_annual_tile = nan
    output_annual_cohorts = nan

    ! LULUC initializations
    lu_state = real(init_lu(:, 1))
    output_annual_luluc_tile = 0

    ! Allocate climate array
    allocate(climate(inputs%ntstepsyear))

    !----------------------------------------------------------------
    ! Populate interface with arguments from R
    !----------------------------------------------------------------

    call inputs%populate(params_species, init_cohort, init_soil, params_tile, params_siml, site_info)

    !----------------------------------------------------------------
    ! Run simulation
    !----------------------------------------------------------------

    yearloop: do yr=1, inputs%params_siml%steering%runyears
      !----------------------------------------------------------------
      ! Define simulations "steering" variables (forcingyear, etc.)
      !----------------------------------------------------------------
      state = get_steering( yr, inputs%params_siml%steering )

      !----------------------------------------------------------------
      ! Get external (environmental) forcing (for biomee, co2 is in inputs%climate)
      !----------------------------------------------------------------
      ! Get climate variables for this year (full fields and 365 daily values for each variable)
      climate = getclimate( &
         nt, &
         inputs%ntstepsyear, &
         forcing, &
         state%climateyear_idx &
      )

      ! Indices for daily output
      ! Spinup years are not stored, which is why we offset by -spinupyears
      idx_daily_start = (state%year - inputs%params_siml%steering%spinupyears - 1) * ndayyear + 1
      idx_daily_end   = idx_daily_start + ndayyear - 1

      ! For each LU (land unit) whose fraction is > 0
      do lu_idx = 1, n_lu
        if (lu_state(lu_idx) > 0) then

          !----------------------------------------------------------------
          ! Call biosphere (wrapper for all modules, contains time loops)
          !----------------------------------------------------------------
          if (state%spinup) then
            ! If spinup, we do not pass the dailay and cohort output arrays
            call biosphere_annual( &
              state, &
              climate, &
              vegn_tiles(lu_idx), &
              output_annual_tile(state%year, :, lu_idx) &
            )
          else
            idx =  state%year - inputs%params_siml%steering%spinupyears
            call biosphere_annual( &
                    state, &
                    climate, &
                    vegn_tiles(lu_idx), &
                    output_annual_tile(state%year, :, lu_idx), &
                    output_daily_tile(idx_daily_start:idx_daily_end, :, lu_idx), &
                    output_annual_cohorts(:, idx,:, lu_idx) &
                    )
          end if

        end if

      end do

      !----------------------------------------------------------------
      ! Update LULUC state and tiles, and fill output
      !----------------------------------------------------------------
      if ((.not.state%spinup) .and. (state%forcingyear_idx <= n_lu_tr_years)) then

        call update_lu_state(lu_state, real(luc_forcing(:,:,state%forcingyear_idx)), vegn_tiles)

      end if
      call populate_outarray_annual_land_use(state%year, lu_state, output_annual_luluc_tile(state%year,:,:))

    end do yearloop

    !----------------------------------------------------------------
    ! Clean-up allocated memory
    !----------------------------------------------------------------
    deallocate(climate)
    call inputs%shut_down()
    do idx = 1, n_lu
      call vegn_tiles(idx)%shut_down()
    end do

  end subroutine biomee_f

end module biomee_mod
