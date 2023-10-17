module md_params_siml_biomee
  !////////////////////////////////////////////////////////////////
  ! Module for handling simulation parameters for BiomeE.
  !----------------------------------------------------------------
  implicit none

  private
  public paramstype_siml, getsteering, outtype_steering

  !----------------------------------------------------------------
  ! Derived type for simulation parameters
  !----------------------------------------------------------------
  type paramstype_siml
    
    ! nice sofun parameters:
    integer :: spinupyears     ! number of spinup years
    integer :: nyeartrend      ! number of transient years
    integer :: firstyeartrend  ! year AD of first transient year
    integer :: recycle         ! length of standard recycling period
    logical :: do_spinup       ! whether this simulation does spinup 
    integer :: runyears        ! number of years of entire simulation (spinup+transient)
    logical :: is_calib

    ! integer :: model_run_years
    logical :: outputhourly
    logical :: outputdaily
    logical :: do_U_shaped_mortality
    logical :: update_annualLAImax
    logical :: do_closedN_run

    character(len=30) :: method_photosynth
    character(len=30) :: method_mortality

  end type paramstype_siml

  type outtype_steering
    integer :: year
    integer :: forcingyear     ! year AD for which forcings are read in (=firstyeartrend during spinup)
    integer :: forcingyear_idx ! year index for which forcings are read in (=1 during spinup)
    integer :: climateyear     ! year AD for which climate is read in (recycling during spinup or when climate is held const.)
    integer :: climateyear_idx ! year index for which climate is read in.
    integer :: outyear         ! year AD written to output
    logical :: spinup          ! is true during spinup
    logical :: init            ! is true in first simulation year
    logical :: finalize        ! is true in the last simulation year
    logical :: do_soilequil    ! true in year of analytical soil equilibration (during spinup)
    logical :: average_soil    ! true in years before analytical soil equilibration, when average in and out are taken
    logical :: project_nmin    ! true in all years before analytical soil equilibration, when projected soil N mineralisation is used
    logical :: dofree_alloc    ! true if allocation is not fixed by 'frac_leaf'
    logical :: add_ninorg      ! true in the first few years to get it started
  end type outtype_steering

contains

  function getsteering( year, params_siml ) result( out_steering )
    !////////////////////////////////////////////////////////////////
    ! Gets variables used for steering simulation for each 
    ! simulation year (setting booleans for opening files, doing   
    ! spinup etc.)
    !----------------------------------------------------------------
    use md_params_core, only: dummy

    ! arguments
    integer, intent(in) :: year ! simulation year, starts counting from 1, starting at the beginning of spinup
    type( paramstype_siml ), intent(in) :: params_siml

    ! function return variable
    type( outtype_steering ) :: out_steering

    ! local variables
    integer :: cycleyear

    integer, parameter :: spinupyr_soilequil_1 = 600   ! year of analytical soil equilibration, based on mean litter -> soil input flux
    integer, parameter :: spinupyr_soilequil_2 = 1200  ! year of analytical soil equilibration, based on mean litter -> soil input flux
    integer, parameter :: spinup_add_ninorg    = 100   ! year until which inorganic N is added to get it started

    out_steering%year = year

    if (params_siml%do_spinup) then

      if (year<=spinup_add_ninorg) then
        out_steering%add_ninorg = .true.
      else
        out_steering%add_ninorg = .false.
      end if

      if (year<=params_siml%spinupyears) then
        ! during spinup
        out_steering%spinup = .true.
        out_steering%forcingyear = params_siml%firstyeartrend
        out_steering%forcingyear_idx = 1
        cycleyear = get_cycleyear( year, params_siml%spinupyears, params_siml%recycle )
        out_steering%climateyear = cycleyear + params_siml%firstyeartrend - 1
        out_steering%climateyear_idx = cycleyear

        ! xxx consistency check
        out_steering%forcingyear_idx = MOD(year - 1, params_siml%recycle) + 1
        out_steering%forcingyear     = out_steering%forcingyear_idx + params_siml%firstyeartrend - 1

        out_steering%climateyear_idx = MOD(year - 1, params_siml%recycle) + 1
        out_steering%climateyear     = out_steering%climateyear_idx + params_siml%firstyeartrend - 1

      else  
        ! during transient simulation
        ! TODO xxx Change to MOD in order to run longer transient years
        out_steering%spinup          = .false.
        out_steering%forcingyear_idx =  year - params_siml%spinupyears 
        out_steering%forcingyear     =  out_steering%forcingyear_idx + params_siml%firstyeartrend - 1

        ! constant climate year not specified
        out_steering%climateyear     = out_steering%forcingyear
        out_steering%climateyear_idx = out_steering%forcingyear_idx
      
      endif
      out_steering%outyear = year + params_siml%firstyeartrend - params_siml%spinupyears - 1

      if ( year > 3 ) then
      ! if (year > (spinupyr_soilequil_1 + 1) ) then
      ! if (out_steering%forcingyear > 2003 ) then
        out_steering%dofree_alloc = .true.
      else
        out_steering%dofree_alloc = .false.
      end if

      if ( (year==spinupyr_soilequil_1 .or. year==spinupyr_soilequil_2 ) .and. year<=params_siml%spinupyears) then
        out_steering%do_soilequil = .true.
      else
        out_steering%do_soilequil = .false.
      end if

      if ( year<=params_siml%spinupyears .and. ( year > ( spinupyr_soilequil_1 - params_siml%recycle ) .and. &
           year <= spinupyr_soilequil_1 .or. year > ( spinupyr_soilequil_2 - params_siml%recycle ) .and. &
           year <= spinupyr_soilequil_2 ) ) then
        out_steering%average_soil = .true.
      else
        out_steering%average_soil = .false.
      end if

      if ( year<=params_siml%spinupyears .and. year <= spinupyr_soilequil_1 ) then
        out_steering%project_nmin = .true.
      else
        out_steering%project_nmin = .false.
      end if

    else

      out_steering%dofree_alloc = .false.
      out_steering%do_soilequil = .false.
      out_steering%average_soil = .false.
      out_steering%project_nmin = .false.
      out_steering%forcingyear = year + params_siml%firstyeartrend - 1 
      out_steering%forcingyear_idx = year
      out_steering%climateyear = out_steering%forcingyear
      out_steering%climateyear_idx = out_steering%forcingyear_idx
      out_steering%outyear = year + params_siml%firstyeartrend - 1

    endif

    if (year==1) then
      out_steering%init = .true.
    else
      out_steering%init = .false.
    endif 

    if (year==params_siml%runyears) then
      out_steering%finalize = .true.
    else
      out_steering%finalize = .false.
    end if

  end function getsteering


  function get_cycleyear( year, spinupyears, recycle ) result( cycleyear )
    !////////////////////////////////////////////////////////////////
    ! Returns cycle year for climate recycling, given number of spinup
    ! years and recycle period length, so that in the last year of
    ! of the spinup, 'cycleyear' is equal to 'recycle'.
    !----------------------------------------------------------------
    ! arguments
    integer, intent(in) :: year
    integer, intent(in) :: spinupyears
    integer, intent(in) :: recycle

    ! local variables
    integer :: remainder
    integer :: nfits
    integer :: first_cycleyear

    ! function return variable
    integer :: cycleyear

    remainder = mod( spinupyears, recycle )
    nfits = (spinupyears - remainder) / recycle
    first_cycleyear = recycle - remainder + 1
    cycleyear = modulo( first_cycleyear + year - 1, recycle )  
    if (cycleyear==0) cycleyear = recycle

  end function get_cycleyear

end module md_params_siml_biomee


