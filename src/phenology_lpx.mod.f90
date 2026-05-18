module md_phenology
  !////////////////////////////////////////////////////////////////
  ! TEMPERATURE-DRIVEN PHENOLOGY 
  ! Adopted from LPX-Bern
  ! Contains the "main" subroutine 'phenology and phenology' and all 
  ! necessary subroutines for handling input/output. 
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------  
  use md_params_core, only: ndayyear, maxgrid, nmonth, middaymonth, npft, nlu, eps, kGsc
  use md_sofunutils, only: daily2monthly, monthly2daily
  use md_tile_cnmodel
  use md_plant_cnmodel
  
  implicit none

  private
  public phenology, phenology_daily, getpar_modl_phenology

  !----------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------
  type paramstype_pheno
    real :: gddbase  ! GDD base, for PFT11-14, a T0 is chosen to be 0deg C (Prentice et al. 1992, J.o.Biogeography), pftpar(pft,33) in LPX
  end type paramstype_pheno

  type( paramstype_pheno ) :: params_pheno

  type pftparamstype_pheno
    real    :: kphio_par_a   ! mid-point of hardening state vs. tmin (deg C)
    real    :: kphio_par_b   ! slope of hardening state vs. tmin (deg C-1)
    real    :: kphio_par_c   ! mid-point of dehardening state vs. GDD (deg C)
    real    :: kphio_par_d   ! slope of dehardening state vs. GDD (deg C-1)
    real    :: kphio_par_e   ! base temperature for GDD summation (deg C)
    real    :: ramp          ! summergreen phenology ramp, GDD requirement to grow full leaf canopy
    logical :: evergreen
    logical :: summergreen
    logical :: raingreen  
  end type pftparamstype_pheno

  type( pftparamstype_pheno ), dimension(npft) :: params_pft_pheno


contains

  subroutine phenology_daily( tile, dtemp, dtmin, tile_fluxes )
    !//////////////////////////////////////////////////////////
    ! Defines temperature-dependent phenology (hardening/de-
    ! hardening), updated each day (within daily loop).
    !----------------------------------------------------------
    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    real, intent(in) :: dtemp    ! daily mean temperature (deg C)
    real, intent(in) :: dtmin    ! daily minimum temperature (deg C)

    ! local variables
    real, dimension(nlu,npft), save :: dra_save
    logical, save :: firstcall = .true.
    real :: level_veggrowth
    real :: diff_dra
    integer :: pft, lu

    ! xxx debug
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    real, save :: tmp = 0.0

    pftloop: do pft=1,npft
      lu = params_pft_plant(pft)%lu_category

      !----------------------------------------------------------------
      ! Low-temperature effect on quantum yield efficiency and grass growth
      !----------------------------------------------------------------
      call calc_ftemp_kphio_coldhard( &
        dtemp, &
        dtmin, &
        tile(lu)%plant(pft)%pheno%level_coldacclim, &   ! value updated (inout)
        tile(lu)%gdd, &                                 ! value updated (inout)
        params_pft_pheno(pft)%kphio_par_a, &
        params_pft_pheno(pft)%kphio_par_b, &
        params_pft_pheno(pft)%kphio_par_c, &
        params_pft_pheno(pft)%kphio_par_d, &
        params_pft_pheno(pft)%kphio_par_e &
      )

      tile_fluxes(lu)%plant(pft)%debug1 = tile(lu)%plant(pft)%pheno%level_coldacclim

      ! ! xxx try: for schematic step-change simulations
      ! tile(lu)%plant(pft)%pheno%level_coldacclim = 1.0

      !----------------------------------------------------------------
      ! Insolation-driven phenophases of grass growth (switching from 
      ! vegetative growth to seed filling and back)
      ! Top-of-atmosphere solar radiation (~day length) triggers the 
      ! seed-filling vs. vegetative growth phenophases.
      !----------------------------------------------------------------
      ! Apply low-pass filter on TOA-radiation
      if (firstcall) then
        dra_save(lu,pft) = tile_fluxes(lu)%canopy%dra
        if (pft == npft .and. lu == nlu) firstcall = .false.
      end if
      diff_dra = tile_fluxes(lu)%canopy%dra - dra_save(lu,pft)

      ! tile(lu)%plant(pft)%pheno%level_veggrowth = calc_level_veggrowth( diff_dra )

      ! xxx try: for schematic step-change simulations
      tile(lu)%plant(pft)%pheno%level_veggrowth = 1.0

      dra_save(lu,pft) = tile_fluxes(lu)%canopy%dra

    end do pftloop

  end subroutine phenology_daily


  function calc_level_veggrowth( diff_dra ) result( f_veggrowth )
    !////////////////////////////////////////////////////////////////
    ! Calculates a factor (0,1) scaling C allocated to vegetative 
    ! growth. The remainder is not allocated and represents a "seed
    ! filling. However, seeds are not treated separately because
    ! no grass cohorts are modelled. C withheld from allocation
    ! (kept as seed C) is available from allocation one this factor
    ! is >0. 
    !----------------------------------------------------------------
    ! arguments
    real, intent(in) :: diff_dra

    ! function return variable
    real :: f_seed, f_veggrowth

    ! local variables
    real, parameter :: par_f_seed = 3000.0
    real :: diff_dra_norm

    ! change in TOA radiation per day, normalised by solar constant (in J m-2 d-1)
    diff_dra_norm = diff_dra/(kGsc * 24 * 60 * 60)

    ! calculate fraction allocated to seeds (1-allocated to vegetative growth)
    f_seed = 1.0 / (1.0 + exp( par_f_seed * diff_dra_norm ))
    f_veggrowth = 1.0 - f_seed

    ! ! Only start filling seeds if LAI > 1
    ! if (tile(lu)%plant(pft)%lai_ind < 1.0) then
    !   f_veggrowth = 0.0
    ! end if

  end function calc_level_veggrowth


  subroutine calc_ftemp_kphio_coldhard(tc, tmin, level_hard, gdd, &
    kphio_par_a, kphio_par_b, kphio_par_c, kphio_par_d, kphio_par_e)
    !////////////////////////////////////////////////////////////////
    ! Calculates the low temperature stress function assuming no stress
    ! at 10 deg C and above and declining below based on a calibratable
    ! parameter and a quadratic function.
    !----------------------------------------------------------------
    ! arguments
    real, intent(in)    :: tc             ! daily mean air temperature in degrees celsius (deg C)
    real, intent(in)    :: tmin           ! daily minimum air temperature in degrees celsius (deg C)
    real, intent(inout) :: level_hard     ! level (temperature) to which cold hardening is adjusted (deg C)
    real, intent(inout) :: gdd            ! growing degree days (deg)
    real, intent(in)    :: kphio_par_a    ! unitless shape parameter for hardening function
    real, intent(in)    :: kphio_par_b    ! unitless shape parameter for hardening function
    real, intent(in)    :: kphio_par_c    ! unitless shape parameter for dehardening function
    real, intent(in)    :: kphio_par_d    ! unitless shape parameter for dehardening function
    real, intent(in)    :: kphio_par_e    ! parameter defining GDD base in dehardening function (deg C)

    ! local variable
    real :: level_hard_new

    ! determine hardening level - responds instantaneously to minimum temperature
    level_hard_new = f_hardening(tmin, kphio_par_a, kphio_par_b)

    if (level_hard_new < level_hard) then

      ! entering deeper hardening
      level_hard = level_hard_new

      ! re-start recovery
      gdd = 0

    end if

    ! accumulate growing degree days (GDD)
    gdd = gdd + max(0.0, (tc - kphio_par_e))

    ! de-harden based on GDD. f_stress = 1: no stress
    level_hard = level_hard + (1.0 - level_hard) * f_dehardening(gdd, kphio_par_c, kphio_par_d)

  end subroutine calc_ftemp_kphio_coldhard


  function f_hardening(tmin, kphio_par_a, kphio_par_b) result(ftemp)
    !////////////////////////////////////////////////////////////////
    ! Hardening function of instantaneous temperature
    !----------------------------------------------------------------
    ! arguments
    real, intent(in)    :: tmin           ! daily minimum air temperature in degrees celsius (deg C)
    real, intent(in)    :: kphio_par_a    ! unitless shape parameter for hardening function
    real, intent(in)    :: kphio_par_b    ! unitless shape parameter for hardening function

    ! function return variable
    real :: ftemp

    ! local variables
    real :: xx

    xx = (-1.0) * tmin
    xx = kphio_par_b * (xx + kphio_par_a)
    ftemp = 1.0 / (1.0 + exp(xx))

  end function f_hardening


  function f_dehardening(gdd, kphio_par_c, kphio_par_d) result(ftemp)
    !////////////////////////////////////////////////////////////////
    ! De-hardening function of temperature sum (cumulative degree days)
    !----------------------------------------------------------------
    ! arguments
    real, intent(in)    :: gdd            ! cumulative degree days (deg C)
    real, intent(in)    :: kphio_par_c    ! unitless shape parameter for dehardening function
    real, intent(in)    :: kphio_par_d    ! unitless shape parameter for dehardening function

    ! function return variable
    real :: ftemp

    ! local variables
    real :: xx

    xx = (-1.0) * gdd
    xx = kphio_par_d * (xx + kphio_par_c)
    ftemp = 1.0 / (1.0 + exp(xx))

  end function f_dehardening


  subroutine phenology( tile, dtemp )
    !//////////////////////////////////////////////////////////
    ! Defines dtphen, the temperature-driven phenology
    !----------------------------------------------------------
    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    real, dimension(ndayyear), intent(in) :: dtemp

    ! local variables
    integer :: warmest, coldest, month, midsummer, firstday, d, pft, day, lu
    real    :: leafon_n, aphen, gdd
    real, dimension(nmonth)       :: mtemp       ! monthly temperature as a mean of daily values in resp. month
    real, dimension(nmonth), save :: mtemp_pvy   ! monthly temperature as a mean of daily values in resp. month, previous year
    real, dimension(ndayyear)     :: dtemp_int   ! daily temperature as linearly interpolated from monthly temperature
    logical, save :: firstcall = .true.

    ! initialise
    do lu=1,nlu
      do pft=1,npft
        tile(lu)%plant(pft)%pheno_ann(:)%dtphen = 0.0
        tile(lu)%plant(pft)%pheno_ann(:)%sprout = .false.
        tile(lu)%plant(pft)%pheno_ann(:)%shedleaves = .false.
      end do
    end do

    ! Phenology is driven by monthly temperatures and daily temperatures
    ! as interpolated from monthly temperatures to remove day-to-day
    ! variability
    mtemp = daily2monthly( dtemp, "mean" )
    if (firstcall) then
      mtemp_pvy(:) = mtemp(:)
      firstcall = .false.
    end if
    dtemp_int = monthly2daily( mtemp, "interpol", .false., mtemp_pvy )

    ! First find warmest and coldest month and mid-summer day
    warmest = 1
    do month = 1, nmonth
      if (mtemp(month) > mtemp(warmest)) warmest = month
    enddo
    coldest = 1
    do month = 1,nmonth
      if (mtemp(month) < mtemp(coldest)) coldest = month
    enddo
    midsummer = middaymonth( warmest )

    pftloop: do pft = 1, npft
      lu = params_pft_plant(pft)%lu_category

      !----------------------------------------------------------
      ! Find day of leaf abscission ('firstday') at end of summer
      ! i.e. when daily temperature falls below gddbase.
      !----------------------------------------------------------
      firstday = midsummer + 1
      do while (dtemp_int(firstday) >= params_pheno%gddbase .and. firstday /= midsummer)
        firstday = firstday + 1
        if (firstday > ndayyear) firstday = 1
      enddo
      
      if (params_pft_pheno(pft)%summergreen) then
        !----------------------------------------------------------
        ! summergreen TAXA
        !----------------------------------------------------------
        if (firstday == midsummer) then 
          tile(lu)%plant(pft)%pheno_ann(:)%dtphen = 1.0     ! no leaf abscission
        else
          gdd = 0.0               ! accumulated growing degree days
          day = firstday + 1
          if (day > ndayyear) day = 1
          do while (day /= firstday)
            if (dtemp_int(day) > params_pheno%gddbase) then ! growing day
              gdd = gdd + dtemp_int(day) - params_pheno%gddbase
              if (params_pft_pheno(pft)%ramp > 0.0) then
                tile(lu)%plant(pft)%pheno_ann(day)%dtphen = min( gdd / params_pft_pheno(pft)%ramp, 1.0 )
              else
                tile(lu)%plant(pft)%pheno_ann(day)%dtphen = 1.0
              endif
            endif
            day = day + 1
            if (day > ndayyear) day = 1
          enddo

        endif

        ! if (params_pft_plant(pft)%tree) then
        !   !----------------------------------------------------------
        !   ! TREES
        !   !----------------------------------------------------------
        !   aphen = sum(dtphen_tmp(:))
        !   if (aphen>210) then 
        !     do d=middaymonth(coldest),middaymonth(coldest) + 75
        !       if (d <= ndayyear) then
        !         day = d
        !       else
        !         day = d - ndayyear      
        !       endif
        !       dtphen_tmp(day,pft) = 0.0
        !     enddo
        !     do d = middaymonth(coldest)-75,middaymonth(coldest)
        !       if (d >= 1) then
        !         day = d
        !       else
        !         day = ndayyear + d
        !       endif
        !       dtphen_tmp(day,pft) = 0.0
        !     enddo
        !   endif
        ! endif

      else
        !----------------------------------------------------------
        ! NON-summergreen TAXA
        !----------------------------------------------------------
        tile(lu)%plant(pft)%pheno_ann(:)%dtphen = 1.0

      endif


      ! save monthly temperature for next year
      mtemp_pvy(:) = mtemp(:)

      ! ! xxx try: really weird: when appplying a loop over pft, dtphen, sprout, 
      ! ! and shedleaves are all set to false after finishing each iteration
      ! ! therefore set to pft=1 here.
      ! if (npft>1) stop 'in phenology: think of something nice'
      ! pft = 1

      ! do day = 2,ndayyear

      !   if (params_pft_pheno(pft)%summergreen) then
      !     !----------------------------------------------------------
      !     ! temperature-driven phenology summergreen
      !     !----------------------------------------------------------
      !     if ( tile(lu)%plant(pft)%pheno_ann(day)%dtphen > 0.0 .and. tile(lu)%plant(pft)%pheno_ann(day-1)%dtphen == 0.0 ) then
      !       !----------------------------------------------------------
      !       ! beginning of season (spring)
      !       !----------------------------------------------------------
      !       tile(lu)%plant(pft)%pheno_ann(day-1)%sprout = .true.
      !       tile(lu)%plant(pft)%pheno_ann(day-1)%shedleaves = .false.
      !       ! print*, 'sprouting on day ', day 
      !       ! print*, sprout(38,pft)

      !     else if ( tile(lu)%plant(pft)%pheno_ann(day)%dtphen > 0.0 ) then
      !       !----------------------------------------------------------
      !       ! during season (after spring and before autumn)
      !       !----------------------------------------------------------
      !       tile(lu)%plant(pft)%pheno_ann(day-1)%sprout = .false.
      !       tile(lu)%plant(pft)%pheno_ann(day-1)%shedleaves = .false.
      !       ! print*, 'active on day ', day

      !     else if ( tile(lu)%plant(pft)%pheno_ann(day)%dtphen == 0.0 .and. tile(lu)%plant(pft)%pheno_ann(day-1)%dtphen > 0.0 ) then
      !       !----------------------------------------------------------
      !       ! end of season (autumn)
      !       !----------------------------------------------------------
      !       tile(lu)%plant(pft)%pheno_ann(day-1)%sprout = .false.
      !       tile(lu)%plant(pft)%pheno_ann(day-1)%shedleaves = .true.
      !       ! print*, 'shedding leaves on day ', day 
      !       ! print*, shedleaves(345,pft)

      !     else if ( tile(lu)%plant(pft)%pheno_ann(day)%dtphen == 0.0 ) then
      !       !----------------------------------------------------------
      !       ! during dormant season (after autumn and before spring)
      !       !----------------------------------------------------------
      !       tile(lu)%plant(pft)%pheno_ann(day-1)%sprout = .false.
      !       tile(lu)%plant(pft)%pheno_ann(day-1)%shedleaves = .false.
      !       ! print*, 'dormant on day ', day

      !     end if

      !   else

      !     stop 'estab_daily not implemented for non-summergreen'

      !   end if

      ! end do

      ! xxx debug
      ! print*,'PHENOLOGY: overriding shedleaves'
      tile(lu)%plant(pft)%pheno_ann(:)%shedleaves = .false.
    
    end do pftloop

  end subroutine phenology


  subroutine getpar_modl_phenology()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads nuptake module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    use md_interface_cnmodel, only: myinterface

    ! local variables
    integer :: phentype
    integer :: pft

    ! growing degree days base (usually 5 deg C)
    params_pheno%gddbase = myinterface%params_calib%gddbase

    do pft = 1, npft

      ! ramp slope for phenology (1 for grasses: immediate phenology turning on)
      params_pft_pheno(pft)%ramp = myinterface%params_calib%ramp

      ! parameter values adopted from results by Yunpeng Luo (photocold project)
      params_pft_pheno(pft)%kphio_par_a = 2.0    ! mid-point of hardening state vs. tmin (deg C)
      params_pft_pheno(pft)%kphio_par_b = 0.3    ! slope of hardening state vs. tmin (deg C-1)
      params_pft_pheno(pft)%kphio_par_c = 150.0  ! mid-point of dehardening state vs. GDD (deg C)
      params_pft_pheno(pft)%kphio_par_d = 0.05   ! slope of dehardening state vs. GDD (deg C-1)
      params_pft_pheno(pft)%kphio_par_e = 5.0    ! base temperature for GDD summation (deg C)

      ! phenology type
      phentype = nint(myinterface%params_calib%phentype)

      if (abs(phentype-1) < eps) params_pft_pheno(pft)%evergreen   = .true.
      if (abs(phentype-2) < eps) params_pft_pheno(pft)%summergreen = .true.
      if (abs(phentype-3) < eps) params_pft_pheno(pft)%raingreen   = .true.

    end do
 
  end subroutine getpar_modl_phenology


end module md_phenology