module md_phenology
  !////////////////////////////////////////////////////////////////
  ! TEMPERATURE-DRIVEN PHENOLOGY 
  ! Adopted from LPX-Bern
  ! Contains the "main" subroutine 'phenology and phenology' and all 
  ! necessary subroutines for handling input/output. 
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------  
  use md_params_core, only: npft, eps
  
  implicit none

  private
  public phenology, getpar_modl_phenology

  !----------------------------------------------------------------
  ! Parameters
  !----------------------------------------------------------------
  type paramstype_pheno
    real :: gddbase ! GDD base, for PFT11-14, a T0 is chosen to be 0deg C (Prentice et al. 1992, J.o.Biogeography), pftpar(pft,33) in LPX
  end type paramstype_pheno

  type( paramstype_pheno ) :: params_pheno

  type pftparamstype_pheno
    real    :: ramp    ! summergreen phenology ramp, GDD requirement to grow full leaf canopy
    logical :: evergreen
    logical :: summergreen
    logical :: raingreen  
  end type pftparamstype_pheno

  type( pftparamstype_pheno ), dimension(npft) :: params_pft_pheno


contains

  subroutine phenology( tile, dtemp )
    !//////////////////////////////////////////////////////////
    ! Defines dtphen, the temperature-driven phenology
    !----------------------------------------------------------
    use md_params_core, only: ndayyear, maxgrid, nmonth, middaymonth, npft, nlu
    use md_sofunutils, only: daily2monthly, monthly2daily
    use md_tile
    use md_plant

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
        tile(lu)%plant(pft)%pheno(:)%dtphen = 0.0
        tile(lu)%plant(pft)%pheno(:)%sprout = .false.
        tile(lu)%plant(pft)%pheno(:)%shedleaves = .false.
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
    warmest=1
    do month=1,nmonth
      if (mtemp(month) > mtemp(warmest)) warmest = month
    enddo
    coldest = 1
    do month = 1,nmonth
      if (mtemp(month) < mtemp(coldest)) coldest = month
    enddo
    midsummer = middaymonth( warmest )

    pftloop: do pft=1,npft
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
          tile(lu)%plant(pft)%pheno(:)%dtphen = 1.0     ! no leaf abscission
        else
          gdd = 0.0               ! accumulated growing degree days
          day = firstday + 1
          if (day > ndayyear) day = 1
          do while (day /= firstday)
            if (dtemp_int(day) > params_pheno%gddbase) then ! growing day
              gdd = gdd + dtemp_int(day) - params_pheno%gddbase
              if (params_pft_pheno(pft)%ramp > 0.0) then
                tile(lu)%plant(pft)%pheno(day)%dtphen = min( gdd / params_pft_pheno(pft)%ramp, 1.0 )
              else
                tile(lu)%plant(pft)%pheno(day)%dtphen = 1.0
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
        !   aphen=sum(dtphen_tmp(:))
        !   if (aphen>210) then 
        !     do d=middaymonth(coldest),middaymonth(coldest)+75
        !       if (d<=ndayyear) then
        !         day=d
        !       else
        !         day=d-ndayyear      
        !       endif
        !       dtphen_tmp(day,pft)=0.0
        !     enddo
        !     do d=middaymonth(coldest)-75,middaymonth(coldest)
        !       if (d>=1) then
        !         day=d
        !       else
        !         day=ndayyear+d
        !       endif
        !       dtphen_tmp(day,pft)=0.0
        !     enddo
        !   endif
        ! endif

      else
        !----------------------------------------------------------
        ! NON-summergreen TAXA
        !----------------------------------------------------------
        tile(lu)%plant(pft)%pheno(:)%dtphen = 1.0

      endif


      ! save monthly temperature for next year
      mtemp_pvy(:) = mtemp(:)

      ! ! xxx try: really weird: when appplying a loop over pft, dtphen, sprout, 
      ! ! and shedleaves are all set to false after finishing each iteration
      ! ! therefore set to pft=1 here.
      ! if (npft>1) stop 'in phenology: think of something nice'
      ! pft = 1

      do day=2,ndayyear

        if (params_pft_pheno(pft)%summergreen) then
          !----------------------------------------------------------
          ! temperature-driven phenology summergreen
          !----------------------------------------------------------
          if ( tile(lu)%plant(pft)%pheno(day)%dtphen > 0.0 .and. tile(lu)%plant(pft)%pheno(day-1)%dtphen == 0.0 ) then
            !----------------------------------------------------------
            ! beginning of season (spring)
            !----------------------------------------------------------
            tile(lu)%plant(pft)%pheno(day-1)%sprout = .true.
            tile(lu)%plant(pft)%pheno(day-1)%shedleaves = .false.
            ! print*, 'sprouting on day ', day 
            ! print*, sprout(38,pft)

          else if ( tile(lu)%plant(pft)%pheno(day)%dtphen > 0.0 ) then
            !----------------------------------------------------------
            ! during season (after spring and before autumn)
            !----------------------------------------------------------
            tile(lu)%plant(pft)%pheno(day-1)%sprout = .false.
            tile(lu)%plant(pft)%pheno(day-1)%shedleaves = .false.
            ! print*, 'active on day ', day

          else if ( tile(lu)%plant(pft)%pheno(day)%dtphen == 0.0 .and. tile(lu)%plant(pft)%pheno(day-1)%dtphen > 0.0 ) then
            !----------------------------------------------------------
            ! end of season (autumn)
            !----------------------------------------------------------
            tile(lu)%plant(pft)%pheno(day-1)%sprout = .false.
            tile(lu)%plant(pft)%pheno(day-1)%shedleaves = .true.
            ! print*, 'shedding leaves on day ', day 
            ! print*, shedleaves(345,pft)

          else if ( tile(lu)%plant(pft)%pheno(day)%dtphen == 0.0 ) then
            !----------------------------------------------------------
            ! during dormant season (after autumn and before spring)
            !----------------------------------------------------------
            tile(lu)%plant(pft)%pheno(day-1)%sprout = .false.
            tile(lu)%plant(pft)%pheno(day-1)%shedleaves = .false.
            ! print*, 'dormant on day ', day

          end if

        else

          stop 'estab_daily not implemented for non-summergreen'

        end if

      end do

      ! xxx debug
      ! print*,'PHENOLOGY: overriding shedleaves'
      tile(lu)%plant(pft)%pheno(:)%shedleaves = .false.
    
    end do pftloop

  end subroutine phenology


  subroutine getpar_modl_phenology()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads nuptake module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    use md_interface_pmodel, only: myinterface

    ! local variables
    integer :: phentype
    integer :: pft

    ! growing degree days base (usually 5 deg C)
    params_pheno%gddbase = myinterface%params_calib%gddbase

    do pft = 1, npft

      ! ramp slope for phenology (1 for grasses: immediate phenology turning on)
      params_pft_pheno(pft)%ramp = myinterface%params_calib%ramp

      ! phenology type
      phentype = nint(myinterface%params_calib%phentype)

      if (abs(phentype-1) < eps) params_pft_pheno(pft)%evergreen   = .true.
      if (abs(phentype-2) < eps) params_pft_pheno(pft)%summergreen = .true.
      if (abs(phentype-3) < eps) params_pft_pheno(pft)%raingreen   = .true.

    end do
 
  end subroutine getpar_modl_phenology


end module md_phenology