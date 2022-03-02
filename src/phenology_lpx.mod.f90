module md_phenology
  !////////////////////////////////////////////////////////////////
  ! TEMPERATURE-DRIVEN PHENOLOGY 
  ! Adopted from LPX-Bern
  ! Contains the "main" subroutine 'gettempphenology and phenology' and all 
  ! necessary subroutines for handling input/output. 
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  use md_params_core, only: npft, ndayyear
  
  implicit none

  private
  public dtphen, gettempphenology, sprout, shedleaves, params_pft_pheno, &
    getpar_modl_phenology

  !----------------------------------------------------------------
  ! Public, module-specific state variables
  !----------------------------------------------------------------
  real, dimension(ndayyear,npft)    :: dtphen       ! daily temperature-driven phenology (=dphen_t in LPX)
  logical, dimension(ndayyear,npft) :: sprout       ! boolean whether PFT is present
  logical, dimension(ndayyear,npft) :: shedleaves   ! boolean whether PFT is present

  !----------------------------------------------------------------
  ! Module-specific output variables
  !----------------------------------------------------------------


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

  subroutine gettempphenology( dtemp )
    !//////////////////////////////////////////////////////////
    ! Defines dtphen, the temperature-driven phenology
    !----------------------------------------------------------
    use md_params_core, only: ndayyear, maxgrid, nmonth, middaymonth
    use md_plant, only: params_pft_plant
    use md_sofunutils, only: daily2monthly, monthly2daily

    ! arguments
    real, dimension(ndayyear), intent(in) :: dtemp

    ! local variables
    integer :: warmest, coldest, month, midsummer, firstday, d, pft, day
    real    :: leafon_n, aphen, gdd
    real, dimension(nmonth)       :: mtemp       ! monthly temperature as a mean of daily values in resp. month
    real, dimension(nmonth), save :: mtemp_pvy   ! monthly temperature as a mean of daily values in resp. month, previous year
    real, dimension(ndayyear)     :: dtemp_int   ! daily temperature as linearly interpolated from monthly temperature
    logical, save :: firstcall = .true.

    dtphen(:,:)     = 0.0
    sprout(:,:)     = .false.
    shedleaves(:,:) = .false.

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

    ! do pft=1,npft

      if (npft>1) stop 'in phenology: think of something nice'
      pft = 1

      !----------------------------------------------------------
      ! Find day of leaf abscission ('firstday') at end of summer
      ! i.e. when daily temperature falls below gddbase.
      !----------------------------------------------------------
      firstday=midsummer+1
      do while (dtemp_int(firstday) >= params_pheno%gddbase .and. firstday /= midsummer)
        firstday = firstday+1
        if (firstday > ndayyear) firstday = 1
      enddo
      
      if (params_pft_pheno(pft)%summergreen) then
        !----------------------------------------------------------
        ! summergreen TAXA
        !----------------------------------------------------------
        if (firstday == midsummer) then 
          dtphen(:,pft)=1.0     ! no leaf abscission
        else
          gdd = 0.0               ! accumulated growing degree days
          day = firstday + 1
          if (day > ndayyear) day = 1
          ! print*,'firstday ', firstday
          ! print*,'day ', day
          do while (day /= firstday)
            if (dtemp_int(day) > params_pheno%gddbase) then ! growing day
              gdd = gdd + dtemp_int(day) - params_pheno%gddbase
              if (params_pft_pheno(pft)%ramp > 0.0) then
                dtphen(day, pft) = min( gdd / params_pft_pheno(pft)%ramp, 1.0 )
              else
                dtphen(day, pft) = 1.0
              endif
            endif
            ! print*,'dtphen_tmp ', dtphen_tmp(day,pft)
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
        dtphen(:,pft) = 1.0

      endif
      
      ! print*, 'gettempphenology: dtphen_tmp(day,pft) '
      ! print*, dtphen(:,pft)
      ! print*, 'a' 
      ! stop


    ! save monthly temperature for next year
    mtemp_pvy(:) = mtemp(:)

    ! do pft=1,npft

      ! xxx try: really weird: when appplying a loop over pft, dtphen, sprout, 
      ! and shedleaves are all set to false after finishing each iteration
      ! therefore set to pft=1 here.
      if (npft>1) stop 'in phenology: think of something nice'
      pft = 1

      do day = 2, ndayyear

        if (params_pft_pheno(pft)%summergreen) then
          !----------------------------------------------------------
          ! temperature-driven phenology summergreen
          !----------------------------------------------------------

          if ( dtphen(day,pft) > 0.0 .and. dtphen(day-1,pft) == 0.0 ) then
            !----------------------------------------------------------
            ! beginning of season (spring)
            !----------------------------------------------------------
            sprout(day,pft) = .true.
            shedleaves(day,pft) = .false.
            ! print*, 'sprouting on day ', day 
            ! print*, sprout(38,pft)

          else if ( dtphen(day,pft) > 0.0 ) then
            !----------------------------------------------------------
            ! during season (after spring and before autumn)
            !----------------------------------------------------------
            sprout(day,pft) = .false.
            shedleaves(day,pft) = .false.
            ! print*, 'active on day ', day

          else if ( dtphen(day,pft) == 0.0 .and. dtphen(day-1,pft) > 0.0 ) then
            !----------------------------------------------------------
            ! end of season (autumn)
            !----------------------------------------------------------
            sprout(day,pft) = .false.
            shedleaves(day,pft) = .true.
            ! print*, 'shedding leaves on day ', day 
            ! print*, shedleaves(345,pft)

          else if ( dtphen(day,pft) == 0.0 ) then
            !----------------------------------------------------------
            ! during dormant season (after autumn and before spring)
            !----------------------------------------------------------
            sprout(day,pft) = .false.
            shedleaves(day,pft) = .false.
            ! print*, 'dormant on day ', day

          end if

        else

          stop 'estab_daily not implemented for trees'

        end if

      end do

      ! xxx debug
      ! print*,'PHENOLOGY: overriding shedleaves'
      shedleaves(:,pft) = .false.
    
    return

  end subroutine gettempphenology


  subroutine getpar_modl_phenology()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads nuptake module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    ! local variables
    real        :: phentype
    integer     :: pft

    ! growing degree days base (usually 5 deg C)
    params_pheno%gddbase = 5.0

    do pft = 1, npft

      ! ramp slope for phenology (1 for grasses: immediate phenology turning on)
      params_pft_pheno(pft)%ramp = 0.0

      ! phenology type
      phentype = 2.0

      if (phentype==1.0) params_pft_pheno(pft)%evergreen   = .true.
      if (phentype==2.0) params_pft_pheno(pft)%summergreen = .true.
      if (phentype==3.0) params_pft_pheno(pft)%raingreen   = .true.

    end do
 
    return
 
  end subroutine getpar_modl_phenology


end module md_phenology