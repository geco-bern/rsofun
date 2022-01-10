module md_phenology
  !////////////////////////////////////////////////////////////////
  ! TEMPERATURE-DRIVEN PHENOLOGY 
  ! Adopted from LPX-Bern
  ! Contains the "main" subroutine 'get_temppheno' and 'phenology' and all 
  ! necessary subroutines for handling input/output. 
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  use md_params_core, only: npft, ndayyear
  
  implicit none

  private
  public get_temppheno, params_pft_pheno, &
    getpar_modl_phenology, temppheno_type

  !----------------------------------------------------------------
  ! Public, module-specific state variables
  !----------------------------------------------------------------
  type temppheno_type
    real    :: dtphen       ! daily temperature-driven phenology (=dphen_t in LPX)
    logical :: sprout       ! boolean, true at start-of-season
    logical :: shedleaves   ! boolean, true and end-of-season
  end type temppheno_type

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

  function get_temppheno( jpngr, dtemp ) result( out_temppheno )
    !//////////////////////////////////////////////////////////
    ! Defines dtphen, the temperature-driven phenology
    !----------------------------------------------------------
    use md_params_core, only: ndayyear, maxgrid, nmonth, middaymonth, npft
    use md_plant, only: params_pft_plant
    use md_sofunutils, only: daily2monthly, monthly2daily

    ! arguments
    integer, intent(in) :: jpngr
    real, dimension(ndayyear), intent(in) :: dtemp

    ! function return variable
    type(temppheno_type), dimension(ndayyear,npft) :: out_temppheno

    ! local variables
    integer :: warmest, coldest, month, midsummer, firstday, d, pft, doy
    real    :: leafon_n, aphen, gdd
    real, dimension(nmonth)         :: mtemp       ! monthly temperature as a mean of daily values in resp. month
    real, dimension(nmonth,maxgrid) :: mtemp_pvy   ! monthly temperature as a mean of daily values in resp. month, previous year
    real, dimension(ndayyear)       :: dtemp_int   ! daily temperature as linearly interpolated from monthly temperature
    logical, save :: firstcall = .true.

    ! out_temppheno(:,:)%dtphen     = 0.0
    ! out_temppheno(:,:)%sprout     = .false.
    ! out_temppheno(:,:)%shedleaves = .false.

    ! ! Phenology is driven by monthly temperatures and daily temperatures
    ! ! as interpolated from monthly temperatures to remove day-to-day
    ! ! variability
    ! mtemp = daily2monthly( dtemp, "mean" )
    ! if (firstcall) then
    !   mtemp_pvy(:,jpngr) = mtemp(:)
    !   firstcall = .false.
    ! end if
    ! dtemp_int = monthly2daily( mtemp, "interpol", .false., mtemp_pvy )

    ! ! First find warmest and coldest month and mid-summer day
    ! warmest=1
    ! do month=1,nmonth
    !   if (mtemp(month)>mtemp(warmest)) warmest=month
    ! enddo
    ! coldest=1
    ! do month=1,nmonth
    !   if (mtemp(month)<mtemp(coldest)) coldest=month
    ! enddo
    ! midsummer = middaymonth( warmest )

    ! ! do pft=1,npft

    !   if (npft>1) stop 'in phenology: think of something nice'
    !   pft = 1

    !   !----------------------------------------------------------
    !   ! Find day of leaf abscission ('firstday') at end of summer
    !   ! i.e. when daily temperature falls below gddbase.
    !   !----------------------------------------------------------
    !   firstday=midsummer+1
    !   do while (dtemp_int(firstday)>=params_pheno%gddbase .and. firstday/=midsummer)
    !     firstday=firstday+1
    !     if (firstday>ndayyear) firstday=1
    !   enddo
      
    !   if (params_pft_pheno(pft)%summergreen) then
    !     !----------------------------------------------------------
    !     ! summergreen TAXA
    !     !----------------------------------------------------------
    !     if (firstday==midsummer) then 
    !       out_temppheno(:,pft)%dtphen=1.0     ! no leaf abscission
    !     else
    !       gdd=0.0               ! accumulated growing degree days
    !       doy=firstday+1
    !       if (doy>ndayyear) doy=1
    !       ! print*,'firstday ', firstday
    !       ! print*,'doy ', doy
    !       do while (doy/=firstday)
    !         if (dtemp_int(doy)>params_pheno%gddbase) then ! growing doy
    !           gdd = gdd + dtemp_int(doy) - params_pheno%gddbase
    !           if (params_pft_pheno(pft)%ramp>0.0) then
    !             out_temppheno(doy,pft)%dtphen = min( gdd / params_pft_pheno(pft)%ramp, 1.0 )
    !           else
    !             out_temppheno(doy,pft)%dtphen = 1.0
    !           endif
    !         endif
    !         ! print*,'dtphen_tmp ', dtphen_tmp(doy,pft)
    !         doy=doy+1
    !         if (doy>ndayyear) doy=1
    !       enddo
    !     endif
        

    !     ! if (params_pft_plant(pft)%tree) then
    !     !   !----------------------------------------------------------
    !     !   ! TREES
    !     !   !----------------------------------------------------------
    !     !   aphen=sum(dtphen_tmp(:))
    !     !   if (aphen>210) then 
    !     !     do d=middaymonth(coldest),middaymonth(coldest)+75
    !     !       if (d<=ndayyear) then
    !     !         doy=d
    !     !       else
    !     !         doy=d-ndayyear      
    !     !       endif
    !     !       dtphen_tmp(doy,pft)=0.0
    !     !     enddo
    !     !     do d=middaymonth(coldest)-75,middaymonth(coldest)
    !     !       if (d>=1) then
    !     !         doy=d
    !     !       else
    !     !         doy=ndayyear+d
    !     !       endif
    !     !       dtphen_tmp(doy,pft)=0.0
    !     !     enddo
    !     !   endif
    !     ! endif

    !   else
    !     !----------------------------------------------------------
    !     ! NON-summergreen TAXA
    !     !----------------------------------------------------------
    !     out_temppheno(:,pft)%dtphen=1.0

    !   endif
      
    !   ! print*, 'get_temppheno: dtphen_tmp(doy,pft) '
    !   ! print*, dtphen(:,pft)
    !   ! print*, 'a' 
    !   ! stop


    ! ! save monthly temperature for next year
    ! mtemp_pvy(:,jpngr) = mtemp(:)

    ! do pft=1,npft

    !   ! xxx try: really weird: when appplying a loop over pft, dtphen, sprout, 
    !   ! and shedleaves are all set to false after finishing each iteration
    !   ! therefore set to pft=1 here.
      
    !   do doy=2,ndayyear

    !     if (params_pft_pheno(pft)%summergreen) then
    !       !----------------------------------------------------------
    !       ! temperature-driven phenology summergreen
    !       !----------------------------------------------------------

    !       if ( out_temppheno(doy,pft)%dtphen > 0.0 .and. out_temppheno(doy-1,pft)%dtphen == 0.0 ) then
    !         !----------------------------------------------------------
    !         ! beginning of season (spring)
    !         !----------------------------------------------------------
    !         out_temppheno(doy,pft)%sprout = .true.
    !         out_temppheno(doy,pft)%shedleaves = .false.
    !         ! print*, 'sprouting on doy ', doy 
    !         ! print*, sprout(38,pft)

    !       else if ( out_temppheno(doy,pft)%dtphen > 0.0 ) then
    !         !----------------------------------------------------------
    !         ! during season (after spring and before autumn)
    !         !----------------------------------------------------------
    !         out_temppheno(doy,pft)%sprout = .false.
    !         out_temppheno(doy,pft)%shedleaves = .false.
    !         ! print*, 'active on doy ', doy

    !       else if ( out_temppheno(doy,pft)%dtphen == 0.0 .and. out_temppheno(doy-1,pft)%dtphen > 0.0 ) then
    !         !----------------------------------------------------------
    !         ! end of season (autumn)
    !         !----------------------------------------------------------
    !         out_temppheno(doy,pft)%sprout = .false.
    !         out_temppheno(doy,pft)%shedleaves = .true.
    !         ! print*, 'shedding leaves on doy ', doy 
    !         ! print*, shedleaves(345,pft)

    !       else if ( out_temppheno(doy,pft)%dtphen == 0.0 ) then
    !         !----------------------------------------------------------
    !         ! during dormant season (after autumn and before spring)
    !         !----------------------------------------------------------
    !         out_temppheno(doy,pft)%sprout = .false.
    !         out_temppheno(doy,pft)%shedleaves = .false.
    !         ! print*, 'dormant on doy ', doy

    !       end if

    !     else

    !       stop 'estab_daily not implemented for trees'

    !     end if

    !   end do

    !   ! xxx debug
    !   print*,'PHENOLOGY: overriding shedleaves'
    !   out_temppheno(:,pft)%shedleaves = .false.
    
    ! end do

    return

  end function get_temppheno


  subroutine getpar_modl_phenology()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads nuptake module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    ! use md_sofunutils, only: getparreal
    use md_plant, only: params_pft_plant

    ! local variables
    real        :: phentype
    integer     :: pft

    ! ! growing degree days base (usually 5 deg C)
    ! params_pheno%gddbase = 5.0 ! getparreal( 'params/params_phenology.dat', 'gddbase' )

    ! do pft=1,npft

    !   ! ramp slope for phenology (1 for grasses: immediate phenology turning on)
    !   params_pft_pheno(pft)%ramp = getparreal( 'params/params_phenology.dat', 'ramp_pft_'//params_pft_plant(pft)%pftname )

    !   ! phenology type
    !   phentype = getparreal( 'params/params_phenology.dat', 'phentype_pft_'//params_pft_plant(pft)%pftname )

    !   if (phentype==1.0) params_pft_pheno(pft)%evergreen   = .true.
    !   if (phentype==2.0) params_pft_pheno(pft)%summergreen = .true.
    !   if (phentype==3.0) params_pft_pheno(pft)%raingreen   = .true.

    ! end do
 
    return
 
  end subroutine getpar_modl_phenology


end module md_phenology





