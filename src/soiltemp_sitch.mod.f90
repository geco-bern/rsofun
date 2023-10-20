module md_soiltemp
  !////////////////////////////////////////////////////////////////
  ! Soil temperature based on LPJ (Sitch et al., 2003)
  !----------------------------------------------------------------
  use md_params_core, only: nlu, maxgrid

  implicit none

  private
  public soiltemp

  !----------------------------------------------------------------
  ! Module-specific state variables
  !----------------------------------------------------------------
  ! real, dimension(nlu,maxgrid) :: dtemp_soil          ! soil temperature [deg C]

  !----------------------------------------------------------------
  ! Module-specific daily output variables
  !----------------------------------------------------------------
  ! real, allocatable, dimension(:,:,:) :: outdtemp_soil

contains

  subroutine soiltemp( soil, dtemp, moy, doy ) 
    !/////////////////////////////////////////////////////////////////////////
    ! Calculates soil temperature based on.
    !-------------------------------------------------------------------------
    use md_params_core, only: ndayyear, nlu, ndaymonth, pi
    use md_sofunutils, only: running
    ! use md_sofunutils, only: daily2monthly
    use md_tile_pmodel, only: soil_type
    use md_interface_pmodel, only: myinterface

    ! arguments
    type( soil_type ), dimension(nlu), intent(inout) :: soil
    real, dimension(ndayyear), intent(in)            :: dtemp        ! daily temperature (deg C)
    integer, intent(in)                              :: moy        ! current month of year
    integer, intent(in)                              :: doy        ! current day of year

    ! local variables
    real, dimension(:),   allocatable, save   :: dtemp_pvy    ! daily temperature of previous year (deg C)
    real, dimension(:,:), allocatable, save   :: wscal_pvy    ! daily Cramer-Prentice-Alpha of previous year (unitless) 
    real, dimension(:,:), allocatable, save   :: wscal_alldays

    !real, dimension(ndayyear), save :: dtemp_buf        ! daily temperature vector containing values of the present day and the preceeding 364 days. Updated daily. (deg C)
    !real, dimension(ndayyear), save :: dwtot_buf        ! daily soil moisture content, containing values of the present day and the preceeding 364 days. Updated daily

    integer :: pm ,ppm, lu

    real :: avetemp, meanw1
    real :: tempthismonth, templastmonth
    real :: diffus
    real :: alag, amp, lag, lagtemp

    ! in first year, use this years air temperature (available for all days in this year)
    if ( myinterface%steering%init .and. doy==1 ) then
      if (.not.allocated(dtemp_pvy    )) allocate( dtemp_pvy(ndayyear) )
      if (.not.allocated(wscal_pvy    )) allocate( wscal_pvy(nlu,ndayyear) )
      if (.not.allocated(wscal_alldays)) allocate( wscal_alldays(nlu,ndayyear) )
      dtemp_pvy(:) = dtemp(:)
    end if

    wscal_alldays(:,doy) = soil(:)%phy%wscal

    avetemp = running( dtemp, doy, ndayyear, ndayyear, "mean", dtemp_pvy(:) ) 

    ! get monthly mean temperature vector from daily vector
    !mtemp     = daily2monthly( dtemp,     "mean" )
    !mtemp_pvy = daily2monthly( dtemp_pvy, "mean" )

    ! get average temperature of the preceeding N days in month (30/31/28 days)
    if (moy==1) then
      pm = 12
      ppm = 11
    else if (moy==2) then
      pm = 1
      ppm = 12
    else
      pm = moy - 1
      ppm = moy - 2
    end if
    tempthismonth = running( dtemp, doy, ndayyear, ndaymonth(pm), "mean", dtemp_pvy(:))
    templastmonth = running( dtemp, modulo( doy - ndaymonth(pm), ndayyear ), ndayyear, ndaymonth(ppm), "mean", dtemp_pvy(:))


    do lu=1,nlu
      !-------------------------------------------------------------------------
      ! recalculate running mean of previous 12 month's temperature and soil moisture
      ! avetemp stores running mean temperature of previous 12 months.
      ! meanw1 stores running mean soil moisture in layer 1 of previous 12 months 
      !-------------------------------------------------------------------------
      if (myinterface%steering%init) then
        meanw1  = running( wscal_alldays(lu,:), doy, ndayyear, ndayyear, "mean")
      else
        meanw1  = running( wscal_alldays(lu,:), doy, ndayyear, ndayyear, "mean", wscal_pvy(lu,:))
      end if

      ! In case of zero soil water, return with soil temp = air temp
      if (meanw1==0.0) then
        soil(lu)%phy%temp = dtemp(doy)
        ! dtemp_soil(lu,jpngr) = dtemp(doy)
        return
      endif

      ! Interpolate thermal diffusivity function against soil water content
      if (meanw1<0.15) then
        diffus = ( soil(lu)%params%thdiff_whc15 - soil(lu)%params%thdiff_wp ) / 0.15 &
                  * meanw1 + soil(lu)%params%thdiff_wp
      else
        diffus = ( soil(lu)%params%thdiff_fc - soil(lu)%params%thdiff_whc15 ) / 0.85 &
                  * ( meanw1 - 0.15 ) + soil(lu)%params%thdiff_whc15
      endif
          
      ! Convert diffusivity from mm2/s to m2/month
      ! multiplication by 1e-6 (-> m2/s) * 2.628e6 (s/month)  =  2.628
      diffus = diffus * 2.628

      ! Calculate amplitude fraction and lag at soil depth 0.25 m
      alag = 0.25 / sqrt( 12.0 * diffus / pi )
      amp  = exp(-alag)
      lag  = alag * ( 6.0 / pi )                                 !convert lag from angular units to months
          
      ! Calculate monthly soil temperatures for this year.  For each month,
      ! calculate average air temp for preceding 12 months (including this one)
          
      ! Estimate air temperature "lag" months ago by linear interpolation
      ! between air temperatures for this and last month
      lagtemp = ( tempthismonth - templastmonth ) * ( 1.0 - lag ) + templastmonth
          
      ! Adjust amplitude of lagged air temp to give estimated soil temp
      ! dtemp_soil(lu,jpngr) = avetemp + amp * ( lagtemp - avetemp )
      soil(lu)%phy%temp = avetemp + amp * ( lagtemp - avetemp )

    end do

    ! save temperature for next year
    if (doy==ndayyear) then
      dtemp_pvy(:) = dtemp(:)
      wscal_pvy(:,:) = wscal_alldays(:,:)
    end if

    return

  end subroutine soiltemp


end module md_soiltemp
