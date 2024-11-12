module md_sofunutils
  !/////////////////////////////////////////////////////////////////////////
  ! Contains various utility functions
  !-------------------------------------------------------------------------
  use md_params_core

  implicit none

  integer, parameter :: int4=SELECTED_INT_KIND(4)
  integer, parameter :: flt4=SELECTED_REAL_KIND(6,37)
  integer, parameter :: dbl8=SELECTED_REAL_KIND(15,307)

contains

  function dampen_variability( var, tau, var_memory ) result( out_memory )
    !/////////////////////////////////////////////////////////////////////////
    ! Calculates the updated variable accounting for a memory time scale tau.
    ! Following Eq. 5 in Makela et al. (2004) Tree Physiology 24, 369–376
    ! 
    ! d(var_memory) / dt = (1 / tau) * var - var_memory
    ! 
    !-------------------------------------------------------------------------
    ! arguments
    real, intent(in)    :: var           ! fast-varying variable
    real, intent(in)    :: tau           ! memory e-folding time scale (d)
    real, intent(in)    :: var_memory    ! damped (low-pass filtered) variable

    ! function return variable
    real :: out_memory

    ! local variable
    real :: dvar

    dvar = (1.0/tau) * (var - var_memory)
    out_memory = var_memory + dvar

  end function dampen_variability


  function running( presval, inow, lenval, lenper, method, prevval ) result( runningval )
    !/////////////////////////////////////////////////////////////////////////
    ! Returns running sum or average. 'prevval' is optional, if not pro-
    ! vided, sum/average is taken only over preceeding days/months of current
    ! year.
    !-------------------------------------------------------------------------
    ! arguments
    ! xxx instead of dimension declaration with 'lenval', use size(presval)
    integer, intent(in) :: lenval                             ! number of timesteps per year
    real, dimension(lenval), intent(in) :: presval            ! vector containing 'lenvals' values for each timestep in this year
    integer, intent(in) :: inow                               ! index corresponding to "now" (day of year or month of year)  
    integer, intent(in) :: lenper                             ! number of timesteps over which to average/sum
    character(len=*), intent(in) :: method                    ! either "sum" or "mean" for running sum or running mean
    real, dimension(lenval), intent(in), optional :: prevval  ! vector containing 'lenvals' values for each timestep in previous year

    ! local variables
    real, dimension(lenval) :: valbuf

    ! function return variable
    real :: runningval

    !print*,'day, lenper ',inow, lenper

    if (present(prevval)) then
      !print*,'A filling up valbuf from ',(lenval-(inow-1)),'to',lenval
      !print*,'A with values from        1 to     ',inow
      valbuf((lenval-(inow-1)):lenval) = presval(1:inow)
      !print*,'B filling up valbuf from  1 to',(lenval-inow)
      !print*,'B with values from       ',(inow+1),' to ',lenval
      valbuf(1:(lenval-inow)) = prevval((inow+1):lenval)
    else
      !print*,'A filling up valbuf from  1 to',inow
      !print*,'A with values from        1 to ',inow
      valbuf(1:inow) = presval(1:inow)
      !print*,'B filling up valbuf from  ',(inow+1),'to',lenval
      !print*,'B with values zero'
      valbuf((inow+1):lenval) = 0.0
    end if

    if (method=="sum") then
      runningval = sum(valbuf((lenval-lenper+1):lenval))
    else if (method=="mean") then
      if (present(prevval)) then
        runningval = sum(valbuf((lenval-lenper+1):lenval))/lenper
      else
        runningval = sum(valbuf((lenval-lenper+1):lenval))/inow
      end if
    else
      ! stop 'RUNNING: declare valid method.'
      runningval = sum(valbuf((lenval-lenper+1):lenval))
    end if

  end function running


  ! function daily2monthly( dval, method ) result( mval )
  !   !/////////////////////////////////////////////////////////////////////////
  !   ! Returns monthly values as a mean over daily values in each month.
  !   !-------------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in), dimension(ndayyear) :: dval ! vector containing 365 (366 in case lapyear is TRUE) daily values
  !   character(len=*), intent(in) :: method        ! true of monthly values represent total of daily values in resp. month

  !   ! function return variable
  !   real, dimension(nmonth) :: mval

  !   ! local variables
  !   integer :: moy
  !   integer, dimension(nmonth) :: istart, iend

  !   istart = cumdaymonth - ndaymonth + 1
  !   iend   = cumdaymonth

  !   ! loop over months and take sum/mean of days in that month
  !   do moy=1,nmonth
  !     if (method=="sum") then
  !       mval(moy) = sum( dval( istart(moy) : iend(moy) ))
  !     else if (method=="mean") then
  !       mval(moy) = sum( dval( istart(moy) : iend(moy) )) / ndaymonth(moy)
  !     else
  !       stop 'DAILY2MONTHLY: select valid method (sum, mean)' 
  !     end if
  !   end do

  ! end function daily2monthly


  ! function monthly2daily_weather( mval_prec, mval_wet, prdaily_random ) result( dval_prec )
  !   !/////////////////////////////////////////////////////////////////////////
  !   ! Weather (rain) generator.
  !   ! Distributes monthly total precipitation to days, given number of 
  !   ! monthly wet days. Adopted from LPX.
  !   !--------------------------------------------------------------------
  !   ! arguments
  !   real, dimension(nmonth), intent(in)     :: mval_prec  ! monthly precipitation totals
  !   real, dimension(nmonth)                 :: mval_wet   ! monthly number of wet days
  !   real, dimension(ndayyear,2), intent(in) :: prdaily_random ! random number seed

  !   ! local vars
  !   integer :: doy, moy, dm, iloop
  !   integer :: daysum            !accumulated days at beginning of month
  !   integer :: nwet              !# of generated wet days
    
  !   logical :: dry

  !   real :: prob, vv, v1, rand
  !   real, parameter :: c1 = 1.0
  !   real, parameter :: c2 = 1.2
  !   real, dimension(nmonth) :: prob_rain
  !   real, dimension(nmonth) :: mprecave      !average precipitation on wet days
  !   real, dimension(nmonth) :: mprecip       !acc. monthly generated precipitation

  !   ! function return variable
  !   real, dimension(ndayyear) :: dval_prec

  !   doy = 0
  !   prob = 0.0
  !   do moy=1,nmonth
  !     prob_rain(moy) = 0.0
  !     mprecave(moy) = 0.0
  !     mprecip(moy) = 0.0
  !   enddo
  !   daysum = 0

  !   ! write(0,*) 'A'

  !   ! Initialize 2nd random number generator
  !   ! call srand( int( prdaily_random(1,1) * 100000 ) )

  !   ! xxx this solves the problem (removing multiplication with 1e5. But number of 
  !   ! actual wet days is not perfectly consistent with prescribed number of wet days.
  !   call srand( int( prdaily_random(1,1) ) )
   
  !   do moy=1,nmonth
  !     if ( mval_wet(moy)<=1.0 ) mval_wet(moy) = 1.0
  !     prob_rain(moy) = mval_wet(moy) / real( ndaymonth(moy) )
  !     mprecave(moy) = mval_prec(moy) / mval_wet(moy)
  !     dry = .TRUE.
  !     iloop = 0

  !     ! write(0,*) 'B'

  !     do while( dry )
  !       ! write(0,*) 'aa'
  !       iloop = iloop + 1
  !       nwet = 0
  !       do dm=1,ndaymonth(moy)
  !         ! write(0,*) 'a'
  !         doy = doy + 1
   
  !         ! Transitional probabilities (Geng et al. 1986)
  !         if (doy>1) then
  !           if (dval_prec(doy-1) < 0.1) then
  !             prob = 0.75 * prob_rain(moy)
  !           else 
  !             prob = 0.25 + (0.75 * prob_rain(moy))
  !           endif
  !         endif
  !         ! write(0,*) 'b'
        
  !         ! Determine we randomly and use Krysanova / Cramer estimates of 
  !         ! parameter values (c1,c2) for an exponential distribution
  !         if (iloop==1) then 
  !           ! write(0,*) 'getting prdaily_random'
  !           vv = real(prdaily_random(doy,1))
  !           ! write(0,*) 'vv', vv
  !         else
  !           ! write(0,*) 'calling rand'
  !           ! xxx problem: rand() generates a random number that leads to floating point exception
  !           vv = rand()
  !           ! write(0,*) 'vv'
  !           ! write(0,*) vv
  !         endif
  !         ! write(0,*) 'c'
  !         ! write(0,*) 'prob', prob
  !         if (vv>prob) then
  !           ! write(0,*) 'd1'
  !           dval_prec(doy) = 0.0
  !         else
  !           ! write(0,*) 'c1'
  !           nwet = nwet + 1
  !           ! write(0,*) 'c2'
  !           v1 = real( prdaily_random(doy,2) )
  !           ! write(0,*) 'c3'
  !           dval_prec(doy) = ((-log(v1)) ** c2) * mprecave(moy) * c1 
  !           ! write(0,*) 'c4'
  !           if (dval_prec(doy) < 0.1) dval_prec(doy) = 0.0
  !           ! write(0,*) 'c5'
  !         endif
  !         ! write(0,*) 'd'
  !         mprecip(moy) = mprecip(moy) + dval_prec(doy)
  !       enddo
    
  !       ! If it never rained this month and mprec(moy)>0 and mval_wet(moy)>0, do
  !       ! again
  !       dry = (nwet==0 .and. iloop<50 .and. mval_prec(moy)>0.1)
  !       if (iloop>50) then
  !         write(0,*) 'Daily.F, prdaily: Warning stopped after 50 tries in cell'
  !       endif

  !       ! Reset counter to start of month          
  !       if (dry) then
  !         doy = doy-ndaymonth(moy)
  !       endif

  !     enddo !while
      
  !     ! write(0,*) 'C'


  !     ! normalise generated precipitation by monthly CRU values
  !     if ( moy > 1 ) daysum = daysum + ndaymonth(moy-1)
  !     if ( mprecip(moy) < 1.0 ) mprecip(moy) = 1.0
  !     do dm=1,ndaymonth(moy)
  !       doy = daysum + dm
  !       dval_prec(doy) = dval_prec(doy) * (mval_prec(moy) / mprecip(moy))
  !       if ( dval_prec(doy) < 0.1 ) dval_prec(doy) = 0.0
  !       ! dval_prec(doy) = mval_prec(moy) / ndaymonth(moy)  !no generator
  !     enddo
         
  !     ! Alternative: equal distribution of rain for fixed number of wet days
  !     ! prob = prob_rain(moy) + prob
  !     ! if (prob.ge.1.0) then   
  !     !   dval_prec(doy) = mprec(moy)
  !     !   prob = prob-1.0
  !     ! else
  !     !   dval_prec(doy) = 0.0
  !     !   prob = prob
  !     ! endif
                      
  !   enddo                     !month 
    

  ! end function monthly2daily_weather


  ! function monthly2daily( mval, method, monthistotal, mval_pvy, mval_nxy ) result( dval )
  !   !/////////////////////////////////////////////////////////////////////////
  !   ! Returns daily values based on monthly values, using a defined method.
  !   !-------------------------------------------------------------------------
  !   ! arguments
  !   real, dimension(nmonth), intent(in) :: mval  ! vector containing 12 monthly values
  !   character(len=*), intent(in) :: method
  !   logical, intent(in), optional :: monthistotal ! true of monthly values represent total of daily values in resp. month
  !   real, dimension(nmonth), intent(in), optional :: mval_pvy  ! vector containing 12 monthly values of the previous year
  !   real, dimension(nmonth), intent(in), optional :: mval_nxy  ! vector containing 12 monthly values of the next year

  !   ! function return variable
  !   real, dimension(ndayyear) :: dval
    
  !   ! local variables
  !   integer :: moy, doy, today, dm
  !   real :: dd, todaysval

  !   real, dimension(0:(nmonth+1))    :: mval_ext
  !   !integer, dimension(0:(nmonth+1)) :: middaymonth_ext
  !   real :: startt, endt, starttemp, endtemp, dt, d2t, d3t, dtold, &
  !     dtnew, lastmonthtemp, nextmonthtemp, deltatemp, polya, polyb, polyc
        
  !   if (method == "interpol") then
  !     !--------------------------------------------------------------------
  !     ! LINEAR INTERPOLATION
  !     ! of monthly to quasi-daily values.
  !     ! If optional argument 'mval_pvy' is provided, take December-value
  !     ! of previous year to interpolate to first 15 days of January,
  !     ! otherwise, use the same year's December value to get first 15 days.
  !     ! corresponds to subroutine 'daily' in LPX
  !     !--------------------------------------------------------------------

  !     ! define extended vector with monthly values for previous Dec and next Jan added
  !     mval_ext(1:nmonth)  = mval(1:nmonth)

  !     !middaymonth_ext(1:nmonth) = middaymonth(1:nmonth)
  !     !middaymonth_ext(0) = middaymonth(nmonth)
  !     !middaymonth_ext(nmonth+1) = 381

  !     if (present(mval_pvy)) then
  !       mval_ext(0) = mval_pvy(nmonth)   ! Dec value of previous year
  !     else
  !       mval_ext(0) = mval(nmonth)       ! take Dec value of this year ==> leads to jump!
  !     end if

  !     if (present(mval_nxy)) then
  !       mval_ext(nmonth+1) = mval_nxy(1) ! Jan value of next year
  !     else
  !       mval_ext(nmonth+1) = mval(1)     ! take Jan value of this year ==> leads to jump!
  !     end if

  !     do moy = 1,nmonth
  !       dd = (mval_ext(moy+1)-mval_ext(moy)) / real(middaymonth(moy+1) - middaymonth(moy))
  !       todaysval = mval_ext(moy)
  !       do doy = middaymonth(moy),middaymonth(moy+1)-1
  !         if (doy<=ndayyear) then
  !           today = doy
  !         else
  !           today = doy-ndayyear
  !         endif
  !         dval(today) = todaysval
  !         todaysval = todaysval + dd
  !       enddo
  !     enddo

  !     if (monthistotal) then
  !       doy = 0
  !       do moy=1,nmonth
  !         do dm=1,ndaymonth(moy)
  !           doy = doy+1
  !           dval(doy) = dval(doy) / real(ndaymonth(moy))
  !         enddo
  !       enddo
  !     endif

  !     !doy=1
  !     !do moy=1,nmonth
  !     !  do dm=1,ndaymonth(moy)
  !     !    doy=doy+1
  !     !    if (doy>middaymonth(moy)) then
  !     !      ! interpolate to next month
  !     !      dval(doy) = mval_ext(moy) + (doy-middaymonth_ext(moy))/ndaymonth_ext(moy) * (mval_ext(moy+1)-mval_ext(moy))
  !     !    else if (doy<middaymonth(moy)) then
  !     !      ! interpolate to previous month
  !     !      dval(doy) = mval_ext(moy-1) + (doy-middaymonth_ext(moy-1))/ndaymonth_ext(moy-1) * (mval_ext(moy)-mval_ext(moy-1))
  !     !    else
  !     !      ! take value directly
  !     !      dval(doy) = mval_ext(moy)
  !     !    end if
  !     !  end do
  !     !end do

  !     !  !if (iftotals) then
  !     !  doy=0
  !     !  do moy=1,nmonth
  !     !    do doyofmonth=1,ndaymonth(moy)
  !     !      doy=doy+1
  !     !      dval(doy)=dval(doy)/dble(ndaymonth(moy))
  !     !    enddo
  !     !  enddo
  !     !endif

  !   else if (method=="polynom") then
  !     !--------------------------------------------------------------------
  !     ! In addition to tempdaily daily values are calculated using a polynom of second
  !     ! order through the middpoints between months. Additionally, average of daily 
  !     ! values is identical to the monthly input data. That's crucial for modelling
  !     ! soil heat diffusion and freezing/thawing processes. 
  !     !--------------------------------------------------------------------!
  !     if (monthistotal) &
  !       stop 'MONTHLY2DAILY: no monthly totals allowed for polynom method'
      
  !     ! Starting conditons of december in previous year
  !     startt = -30.5               ! midpoint between Nov-Dec of previous year
  !     endt = 0.5                   ! midpoint between Dec-Jan of this year
  !     dt = real(ndaymonth(nmonth)) ! number of Dec days
  !     if (present(mval_pvy)) then
  !       lastmonthtemp = mval_pvy(nmonth) ! Dec mean temperature
  !     else
  !       lastmonthtemp = mval(nmonth)     ! Dec mean temperature
  !     end if

  !     doy = 0                      ! ((interface%steering%init))ialisation of this years days
      
  !     do moy=1,nmonth
  !       dtold = dt
  !       startt = endt
  !       endt = endt + dt
  !       if (moy<nmonth) then
  !         dtnew = real(ndaymonth(moy+1))
  !         nextmonthtemp = mval(moy+1)
  !       else
  !         dtnew = real(ndaymonth(1))
  !         if (present(mval_nxy)) then
  !           nextmonthtemp = mval_nxy(1)
  !         else
  !           nextmonthtemp = mval(1)
  !         end if
  !       endif

  !       starttemp = (mval(moy)*dt+lastmonthtemp*dtold)/(dt+dtold)
  !       endtemp = (nextmonthtemp*dtnew+mval(moy)*dt)/(dtnew+dt)
  !       deltatemp = endtemp-starttemp
        
  !       ! Calculate vars for a,b,c coefficients in polynom y = ax^2 +bx + c
  !       d2t = endt**2.0 - startt**2.0
  !       d3t = endt**3.0 - startt**3.0

  !       ! Take a sheet of paper and try solve the polynom, well here is the outcome
  !       polya = (mval(moy)*dt - deltatemp*d2t/dt/2.0 - starttemp*dt + deltatemp*startt) & 
  !         / (d3t/3.0 - d2t**2.0/dt/2.0 - dt*startt**2.0 + startt*d2t)
  !       polyb = deltatemp/dt - polya*(startt+endt)
  !       polyc = starttemp - polya*startt**2.0 - polyb*startt

  !       ! Calculate daily values with the polynom function
  !       do dm=1,ndaymonth(moy)
  !         doy = doy + 1
  !         dval(doy) = polya * real(doy)**2.0 + polyb * real(doy) + polyc
  !       enddo
  !       lastmonthtemp = mval(moy)
  !     enddo

  !   else if (method=="uniform" ) then
  !     !--------------------------------------------------------------------
  !     ! Each day in month has the same (monthly) value
  !     !--------------------------------------------------------------------!      
  !     doy=0
  !     do moy=1,nmonth
  !       do dm=1,ndaymonth(moy)
  !         doy=doy+1
  !         dval(doy) = mval(moy)
  !       end do
  !     end do

  !   else
  !     stop 'MONTHLY2DAILY: select viable case.'
  !   end if

  ! end function monthly2daily


  ! function read1year_daily( filename )
  !   !////////////////////////////////////////////////////////////////
  !   ! Function reads a file that contains 365 lines, each line for
  !   ! a daily value. 
  !   !----------------------------------------------------------------
  !   implicit none

  !   ! arguments
  !   character(len=*), intent(in) :: filename

  !   ! local variables
  !   real, dimension(ndayyear) :: dval

  !   ! function return value
  !   real, dimension(ndayyear) :: read1year_daily

  !   open(20, file='./input/'//filename, status='old',  form='formatted', action='read', err=888)
  !   read(20,*) dval
  !   close(20)

  !   read1year_daily = dval

  !   return
  !   !600 format (F10.7)
  !   888 write(0,*) 'READ1YEAR_DAILY: error opening file '//trim(filename)//'. Abort. '
  !   stop

  ! end function read1year_daily


  ! function read1year_monthly( filename )
  !   !////////////////////////////////////////////////////////////////
  !   ! Function reads a file that contains 12 lines, each line for
  !   ! a daily value. 
  !   !----------------------------------------------------------------
  !   implicit none

  !   ! arguments
  !   character(len=*), intent(in) :: filename

  !   ! local variables
  !   real, dimension(nmonth) :: mval

  !   ! function return value
  !   real, dimension(nmonth) :: read1year_monthly

  !   open(20, file='./input/'//trim(filename), status='old',  form='formatted', action='read', err=888)
  !   read(20,*) mval
  !   close(20)

  !   read1year_monthly = mval

  !   return
  !   !600 format (F10.7)
  !   888 write(0,*) 'READ1YEAR_MONTHLY: error opening file ./input/'//trim(filename)//'. Abort. '
  !   stop

  ! end function read1year_monthly


  function area( lat, dx, dy ) result( out_area )
    !////////////////////////////////////////////////////////////////
    ! Calculates grid cell area in m2 on a spherical Earth
    !----------------------------------------------------------------
    ! arguments
    real, intent(in) :: lat      ! latitude (degrees N)
    real, intent(in) :: dx       ! resolution in longitude (degrees)
    real, intent(in) :: dy       ! resolution in latitude (degrees)

    ! function return variable
    real :: out_area             ! gridcell surface area (m2)

    ! local variables
    real, parameter :: r_earth = 6370000

    out_area = 4.0 * r_earth**2 * 0.5 * dx * pi / 180.0 * cos( abs(lat) * pi / 180.0 ) * sin( 0.5 * dy * pi / 180.0 )    

  end function area


  function calc_patm( elv ) result( patm )
    !----------------------------------------------------------------   
    ! Calculates atmospheric pressure for a given elevation, assuming
    ! standard atmosphere at sea level (kPo)
    ! Ref:      Allen et al. (1998)
    ! This function is copied from SPLASH
    !----------------------------------------------------------------
    ! arguments
    real, intent(in) :: elv ! elevation above sea level, m

    ! function return value
    real ::  patm ! atmospheric pressure (Pa)

    patm = kPo * (1.0 - kL * elv / kTo) ** (kG * kMa * 1.e-3 / (kR * kL))

  end function calc_patm


  function median( vec, len ) result( out )
    !--------------------------------------------------------------------
    ! This function receives an array vec of N entries, copies its value
    ! to a local array Temp(), sorts Temp() and computes the median.
    ! The returned value is of REAL type.
    ! Copied from https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/median.f90
    !--------------------------------------------------------------------
    real, dimension(:), intent(in)     :: vec
    integer, intent(in)                :: len

    ! function return variable
    real                               :: out

    ! local variables
    real, dimension(1:len)             :: tmp

    tmp(:) = vec(:)

    call  sort(tmp, len)                ! sort the copy

    if (mod(len,2) == 0) then           ! compute the median
       out = (tmp(len/2) + tmp(len/2+1)) / 2.0
    else
       out = tmp(len/2+1)
    end if

  end function median


  subroutine sort( vec, len )
    !--------------------------------------------------------------------
    ! This subroutine receives an array vec() and sorts it into ascending
    ! order.
    ! Copied from https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/sort.f90
    !--------------------------------------------------------------------
    real, dimension(:), intent(inout)     :: vec
    integer, intent(in)                   :: len
    integer                               :: i
    integer                               :: location

    do i = 1, len-1                           ! eveccept for the last
      location = find_minimum( vec, i, len )  ! find min from this to last
      call swap( vec(i), vec(location) )      ! swap this and the minimum
    end do

  end subroutine sort


  subroutine swap( a, b )
    !--------------------------------------------------------------------
    ! This subroutine swaps the values of its two formal arguments.
    ! Copied from https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/sort.f90
    !--------------------------------------------------------------------
    real, intent(inout) :: a, b
    real                :: tmp

    tmp = a
    a   = b
    b   = tmp

  end subroutine  swap


  function find_minimum( vec, start, end ) result( out )
    !--------------------------------------------------------------------
    ! This function returns the location of the minimum in the section
    ! between start and end.
    ! Copied from https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/sort.f90
    !--------------------------------------------------------------------
    real, dimension(:), intent(in)     :: vec
    integer, intent(in)                :: start, end
    real                               :: minimum
    integer                            :: location
    integer                            :: i
    integer                            :: out

    minimum  = vec(start)          ! assume the first is the min
    location = start               ! record its position
    do i = start+1, end            ! start with nevect elements
      if (vec(i) < minimum) THEN  ! if vec(i) less than the min?
        minimum  = vec(i)        ! Yes, a new minimum found
        location = i             ! record its position
      end if
    end do
    out = location                 ! return the position
   
  end function find_minimum


  function findroot_quadratic( a, b, c ) result( root )
    !-----------------------------------------------------------------------
    ! Returns the solution for a quadratic function:
    ! a + bx + cx**2 = 0
    ! Per default returns root2 
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in) :: a, b, c

    ! function return variable
    real, dimension(2) :: root

    ! local variables
    real :: d

    d = b*b - 4.0*a*c
    if (d >= 0.0) then              ! is it solvable?
      d = sqrt(d)
      root(1) = (-b + d)/(2.0*c)    ! first root
      root(2) = (-b - d)/(2.0*c)    ! second root
    else                            ! complex roots
      ! stop 'findroot_quadratic(): There is no real root.'
    end if

  end function findroot_quadratic


  function dgcos( x ) result( dgcos_out )
    !----------------------------------------------------------------   
    ! Calculates the cosine of an angle given in degrees. Equal to 
    ! 'dsin' in Python version.
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: x  ! angle, degrees (0-360)

    ! function return value
    real :: dgcos_out ! cosine value of x when x is in degrees

    !dgcos = dcos(x*pi/180.0)
    dgcos_out = cos(x*pi/180.0)  ! xxx use cos with single-precision compilation

  end function dgcos


  function dgsin( x ) result( dgsin_out )
    !----------------------------------------------------------------   
    ! Calculates the sinus of an angle given in degrees. Equal to 
    ! 'dsin' in Python version.
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: x  ! angle, degrees (0-360)

    ! function return value
    real :: dgsin_out ! sinus value of x when x is in degrees

    !dgsin_out = dsin(x*pi/180.0)
    dgsin_out = sin(x*pi/180.0)   ! xxx use cos with single-precision compilation

  end function dgsin


  function degrees( x ) result( degrees_out )
    !----------------------------------------------------------------   
    ! Returns corresponding degrees if x is given in radians
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: x  ! angle, radians

    ! function return value
    real :: degrees_out

    degrees_out = x*180.0/pi

  end function degrees


  function radians( x ) result( radians_out )
    !----------------------------------------------------------------   
    ! Returns corresponding radians if x is given in degrees
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: x  ! angle, radians

    ! function return value
    real :: radians_out

    radians_out = x*pi/180.0

  end function radians
  

  function zero ( a, b, f, t ) result(val)

    !*****************************************************************************80
    !
    !! ZERO seeks the root of a function F(X) in an interval [A,B].
    !
    !  Discussion:
    !
    !    The interval [A,B] must be a change of sign interval for F.
    !    That is, F(A) and F(B) must be of opposite signs.  Then
    !    assuming that F is continuous implies the existence of at least
    !    one value C between A and B for which F(C) = 0.
    !
    !    The location of the zero is determined to within an accuracy
    !    of 6 * MACHEPS * abs ( C ) + 2 * T.
    !
    !    Thanks to Thomas Secretin for pointing out a transcription error in the
    !    setting of the value of P, 11 February 2013.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    11 February 2013
    !
    !  Author:
    !
    !    Original FORTRAN77 version by Richard Brent.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    Richard Brent,
    !    Algorithms for Minimization Without Derivatives,
    !    Dover, 2002,
    !    ISBN: 0-486-41998-3,
    !    LC: QA402.5.B74.
    !
    !  Parameters:
    !
    !    Input, real (kind = dbl8) A, B, the endpoints of the change of 
    !    sign interval.
    !
    !    Input, real (kind = dbl8) MACHEP, an estimate for the relative machine
    !    precision.
    !
    !    Input, real (kind = dbl8) T, a positive error tolerance.
    !
    !    Input, external real (kind = dbl8) F, the name of a user-supplied
    !    function, of the form "FUNCTION F ( X )", which evaluates the
    !    function whose zero is being sought.
    !
    !    Output, real (kind = dbl8) ZERO, the estimated value of a zero of
    !    the function F.
    !
    implicit none

    real (kind = dbl8)  ::  a, b, c, d, e
    real (kind = dbl8)  ::  f
    real (kind = dbl8)  ::  fa, fb, fc
    real (kind = dbl8)  ::  m
    real (kind = dbl8)  ::  machep
    real (kind = dbl8)  ::  p, q, r, s, sa, sb
    real (kind = dbl8)  ::  t
    real (kind = dbl8)  ::  tol
    real (kind = dbl8)  ::  val

    machep = epsilon ( 1D+00 )
    !
    !  Make local copies of A and B.
    !
    sa = a
    sb = b
    fa = f ( sa )
    fb = f ( sb )

    c = sa
    fc = fa
    e = sb - sa
    d = e

    do

    if ( abs ( fc ) < abs ( fb ) ) then

      sa = sb
      sb = c
      c = sa
      fa = fb
      fb = fc
      fc = fa

    end if

    tol = 2.0D+00 * machep * abs ( sb ) + t
    m = 0.5D+00 * ( c - sb )

    if ( abs ( m ) <= tol .or. fb == 0.0D+00 ) then
      exit
    end if

    if ( abs ( e ) < tol .or. abs ( fa ) <= abs ( fb ) ) then

      e = m
      d = e

    else

      s = fb / fa

      if ( sa == c ) then

      p = 2.0D+00 * m * s
      q = 1.0D+00 - s

      else

      q = fa / fc
      r = fb / fc
      p = s * ( 2.0D+00 * m * q * ( q - r ) - ( sb - sa ) * ( r - 1.0D+00 ) )
      q = ( q - 1.0D+00 ) * ( r - 1.0D+00 ) * ( s - 1.0D+00 )

      end if

      if ( 0.0D+00 < p ) then
      q = - q
      else
      p = - p
      end if

      s = e
      e = d

      if ( 2.0D+00 * p < 3.0D+00 * m * q - abs ( tol * q ) .and. &
      p < abs ( 0.5D+00 * s * q ) ) then
      d = p / q
      else
      e = m
      d = e
      end if

    end if

    sa = sb
    fa = fb

    if ( tol < abs ( d ) ) then
      sb = sb + d
    else if ( 0.0D+00 < m ) then
      sb = sb + tol
    else
      sb = sb - tol
    end if

    fb = f ( sb )

    if ( ( 0.0D+00 < fb .and. 0.0D+00 < fc ) .or. &
        ( fb <= 0.0D+00 .and. fc <= 0.0D+00 ) ) then
      c = sa
      fc = fa
      e = sb - sa
      d = e
    end if

    end do

    val = sb

  end function zero

  function alngam ( xvalue, ifault )

    !*****************************************************************************80
    !
    !! ALNGAM computes the logarithm of the gamma function.
    !
    !  Modified:
    !
    !    13 January 2008
    !
    !  Author:
    !
    !    Original FORTRAN77 version by Allan Macleod.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    Allan Macleod,
    !    Algorithm AS 245,
    !    A Robust and Reliable Algorithm for the Logarithm of the Gamma Function,
    !    Applied Statistics,
    !    Volume 38, Number 2, 1989, pages 397-402.
    !
    !  Parameters:
    !
    !    Input, real (kind = dbl8) XVALUE, the argument of the Gamma function.
    !
    !    Output, integer (kind = int4) IFAULT, error flag.
    !    0, no error occurred.
    !    1, XVALUE is less than or equal to 0.
    !    2, XVALUE is too big.
    !
    !    Output, real (kind = dbl8) ALNGAM, the logarithm of the gamma function of X.
    !
    implicit none
    
    real (kind = dbl8) alngam
    real (kind = dbl8), parameter :: alr2pi = 0.918938533204673D+00
    integer (kind = int4) ifault
    real (kind = dbl8), dimension ( 9 ) :: r1 = (/ &
      -2.66685511495D+00, &
      -24.4387534237D+00, &
      -21.9698958928D+00, &
      11.1667541262D+00, &
      3.13060547623D+00, &
      0.607771387771D+00, &
      11.9400905721D+00, &
      31.4690115749D+00, &
      15.2346874070D+00 /)
    real (kind = dbl8), dimension ( 9 ) :: r2 = (/ &
      -78.3359299449D+00, &
      -142.046296688D+00, &
      137.519416416D+00, &
      78.6994924154D+00, &
      4.16438922228D+00, &
      47.0668766060D+00, &
      313.399215894D+00, &
      263.505074721D+00, &
      43.3400022514D+00 /)
    real (kind = dbl8), dimension ( 9 ) :: r3 = (/ &
      -2.12159572323D+05, &
      2.30661510616D+05, &
      2.74647644705D+04, &
      -4.02621119975D+04, &
      -2.29660729780D+03, &
      -1.16328495004D+05, &
      -1.46025937511D+05, &
      -2.42357409629D+04, &
      -5.70691009324D+02 /)
    real (kind = dbl8), dimension ( 5 ) :: r4 = (/ &
      0.279195317918525D+00, &
      0.4917317610505968D+00, &
      0.0692910599291889D+00, &
      3.350343815022304D+00, &
      6.012459259764103D+00 /)
    real (kind = dbl8) ::  x
    real (kind = dbl8) ::  x1
    real (kind = dbl8) ::  x2
    real (kind = dbl8), parameter :: xlge = 5.10D+05
    real (kind = dbl8), parameter :: xlgst = 1.0D+30
    real (kind = dbl8) xvalue
    real (kind = dbl8) y
    
    x = xvalue
    alngam = 0.0D+00
    !
    !  Check the input.
    !
    if ( xlgst <= x ) then
      ifault = 2
      return
    end if
    
    if ( x <= 0.0D+00 ) then
      ifault = 1
      return
    end if
    
    ifault = 0
    !
    !  Calculation for 0 < X < 0.5 and 0.5 <= X < 1.5 combined.
    !
    if ( x < 1.5D+00 ) then
    
      if ( x < 0.5D+00 ) then
    
      alngam = - log ( x )
      y = x + 1.0D+00
    !
    !  Test whether X < machine epsilon.
    !
      if ( y == 1.0D+00 ) then
        return
      end if
    
      else
    
      alngam = 0.0D+00
      y = x
      x = ( x - 0.5D+00 ) - 0.5D+00
    
      end if
    
      alngam = alngam + x * (((( &
        r1(5)   * y &
      + r1(4) ) * y &
      + r1(3) ) * y &
      + r1(2) ) * y &
      + r1(1) ) / (((( &
            y &
      + r1(9) ) * y &
      + r1(8) ) * y &
      + r1(7) ) * y &
      + r1(6) )
    
      return
    
    end if
    !
    !  Calculation for 1.5 <= X < 4.0.
    !
    if ( x < 4.0D+00 ) then
    
      y = ( x - 1.0D+00 ) - 1.0D+00
    
      alngam = y * (((( &
        r2(5)   * x &
      + r2(4) ) * x &
      + r2(3) ) * x &
      + r2(2) ) * x &
      + r2(1) ) / (((( &
            x &
      + r2(9) ) * x &
      + r2(8) ) * x &
      + r2(7) ) * x &
      + r2(6) )
    !
    !  Calculation for 4.0 <= X < 12.0.
    !
    else if ( x < 12.0D+00 ) then
    
      alngam = (((( &
        r3(5)   * x &
      + r3(4) ) * x &
      + r3(3) ) * x &
      + r3(2) ) * x &
      + r3(1) ) / (((( &
            x &
      + r3(9) ) * x &
      + r3(8) ) * x &
      + r3(7) ) * x &
      + r3(6) )
    !
    !  Calculation for 12.0 <= X.
    !
    else
    
      y = log ( x )
      alngam = x * ( y - 1.0D+00 ) - 0.5D+00 * y + alr2pi
    
      if ( x <= xlge ) then
    
      x1 = 1.0D+00 / x
      x2 = x1 * x1
    
      alngam = alngam + x1 * ( ( &
          r4(3)   * &
        x2 + r4(2) ) * &
        x2 + r4(1) ) / ( ( &
        x2 + r4(5) ) * &
        x2 + r4(4) )
    
      end if
    
    end if
    
    return
  end

    
  function alnorm ( x, upper )
    
    !*****************************************************************************80
    !
    !! ALNORM computes the cumulative density of the standard normal distribution.
    !
    !  Modified:
    !
    !    13 January 2008
    !
    !  Author:
    !
    !    Original FORTRAN77 version by David Hill.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    David Hill,
    !    Algorithm AS 66:
    !    The Normal Integral,
    !    Applied Statistics,
    !    Volume 22, Number 3, 1973, pages 424-427.
    !
    !  Parameters:
    !
    !    Input, real (kind = dbl8) X, is one endpoint of the semi-infinite interval
    !    over which the integration takes place.
    !
    !    Input, logical UPPER, determines whether the upper or lower
    !    interval is to be integrated:
    !    .TRUE.  => integrate from X to + Infinity;
    !    .FALSE. => integrate from - Infinity to X.
    !
    !    Output, real (kind = dbl8) ALNORM, the integral of the standard normal
    !    distribution over the desired interval.
    !
    implicit none
    
    real (kind = dbl8), parameter :: a1 = 5.75885480458D+00
    real (kind = dbl8), parameter :: a2 = 2.62433121679D+00
    real (kind = dbl8), parameter :: a3 = 5.92885724438D+00
    real (kind = dbl8) alnorm
    real (kind = dbl8), parameter :: b1 = -29.8213557807D+00
    real (kind = dbl8), parameter :: b2 = 48.6959930692D+00
    real (kind = dbl8), parameter :: c1 = -0.000000038052D+00
    real (kind = dbl8), parameter :: c2 = 0.000398064794D+00
    real (kind = dbl8), parameter :: c3 = -0.151679116635D+00
    real (kind = dbl8), parameter :: c4 = 4.8385912808D+00
    real (kind = dbl8), parameter :: c5 = 0.742380924027D+00
    real (kind = dbl8), parameter :: c6 = 3.99019417011D+00
    real (kind = dbl8), parameter :: con = 1.28D+00
    real (kind = dbl8), parameter :: d1 = 1.00000615302D+00
    real (kind = dbl8), parameter :: d2 = 1.98615381364D+00
    real (kind = dbl8), parameter :: d3 = 5.29330324926D+00
    real (kind = dbl8), parameter :: d4 = -15.1508972451D+00
    real (kind = dbl8), parameter :: d5 = 30.789933034D+00
    real (kind = dbl8), parameter :: ltone = 7.0D+00
    real (kind = dbl8), parameter :: p = 0.398942280444D+00
    real (kind = dbl8), parameter :: q = 0.39990348504D+00
    real (kind = dbl8), parameter :: r = 0.398942280385D+00
    logical up
    logical upper
    real (kind = dbl8), parameter :: utzero = 18.66D+00
    real (kind = dbl8) x
    real (kind = dbl8) y
    real (kind = dbl8) z
    
    up = upper
    z = x
    
    if ( z < 0.0D+00 ) then
      up = .not. up
      z = - z
    end if
    
    if ( ltone < z .and. ( ( .not. up ) .or. utzero < z ) ) then
    
      if ( up ) then
      alnorm = 0.0D+00
      else
      alnorm = 1.0D+00
      end if
    
      return
    
    end if
    
    y = 0.5D+00 * z * z
    
    if ( z <= con ) then
    
      alnorm = 0.5D+00 - z * ( p - q * y &
      / ( y + a1 + b1 &
      / ( y + a2 + b2 & 
      / ( y + a3 ))))
    
    else
    
      alnorm = r * exp ( - y ) &
      / ( z + c1 + d1 &
      / ( z + c2 + d2 &
      / ( z + c3 + d3 &
      / ( z + c4 + d4 &
      / ( z + c5 + d5 &
      / ( z + c6 ))))))
    
    end if
    
    if ( .not. up ) then
      alnorm = 1.0D+00 - alnorm
    end if
    
    return
    end


    function gammad ( x, p, ifault )
    
    !*****************************************************************************80
    !
    !! GAMMAD computes the Lower Incomplete Gamma Integral y(a,x)/G(a)
    !
    !  Auxiliary functions:
    !
    !    ALOGAM = logarithm of the gamma function, 
    !    ALNORM = algorithm AS66
    !
    !  Modified:
    !
    !    20 January 2008
    !
    !  Author:
    !
    !    Original FORTRAN77 version by B Shea.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    B Shea,
    !    Algorithm AS 239:
    !    Chi-squared and Incomplete Gamma Integral,
    !    Applied Statistics,
    !    Volume 37, Number 3, 1988, pages 466-473.
    !
    !  Parameters:
    !
    !    Input, real (kind = dbl8) X, P, the parameters of the incomplete 
    !    gamma ratio.  0 <= X, and 0 < P.
    !
    !    Output, integer (kind = int4) IFAULT, error flag.
    !    0, no error.
    !    1, X < 0 or P <= 0.
    !
    !    Output, real (kind = dbl8) GAMMAD, the value of the incomplete 
    !    Gamma integral.
    !
    implicit none
    
    real (kind = dbl8) a
    ! real (kind = dbl8) alnorm
    ! real (kind = dbl8) alngam
    real (kind = dbl8) an
    real (kind = dbl8) arg
    real (kind = dbl8) b
    real (kind = dbl8) c
    real (kind = dbl8), parameter :: elimit = - 88.0D+00
    real (kind = dbl8) gammad
    integer (kind = int4) ifault
    real (kind = dbl8), parameter :: oflo = 1.0D+37
    real (kind = dbl8) p
    real (kind = dbl8), parameter :: plimit = 1000.0D+00
    real (kind = dbl8) pn1
    real (kind = dbl8) pn2
    real (kind = dbl8) pn3
    real (kind = dbl8) pn4
    real (kind = dbl8) pn5
    real (kind = dbl8) pn6
    real (kind = dbl8) rn
    real (kind = dbl8), parameter :: tol = 1.0D-14
    logical upper
    real (kind = dbl8) x
    real (kind = dbl8), parameter :: xbig = 1.0D+08
    
    gammad = 0.0D+00
    !
    !  Check the input.
    !
    if ( x < 0.0D+00 ) then
      ifault = 1
      return
    end if
    
    if ( p <= 0.0D+00 ) then
      ifault = 1
      return
    end if
    
    ifault = 0
    
    if ( x == 0.0D+00 ) then
      gammad = 0.0D+00
      return
    end if
    !
    !  If P is large, use a normal approximation.
    !
    if ( plimit < p ) then
    
      pn1 = 3.0D+00 * sqrt ( p ) * ( ( x / p )**( 1.0D+00 / 3.0D+00 ) &
      + 1.0D+00 / ( 9.0D+00 * p ) - 1.0D+00 )
    
      upper = .false.
      gammad = alnorm ( pn1, upper )
      return
    
    end if
    !
    !  If X is large set GAMMAD = 1.
    !
    if ( xbig < x ) then
      gammad = 1.0D+00
      return
    end if
    !
    !  Use Pearson's series expansion.
    !  (Note that P is not large enough to force overflow in ALOGAM).
    !  No need to test IFAULT on exit since P > 0.
    !
    if ( x <= 1.0D+00 .or. x < p ) then
    
      arg = p * log ( x ) - x - alngam ( p + 1.0D+00, ifault )
      c = 1.0D+00
      gammad = 1.0D+00
      a = p
    
      do
    
      a = a + 1.0D+00
      c = c * x / a
      gammad = gammad + c
    
      if ( c <= tol ) then
        exit
      end if
    
      end do
    
      arg = arg + log ( gammad )
    
      if ( elimit <= arg ) then
      gammad = exp ( arg )
      else
      gammad = 0.0D+00
      end if
    !
    !  Use a continued fraction expansion.
    !
    else 
    
      arg = p * log ( x ) - x - alngam ( p, ifault )
      a = 1.0D+00 - p
      b = a + x + 1.0D+00
      c = 0.0D+00
      pn1 = 1.0D+00
      pn2 = x
      pn3 = x + 1.0D+00
      pn4 = x * b
      gammad = pn3 / pn4
    
      do
    
      a = a + 1.0D+00
      b = b + 2.0D+00
      c = c + 1.0D+00
      an = a * c
      pn5 = b * pn3 - an * pn1
      pn6 = b * pn4 - an * pn2
    
      if ( pn6 /= 0.0D+00 ) then
    
        rn = pn5 / pn6
    
        if ( abs ( gammad - rn ) <= min ( tol, tol * rn ) ) then
        exit
        end if
    
        gammad = rn
    
      end if
    
      pn1 = pn3
      pn2 = pn4
      pn3 = pn5
      pn4 = pn6
    !
    !  Re-scale terms in continued fraction if terms are large.
    !
      if ( oflo <= abs ( pn5 ) ) then
        pn1 = pn1 / oflo
        pn2 = pn2 / oflo
        pn3 = pn3 / oflo
        pn4 = pn4 / oflo
      end if
    
      end do
    
      arg = arg + log ( gammad )
    
      if ( elimit <= arg ) then
      gammad = 1.0D+00 - exp ( arg )
      else
      gammad = 1.0D+00
      end if
    
    end if
    
    return
  end
    

end module md_sofunutils
