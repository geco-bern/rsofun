module md_biosphere_lm3ppa

  use datatypes
  use md_vegetation_lm3ppa
  use md_soil_lm3ppa
  use md_params_core_lm3ppa


  implicit none

  private
  public biosphere_annual

  !----------------------------------------------------------------
  ! ForestESS stuff
  !----------------------------------------------------------------
   type(vegn_tile_type),  pointer :: vegn
   type(soil_tile_type),  pointer :: soil
   type(cohort_type),     pointer :: cx, cc

  ! !----------------------------------------------------------------
  ! ! Module-specific (private) variables
  ! !----------------------------------------------------------------
  ! ! derived types from L1 modules
  ! type( tile_type ),         allocatable, dimension(:,:) :: tile
  ! type( tile_fluxes_type ),  allocatable, dimension(:)   :: tile_fluxes
  ! type( plant_type ),        allocatable, dimension(:,:) :: plant
  ! type( plant_fluxes_type ), allocatable, dimension(:)   :: plant_fluxes

  ! ! derived types from L2 modules
  ! type( solartype )                              :: solar
  ! type( outtype_pmodel ), dimension(npft,nmonth) :: out_pmodel ! P-model output variables for each month and PFT determined beforehand (per unit fAPAR and PPFD only)

contains

  function biosphere_annual() result( out_biosphere )
    !////////////////////////////////////////////////////////////////
    ! function BIOSPHERE_annual calculates net ecosystem exchange (nee)
    ! in response to environmental boundary conditions (atmospheric 
    ! CO2, temperature, Nitrogen deposition. This SR "replaces" 
    ! LPJ, also formulated as subroutine.
    ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
    ! contact: b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    use md_interface_lm3ppa, only: myinterface, outtype_biosphere
    use md_params_core, only: ntstepsyear
  
    ! return variable
    type(outtype_biosphere) :: out_biosphere

    ! ! local variables
    integer :: dm, moy, jpngr, doy
    ! logical, save           :: init_daily = .true.   ! is true only on the first day of the simulation 
    logical, parameter      :: verbose = .false.       ! change by hand for debugging etc.

    !----------------------------------------------------------------
    ! Biome-E stuff
    !----------------------------------------------------------------
    integer, parameter :: rand_seed = 86456
    integer, parameter :: totalyears = 10
    integer, parameter :: nCohorts = 1
    ! integer :: yr_data   ! Years of the forcing data
    ! integer :: days_data ! days of the forcing data
    ! integer :: steps_per_day ! 24 or 48
    real    :: timestep  ! hour, Time step of forcing data, usually hourly (1.0)
    real    :: tsoil, soil_theta
    real    :: NPPtree,fseed, fleaf, froot, fwood ! for output
    real    :: dDBH ! yearly growth of DBH, mm
    real    :: plantC,plantN, soilC, soilN
    real    :: dSlowSOM  ! for multiple tests only
    character(len=150) :: plantcohorts, plantCNpools, soilCNpools, allpools, faststepfluxes  ! output file names
    logical :: new_annual_cycle = .False.
    logical :: switch = .True.
    integer :: istat1, istat2, istat3
    integer :: year0,  year1, iyears
    integer :: fno1, fno2, fno3, fno4, fno5 ! output files
    integer :: totyears
    integer :: i, j, k, idays, idoy
    integer :: idata
    integer, save :: simu_steps !, datalines
    integer, save :: idays
    character(len=50) :: filepath_out, filesuffix
    character(len=50) :: parameterfile(10), chaSOM(10)
    character(len=50) :: namelistfile = '~/sofun/params/parameters_Allocation.nml' !'parameters_WC_biodiversity.nml' ! 'parameters_CN.nml'
    !  !'parameters_Konza.nml' !

    ! xxx debug
    integer :: idx, forcingyear

    !------------------------------------------------------------------------
    ! Create output files
    ! XXX add this to output instead
    !------------------------------------------------------------------------
    filepath_out = '/Users/lmarques/rsofun/output/'
    filesuffix   = '_test.csv' ! tag for simulation experiments
    plantcohorts = trim(filepath_out)//'Annual_cohorts'//trim(filesuffix)
    plantCNpools = trim(filepath_out)//'Cohorts_daily'//trim(filesuffix)  ! daily
    soilCNpools  = trim(filepath_out)//'Ecosystem_daily'//trim(filesuffix)
    allpools     = trim(filepath_out)//'Ecosystem_yearly'//trim(filesuffix)
    faststepfluxes = trim(filepath_out)//'PhotosynthesisDynamics'//trim(filesuffix) ! hourly

    fno1=91; fno2=101; fno3=102; fno4=103; fno5=104
    open(fno1, file=trim(faststepfluxes),ACTION='write', IOSTAT=istat1)
    open(fno2,file=trim(plantcohorts),   ACTION='write', IOSTAT=istat1)
    open(fno3,file=trim(plantCNpools),   ACTION='write', IOSTAT=istat2)
    open(fno4,file=trim(soilCNpools),    ACTION='write', IOSTAT=istat3)
    open(fno5,file=trim(allpools),       ACTION='write', IOSTAT=istat3)


    !------------------------------------------------------------------------
    ! Read in forcing data
    ! Requires data frame with one row for each time step (hourly) and columns:
    !
    !   YEAR     : year
    !   DOY      : day of the year  
    !   HOUR     : hour of the day
    !   PAR      : umol/m2/s
    !   Swdown   : W/m2         
    !   TEMP     : air temperature, deg C       
    !   STEMP    : soil temperature, deg C  
    !   RH       : relative humidity, %     
    !   RAIN     : kgH2O m-2 s-1
    !   WIND     : wind velocity (m s-1)       
    !   PRESSURE : Pa         
    !   aCO2_AW  : ???
    !   amb_co2  : ???       
    !
    ! To define array 'forcingData'
    ! This is how it's read from file input/Temperate_forcing.txt:
    !
    ! forcingData(i)%year      = year_data(i)                     ! Year
    ! forcingData(i)%doy       = doy_data(i)                      ! day of the year
    ! forcingData(i)%hod       = hour_data(i)                     ! hour of the day
    ! forcingData(i)%PAR       = input_data(1,i)                  ! umol/m2/s
    ! forcingData(i)%radiation = input_data(2,i)                  ! W/m2
    ! forcingData(i)%Tair      = input_data(3,i) + 273.16         ! air temperature, K
    ! forcingData(i)%Tsoil     = input_data(4,i) + 273.16         ! soil temperature, K
    ! forcingData(i)%RH        = input_data(5,i)                  ! relative humidity
    ! forcingData(i)%rain      = input_data(6,i)/(timestep * 3600)! kgH2O m-2 s-1
    ! forcingData(i)%windU     = input_data(7,i)                  ! wind velocity (m s-1)
    ! forcingData(i)%pressure  = input_data(8,i)                  ! pa
    ! forcingData(i)%soilwater = 0.8                              ! soil moisture, vol/vol
    !------------------------------------------------------------------------
    
    !----------------------------------------------------------------
    ! INITIALISATIONS
    !----------------------------------------------------------------
    if (myinterface%steering%init) then

      !------------------------------------------------------------------------
      ! Translation to LM3-PPA variables
      !------------------------------------------------------------------------
      ! head
      write(fno1,'(5(a8,","),25(a12,","))')      &
          'year','doy','hour','rad',            &
          'Tair','Prcp', 'GPP', 'Resp',         &
          'Transp','Evap','Runoff','Soilwater', &
          'wcl','FLDCAP','WILTPT'
      write(fno2,'(3(a5,","),25(a9,","))')            &
          'cID','PFT','layer','density', 'f_layer',  &
          'dDBH','dbh','height','Acrown',            &
          'wood','nsc', 'NSN','NPPtr','seed',        &
          'NPPL','NPPR','NPPW','GPP-yr','NPP-yr',    &
          'N_uptk','N_fix','maxLAI'

      write(fno3,'(5(a5,","),25(a8,","))')              &
          'year','doy','hour','cID','PFT',             &
          'layer','density', 'f_layer', 'LAI',         &
          'gpp','resp','transp',                       &
          'NPPleaf','NPProot','NPPwood',               &
          'NSC','seedC','leafC','rootC','SW-C','HW-C', &
          'NSN','seedN','leafN','rootN','SW-N','HW-N'

      write(fno4,'(2(a5,","),55(a10,","))')  'year','doy',    &
          'Tc','Prcp', 'totWs',  'Trsp', 'Evap','Runoff',    &
          'ws1','ws2','ws3', 'LAI','GPP', 'Rauto', 'Rh',     &
          'NSC','seedC','leafC','rootC','SW-C','HW-C',       &
          'NSN','seedN','leafN','rootN','SW-N','HW-N',       &
          'McrbC', 'fastSOM',   'slowSOM',                   &
          'McrbN', 'fastSoilN', 'slowSoilN',                 &
          'mineralN', 'N_uptk'

      write(fno5,'(1(a5,","),80(a12,","))')  'year',              &
          'CAI','LAI','GPP', 'Rauto',   'Rh',                    &
          'rain','SoilWater','Transp','Evap','Runoff',           &
          'plantC','soilC',    'plantN', 'soilN','totN',         &
          'NSC', 'SeedC', 'leafC', 'rootC', 'SapwoodC', 'WoodC', &
          'NSN', 'SeedN', 'leafN', 'rootN', 'SapwoodN', 'WoodN', &
          'McrbC','fastSOM',   'SlowSOM',                        &
          'McrbN','fastSoilN', 'slowSoilN',                      &
          'mineralN', 'N_fxed','N_uptk','N_yrMin','N_P2S','N_loss', &
          'seedC','seedN','Seedling-C','Seedling-N'

      !------------------------------------------------------------------------
      ! Initialisations
      !------------------------------------------------------------------------
      allocate(out_biosphere%hourly_tile(ntstepsyear))

      ! Parameter initialization: Initialize PFT parameters
      call initialize_PFT_data(namelistfile)

      ! Initialize vegetation tile and plant cohorts
      allocate(vegn)
      call initialize_vegn_tile(vegn,nCohorts,namelistfile)
      
      ! Sort and relayer cohorts
      call relayer_cohorts(vegn)
      call Zero_diagnostics(vegn)

      !------------------------------------------------------------------------
      ! Read in forcing data
      ! This reads it all into memory and then extracts from the huge array in
      ! daily/hourly steps
      !------------------------------------------------------------------------
      ! !call read_FACEforcing(forcingData,datalines,days_data,yr_data,timestep)
      ! call read_NACPforcing(forcingData,datalines,days_data,yr_data,timestep)
      ! steps_per_day = int(24.0/timestep)
      ! dt_fast_yr = 1.0/(365.0 * steps_per_day)
      ! step_seconds = 24.0*3600.0/steps_per_day ! seconds_per_year * dt_fast_yr
      ! write(*,*)steps_per_day,dt_fast_yr,step_seconds

      year0 = myinterface%climate(1)%year  ! forcingData(1)%year
      iyears = 1
      idoy   = 0
      idays  = 0

    endif 
    simu_steps = 0


    !----------------------------------------------------------------
    ! LOOP THROUGH GRIDCELLS
    !----------------------------------------------------------------
    if (verbose) print*,'looping through gridcells ...'
    gridcellloop: do jpngr=1,size(myinterface%grid)

      if (myinterface%grid(jpngr)%dogridcell) then

        !----------------------------------------------------------------
        ! LOOP THROUGH MONTHS
        !----------------------------------------------------------------
        doy=0
        monthloop: do moy=1,nmonth

          !----------------------------------------------------------------
          ! LOOP THROUGH DAYS
          !----------------------------------------------------------------
          dayloop: do dm=1,ndaymonth(moy)
            doy=doy+1
            idays = idays + 1


            if (verbose) print*,'----------------------'
            if (verbose) print*,'YEAR, Doy ', myinterface%steering%year, doy
            if (verbose) print*,'----------------------'

            idoy = idoy + 1

            !----------------------------------------------------------------
            ! Get daily mean air and soil temperature
            !----------------------------------------------------------------
            ! vegn%Tc_daily = myinterface%climate(jpngr)%dtemp(doy)
            ! tsoil         =                                       ! soil temp. is prescribed
            ! soil_theta    = 

            ! ForestESS:
            ! get daily mean temperature from hourly/half-hourly data
            vegn%Tc_daily = 0.0
            tsoil         = 0.0


            do i=1,myinterface%steps_per_day

          ! idata = MOD(simu_steps, myinterface%datalines)+1
          idata = simu_steps + 1
          year0 =  myinterface%climate(idata)%year  ! Current year
          vegn%Tc_daily = vegn%Tc_daily +  myinterface%climate(idata)%Tair

          ! if (iyears>2 .and. doy >60) print*,myinterface%climate(idata)
          ! if (iyears>2 .and. doy >60) stop

          tsoil         = myinterface%climate(idata)%tsoil
          simu_steps    = simu_steps + 1

          ! fast-step calls, hourly or half-hourly
          ! if (idata==8000) then
          !   print*,'B i ', i
          !   print*,'idata ', idata
          !   print*,' myinterface%climate(idata)%year',  myinterface%climate(idata)%year
          !   print*,' myinterface%climate(idata)%doy',  myinterface%climate(idata)%doy
          !   print*,' myinterface%climate(idata)%hod',  myinterface%climate(idata)%hod
          !   print*,' myinterface%climate(idata)%PAR',  myinterface%climate(idata)%PAR
          !   print*,' myinterface%climate(idata)%radiation',  myinterface%climate(idata)%radiation
          !   print*,' myinterface%climate(idata)%Tair',  myinterface%climate(idata)%Tair
          !   print*,' myinterface%climate(idata)%Tsoil',  myinterface%climate(idata)%Tsoil
          !   print*,' myinterface%climate(idata)%rain',  myinterface%climate(idata)%rain
          !   print*,' myinterface%climate(idata)%windU',  myinterface%climate(idata)%windU
          !   print*,' myinterface%climate(idata)%P_air',  myinterface%climate(idata)%P_air
          !   print*,' myinterface%climate(idata)%RH',  myinterface%climate(idata)%RH
          !   print*,' myinterface%climate(idata)%CO2',  myinterface%climate(idata)%CO2
          !   print*,' myinterface%climate(idata)%soilwater',  myinterface%climate(idata)%soilwater
          !   stop
          ! end if

           call vegn_CNW_budget_fast(vegn, myinterface%climate(idata))
          
          ! diagnostics
          call hourly_diagnostics(vegn, myinterface%climate(idata), iyears, idoy, i, idays, fno1, out_biosphere%hourly_tile(idata) )

        enddo ! hourly or half-hourly
        vegn%Tc_daily = vegn%Tc_daily/myinterface%steps_per_day
        tsoil         = tsoil/myinterface%steps_per_day
        soil_theta    = vegn%thetaS

        ! print*,'vegn%NSC, vegn%NSN, vegn%SapwoodC, vegn%leafC, vegn%rootC', vegn%NSC, vegn%NSN, vegn%SapwoodC, vegn%leafC, vegn%rootC
        ! if (iyears>1000) stop 'here'

        ! if (simu_steps==17520) then
        !   print*,'vegn%n_cohorts', vegn%n_cohorts
        !   do idx = 1,vegn%n_cohorts
        !     print*,'idx, nindivs', idx, vegn%cohorts(idx)
        !   end do
        !   print*,'tsoil ', tsoil
        !   stop 'beni'
        ! end if



        !-------------------------------------------------
        ! Daily calls
        !-------------------------------------------------
        !print*,'6: ', vegn%SapwoodC

        call daily_diagnostics(vegn, myinterface%climate(idata), iyears, idoy, idays, fno3, fno4, out_biosphere%daily_cohorts(doy,:), out_biosphere%daily_tile(doy) )

        ! Determine start and end of season and maximum leaf (root) mass
        !print*,'7: ', vegn%SapwoodC
        call vegn_phenology(vegn, j)

        ! Kill all individuals of a cohort if NSC falls below threshold
        !call vegn_starvation(vegn)

        ! Produce new biomass from 'carbon_gain' (is zero afterwards)
        !print*,'8: ', vegn%SapwoodC
        call vegn_growth_EW(vegn)

        !----------------------------------------------------------------
        ! populate function return variable
        !----------------------------------------------------------------

      end do dayloop

    end do monthloop

    !----------------------------------------------------------------
    ! Annual calls
    !----------------------------------------------------------------
    idoy = 0

    print*,'sim. year  ', iyears
    print*,'real year: ', year0


    if(update_annualLAImax) call vegn_annualLAImax_update(vegn)

    call annual_diagnostics(vegn, iyears, fno2, fno5, out_biosphere%annual_cohorts(:), out_biosphere%annual_tile)

    !---------------------------------------------
    ! Reproduction and mortality
    !---------------------------------------------        
    ! Kill all individuals in a cohort if NSC falls below critical point
    call vegn_annual_starvation(vegn)

    ! Natural mortality (reducing number of individuals 'nindivs')
    ! (~Eq. 2 in Weng et al., 2015 BG)
    call vegn_nat_mortality(vegn, real(seconds_per_year))

    ! seed C and germination probability (~Eq. 1 in Weng et al., 2015 BG)
    call vegn_reproduction(vegn)

    !---------------------------------------------
    ! Re-organize cohorts
    !---------------------------------------------
    call kill_lowdensity_cohorts(vegn)
    call relayer_cohorts(vegn)
    call vegn_mergecohorts(vegn)

    !---------------------------------------------
    ! Set annual variables zero
    !---------------------------------------------
    call Zero_diagnostics(vegn)

    ! update the years of model run
    iyears = iyears + 1

    if (myinterface%steering%finalize) then
      !----------------------------------------------------------------
      ! Finazlize run: deallocating memory
      !----------------------------------------------------------------
      close(91)
      close(101)
      close(102)
      close(103)
      close(104)
      deallocate(vegn%cohorts)
      !deallocate(myinterface%climate)
      !deallocate(out_biosphere%hourly_tile)
      ! stop 'actually finalizing'

    end if

    if (verbose) print*,'Done with biosphere for this year. Guete Rutsch!'

  end function biosphere_annual

end module md_biosphere



  !========================================================================
  ! read in forcing data (Users need to write their own data input procedure)
  subroutine read_FACEforcing(forcingData,datalines,days_data,yr_data,timestep)
    type(climate_data_type),pointer,intent(inout) :: forcingData(:)
    integer,intent(inout) :: datalines,days_data,yr_data
    real, intent(inout)   :: timestep
    !------------local var -------------------
    type(climate_data_type), pointer :: climateData(:)
    character(len=80)  commts
    integer, parameter :: niterms=9       ! MDK data for Oak Ridge input
    integer, parameter :: ilines=22*366*24 ! the maxmum records of Oak Ridge FACE, 1999~2007
    integer,dimension(ilines) :: year_data
    real,   dimension(ilines) :: doy_data,hour_data
    real input_data(niterms,ilines)
    real inputstep
    integer :: istat1,istat2,istat3
    integer :: doy,idays
    integer :: i,j,k
    integer :: m,n

    climfile=trim(filepath_in)//trim(climfile)

  ! open forcing data
    open(11,file=climfile,status='old',ACTION='read',IOSTAT=istat2)
    write(*,*)istat2
  ! skip 2 lines of input met data file
    read(11,'(a160)') commts
  ! read(11,'(a160)') commts ! MDK data only has one line comments
    m       = 0  ! to record the lines in a file
    idays   = 1  ! the total days in a data file
    yr_data = 0 ! to record years of a dataset
    do    ! read forcing files
        m=m+1
        read(11,*,IOSTAT=istat3)year_data(m),doy_data(m),hour_data(m),   &
                                (input_data(n,m),n=1,niterms)
        if(istat3<0)exit
        if(m == 1) then
            doy = doy_data(m)
        else
            doy = doy_data(m-1)
        endif
        if(doy /= doy_data(m)) idays = idays + 1
        !write(*,*)year_data(m),doy_data(m),hour_data(m)
    enddo ! end of reading the forcing file

    timestep = hour_data(2) - hour_data(1)
    write(*,*)"forcing",datalines,yr_data,timestep,dt_fast_yr
    if (timestep==1.0)then
        write(*,*)"the data freqency is hourly"
    elseif(timestep==0.5)then
        write(*,*)"the data freqency is half hourly"
    else
        write(*,*)"Please check time step!"
        stop
    endif
    close(11)    ! close forcing file
  ! Put the data into forcing 
    datalines = m - 1
    days_data = idays
    yr_data  = year_data(datalines-1) - year_data(1) + 1

    allocate(climateData(datalines))
    do i=1,datalines
       climateData(i)%year      = year_data(i)          ! Year
       climateData(i)%doy       = doy_data(i)           ! day of the year
       climateData(i)%hod       = hour_data(i)          ! hour of the day
       climateData(i)%PAR       = input_data(1,i)       ! umol/m2/s
       climateData(i)%radiation = input_data(2,i)       ! W/m2
       climateData(i)%Tair      = input_data(3,i) + 273.16  ! air temperature, K
       climateData(i)%Tsoil     = input_data(4,i) + 273.16  ! soil temperature, K
       climateData(i)%RH        = input_data(5,i) * 0.01    ! relative humidity (0.xx)
       climateData(i)%rain      = input_data(6,i)/(timestep * 3600)! ! kgH2O m-2 s-1
       climateData(i)%windU     = input_data(7,i)        ! wind velocity (m s-1)
       climateData(i)%P_air     = input_data(8,i)        ! pa
       climateData(i)%CO2       = input_data(9,i) * 1.0e-6       ! mol/mol
       climateData(i)%soilwater = 0.8    ! soil moisture, vol/vol
    enddo
    forcingData => climateData
    write(*,*)"forcing", datalines,days_data,yr_data
  end subroutine read_FACEforcing

  !=============================================================
  ! for reading in NACP site synthesis forcing
subroutine read_NACPforcing(forcingData,datalines,days_data,yr_data,timestep)
  type(climate_data_type),pointer,intent(inout) :: forcingData(:)
  integer,intent(inout) :: datalines,days_data,yr_data
  real, intent(inout)   :: timestep
  !------------local var -------------------
  type(climate_data_type), pointer :: climateData(:)
  character(len=80)  commts
  integer, parameter :: niterms=15       ! NACP site forcing
  integer, parameter :: ilines=22*366*48 ! the maxmum records
  integer,dimension(ilines) :: year_data
  real,   dimension(ilines) :: doy_data,hour_data
  real input_data(niterms,ilines)
  real inputstep
  integer :: istat1,istat2,istat3
  integer :: doy,idays
  integer :: i,j,k
  integer :: m,n

  ! xxx try
  integer :: idx_climatedata

  climfile=trim(filepath_in)//trim(climfile)
  write(*,*)'inputfile: ',climfile
! open forcing data
  open(11,file=climfile,status='old',ACTION='read',IOSTAT=istat2)
  write(*,*)istat2
! skip 2 lines of input met data file
  read(11,'(a160)') commts
  read(11,'(a160)') commts
  m       = 0  ! to record the lines in a file
  idays   = 1  ! the total days in a data file
  yr_data = 0 ! to record years of a dataset
  do    ! read forcing files
      m=m+1
      read(11,*,IOSTAT=istat3)year_data(m),doy_data(m),hour_data(m),   &
                              (input_data(n,m),n=1,niterms)

      if(istat3<0)exit
      if(m == 1) then
          doy = doy_data(m)
      else
          doy = doy_data(m-1)
      endif
      if(doy /= doy_data(m)) idays = idays + 1
      !write(*,*)year_data(m),doy_data(m),hour_data(m)
      ! discard one line
      !read(11,*,IOSTAT=istat3)year_data(m),doy_data(m),hour_data(m),   &
      !                        (input_data(n,m),n=1,niterms)
  enddo ! end of reading the forcing file

  timestep = hour_data(2) - hour_data(1)
  write(*,*)"forcing",datalines,yr_data,timestep,dt_fast_yr
  if (timestep==1.0)then
      write(*,*)"the data freqency is hourly"
  elseif(timestep==0.5)then
      write(*,*)"the data freqency is half hourly"
  else
      write(*,*)"Please check time step!"
      stop
  endif
  close(11)    ! close forcing file
! Put the data into forcing 
  datalines = m - 1
  days_data = idays
  yr_data  = year_data(datalines-1) - year_data(1) + 1

  allocate(climateData(datalines))

  ! xxx try
  allocate(climateData(datalines - 96))
  days_data = days_data - 2

  idx_climatedata = 0
  do i=1,datalines

     if (.not. (doy_data(i)==60 .and. (year_data(i)==2000 .or. year_data(i)==2004))) then
  
       idx_climatedata = idx_climatedata + 1

       climateData(idx_climatedata)%year      = year_data(i)          ! Year
       climateData(idx_climatedata)%doy       = doy_data(i)           ! day of the year
       climateData(idx_climatedata)%hod       = hour_data(i)          ! hour of the day
       climateData(idx_climatedata)%PAR       = input_data(11,i)*2.0  ! umol/m2/s
       climateData(idx_climatedata)%radiation = input_data(11,i)      ! W/m2
       climateData(idx_climatedata)%Tair      = input_data(1,i)       ! air temperature, K
       climateData(idx_climatedata)%Tsoil     = input_data(1,i)       ! soil temperature, K
       climateData(idx_climatedata)%rain      = input_data(7,i)       ! kgH2O m-2 s-1
       climateData(idx_climatedata)%windU     = input_data(5,i)        ! wind velocity (m s-1)
       climateData(idx_climatedata)%P_air     = input_data(9,i)        ! pa
       climateData(idx_climatedata)%RH        = input_data(3,i)/mol_h2o*mol_air* & ! relative humidity (0.xx)
                                  climateData(idx_climatedata)%P_air/esat(climateData(idx_climatedata)%Tair-273.16)
       climateData(idx_climatedata)%CO2       = input_data(15,i) * 1.0e-6       ! mol/mol
       climateData(idx_climatedata)%soilwater = 0.8    ! soil moisture, vol/vol

       if (abs(climateData(idx_climatedata)%hod-12.0)<0.001) then
         print*,'A year, doy, Tair ', climateData(idx_climatedata)%year, &
          climateData(idx_climatedata)%doy, &
          climateData(idx_climatedata)%Tair
       end if 

     end if

  enddo
  forcingData => climateData

  ! xxx try
  datalines = datalines - 96

  write(*,*)"forcing", datalines,days_data,yr_data

end subroutine read_NACPforcing


end module md_biosphere_lm3ppa
