module pmodel_mod
  !////////////////////////////////////////////////////////////////
  ! Contains main subroutine handling I/O with C and R.
  !----------------------------------------------------------------
  use, intrinsic :: iso_c_binding, only: c_double, c_int, c_char, c_bool

  implicit none

  private
  public :: pmodel_f, pmodel_onestep_f

contains

  subroutine pmodel_onestep_f( &
    lc4,                       &
    par,                       &
    forcing,                   &
    output                     &
    ) bind(C, name = "pmodel_onestep_f_")
    !////////////////////////////////////////////////////////////////
    ! Main subroutine to handle I/O with C and R for a run with one
    ! "time" step.
    ! Receives simulation parameters, site parameters. Forcing is for 
    ! one step.
    !----------------------------------------------------------------
    use md_params_core, only: eps, c_molmass
    use md_photosynth

    implicit none

    ! arguments
    integer(kind=c_int),  intent(in) :: lc4         ! logical type is not supported in the C interface (LTO)
    real(kind=c_double),  dimension(6), intent(in) :: par  ! free (calibratable) model parameters
    real(kind=c_double),  dimension(1,5), intent(in) :: forcing  ! 1-row array containing forcing data (rows: time steps; columns: 1=air temperature, 2=vpd, 3=ppfd, 4=co2, 5=patm) 
    real(kind=c_double),  dimension(9), intent(out) :: output

    ! local variables: model parameters
    real :: kphio, kphio_par_a, kphio_par_b, beta_unitcostratio, rd_to_vcmax, kc_jmax

    ! local variables: forcing
    real :: temp   ! temperature (deg C), relevant for photosynthesis
    real :: vpd    ! vapour pressure deficit (Pa)
    real :: ppfd   ! photosynthetic photon flux density (mol m-2 s-1)
    real :: co2    ! (ambient) atmospheric CO2 concentration (ppm) 
    real :: patm   ! atmospheric pressure (Pa)

    ! local variables: derived quantities to output
    real :: kphio_temp  ! quantum yield, modified by temperature 
    real :: vcmax       ! maximum rate of Rubisco carboxylation (mol m-2 s-1)
    real :: jmax        ! maximum rate of electron transport (mol m-2 s-1)
    real :: rd          ! dark respiration rate (mol m-2 s-1)
    real :: bigdelta    ! 13C discrimination by photosynthesis, expressed as the difference to the atmospheric signature (permil)

    ! other local variables
    logical :: c4
    type(outtype_pmodel) :: out_pmodel  ! list of P-model output variables

    ! convert integer to logcial
    c4 = lc4 /= 0

    !----------------------------------------------------------------
    ! GET CALIBRATABLE MODEL PARAMETERS (so far a small list)
    !----------------------------------------------------------------
    kphio              = real(par(1)) ! (mol mol-1)
    kphio_par_a        = real(par(2)) ! ((deg C)-2)
    kphio_par_b        = real(par(3)) ! (deg C)
    beta_unitcostratio = real(par(4)) ! (-)
    rd_to_vcmax        = real(par(5)) ! (-/-), ratio of rd to Vcmax
    kc_jmax            = real(par(6)) ! (-), jmax cost ratio, a.k.a c*

    !----------------------------------------------------------------
    ! GET FORCING
    !----------------------------------------------------------------
    temp = real(forcing(1,1)) ! (deg C)
    vpd  = real(forcing(1,2)) ! (Pa)
    ppfd = real(forcing(1,3)) ! (mol m-2 s-1) (while netrad is W m-2)
    co2  = real(forcing(1,4)) ! (ppm)
    patm = real(forcing(1,5)) ! (Pa)

    !----------------------------------------------------------------
    ! Low-temperature effect on quantum yield efficiency 
    !----------------------------------------------------------------
    ! take the instananeously varying temperature for governing quantum yield variations
    if (abs(kphio_par_a) < eps) then
      
      kphio_temp = kphio
    
    else
      
      kphio_temp = calc_kphio_temp( &
        temp, &
        c4,           &
        kphio,         &
        kphio_par_a,   &
        kphio_par_b    &
        )

    end if

    !================================================================
    ! P-model call to get acclimated quantities as a function of the
    ! damped climate forcing.
    !----------------------------------------------------------------
    out_pmodel = pmodel(  &
      kphio          = kphio_temp, &
      beta           = beta_unitcostratio, &
      kc_jmax        = kc_jmax, &
      ppfd           = ppfd, &
      co2            = co2, &
      tc             = temp, &
      vpd            = vpd, &
      patm           = patm, &
      c4             = c4, &
      method_optci   = "prentice14", &
      method_jmaxlim = "wang17" &
      )

    ! derived quantities
    vcmax    = calc_ftemp_inst_vcmax( temp, temp, tcref = 25.0 ) * out_pmodel%vcmax25
    jmax     = calc_ftemp_inst_jmax(  temp, temp, tcref = 25.0 ) * out_pmodel%jmax25
    rd       = out_pmodel%vcmax25 * rd_to_vcmax * calc_ftemp_inst_rd( temp ) * c_molmass
    bigdelta = calc_bigdelta( out_pmodel%chi, out_pmodel%ca, out_pmodel%gammastar )

    !----------------------------------------------------------------
    ! Populate Fortran output array which is passed back to C/R
    !----------------------------------------------------------------
    output(1) = dble(vcmax)  
    output(2) = dble(jmax)    
    output(3) = dble(out_pmodel%vcmax25) 
    output(4) = dble(out_pmodel%jmax25)
    output(5) = dble(out_pmodel%gs_setpoint)
    output(6) = dble(out_pmodel%chi)
    output(7) = dble(out_pmodel%iwue)
    output(8) = dble(rd)
    output(9) = dble(bigdelta)

  end subroutine pmodel_onestep_f


  subroutine pmodel_f(         &
    spinup,                    &   
    spinupyears,               &        
    recycle,                   &    
    firstyeartrend,            &           
    nyeartrend,                &  
    secs_per_tstep,            &     
    in_ppfd,                   &    
    in_netrad,                 &      
    outdt,                     &  
    ltre,                      & 
    ltne,                      & 
    ltrd,                      & 
    ltnd,                      & 
    lgr3,                      & 
    lgn3,                      & 
    lgr4,                      & 
    longitude,                 &      
    latitude,                  &     
    altitude,                  &   
    whc,                       &
    nt,                        &
    par,                       &
    forcing,                   &
    output                     &
    ) bind(C, name = "pmodel_f_")
    !////////////////////////////////////////////////////////////////
    ! Main subroutine to handle I/O with C and R for a time series
    ! simulation with dynamic acclimation of photosynthesis. 
    ! Receives simulation parameters, site parameters, and the full 
    ! simulation's forcing as time series
    !----------------------------------------------------------------
    use md_forcing_pmodel, only: getclimate, getco2, getfapar, get_fpc_grid
    use md_interface_pmodel, only: interfacetype_biosphere, outtype_biosphere, myinterface
    use md_params_core
    use md_biosphere_pmodel, only: biosphere_annual

    implicit none

    ! arguments
    integer(kind=c_int),  intent(in) :: spinup         ! logical type is not supported in the C interface (LTO)
    integer(kind=c_int),  intent(in) :: spinupyears
    integer(kind=c_int),  intent(in) :: recycle
    integer(kind=c_int),  intent(in) :: firstyeartrend
    integer(kind=c_int),  intent(in) :: nyeartrend
    integer(kind=c_int),  intent(in) :: secs_per_tstep
    integer(kind=c_int),  intent(in) :: in_ppfd        ! logical
    integer(kind=c_int),  intent(in) :: in_netrad      ! logical
    integer(kind=c_int),  intent(in) :: outdt
    integer(kind=c_int), intent(in) :: ltre            ! logical
    integer(kind=c_int), intent(in) :: ltne            ! logical
    integer(kind=c_int), intent(in) :: ltrd            ! logical
    integer(kind=c_int), intent(in) :: ltnd            ! logical
    integer(kind=c_int), intent(in) :: lgr3            ! logical
    integer(kind=c_int), intent(in) :: lgn3            ! logical
    integer(kind=c_int), intent(in) :: lgr4            ! logical
    real(kind=c_double),  intent(in) :: longitude
    real(kind=c_double),  intent(in) :: latitude
    real(kind=c_double),  intent(in) :: altitude
    real(kind=c_double),  intent(in) :: whc
    integer(kind=c_int),  intent(in) :: nt ! number of time steps
    real(kind=c_double),  dimension(9), intent(in) :: par  ! free (calibratable) model parameters
    real(kind=c_double),  dimension(nt,12), intent(in) :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=fapar, 10=patm, 11=tmin, 12=tmax) 
    real(kind=c_double),  dimension(nt,21), intent(out) :: output

    ! local variables
    type(outtype_biosphere) :: out_biosphere  ! holds all the output used for calculating the cost or maximum likelihood function 
    integer :: yr, idx_start, idx_end ! npft_local

    !----------------------------------------------------------------
    ! GET SIMULATION PARAMETERS
    !----------------------------------------------------------------
    myinterface%params_siml%steering_input%do_spinup      = spinup /= 0
    myinterface%params_siml%steering_input%spinupyears    = spinupyears
    myinterface%params_siml%steering_input%recycle        = recycle
    myinterface%params_siml%steering_input%firstyeartrend = firstyeartrend
    myinterface%params_siml%steering_input%nyeartrend     = nyeartrend

    if (myinterface%params_siml%steering_input%do_spinup) then
      myinterface%params_siml%steering_input%runyears = myinterface%params_siml%steering_input%nyeartrend &
              + myinterface%params_siml%steering_input%spinupyears
    else
      myinterface%params_siml%steering_input%runyears = myinterface%params_siml%steering_input%nyeartrend
      myinterface%params_siml%steering_input%spinupyears = 0
    endif
    
    myinterface%params_siml%in_ppfd            = in_ppfd /= 0
    myinterface%params_siml%in_netrad          = in_netrad /= 0
    myinterface%params_siml%outdt              = outdt
    myinterface%params_siml%ltre               = ltre /= 0
    myinterface%params_siml%ltne               = ltne /= 0
    myinterface%params_siml%ltrd               = ltrd /= 0
    myinterface%params_siml%ltnd               = ltnd /= 0
    myinterface%params_siml%lgr3               = lgr3 /= 0
    myinterface%params_siml%lgn3               = lgn3 /= 0
    myinterface%params_siml%lgr4               = lgr4 /= 0
    myinterface%params_siml%secs_per_tstep     = secs_per_tstep

    !----------------------------------------------------------------
    ! GET GRID INFORMATION
    !----------------------------------------------------------------
    myinterface%grid%lon = real( longitude )
    myinterface%grid%lat = real( latitude )
    myinterface%grid%elv = real( altitude )

    !----------------------------------------------------------------
    ! GET SOIL PARAMETERS
    !----------------------------------------------------------------
    myinterface%whc_prescr = real( whc )
    
    !----------------------------------------------------------------
    ! GET CALIBRATABLE MODEL PARAMETERS (so far a small list)
    !----------------------------------------------------------------
    myinterface%params_calib%kphio              = real(par(1)) ! (mol mol-1)
    myinterface%params_calib%kphio_par_a        = real(par(2)) ! ((deg C)-2)
    myinterface%params_calib%kphio_par_b        = real(par(3)) ! (deg C)
    myinterface%params_calib%soilm_thetastar    = real(par(4))
    myinterface%params_calib%soilm_betao        = real(par(5))
    myinterface%params_calib%beta_unitcostratio = real(par(6)) ! (-)
    myinterface%params_calib%rd_to_vcmax        = real(par(7)) ! (-/-), ratio of rd to Vcmax
    myinterface%params_calib%tau_acclim         = real(par(8))
    myinterface%params_calib%kc_jmax            = real(par(9)) ! (-), jmax cost ratio, a.k.a c*
    
    !----------------------------------------------------------------
    ! GET VEGETATION COVER (fractional projective cover by PFT)
    !----------------------------------------------------------------
    myinterface%fpc_grid(:) = get_fpc_grid( myinterface%params_siml )
    
    do yr=1,myinterface%params_siml%steering_input%runyears

      !----------------------------------------------------------------
      ! Define simulations "steering" variables (forcingyear, etc.)
      !----------------------------------------------------------------
      myinterface%steering_state = get_steering( yr, myinterface%params_siml%steering_input )

      !----------------------------------------------------------------
      ! Get external (environmental) forcing
      !----------------------------------------------------------------
      ! Get climate variables for this year (full fields and 365 daily values for each variable)
      myinterface%climate(:) = getclimate(nt, &
                                          forcing, &
                                          myinterface%steering_state%climateyear_idx, &
                                          myinterface%params_siml%in_ppfd,  &
                                          myinterface%params_siml%in_netrad &
                                          )

      ! Get annual, gobally uniform CO2
      myinterface%pco2 = getco2(  nt, &
                                  forcing, &
                                  myinterface%steering_state%forcingyear, &
                                  myinterface%params_siml%steering_input%firstyeartrend &
                                  )

      !----------------------------------------------------------------
      ! Get prescribed fAPAR if required (otherwise set to dummy value)
      !----------------------------------------------------------------
      myinterface%vegcover(:) = getfapar( &
                                        nt, &
                                        forcing, &
                                        myinterface%steering_state%forcingyear_idx &
                                        )

      !----------------------------------------------------------------
      ! Call biosphere (wrapper for all modules, contains gridcell loop)
      !----------------------------------------------------------------
      out_biosphere = biosphere_annual() 
      !----------------------------------------------------------------

      !----------------------------------------------------------------
      ! Populate Fortran output array which is passed back to C/R
      !----------------------------------------------------------------
      if (yr > myinterface%params_siml%steering_input%spinupyears ) then

        idx_start = (myinterface%steering_state%forcingyear_idx - 1) * ndayyear + 1
        idx_end   = idx_start + ndayyear - 1

        output(idx_start:idx_end,1)  = dble(out_biosphere%fapar(:))  
        output(idx_start:idx_end,2)  = dble(out_biosphere%gpp(:))    
        output(idx_start:idx_end,3)  = dble(out_biosphere%transp(:)) 
        output(idx_start:idx_end,4)  = dble(out_biosphere%latenth(:))
        output(idx_start:idx_end,5)  = dble(out_biosphere%pet(:))
        output(idx_start:idx_end,6)  = dble(out_biosphere%vcmax(:))  
        output(idx_start:idx_end,7)  = dble(out_biosphere%jmax(:))    
        output(idx_start:idx_end,8)  = dble(out_biosphere%vcmax25(:)) 
        output(idx_start:idx_end,9)  = dble(out_biosphere%jmax25(:))
        output(idx_start:idx_end,10) = dble(out_biosphere%gs_accl(:))
        output(idx_start:idx_end,11) = dble(out_biosphere%wscal(:))
        output(idx_start:idx_end,12) = dble(out_biosphere%chi(:))
        output(idx_start:idx_end,13) = dble(out_biosphere%iwue(:))
        output(idx_start:idx_end,14) = dble(out_biosphere%rd(:))
        output(idx_start:idx_end,15) = dble(out_biosphere%tsoil(:))
        output(idx_start:idx_end,16) = dble(out_biosphere%netrad(:))
        output(idx_start:idx_end,17) = dble(out_biosphere%wcont(:))
        output(idx_start:idx_end,18) = dble(out_biosphere%snow(:))
        output(idx_start:idx_end,19) = dble(out_biosphere%cond(:))
        output(idx_start:idx_end,20) = dble(out_biosphere%cleaf(:))
        output(idx_start:idx_end,21) = dble(out_biosphere%cleaf13(:))

      end if

    enddo

  end subroutine pmodel_f

end module pmodel_mod
