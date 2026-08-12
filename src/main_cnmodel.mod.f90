module cnmodel_mod
  ! Main CN-model routine handling I/O with C and R.
  use, intrinsic :: iso_c_binding, only: c_double, c_int, c_bool

  implicit none

  private
  public :: cnmodel_f

contains

  ! simulation's forcing as time series
  !----------------------------------------------------------------
  subroutine cnmodel_f(        &
    c_only,                    &
    write_soil_diagnostics,    &
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
    ) bind(C, name = "cnmodel_f_")

    use md_params_siml_cnmodel, only: getsteering
    use md_forcing_cnmodel, only: getclimate, getco2, getfapar, getlanduse, get_fpc_grid
    use md_interface_cnmodel, only: interfacetype_biosphere, outtype_biosphere, myinterface
    use md_params_core, only: nlayers_soil, ndayyear, npft
    use md_biosphere_cnmodel, only: biosphere_annual

    implicit none

    ! arguments
    logical(kind=c_bool), intent(in) :: c_only
    logical(kind=c_bool), intent(in) :: write_soil_diagnostics
    logical(kind=c_bool), intent(in) :: spinup
    integer(kind=c_int),  intent(in) :: spinupyears
    integer(kind=c_int),  intent(in) :: recycle
    integer(kind=c_int),  intent(in) :: firstyeartrend
    integer(kind=c_int),  intent(in) :: nyeartrend
    integer(kind=c_int),  intent(in) :: secs_per_tstep
    logical(kind=c_bool), intent(in) :: in_ppfd
    logical(kind=c_bool), intent(in) :: in_netrad
    integer(kind=c_int),  intent(in) :: outdt
    logical(kind=c_bool), intent(in) :: ltre
    logical(kind=c_bool), intent(in) :: ltne
    logical(kind=c_bool), intent(in) :: ltrd
    logical(kind=c_bool), intent(in) :: ltnd
    logical(kind=c_bool), intent(in) :: lgr3
    logical(kind=c_bool), intent(in) :: lgn3
    logical(kind=c_bool), intent(in) :: lgr4
    real(kind=c_double),  intent(in) :: longitude
    real(kind=c_double),  intent(in) :: latitude
    real(kind=c_double),  intent(in) :: altitude
    real(kind=c_double),  intent(in) :: whc
    integer(kind=c_int),  intent(in) :: nt ! number of time steps
    real(kind=c_double),  dimension(86), intent(in) :: par  ! Model parameters
    real(kind=c_double),  dimension(nt,18), intent(in) :: forcing  ! temp = 1, rain = 2, vpd = 3, ppfd = 4, netrad = 5, fsun = 6, snow = 7, co2 = 8, fapar = 9, patm = 10, tmin = 11, tmax = 12, fharv = 13, dno3 = 14, dnh4 = 15, cseed = 16, nseed = 17, lai = 18
    real(kind=c_double),  dimension(nt,70), intent(out) :: output

    ! local variables
    type(outtype_biosphere), dimension(ndayyear) :: out_biosphere  ! holds all the output used for calculating the cost or maximum likelihood function
    integer :: npft_local, yr, idx_start, idx_end

    !----------------------------------------------------------------
    ! GET SIMULATION PARAMETERS
    !----------------------------------------------------------------
    myinterface%params_siml%c_only         = c_only
    myinterface%params_siml%write_soil_diagnostics = write_soil_diagnostics
    myinterface%params_siml%do_spinup      = spinup
    myinterface%params_siml%spinupyears    = spinupyears
    myinterface%params_siml%recycle        = recycle
    myinterface%params_siml%firstyeartrend = firstyeartrend
    myinterface%params_siml%nyeartrend     = nyeartrend

    if (myinterface%params_siml%do_spinup) then
      myinterface%params_siml%runyears = myinterface%params_siml%nyeartrend + myinterface%params_siml%spinupyears
    else
      myinterface%params_siml%runyears = myinterface%params_siml%nyeartrend
      myinterface%params_siml%spinupyears = 0
    endif
    
    myinterface%params_siml%in_ppfd            = in_ppfd
    myinterface%params_siml%in_netrad          = in_netrad
    myinterface%params_siml%outdt              = outdt
    myinterface%params_siml%ltre               = ltre
    myinterface%params_siml%ltne               = ltne
    myinterface%params_siml%ltrd               = ltrd
    myinterface%params_siml%ltnd               = ltnd
    myinterface%params_siml%lgr3               = lgr3
    myinterface%params_siml%lgn3               = lgn3
    myinterface%params_siml%lgr4               = lgr4
    myinterface%params_siml%secs_per_tstep     = secs_per_tstep

    ! Count PFTs to be simulated
    npft_local = 0
    if (myinterface%params_siml%ltre) npft_local = npft_local + 1
    if (myinterface%params_siml%ltne) npft_local = npft_local + 1
    if (myinterface%params_siml%ltrd) npft_local = npft_local + 1
    if (myinterface%params_siml%ltnd) npft_local = npft_local + 1
    if (myinterface%params_siml%lgr3) npft_local = npft_local + 1
    if (myinterface%params_siml%lgr4) npft_local = npft_local + 1
    if (myinterface%params_siml%lgn3) npft_local = npft_local + 1

    ! set parameter to define that this is not a calibration run (otherwise sofun.f90 would not have been compiled, but sofun_simsuite.f90)
    myinterface%params_siml%is_calib = .true.  ! treat paramters passed through R/C-interface the same way as calibratable parameters

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
    ! GET CALIBRATABLE MODEL PARAMETERS
    ! (not necessarily all to actually be calibrated)
    !----------------------------------------------------------------
    myinterface%params_calib%kphio                 = real(par(1))
    myinterface%params_calib%kphio_par_a           = real(par(2))
    myinterface%params_calib%kphio_par_b           = real(par(3))
    myinterface%params_calib%soilm_thetastar       = real(par(4))
    myinterface%params_calib%soilm_betao           = real(par(5))
    myinterface%params_calib%beta_unitcostratio    = real(par(6))
    myinterface%params_calib%rd_to_vcmax           = real(par(7))
    myinterface%params_calib%tau_acclim            = real(par(8))
    myinterface%params_calib%kc_jmax               = real(par(9))
    myinterface%params_calib%f_nretain             = real(par(10))
    myinterface%params_calib%fpc_tree_max          = real(par(11))
    myinterface%params_calib%growtheff             = real(par(12))
    myinterface%params_calib%r_root                = real(par(13))
    myinterface%params_calib%r_sapw                = real(par(14))
    myinterface%params_calib%exurate               = real(par(15))
    myinterface%params_calib%cton_soil             = real(par(16))
    myinterface%params_calib%k_decay_leaf          = real(par(17))
    myinterface%params_calib%r_cton_seed           = real(par(18))
    myinterface%params_calib%k_decay_root          = real(par(19))
    myinterface%params_calib%k_decay_labl          = real(par(20))
    myinterface%params_calib%k_decay_sapw          = real(par(21))
    myinterface%params_calib%r_cton_root           = real(par(22))
    myinterface%params_calib%r_cton_wood           = real(par(23))
    myinterface%params_calib%ncw_min               = real(par(24))
    myinterface%params_calib%r_n_cw_v              = real(par(25))
    myinterface%params_calib%r_ctostructn_leaf     = real(par(26))
    myinterface%params_calib%kbeer                 = real(par(27))
    myinterface%params_calib%gddbase               = real(par(28))
    myinterface%params_calib%ramp                  = real(par(29))
    myinterface%params_calib%phentype              = real(par(30))
    myinterface%params_calib%perc_k1               = real(par(31))
    myinterface%params_calib%thdiff_wp             = real(par(32))
    myinterface%params_calib%thdiff_whc15          = real(par(33))
    myinterface%params_calib%thdiff_fc             = real(par(34))
    myinterface%params_calib%forg                  = real(par(35))
    myinterface%params_calib%wbwp                  = real(par(36))
    myinterface%params_calib%por                   = real(par(37))
    myinterface%params_calib%fsand                 = real(par(38))
    myinterface%params_calib%fclay                 = real(par(39))
    myinterface%params_calib%fsilt                 = real(par(40))
    myinterface%params_calib%kA                    = real(par(41))
    myinterface%params_calib%kalb_sw               = real(par(42))
    myinterface%params_calib%kalb_vis              = real(par(43))
    myinterface%params_calib%kb                    = real(par(44))
    myinterface%params_calib%kc                    = real(par(45))
    myinterface%params_calib%kCw                   = real(par(46))
    myinterface%params_calib%kd                    = real(par(47))
    myinterface%params_calib%ke                    = real(par(48))
    myinterface%params_calib%keps                  = real(par(49))
    myinterface%params_calib%kWm                   = real(par(50))
    myinterface%params_calib%kw                    = real(par(51))
    myinterface%params_calib%komega                = real(par(52))
    myinterface%params_calib%maxmeltrate           = real(par(53))
    myinterface%params_calib%klitt_af10            = real(par(54))
    myinterface%params_calib%klitt_as10            = real(par(55))
    myinterface%params_calib%klitt_bg10            = real(par(56))
    myinterface%params_calib%kexu10                = real(par(57))
    myinterface%params_calib%ksoil_fs10            = real(par(58))
    myinterface%params_calib%ksoil_sl10            = real(par(59))
    myinterface%params_calib%ntoc_crit1            = real(par(60))
    myinterface%params_calib%ntoc_crit2            = real(par(61))
    myinterface%params_calib%cton_microb           = real(par(62))
    myinterface%params_calib%tmppar                = real(par(63)) ! used for development
    myinterface%params_calib%fastfrac              = real(par(64))
    myinterface%params_calib%eff_nup               = real(par(65))
    myinterface%params_calib%minimumcostfix        = real(par(66))
    myinterface%params_calib%fixoptimum            = real(par(67))
    myinterface%params_calib%a_param_fix           = real(par(68))
    myinterface%params_calib%b_param_fix           = real(par(69))
    myinterface%params_calib%maxnitr               = real(par(70))
    myinterface%params_calib%non                   = real(par(71))
    myinterface%params_calib%n2on                  = real(par(72))
    myinterface%params_calib%kn                    = real(par(73))
    myinterface%params_calib%kdoc                  = real(par(74))
    myinterface%params_calib%docmax                = real(par(75))
    myinterface%params_calib%dnitr2n2o             = real(par(76))
    myinterface%params_calib%frac_leaf             = real(par(77))
    myinterface%params_calib%frac_wood             = real(par(78))
    myinterface%params_calib%frac_avl_labl         = real(par(79))
    myinterface%params_calib%nv_vcmax25            = real(par(80))
    myinterface%params_calib%nuptake_kc            = real(par(81))
    myinterface%params_calib%nuptake_kv            = real(par(82))
    myinterface%params_calib%nuptake_vmax          = real(par(83))
    ! Appended to preserve the positions of the original 83 parameters.
    myinterface%params_calib%dehardening_gdd_midpoint = real(par(84))
    myinterface%params_calib%dehardening_gdd_slope    = real(par(85))
    myinterface%params_calib%dehardening_gdd_base     = real(par(86))

    !----------------------------------------------------------------
    ! GET VEGETATION COVER (fractional projective cover by PFT)
    !----------------------------------------------------------------
    myinterface%fpc_grid(:) = get_fpc_grid( myinterface%params_siml )
    
    yearloop: do yr=1,myinterface%params_siml%runyears

      !----------------------------------------------------------------
      ! Define simulations "steering" variables (forcingyear, etc.)
      !----------------------------------------------------------------
      myinterface%steering = getsteering( yr, myinterface%params_siml )

      !----------------------------------------------------------------
      ! Get external (environmental) forcing
      !----------------------------------------------------------------
      ! Get climate variables for this year (full fields and 365 daily values for each variable)
      myinterface%climate(:) = getclimate(nt, &
                                          forcing, &
                                          myinterface%steering%climateyear_idx, &
                                          myinterface%params_siml%in_ppfd,  &
                                          myinterface%params_siml%in_netrad, &
                                          myinterface%grid%elv &
                                          )

      ! Get annual, gobally uniform CO2
      myinterface%pco2 = getco2(nt, &
                                forcing, &
                                myinterface%steering%forcingyear, &
                                myinterface%params_siml%firstyeartrend &
                                )

      ! Get land use forcing (dates of harvesting, fertilisation, etc.)
      myinterface%landuse(:) = getlanduse(nt, &
                                          forcing, &
                                          myinterface%steering%forcingyear, &
                                          myinterface%params_siml%firstyeartrend &
                                          )

      !----------------------------------------------------------------
      ! Call biosphere (wrapper for all modules, contains gridcell loop)
      !----------------------------------------------------------------
      out_biosphere = biosphere_annual() 
      !----------------------------------------------------------------

      !----------------------------------------------------------------
      ! Populate Fortran output array which is passed back to C/R
      !----------------------------------------------------------------
      if (yr > myinterface%params_siml%spinupyears ) then

        idx_start = (myinterface%steering%forcingyear_idx - 1) * ndayyear + 1
        idx_end   = idx_start + ndayyear - 1

        output(idx_start:idx_end,1)  = dble(out_biosphere(:)%fapar)  
        output(idx_start:idx_end,2)  = dble(out_biosphere(:)%gpp)    
        output(idx_start:idx_end,3)  = dble(out_biosphere(:)%transp) 
        output(idx_start:idx_end,4)  = dble(out_biosphere(:)%latenth)
        output(idx_start:idx_end,5)  = dble(out_biosphere(:)%pet)
        output(idx_start:idx_end,6)  = dble(out_biosphere(:)%vcmax)  
        output(idx_start:idx_end,7)  = dble(out_biosphere(:)%jmax)    
        output(idx_start:idx_end,8)  = dble(out_biosphere(:)%vcmax25) 
        output(idx_start:idx_end,9)  = dble(out_biosphere(:)%jmax25)
        output(idx_start:idx_end,10) = dble(out_biosphere(:)%gs_accl)
        output(idx_start:idx_end,11) = dble(out_biosphere(:)%wscal)
        output(idx_start:idx_end,12) = dble(out_biosphere(:)%chi)
        output(idx_start:idx_end,13) = dble(out_biosphere(:)%iwue)
        output(idx_start:idx_end,14) = dble(out_biosphere(:)%tsoil   )
        output(idx_start:idx_end,15) = dble(out_biosphere(:)%lai     )
        output(idx_start:idx_end,16) = dble(out_biosphere(:)%cleaf   )
        output(idx_start:idx_end,17) = dble(out_biosphere(:)%nleaf   )
        output(idx_start:idx_end,18) = dble(out_biosphere(:)%croot   )
        output(idx_start:idx_end,19) = dble(out_biosphere(:)%nroot   )
        output(idx_start:idx_end,20) = dble(out_biosphere(:)%clabl   )
        output(idx_start:idx_end,21) = dble(out_biosphere(:)%nlabl   )
        output(idx_start:idx_end,22) = dble(out_biosphere(:)%ninorg  )
        output(idx_start:idx_end,23) = dble(out_biosphere(:)%pnh4    )
        output(idx_start:idx_end,24) = dble(out_biosphere(:)%pno3    )
        output(idx_start:idx_end,25) = dble(out_biosphere(:)%dnleach )
        output(idx_start:idx_end,26) = dble(out_biosphere(:)%dn2o    )
        output(idx_start:idx_end,27) = dble(out_biosphere(:)%npp     )
        output(idx_start:idx_end,28) = dble(out_biosphere(:)%csoil )
        output(idx_start:idx_end,29) = dble(out_biosphere(:)%nsoil )
        output(idx_start:idx_end,30) = dble(out_biosphere(:)%clitt )
        output(idx_start:idx_end,31) = dble(out_biosphere(:)%nlitt )
        output(idx_start:idx_end,32) = dble(out_biosphere(:)%nfix )
        output(idx_start:idx_end,33) = dble(out_biosphere(:)%nup )
        output(idx_start:idx_end,34) = dble(out_biosphere(:)%cex )
        output(idx_start:idx_end,35) = dble(out_biosphere(:)%netmin )
        output(idx_start:idx_end,36) = dble(out_biosphere(:)%dcharv )
        output(idx_start:idx_end,37) = dble(out_biosphere(:)%dnharv )
        output(idx_start:idx_end,38) = dble(out_biosphere(:)%drd )
        output(idx_start:idx_end,39) = dble(out_biosphere(:)%lma )
        output(idx_start:idx_end,40) = dble(out_biosphere(:)%narea )
        output(idx_start:idx_end,41) = dble(out_biosphere(:)%narea_v )
        output(idx_start:idx_end,42) = dble(out_biosphere(:)%nloss )
        output(idx_start:idx_end,43) = dble(out_biosphere(:)%seedc )
        output(idx_start:idx_end,44) = dble(out_biosphere(:)%seedn )
        output(idx_start:idx_end,45) = dble(out_biosphere(:)%npp_leaf )
        output(idx_start:idx_end,46) = dble(out_biosphere(:)%npp_root )
        output(idx_start:idx_end,47) = dble(out_biosphere(:)%npp_wood )
        output(idx_start:idx_end,48) = dble(out_biosphere(:)%asat )
        output(idx_start:idx_end,49) = dble(out_biosphere(:)%cwood )
        output(idx_start:idx_end,50) = dble(out_biosphere(:)%nwood )
        output(idx_start:idx_end,51) = dble(out_biosphere(:)%rleaf )
        output(idx_start:idx_end,52) = dble(out_biosphere(:)%rwood )
        output(idx_start:idx_end,53) = dble(out_biosphere(:)%rroot )
        output(idx_start:idx_end,54) = dble(out_biosphere(:)%rcex )
        output(idx_start:idx_end,55) = dble(out_biosphere(:)%rhet )
        output(idx_start:idx_end,56) = dble(out_biosphere(:)%cresv )
        output(idx_start:idx_end,57) = dble(out_biosphere(:)%nresv )
        output(idx_start:idx_end,58) = dble(out_biosphere(:)%rgrow )
        output(idx_start:idx_end,59) = dble(out_biosphere(:)%npp_seed )
        output(idx_start:idx_end,60) = dble(out_biosphere(:)%dclabl )
        output(idx_start:idx_end,61) = dble(out_biosphere(:)%dnlabl )
        output(idx_start:idx_end,62) = dble(out_biosphere(:)%dnleaf )
        output(idx_start:idx_end,63) = dble(out_biosphere(:)%dnroot )
        output(idx_start:idx_end,64) = dble(out_biosphere(:)%dnwood )
        output(idx_start:idx_end,65) = dble(out_biosphere(:)%dnseed )
        output(idx_start:idx_end,66) = dble(out_biosphere(:)%nresorb )
        output(idx_start:idx_end,67) = dble(out_biosphere(:)%x1 )
        output(idx_start:idx_end,68) = dble(out_biosphere(:)%x2 )
        output(idx_start:idx_end,69) = dble(out_biosphere(:)%x3 )
        output(idx_start:idx_end,70) = dble(out_biosphere(:)%x4 )

      end if

    enddo yearloop

  end subroutine cnmodel_f

end module cnmodel_mod
