module md_gpp_lm3ppa
  !//////////////////////////////////////////////////////////////////////
  ! GPP MODULE
  ! Uses LM3-PPA structure to call the gs_leuning() photosynthesis routine
  !------------------------------------------------------------------------
  use datatypes
  use md_soil_lm3ppa, only: water_supply_layer
  use md_interface_lm3ppa, only: myinterface

  implicit none

  private
  public gpp, getpar_modl_gpp

  !-----------------------------------------------------------------------
  ! P-model parameters created here for pmodel option. takes no effect in gs_leuning option
  !-----------------------------------------------------------------------
  type paramstype_gpp
    real :: beta         ! Unit cost of carboxylation (dimensionless)
    real :: soilm_par_a
    real :: soilm_par_b
    real :: rd_to_vcmax  ! Ratio of Rdark to Vcmax25, number from Atkin et al., 2015 for C3 herbaceous
    real :: tau_acclim   ! acclimation time scale of photosynthesis (d)
    real :: tau_acclim_tempstress
    real :: par_shape_tempstress
  end type paramstype_gpp

  ! ! PFT-DEPENDENT PARAMETERS
  ! type pftparamstype_gpp
  !   real :: kphio        ! quantum efficiency (Long et al., 1993)  
  ! end type pftparamstype_gpp

  type(paramstype_gpp) :: params_gpp
  ! type(pftparamstype_gpp), dimension(npft) :: params_pft_gpp

contains

  subroutine gpp( forcing, vegn, init )
    !//////////////////////////////////////////////////////////////////////
    ! GPP
    ! Calculates light availability and photosynthesis for each cohort 
    ! and sets the following cohort-level variables:
    ! - An_op   
    ! - An_cl   
    ! - w_scale 
    ! - transp  
    ! - gpp     
    !
    ! Subroutines from BiomeE-Allocation
    !------------------------------------------------------------------------
    use md_forcing_lm3ppa, only: climate_type
    use md_photosynth, only: pmodel, zero_pmodel, outtype_pmodel, calc_ftemp_inst_rd
    use md_photosynth, only: calc_ftemp_kphio_tmin, calc_ftemp_kphio, calc_soilmstress
    use md_params_core, only: kTkelvin, kfFEC
    use md_sofunutils, only: dampen_variability

    type(climate_type), intent(in):: forcing
    type(vegn_tile_type), intent(inout) :: vegn
    logical, intent(in) :: init   ! is true on the very first simulation day (first subroutine call of each gridcell)

    ! local variables used for BiomeE-Allocation part
    type(cohort_type), pointer :: cc
    integer, parameter :: nlayers_max = 10                 ! maximum number of canopy layers to be considered 
    real   :: rad_top                                      ! downward radiation at the top of the canopy, W/m2
    real   :: rad_net                                      ! net radiation absorbed by the canopy, W/m2
    real   :: Tair, TairK                                  ! air temperature, degC and degK
    real   :: cana_q                                       ! specific humidity in canopy air space, kg/kg
    real   :: cana_co2                                     ! co2 concentration in canopy air space, mol CO2/mol dry air
    real   :: p_surf                                       ! surface pressure, Pa
    real   :: water_supply                                 ! water supply per m2 of leaves
    real   :: fw, fs                                       ! wet and snow-covered fraction of leaves
    real   :: psyn                                         ! net photosynthesis, mol C/(m2 of leaves s)
    real   :: resp                                         ! leaf respiration, mol C/(m2 of leaves s)
    real   :: w_scale2, transp                             ! mol H20 per m2 of leaf per second
    real   :: kappa                                        ! light extinction coefficient of crown layers
    real   :: f_light(nlayers_max+1)                       ! incident light fraction at top of a given layer
    ! real   :: f_apar(nlayers_max)                          ! absorbed light fraction at top of a given layer
    real   :: LAIlayer(nlayers_max)                        ! leaf area index per layer, corrected for gaps (representative for the tree-covered fraction)
    ! real   :: cum_lai_layer(nlayers_max+1)                 ! cumulative leaf area index of layers above a given layer
    real   :: crownarea_layer(nlayers_max)                 ! additional GPP for lower layer cohorts due to gaps
    real   :: accuCAI, f_gap
    real   :: myrd, mygpp                                  ! just for temporary use
    real, dimension(vegn%n_cohorts) :: fapar_tree          ! tree-level fAPAR based on LAI within the crown
    real, dimension(nlayers_max) :: par_layer              ! incident photosynthetically active radiation at the top of each layer, attenuated by layers above (W m-2)
    real, dimension(nlayers_max) :: apar_layer             ! absorbed photosynthetically active radiation per layer (W m-2)
    integer:: i, layer=0, layer_old

    ! local variables used for P-model part
    real :: tk, ftemp_kphio
    real, save :: co2_memory
    real, save :: vpd_memory
    real, save :: temp_memory
    real, save :: patm_memory
    type(outtype_pmodel) :: out_pmodel      ! list of P-model output variables

    ! for debugging
    real :: tmp
    logical :: stoplater = .false.

    if (trim(myinterface%params_siml%method_photosynth) == "gs_leuning") then   !XXXXX
      !===========================================================
      ! Original BiomeE-Allocation
      !-----------------------------------------------------------
      ! Water supply for photosynthesis, Layers
      call water_supply_layer(vegn)

      ! Sum leaf area over cohorts in each crown layer -> LAIlayer(layer)
      f_gap = 0.1 ! 0.1
      accuCAI = 0.0
      LAIlayer = 0.0
      do i = 1, vegn%n_cohorts
        cc => vegn%cohorts(i)
        layer = Max(1, Min(cc%layer,9))
        LAIlayer(layer) = LAIlayer(layer) + cc%leafarea * cc%nindivs /(1.-f_gap)
      enddo

      ! ! Calculate kappa according to sun zenith angle 
      ! kappa = cc%extinct/max(cosz,0.01)
      
      ! Use constant light extinction coefficient
      kappa = cc%extinct

      ! Get light received at each crown layer as a fraction of top-of-canopy -> f_light(layer) 
      f_light(:) = 0.0
      f_light(1) = 1.0
      do i=2,layer
        f_light(i) = f_light(i-1) * (exp(0.0 - kappa * LAIlayer(i-1)) + f_gap)
      enddo

      ! Photosynthesis
      accuCAI = 0.0

      do i = 1, vegn%n_cohorts

        cc => vegn%cohorts(i)
        associate ( sp => spdata(cc%species) )

        if (cc%status == LEAF_ON .and. cc%lai > 0.1) then

          ! Convert forcing data
          layer    = Max (1, Min(cc%layer,9))
          rad_top  = f_light(layer) * forcing%radiation ! downward radiation at the top of the canopy, W/m2

          !===============================
          ! ORIGINAL
          !===============================
          rad_net  = f_light(layer) * forcing%radiation * 0.9 ! net radiation absorbed by the canopy, W/m2
          p_surf   = forcing%P_air  ! Pa
          TairK    = forcing%Tair ! K
          Tair     = forcing%Tair - 273.16 ! degC
          cana_q   = (esat(Tair) * forcing%RH * mol_h2o) / (p_surf * mol_air)  ! air specific humidity, kg/kg
          cana_co2 = forcing%CO2 ! co2 concentration in canopy air space, mol CO2/mol dry air

          ! recalculate the water supply to mol H20 per m2 of leaf per second
          water_supply = cc%W_supply / (cc%leafarea * myinterface%step_seconds * mol_h2o) ! mol m-2 leafarea s-1

          !call get_vegn_wet_frac (cohort, fw=fw, fs=fs)
          fw = 0.0
          fs = 0.0

          call gs_leuning(rad_top, rad_net, TairK, cana_q, cc%lai, &
            p_surf, water_supply, cc%species, sp%pt, &
            cana_co2, cc%extinct, fs+fw, &
            psyn, resp, w_scale2, transp )

          !===============================
          ! XXX Experiment: increasing net photosynthesis 15% and 30%
          !===============================
          ! if (myinterface%steering%year>myinterface%params_siml%spinupyears) then
          !   psyn = psyn * 1.30
          !   resp = resp * 1.30
          ! endif

          ! store the calculated photosynthesis, photorespiration, and transpiration for future use in growth
          cc%An_op   = psyn  ! molC s-1 m-2 of leaves ! net photosynthesis, mol C/(m2 of leaves s)
          cc%An_cl   = -resp  ! molC s-1 m-2 of leaves
          cc%w_scale = w_scale2
          cc%transp  = transp * mol_h2o * cc%leafarea * myinterface%step_seconds ! Transpiration (kgH2O/(tree step), Weng, 2017-10-16
          cc%resl    = -resp         * mol_C * cc%leafarea * myinterface%step_seconds ! kgC tree-1 step-1
          cc%gpp     = (psyn - resp) * mol_C * cc%leafarea * myinterface%step_seconds ! kgC step-1 tree-1
          ! print *, cc%gpp

          !if (isnan(cc%gpp)) stop '"gpp" is a NaN'

          else

          ! no leaves means no photosynthesis and no stomatal conductance either
            cc%An_op   = 0.0
            cc%An_cl   = 0.0
            cc%gpp     = 0.0
            cc%transp  = 0.0
            cc%w_scale = -9999

          endif
        end associate
      enddo

    else if (trim(myinterface%params_siml%method_photosynth) == "pmodel") then
    
      !===========================================================
      ! P-model
      !-----------------------------------------------------------
      ! Calculate environmental conditions with memory, time scale 
      ! relevant for Rubisco turnover
      !----------------------------------------------------------------
      if (init) then
        co2_memory  = forcing%CO2 * 1.0e6
        temp_memory = (forcing%Tair - kTkelvin)
        vpd_memory  = forcing%vpd
        patm_memory = forcing%P_air
      end if 
      
      co2_memory  = dampen_variability( forcing%CO2 * 1.0e6,        params_gpp%tau_acclim, co2_memory )
      temp_memory = dampen_variability( (forcing%Tair - kTkelvin),  params_gpp%tau_acclim, temp_memory)
      vpd_memory  = dampen_variability( forcing%vpd,                params_gpp%tau_acclim, vpd_memory )
      patm_memory = dampen_variability( forcing%P_air,              params_gpp%tau_acclim, patm_memory )

      tk = forcing%Tair + kTkelvin

      !----------------------------------------------------------------
      ! Instantaneous temperature effect on quantum yield efficiency
      !----------------------------------------------------------------
      ftemp_kphio = calc_ftemp_kphio( (forcing%Tair - kTkelvin), .false. )  ! no C4

      !----------------------------------------------------------------

      ! Light absorption
      !----------------------------------------------------------------
      apar_layer(:) = 0.0
      par_layer(1) = forcing%PAR * 1.0e-6
      layer_old = 1

      do i = 1, vegn%n_cohorts

        cc => vegn%cohorts(i)
        associate ( sp => spdata(cc%species) )

        ! cohort-dependent light extinction coefficient
        kappa = cc%extinct

        ! check if we're one layer deeper. if so, update light level based on absorbed light by layer above
        layer = max(1, min(cc%layer, 9))
        if (layer /= layer_old) then
          ! print*,'new layer: ', layer
          par_layer(layer) = par_layer(layer - 1) - apar_layer(layer - 1)
          tmp = par_layer(layer)/(forcing%PAR * 1.0e-6)
          if (tmp < 0.9) then
            ! print*,'light attenuated to fracion: ', tmp
            ! stoplater = .true.
          end if
        end if
        layer_old = layer

        !print*,'cc%status == LEAF_ON, cc%lai, temp_memory', cc%status == LEAF_ON, cc%lai, temp_memory      

        !----------------------------------------------------------------
        ! Get light absorbed by cohort, considering LAI within the crown
        !----------------------------------------------------------------
        ! tree-level fAPAR
        if (cc%status == LEAF_ON) then
          fapar_tree(i) = 1.0 - exp(-kappa * cc%leafarea / cc%crownarea)   ! at individual-level: cc%leafarea represents leaf area index within the crown
          ! print*,'fapar_tree, kappa, leafarea, crownarea ', fapar_tree(i), kappa, cc%leafarea, cc%crownarea
        else
          fapar_tree(i) = 0.0
        end if

        ! total absorbed light per layer, integrating over crownarea
        apar_layer(layer) = apar_layer(layer) + par_layer(layer) * fapar_tree(i) * cc%crownarea * cc%nindivs

        ! print*,'coh, lay, par, fapar, aca', i, layer, par_layer(layer), fapar_tree(i), cc%crownarea * cc%nindivs

        end associate

      end do

      ! !----------------------------------------------------------------
      ! ! Average LAI per unit ground area (excluding f_gap) by layer
      ! !----------------------------------------------------------------
      ! accuCAI = 0.0
      ! LAIlayer(:) = 0.0
      ! crownarea_layer(:) = 0.0
      ! do i = 1, vegn%n_cohorts
      !   cc => vegn%cohorts(i)
      !   layer = max(1, min(cc%layer, 9))
      !   LAIlayer(layer) = LAIlayer(layer) + cc%leafarea * cc%nindivs
      !   crownarea_layer(layer) = crownarea_layer(layer) + cc%crownarea * cc%nindivs
      ! end do

      ! !----------------------------------------------------------------
      ! ! Fraction of light received at top of each layer
      ! !----------------------------------------------------------------      
      ! ! Use constant light extinction coefficient
      ! kappa = cc%extinct
      ! f_light(:) = 0.0
      ! f_light(1) = 1.0
      ! do layer = 2, (nlayers_max + 1)
      !   f_light(layer) = f_light(layer - 1) * exp(- kappa * LAIlayer(layer - 1))
      ! end do


      ! ! print*,'LAIlayer(:)      ', LAIlayer(:)
      ! ! print*,'f_light(:)       ', f_light(:)      
      ! ! print*,'------------------------------'
      ! ! if (stoplater) stop 'beni'


      !----------------------------------------------------------------
      ! Photosynthesis for each cohort
      !----------------------------------------------------------------
      accuCAI = 0.0


      cohortsloop: do i = 1, vegn%n_cohorts

        cc => vegn%cohorts(i)
        associate ( sp => spdata(cc%species) )


        if (cc%status == LEAF_ON .and. temp_memory > -5.0) then

          !----------------------------------------------------------------
          ! P-model call for C3 plants to get a list of variables that are 
          ! acclimated to slowly varying conditions
          !----------------------------------------------------------------
          if (fapar_tree(i) > 0.0 .and. forcing%PAR > 0.0) then


            out_pmodel = pmodel(  &
                                  kphio          = sp%kphio, &
                                  beta           = params_gpp%beta, &
                                  ppfd           = par_layer(layer), &    ! required in mol m-2 s-1, unit ground area
                                  co2            = co2_memory, &
                                  tc             = temp_memory, &
                                  vpd            = vpd_memory, &
                                  patm           = patm_memory, &
                                  c4             = .false., &
                                  method_optci   = "prentice14", &
                                  method_jmaxlim = "wang17" &
                                  )

            ! irrelevant variables for this setup  
            cc%An_op   = 0.0
            cc%An_cl   = 0.0
            cc%transp  = 0.0
            cc%w_scale = -9999

            ! quantities per unit ground area
            mygpp = fapar_tree(i) * out_pmodel%lue * par_layer(layer)                                                              ! g s-1 m-2
            myrd  = fapar_tree(i) * out_pmodel%vcmax25 * params_gpp%rd_to_vcmax * calc_ftemp_inst

            ! converting to quantities per tree and cumulated over seconds in time step
            cc%resl = myrd  * cc%crownarea * myinterface%step_seconds * mol_C    ! kgC step-1 tree-1 
            cc%gpp  = mygpp * cc%crownarea * myinterface%step_seconds * 1.0e-3   ! kgC step-1 tree-1

          else

            cc%An_op   = 0.0
            cc%An_cl   = 0.0
            cc%transp  = 0.0
            cc%w_scale = -9999
            cc%resl    = 0.0
            cc%gpp     = 0.0

          end if

        else

          ! no leaves means no photosynthesis and no stomatal conductance either
          cc%An_op   = 0.0
          cc%An_cl   = 0.0
          cc%transp  = 0.0
          cc%w_scale = -9999
          cc%resl    = 0.0
          cc%gpp     = 0.0

        endif

        end associate

      end do cohortsloop

      ! print*,'crownarea_layer ', crownarea_layer(:)
      ! print*,'apar      ', apar_layer(:)
      ! print*,'diff light', forcing%PAR * 1.0e-6 * (f_light(1:nlayers_max) - f_light(2:nlayers_max+1))

    else

      !print*,'WARNING: gpp(): myinterface%params_siml%method_photosynth not recognized'

    end if

  end subroutine gpp


  subroutine gs_leuning( rad_top, rad_net, tl, ea, lai, &
    p_surf, ws, pft, pt, ca, kappa, leaf_wet, &
    apot, acl,w_scale2, transp )

    real,    intent(in)    :: rad_top ! PAR dn on top of the canopy, w/m2
    real,    intent(in)    :: rad_net ! PAR net on top of the canopy, w/m2
    real,    intent(in)    :: tl   ! leaf temperature, degK
    real,    intent(in)    :: ea   ! specific humidity in the canopy air, kg/kg
    real,    intent(in)    :: lai  ! leaf area index
    !real,    intent(in)    :: leaf_age ! age of leaf since budburst (deciduos), days
    real,    intent(in)    :: p_surf ! surface pressure, Pa
    real,    intent(in)    :: ws   ! water supply, mol H20/(m2 of leaf s)
    integer, intent(in)    :: pft  ! species
    integer, intent(in)    :: pt   ! physiology type (C3 or C4)
    real,    intent(in)    :: ca   ! concentartion of CO2 in the canopy air space, mol CO2/mol dry air
    real,    intent(in)    :: kappa ! canopy extinction coefficient (move inside f(pft))
    real,    intent(in)    :: leaf_wet ! fraction of leaf that's wet or snow-covered
    ! integer, intent(in)    :: layer  ! the layer of this canopy
    ! note that the output is per area of leaf; to get the quantities per area of
    ! land, multiply them by LAI
    !real,    intent(out)   :: gs   ! stomatal conductance, m/s
    real,    intent(out)   :: apot ! net photosynthesis, mol C/(m2 s)
    real,    intent(out)   :: acl  ! leaf respiration, mol C/(m2 s)
    real,    intent(out)   :: w_scale2,transp  ! transpiration, mol H20/(m2 of leaf s)
    ! local variables     
    ! photosynthesis
    real :: vm
    real :: kc, ko ! Michaelis-Menten constants for CO2 and O2, respectively
    real :: ci
    real :: capgam ! CO2 compensation point
    real :: f2, f3
    real :: coef0, coef1
    real :: Resp
    ! conductance related
    real :: gs
    real :: b
    real :: ds  ! humidity deficit, kg/kg
    real :: hl  ! saturated specific humidity at the leaf temperature, kg/kg
    real :: do1
    ! misceleneous
    real :: dum2
    real, parameter :: light_crit = 0
    real, parameter :: gs_lim = 0.25
    real, parameter :: Rgas = 8.314 ! J mol-1 K-1, universal gas constant
    ! new average computations
    real :: lai_eq
    real, parameter :: rad_phot = 0.0000046 ! PAR conversion factor of J -> mol of quanta 
    real :: light_top
    real :: par_net
    real :: Ag
    real :: An
    real :: Ag_l
    real :: Ag_rb
    real :: anbar
    real :: gsbar
    real :: w_scale
    real, parameter :: p_sea = 1.0e5 ! sea level pressure, Pa
    ! soil water stress
    real :: Ed, an_w, gs_w
    b = 0.01
    do1 = 0.09 ! kg/kg
    if (pft < 2) do1 = 0.15
    ! Convert Solar influx from W/(m^2s) to mol_of_quanta/(m^2s) PAR,
    ! empirical relationship from McCree is light=rn*0.0000046
    light_top = rad_top*rad_phot;
    par_net   = rad_net*rad_phot;
    ! calculate humidity deficit, kg/kg
    call qscomp(tl, p_surf, hl)
    ds = max(hl - ea,0.0)
    !  ko=0.25   *exp(1400.0*(1.0/288.2-1.0/tl))*p_sea/p_surf;
    !  kc=0.00015*exp(6000.0*(1.0/288.2-1.0/tl))*p_sea/p_surf;
    !  vm=spdata(pft)%Vmax*exp(3000.0*(1.0/288.2-1.0/tl));
    ! corrected by Weng, 2013-01-17
    ! Weng, 2013-01-10
    ko=0.248    * exp(35948/Rgas*(1.0/298.2-1.0/tl))*p_sea/p_surf ! Weng, 2013-01-10
    kc=0.000404 * exp(59356/Rgas*(1.0/298.2-1.0/tl))*p_sea/p_surf ! Weng, 2013-01-10
    vm=spdata(pft)%Vmax*exp(24920/Rgas*(1.0/298.2-1.0/tl)) ! / ((layer-1)*1.0+1.0) ! Ea = 33920
    !decrease Vmax due to aging of temperate deciduous leaves 
    !(based on Wilson, Baldocchi and Hanson (2001)."Plant,Cell, and Environment", vol 24, 571-583)
    !! Turned off by Weng, 2013-02-01, since we can't trace new leaves
    !  if (spdata(pft)%leaf_age_tau>0 .and. leaf_age>spdata(pft)%leaf_age_onset) then
    !     vm=vm*exp(-(leaf_age-spdata(pft)%leaf_age_onset)/spdata(pft)%leaf_age_tau)
    !  endif

    ! capgam=0.209/(9000.0*exp(-5000.0*(1.0/288.2-1.0/tl))); - Foley formulation, 1986
    capgam=0.5*kc/ko*0.21*0.209; ! Farquhar & Caemmerer 1982

    ! Find respiration for the whole canopy layer

    !  Resp=spdata(pft)%gamma_resp*vm*lai /((layer-1)*1.0+1.0)  ! Weng, 2013-01-17 add '/ ((layer-1)*1.0+1.0)'

    ! 2014-09-03, for Nitrogen model: resp = D*(A + B*LMA)
    ! (A+B*LMA) = LNA, D=Vmax/LNA = 25E-6/0.0012 = 0.02 for a standard deciduous species
    !! Leaf resp as a function of nitrogen
    !  Resp=spdata(pft)%gamma_resp*0.04*spdata(pft)%LNA  & ! basal rate, mol m-2 s-1
    !       * exp(24920/Rgas*(1.0/298.2-1.0/tl))         & ! temperature scaled
    !       * lai                                        & ! whole canopy
    !       /((layer-1)*1.0+1.0)                         !
    !! as a function of LMA
    !  Resp=(spdata(pft)%gamma_LNbase*spdata(pft)%LNbase+spdata(pft)%gamma_LMA*spdata(pft)%LMA)  & ! basal rate, mol m-2 s-1
    !  Resp=spdata(pft)%gamma_LNbase*(2.5*spdata(pft)%LNA-1.5*spdata(pft)%LNbase)     & ! basal rate, mol m-2 s-1
    Resp = spdata(pft)%gamma_LN/seconds_per_year & ! per seconds, ! basal rate, mol m-2 s-1
            * spdata(pft)%LNA * lai / mol_c    &     ! whole canopy, ! basal rate, mol m-2 s-1
            * exp(24920/Rgas*(1.0/298.2-1.0/tl))     ! temperature scaled
    !                                  &
    !       /((layer-1)*1.0+1.0)
    ! Temperature effects
    Resp=Resp/((1.0+exp(0.4*(5.0-tl+TFREEZE)))*(1.0+exp(0.4*(tl-45.0-TFREEZE))));


    ! ignore the difference in concentrations of CO2 near
    !  the leaf and in the canopy air, rb=0.
    Ag_l=0.;
    Ag_rb=0.;
    Ag=0.;
    anbar=-Resp/lai;
    gsbar=b;
    ! find the LAI level at which gross photosynthesis rates are equal
    ! only if PAR is positive
    if ( light_top > light_crit ) then

      if (pt==PT_C4) then ! C4 species

        coef0=(1+ds/do1)/spdata(pft)%m_cond
        ci=(ca+1.6*coef0*capgam)/(1+1.6*coef0)

        if (ci>capgam) then
          f2=vm
          f3=18000.0*vm*ci ! 18000 or 1800?
          dum2=min(f2,f3)

          ! find LAI level at which rubisco limited rate is equal to light limited rate
          lai_eq = -log(dum2/(kappa*spdata(pft)%alpha_phot*light_top))/kappa
          lai_eq = min(max(0.0,lai_eq),lai) ! limit lai_eq to physically possible range

          ! gross photosynthesis for light-limited part of the canopy
          Ag_l   = spdata(pft)%alpha_phot * par_net &
                  * (exp(-lai_eq*kappa)-exp(-lai*kappa))/(1-exp(-lai*kappa))

          ! gross photosynthesis for rubisco-limited part of the canopy
          Ag_rb  = dum2*lai_eq

          Ag=(Ag_l+Ag_rb)/((1.0+exp(0.4*(5.0-tl+TFREEZE)))*(1.0+exp(0.4*(tl-45.0-TFREEZE))))
          An=Ag-Resp
          anbar=An/lai

          if (anbar>0.0) then
            gsbar=anbar/(ci-capgam)/coef0
          endif

        endif ! ci>capgam

      else ! C3 species

        coef0=(1+ds/do1)/spdata(pft)%m_cond
        coef1=kc*(1.0+0.209/ko)
        ci=(ca+1.6*coef0*capgam)/(1+1.6*coef0)
        f2=vm*(ci-capgam)/(ci+coef1)
        f3=vm/2.
        dum2=min(f2,f3)

        if (ci>capgam) then
          ! find LAI level at which rubisco limited rate is equal to light limited rate
          lai_eq=-log(dum2*(ci+2.*capgam)/(ci-capgam)/ &
           (spdata(pft)%alpha_phot*light_top*kappa))/kappa
          lai_eq = min(max(0.0,lai_eq),lai) ! limit lai_eq to physically possible range

          ! gross photosynthesis for light-limited part of the canopy
          Ag_l   = spdata(pft)%alpha_phot * (ci-capgam)/(ci+2.*capgam) * par_net &
                   * (exp(-lai_eq*kappa)-exp(-lai*kappa))/(1.0-exp(-lai*kappa))

          ! gross photosynthesis for rubisco-limited part of the canopy
          Ag_rb  = dum2*lai_eq

          Ag=(Ag_l+Ag_rb) /((1.0+exp(0.4*(5.0-tl+TFREEZE)))*(1.0+exp(0.4*(tl-45.0-TFREEZE))));
          An=Ag-Resp;
          anbar=An/lai
          !write(*,*)'An,Ag,AG_l,Ag_rb,lai',An,Ag, Ag_l, Ag_rb,lai

          if (anbar>0.0) then
            gsbar=anbar/(ci-capgam)/coef0;
          endif

        endif

      endif

    endif ! light is available for photosynthesis

    !write(898,'(1(I4,","),10(E10.4,","))') &
    !     layer, light_top, par_net, kappa, lai, lai_eq, ci, capgam, Ag_l, Ag_rb, Ag

    an_w=anbar

    if (an_w > 0.) then
      an_w=an_w*(1-spdata(pft)%wet_leaf_dreg*leaf_wet);
    endif
    gs_w = 1.56 * gsbar *(1-spdata(pft)%wet_leaf_dreg*leaf_wet); !Weng: 1.56 for H2O?

    if (gs_w > gs_lim) then
      if (an_w > 0.) an_w = an_w*gs_lim/gs_w
      gs_w = gs_lim
    endif

    ! find water availability diagnostic demand
    Ed = gs_w * ds * mol_air / mol_h2o ! ds*mol_air/mol_h2o is the humidity deficit in [mol_h2o/mol_air]

    ! the factor mol_air/mol_h2o makes units of gs_w and humidity deficit ds compatible:
    if (Ed>ws) then
      w_scale = ws/Ed
      gs_w = w_scale * gs_w
      if (an_w > 0.0) an_w = an_w * w_scale
      if (an_w < 0.0 .and. gs_w >b) gs_w = b
    endif

    gs=gs_w
    apot=an_w
    acl=-Resp/lai
    transp = min(ws,Ed) ! mol H20/(m2 of leaf s)
    ! just for reporting
    if (Ed>0.0) then
      w_scale2=min(1.0,ws/Ed)
    else
      w_scale2=1.0
    end if 

    ! finally, convert units of stomatal conductance to m/s from mol/(m2 s) by
    ! multiplying it by a volume of a mole of gas
    gs = gs * Rugas * Tl / p_surf
    !write(899, '(25(E12.4,","))') rad_net,par_net,apot*3600*12,acl*3600*12,Ed

  end subroutine gs_leuning


  ! subroutine calc_solarzen(td, latdegrees, cosz, solarelev, solarzen)
  !   ! Calculate solar zenith angle **in radians**
  !   ! From Spitters, C. J. T. (1986), AgForMet 38: 231-242.
  !   implicit none
  !   real, intent(in) :: td             ! day(to minute fraction)
  !   real, intent(in) :: latdegrees     ! latitude in degrees
  !   real :: hour,latrad
  !   real :: delta    ! declination angle
  !   real :: pi, rad
  !   real, intent(out) :: cosz        ! cosz=cos(zen angle)=sin(elev angle)
  !   real, intent(out) :: solarelev    ! solar elevation angle (rad)
  !   real, intent(out) :: solarzen     ! solar zenith angle (rad)

  !   pi  = 3.1415926
  !   rad = pi / 180.0 ! Conversion from degrees to radians.
  !   hour = (td-floor(td))*24.0
  !   latrad = latdegrees*rad
  !   delta  = asin(-sin(rad*23.450)*cos(2.0*pi*(td+10.0)/365.0))
  !   cosz = sin(latrad)*sin(delta) + &
  !   cos(latrad)*cos(delta)*cos(rad* 15.0*(hour-12.0))
  !   cosz = max (cosz, 0.01)  ! Sun's angular is 0.01
  !   ! compute the solar elevation and zenth angles below
  !   solarelev = asin(cosz)/pi*180.0  !since asin(cos(zen))=pi/2-zen=elev
  !   solarzen = 90.0 - solarelev ! pi/2.d0 - solarelev

  ! end subroutine calc_solarzen


  subroutine getpar_modl_gpp()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads module-specific parameters from input file.
    !----------------------------------------------------------------
    ! local variables
    integer :: pft

    !----------------------------------------------------------------
    ! PFT-independent parameters
    !----------------------------------------------------------------
    ! unit cost of carboxylation
    params_gpp%beta  = 146.000000

    ! Ratio of Rdark to Vcmax25, number from Atkin et al., 2015 for C3 herbaceous
    params_gpp%rd_to_vcmax  = 0.01400000

    ! Apply identical temperature ramp parameter for all PFTs
    params_gpp%tau_acclim     = 30.0
    params_gpp%soilm_par_a    = 1.0
    params_gpp%soilm_par_b    = 0.0

    ! temperature stress time scale is calibratable
    params_gpp%tau_acclim_tempstress = 20.0
    params_gpp%par_shape_tempstress  = 0.0

    ! ! PFT-dependent parameter(s)
    ! params_pft_gpp%kphio = myinterface%params_species(1)%kphio  ! is provided through standard input

  end subroutine getpar_modl_gpp


  ! adopted from BiomeE-Allocation, should use the one implemented in SOFUN instead (has slightly different parameters)
  FUNCTION esat(T) ! pressure, Pa
    IMPLICIT NONE
    REAL :: esat
    REAL, INTENT(IN) :: T ! degC
    esat=610.78*exp(17.27*T/(T+237.3))
  END FUNCTION esat


end module md_gpp_lm3ppa
