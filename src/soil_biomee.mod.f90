module md_soil_biomee
  !/////////////////////////////////////////////////////////////////////////
  ! The subroutines are from BiomeESS, the version used in Weng et al. 2016.
  ! This simulator can simulate evolutionarily stable strategy (ESS) of LMA
  ! and reproduce the forest succession patterns shown in Weng et al.,
  ! 2016 Global Change Biology along the graidient of temperature. 
  ! Code is adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !-------------------------------------------------------------------------
 use md_interface_biomee, only: myinterface
 use datatypes
 implicit none
 private

! ------ public subroutines ---------
public :: SoilWaterDynamicsLayer, water_supply_layer
public :: soil_data_beta

!---------------------------------
! ==== module data ===========================================================
real, public :: &
     cpw = 1952.0, & ! specific heat of water vapor at constant pressure
     clw = 4218.0, & ! specific heat of water (liquid)
     csw = 2106.0    ! specific heat of water (ice)

! soil layer depth
real     :: dz(max_lev) = thksl   ! thicknesses of layers
real     :: zfull(max_lev)

contains ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


! =============== soil water subroutines ==================================
! =========================================================================

!========================================================================
! Weng 2017-10-18 ! compute available water for photosynthesis
subroutine water_supply_layer( vegn)
  use md_forcing_biomee, only: climate_type
  type(vegn_tile_type), intent(inout) :: vegn

!----- local var --------------
  type(cohort_type),pointer :: cc
  real :: fWup(max_lev)      ! fraction to the actual soil water
  real :: freewater(max_lev)
  real :: totWsup(max_lev) ! potential water uptake, mol s-1 m-2
  real :: psi_soil, psi_leaf ! Pa, water potentials from soil to leaf
  real :: thetaS(max_lev) ! soil moisture index (0~1)
  real :: dpsiSR(max_lev) ! pressure difference between soil water and root water, Pa
  integer :: i,j

!! Plant hydraulics
   psi_leaf = -2.31 *1.0e6 ! pa, Katul et al. 2003, for clay soil
!! Water supply from each layer
  do i=1, max_lev ! Calculate water uptake potential layer by layer
     freewater(i) = max(0.0,((vegn%wcl(i)-WILTPT) * thksl(i) * 1000.0))
     thetaS(i)    = max(0.0, (vegn%wcl(i)-WILTPT)/(FLDCAP-WILTPT))
     !Soil water pressure
     psi_soil = soilpars(vegn%soiltype)%psi_sat_ref * &  ! Pa
            ((FLDCAP/vegn%wcl(i))**soilpars(vegn%soiltype)%chb)! water retention curve
     dpsiSR(i) = 1.5 *1.0e6 * thetaS(i)**2 ! Pa
     ! Layer allocation, water uptake capacity
     totWsup(i) = 0.0 ! Potential water uptake per layer by all cohorts
     do j = 1, vegn%n_cohorts
        cc => vegn%cohorts(j)
        associate ( sp => spdata(cc%species) )
        cc%WupL(i) = cc%rootareaL(i)*sp%Kw_root*dpsiSR(i) * (myinterface%step_seconds*h2o_molmass*1e-3) ! kg H2O tree-1 step-1
        totWsup(i) = totWsup(i) + cc%WupL(i) * cc%nindivs ! water uptake per layer by all cohorts
        end associate
     enddo
     ! adjust cc%WupL(i) according to available water
     do j = 1, vegn%n_cohorts
        cc => vegn%cohorts(j)
        if(totWsup(i)>0.0) &
            fWup(i) = Min(0.25 * freewater(i) / totWsup(i),1.0)! ratio of available soil water
        cc%WupL(i) = fWup(i) * cc%WupL(i) ! kg tree-1 step-1
     enddo ! cohort for each layer
  enddo    ! all layers

! total water suplly for each cohort
  do j = 1, vegn%n_cohorts
     cc => vegn%cohorts(j)
     cc%W_supply = sum(cc%WupL(:))
  enddo
 end subroutine water_supply_layer

! ============================================================================
! Weng, 2017-10-27
subroutine SoilWaterDynamicsLayer(forcing,vegn)    !outputs
!     All of inputs, the unit of water is 'mm',
!     soil moisture (soil water content) is a ratio
  use md_forcing_biomee, only: climate_type
  use md_params_core, only: kR

  type(vegn_tile_type), intent(inout) :: vegn
  type(climate_type),intent(in):: forcing

!----- local var --------------
  type(cohort_type),pointer :: cc
  real    :: rainwater,W_deficit(max_lev),W_add(max_lev)
  real    :: kappa  ! light extinction coefficient of corwn layers
  real    :: Esoil      ! soil surface evaporation, kg m-2 s-1
  real    :: Rsoilabs   ! W/m2
  real    :: Hgrownd    ! Ground heat flux, W/m2
  real    :: TairK,Tair      ! temperature, K and C, respectively
  real    :: RH         ! relative humidity, ratio to the saturated (0~1)
  real    :: Dair       ! VPD, pa
  real    :: rhocp !
  real    :: H2OLv
  real    :: slope
  real    :: psyc
  real    :: Cmolar ! mole density of air (mol/m3)
  real    :: rsoil  ! s m-1
  real    :: raero
  real    :: rLAI
  real    :: transp,fsupply ! fraction of transpiration from a soil layer
  real    :: WaterBudgetL(max_lev)
  integer :: i,j

  ! Soil water conditions
  !call water_supply_layer(forcing, vegn)

  ! Water uptaken by roots, hourly
  WaterBudgetL = 0.0
  vegn%transp = 0.0
  do j = 1, vegn%n_cohorts
      cc => vegn%cohorts(j)
      ! Compare with soil water
      ! cc%W_supply = sum(cc%WupL(:))
      ! cc%transp   = min(cc%transp,cc%W_supply)
      ! deduct from soil water pool
      if(cc%W_supply>0.0)then
         do i=1,max_lev
            fsupply = cc%WupL(i)/cc%W_supply
            transp  = fsupply * cc%transp * cc%nindivs
            !vegn%wcl(i) = vegn%wcl(i) - transp/(thksl(i)*1000.0)
            WaterBudgetL(i) = WaterBudgetL(i) - transp
            vegn%transp = vegn%transp + transp
         enddo
      endif
  enddo ! all cohorts

!! Soil surface evaporation
!    calculate kappa  ! light extinction coefficient of corwn layers
     kappa = 0.75
!    thermodynamic parameters for air

! print*, forcing%radiation, kappa, vegn%LAI    ! xxx debug

      Rsoilabs = forcing%radiation * exp(-kappa*vegn%LAI)

! print*,'forcing%radiation', forcing%radiation
! print*,'kappa', kappa
! print*,'vegn%LAI', vegn%LAI
! print*,'Rsoilabs', Rsoilabs
! print*, 'transp', transp

      Hgrownd = 0.0
      TairK = forcing%Tair
      Tair  = forcing%Tair - 273.16
      rhocp = cp * 1.0e3 * forcing%P_air * kMa * 1e-3 / (kR * TairK)
      H2OLv = H2oLv0 - 2.365e3 * Tair
      RH = forcing%RH  ! Check forcing's unit of humidity
      Dair  = calc_esat(Tair)*(1.0 - RH)
      slope = (calc_esat(Tair+0.1)-calc_esat(Tair))/0.1
      psyc = forcing%P_air * cp * 1.0e3 * kMa / (H2OLv * h2o_molmass)
      Cmolar = forcing%P_air/(kR * TairK) ! mole density of air (mol/m3)
      rsoil = exp(8.206-4.255*fldcap) ! s m-1, Liu Yanlan et al. 2017, PNAS
      !Rsoil=3.0E+10 * (FILDCP-vegn%wcl(1))**16 ! Kondo et al. 1990
      !rsoil=7500 * exp(-50.0*vegn%wcl(1))  ! s m-1
      raero = 50./(forcing%windU + 0.2)
      rLAI = exp(vegn%LAI)
!     latent heat flux into air from soil
!           Eleaf(ileaf)=1.0*
!     &     (slope*Y*Rnstar(ileaf)+rhocp*Dair/(rbH_L+raero))/    !2* Weng 0215
!     &     (slope*Y+psyc*(rswv+rbw+raero)/(rbH_L+raero))
      Esoil=(slope*Rsoilabs + rhocp*Dair/raero)/ &
            (slope + psyc*(1.0+rsoil/raero)) *   &
            max(vegn%wcl(1),0.0)/FLDCAP ! (vegn%wcl(1)-ws0)/(FLDCAP-ws0)

  !Calculate Esoil, kg m-2 step-1
  vegn%evap = min(Esoil/H2OLv * myinterface%step_seconds, 0.2*vegn%wcl(1) * thksl(1) *1000.) ! kg m-2 step-1
  !vegn%wcl(1) = vegn%wcl(1) - vegn%evap/(thksl(1) *1000.)
  WaterBudgetL(1) = WaterBudgetL(1) - vegn%evap
! print*,'slope', slope
! print*,'Rsoilabs', Rsoilabs
! print*,'rhocp', rhocp
! print*,'Dair', Dair
! print*,'raero', raero
! print*,'psyc', psyc
! print*,'rsoil', rsoil
! print*,'vegn%wcl(1)', vegn%wcl(1)
! print*,'FLDCAP', FLDCAP

! print*,'vegn%evap',vegn%evap
! print*,'Esoil',Esoil
! print*,'H2OLv',H2OLv
! print*,'myinterface%step_seconds',myinterface%step_seconds
! print*,'vegn%wcl(1)',vegn%wcl(1)
! print*,'thksl(1)',thksl(1)

!! soil water refill by precipitation
  rainwater =  forcing%rain * myinterface%step_seconds
  if(rainwater > 0.0)then
     do i=1, max_lev
        W_deficit(i) = (FLDCAP - vegn%wcl(i)) * thksl(i)*1000.0
        W_add(i) = min(rainwater, W_deficit(i))
        rainwater = rainwater - W_add(i)
        !vegn%wcl(i) = vegn%wcl(i) + W_add(i)/(thksl(i)*1000.0)
        WaterBudgetL(i) = WaterBudgetL(i) + W_add(i)

        if(rainwater<=0.0)exit
     enddo
  endif
  vegn%runoff = rainwater ! mm step-1

  ! Total soil water
  vegn%SoilWater = 0.0
  do i=1,max_lev
     vegn%wcl(i) = vegn%wcl(i) +  WaterBudgetL(i)/(thksl(i)*1000.0)
     vegn%SoilWater = vegn%SoilWater + vegn%wcl(i)*thksl(i)*1000.0
  enddo

end subroutine SoilWaterDynamicsLayer

! ==============used in LM3, but not here =============================
! =============== just for reference =========================================
! ============================================================================
! compute uptake-related properties
subroutine soil_data_beta(soil, vegn, soil_beta, soil_water_supply, &
                            soil_uptake_T )
  type(soil_tile_type), intent(in)    :: soil
  type(vegn_tile_type), intent(inout) :: vegn
  real, intent(out) :: soil_beta(:) ! relative water availability, used only in VEGN_PHOT_SIMPLE treatment
  real, intent(out) :: soil_water_supply(:) ! max rate of water supply to roots, kg/(indiv s)
  real, intent(out) :: soil_uptake_T(:) ! an estimate of temperature of the water 
             ! taken up by transpiration. In case of 'linear' uptake it is an exact
             ! value; in case of 'darcy*' treatments the actual uptake profile
             ! is calculated only in step 2, so the value returned is an estimate  

  ! ---- local vars
  integer :: k, l
  real    :: dz(max_lev)    ! thicknesses of layers
  real, dimension(num_l) :: &
       uptake_frac_max, & ! normalized root distribution
       vegn_uptake_term, &
       vlc, vsc, & ! volumetric fractions of water and ice in the layer
   !    root_length, & ! vertical distribution of volumetric root length, m/m3
       VRL, & ! volumetric root length
       u, du ! uptake and its derivative (the latter is not used)
  ! real :: z  !  soil depth
  !real :: psi_wilt ! added by Weng, 2017-10-29
  logical :: uptake_oneway = .TRUE. ! added by Weng
  logical :: uptake_from_sat = .true.
  type (cohort_type), pointer :: cc

  dz = thksl ! Weng
  do l = 1, num_l
    vlc(l) = max(0., soil%prog(l)%wl / (dens_h2o*dz(l)))
    vsc(l) = max(0., soil%prog(l)%ws / (dens_h2o*dz(l)))
  enddo

  ! calculate volumetric root length for the entire tile
  VRL(:) = 0.0
  do k = 1, vegn%n_cohorts
     cc=>vegn%cohorts(k)
     call cohort_root_properties(cc, dz(1:num_l), cc%root_length(1:num_l), &
                                 cc%K_r, cc%r_r)
     VRL(:) = VRL(:)+cc%root_length(1:num_l)*cc%nindivs
  enddo
  
  ! calculate characteristic half-distance between roots, m
  where (VRL(:) > 0)
     vegn%root_distance(1:num_l) = 1.0/sqrt(PI*VRL(:))
  elsewhere
     vegn%root_distance(1:num_l) = 1.0 ! the value doesn't matter since uptake is 0 anyway 
  end where

  do k = 1, vegn%n_cohorts 
     cc=>vegn%cohorts(k)
     call cohort_uptake_profile (cc, dz(1:num_l), uptake_frac_max, vegn_uptake_term )

     do l = 1, num_l
        cc%uptake_frac(l) = uptake_frac_max(l) &
             * max(0.0, min(1.0,(vlc(l)-soil%w_wilt(l))/&
                  (0.75*(soil%w_fc(l)-soil%w_wilt(l)))))
     enddo
     soil_beta(k) = sum(cc%uptake_frac(:))
     if (soil_beta(k) /= 0) then
          cc%uptake_frac(:) = cc%uptake_frac(:) / soil_beta(k)
     else
          cc%uptake_frac(:) = uptake_frac_max(:)
     endif

     ! calculate total water supply
     call darcy2d_uptake_lin ( soil, psi_wilt, vegn%root_distance, cc%root_length, &
             cc%K_r, cc%r_r, uptake_oneway, uptake_from_sat, u, du)
     soil_water_supply(k) = max(0.0,sum(u))
     soil_uptake_T(k) = soil%uptake_T

  enddo
end subroutine soil_data_beta
! ============================================================================
subroutine darcy2d_uptake_lin ( soil, psi_x0, R, VRL, K_r, r_r,uptake_oneway, &
    uptake_from_sat, u, du )
  type(soil_tile_type), intent(in) :: soil
  real, intent(in) :: &
       psi_x0,    & ! water potential inside roots (in xylem) at zero depth, m
       R(:),      & ! characteristic half-distance between roots, m
       VRL(:),    & ! Volumetric Root Length (root length per unit volume), m/m3
       K_r,       & ! permeability of the root membrane per unit area, kg/(m3 s)
       r_r          ! radius of fine roots, m
  logical, intent(in) :: &
       uptake_oneway, & ! if true, then the roots can only take up water, but 
                   ! never loose it to the soil
       uptake_from_sat   ! if false, uptake from saturated soil is prohibited
  real, intent(out) :: &
       u(:), &      ! layer-by-layer distribution of uptake, kg/(m2 s)
       du(:)        ! derivative of u w.r.t. root water potential, kg/(m3 s)
  ! ---- local vars
  integer :: k
  real :: psi_x     ! water potential inside roots (psi_x0+z), m
  real :: psi_soil  ! water potential of soil, m
  real :: psi_sat   ! saturation soil water potential, m
  real :: K_sat     ! hydraulic conductivity of saturated soil, kg/(m2 s)
  
  real :: psi_root  ! water potential at the root/soil interface, m
  real :: psi_root0 ! initial guess of psi_root, m


  ! calculate some hydraulic properties common for all soil layers
  psi_sat = soil%pars%psi_sat_ref/soil%pars%alpha
  K_sat   = soil%pars%k_sat_ref*soil%pars%alpha**2

  u = 0; du = 0
  do k = 1, num_l
     psi_x    = psi_x0 + zfull(k)
     psi_soil = soil%psi(k)
     psi_root0= soil%psi(k) ! change it later to prev. time step value
     if ( soil%prog(k)%ws > 0 ) &
          cycle ! skip layers with ice
     if ( uptake_oneway.and.psi_x > soil%psi(k) ) &
          cycle ! skip layers where roots would loose water
     if ( .not.(uptake_from_sat).and.psi_soil >= psi_sat ) &
          cycle ! skip layers where the soil is saturated

     ! calculates soil term of uptake expression
     call darcy2d_flow_lin (psi_x, psi_soil, psi_root0, K_sat, psi_sat, soil%pars%chb, &
          K_r, r_r, R(k), u(k), du(k), psi_root)

     ! scale by volumetric root length and thickness of layer to get total 
     ! uptake from the current soil layer
     u(k)  = VRL(k)*dz(k)*u(k)
     du(k) = VRL(k)*dz(k)*du(k)
  enddo

end subroutine darcy2d_uptake_lin

! ============================================================================
! given soil and root parameters, calculate the flux of water toward root
! per unit root length, and its derivative w.r.t. xylem water potential
! this version calculates fluxes linearized around psi_root0
subroutine darcy2d_flow_lin (psi_x, psi_soil, psi_root0, K_sat, psi_sat, b, K_r, &
     r_r, R, u, du, psi_root)
  real, intent(in) :: &
       psi_x,    & ! xylem water potential, m
       psi_soil, & ! soil water potential, m
       psi_root0,& ! value of psi_root we linearize around, m
       K_sat,    & ! saturated soil hydraulic conductivity, kg/(m2 s)
       psi_sat,  & ! saturates soil water potential, m
       b,        & ! power of soil moisture characteristic function
       K_r,      & ! root membrane permeability per unit area, kg/(m3 s)
       r_r,      & ! radius of root, m
       R           ! characteristic radial half-distance between roots, m
  real, intent(out) :: &
       u,        & ! uptake, kg/(m s)
       du,       & ! derivative of uptake w.r.t psi_x, kg/(m2 s)
       psi_root    ! water potential at the root/soil interface, m

  ! ---- local vars
  real :: u_soil0 ! flux through soil for psi_root = psi_root0
  real :: du_soil ! its derivative w.r.t. psi_root
  real :: C_r !
  real :: n
  real :: K_root  ! root membrane permeability per unit length, kg/(m2 s)

  C_r=2*PI/(log(R/r_r))
  n = -(1+3/b)
  K_root = 2*PI*r_r*K_r

  ! calculate flux through soil for psi_root = psi_root0
  u_soil0 = C_r*K_sat*&
       (psi_sat/n* &
          (  (min(psi_soil ,psi_sat)/psi_sat)**n   &
            -(min(psi_root0,psi_sat)/psi_sat)**n ) &
          + max(0.0, psi_soil  - psi_sat)          &
          - max(0.0, psi_root0 - psi_sat)          )
  ! and its derivative w.r.t. psi_root at psi_root0
  du_soil=-C_r*K_sat*(min(psi_root0,psi_sat)/psi_sat)**(n-1)

  ! flux through soil+membrane
  u  = K_root/(-du_soil+K_root)*(u_soil0+du_soil*(psi_x-psi_root0))
  ! and its derivative w.r.t. psi_x
  du = K_root/(-du_soil+K_root)*du_soil
  ! water potential at the root-soil interface
  psi_root = psi_x + u/K_root
end subroutine

! ============================================================================
! returns properties of the fine roots
subroutine cohort_root_properties(cohort, dz, vrl, K_r, r_r)
  type(cohort_type), intent(in)  :: cohort
  real, intent(in)  :: dz(:)
  real, intent(out) :: &
       vrl(:), & ! volumetric fine root length, m/m3
       K_r,    & ! root membrane permeability per unit area, kg/(m3 s)
       r_r       ! radius of fine roots, m

  integer :: sp, l
  real :: factor, z
  real :: vbr ! volumetric biomass of fine roots, kg C/m3

  sp = cohort%species

  factor = 1.0/(1.0-exp(-sum(dz)/cohort%root_zeta))
  z = 0
  do l = 1, size(dz)
     ! calculate the volumetric fine root biomass density [kgC/m3] for current layer
     ! NOTE: sum(brv*dz) must be equal to cohort%proot%c%c12, which is achieved by normalizing
     ! factor
     vbr = cohort%proot%c%c12 * &
          (exp(-z/cohort%root_zeta) - exp(-(z+dz(l))/cohort%root_zeta))*factor/dz(l)
     ! calculate the volumetric fine root length
     vrl(l) = vbr*spdata(sp)%srl

     z = z + dz(l)
  enddo

  K_r = spdata(sp)%root_perm
  r_r = spdata(sp)%root_r

end subroutine

! ============================================================================
! calculates vertical distribution of active roots: given layer thicknesses,
! returns fraction of active roots per level
subroutine cohort_uptake_profile(cohort, dz, uptake_frac_max, vegn_uptake_term)

  type(cohort_type), intent(in)  :: cohort
  real, intent(in)  :: dz(:)
  real, intent(out) :: uptake_frac_max(:)
  real, intent(out) :: vegn_uptake_term(:)

  real, parameter :: res_scaler = kMa / h2o_molmass  ! scaling factor for water supply

  ! NOTE: there is an inconsistency there between the 
  ! units of stomatal conductance [mol/(m2 s)], and the units of humidity deficit [kg/kg],
  ! in the calculations of water demand. Since the uptake options other than LINEAR can't 
  ! use res_scaler, in this code the units of humidity deficit are converted to mol/mol,
  ! and the additional factor is introduced in res_scaler to ensure that the LINEAR uptake 
  ! gives the same results.

  integer :: l
  real    :: z, sum_rf

     !linear scaling, LM3V
     z = 0
     do l = 1, size(dz)
        uptake_frac_max(l) = (exp(-z/cohort%root_zeta)    &
                - exp(-(z+dz(l))/cohort%root_zeta))
        uptake_frac_max(l) = &
                max( uptake_frac_max(l), 0.0)
        z = z + dz(l)
     enddo
  
  sum_rf = sum(uptake_frac_max)
  if(sum_rf>0) &
       uptake_frac_max(:) = uptake_frac_max(:)/sum_rf
  
  if (cohort%proot%c%c12 <= 0) then
     vegn_uptake_term(:) = 0.0
  else   
     vegn_uptake_term(:) = uptake_frac_max(:) * &
          res_scaler * spdata(cohort%species)%root_r * cohort%proot%c%c12
  endif

end subroutine 
! ================================================

end module md_soil_biomee



