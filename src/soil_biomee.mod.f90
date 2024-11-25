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
 use md_sofunutils, only: calc_esat
 implicit none
 private

! ------ public subroutines ---------
public :: SoilWaterDynamicsLayer, water_supply_layer

!---------------------------------
! ==== module data ===========================================================
real, public :: &
     cpw = 1952.0, & ! specific heat of water vapor at constant pressure
     clw = 4218.0, & ! specific heat of water (liquid)
     csw = 2106.0    ! specific heat of water (ice)

contains

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
      real :: thetaS(max_lev) ! soil moisture index (0~1)
      real :: dpsiSR(max_lev) ! pressure difference between soil water and root water, Pa
      integer :: i,j

    !! Water supply from each layer
      do i=1, max_lev ! Calculate water uptake potential layer by layer
         freewater(i) = max(0.0,((vegn%wcl(i)-WILTPT) * thksl(i) * 1000.0))
         thetaS(i)    = max(0.0, (vegn%wcl(i)-WILTPT)/(FLDCAP-WILTPT))
         dpsiSR(i) = 1.5 *1.0e6 * thetaS(i)**2 ! Pa
         ! Layer allocation, water uptake capacity
         totWsup(i) = 0.0 ! Potential water uptake per layer by all cohorts
         do j = 1, vegn%n_cohorts
            cc => vegn%cohorts(j)
            associate ( sp => myinterface%params_species(cc%species) )
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
      real    :: transp,fsupply ! fraction of transpiration from a soil layer
      real    :: WaterBudgetL(max_lev)
      integer :: i,j

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

end module md_soil_biomee



