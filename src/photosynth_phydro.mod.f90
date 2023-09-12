module md_photosynth_phydro
  use md_photosynth, only: calc_viscosity_h2o, calc_density_h2o, calc_kmm, calc_gammastar
  use md_sofunutils, only: calc_patm, zero, gammad
  

  ! -------------------------------------------------------------
  ! Definitions: Precision 
  !--------------------------------------------------------------
  implicit none

  integer, parameter :: int4=SELECTED_INT_KIND(4)
  integer, parameter :: flt4=SELECTED_REAL_KIND(6,37)
  integer, parameter :: dbl8=SELECTED_REAL_KIND(15,307)

  ! -------------------------------------------------------------
  ! Definitions: Environment
  !--------------------------------------------------------------
  ! list of methods to calculate gs
  integer (kind = int4), parameter :: GS_IGF = 0, GS_QNG = 1, GS_APX = 2, GS_APX2 = 3
  
  integer (kind = int4), parameter :: ET_DIFFUSION = 0, ET_PM = 1

  ! Define the data type for ParEnv
  type par_env_type
    real(kind = dbl8) :: tc          ! Temperature [degC]
    real(kind = dbl8) :: patm        ! Atmospheric pressure [Pa]
    real(kind = dbl8) :: vpd         ! VPD [Pa]
    real(kind = dbl8) :: Rn          ! Net radiation [W m-2]
    real(kind = dbl8) :: v_wind      ! Wind speed [m s-1]
    real(kind = dbl8) :: viscosity_water  ! [Pa s]
    real(kind = dbl8) :: density_water    ! [kg m-3]
    real(kind = dbl8) :: rho         ! Density of air [kg m-3]
    real(kind = dbl8) :: cp          ! Specific heat capacity of moist air [J kg-1 K-1]
    real(kind = dbl8) :: gamma       ! Psychrometric constant [Pa K-1]
    real(kind = dbl8) :: epsilon     ! Slope of saturation-pressure - temp curve [Pa K-1]
    real(kind = dbl8) :: lv          ! Latent heat of vaporization of water [J kg-1]
    integer(kind = int4) :: gs_method = GS_IGF ! GsMethod
    integer(kind = int4) :: et_method = ET_DIFFUSION  ! ETMethod
  end type par_env_type

  ! ! Interface for member subroutines
  ! interface par_env_type_interface
  !   module procedure :: create_par_env
  !   ! module procedure :: calc_temp_dependencies
  !   ! module procedure :: print_par_env
  ! end interface

  ! -------------------------------------------------------------
  ! Definitions: Phydro transpiration
  !--------------------------------------------------------------
  type par_plant_type
    real (kind = dbl8) :: conductivity        ! = ci/ca, leaf-internal to ambient CO2 partial pressure, ci/ca (unitless)
    real (kind = dbl8) :: psi50               ! leaf-internal CO2 partial pressure (Pa)
    real (kind = dbl8) :: b                   ! ci-limitation factor of light-limited assimilation (unitless)

    real (kind = dbl8) :: h_canopy = 20
    real (kind = dbl8) :: h_wind_measurement = 22
    real (kind = dbl8) :: tchome = 25

    integer (kind = int4) :: gs_method = GS_IGF
  end type par_plant_type


  ! -------------------------------------------------------------
  ! Definitions: Phydro photosynthesis
  !--------------------------------------------------------------

  ! list of methods to model temperature dependencies of Vcmax and Jmax
  integer (kind = int4), parameter :: FV_kattge07 = 0, FV_kumarathunge19 = 1, FV_leuning02 = 2

  ! list of methods to model temperature dependencies of Rd
  integer (kind = int4), parameter :: FR_heskel16 = 0, FR_arrhenius = 1, FR_q10 = 2

  ! list of methods to model temperature dependencies of br
  integer (kind = int4), parameter :: FB_atkin15 = 0, FB_kumarathunge19 = 1

  type par_photosynth_type
    real (kind = dbl8) ::  kmm
    real (kind = dbl8) ::  gammastar
    real (kind = dbl8) ::  phi0
    real (kind = dbl8) ::  ca
    real (kind = dbl8) ::  delta

    integer(kind = int4) :: ftemp_vj_method;
    integer(kind = int4) :: ftemp_rd_method;
    integer(kind = int4) :: ftemp_br_method;
  
    real (kind = dbl8) ::  Iabs
    real (kind = dbl8) ::  patm

    real (kind = dbl8) ::  fT_vcmax;
    real (kind = dbl8) ::  fT_jmax;
    real (kind = dbl8) ::  fT_rd;
  
  end type par_photosynth_type

  type ACi_type
    real(kind=dbl8) :: a
    real(kind=dbl8) :: ci
    logical :: isVcmaxLimited
  end type ACi_type


  ! -------------------------------------------------------------
  ! Definitions: Phydro solver
  !--------------------------------------------------------------

  type par_cost_type
    real (kind = dbl8) :: alpha
    real (kind = dbl8) :: gamma
  end type par_cost_type
  
  type dpsi_bounds_type
    real (kind = dbl8) ::  exact
    real (kind = dbl8) ::  approx_O2
    real (kind = dbl8) ::  Iabs_bound
  end type dpsi_bounds_type

  type dfdx_type
    real (kind = dbl8) ::  dPdx
    real (kind = dbl8) ::  J
    real (kind = dbl8) ::  djmax_dJ
    real (kind = dbl8) ::  dJ_dchi
  end type dfdx_type

  ! -------------------------------------------------------------
  ! Definitions: Phydro main
  !--------------------------------------------------------------

  type phydro_result_type
    real(kind = dbl8) :: a
    real(kind = dbl8) :: e
    real(kind = dbl8) :: gs
    real(kind = dbl8) :: ci
    real(kind = dbl8) :: chi
    real(kind = dbl8) :: vcmax
    real(kind = dbl8) :: jmax
    real(kind = dbl8) :: dpsi
    real(kind = dbl8) :: psi_l
    real(kind = dbl8) :: nfnct
    real(kind = dbl8) :: niter
    real(kind = dbl8) :: mc
    real(kind = dbl8) :: mj
    real(kind = dbl8) :: gammastar
    real(kind = dbl8) :: kmm
    real(kind = dbl8) :: vcmax25
    real(kind = dbl8) :: jmax25
    real(kind = dbl8) :: rd
    real(kind = dbl8) :: isVcmaxLimited
    real(kind = dbl8) :: ac
    real(kind = dbl8) :: aj
    real(kind = dbl8) :: le
    real(kind = dbl8) :: le_s_wet
  end type phydro_result_type

  type par_control_type
    integer(kind = int4) :: gs_method       = GS_IGF
    integer(kind = int4) :: et_method       = ET_DIFFUSION
    integer(kind = int4) :: ftemp_vj_method = FV_kumarathunge19
    integer(kind = int4) :: ftemp_rd_method = FR_heskel16
    integer(kind = int4) :: ftemp_br_method = FB_atkin15
    integer(kind = int4) :: scale_alpha     = 0
  end type par_control_type



  contains

  ! -------------------------------------------------------------
  ! Functions: Physical relationships
  !--------------------------------------------------------------
  function calc_esat(TdegC, patm) result(esatval)
    real(kind = dbl8), intent(in) :: TdegC, patm
    real(kind = dbl8) :: esatval
    real(kind = dbl8) :: a, b, c, f
  
    a = 611.21
    b = 17.502
    c = 240.97
    f = 1.0007 + 3.46e-8 * patm
  
    esatval = f * a * exp(b * TdegC / (c + TdegC))
  end function calc_esat
  
  function calc_density_air(tc_air, patm, vpd, moist) result(rho)
    real(kind = dbl8), intent(in) :: tc_air, patm, vpd
    logical, intent(in) :: moist
    real(kind = dbl8) :: rho, tk, R
    real(kind = dbl8) :: vp, rv, tv
  
    tk = tc_air + 273.16
    R = 287.052874
  
    if (.not. moist) then
    rho = patm / R / tk
    else
    vp = calc_esat(tc_air, patm) - vpd
    rv = 0.622 * vp / (patm - vp)
    tv = tk * (1.0 + rv / 0.622) / (1.0 + rv)
  
    rho = patm / R / tv
    end if
  end function calc_density_air
  
  function calc_enthalpy_vap(tc) result(enthalpy)
    real(kind = dbl8), intent(in) :: tc
    real(kind = dbl8) :: enthalpy, tk, a
  
    tk = tc + 273.15
    a = tk / (tk - 33.91)
  
    enthalpy = 1.91846e6 * a**2
  end function calc_enthalpy_vap
  
  function calc_cp_moist_air(tc) result(cp)
    real(kind = dbl8), intent(in) :: tc
    real(kind = dbl8) :: cp, my_tc
  
    my_tc = max(min(tc, 100.0), 0.0)
    
    cp =         (1.0045714270 +     &
       my_tc * (2.050632750e-3 +   &
       my_tc * (-1.631537093e-4 +  &
       my_tc * (6.212300300e-6 -   &
       my_tc * (8.830478888e-8 -   &
       my_tc * 5.071307038e-10))))) * 1e3
       
  end function calc_cp_moist_air
  
  function calc_psychro(tc, patm) result(psychro)
    real(kind = dbl8), intent(in) :: tc, patm
    real(kind = dbl8) :: psychro, Ma, Mv, cp, lv
  
    Ma = 0.02896
    Mv = 0.018016
  
    cp = calc_cp_moist_air(tc)
    lv = calc_enthalpy_vap(tc)
  
    psychro = cp * patm / ((Mv / Ma) * lv)
  end function calc_psychro
  
  function calc_sat_slope(tc) result(slope)
    real(kind = dbl8), intent(in) :: tc
    real(kind = dbl8) :: slope
  
    slope = 17.269 * 237.3 * 610.78 * exp(tc * 17.269 / (tc + 237.3)) / ((tc + 237.3)**2)
  end function calc_sat_slope
   
  ! -------------------------------------------------------------
  ! Functions: Environment
  !--------------------------------------------------------------
  ! Constructor for ParEnv
  subroutine create_par_env(this, tc, patm, vpd, Rn, v_wind) 
    type(par_env_type), intent(inout) :: this
    real(kind = dbl8), intent(in) :: tc, patm, vpd, Rn, v_wind
    this%tc = tc
    this%vpd = vpd
    this%patm = patm
    this%Rn = Rn
    this%v_wind = v_wind
    this%gs_method = GS_IGF 
    this%et_method = ET_DIFFUSION
    call calc_temp_dependencies(this)
  end subroutine create_par_env

  ! Separate constructor without v_wind as a parameter
  subroutine create_par_env_no_wind(this, tc, patm, vpd, Rn)
    type(par_env_type), intent(out) :: this
    real(kind = dbl8), intent(in) :: tc, patm, vpd, Rn
    call create_par_env(this, tc, patm, vpd, Rn, 3.0d0) ! Default v_wind
  end subroutine create_par_env_no_wind

  ! Calculate temperature dependencies
  subroutine calc_temp_dependencies(this)
    type(par_env_type), intent(inout) :: this
    this%viscosity_water = calc_viscosity_h2o(real(this%tc), real(this%patm))
    this%density_water = calc_density_h2o(real(this%tc), real(this%patm))
    this%rho = calc_density_air(this%tc, this%patm, this%vpd, .true.)
    this%cp = calc_cp_moist_air(this%tc)
    this%gamma = calc_psychro(this%tc, this%patm)
    this%epsilon = calc_sat_slope(this%tc) / this%gamma
    this%lv = calc_enthalpy_vap(this%tc)
  end subroutine calc_temp_dependencies

  ! Print ParEnv information
  subroutine print_par_env(this)
    type(par_env_type), intent(in) :: this
    write(*, *) "Env:"
    write(*, *) "   tc = ", this%tc, " [degC]"
    write(*, *) "   patm = ", this%patm, " [Pa]"
    write(*, *) "   vpd = ", this%vpd, " [Pa]"
    write(*, *) "   Rn = ", this%Rn, " [W m-2]"
    write(*, *) "   v_wind = ", this%v_wind, " [m s-1]"
    write(*, *) "   viscosity_water = ", this%viscosity_water, " [Pa s]"
    write(*, *) "   density_water = ", this%density_water, " [kg m-3]"
    write(*, *) "   rho = ", this%rho, " [kg m-3]"
    write(*, *) "   cp = ", this%cp, " [J kg-1 K-1]"
    write(*, *) "   gamma = ", this%gamma, " [Pa K-1]"
    write(*, *) "   epsilon = ", this%epsilon, " [Pa K-1]"
    write(*, *) "   lv = ", this%lv, " [J kg-1]"
  end subroutine print_par_env

  ! -------------------------------------------------------------
  ! Functions: PM 
  !--------------------------------------------------------------
 
  function calc_g_aero(h_canopy, v_wind, z_measurement) result(g_aero)
    ! Aerodynamic conductance [m s-1]
    ! To convert to mol m-2 s-1, see this: https://rdrr.io/cran/bigleaf/man/ms.to.mol.html (but not convincing)
    ! Refs: 
    !    Eq 13 in Leuning et al (2008). https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2007WR006562
    !    Eq 7 in Zhang et al (2008): https://agupubs.onlinelibrary.wiley.com/doi/10.1002/2017JD027025
    !    Box 4 in https://www.fao.org/3/x0490e/x0490e06.htm 
    real(kind = dbl8), intent(in) :: h_canopy, v_wind, z_measurement
    real(kind = dbl8) :: g_aero, k_karman, d, z_om, z_ov
    
    k_karman = 0.41        ! von Karman's constant [-]
    d = h_canopy * 2.0 / 3.0   ! zero-plane displacement height [m]
    z_om = 0.123 * h_canopy    ! roughness lengths governing transfer of water and momentum [m]
    z_ov = 0.1 * z_om
    
    g_aero = (k_karman * k_karman * v_wind) / (log((z_measurement - d) / z_om) * log((z_measurement - d) / z_ov))
  end function calc_g_aero


  function gs_conv(tc, patm) result(gs_conv_value)
    ! multiplier to convert:
    !   stomatal conductance to CO2 [mol m-2 s-1] ----> stomatal conductance to water [m s-1]
    real(kind = dbl8), intent(in) :: tc, patm
    real(kind = dbl8) :: gs_conv_value, R
    
    R = 8.31446261815324 ! Universal gas constant [J mol-1 K-1]
    
    gs_conv_value = 1.6 * R * (tc + 273.16) / patm
  end function gs_conv
    

  function calc_transpiration_pm(gs, ga, par_env) result(trans)
    ! Calculate PML transpiration [mol m-2 s-1]
    ! gs   Stomatal conductance to CO2 [mol m-2 s-1]
    ! ga   Aerodynamic conductance [m s-1]
    ! Rn   Absorbed net radiation [W m-2]
    real(kind = dbl8), intent(in) :: gs, ga
    type(par_env_type), intent(in) :: par_env
    real(kind = dbl8) :: trans, gw, latent_energy
    
    gw = gs * gs_conv(par_env%tc, par_env%patm)  ! gw in [m s-1]
    
    latent_energy = (par_env%epsilon * par_env%Rn + (par_env%rho * par_env%cp / par_env%gamma) &
                    * ga * par_env%vpd) / (par_env%epsilon + 1 + ga / gw) ! latent energy W m-2 
    trans = latent_energy * (55.5 / par_env%lv) ! W m-2 ---> mol m-2 s-1
  end function calc_transpiration_pm


  function calc_max_transpiration_pm(ga, par_env) result(trans_max)
    ! Calculate maximum possible PML transpiration for a given ga, calculated by setting gs = inf, [mol m-2 s-1]
    ! ga   Aerodynamic conductance [m s-1]
    ! Rn   Absorbed net radiation [W m-2]
    real(kind = dbl8), intent(in) :: ga
    type(par_env_type), intent(in) :: par_env
    real(kind = dbl8) :: trans_max, latent_energy

    latent_energy = (par_env%epsilon * par_env%Rn + (par_env%rho * par_env%cp / par_env%gamma) &
                    * ga * par_env%vpd) / (par_env%epsilon + 1) ! latent energy W m-2 
    trans_max = latent_energy * (55.5 / par_env%lv) ! W m-2 ---> mol m-2 s-1
  end function calc_max_transpiration_pm


  function calc_gs_pm(Q, ga, par_env) result(gs)
    ! Calculate PML stomatal conductance to CO2 [mol m-2 s-1]
    ! Q    Sap flux [mol m-2 s-1]
    ! ga   Aerodynamic conductance [m s-1]
    ! Rn   Absorbed net radiation [W m-2]
    real(kind = dbl8), intent(in) :: Q, ga
    type(par_env_type), intent(in) :: par_env
    real(kind = dbl8) :: gs, Q_energy, den, gw

    Q_energy = Q * (par_env%lv / 55.5)

    den = par_env%epsilon * par_env%Rn + (par_env%rho * par_env%cp / par_env%gamma) &
          * ga * par_env%vpd - (1 + par_env%epsilon) * Q_energy
    !den = fmax(den, 0)

    gw = ga * Q_energy / den ! stomatal conductance to water [m s-1]

    gs = gw / gs_conv(par_env%tc, par_env%patm) ! stomatal conductance to CO2 [mol m-2 s-1]
  end function calc_gs_pm


  function calc_dE_dgs_pm(gs, ga, par_env) result(dE_dgs)
    ! Calculate derivative of transpiration wrt stomatal conductance to CO2 [unitless] - analytical version
    real(kind = dbl8), intent(in) :: gs, ga
    type(par_env_type), intent(in) :: par_env
    real(kind = dbl8) :: dE_dgs, gw, num, den, d_le_dgw

    gw = gs * gs_conv(par_env%tc, par_env%patm)  ! [m s-1]

    num = ga * (par_env%epsilon * par_env%Rn + (par_env%rho * par_env%cp / par_env%gamma) * ga * par_env%vpd)
    den = par_env%epsilon * gw + gw + ga

    d_le_dgw = (num / den / den) ! derivative of latent energy wrt stomatal conductance for water in m s-1

    dE_dgs = d_le_dgw * (55.5 / par_env%lv) * gs_conv(par_env%tc, par_env%patm)
  end function calc_dE_dgs_pm


  function calc_dE_dgs_pm_num(gs, ga, par_env) result(dE_dgs)
    ! Calculate derivative of transpiration wrt stomatal conductance to CO2 [unitless] - numerical version
    real(kind = dbl8), intent(in) :: gs, ga
    type(par_env_type), intent(in) :: par_env
    real(kind = dbl8) :: dE_dgs, E, E_plus

    E = calc_transpiration_pm(gs, ga, par_env)
    E_plus = calc_transpiration_pm(gs + 1.0e-6, ga, par_env)

    dE_dgs = (E_plus - E) / 1.0e-6
  end function calc_dE_dgs_pm_num

  ! -------------------------------------------------------------
  ! Functions: Phydro transpiration
  !--------------------------------------------------------------
  ! Constructors for par plant
  subroutine init_par_plant(this, cond, psi, b)
    class(par_plant_type), intent(out) :: this
    real(kind=dbl8), intent(in) :: cond, psi, b
    this%conductivity = cond
    this%psi50 = psi
    this%b = b
  end subroutine init_par_plant

  subroutine init_par_plant_6args(this, cond, psi, b, hcanopy, hwind, tchome)
    class(par_plant_type), intent(out) :: this
    real(kind=dbl8), intent(in) :: cond, psi, b, hcanopy, hwind, tchome
    this%conductivity = cond
    this%psi50 = psi
    this%b = b
    this%h_canopy = hcanopy
    this%h_wind_measurement = hwind
    this%tchome = tchome
  end subroutine init_par_plant_6args   


  
  !!! Vulnerability curve
  !!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  function P(psi, psi50, b)
    real (kind = dbl8), intent(in) :: psi
    real (kind = dbl8), intent(in) :: psi50
    real (kind = dbl8), intent(in) :: b
    real (kind = dbl8) :: P
    P = 0.5 ** ((psi/psi50) ** b)
  end

  function Pprime(psi, psi50, b)
    real (kind = dbl8), intent(in) :: psi
    real (kind = dbl8), intent(in) :: psi50
    real (kind = dbl8), intent(in) :: b
    real (kind = dbl8) :: Pprime
    Pprime = log(0.5) * P(psi,psi50,b) * b * ((psi/psi50)**(b-1)) / psi50
  end

  function Pprimeprime(psi, psi50, b)
    real (kind = dbl8), intent(in) :: psi
    real (kind = dbl8), intent(in) :: psi50
    real (kind = dbl8), intent(in) :: b
    real (kind = dbl8) :: Pprimeprime
    Pprimeprime = log(0.5)*b*((psi/psi50)**(b-1))/psi50 * Pprime(psi, psi50, b) &
                + log(0.5)*P(psi, psi50, b)/(psi50*psi50)*b*(b-1)* ((psi/psi50)**(b-2))
  end

  !!! Convert conductivity from m (m3/m2) to mol/m2/s/Mpa
  function scale_conductivity(K, par_env) result(K4)
    real (kind = dbl8), intent(in) :: K
    type(par_env_type), intent(in) :: par_env
    real (kind = dbl8) :: K2, K3, K4
    real (kind = dbl8) :: mol_h20_per_kg_h20 = 55.5

    ! Flow rate in m3/m2/s/Pa
    K2 = K/par_env%viscosity_water
  
    ! Flow rate in mol/m2/s/Pa
    K3 = K2 * par_env%density_water * mol_h20_per_kg_h20;
    
    ! Flow rate in mol/m2/s/Mpa
    K4 = K3 * 1e6;
  end function scale_conductivity
  

  !!! integrate vulnerability curve
  function integral_P_analytical(dpsi, psi_soil, psi50, b) result(I)
  ! int P(p, p50, b) = -(p/b) * (log2)^(-1/b) * G(1/b, (x/p)^b*log2)  <--- G is unnormalized upper incomplete gamma function
  !                  = -(p/b) * (log2)^(-1/b) * G(1/b) * (1 - I((x/p)^b*log2) <--- I is lower incomplete gamma integral
  !                  = -(p/b) * (log2)^(-1/b) * G(1/b) * (- I((pl/p)^b*log2 + I((ps/p)^b*log2) <--- I is lower incomplete gamma integral
  !                  = +(p/b) * (log2)^(-1/b) * G(1/b) * (  I((pl/p)^b*log2 - I((ps/p)^b*log2) <--- I is lower incomplete gamma integral
    real (kind = dbl8), intent(in) :: dpsi, psi_soil, psi50, b
    real (kind = dbl8) :: I, ps, pl, l2
    integer (kind = int4) :: ifault
    ps = psi_soil/psi50;
    pl = (psi_soil-dpsi)/psi50;
    l2 = log(2.0);
    I = (psi50/b) * (l2**(-1/b)) * gamma(1/b) * (gammad(l2*(pl**b), 1/b, ifault) - gammad(l2*(ps**b), 1/b, ifault))
  end


  function integral_P_approx(dpsi, psi_soil, psi50, b) result(I)
    real (kind = dbl8), intent(in) :: dpsi, psi_soil, psi50, b
    real (kind = dbl8) :: I
    I = -P(psi_soil-dpsi/2.0, psi50, b)*dpsi
  end


  function integral_P_approx2(dpsi, psi_soil, psi50, b) result(I)
    real (kind = dbl8), intent(in) :: dpsi, psi_soil, psi50, b
    real (kind = dbl8) :: I
    I = -(P(psi_soil, psi50, b)+P(psi_soil-dpsi, psi50, b))/2 * dpsi
  end


  function integral_P(dpsi, psi_soil, par_plant) result(I)
    real (kind = dbl8), intent(in) :: dpsi, psi_soil
    type(par_plant_type), intent(in) :: par_plant
    real (kind = dbl8) :: I

    ! if      (par_plant%gs_method == GS_QNG)  then; I = integral_P_numerical( dpsi, psi_soil, par_plant%psi50, par_plant%b);
    if      (par_plant%gs_method == GS_IGF)  then; I = integral_P_analytical(dpsi, psi_soil, par_plant%psi50, par_plant%b);
    else if (par_plant%gs_method == GS_APX)  then; I = integral_P_approx(    dpsi, psi_soil, par_plant%psi50, par_plant%b);
    else if (par_plant%gs_method == GS_APX2) then; I = integral_P_approx2(   dpsi, psi_soil, par_plant%psi50, par_plant%b);
    else; error stop "Unsupported gs_method specified"
    end if
  end

  !!! Transpiration and stomatal conductance
  !!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  function calc_sapflux(dpsi, psi_soil, par_plant, par_env) result(E)
    real (kind = dbl8), intent(in) :: dpsi, psi_soil
    type(par_plant_type), intent(in) :: par_plant
    type(par_env_type), intent(in) :: par_env
    real (kind = dbl8) :: E, K

    K = scale_conductivity(par_plant%conductivity, par_env)
    E = K * (-integral_P(dpsi, psi_soil, par_plant))
  end

  function calc_max_sapflux(psi_soil, par_plant, par_env) result(E)
    real (kind = dbl8), intent(in) :: psi_soil
    type(par_plant_type), intent(in) :: par_plant
    type(par_env_type), intent(in) :: par_env
    real (kind = dbl8) :: E, K
    
    K = scale_conductivity(par_plant%conductivity, par_env)
    E = K * (-integral_P(1e20_dbl8, psi_soil, par_plant))
  end


  !                                 _ps-dpsi 
  ! Calculate dpsi that solves    _/   K(psi') dpsi' = Q
  !                             ps
  function calc_dpsi_from_sapflux(Q, psi_soil, par_plant, par_env) result(dpsi)
    type(par_plant_type) :: par_plant
    type(par_env_type) :: par_env
    real(kind=dbl8) :: Q, psi_soil, dpsi, Qmax
    
    Qmax = calc_max_sapflux(psi_soil, par_plant, par_env);
    if (Q > Qmax) then
      dpsi = 999999999.0_dbl8
    else
      dpsi = zero(0.0_dbl8, 100.0_dbl8, f, 1e-6_dbl8)
    endif
  
    contains
    
      function f(dpsi) 
        real(kind=dbl8), intent(in) :: dpsi
        real(kind=dbl8) :: f      
        f = calc_sapflux(dpsi, psi_soil, par_plant, par_env) - Q;
      end function f

  end function calc_dpsi_from_sapflux
  

  ! Calculates regulated stomatal conductance given transpiration/sapflux
  ! water balance is assumed
  ! plant hydraulic traits, and the environment.
  function calc_gs_from_Q(Q, psi_soil, par_plant, par_env) result(gs)
    real(dbl8), intent(in) :: Q, psi_soil
    type(par_plant_type), intent(in) :: par_plant
    type(par_env_type), intent(in) :: par_env
    real(dbl8) :: D, gs, ga

    D = (par_env%vpd / par_env%patm)

    if (par_env%et_method == ET_DIFFUSION) then
        ! print *, "Using diffusion ET"
        gs = Q / (1.6d0 * D)
    else if (par_env%et_method == ET_PM) then
        ! print *, "Using PM ET"
        ga = calc_g_aero(par_plant%h_canopy, dble(par_env%v_wind), par_plant%h_wind_measurement)
        gs = calc_gs_pm(Q, ga, par_env)
    else
        write(*,*) 'Unknown et_method:', par_env%et_method
        stop
    end if
  end function calc_gs_from_Q

  ! Derivative of sapflux wrt dpsi, dQ/ddpsi
  function calc_Qprime_analytical(dpsi, psi_soil, par_plant, par_env) result(Qprime)
    real(dbl8), intent(in) :: dpsi, psi_soil
    type(par_plant_type), intent(in) :: par_plant
    type(par_env_type), intent(in) :: par_env
    real(dbl8) :: K
    real(dbl8) :: Qprime

    K = scale_conductivity(par_plant%conductivity, par_env)
    Qprime = K * P(psi_soil - dpsi, par_plant%psi50, par_plant%b)
  end function calc_Qprime_analytical


  function calc_Qprime_approx(dpsi, psi_soil, par_plant, par_env) result(Qprime)
    type(par_plant_type) :: par_plant
    type(par_env_type) :: par_env
    real(kind=dbl8) :: dpsi, psi_soil, Qprime, K

    K = scale_conductivity(par_plant%conductivity, par_env)
    Qprime = K * (P(psi_soil - dpsi / 2, par_plant%psi50, par_plant%b) - &
                  Pprime(psi_soil - dpsi / 2, par_plant%psi50, par_plant%b) * dpsi / 2)
  end function calc_Qprime_approx

  function calc_Qprime_approx2(dpsi, psi_soil, par_plant, par_env) result(Qprime)
    type(par_plant_type) :: par_plant
    type(par_env_type) :: par_env
    real(kind=dbl8) :: dpsi, psi_soil, Qprime, K

    K = scale_conductivity(par_plant%conductivity, par_env)
    Qprime = K * ((P(psi_soil, par_plant%psi50, par_plant%b) &
                 + P(psi_soil - dpsi, par_plant%psi50, par_plant%b)) / 2 &
                 - Pprime(psi_soil - dpsi, par_plant%psi50, par_plant%b) * dpsi / 2)
  end function calc_Qprime_approx2

  ! Derivative of sapflux wrt dpsi, dQ/ddpsi
  function calc_Qprime(dpsi, psi_soil, par_plant, par_env) result(Qprime)
    type(par_plant_type) :: par_plant
    type(par_env_type) :: par_env
    real(kind=dbl8) :: dpsi, psi_soil, Qprime

    if (par_env%gs_method == GS_APX) then
        Qprime = calc_Qprime_approx(dpsi, psi_soil, par_plant, par_env)
    else if (par_env%gs_method == GS_APX2) then
        Qprime = calc_Qprime_approx2(dpsi, psi_soil, par_plant, par_env)
    else if (par_env%gs_method == GS_IGF) then
        Qprime = calc_Qprime_analytical(dpsi, psi_soil, par_plant, par_env)
    ! else if (par_env%gs_method == GS_QNG) then
    !     Qprime = calc_Qprime_analytical(dpsi, psi_soil, par_plant, par_env)
    else
        write(*,*) "Unsupported gs_method specified"
        stop
    end if
  end function calc_Qprime

  function calc_dE_dgs_dif(par_env) result(dE_dgs)
    type(par_env_type) :: par_env
    real(kind=dbl8) :: dE_dgs, D

    D = dble(par_env%vpd) / dble(par_env%patm)
    dE_dgs = 1.6 * D
  end function calc_dE_dgs_dif

  function calc_dE_dgs_pm_from_gs(gs, par_plant, par_env) result(dE_dgs)
    type(par_plant_type) :: par_plant
    type(par_env_type) :: par_env
    real(kind=dbl8) :: gs, dE_dgs, ga

    ga = calc_g_aero(par_plant%h_canopy, dble(par_env%v_wind), par_plant%h_wind_measurement)
    dE_dgs = calc_dE_dgs_pm(gs, ga, par_env)
  end function calc_dE_dgs_pm_from_gs

  function calc_dE_dgs_pm_from_dpsi(dpsi, psi_soil, par_plant, par_env) result(dE_dgs)
    type(par_plant_type) :: par_plant
    type(par_env_type) :: par_env
    real(kind=dbl8) :: dpsi, psi_soil, dE_dgs, ga, Q, gs

    ga = calc_g_aero(par_plant%h_canopy, dble(par_env%v_wind), par_plant%h_wind_measurement)
    Q = calc_sapflux(dpsi, psi_soil, par_plant, par_env)
    gs = calc_gs_pm(Q, ga, par_env)
    dE_dgs = calc_dE_dgs_pm(gs, ga, par_env)
  end function calc_dE_dgs_pm_from_dpsi

  ! Derivative of E wrt gs
  function calc_dE_dgs_from_gs(gs, par_plant, par_env) result(dE_dgs)
    type(par_plant_type) :: par_plant
    type(par_env_type) :: par_env
    real(kind=dbl8) :: gs, dE_dgs

    if (par_env%et_method == ET_DIFFUSION) then
        dE_dgs = calc_dE_dgs_dif(par_env)
    else if (par_env%et_method == ET_PM) then
        dE_dgs = calc_dE_dgs_pm_from_gs(gs, par_plant, par_env)
    else
        write(*,*) "Unknown et_method:", par_env%et_method
        stop
    end if
  end function calc_dE_dgs_from_gs

  ! Derivative of E wrt gs
  function calc_dE_dgs_from_dpsi(dpsi, psi_soil, par_plant, par_env) result(dE_dgs)
    type(par_plant_type) :: par_plant
    type(par_env_type) :: par_env
    real(kind=dbl8) :: dpsi, psi_soil, dE_dgs

    if (par_env%et_method == ET_DIFFUSION) then
        dE_dgs = calc_dE_dgs_dif(par_env)
    else if (par_env%et_method == ET_PM) then
        dE_dgs = calc_dE_dgs_pm_from_dpsi(dpsi, psi_soil, par_plant, par_env)
    else
        write(*,*) "Unknown et_method:", par_env%et_method
        stop
    end if
  end function calc_dE_dgs_from_dpsi

  ! Derivative of gs wrt dpsi, dgs/ddpsi
  ! This version of the function avoids recomputation of gs when it is already known
  function calc_gsprime(dpsi, gs, psi_soil, par_plant, par_env) result(gsprime)
    type(par_plant_type) :: par_plant
    type(par_env_type) :: par_env
    real(kind=dbl8) :: dpsi, gs, psi_soil, gsprime, Qprime, Eprime

    Qprime = calc_Qprime(dpsi, psi_soil, par_plant, par_env)
    Eprime = calc_dE_dgs_from_gs(gs, par_plant, par_env)
    gsprime = Qprime / Eprime
  end function calc_gsprime

  ! Derivative of gs wrt dpsi, dgs/ddpsi
  ! This version is for use when gs is not known, and needs to be computed anyway
  function calc_gsprime_from_dpsi(dpsi, psi_soil, par_plant, par_env) result(gsprime)
    type(par_plant_type) :: par_plant
    type(par_env_type) :: par_env
    real(kind=dbl8) :: dpsi, psi_soil, gsprime, Qprime, Eprime

    Qprime = calc_Qprime(dpsi, psi_soil, par_plant, par_env)
    Eprime = calc_dE_dgs_from_dpsi(dpsi, psi_soil, par_plant, par_env)
    gsprime = Qprime / Eprime
  end function calc_gsprime_from_dpsi

  ! -------------------------------------------------------------
  ! Functions: Phydro photosynthesis
  !--------------------------------------------------------------
  subroutine create_par_photosynth(this, tc, patm, kphio, co2, ppfd, fapar, rdark25, tcgrowth, tchome, &
                                   ftemp_vj_method, ftemp_rd_method, ftemp_br_method)
    
    type(par_photosynth_type), intent(out) :: this
    real(kind = dbl8), intent(in) :: tc, patm, kphio, co2, ppfd, fapar, rdark25, tcgrowth, tchome
    integer(kind = int4), intent(in) :: ftemp_vj_method, ftemp_rd_method, ftemp_br_method

    ! Calculate temperature scaling factors
    this%fT_vcmax = calc_ftemp_inst_vcmax(real(tc), real(tcgrowth), 25.0, ftemp_vj_method)
    this%fT_jmax = calc_ftemp_inst_jmax(real(tc), real(tcgrowth), real(tchome), 25.0, ftemp_vj_method)
    this%fT_rd = calc_ftemp_inst_rd(real(tc), ftemp_rd_method)

    ! Calculate other parameters
    this%kmm = calc_kmm(real(tc), real(patm))
    this%gammastar = calc_gammastar(real(tc), real(patm))
    this%phi0 = kphio !* calc_kphio_temp(real(tc), .false.)
    this%Iabs = ppfd * fapar
    this%ca = co2 * patm * 1.0d-6
    this%patm = patm
    this%delta = rdark25 * this%fT_rd / this%fT_vcmax

    ! Set the temperature scaling methods
    this%ftemp_vj_method = ftemp_vj_method
    this%ftemp_rd_method = ftemp_rd_method
    this%ftemp_br_method = ftemp_br_method

  end subroutine create_par_photosynth

  subroutine print_par_photosynth(this)
    type(par_photosynth_type), intent(in) :: this

    print *, "ParPhotosynth: "
    print *, "   fT_vcmax", this%fT_vcmax
    print *, "   fT_jmax", this%fT_jmax
    print *, "   fT_rd", this%fT_rd
    print *, "   kmm", this%kmm
    print *, "   gammastar", this%gammastar
    print *, "   phi0", this%phi0
    print *, "   Iabs", this%Iabs
    print *, "   ca", this%ca
    print *, "   patm", this%patm
    print *, "   delta", this%delta
    print *, "   ftemp_vj_method", this%ftemp_vj_method
    print *, "   ftemp_rd_method", this%ftemp_rd_method
    print *, "   ftemp_br_method", this%ftemp_br_method
  end subroutine print_par_photosynth

  function calc_ftemp_arrhenius(tk, dha, tkref) result(ftemp)
    ! Output:   Factor fv to correct for instantaneous temperature response
    !           of Vcmax for:
    !
    !               Vcmax(temp) = fv * Vcmax(25 deg C) 
    !
    ! Input:
    !   tk      - Leaf temperature in Kelvin
    !   dha     - Activation energy (J/mol)
    !   tkref   - Reference temperature in Kelvin (default: 298.15 K)

    real(kind = dbl8), intent(in) :: tk, dha, tkref
    real(kind = dbl8), parameter :: kR = 8.3145 ! Universal gas constant, J/mol/K
    real(kind = dbl8) :: ftemp

    ! Calculate temperature scaling factor using Arrhenius equation
    ftemp = exp(dha * (tk - tkref) / (tkref * kR * tk))

  end function calc_ftemp_arrhenius


  function calc_ftemp_inst_vcmax(tcleaf, tcgrowth, tcref, method_ftemp) result(fv)
    real(kind = flt4), intent(in) :: tcleaf, tcgrowth, tcref
    real(kind = dbl8) :: fv 
    integer(kind = int4), intent(in) :: method_ftemp
    real(kind = dbl8), parameter :: Rgas = 8.3145 ! Universal gas constant (J/mol/K)
    real(kind = dbl8) :: tkref  
    real(kind = dbl8) :: tkleaf 
    real(kind = dbl8) :: Hd, Ha, a_ent, b_ent, dent, fva, fvb
    real(kind = dbl8) :: Sv, term_1, term_2, term_3

    tkref = tcref + 273.15 ! Convert reference temperature to Kelvin
    tkleaf = tcleaf + 273.15 ! Convert leaf temperature to Kelvin

    if (method_ftemp == FV_kattge07 .or. method_ftemp == FV_kumarathunge19) then
        ! Kattge2007 Parametrization
        Hd = 200000.0 ! Deactivation energy (J/mol)
        Ha = 71513.0 ! Activation energy (J/mol)
        a_ent = 668.39 ! Offset of entropy vs. temperature relationship from Kattge & Knorr (2007) (J/mol/K)
        b_ent = 1.07 ! Slope of entropy vs. temperature relationship from Kattge & Knorr (2007) (J/mol/K^2)

        if (method_ftemp == FV_kumarathunge19) then
            ! Kumarathunge2019 Implementation:
            ! local parameters
            a_ent = 645.13 ! Offset of entropy vs. temperature relationship (J/mol/K)
            b_ent = 0.38 ! Slope of entropy vs. temperature relationship (J/mol/K^2)
            
            ! local variables
            Ha = 42600.0 + (1140.0 * tcgrowth) ! Acclimation for vcmax
        end if

        ! Calculate entropy following Kattge & Knorr (2007), negative slope and y-axis intersect is when expressed as a function of temperature in degrees Celsius, not Kelvin!
        dent = a_ent - (b_ent * tcgrowth)  ! 'tcgrowth' corresponds to 'tmean' in Nicks, 'tc25' is 'to' in Nick's

        fva = calc_ftemp_arrhenius(tkleaf, Ha, tkref)
        fvb = (1.0 + exp((tkref * dent - Hd) / (Rgas * tkref))) / (1.0 + exp((tkleaf * dent - Hd) / (Rgas * tkleaf)))
        fv = fva * fvb
    elseif (method_ftemp == FV_leuning02) then
        ! Ref: Leuning, R. (2002). Temperature dependence of two parameters in a photosynthesis model. Plant, Cell & Environment, 25(9), 1205–1210. https://doi.org/10.1046/j.1365-3040.2002.00898.x
        ! Table 2:
        Ha = 73637.0
        Hd = 149252.0
        Sv = 486.0

        term_1 = 1.0 + exp((Sv * tkref - Hd) / (Rgas * tkref))
        term_3 = 1.0 + exp((Sv * tkleaf - Hd) / (Rgas * tkleaf))
        term_2 = exp((Ha / (Rgas * tkref)) * (1.0 - tkref / tkleaf)) ! Careful: In Eq. (1) in Leuning et al. (1992), there is a bracket missing in this term!

        fv = term_1 * term_2 / term_3
    else
        write(*,*) "Invalid method_ftemp:", method_ftemp
        stop
    end if
  end function calc_ftemp_inst_vcmax


  function calc_ftemp_inst_jmax(tcleaf, tcgrowth, tchome, tcref, method_ftemp) result(fv)
    real(kind = flt4), intent(in) :: tcleaf, tcgrowth, tchome, tcref
    integer(kind = int4), intent(in) :: method_ftemp

    real(kind = dbl8), parameter :: Rgas = 8.3145 ! Universal gas constant (J/mol/K)
    real(kind = dbl8) :: tkref ! Convert reference temperature to Kelvin
    real(kind = dbl8) :: tkleaf ! Convert leaf temperature to Kelvin
    real(kind = dbl8) :: fv

    real(kind = dbl8) :: Hd ! Deactivation energy (J/mol)
    real(kind = dbl8) :: Ha ! Activation energy (J/mol)
    real(kind = dbl8) :: a_ent ! Offset of entropy vs. temperature relationship from Kattge & Knorr (2007) (J/mol/K)
    real(kind = dbl8) :: b_ent ! Slope of entropy vs. temperature relationship from Kattge & Knorr (2007) (J/mol/K^2)
    real(kind = dbl8) :: c_ent
    real(kind = dbl8) :: dent ! Entropy calculation, equations given in Celsius, not in Kelvin
    real(kind = dbl8) :: fva
    real(kind = dbl8) :: fvb

    real(kind = dbl8) :: Sv, term_1, term_2, term_3

    tkref = tcref + 273.15
    tkleaf = tcleaf + 273.15

    if (method_ftemp == FV_kattge07 .or. method_ftemp == FV_kumarathunge19) then
      Hd = 200000.0
      Ha = 49884.0
      a_ent = 659.70
      b_ent = 0.75

      dent = a_ent - b_ent * tcgrowth

      if (method_ftemp == FV_kumarathunge19) then
        Ha = 40710.0
        a_ent = 658.77
        b_ent = 0.84
        c_ent = 0.52

        dent = a_ent - (b_ent * tchome) - c_ent * (tcgrowth - tchome)
      end if

      fva = calc_ftemp_arrhenius(tkleaf, Ha, tkref)
      fvb = (1.0 + exp((tkref * dent - Hd) / (Rgas * tkref))) / (1.0 + exp((tkleaf * dent - Hd) / (Rgas * tkleaf)))
      fv = fva * fvb

    elseif (method_ftemp == FV_leuning02) then
      Ha = 50300.0
      Hd = 152044.0
      Sv = 495.0

      term_1 = 1.0 + exp((Sv * tkref - Hd) / (Rgas * tkref))
      term_3 = 1.0 + exp((Sv * tkleaf - Hd) / (Rgas * tkleaf))
      term_2 = exp((Ha / (Rgas * tkref)) * (1.0 - tkref / tkleaf))

      fv = term_1 * term_2 / term_3

    else
        write(*,*) "Invalid method_ftemp:", method_ftemp
        stop
    end if

  end function calc_ftemp_inst_jmax


  function calc_ftemp_inst_rd(tc_leaf, method_rd_scale) result(f)
    real(kind = dbl8) :: f
    real(kind = flt4), intent(in) :: tc_leaf
    integer(kind=int4), intent(in) :: method_rd_scale
    real(kind = dbl8) :: apar, bpar, dha

    if (method_rd_scale == FR_heskel16) then
      ! Heskel et al. (2016) temperature scaling
      apar = 0.1012
      bpar = 0.0005
      f = exp(apar * (tc_leaf - 25.0) - bpar * (tc_leaf*tc_leaf - 25.0*25.0))
    elseif (method_rd_scale == FR_arrhenius) then
      ! Arrhenius temperature scaling
      dha = 20700.0 ! Activation energy taken from Kumarathunge et al. (2019), Table 1, Mature Natural Environment
      f = calc_ftemp_arrhenius(dble(tc_leaf) + 273.15, dha, 298.15_dbl8) ! Convert temperature to Kelvin and call calc_ftemp_arrh function
    elseif (method_rd_scale == FR_q10) then
      ! Q10 temperature scaling according to Tjoelker et al. (2001)
      f = (3.22 - 0.046 * tc_leaf)**(tc_leaf - 25.0) / 10.0
    else
      write(*,*) "Invalid method_rd_scale:", method_rd_scale
      stop
    end if

  end function calc_ftemp_inst_rd


  function calc_brd25(method_rd25, tc_growth) result(rd_to_vcmax)
    real(kind = dbl8) :: rd_to_vcmax
    real(kind = dbl8), intent(in) :: tc_growth
    integer(kind = int4), intent(in) :: method_rd25

    if (method_rd25 == FB_atkin15) then
      rd_to_vcmax = 0.015 ! Ratio of Rdark to Vcmax25, Atkin et al., 2015 for C3 herbaceous
    elseif (method_rd25 == FB_kumarathunge19) then
      rd_to_vcmax = 0.0360 - 0.0010 * tc_growth ! Acclimated rd_to_vcmax taken from Kumarathunge et al. (2019), Table 1, Mature Natural Environment
    else
      write(*,*) "Invalid method_rd25:", method_rd25
      stop
    end if

  end function calc_brd25


  !-------------------------------------------------------
  !   Ac / Aj calculations
  !-------------------------------------------------------
  function QUADM(A, B, C)
    real(kind=dbl8) :: QUADM
    real(kind=dbl8), intent(in) :: A, B, C
    if (A == 0) then
      QUADM = -C/B
    else 
      QUADM = (-B - sqrt(B*B - 4.0d0*A*C)) / (2.0d0*A)
    end if
  end function QUADM

  function QUADP(A, B, C)
    real(kind=dbl8) :: QUADP
    real(kind=dbl8), intent(in) :: A, B, C
    QUADP = (-B + sqrt(B*B - 4.0d0*A*C)) / (2.0d0*A)
  end function QUADP


  function calc_assim_rubisco_limited(gs_in, vcmax, par_photosynth) result(res)
    real(kind=dbl8), intent(in) :: gs_in
    real(kind=dbl8), intent(in) :: vcmax
    type(ACi_type) :: res
    type(par_photosynth_type) :: par_photosynth
    real(kind=dbl8) :: ca, d, A, B, C, gs

    gs = gs_in

    ca = par_photosynth%ca
    gs = gs * 1.0d6 / par_photosynth%patm
    d = par_photosynth%delta

    A = -gs
    B = gs * ca - gs * par_photosynth%kmm - vcmax*(1.0d0-d)
    C = gs * ca * par_photosynth%kmm + vcmax * (par_photosynth%gammastar + par_photosynth%kmm*d)

    res%ci = QUADM(A, B, C)
    res%a = gs * (ca - res%ci)
    res%isVcmaxLimited = .true.

  end function calc_assim_rubisco_limited


  function calc_assim_light_limited(gs_in, jmax, par_photosynth) result(res)
    real(kind=dbl8), intent(in) :: gs_in
    real(kind=dbl8), intent(in) :: jmax
    type(ACi_type) :: res
    type(par_photosynth_type) :: par_photosynth
    real(kind=dbl8) :: ca, d, phi0iabs, jj, jlim, A, B, C, gs

    gs = gs_in

    ca = par_photosynth%ca
    gs = gs * 1.0d6 / par_photosynth%patm
    gs = gs !+ 1.0d-12
    d = par_photosynth%delta

    phi0iabs = par_photosynth%phi0 * par_photosynth%Iabs
    jj = 4.0d0 * phi0iabs / jmax
    jlim = phi0iabs / sqrt(1.0d0 + jj*jj)

    A = -gs
    B = gs * ca - gs * 2.0d0 * par_photosynth%gammastar - jlim * (1.0d0-d)
    C = gs * ca * 2.0d0 * par_photosynth%gammastar + jlim * (par_photosynth%gammastar + d*par_photosynth%kmm)

    res%ci = QUADM(A, B, C)
    res%a = gs * (ca - res%ci)
    res%isVcmaxLimited = .false.

  end function calc_assim_light_limited


  function calc_assimilation_limiting(vcmax, jmax, gs, par_photosynth) result(Aout)
    real(kind=dbl8), intent(in) :: vcmax, jmax
    real(kind=dbl8), intent(in) :: gs
    type(ACi_type) :: Ac, Aj, Aout
    type(par_photosynth_type) :: par_photosynth

    Ac = calc_assim_rubisco_limited(gs, vcmax, par_photosynth)
    Aj = calc_assim_light_limited(gs, jmax, par_photosynth)

    if (Ac%ci > Aj%ci) then
      Aout = Ac
    else
      Aout = Aj
    end if
  end function calc_assimilation_limiting  

  ! -------------------------------------------------------------
  ! Functions: Phydro solver
  !--------------------------------------------------------------

  !!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !!! Phydro analytical solver (acclimating)
  !!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  function calc_J(gs, x, par_photosynth) result(J)
    real (kind = dbl8), intent(in) :: gs, x
    type(par_photosynth_type), intent(in) :: par_photosynth
    real (kind = dbl8) :: g, K, ca, d, J
    g = par_photosynth%gammastar / par_photosynth%ca
    k = par_photosynth%kmm / par_photosynth%ca
    ca = par_photosynth%ca / par_photosynth%patm*1e6
    d = par_photosynth%delta
    J = 4*gs*ca*(1-x)*(x+ 2*g)/(x*(1-d)-(g+d*k))
  end


  function calc_jmax_from_J(J, par_photosynth) result(jmax)
    real (kind = dbl8), intent(in) :: J
    type(par_photosynth_type), intent(in) :: par_photosynth
    real (kind = dbl8) :: pp, pj, jmax
    pp = 4*par_photosynth%phi0 * par_photosynth%Iabs;
    pj = pp/J;
    jmax = pp/sqrt(pj*pj-1);
  end


  function calc_djmax_dJ(J, par_photosynth) result(djdj)
    real (kind = dbl8), intent(in) :: J
    type(par_photosynth_type), intent(in) :: par_photosynth
    real (kind = dbl8) :: pp, sq, psq, djdj
    pp = 4*par_photosynth%phi0 * par_photosynth%Iabs
    sq = sqrt(pp*pp-J*J)
    psq = pp/sq
    djdj = psq*psq*psq
  end


  function calc_dJ_dchi(gs, x, par_photosynth) result(djdx)
    real (kind = dbl8), intent(in) :: gs, x
    type(par_photosynth_type), intent(in) :: par_photosynth
    real (kind = dbl8) :: g, K, ca, d, djdx, d1
    g = par_photosynth%gammastar / par_photosynth%ca
    k = par_photosynth%kmm / par_photosynth%ca
    ca = par_photosynth%ca / par_photosynth%patm*1e6
    d = par_photosynth%delta
    ! gs*ca * ((d*(2*g*(k + 1) + k*(2*x - 1) + x^2) + 2*g^2 + g*(2*x - 3) - x^2)/(d*(k + x) + g - x)^2)
    d1 = d*(k + x) + g - x;
    djdx = 4*gs*ca * ((d*(2*g*(k + 1) + k*(2*x - 1) + x*x) - ((x-g)*(x-g)+3*g*(1-g)))/(d1*d1));
    ! gs*ca*(3*(g-1)*g/(g-x)^2 - 1)
  end


  function calc_dJ_ddpsi(gsprime, x, par_photosynth) result(djdp)
    real (kind = dbl8), intent(in) :: gsprime, x
    type(par_photosynth_type), intent(in) :: par_photosynth
    real (kind = dbl8) :: g, K, ca, d, djdp
    g = par_photosynth%gammastar / par_photosynth%ca
    k = par_photosynth%kmm / par_photosynth%ca
    ca = par_photosynth%ca / par_photosynth%patm*1e6
    d = par_photosynth%delta
    djdp = 4*gsprime*ca*(1-x)*(x+2*g)/(x*(1-d)-(g+d*k))
  end


  function calc_x_from_dpsi(dpsi, gsprime, par_photosynth, par_cost) result(x)
    real (kind = dbl8), intent(in) :: dpsi, gsprime
    type(par_photosynth_type), intent(in) :: par_photosynth
    type(par_cost_type), intent(in) :: par_cost

    real (kind = dbl8) gstar, Km, ca, br, y, ca2, x

    gstar = par_photosynth%gammastar/par_photosynth%patm*1e6
    Km = par_photosynth%kmm/par_photosynth%patm*1e6
    ca = par_photosynth%ca/par_photosynth%patm*1e6
    br = par_photosynth%delta
    y = par_cost%gamma
    
    ca2 = ca*ca;
    x = (-2*ca*dpsi*(gstar + br*Km)*y + &
      ca2*((3 - 2*br)*gstar + br*Km)*gsprime + &
      -sqrt(2.0D+00)*sqrt( &
        ca2*dpsi*((-3 + 2*br)*gstar - br*Km)*((-1 + br)*ca + gstar + &
                                                  br*Km)*y* &
          (-2*dpsi*y + (ca + 2*gstar)* &
              gsprime)))/ &
      (ca2*(2*(-1 + br)*dpsi*y + ((3 - 2*br)*gstar + br*Km)* &
              gsprime))
    
    if (x < (gstar + br*Km)/(ca - br*ca)) x = (gstar + br*Km)/(ca - br*ca)+1e-12
  end
  

  function dFdx(dpsi, psi_soil, par_plant, par_env, par_photosynth, par_cost) result(res)
    real (kind = dbl8), intent(in) :: dpsi, psi_soil
    type(par_plant_type), intent(in) :: par_plant
    type(par_env_type), intent(in) :: par_env
    type(par_photosynth_type), intent(in) :: par_photosynth
    type(par_cost_type), intent(in) :: par_cost

    real (kind = dbl8) :: Q, gs, gsprime, X, J, ca, g, djmax_dJ, dJ_dchi, dP_dx
    type(dfdx_type) :: res

    Q = calc_sapflux(dpsi, psi_soil, par_plant, par_env)
    gs = calc_gs_from_Q(Q, psi_soil, par_plant, par_env)
    gsprime = calc_gsprime(dpsi, gs, psi_soil, par_plant, par_env)
  
    X =  calc_x_from_dpsi(dpsi, gsprime, par_photosynth, par_cost)

    J = calc_J(gs, X, par_photosynth)

    ca = par_photosynth%ca / par_photosynth%patm*1e6
    g = par_photosynth%gammastar / par_photosynth%ca

    djmax_dJ = calc_djmax_dJ(J, par_photosynth)
    dJ_dchi  = calc_dJ_dchi(gs, X, par_photosynth)

    dP_dx = -gs*ca - par_cost%alpha * djmax_dJ * dJ_dchi

    res = dfdx_type(dP_dx, J, djmax_dJ, dJ_dchi)
  end


  function calc_dpsi_bound(psi_soil, par_plant, par_env, par_photosynth, par_cost) result(bounds)
    real (kind = dbl8), intent(in) :: psi_soil
    type(par_plant_type), intent(in) :: par_plant
    type(par_env_type), intent(in) :: par_env
    type(par_photosynth_type), intent(in) :: par_photosynth
    type(par_cost_type), intent(in) :: par_cost

    type(dpsi_bounds_type) :: bounds 

    real (kind = dbl8) :: gstar, ca, y, K, Pox, Ppox, Pppox
    real (kind = dbl8) :: a,b,c,del
    real (kind = dbl8) :: ex, appo2, iabsb, use_bound
    real (kind = dbl8) :: ga, Qmax, max_dpsi

    gstar = par_photosynth%gammastar/par_photosynth%patm*1e6
    ca = par_photosynth%ca/par_photosynth%patm*1e6
    y = par_cost%gamma

    K = scale_conductivity(par_plant%conductivity, par_env)/(1.6*par_env%vpd/par_env%patm);
 
    Pox = P(psi_soil, par_plant%psi50, par_plant%b);
    Ppox = Pprime(psi_soil, par_plant%psi50, par_plant%b);
    Pppox = Pprimeprime(psi_soil, par_plant%psi50, par_plant%b);
    
    a = (ca + 2*gstar)*K*Pppox*4.0d0/8.0d0;
    b = -(2*y + (ca + 2*gstar)*K*Ppox);
    c = (ca + 2*gstar)*K*Pox;
    del = b*b-4*a*c;

    appo2 = (-b-sqrt(del))/(2*a)
    ex = zero(0.0d0, 10.0d0, f2, 1d-6)

    use_bound = ex
    
    iabsb = zero(use_bound*0.001, use_bound*0.99, f1, 1D-6);
       
    ! If using PM, find max dpsi from max possible transpiration 
    if (par_env%et_method == ET_PM) then
      ga = calc_g_aero(par_plant%h_canopy, dble(par_env%v_wind), par_plant%h_wind_measurement);
      Qmax = calc_max_transpiration_pm(ga, par_env);
      max_dpsi = calc_dpsi_from_sapflux(Qmax, psi_soil, par_plant, par_env);
      iabsb = min(max_dpsi, iabsb);
    endif
  

    bounds = dpsi_bounds_type(ex, appo2, iabsb)

    contains

    function f2(dpsi) result(gg)
      real(kind = dbl8), intent(in) :: dpsi
      real(kind = dbl8) :: gg, gsprime
      gsprime = calc_gsprime_from_dpsi(dpsi, psi_soil, par_plant, par_env)
      gg = (-2*dpsi*y + (ca + 2*gstar)*gsprime)
    end

    function f1(dpsi) result(J)
      real(kind = dbl8), intent(in) :: dpsi
      real(kind = dbl8) :: J, gs, x, Q, gsprime
      Q = calc_sapflux(dpsi, psi_soil, par_plant, par_env);
      gs = calc_gs_from_Q(Q, psi_soil, par_plant, par_env);
      gsprime = calc_gsprime(dpsi, gs, psi_soil, par_plant, par_env);
      x = calc_x_from_dpsi(dpsi,gsprime, par_photosynth, par_cost);
      J = calc_J(gs, x, par_photosynth)-4.0d0*par_photosynth%phi0*par_photosynth%Iabs;
    end

  end



  !!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !!! Phydro analytical solver (instantaneous)
  !!! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  function calc_dP_ddpsi(dpsi, vcmax, jmax, psi_soil, par_plant, par_env, par_photosynth, par_cost) result(dP_ddpsi)
    real(kind=dbl8), intent(in) :: dpsi, vcmax, jmax, psi_soil
    type(par_plant_type), intent(in) :: par_plant
    type(par_env_type), intent(in) :: par_env
    type(par_photosynth_type), intent(in) :: par_photosynth
    type(par_cost_type), intent(in) :: par_cost
    real(kind=dbl8) :: gstar, Km, ca, br, y, Q, gs, P, dpsi1, Q1, gs1, P1
    type(ACi_type) :: Assim, Assim1
    real(kind=dbl8) :: dP_ddpsi
  
    gstar = par_photosynth%gammastar / par_photosynth%patm * 1.0d6
    Km = par_photosynth%kmm / par_photosynth%patm * 1.0d6
    ca = par_photosynth%ca / par_photosynth%patm * 1.0d6
    br = par_photosynth%delta
    y = par_cost%gamma

    Q = calc_sapflux(dpsi, psi_soil, par_plant, par_env)
    gs = calc_gs_from_Q(Q, psi_soil, par_plant, par_env)
    Assim = calc_assimilation_limiting(vcmax, jmax, gs, par_photosynth)
    P = Assim%a - y * dpsi * dpsi
  
    dpsi1 = dpsi + 1.0d-6
    Q1 = calc_sapflux(dpsi1, psi_soil, par_plant, par_env)
    gs1 = calc_gs_from_Q(Q1, psi_soil, par_plant, par_env)
    Assim1 = calc_assimilation_limiting(vcmax, jmax, gs1, par_photosynth)
    P1 = Assim1%a - y * (dpsi1) * (dpsi1)

    ! print *, "dpsi = ", dpsi
    ! print *, "P ", dpsi, Q, gs, Assim%a
    ! print *, "P1", dpsi, Q1, gs1, Assim1%a

    dP_ddpsi = (P1 - P) / 1.0d-6
  
  end function calc_dP_ddpsi
  

  function calc_dpsi_bound_inst(psi_soil, par_plant, par_env, par_photosynth, par_cost) result(bound)
    real(kind=dbl8), intent(in) :: psi_soil
    type(par_plant_type), intent(in) :: par_plant
    type(par_env_type), intent(in) :: par_env
    type(par_photosynth_type), intent(in) :: par_photosynth
    type(par_cost_type), intent(in) :: par_cost
    real(kind=dbl8) :: bound, ga, Qmax, max_dpsi
  
    bound = 100.0d0
  
    ! If using PM, find max dpsi from max possible transpiration 
    if (par_env%et_method == ET_PM) then
      ga = calc_g_aero(par_plant%h_canopy, dble(par_env%v_wind), par_plant%h_wind_measurement)
      Qmax = calc_max_transpiration_pm(ga, par_env)
      max_dpsi = calc_dpsi_from_sapflux(Qmax, psi_soil, par_plant, par_env)
      bound = min(max_dpsi, bound)
    end if
  
  end function calc_dpsi_bound_inst


  ! -------------------------------------------------------------
  ! Functions: Phydro main
  !--------------------------------------------------------------
  function phydro_analytical(tc, tg, ppfd, netrad, vpd, co2, elv, fapar, kphio, psi_soil, rdark, vwind, &
                             par_plant, par_cost, par_control) result(res)
    real(kind=dbl8), intent(in) :: tc, tg, ppfd, netrad, vpd, co2, elv, fapar, kphio, psi_soil, rdark, vwind
    type(par_plant_type), intent(in) :: par_plant
    type(par_cost_type), intent(inout) :: par_cost
    type(par_control_type), intent(in) :: par_control
    type(par_env_type) :: par_env
    type(par_photosynth_type) :: par_photosynth
    type(phydro_result_type) :: res

    real(kind=dbl8) :: pa, e, gs, gsprime, x, J, jmax, vcmax, a, dpsi_opt
    type(dpsi_bounds_type) :: bounds
  
    pa = calc_patm(real(elv))
    call create_par_photosynth(par_photosynth, tc, pa, kphio, co2, ppfd, fapar, rdark, tg, par_plant%tchome, &
                               par_control%ftemp_vj_method, par_control%ftemp_rd_method, par_control%ftemp_br_method)
    call create_par_env(par_env, tc, pa, vpd, netrad, vwind)
    
    if (par_control%scale_alpha > 0) par_cost%alpha = par_cost%alpha / par_photosynth%fT_jmax  ! Convert alpha from cost of jmax to cost of jmax25
    par_env%gs_method = par_control%gs_method
    par_env%et_method = par_control%et_method
    
    bounds = calc_dpsi_bound(dble(psi_soil), par_plant, par_env, par_photosynth, par_cost)
    dpsi_opt = zero(bounds%Iabs_bound * 0.001, bounds%Iabs_bound * 0.999, profit_fun, 1.0d-6)
    
    e = calc_sapflux(dpsi_opt, dble(psi_soil), par_plant, par_env)
    gs = calc_gs_from_Q(e, dble(psi_soil), par_plant, par_env)
    gsprime = calc_gsprime(dpsi_opt, gs, dble(psi_soil), par_plant, par_env)
    x = calc_x_from_dpsi(dpsi_opt, gsprime, par_photosynth, par_cost)
    J = calc_J(gs, x, par_photosynth)
    jmax = calc_jmax_from_J(J, par_photosynth)
    vcmax = (J / 4.0d0) * (x * par_photosynth%ca + par_photosynth%kmm) / (x * par_photosynth%ca + 2.0d0 * par_photosynth%gammastar)
    a = gs * (par_photosynth%ca / par_photosynth%patm * 1.0d6) * (1.0d0 - x)
    
    res%a = a
    res%e = e
    res%ci = x * par_photosynth%ca
    res%gs = gs
    res%chi = x
    res%vcmax = vcmax
    res%jmax = jmax
    res%dpsi = dpsi_opt
    res%psi_l = psi_soil - dpsi_opt
    res%nfnct = -999
    res%mc = (x * par_photosynth%ca - par_photosynth%gammastar) / (x * par_photosynth%ca + par_photosynth%kmm)
    res%mj = (x * par_photosynth%ca - par_photosynth%gammastar) / (x * par_photosynth%ca + 2.0d0 * par_photosynth%gammastar)
    res%gammastar = par_photosynth%gammastar
    res%kmm = par_photosynth%kmm
    res%vcmax25 = vcmax / par_photosynth%fT_vcmax
    res%jmax25 = jmax / par_photosynth%fT_jmax
    res%rd = vcmax * par_photosynth%delta
    res%isVcmaxLimited = 0.5d0
    res%ac = a
    res%aj = a
    res%le = e * 0.018015d0 * par_env%lv
    res%le_s_wet = (1.0d0 - fapar) * netrad * (par_env%epsilon / (1.0d0 + par_env%epsilon))
  
    contains

    function profit_fun(dpsi)
      real(kind = dbl8), intent(in) :: dpsi
      real(kind = dbl8) :: profit_fun
      type(dfdx_type) :: dfdx_res
      dfdx_res = dFdx(dpsi, dble(psi_soil), par_plant, par_env, par_photosynth, par_cost)
      profit_fun = dfdx_res%dPdx
    end  

  end function phydro_analytical
  
  function phydro_instantaneous_analytical(vcmax25, jmax25, tc, tg, ppfd, netrad, vpd, co2, elv, &
                                           fapar, kphio, psi_soil, rdark, vwind, par_plant, par_cost, par_control) result(res)
    real(kind=dbl8), intent(in) :: vcmax25, jmax25, tc, tg, ppfd, netrad, vpd, co2, elv, fapar, kphio, psi_soil, rdark, vwind
    type(par_plant_type), intent(in) :: par_plant
    type(par_cost_type), intent(inout) :: par_cost
    type(par_control_type), intent(in) :: par_control
    type(par_env_type) :: par_env
    type(par_photosynth_type) :: par_photosynth
    type(phydro_result_type) :: res
    real(kind=dbl8) :: pa, e, gs
    real(kind=dbl8) :: bound, jmax, vcmax, dpsi_opt
    type(ACi_type) :: Aa, Ac, Aj
    
    pa = calc_patm(real(elv))
    call create_par_photosynth(par_photosynth, tc, pa, kphio, co2, ppfd, fapar, rdark, tg, par_plant%tchome, &
                               par_control%ftemp_vj_method, par_control%ftemp_rd_method, par_control%ftemp_br_method)
    call create_par_env(par_env, tc, pa, vpd, netrad, vwind)
    
    ! call print_par_photosynth(par_photosynth)

    ! optionally convert alpha from cost of jmax to cost of jmax25
    if (par_control%scale_alpha > 0) par_cost%alpha = par_cost%alpha / par_photosynth%fT_jmax  !
    par_env%gs_method = par_control%gs_method
    par_env%et_method = par_control%et_method
    
    vcmax = vcmax25 * par_photosynth%fT_vcmax
    jmax = jmax25 * par_photosynth%fT_jmax
    
    bound = calc_dpsi_bound_inst(psi_soil, par_plant, par_env, par_photosynth, par_cost)
    dpsi_opt = zero(0.0d0, 0.99d0 * bound, profit_fun_inst, 1.0d-6)
    
    e = calc_sapflux(dpsi_opt, psi_soil, par_plant, par_env)
    gs = calc_gs_from_Q(e, psi_soil, par_plant, par_env)
    Aa = calc_assimilation_limiting(vcmax, jmax, gs, par_photosynth)
    Ac = calc_assim_rubisco_limited(gs, vcmax, par_photosynth)
    Aj = calc_assim_light_limited(gs, jmax, par_photosynth)
    
    res%a = Aa%a
    res%e = e
    res%ci = Aa%ci
    res%gs = gs
    res%chi = Aa%ci / par_photosynth%ca
    res%vcmax = vcmax
    res%jmax = jmax
    res%dpsi = dpsi_opt
    res%psi_l = psi_soil - dpsi_opt
    res%mc = (Aa%ci - par_photosynth%gammastar) / (Aa%ci + par_photosynth%kmm)
    res%mj = (Aa%ci - par_photosynth%gammastar) / (Aa%ci + 2.0d0 * par_photosynth%gammastar)
    res%gammastar = par_photosynth%gammastar
    res%kmm = par_photosynth%kmm
    res%vcmax25 = vcmax25
    res%jmax25 = jmax25
    res%rd = vcmax * par_photosynth%delta
    res%isVcmaxLimited = merge(1.d0, 0.d0, Aa%isVcmaxLimited)
    res%ac = Ac%a
    res%aj = Aj%a
    res%le = e * 0.018015d0 * par_env%lv
    res%le_s_wet = (1.0d0 - fapar) * netrad * (par_env%epsilon / (1.0d0 + par_env%epsilon))
  
    contains

    function profit_fun_inst(dpsi)
      real(kind = dbl8), intent(in) :: dpsi
      real(kind = dbl8) :: profit_fun_inst
      profit_fun_inst = calc_dP_ddpsi(dpsi, vcmax, jmax, psi_soil, par_plant, par_env, par_photosynth, par_cost)
    end  


  end function phydro_instantaneous_analytical


end module md_photosynth_phydro
