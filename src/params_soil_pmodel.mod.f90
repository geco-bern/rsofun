module md_params_soil_pmodel
  !////////////////////////////////////////////////////////////////
  ! Module handling soil parameters
  !----------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, mydb=>real64, mysg=>real32
  use md_params_core_pmodel, only: nlayers_soil

  implicit none

  private
  public paramtype_soil, getsoil

  type paramtype_soil
    real(kind=mysg) :: fsand
    real(kind=mysg) :: fclay
    real(kind=mysg) :: forg
    real(kind=mysg) :: fgravel
    real(kind=mysg) :: fc
    real(kind=mysg) :: pwp
    real(kind=mysg) :: whc_dz
    real(kind=mysg) :: whc
    real(kind=mysg) :: ksat
    real(kind=mysg) :: thdiff_wp
    real(kind=mysg) :: thdiff_whc15
    real(kind=mysg) :: thdiff_fc
  end type

contains

  function getsoil( soiltexture ) result( params_soil )
    !////////////////////////////////////////////////////////////////
    ! Function to calculate soil parameters from texture info.
    !----------------------------------------------------------------
    ! arguments
    real(kind=mydb), dimension(4,nlayers_soil), intent(in) :: soiltexture   ! soil texture (rows: fractions of sand, clay, organic, gravel; columns: top, bottom)

    ! function return variable
    type(paramtype_soil) :: params_soil

    ! ADOPTED FROM David Sandoval (https://github.com/dsval/rsplash/blob/master/R/splash.point.R)
    ! ************************************************************************
    ! Name:     soil_hydro
    ! Input:    - float, fsand, (percent)
    !           - float, fclay, (percent)
    !           - float, OM Organic Matter (percent)
    !           - float,fgravel, (percent-volumetric)
    ! Output:   list:
    !           - float, FC, (volumetric fraction)
    !           - float, WP (volumetric fraction)
    !           - float,SAT, (volumetric fraction)
    !           - float, AWC (volumetric fraction)
    !           - float,Ksat, Saturate hydraulic conductivity/infiltration capacity(mm/hr)
    !           - float, A (Coefficient)
    !           - float, B (Clapp and Hornberger (1978) pore-size distribution index)
    ! Features: calculate some soil hydrophysic characteristics
    ! Ref:      Balland et al. 
    ! ************************************************************************

    ! local variables
    real(kind=mysg) :: fsand, fclay, forg, fgravel, fsand_forg, fclay_forg, &
      fsand_fclay, dp, bd, sat, fc, pwp, L_10_Ksat, ksat, whc_dz
    integer idx

    ! from David's code
    real(kind=mysg), parameter :: depth    =  30.0
    real(kind=mysg), parameter :: topsoil  =  1.0

    ! calibrated paramters according to David Sandoval's PhD project report 
    real(kind=mysg), parameter :: c_wp = 0.2018 ! 0.1437904
    real(kind=mysg), parameter :: d_wp = 0.7809 ! 0.8398534

    real(kind=mysg), parameter :: a_fc = -0.0547 ! 0.03320495
    real(kind=mysg), parameter :: b_fc = -0.001  ! 0.2755312
    real(kind=mysg), parameter :: c_fc = 0.4760  ! 0.3366685
    real(kind=mysg), parameter :: d_fc = 0.9402  ! 1.417544

    real(kind=mysg), parameter :: a_ks = -2.6539  ! -2.793574
    real(kind=mysg), parameter :: b_ks = 3.0924   ! 3.12048
    real(kind=mysg), parameter :: c_ks = 4.2146   ! 4.358185

    ! do idx = 1, nlayers_soil

      fsand   = real(soiltexture(1,1))
      fclay   = real(soiltexture(2,1))
      forg    = real(soiltexture(3,1))
      fgravel = real(soiltexture(4,1))

      fsand_forg  = fsand * forg
      fclay_forg  = fclay * forg
      fsand_fclay = fsand * fclay

      ! particle density
      dp = 1.0/((forg/1.3)+((1.0-forg)/2.65))  
      
      ! bulk density
      bd = (1.5 + (dp-1.5-1.10*(1.0 - fclay))*(1.0-exp(-0.022*depth)))/(1.0+6.27*forg) 
      
      ! water storage at saturation?
      sat = 1.0 - (bd/dp)  
    
      ! ! field capacity, interpretation of equation from David's report Eq. 18:
      ! fc = (sat/bd) * (c_fc + (d_fc - c_fc)) * fclay**0.5 * exp(-(a_fc * fsand - b_fc * forg)/(sat/bd))

      ! david's code: 
      fc = (sat/bd) * (c_fc + (d_fc - c_fc) * fclay**0.5) * exp(-(a_fc * fsand - b_fc * forg)/(sat/bd))  
      fc = max(fc, 0.0)
      fc = min(fc, 1.0)

      ! ! david's code:
      ! fc = (sat/bd)*(0.3366685 + (1.417544 - 0.3366685)*clay^0.5)*exp(-1*(0.03320495*sand - 0.2755312* OM)/(sat/bd))

      ! ! mct
      ! dplyr::mutate( fc = (sat/bd)*(0.3366685 + (1.417544 - 0.3366685)*fclay^0.5)*exp(-(0.03320495*fsand - 0.2755312* forg)/(sat/bd)) ) %>% 
      ! dplyr::mutate( fc = ifelse(fc<0, -0.1, fc) ) %>%
      ! dplyr::mutate( fc = ifelse(fc>1, 1, fc)) %>% 

      
      ! wilting point
      pwp = fc*(c_wp + (d_wp - c_wp)*fclay**0.5)  

      ! ! david's code:
      ! wp<- fc*(0.1437904 + (0.8398534 - 0.1437904)*clay^0.5) 

      ! ! mct:
      ! dplyr::mutate( pwp = fc*(0.1437904 + (0.8398534 - 0.1437904)*fclay^0.5) ) %>% 


      ! conductivity at saturation
      L_10_Ksat = a_ks + b_ks * log10(dp-bd) + c_ks * fsand  
      ksat = 10**L_10_Ksat 
      ksat = ksat * 10.0   ! to mm/h

      ! ! david's code: 
      ! L_10_Ksat = -2.793574+3.12048*log10(dp-bd)+4.358185*sand
      ! ksat = 10^L_10_Ksat
      ! ! to mm/h
      ! ksat<-ksat*10

      ! ! 
      ! moist_fvol33init = 0.278*fsand+0.034*fclay+0.022*forg-0.018*(fsand_forg)-0.027*(fclay_forg)-0.584*(fsand_fclay)+0.078 
      ! moist_fvol33 = moist_fvol33init+(0.636*moist_fvol33init-0.107)  
            
      ! ! get parameters for BC eqn form SAxton 2006
      ! coef_B = (log(1500)-log(33))/(log(fc)-log(pwp))  
      ! coef_A = exp(log(33)+coef_B*log(fc))  
      ! coef_lambda = 1.0/coef_B  
      
      ! ! Ksat = 1930*(SAT_fvol-FC_fvol)**(3-coef_lambda)
      
      ! bub_init = -21.6*fsand-27.93*fclay-81.97*moist_fvol33+71.12*(fsand*moist_fvol33)+8.29*(fclay*moist_fvol33)+14.05*(fsand_fclay)+27.16  
      ! bubbling_p = bub_init+(0.02*bub_init**2-0.113*bub_init-0.7)  
      
      ! ! 101.97162129779 converts from KPa to mmH2O
      ! bubbling_p = bubbling_p * -101.97162129779  
      
      ! ! error in empirical fitting, not possible matric potential positive
      ! if (bubbling_p > 0) then
      !   bubbling_p = bubbling_p * -1.0
      ! end if

      ! ! residual water content for BC eqn, Rawls, 1985
      ! fsand = fsand * 100, 
      ! fclay = fclay * 100,
      ! silt = 100 - fsand - fclay,
      ! forg = forg * 100 

      ! ! Ksat = 10*2.54*10**(-0.6+0.012*fsand-0.0064*fclay)
      ! RES = -0.018+0.0009*fsand+0.005*fclay+0.029*sat -0.0002*fclay**2-0.001*fsand*sat-0.0002*fclay**2*sat**2+0.0003*fclay**2*sat -0.002*sat**2*fclay  

      ! ! parameters for van Genutchen eqn
      ! VG_alpha = exp(-14.96 + 0.03135*fclay + 0.0351*silt + 0.646*forg +15.29*dp - 0.192*topsoil -4.671*dp**2- 0.000781*fclay**2 - 0.00687*forg**2 + 0.0449/forg + 0.0663*log(silt) + 0.1482*log(forg) - 0.04546*dp *silt - 0.4852*dp*forg + 0.00673*topsoil*fclay)  
      ! VG_n = 1.0+exp(-25.23 - 0.02195*fclay + 0.0074*silt - 0.1940*forg + 45.5*dp - 7.24*dp**2 +0.0003658*fclay**2 + 0.002885*forg**2 -12.81/dp - 0.1524/silt - 0.01958/forg - 0.2876*log(silt) - 0.0709*log(forg) -44.6*log(dp) - 0.02264*dp*fclay + 0.0896*dp*forg +0.00718*topsoil*fclay)  
      ! VG_m = 1-(VG_n)  
      
      ! water holding capacity
      whc_dz = (fc-pwp)*(1-fgravel)

      ! add to soil paramters type
      params_soil%fsand        = real(fsand)
      params_soil%fclay        = real(fclay)
      params_soil%forg         = real(forg)
      params_soil%fgravel      = real(fgravel)
      params_soil%fc           = real(fc)
      params_soil%pwp          = real(pwp)
      params_soil%whc_dz       = real(whc_dz)
      params_soil%ksat         = real(ksat)
      params_soil%thdiff_wp    = 0.2    ! value chosen from LPX (most soil codes have 0.2)
      params_soil%thdiff_whc15 = 0.6 ! value chosen from LPX (most soil codes have 0.2)
      params_soil%thdiff_fc    = 0.4

    ! end do

  end function getsoil

end module md_params_soil_pmodel

