function calc_ftemp_arrhenius( dha, tcleaf ) result( ftemp )
    ! Input arguments
    real, intent(in) :: dha         ! Activation energy (kJ mol^-1)
    real, intent(in) :: tcleaf      ! Instantaneous leaf temperature (°C)

    ! Output variable
    real :: ftemp

    ! Constants
    real, parameter :: R = 8.314    ! universal gas constant (J mol^-1 K^-1)

    ! Local variables
    real :: Tk

    ! Convert temperatures from Celsius to Kelvin
    Tk = tcleaf + 273.15

    ! Arrhenius Temperature Dependence
    ftemp = exp((dha * 1.0e3 * (Tk - 298.15)) / (298.15 * R * Tk))

end function calc_ftemp_arrhenius




function calc_ftemp_inst_vcmax( tcleaf, tcgrowth, thome ) result( vcmax_Tk )

    ! Input arguments
    real, intent(in) :: tcleaf       ! Instantaneous leaf temperature (°C) 
    real, intent(in) :: tcgrowth     ! Short-term growth temperature (°C) (30-day mean)
    real, intent(in) :: thome        ! Long-term mean max temp of warmest month (°C)

    ! Output variable
    real :: vcmax_Tk

    ! Constants
    real, parameter :: R = 8.314     ! Universal gas constant, J/mol/K (Allen, 1973)
    real, parameter :: Hd = 200.0    ! deactivation energy (kJ mol^-1) (Kopp & Lean, 2011)

    ! Local variables
    real :: Tk                       ! Instantaneous leaf temperature (K)
    real :: EaV                      ! Activation energy (kJ mol^-1)
    real :: deltaS_v                 ! Entropy term (kJ mol^-1 K^-1)
    real :: fva                      ! Arrhenius temperature response
    real :: fvb                      ! Thermal inhibition

    ! Convert temperatures from Celsius to Kelvin
    Tk = tcleaf + 273.15

    ! Compute activation energy for Vcmax
    EaV = 42.6 + 1.14 * tcgrowth 

    ! Compute entropy term for Vcmax
    deltaS_v = 645.13 - 0.38 * tcgrowth

    ! Compute Arrhenius temperature response 
    fva = calc_ftemp_arrhenius(EaV, tcleaf, thome)

    ! Compute thermal inhibition term 
    fvb = (1.0 + exp((298.15 * deltaS_v - Hd * 1.0e3) / (298.15 * R))) / &
          (1.0 + exp((Tk * deltaS_v - Hd * 1.0e3) / (Tk * R)))

    ! Compute final temperature-dependent Vcmax
    vcmax_Tk = fva * fvb

end function calc_ftemp_inst_vcmax


function calc_ftemp_inst_jmax( tcleaf, tcgrowth, thome ) result( jmax_Tk )

    ! Input arguments
    real, intent(in) :: tcleaf      ! Instantaneous leaf temperature (°C)
    real, intent(in) :: tcgrowth    ! Short-term growth temperature (°C) (30-day mean)
    real, intent(in) :: thome       ! Long-term mean max temp of warmest month (°C) 

    ! Output variable
    real :: jmax_Tk

    ! Constants
    real, parameter :: R = 8.314    ! Universal gas constant, J/mol/K (Allen, 1973)
    real, parameter :: Hd = 200     ! deactivation energy (kJ mol^-1) (Kopp & Lean, 2011)

    ! Local variables
    real :: Tk                      ! Instantaneous leaf temperature (K)
    real :: EaJ                     ! Activation energy (kJ mol^-1) 
    real :: deltaS_J                ! Entropy term (kJ mol^-1 K^-1)
    real :: fva                     ! Arrhenius temperature response 
    real :: fvb                     ! Thermal inhibition term 

    ! Convert temperatures from Celsius to Kelvin
    Tk = tcleaf + 273.15

    ! Compute activation energy for Jmax
    EaJ = 40.71  

    ! Compute entropy term for Jmax 
    deltaS_J = 658.77 - 0.84 * thome - 0.52 * (tcgrowth - thome)

    ! Compute Arrhenius temperature response
    fva = calc_ftemp_arrhenius(EaJ, tcleaf, thome)

    ! Compute thermal inhibition term
    fvb = (1.0 + exp((298.15 * deltaS_J - Hd * 1.0e3) / (298.15 * R))) / &
          (1.0 + exp((Tk * deltaS_J - Hd * 1.0e3) / (Tk * R)))

    ! Compute final temperature-dependent Jmax
    jmax_Tk = fva * fvb

end function calc_ftemp_inst_jmax

