module md_ntransform
  !////////////////////////////////////////////////////////////////
  ! INORGANIC NITROGEN DYNAMICS MODULE AFTER XURI & PRENTICE 2008
  ! Contains the "main" subroutine 'ntransform' and all necessary 
  ! subroutines for handling input/output. 
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  use md_classdefs
  use md_params_core, only: nlu, maxgrid, ndayyear
  use md_tile

  implicit none

  private 
  public ntransform, getpar_modl_ntransform

  !-----------------------------------------------------------------------
  ! Uncertain (unknown) parameters. Runtime read-in
  !-----------------------------------------------------------------------
  type params_ntransform_type
    real :: maxnitr                           ! maximum nitrification rate
    real :: non                               ! maximum NO from nitrification (day-1)
    real :: n2on                              ! maximum N2O from nitrification (day-1)
    real :: kn                                ! Michaelis-Menten coefficient [gN/m2]. Use this value if soil represents top 100 cm 
    real :: kdoc                              ! Michaelis-Menten coefficient [gC/m2]. Use this value if soil represents top 100 cm 
    real :: docmax                            ! docmax
    real :: dnitr2n2o                         ! Fraction of denitrification lost as N2O. Range of possible values: 0.002 - 0.047 (Xu-Ri and Prentice, 2008)
  end type params_ntransform_type

  type( params_ntransform_type ) :: params_ntransform

contains

  subroutine ntransform( tile, tile_fluxes, landuse, aprec, doy )
    !////////////////////////////////////////////////////////////////
    !  Litter and SOM decomposition and nitrogen mineralisation.
    !  1st order decay of litter and SOM _pools, governed by temperature
    !  and soil moisture following LPJ (Sitch et al., 2003) and 
    !  Xu-Ri & Prentice (XXX).
    !  June 2014
    !  b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    use md_params_core, only: pft_start, pft_end
    use md_rates
    use md_interface_pmodel
    use md_forcing_pmodel, only: landuse_type

    ! XXX try: this is wrong: dw1 is only plant available water. 
    ! should be water-filled pore space = ( (porosity - ice) - (total fluid water volume) ) / dz

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(landuse_type), intent(in) :: landuse
    real :: aprec         ! annual total precipitation [mm/d] 
    integer :: doy
    
    ! local variables
    real :: dn2                         ! soil N2 emissions [gN/m2/d]
    real :: dno                         ! soil NO emissions [gN/m2/d]
    real :: ddenitr                     ! gross denitrification [gN/m2/d]
    real :: dnitr                       ! gross nitrification [gN/m2/d]
    real :: dnvol                       ! N volatilisation[gN/m2/d]
    
    real, save :: ph_soil
    real, save :: nh3max

    integer    :: lu                     ! gridcell unit counter variable
        
    real :: dnmax                  ! labile carbon availability modifier
    real :: ftemp_vol              ! temperature rate modifier for ammonia volatilization
    real :: ftemp_nitr             ! temperature rate modifier for nitrification
    real :: ftemp_denitr           ! temperature rate modifier for denitrification
    real :: ftemp_diffus           ! temperature rate modifier for gas difussion from soil
    real :: fph                    ! soil-pH modifier
    real :: fwet                   ! fraction of pools in wet microsites (subject to denitrification)
    real :: fdry                   ! fraction of pools in dry microsites (subject to nitrification)
    
    real :: no3_inc, n2o_inc, no_inc, no2_inc, n2_inc      ! pool increments, temporary variables
    real :: tmp                                            ! temporary variable
        
    real :: nh4_w, no3_w, no2_w    ! anaerobic pools
    real :: nh4_d, no3_d, no2_d    ! aerobic pools
    real :: doc_w, no, n2o, n2     ! anaerobic pools
    
    ! Variables N balance test
    logical, parameter :: baltest_trans = .false.  ! set to false to do mass conservation test during transient simulation
    logical :: verbose = .false.  ! set to true to activate verbose mode
    logical :: baltest
    real :: nbal_before_1, nbal_after_1, nbal1, nbal_before_2, nbal_after_2, nbal2
    real :: no3bal_0, no3bal_1, nh4bal_0, nh4bal_1
    real, parameter :: eps = 9.999e-8    ! numerical imprecision allowed in mass conservation tests

    if (baltest_trans .and. .not. myinterface%steering%spinup) then
      baltest = .true.
      verbose = .true.
    else
      baltest = .false.
    end if

    !-------------------------------------------------------------------------
    ! Record for balances
    !-------------------------------------------------------------------------
    ! all pools plus all losses summed up
    lu = 1
    if (verbose) print*,'              with state variables:'
    if (verbose) print*,'              ninorg = ', tile(lu)%soil%pno3%n14 + tile(lu)%soil%pnh4%n14 &
      + tile(lu)%soil%no_w + tile(lu)%soil%no_d + tile(lu)%soil%n2o_w + tile(lu)%soil%n2o_d &
      + tile(lu)%soil%n2_w + tile(lu)%soil%pno2
    if (verbose) print*,'              nloss  = ', tile_fluxes(lu)%soil%dnloss
    if (verbose) print*,'              dndep  = ', landuse%dnoy + landuse%dnhx
    if (baltest) nbal_before_1 = tile(lu)%soil%pno3%n14 + tile(lu)%soil%pnh4%n14 &
      + tile_fluxes(lu)%soil%dnloss + tile(lu)%soil%no_w + tile(lu)%soil%no_d + tile(lu)%soil%n2o_w &
      + tile(lu)%soil%n2o_d + tile(lu)%soil%n2_w + tile(lu)%soil%pno2 + landuse%dnoy + landuse%dnhx
    if (baltest) nbal_before_2 = tile(lu)%soil%pno3%n14 + tile(lu)%soil%pnh4%n14 + ddenitr &
      + dnitr + dnvol + tile_fluxes(lu)%soil%dnleach + landuse%dnoy + landuse%dnhx
    if (verbose) print*,'executing ntransform() ... '

    !///////////////////////////////////////////////////////////////////////
    ! INITIALIZATION 
    !-----------------------------------------------------------------------    
    if ( doy == 1 ) then
      !///////////////////////////////////////////////////////////////////////
      ! ANNUAL INITIALIZATION 
      !-----------------------------------------------------------------------
      ! Calculate soil PH using empirical relationship with annual precip
      ! Eq.5, Tab.5, XP08 (ntransform.cpp:65) (c++:aprec in mm/yr; F: mm/yr)
      !------------------------------------------------------------------
      ph_soil = 3810.0 / (762.0 + aprec) + 3.8

      ! Deprotonation of NH4 to NH3 depends on soil pH
      !------------------------------------------------------------------
      if (ph_soil > 6.0) then
        nh3max = 1.0
      else
        nh3max = 0.00001
      endif
      
    endif
          
    ! LOOP OVER GRIDCELL LAND UNITS
    luloop: do lu=1,nlu

      !-------------------------------------------------------------------------
      ! Add N deposition to inorganic pools
      !-------------------------------------------------------------------------
      tile(lu)%soil%pno3%n14 = tile(lu)%soil%pno3%n14 + landuse%dnoy
      tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 + landuse%dnhx

      !-------------------------------------------------------------------------
      ! Record for balances
      !-------------------------------------------------------------------------
      ! all pools plus all losses summed up
      if (verbose) print*,'              before:'
      if (verbose) print*,'              no3 = ', tile(lu)%soil%pno3%n14
      if (verbose) print*,'              no4 = ', tile(lu)%soil%pnh4%n14
      if (baltest) no3bal_0 = tile(lu)%soil%pno3%n14
      if (baltest) nh4bal_0 = tile(lu)%soil%pnh4%n14
 

      ! must rather be wtot_up which includes water below permanent wilting point (see waterbalance.F).
      !------------------------------------------------------------------
      ! reference temperature: 25°C
      ftemp_vol = min( 1.0, ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor", ref_temp = 25.0 ) )   

      !///////////////////////////////////////////////////////////////////////
      ! AMMONIUM VOLATILIZATION (ntransform.cpp:41)
      !-----------------------------------------------------------------------
      ! use mw1 for monthly timestep and wpool for daily, because this is updated daily
      ! XXX nh3max is not considered in the equations presented in the paper! XXX
      fph = exp( 2.0 * ( ph_soil - 10.0 ) )
      dnvol = nh3max * ftemp_vol**2 * fph * tile(lu)%soil%phy%wscal &
        * ( 1.0 - tile(lu)%soil%phy%wscal ) * tile(lu)%soil%pnh4%n14
      tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 - dnvol
      tile_fluxes(lu)%soil%dnloss = tile_fluxes(lu)%soil%dnloss + dnvol

      ! if (nh4>0.0) print*,'fvol ', dnvol / nh4 

      !///////////////////////////////////////////////////////////////////////
      ! NITRATE LEACHING
      !-----------------------------------------------------------------------
      ! Reduce NO3 by fraction tile_fluxes(lu)%soil%dnleach
      !------------------------------------------------------------------      
      tile_fluxes(lu)%soil%dnleach = tile(lu)%soil%pno3%n14 * tile_fluxes(lu)%canopy%dfleach
      tile(lu)%soil%pno3%n14 = tile(lu)%soil%pno3%n14 - tile_fluxes(lu)%soil%dnleach
      tile_fluxes(lu)%soil%dnloss = tile_fluxes(lu)%soil%dnloss + tile_fluxes(lu)%soil%dnleach


      !///////////////////////////////////////////////////////////////////////
      ! SUBSTRATE PARTITIONING (ntransform.cpp:95)
      !------------------------------------------------------------------
      ! Nitrification (aerobic) and denitrification (anaerobic) can occur
      ! simulataneously in different microsites. Substrate is thus parti-
      ! tioned according to the water content.
      
      ! wet (anaerobic) fraction
      !------------------------------------------------------------------
      ! print*,'ntransform wscal ', tile(lu)%soil%phy%wscal

      fwet  = tile(lu)%soil%phy%wscal / 3.3
      nh4_w = fwet * tile(lu)%soil%pnh4%n14
      no3_w = fwet * tile(lu)%soil%pno3%n14
      no2_w = fwet * tile(lu)%soil%pno2

      doc_w = tile(lu)%soil%pexud%c12 * fwet

      ! dry (aerobic) fraction
      !------------------------------------------------------------------
      fdry  = 1.0 - fwet
      nh4_d = fdry * tile(lu)%soil%pnh4%n14
      no3_d = fdry * tile(lu)%soil%pno3%n14
      no2_d = fdry * tile(lu)%soil%pno2


      !///////////////////////////////////////////////////////////////////////
      ! NITRIFICATION in aerobic microsites (ntransform.cpp:123)
      !------------------------------------------------------------------
      ftemp_nitr = max( min( &
        (((70.0-tile(lu)%soil%phy%temp)/(70.0-38.0))**12.0) &
        * exp(12.0*(tile(lu)%soil%phy%temp-38.0)/(70.0-38.0)) &
        , 1.0), 0.0)
      
      ! gross nitrification rate (Eq.1, Tab.8, XP08)
      !------------------------------------------------------------------
      no3_inc    = params_ntransform%maxnitr * ftemp_nitr * nh4_d
      dnitr      = no3_inc
      nh4_d      = nh4_d - no3_inc   
  
      ! NO from nitrification (Eq.3, Tab.8, XP08)
      !------------------------------------------------------------------
      no_inc         = params_ntransform%non * no3_inc
      no3_inc        = no3_inc - no_inc      
      tile(lu)%soil%no_d = tile(lu)%soil%no_d + no_inc
      
      ! N2O from nitrification (Eq.4, Tab.8, XP08)
      !------------------------------------------------------------------
      n2o_inc         = params_ntransform%n2on * no3_inc
      no3_inc         = no3_inc - n2o_inc
      tile(lu)%soil%n2o_d = tile(lu)%soil%n2o_d + n2o_inc
      no3_d           = no3_d + no3_inc
            
      ! if N loss is defined w.r.t. reduction in NH4 and NO3 pools, then this is the correct formulation:
      tile_fluxes(lu)%soil%dnloss = tile_fluxes(lu)%soil%dnloss + n2o_inc + no_inc

      ! xxx debug
      if (baltest) no3bal_1 = no3_w + no3_d - no3_inc
      if (baltest) nh4bal_1 = nh4_w + nh4_d + dnitr

      if (baltest) nbal1 = no3bal_1 - no3bal_0
      if (baltest) nbal2 = nh4bal_1 - nh4bal_0
      if (verbose) print*,'              --- preliminary balance after nitrification '
      if (verbose) print*,'              ', nbal1
      if (verbose) print*,'              ', nbal2
      if (baltest .and. abs(nbal1)>eps) stop 'balance 1 not satisfied'
      if (baltest .and. abs(nbal2)>eps) stop 'balance 2 not satisfied'


      !///////////////////////////////////////////////////////////////////////
      ! DENITRIFICATION (ntransform.cpp:177) in anaerobic microsites
      !------------------------------------------------------------------
      ! reference temperature: 22°C
      ftemp_denitr = ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor", ref_temp=22.0 )

      ! Effect of labile carbon availability on denitrification (Eq.2, Tab.9, XP08)
      ! doc is last year's doc because it is only available at the end of the month
      ! while this SR is calculated daily, even when _dailymode==0.
      !------------------------------------------------------------------
      dnmax = params_ntransform%docmax * doc_w / ( params_ntransform%kdoc + doc_w )                     ! dnmax < 1 for all doc_w 
      
      ! Denitrification ratio, NO3 -> NO2 (Eq.3, Tab.9, XP08)
      !------------------------------------------------------------------
      no2_inc = min( dnmax * ftemp_denitr * no3_w / ( params_ntransform%kn + no3_w ) * 1000.0, no3_w )
      if (no2_inc > no3_w) stop 'no2_inc > no3_w'
      
      no3_w   = no3_w - no2_inc
      no2_w   = no2_w + no2_inc
      ddenitr = no2_inc
      
      ! if N loss is defined w.r.t. reduction in NH4 and NO3 pools, then this is the correct formulation:
      tile_fluxes(lu)%soil%dnloss = tile_fluxes(lu)%soil%dnloss + no2_inc

      ! Transformation NO2 -> N2 (Eq.4., Tab.9, XP08)
      !------------------------------------------------------------------
      n2_inc = min( dnmax * ftemp_denitr * no2_w / ( params_ntransform%kn + no2_w ) * 1000.0, no2_w )
      if (n2_inc > no2_w) stop 'n2_inc > no2_w'

      no2_w = no2_w - n2_inc
      
      ! N2O from denitrification (Eq.6, Tab.9, XP08)
      !------------------------------------------------------------------
      ! n2o_inc = 0.018d0*ftemp_denitr*(1.01d0-0.21d0*tile(lu)%soil%phy%wscal) * n2_inc  !Colin says 0.018 was used here. Code I got had 0.015
      ! Factor reduced from 1.8% to 1.2% to get ~6.5 TgN/yr N2O emissions
      ! n2o_inc = dnitr2n2o*ftemp_denitr*(1.01-0.21*tile(lu)%soil%phy%wscal) * n2_inc
      ! XXX try: Changed this to from 0.21 to 1.0 in order to get plausible results
      n2o_inc = params_ntransform%dnitr2n2o * ftemp_denitr &
        * ( 1.01 - 0.8 * tile(lu)%soil%phy%wscal ) * n2_inc
      n2_inc  = n2_inc - n2o_inc
      tile(lu)%soil%n2o_w = tile(lu)%soil%n2o_w + n2o_inc

      ! NO from denitrification (Eq.5, Tab.9, XP08)
      !------------------------------------------------------------------
      ! no_inc = 0.0001*ftemp_denitr*(1.01-0.21*tile(lu)%soil%phy%wscal) * n2_inc
      ! XXX try: Changed this to from 0.21 to 1.0 in order to get plausible results
      no_inc = 0.0001 * ftemp_denitr * ( 1.01 - 0.8 * tile(lu)%soil%phy%wscal ) * n2_inc
      n2_inc = n2_inc - no_inc
      tile(lu)%soil%no_w = tile(lu)%soil%no_w + no_inc

      ! N2 from denitrification
      !------------------------------------------------------------------
      tile(lu)%soil%n2_w = tile(lu)%soil%n2_w + n2_inc


      !///////////////////////////////////////////////////////////////////////
      ! UPDATE POOLS (ntransform.cpp:389)
      !------------------------------------------------------------------
      ! nh4, no3 and no2 (sum of sub-_pools in wet and dry microsites) are
      ! defined as global variables. They are split into sub-_pools at the
      ! beginning of ntransform (see "substrate partitioning"), processed
      ! through "denitrification" and "nitrification" and added up again
      ! here. In contrast, each sub-pool for wet and dry conditions of no,
      ! n2o and n2 is defined as a global (common) variable, while the sum
      ! of the sup-_pools (no, n2o and n2) is defined locally and only used
      ! for the diffusion/emission (see below).
      !------------------------------------------------------------------
      tile(lu)%soil%pnh4%n14 = nh4_w + nh4_d
      tile(lu)%soil%pno3%n14 = no3_w + no3_d
      tile(lu)%soil%pno2     = no2_w + no2_d

      no  = tile(lu)%soil%no_w  + tile(lu)%soil%no_d
      n2o = tile(lu)%soil%n2o_w + tile(lu)%soil%n2o_d
      n2  = tile(lu)%soil%n2_w

      !///////////////////////////////////////////////////////////////////////
      ! Diffusion of NO, N2O and N2 from the soil (ntransform.cpp:281)
      !------------------------------------------------------------------
      ! reference temperature: 25°C. Corresponds to Eq.1, Tab.10, Xu-Ri & Prentice, 2008
      ftemp_diffus = min( 1.0, ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor", ref_temp = 25.0 ))

      ! Total gaseous escape
      !------------------------------------------------------------------
      tile_fluxes(lu)%soil%dn2o = ftemp_diffus * (1.0 - tile(lu)%soil%phy%wscal) * n2o
      dno                       = ftemp_diffus * (1.0 - tile(lu)%soil%phy%wscal) * no
      dn2                       = ftemp_diffus * (1.0 - tile(lu)%soil%phy%wscal) * n2

      ! if N loss is defined w.r.t. gaseous escape, then this is the correct formulation:
      ! tile_fluxes(lu)%soil%dnloss = tile_fluxes(lu)%soil%dnloss + dno + tile_fluxes(lu)%soil%dn2o  + dn2

      ! Gaseous escape of pools at dry microsites
      !------------------------------------------------------------------
      tmp = ftemp_diffus * (1.0 - tile(lu)%soil%phy%wscal) * tile(lu)%soil%no_d
      tile(lu)%soil%no_d = tile(lu)%soil%no_d - tmp
      
      tmp = ftemp_diffus * (1.0 - tile(lu)%soil%phy%wscal) * tile(lu)%soil%n2o_d
      tile(lu)%soil%n2o_d = tile(lu)%soil%n2o_d - tmp

      ! Gaseous escape of pools at wet microsites
      !------------------------------------------------------------------
      tmp = ftemp_diffus * (1.0 - tile(lu)%soil%phy%wscal) * tile(lu)%soil%no_w
      tile(lu)%soil%no_w = tile(lu)%soil%no_w - tmp
      
      tmp = ftemp_diffus * (1.0 - tile(lu)%soil%phy%wscal) * tile(lu)%soil%n2o_w
      tile(lu)%soil%n2o_w = tile(lu)%soil%n2o_w - tmp
                 
      tile(lu)%soil%n2_w = tile(lu)%soil%n2_w - dn2
      
    enddo luloop

    !-------------------------------------------------------------------------
    ! Test mass conservation
    !-------------------------------------------------------------------------
    ! all pools plus all losses summed up
    lu = 1
    if (baltest) nbal_after_1 = tile(lu)%soil%pno3%n14 + tile(lu)%soil%pnh4%n14 + tile_fluxes(lu)%soil%dnloss &
      + tile(lu)%soil%no_w + tile(lu)%soil%no_d + tile(lu)%soil%n2o_w + tile(lu)%soil%n2o_d + tile(lu)%soil%n2_w &
      + tile(lu)%soil%pno2
    if (baltest) nbal_after_2 = tile(lu)%soil%pno3%n14 + tile(lu)%soil%pnh4%n14 + ddenitr &
      + dnitr + dnvol + tile_fluxes(lu)%soil%dnleach - no3_inc
    if (baltest) nbal1 = nbal_after_1 - nbal_before_1
    if (baltest) nbal2 = nbal_after_2 - nbal_before_2
    if (verbose) print*,'              ==> returned:'
    if (verbose) print*,'              ninorg = ', tile(lu)%soil%pno3%n14 + tile(lu)%soil%pnh4%n14 &
      + tile(lu)%soil%no_w + tile(lu)%soil%no_d + tile(lu)%soil%n2o_w + tile(lu)%soil%n2o_d &
      + tile(lu)%soil%n2_w + tile(lu)%soil%pno2
    if (verbose) print*,'              nloss  = ', tile_fluxes(lu)%soil%dnloss
    if (verbose) print*,'   --- balance: '
    if (verbose) print*,'       d( ninorg + loss )', nbal1
    if (verbose) print*,'       d( ninorg + loss )', nbal2
    if (baltest .and. abs(nbal1) > eps) stop 'balance 1 not satisfied'
    if (baltest .and. abs(nbal2) > eps) stop 'balance 2 not satisfied'

  end subroutine ntransform

  
  subroutine getpar_modl_ntransform()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads waterbalance module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    use md_interface_pmodel, only: myinterface

    ! maximum nitrification rate
    params_ntransform%maxnitr   = myinterface%params_calib%maxnitr

    ! maximum NO from nitrification (day-1)
    params_ntransform%non       = myinterface%params_calib%non

    ! maximum N2O from nitrification (day-1)
    params_ntransform%n2on      = myinterface%params_calib%n2on

    ! Michaelis-Menten coefficient [gN/m2]. Use this value if soil represents top 100 cm 
    params_ntransform%kn        = myinterface%params_calib%kn

    ! Michaelis-Menten coefficient [gC/m2]. Use this value if soil represents top 100 cm 
    params_ntransform%kdoc      = myinterface%params_calib%kdoc

    ! docmax
    params_ntransform%docmax    = myinterface%params_calib%docmax

    ! Fraction of denitrification lost as N2O. Range of possible values is 0.002 - 0.047 (Xu-Ri and Prentice, 2008)
    params_ntransform%dnitr2n2o = myinterface%params_calib%dnitr2n2o

  end subroutine getpar_modl_ntransform


end module md_ntransform
