module md_littersom
  !////////////////////////////////////////////////////////////////
  ! LPJ LITTERSOM MODULE
  ! Contains the "main" subroutine 'littersom' and all necessary 
  ! subroutines for handling input/output. 
  ! Every module that implements 'littersom' must contain this list 
  ! of subroutines (names that way).
  ! Required module-independent model state variables (necessarily 
  ! updated by 'littersom') are:
  !   - xxx
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  use md_classdefs
  use md_params_core, only: npft, maxgrid, nlu, ndayyear

  implicit none
  
  private 
  public psoil_fs, psoil_sl, drhet, getpar_modl_littersom, initio_littersom, &
    initoutput_littersom, &
    getout_annual_littersom, writeout_ascii_littersom, &
    littersom, initdaily_littersom, initglobal_littersom, &
    outdnetmin ! xxx debug


  !----------------------------------------------------------------
  ! Public, module-specific state variables
  !----------------------------------------------------------------
  ! pools
  type(orgpool), dimension(nlu,maxgrid)  :: psoil_sl        ! soil organic matter, fast turnover [gC/m2]
  type(orgpool), dimension(nlu,maxgrid)  :: psoil_fs        ! soil organic matter, slow turnover [gC/m2]

  ! fluxes
  type(carbon), dimension(nlu) :: drsoil   ! soil respiration (only from exudates decomp.) [gC/m2/d]
  type(carbon), dimension(nlu) :: drhet    ! heterotrophic respiration [gC/m2/d]

  !-----------------------------------------------------------------------
  ! Uncertain (unknown) parameters. Runtime read-in
  !-----------------------------------------------------------------------
  type params_littersom_type
    real :: klitt_af10        ! above-ground fast (leaf) litter decay rate [1/d]
    real :: klitt_as10        ! above-ground slow (woody) litter decay rate [1/d] 
    real :: klitt_bg10        ! below-ground (root) litter decay rate [1/d] 
    real :: kexu10            ! exudates decay rate [1/d]
    real :: ksoil_fs10        ! fast soil pool decay rate [1/d]
    real :: ksoil_sl10        ! slow soil pool decay rate [1/d]
    real :: ntoc_crit1        ! factor for "Manzoni Equation" (XPXXX) [1]
    real :: ntoc_crit2        ! exponent for "Manzoni Equation" (XPXXX) [1]
    real :: cton_soil         ! C:N ratio of SOM
    real :: ntoc_soil         ! N:C ratio of SOM, calculated as the inverse of the read-in parameter cton_soil
    real :: fastfrac          ! fraction of litter input to fast soil pool [1]
  end type params_littersom_type

  type( params_littersom_type ) :: params_littersom


  !----------------------------------------------------------------
  ! Module-specific output variables
  !----------------------------------------------------------------
  ! daily
  real, allocatable, dimension(:,:,:) :: outdnetmin
  real, allocatable, dimension(:,:,:) :: outdnetmin_soil
  real, allocatable, dimension(:,:,:) :: outdnetmin_litt
  real, allocatable, dimension(:,:,:) :: outdnfixfree  ! N fixation by free-living organisms [gN/m2/d]

  ! annual
  real, dimension(npft,maxgrid) :: outaclitt
  real, dimension(nlu,maxgrid)  :: outacsoil
  real, dimension(nlu,maxgrid)  :: outanreq      ! N required from litter -> soil transfer [gN/m2/d]
  real, dimension(nlu,maxgrid)  :: outaclit2soil
  real, dimension(nlu,maxgrid)  :: outanlit2soil
  real, dimension(nlu,maxgrid)  :: outaCdsoil
  real, dimension(nlu,maxgrid)  :: outaNdsoil
  real, dimension(nlu,maxgrid)  :: outaNimmo

contains

  subroutine littersom( plant, phy, jpngr, doy, dtemp )
    !////////////////////////////////////////////////////////////////
    !  Litter and SOM decomposition and nitrogen mineralisation.
    !  1st order decay of litter and SOM _pools, governed by temperature
    !  and soil moisture following LPJ (Sitch et al., 2003) and 
    !  Xu-Ri & Prentice (XXX).
    !  June 2014
    !  b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    use md_params_core, only: nmonth, nlu, pft_start, pft_end
    use md_interface
    use md_classdefs
    use md_rates, only: ftemp, fmoist
    use md_plant, only: plant_type, params_pft_plant
    use md_waterbal, only: soilphys
    ! use md_ntransform, only: pnh4, pno3
    use md_tile, only: psoilphystype

    ! arguments
    type(plant_type), dimension(npft), intent(inout) :: plant
    type(psoilphystype), dimension(nlu), intent(in)  :: phy
    integer, intent(in) :: jpngr                    ! grid cell number
    integer, intent(in) :: doy                      ! day of year
    real, intent(in)    :: dtemp                    ! daily temperature (deg C)

    ! local variables
    integer :: lu                                   ! counter variable for landuse class
    integer :: pft                                  ! counter variable for PFT number

    ! temperature/soil moisture-modified decay constants
    real, dimension(nlu)  :: klitt_af               ! decay rate, above-ground fast (leaf) litter (= k_litter_leaf)
    real, dimension(nlu)  :: klitt_as               ! decay rate, above-ground slow (woody) litter (= k_litter_woody)
    real, dimension(nlu)  :: klitt_bg               ! decay rate, below-ground fast litter (= k_litter_root)
    real, dimension(nlu)  :: kexu                   ! decay rate, exudates (= k_exu)
    real, dimension(nlu)  :: ksoil_fs               ! decay rate, fast soil (= k_fast)
    real, dimension(nlu)  :: ksoil_sl               ! decay rate, slow soil (= k_slow)

    ! temporary _pools
    type(carbon)  :: dexu                           ! exudates decomposed in time step (= exu_decom)
    type(orgpool) :: dlitt                          ! total litter decomposed per time step
    type(orgpool), dimension(npft) :: dlitt_af      ! above-ground fast litter decomposed per time step (= litterdag_fast)
    type(orgpool), dimension(npft) :: dlitt_as      ! above-ground slow litter decomposed per time step (= litterdag_slow)
    type(orgpool), dimension(npft) :: dlitt_bg      ! below-ground slow litter decomposed per time step (= litter_decom_bg)
    type(orgpool) :: dsoil_sl                       ! (= cflux_fast_atmos)
    type(orgpool) :: dsoil_fs                       ! (= cflux_fast_atmos)
   
    ! temporary variables
    real :: eff                                     ! microbial growth efficiency 
    real :: ntoc_crit                               ! critical N:C ratio below which immobilisation occurrs  
    real :: Nreq_S                                  ! N required in litter decomposition to maintain SOM C:N
    real :: Nfix                                    ! temporary variable, N fixation implied in litter decomposition,
    real :: rest                                    ! temporary variable
    real :: req                                     ! N required for litter decomposition 
    real :: avl                                     ! mineral N available as inorganic N
    real :: netmin_litt                             ! net N mineralisation from litter decomposition

    integer, save :: invocation = 0                 ! internally counted simulation year

    real, dimension(nlu,maxgrid), save :: mean_insoil_fs
    real, dimension(nlu,maxgrid), save :: mean_insoil_sl
    real, dimension(nlu,maxgrid), save :: mean_ksoil_sl
    real, dimension(nlu,maxgrid), save :: mean_ksoil_fs
    real :: ntoc_save_fs, ntoc_save_sl


    !-------------------------------------------------------------------------
    ! Count number of calls (one for each simulation year)
    !-------------------------------------------------------------------------
    if (doy==1) invocation = invocation + 1

    ! initialise average fluxes
    if (invocation==1 .and. doy==1) then
      mean_insoil_fs(:,:) = 0.0
      mean_insoil_sl(:,:) = 0.0
      mean_ksoil_fs(:,:)  = 0.0
      mean_ksoil_sl(:,:)  = 0.0
    end if

    !-------------------------------------------------------------------------
    ! Set soil turnover accelerator for equilibration during spinup.
    ! Evenly scale soil inputs and soil decomposition _rates during
    ! first 200 years with a scalar that linearly decreases from 200
    ! to 1 over the first 200 years of the simulation.
    ! Value 200 is chosen for quick equilibration without overshooting
    ! for a temperate climate (Switzerland). May have to adjust this
    ! for improving performance with a global simulation.
    !-------------------------------------------------------------------------

    do lu=1,nlu

      !/////////////////////////////////////////////////////////////////////////
      ! DECAY RATES
      !-------------------------------------------------------------------------
      ! Calculate daily (monthly) decomposition _rates as a function of
      ! temperature and moisture
                 
      ! k = k_10 * respir_modifier

      ! (1) dc/dt = -kc     where c=pool size, t=time, k=decomposition rate
      ! from (1),
      ! (2) c = c0*exp(-kt) where c0=initial pool size
      ! from (2), decomposition in any month given by
      ! (3) delta_c = c0 - c0*exp(-k)
      ! from (4)
      ! (4) delta_c = c0*(1.00-exp(-k))
      !-------------------------------------------------------------------------
      do pft=1,npft
        if (params_pft_plant(pft)%islu(lu)) then
            
          !-------------------------------------------------------------------------
          ! LITTER TEMPERATURE AND MOISTURE MODIFIER
          ! Temperature: Lloyd & Taylor 1994, Brovkin et al., 2012
          ! Moisture: Foley, 1995; Fang and Moncrieff, 1999; Gerten et al., 2004;
          ! Wania et al., 2009; Frolking et al., 2010; Spahni et al., 2012
          !-------------------------------------------------------------------------
          ! define decomposition _rates for current soil temperature and moisture 
          klitt_af(lu) = params_littersom%klitt_af10 * ftemp( dtemp,        "lloyd_and_taylor" ) * fmoist( soilphys(lu)%wscal, "foley" ) ! alternative: "gerten"
          klitt_as(lu) = params_littersom%klitt_as10 * ftemp( dtemp,        "lloyd_and_taylor" ) * fmoist( soilphys(lu)%wscal, "foley" ) ! alternative: "gerten"
          klitt_bg(lu) = params_littersom%klitt_bg10 * ftemp( phy(lu)%temp, "lloyd_and_taylor" ) * fmoist( soilphys(lu)%wscal, "foley" ) ! alternative: "gerten"
          kexu(lu)     = params_littersom%kexu10     * ftemp( phy(lu)%temp, "lloyd_and_taylor" ) * fmoist( soilphys(lu)%wscal, "foley" ) ! alternative: "gerten"

        end if
      end do

      !-------------------------------------------------------------------------
      ! SOIL TEMPERATURE AND MOISTURE MODIFIER
      ! Temperature: Lloyd & Taylor 1994
      ! Moisture: Foley, 1995; Fang and Moncrieff, 1999; Gerten et al., 2004;
      !           Wania et al., 2009; Frolking et al., 2010; Spahni et al., 2012
      !-------------------------------------------------------------------------
      ksoil_fs(lu) = params_littersom%ksoil_fs10 * ftemp( phy(lu)%temp, "lloyd_and_taylor" ) * fmoist( soilphys(lu)%wscal, "foley" )     ! alternative: "gerten"
      ksoil_sl(lu) = params_littersom%ksoil_sl10 * ftemp( phy(lu)%temp, "lloyd_and_taylor" ) * fmoist( soilphys(lu)%wscal, "foley" )     ! alternative: "gerten"

      !-------------------------------------------------------------------------
      ! Initialisation of decomposing pool 
      ! (temporary, decomposition for each LU-class).
      !-------------------------------------------------------------------------
      dlitt = orgpool( carbon(0.0), nitrogen(0.0) ) 

      do pft=1,npft
        if (params_pft_plant(pft)%islu(lu)) then
                        
          !////////////////////////////////////////////////////////////////
          ! LITTER DECAY
          ! Collect PFT-specific litter decomposition into LU-specific 
          ! pool 'dlitt'.
          ! All goes to daily updated litter decomposition pool
          !----------------------------------------------------------------
          dlitt_af = orgfrac( 1.0 - exp( -klitt_af(lu)), plant(pft)%plitt_af )
          dlitt_as = orgfrac( 1.0 - exp( -klitt_as(lu)), plant(pft)%plitt_as )
          dlitt_bg = orgfrac( 1.0 - exp( -klitt_bg(lu)), plant(pft)%plitt_bg )

          ! Update the litter _pools
          call orgmv( dlitt_af(pft), plant(pft)%plitt_af, dlitt )
          call orgmv( dlitt_as(pft), plant(pft)%plitt_as, dlitt )
          call orgmv( dlitt_bg(pft), plant(pft)%plitt_bg, dlitt )
      
          !////////////////////////////////////////////////////////////////
          ! EXUDATES DECAY
          ! Calculate the exudates respiration before litter respiration.
          ! Exudates are mostly short organic compounds (poly- and mono-
          ! saccharides, amino acids, organic acids, phenolic compounds and
          ! enzymes) and are quickly respired and released as CO2.
          ! Exudates decay goes to soil respiration 'drsoil'.
          ! This is executed after litter mineralisation as N fixation by 
          ! free-living bacteria is driven by exudates availability.
          !----------------------------------------------------------------                
          dexu = cfrac( 1.0 - exp(-kexu(lu)), plant(pft)%pexud )
          call cmv( dexu, plant(pft)%pexud, drsoil(lu) )

        end if
      end do

      !----------------------------------------------------------------
      ! Soil C:N ratio is the average PFT-specific prescribed C:N ratio
      ! weighted by the PFT-specific decomposition.
      !----------------------------------------------------------------
      if ( dlitt%c%c12 > 0.0 ) then

        !////////////////////////////////////////////////////////////////
        ! ATMOSPHERIC FRACTION ~ 1 - MICROBIAL GROWTH EFFICIENCY
        ! critical C:N ratio for net mineralisation is a function of C:N
        ! ratio of decomposing litter. Eq. 9 in Xu-Ri & Prentice, 2014
        !----------------------------------------------------------------
        ntoc_crit = params_littersom%ntoc_crit1 * ntoc( dlitt, default=0.0 ) ** params_littersom%ntoc_crit2  ! = rCR
        eff = ntoc_crit * params_littersom%cton_soil

        !////////////////////////////////////////////////////////////////
        ! LITTER -> SOIL FLUX AND NET MINERALISATION/IMMOBILISATION
        ! Calculate net mineralisation/immobilisation based on Manzoni
        ! et al. (2008) and Xu-Ri & Prentice (2014).
        !----------------------------------------------------------------    
        ! CARBON LITTER -> SOIL TRANSFER
        !----------------------------------------------------------------    
        ! record N:C ratio to override later (compensating for numerical imprecision)
        ntoc_save_fs = ntoc( psoil_fs(lu,jpngr), default=0.0 )
        ntoc_save_sl = ntoc( psoil_sl(lu,jpngr), default=0.0 )

        ! move fraction 'eff' of C from litter to soil
        call ccp( cfrac( eff*params_littersom%fastfrac      , dlitt%c ), psoil_fs(lu,jpngr)%c )
        call ccp( cfrac( eff*(1.0-params_littersom%fastfrac), dlitt%c ), psoil_sl(lu,jpngr)%c )

        ! move fraction '(1-eff)' of C to heterotrophic respiration
        call ccp( cfrac( (1.0-eff), dlitt%c ), drhet(lu) )

        ! get average litter -> soil flux for analytical soil C equilibration
        if ( interface%steering%average_soil ) then
          mean_insoil_fs(lu,jpngr) = mean_insoil_fs(lu,jpngr) + eff * params_littersom%fastfrac * dlitt%c%c12
          mean_insoil_sl(lu,jpngr) = mean_insoil_sl(lu,jpngr) + eff * (1.0-params_littersom%fastfrac) * dlitt%c%c12
        end if

        !----------------------------------------------------------------    
        ! N MINERALISATION
        !----------------------------------------------------------------    
        ! N requirement to maintain rS (SOM N:C ratio)
        Nreq_S = dlitt%c%c12 * eff * params_littersom%ntoc_soil  ! 1/cton_soil = rS

        ! OUTPUT COLLECTION
        outanreq(lu,jpngr)      = outanreq(lu,jpngr)      + Nreq_S
        outaclit2soil(lu,jpngr) = outaclit2soil(lu,jpngr) + dlitt%c%c12 * eff
        outanlit2soil(lu,jpngr) = outanlit2soil(lu,jpngr) + Nreq_S

        ! write(0,*) 'outaclit2soil(lu,jpngr)',outaclit2soil(lu,jpngr)
        ! outanlit2soil(pft,jpngr) = outanlit2soil(pft,jpngr) + dlitt%n%n14

        !----------------------------------------------------------------    
        ! If N supply is sufficient, mineralisation occurrs: positive (dNLit-Nreq).
        ! otherwise, immobilisation occurrs: negative (dNLit-Nreq).
        ! Thus, the balance for total organic N is:
        ! dN/dt = -(dNLit - Nreq)
        !       = -(dCLit*rL - dCLit*eff*rS)
        !       = dCLit*(rCR - rL) , rCR=eff*rS ('critical' N:C ratio)
        ! This corresponds to Eq. S3 in Manzoni et al., 2010, but ...
        ! rS takes the place of rB.
        !----------------------------------------------------------------    
        netmin_litt = dlitt%n%n14 - Nreq_S

        Nfix = 0.0

        ! OUTPUT COLLECTION
        if (interface%params_siml%loutlittersom) then
          outdnetmin(lu,doy,jpngr)      = outdnetmin(lu,doy,jpngr) + netmin_litt
          outdnetmin_litt(lu,doy,jpngr) = outdnetmin_litt(lu,doy,jpngr) + netmin_litt
        end if
        
        outaNimmo(lu,jpngr)             = outaNimmo(lu,jpngr) - netmin_litt  ! minus because netmin_litt < 0 for immobilisation

        ! if (netmin_litt>0.0) then
        !   !----------------------------------------------------------------    
        !   ! net N mineralisation
        !   !----------------------------------------------------------------    
        !   pnh4(lu,jpngr)%n14 = pnh4(lu,jpngr)%n14 + netmin_litt
          
        ! else

        !   if ( (-1.0 * netmin_litt) > (pnh4(lu,jpngr)%n14 + pno3(lu,jpngr)%n14) ) print*, 'too much immo'

        !   !----------------------------------------------------------------    
        !   ! Immobilisation: first deplete NH4 pool
        !   !----------------------------------------------------------------    
        !   req = -1.0 * netmin_litt
        !   avl = pnh4(lu,jpngr)%n14

        !   if (avl>=req) then
        !     ! enough mineral N for immobilisation
        !     pnh4(lu,jpngr)%n14 = pnh4(lu,jpngr)%n14 - req
        !     req = 0.0
        !   else
        !     ! not enough NH4 for immobilisation
        !     pnh4(lu,jpngr)%n14 = pnh4(lu,jpngr)%n14 - avl
        !     req = req - avl

        !     !----------------------------------------------------------------    
        !     ! Immobilisation: second deplete NO3 pool
        !     !----------------------------------------------------------------    
        !     avl = pno3(lu,jpngr)%n14

        !     if (avl>=req) then
        !       ! enough mineral N for immobilisation
        !       pno3(lu,jpngr)%n14 = pno3(lu,jpngr)%n14 - req
        !       req = 0.0
        !     else
        !       ! not enough NO3 for immobilisation
        !       pno3(lu,jpngr)%n14 = pno3(lu,jpngr)%n14 - avl
        !       req = req - avl

        !       !----------------------------------------------------------------    
        !       ! N fixation by free-living bacteria in litter to satisfy remainder
        !       !----------------------------------------------------------------    
        !       Nfix = req
        !       ! print*,'req ', req
        !       req = 0.0
        !       ! stop 'could not get enough N upon immobilisation'

        !     end if

        !   end if
          
        ! end if

        ! Nreq_S (= dlitt - netmin) remains in the system: 
        call ncp( nfrac( params_littersom%fastfrac      , nitrogen(Nreq_S) ), psoil_fs(lu,jpngr)%n )
        call ncp( nfrac( (1.0-params_littersom%fastfrac), nitrogen(Nreq_S) ), psoil_sl(lu,jpngr)%n )

        if ( abs( cton(psoil_fs(lu,jpngr)) - params_littersom%cton_soil ) > 1e-5 ) then
          write(0,*) 'psoil_fs', cton( psoil_fs(lu,jpngr) )
          stop 'B fs: C:N not ok'
        end if
        if ( abs( cton(psoil_sl(lu,jpngr)) - params_littersom%cton_soil ) > 1e-5 ) then
          write(0,*) 'psoil_sl', cton( psoil_sl(lu,jpngr) )
          stop 'B sl: C:N not ok'
        end if

        ! OUTPUT COLLECTION
        if (interface%params_siml%loutlittersom) then
          outdnfixfree(lu,doy,jpngr) = outdnfixfree(lu,doy,jpngr) + Nfix
        end if

      end if

      !////////////////////////////////////////////////////////////////
      ! SOIL DECAY
      !----------------------------------------------------------------
      ! Calculate daily/monthly soil decomposition to the atmosphere

      ! record N:C ratio to override later (compensating for numerical imprecision)
      ntoc_save_fs = ntoc( psoil_fs(lu,jpngr), default=0.0 )
      ntoc_save_sl = ntoc( psoil_sl(lu,jpngr), default=0.0 )

      dsoil_fs = orgfrac( (1.0-exp(-ksoil_fs(lu))), psoil_fs(lu,jpngr) )
      dsoil_sl = orgfrac( (1.0-exp(-ksoil_sl(lu))), psoil_sl(lu,jpngr) )

      ! soil decay
      psoil_fs(lu,jpngr) = orgminus( psoil_fs(lu,jpngr), dsoil_fs )
      psoil_sl(lu,jpngr) = orgminus( psoil_sl(lu,jpngr), dsoil_sl )
      
      ! C to heterotrophic respiration
      drhet(lu) = cplus( drhet(lu), dsoil_fs%c, dsoil_sl%c )

      ! Spinup trick: use projected soil N mineralisation before soil equilibration
      ! if ( interface%steering%project_nmin ) then
      !   ! projected soil N mineralisation
      !   if (dlitt%c%c12 > 0.0) pnh4(lu,jpngr)%n14 = pnh4(lu,jpngr)%n14 + eff * dlitt%c%c12 / params_littersom%cton_soil
      ! else
      !   ! actual soil N mineralisation
      !   pnh4(lu,jpngr)%n14 = pnh4(lu,jpngr)%n14 + dsoil_fs%n%n14 + dsoil_sl%n%n14
      ! end if

      if ( psoil_fs(lu,jpngr)%c%c12 > 0.0 .and. abs( cton( psoil_fs(lu,jpngr), default=0.0 ) - params_littersom%cton_soil ) > 1e-4 ) then
        write(0,*) 'psoil_fs', cton( psoil_fs(lu,jpngr) )
        stop 'C fs: C:N not ok'
      end if
      if ( psoil_sl(lu,jpngr)%c%c12 > 0.0 .and. abs( cton( psoil_sl(lu,jpngr), default=0.0 ) - params_littersom%cton_soil ) > 1e-4 ) then
        write(0,*) 'psoil_sl', cton( psoil_sl(lu,jpngr) )
        stop 'C sl: C:N not ok'
      end if
      
      ! get average litter -> soil flux for analytical soil C equilibration
      if ( interface%steering%average_soil ) then
        mean_ksoil_fs(lu,jpngr) = mean_ksoil_fs(lu,jpngr) + ksoil_fs(lu)
        mean_ksoil_sl(lu,jpngr) = mean_ksoil_sl(lu,jpngr) + ksoil_sl(lu)
      end if

      ! analytical soil C equilibration
      if ( interface%steering%do_soilequil .and. doy==ndayyear ) then
        psoil_fs(lu,jpngr)%c%c12 = mean_insoil_fs(lu,jpngr) / mean_ksoil_fs(lu,jpngr)
        psoil_sl(lu,jpngr)%c%c12 = mean_insoil_sl(lu,jpngr) / mean_ksoil_sl(lu,jpngr)
        psoil_fs(lu,jpngr)%n%n14 = psoil_fs(lu,jpngr)%c%c12 * ntoc_save_fs
        psoil_sl(lu,jpngr)%n%n14 = psoil_sl(lu,jpngr)%c%c12 * ntoc_save_sl
        mean_insoil_fs(lu,jpngr) = 0.0
        mean_insoil_sl(lu,jpngr) = 0.0
        mean_ksoil_fs(lu,jpngr)  = 0.0
        mean_ksoil_sl(lu,jpngr)  = 0.0
      end if
 
      ! OUTPUT COLLECTION
      if (interface%params_siml%loutlittersom) then
        outdnetmin(lu,doy,jpngr)      = outdnetmin(lu,doy,jpngr) + dsoil_fs%n%n14 + dsoil_sl%n%n14
        outdnetmin_soil(lu,doy,jpngr) = outdnetmin_soil(lu,doy,jpngr) + dsoil_fs%n%n14 + dsoil_sl%n%n14
        outaCdsoil(lu,jpngr)          = outaCdsoil(lu,jpngr)     + dsoil_fs%c%c12 + dsoil_sl%c%c12
        outaNdsoil(lu,jpngr)          = outaNdsoil(lu,jpngr)     + dsoil_fs%n%n14 + dsoil_sl%n%n14
      end if

    enddo                   !lu

  
  end subroutine littersom


  subroutine getpar_modl_littersom()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads littersom module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    use md_sofunutils, only: getparreal

    ! above-ground fast (foliage and roots) litter decay rate [1/d] 
    params_littersom%klitt_af10 = getparreal( 'params/params_littersom_lpj.dat', 'klitt_af10' ) / ndayyear

    ! above-ground slow (woody) litter decay rate [1/d] 
    params_littersom%klitt_as10 = getparreal( 'params/params_littersom_lpj.dat', 'klitt_as10' ) / ndayyear

    ! below-ground (root) litter decay rate [1/d] 
    params_littersom%klitt_bg10 = getparreal( 'params/params_littersom_lpj.dat', 'klitt_bg10' ) / ndayyear

    ! exudates decay rate [1/d]
    params_littersom%kexu10 = getparreal( 'params/params_littersom_lpj.dat', 'kexu10' ) / ndayyear

    ! fast soil pool decay rate [1/d]
    params_littersom%ksoil_fs10 = getparreal( 'params/params_littersom_lpj.dat', 'ksoil_fs10' ) / ndayyear

    ! slow soil pool decay rate [1/d]
    params_littersom%ksoil_sl10 = getparreal( 'params/params_littersom_lpj.dat', 'ksoil_sl10' ) / ndayyear

    ! factor for "Manzoni Equation" (XPXXX) [1]
    params_littersom%ntoc_crit1 = getparreal( 'params/params_littersom_lpj.dat', 'ntoc_crit1' ) 
 
    ! exponent for "Manzoni Equation" (XPXXX) [1]
    params_littersom%ntoc_crit2 = getparreal( 'params/params_littersom_lpj.dat', 'ntoc_crit2' ) 
 
    ! C:N ratio of SOM
    params_littersom%cton_soil = getparreal( 'params/params_littersom_lpj.dat', 'cton_soil' ) 

    ! N:C ratio of SOM
    params_littersom%ntoc_soil = 1.0 / params_littersom%cton_soil
 
    ! fraction of litter input to fast soil pool [1]
    params_littersom%fastfrac = getparreal( 'params/params_littersom_lpj.dat', 'fastfrac' ) 

  end subroutine getpar_modl_littersom


  subroutine initglobal_littersom()
    !////////////////////////////////////////////////////////////////
    !  Initialisation of all pools on all gridcells at the beginning
    !  of the simulation.
    !----------------------------------------------------------------
    ! ! try:
    ! psoil_fs(:,:) = orgpool( carbon( 10.0 * params_littersom%cton_soil ), nitrogen( 10.0 ) )  
    ! psoil_sl(:,:) = orgpool(carbon(0.0),nitrogen(0.0))  

    psoil_fs(:,:) = orgpool(carbon(0.0),nitrogen(0.0))  
    psoil_sl(:,:) = orgpool(carbon(0.0),nitrogen(0.0))  

  end subroutine initglobal_littersom


  subroutine initdaily_littersom()
    !////////////////////////////////////////////////////////////////
    ! Initialises all daily variables with zero.
    !----------------------------------------------------------------
    drhet(:)  = carbon(0.0)
    drsoil(:) = carbon(0.0)

  end subroutine initdaily_littersom


  subroutine initio_littersom()
    !////////////////////////////////////////////////////////////////
    ! OPEN ASCII OUTPUT FILES FOR OUTPUT
    !----------------------------------------------------------------
    use md_interface

    ! local variables
    character(len=256) :: prefix
    character(len=256) :: filnam

    prefix = "./output/"//trim(interface%params_siml%runname)

    !----------------------------------------------------------------
    ! DAILY OUTPUT
    !----------------------------------------------------------------
    if (interface%params_siml%loutlittersom) then

      ! NET N MINERALISATION
      filnam=trim(prefix)//'.d.netmin.out'
      open(106,file=filnam,err=999,status='unknown')

      ! BIOLOGICAL NITROGEN FIXATION OF FREE-LIVING  ORGANISMS
      filnam=trim(prefix)//'.d.nfixfree.out'
      open(108,file=filnam,err=999,status='unknown')

      ! NET LITTER N MINERALISATION
      filnam=trim(prefix)//'.d.netmin_litt.out'
      open(116,file=filnam,err=999,status='unknown')

      ! NET SOIL N MINERALISATION
      filnam=trim(prefix)//'.d.netmin_soil.out'
      open(117,file=filnam,err=999,status='unknown')

      !----------------------------------------------------------------
      ! ANNUAL OUTPUT
      !----------------------------------------------------------------
      ! LITTER C
      filnam=trim(prefix)//'.a.clitt.out'
      open(301,file=filnam,err=999,status='unknown')

      ! SOIL C
      filnam=trim(prefix)//'.a.csoil.out'
      open(302,file=filnam,err=999,status='unknown')

      ! N REQUIRED FOR LITTER -> SOIL TRANSFER
      filnam=trim(prefix)//'.a.nreq.out'
      open(304,file=filnam,err=999,status='unknown')

      ! C LITTER -> SOIL TRANSFER
      filnam=trim(prefix)//'.a.clit2soil.out'
      open(305,file=filnam,err=999,status='unknown')

      ! N LITTER -> SOIL TRANSFER
      filnam=trim(prefix)//'.a.nlit2soil.out'
      open(306,file=filnam,err=999,status='unknown')

      ! C MINERALISATION FROM SOIL DECOMPOSITION
      filnam=trim(prefix)//'.a.cdsoil.out'
      open(313,file=filnam,err=999,status='unknown')

      ! N MINERALISATION FROM SOIL DECOMPOSITION
      filnam=trim(prefix)//'.a.ndsoil.out'
      open(314,file=filnam,err=999,status='unknown')

      ! N IMMOBILISATION FROM LITTER DECOMPOSITION
      filnam=trim(prefix)//'.a.nimmo.out'
      open(315,file=filnam,err=999,status='unknown')

    end if

    return

    888  stop 'INITIO_littersom: error opening output files'
    999  stop 'INITIO: error opening output files'

  end subroutine initio_littersom


  subroutine initoutput_littersom()
    !////////////////////////////////////////////////////////////////
    !  Initialises littersomance-specific output variables
    !----------------------------------------------------------------
    use md_interface

    if (interface%params_siml%loutlittersom) then
  
      if (interface%steering%init) allocate( outdnetmin(nlu,ndayyear,maxgrid)      )
      if (interface%steering%init) allocate( outdnetmin_soil(nlu,ndayyear,maxgrid) )
      if (interface%steering%init) allocate( outdnetmin_litt(nlu,ndayyear,maxgrid) )
      if (interface%steering%init) allocate( outdnfixfree(nlu,ndayyear,maxgrid)    )

      outdnetmin(:,:,:)      = 0.0
      outdnetmin_soil(:,:,:) = 0.0
      outdnetmin_litt(:,:,:) = 0.0
      outdnfixfree(:,:,:)    = 0.0

      outaclitt(:,:)         = 0.0
      outacsoil(:,:)         = 0.0
      outanreq(:,:)          = 0.0
      outaclit2soil(:,:)     = 0.0
      outanlit2soil(:,:)     = 0.0
      outaNdsoil(:,:)        = 0.0
      outaCdsoil(:,:)        = 0.0
      outaNimmo(:,:)         = 0.0
    
    end if

  end subroutine initoutput_littersom


  subroutine getout_annual_littersom( plant, jpngr )
    !////////////////////////////////////////////////////////////////
    !  SR called once a year to gather annual output variables.
    !----------------------------------------------------------------
    use md_interface
    use md_plant, only: plant_type

    ! arguments
    type(plant_type), dimension(npft), intent(in) :: plant
    integer, intent(in) :: jpngr

    if (interface%params_siml%loutlittersom) then
      outaclitt(:,jpngr) = plant(:)%plitt_af%c%c12 + plant(:)%plitt_as%c%c12 + plant(:)%plitt_bg%c%c12
      outacsoil(:,jpngr) = psoil_sl(:,jpngr)%c%c12 + psoil_fs(:,jpngr)%c%c12
    end if

  end subroutine getout_annual_littersom


  subroutine writeout_ascii_littersom( year )
    !/////////////////////////////////////////////////////////////////////////
    ! WRITE littersom-SPECIFIC VARIABLES TO OUTPUT
    !-------------------------------------------------------------------------
    use md_params_core, only: ndayyear, nmonth
    use md_interface

    ! arguments
    integer, intent(in) :: year       ! simulation year

    ! Local variables
    real :: itime
    integer :: day, moy, jpngr
    
    ! xxx implement this: sum over gridcells? single output per gridcell?
    if (maxgrid>1) stop 'writeout_ascii: think of something ...'
    jpngr = 1

    !-------------------------------------------------------------------------
    ! DAILY OUTPUT
    !-------------------------------------------------------------------------
    if ( .not. interface%steering%spinup &
      .and. interface%steering%outyear>=interface%params_siml%daily_out_startyr &
      .and. interface%steering%outyear<=interface%params_siml%daily_out_endyr ) then

      ! Write daily output only during transient simulation
      do day=1,ndayyear

        ! Define 'itime' as a decimal number corresponding to day in the year + year
        itime = real(interface%steering%outyear) + real(day-1)/real(ndayyear)

        if (nlu>1) stop 'writeout_ascii_littersom: write out lu-area weighted sum'

        ! xxx lu-area weighted sum if nlu>0
        if (interface%params_siml%loutlittersom) then
          write(106,999) itime, sum(outdnetmin(:,day,jpngr))
          write(116,999) itime, sum(outdnetmin_litt(:,day,jpngr))
          write(117,999) itime, sum(outdnetmin_soil(:,day,jpngr))
          write(108,999) itime, sum(outdnfixfree(:,day,jpngr))
        end if

      end do
    end if

    !-------------------------------------------------------------------------
    ! ANNUAL OUTPUT
    ! Write annual value, summed over all PFTs / LUs
    ! xxx implement taking sum over PFTs (and gridcells) in this land use category
    !-------------------------------------------------------------------------
    if (interface%params_siml%loutlittersom) then
    
      itime = real(interface%steering%outyear)

      write(301,999) itime, sum(outaclitt(:,jpngr))
      write(302,999) itime, sum(outacsoil(:,jpngr))
      write(304,999) itime, sum(outanreq(:,jpngr))
      write(305,999) itime, sum(outaclit2soil(:,jpngr))
      write(306,999) itime, sum(outanlit2soil(:,jpngr))
      write(313,999) itime, sum(outaCdsoil(:,jpngr))
      write(314,999) itime, sum(outaNdsoil(:,jpngr))
      write(315,999) itime, sum(outaNimmo(:,jpngr))

    end if

    return
    
    999 format (F20.8,F20.8)

  end subroutine writeout_ascii_littersom

end module md_littersom
