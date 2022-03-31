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
  use md_params_core
  use md_plant
  use md_tile

  implicit none
  
  private 
  public getpar_modl_littersom, littersom

  !-----------------------------------------------------------------------
  ! Parameters determining soil organic matter dynamics
  !-----------------------------------------------------------------------
  type paramstype_littersom
    real :: klitt_af10
    real :: klitt_as10
    real :: klitt_bg10
    real :: kexu10
    real :: ksoil_fs10
    real :: ksoil_sl10
    real :: ntoc_crit1
    real :: ntoc_crit2
    real :: cton_soil
    real :: ntoc_soil
    real :: fastfrac
  end type
    
  type( paramstype_littersom ) :: params_littersom


contains

  subroutine littersom( tile, tile_fluxes, climate, doy )
    !////////////////////////////////////////////////////////////////
    !  Litter and SOM decomposition and nitrogen mineralisation.
    !  1st order decay of litter and SOM _pools, governed by temperature
    !  and soil moisture following LPJ (Sitch et al., 2003) and 
    !  Xu-Ri & Prentice (XXX).
    !  June 2014
    !  b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    use md_interface_pmodel, only: myinterface
    use md_forcing_pmodel, only: climate_type

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(climate_type), intent(in) :: climate
    integer, intent(in) :: doy

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

    real, dimension(nlu), save :: mean_insoil_fs
    real, dimension(nlu), save :: mean_insoil_sl
    real, dimension(nlu), save :: mean_ksoil_sl
    real, dimension(nlu), save :: mean_ksoil_fs

    real :: ntoc_save_fs, ntoc_save_sl

    !-------------------------------------------------------------------------
    ! Count number of calls (one for each simulation year)
    !-------------------------------------------------------------------------
    if (doy==1) invocation = invocation + 1

    ! initialise average fluxes
    if (invocation==1 .and. doy==1) then
      mean_insoil_fs(:) = 0.0
      mean_insoil_sl(:) = 0.0
      mean_ksoil_fs(:)  = 0.0
      mean_ksoil_sl(:)  = 0.0
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

    luloop: do lu=1,nlu
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
      
      !-------------------------------------------------------------------------
      ! LITTER TEMPERATURE AND MOISTURE MODIFIER
      ! Temperature: Lloyd & Taylor 1994, Brovkin et al., 2012
      ! Moisture: Foley, 1995; Fang and Moncrieff, 1999; Gerten et al., 2004;
      ! Wania et al., 2009; Frolking et al., 2010; Spahni et al., 2012
      !-------------------------------------------------------------------------
      ! define decomposition _rates for current soil temperature and moisture 
      klitt_af(lu) = params_littersom%klitt_af10 * &
        ftemp( climate%dtemp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )
      
      klitt_as(lu) = params_littersom%klitt_as10 * &
        ftemp( climate%dtemp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )
      
      klitt_bg(lu) = params_littersom%klitt_bg10 * &
        ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )
      
      kexu(lu) = params_littersom%kexu10 * &
        ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )

      !-------------------------------------------------------------------------
      ! SOIL TEMPERATURE AND MOISTURE MODIFIER
      ! Temperature: Lloyd & Taylor 1994
      ! Moisture: Foley, 1995; Fang and Moncrieff, 1999; Gerten et al., 2004;
      !           Wania et al., 2009; Frolking et al., 2010; Spahni et al., 2012
      !-------------------------------------------------------------------------
      ksoil_fs(lu) = params_littersom%ksoil_fs10 * &
        ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )

      ksoil_sl(lu) = params_littersom%ksoil_sl10 * &
        ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )

      !////////////////////////////////////////////////////////////////
      ! LITTER DECAY
      ! Collect PFT-specific litter decomposition into LU-specific 
      ! pool 'dlitt'.
      ! All goes to daily updated litter decomposition pool
      !----------------------------------------------------------------

      !-------------------------------------------------------------------------
      ! Initialisation of decomposing pool 
      ! (temporary, decomposition for each LU-class).
      !-------------------------------------------------------------------------
      call orginit(dlitt)

      ! amount of litter decay
      dlitt_af = orgfrac( 1.0 - exp( -klitt_af(lu)), tile(lu)%soil%plitt_af )
      dlitt_as = orgfrac( 1.0 - exp( -klitt_as(lu)), tile(lu)%soil%plitt_as )
      dlitt_bg = orgfrac( 1.0 - exp( -klitt_bg(lu)), tile(lu)%soil%plitt_bg )

      ! Update the litter _pools
      call orgmv( dlitt_af(pft), tile(lu)%soil%plitt_af, dlitt )
      call orgmv( dlitt_as(pft), tile(lu)%soil%plitt_as, dlitt )
      call orgmv( dlitt_bg(pft), tile(lu)%soil%plitt_bg, dlitt )
  
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
      dexu = cfrac( 1.0 - exp(-kexu(lu)), tile(lu)%soil%pexud )
      call cmv( dexu, tile(lu)%soil%pexud, tile_fluxes(lu)%soil%drsoil )

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
        ntoc_save_fs = ntoc( tile(lu)%soil%psoil_fs, default = 0.0 )
        ntoc_save_sl = ntoc( tile(lu)%soil%psoil_sl, default = 0.0 )

        ! move fraction 'eff' of C from litter to soil
        call ccp( cfrac( eff*params_littersom%fastfrac        , dlitt%c ), tile(lu)%soil%psoil_fs%c )
        call ccp( cfrac( eff*(1.0 - params_littersom%fastfrac), dlitt%c ), tile(lu)%soil%psoil_sl%c )

        ! move fraction '(1-eff)' of C to heterotrophic respiration
        call ccp( cfrac( (1.0 - eff), dlitt%c ), tile_fluxes(lu)%soil%drhet )

        ! get average litter -> soil flux for analytical soil C equilibration
        if ( myinterface%steering%average_soil ) then
          mean_insoil_fs(lu) = mean_insoil_fs(lu) + eff * params_littersom%fastfrac * dlitt%c%c12
          mean_insoil_sl(lu) = mean_insoil_sl(lu) + eff * (1.0 - params_littersom%fastfrac) * dlitt%c%c12
        end if

        !----------------------------------------------------------------    
        ! N MINERALISATION
        !----------------------------------------------------------------    
        ! N requirement to maintain rS (SOM N:C ratio)
        Nreq_S = dlitt%c%c12 * eff * params_littersom%ntoc_soil  ! 1/cton_soil = rS

        ! ! OUTPUT COLLECTION
        ! outanreq(lu)      = outanreq(lu)      + Nreq_S
        ! outaclit2soil(lu) = outaclit2soil(lu) + dlitt%c%c12 * eff
        ! outanlit2soil(lu) = outanlit2soil(lu) + Nreq_S

        ! write(0,*) 'outaclit2soil(lu)',outaclit2soil(lu)
        ! outanlit2soil(pft) = outanlit2soil(pft) + dlitt%n%n14

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
        ! tile_fluxes(lu)%soil%dnetmin = nplus( tile_fluxes(lu)%soil%dnetmin, netmin_litt )
        tile_fluxes(lu)%soil%dnetmin%n14 = tile_fluxes(lu)%soil%dnetmin%n14 + netmin_litt
        
        if (netmin_litt > 0.0) then
          !----------------------------------------------------------------    
          ! net N mineralisation
          !----------------------------------------------------------------    
          tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 + netmin_litt
          
        else

          if ( (-1.0 * netmin_litt) > (tile(lu)%soil%pnh4%n14 + tile(lu)%soil%pno3%n14) ) print*, 'too much immo'

          !----------------------------------------------------------------    
          ! Immobilisation: first deplete NH4 pool
          !----------------------------------------------------------------    
          req = -1.0 * netmin_litt
          avl = tile(lu)%soil%pnh4%n14

          if (avl>=req) then
            ! enough mineral N for immobilisation
            tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 - req
            req = 0.0
          else
            ! not enough NH4 for immobilisation
            tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 - avl
            req = req - avl

            !----------------------------------------------------------------    
            ! Immobilisation: second deplete NO3 pool
            !----------------------------------------------------------------    
            avl = tile(lu)%soil%pno3%n14

            if (avl>=req) then
              ! enough mineral N for immobilisation
              tile(lu)%soil%pno3%n14 = tile(lu)%soil%pno3%n14 - req
              req = 0.0
            else
              ! not enough NO3 for immobilisation
              tile(lu)%soil%pno3%n14 = tile(lu)%soil%pno3%n14 - avl
              req = req - avl

              !----------------------------------------------------------------    
              ! N fixation by free-living bacteria in litter to satisfy remainder
              !----------------------------------------------------------------    
              Nfix = req
              ! print*,'req ', req
              req = 0.0
              ! stop 'could not get enough N upon immobilisation'

            end if

          end if
          
        end if

        ! Nreq_S (= dlitt - netmin) remains in the system: 
        call ncp( nfrac( params_littersom%fastfrac        , nitrogen(Nreq_S) ), tile(lu)%soil%psoil_fs%n )
        call ncp( nfrac( (1.0 - params_littersom%fastfrac), nitrogen(Nreq_S) ), tile(lu)%soil%psoil_sl%n )

        ! if ( abs( cton(tile(lu)%soil%psoil_fs) - params_littersom%cton_soil ) > 1e-5 ) then
        !   write(0,*) 'psoil_fs', cton( tile(lu)%soil%psoil_fs )
        !   stop 'B fs: C:N not ok'
        ! end if
        ! if ( abs( cton(tile(lu)%soil%psoil_sl) - params_littersom%cton_soil ) > 1e-5 ) then
        !   write(0,*) 'psoil_sl', cton( tile(lu)%soil%psoil_sl )
        !   stop 'B sl: C:N not ok'
        ! end if

        ! ! OUTPUT COLLECTION
        ! if (myinterface%params_siml%loutlittersom) then
        !   outdnfixfree(lu,doy) = outdnfixfree(lu,doy) + Nfix
        ! end if

      end if

      !////////////////////////////////////////////////////////////////
      ! SOIL DECAY
      !----------------------------------------------------------------
      ! Calculate daily/monthly soil decomposition to the atmosphere

      ! record N:C ratio to override later (compensating for numerical imprecision)
      ntoc_save_fs = ntoc( tile(lu)%soil%psoil_fs, default = 0.0 )
      ntoc_save_sl = ntoc( tile(lu)%soil%psoil_sl, default = 0.0 )

      dsoil_fs = orgfrac( (1.0 - exp(-ksoil_fs(lu))), tile(lu)%soil%psoil_fs )
      dsoil_sl = orgfrac( (1.0 - exp(-ksoil_sl(lu))), tile(lu)%soil%psoil_sl )

      ! soil decay
      tile(lu)%soil%psoil_fs = orgminus( tile(lu)%soil%psoil_fs, dsoil_fs )
      tile(lu)%soil%psoil_sl = orgminus( tile(lu)%soil%psoil_sl, dsoil_sl )
      
      ! C to heterotrophic respiration
      tile_fluxes(lu)%soil%drhet = cplus( tile_fluxes(lu)%soil%drhet, dsoil_fs%c, dsoil_sl%c )

      ! Spinup trick: use projected soil N mineralisation before soil equilibration
      ! if ( myinterface%steering%project_nmin ) then
      !   ! projected soil N mineralisation
      !   if (dlitt%c%c12 > 0.0) tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 + eff * dlitt%c%c12 / params_littersom%cton_soil
      ! else
      !   ! actual soil N mineralisation
      !   tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 + dsoil_fs%n%n14 + dsoil_sl%n%n14
      ! end if

      ! if ( tile(lu)%soil%psoil_fs%c%c12 > 0.0 .and. abs( cton( tile(lu)%soil%psoil_fs, default=0.0 ) - params_littersom%cton_soil ) > 1e-4 ) then
      !   write(0,*) 'psoil_fs', cton( tile(lu)%soil%psoil_fs )
      !   stop 'C fs: C:N not ok'
      ! end if
      ! if ( tile(lu)%soil%psoil_sl%c%c12 > 0.0 .and. abs( cton( tile(lu)%soil%psoil_sl, default=0.0 ) - params_littersom%cton_soil ) > 1e-4 ) then
      !   write(0,*) 'psoil_sl', cton( tile(lu)%soil%psoil_sl )
      !   stop 'C sl: C:N not ok'
      ! end if
      
      ! get average litter -> soil flux for analytical soil C equilibration
      if ( myinterface%steering%average_soil ) then
        mean_ksoil_fs(lu) = mean_ksoil_fs(lu) + ksoil_fs(lu)
        mean_ksoil_sl(lu) = mean_ksoil_sl(lu) + ksoil_sl(lu)
      end if

      ! analytical soil C equilibration
      if ( myinterface%steering%do_soilequil .and. doy==ndayyear ) then
        tile(lu)%soil%psoil_fs%c%c12 = mean_insoil_fs(lu) / mean_ksoil_fs(lu)
        tile(lu)%soil%psoil_sl%c%c12 = mean_insoil_sl(lu) / mean_ksoil_sl(lu)
        tile(lu)%soil%psoil_fs%n%n14 = tile(lu)%soil%psoil_fs%c%c12 * ntoc_save_fs
        tile(lu)%soil%psoil_sl%n%n14 = tile(lu)%soil%psoil_sl%c%c12 * ntoc_save_sl
        mean_insoil_fs(lu) = 0.0
        mean_insoil_sl(lu) = 0.0
        mean_ksoil_fs(lu)  = 0.0
        mean_ksoil_sl(lu)  = 0.0
      end if
 
      ! OUTPUT COLLECTION
      ! tile_fluxes(lu)%soil%dnetmin = nplus( tile_fluxes(lu)%soil%dnetmin, dsoil_fs%n%n14, dsoil_sl%n%n14 )
      tile_fluxes(lu)%soil%dnetmin%n14 = tile_fluxes(lu)%soil%dnetmin%n14 + dsoil_fs%n%n14 + dsoil_sl%n%n14

    enddo luloop

  
  end subroutine littersom


  subroutine getpar_modl_littersom()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads littersom module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    ! above-ground fast (foliage and roots) litter decay rate [1/d] 
    params_littersom%klitt_af10  = 1.2 / ndayyear

    ! above-ground slow (woody) litter decay rate [1/d] 
    params_littersom%klitt_as10  = 0.35 / ndayyear

    ! below-ground (root) litter decay rate [1/d] 
    params_littersom%klitt_bg10  = 0.35 / ndayyear
    
    ! exudates decay rate [1/d]
    params_littersom%kexu10      = 50.0 / ndayyear

    ! fast soil pool decay rate [1/d]
    params_littersom%ksoil_fs10  = 0.021 / ndayyear

    ! slow soil pool decay rate [1/d]    
    params_littersom%ksoil_sl10  = 7.0e-04 / ndayyear

    ! factor for "Manzoni Equation" (XPXXX) [1]
    params_littersom%ntoc_crit1  = 0.45

    ! exponent for "Manzoni Equation" (XPXXX) [1]
    params_littersom%ntoc_crit2  = 0.76

    ! params_littersom%cton_microb = 10.0

    ! C:N ratio of SOM
    params_littersom%cton_soil   = 9.77

    ! N:C ratio of SOM
    params_littersom%ntoc_soil = 1.0 / params_littersom%cton_soil

    ! fraction of litter input to fast soil pool [1]
    params_littersom%fastfrac    = 0.985

  end subroutine getpar_modl_littersom


end module md_littersom
