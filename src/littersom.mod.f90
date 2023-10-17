module md_littersom
  !////////////////////////////////////////////////////////////////
  ! Litter and soil organic matter dynamics.
  ! Code partly adapted from LPX-Bern.
  !----------------------------------------------------------------
  use md_classdefs
  use md_params_core
  use md_plant_cnmodel
  use md_tile_cnmodel

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
    ! Litter and soil organic matter dynamics.
    ! Code partly adapted from LPX-Bern.
    !----------------------------------------------------------------
    use md_interface_cnmodel, only: myinterface
    use md_forcing_cnmodel, only: climate_type

    ! arguments
    type(tile_type), dimension(nlu), intent(inout) :: tile
    type(tile_fluxes_type), dimension(nlu), intent(inout) :: tile_fluxes
    type(climate_type), intent(in) :: climate
    integer, intent(in) :: doy

    ! local variables
    integer :: lu                                   ! counter variable for landuse class

    ! temperature/soil moisture-modified decay constants
    real :: klitt_af               ! decay rate, above-ground fast (leaf) litter (= k_litter_leaf)
    real :: klitt_as               ! decay rate, above-ground slow (woody) litter (= k_litter_woody)
    real :: klitt_bg               ! decay rate, below-ground fast litter (= k_litter_root)
    real :: kexu                   ! decay rate, exudates (= k_exu)
    real :: ksoil_fs               ! decay rate, fast soil (= k_fast)
    real :: ksoil_sl               ! decay rate, slow soil (= k_slow)

    ! temporary _pools
    type(carbon)  :: dexu                           ! exudates decomposed in time step (= exu_decom)
    type(orgpool) :: dlitt                          ! total litter decomposed per time step
    type(orgpool) :: dsoil                          ! total soil decomposed per time step
    type(orgpool) :: dlitt_af                       ! above-ground fast litter decomposed per time step (= litterdag_fast)
    type(orgpool) :: dlitt_as                       ! above-ground slow litter decomposed per time step (= litterdag_slow)
    type(orgpool) :: dlitt_bg                       ! below-ground slow litter decomposed per time step (= litter_decom_bg)
    type(orgpool) :: dsoil_sl                       ! (= cflux_fast_atmos)
    type(orgpool) :: dsoil_fs                       ! (= cflux_fast_atmos)
   
    ! temporary variables
    real :: eff                                     ! microbial growth efficiency 
    real :: ntoc_crit                               ! critical N:C ratio below which immobilisation occurrs  
    real :: nreq                                    ! N required in litter decomposition to maintain SOM C:N
    real :: nfix                                    ! temporary variable, N fixation implied in litter decomposition,
    real :: rest                                    ! temporary variable
    real :: req                                     ! N required for litter decomposition 
    real :: avl                                     ! mineral N available as inorganic N
    real :: netmin_litt                             ! net N mineralisation from litter decomposition
    real :: netmin_soil                             ! net N mineralisation from soil decomposition

    integer, save :: ncall = 0                 ! internally counted simulation year

    real, dimension(nlu), save :: mean_insoil_fs
    real, dimension(nlu), save :: mean_insoil_sl
    real, dimension(nlu), save :: mean_ksoil_sl
    real, dimension(nlu), save :: mean_ksoil_fs

    real :: ntoc_save_fs, ntoc_save_sl

    ! xxx debug
    real :: nbal1, nbal2

    !-------------------------------------------------------------------------
    ! Count number of calls (one for each simulation year)
    !-------------------------------------------------------------------------
    if (doy==1) ncall = ncall + 1

    ! initialise average fluxes
    if (ncall==1 .and. doy==1) then
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
      klitt_af  = params_littersom%klitt_af10 * &
        ftemp( climate%dtemp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )
      
      klitt_as = params_littersom%klitt_as10 * &
        ftemp( climate%dtemp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )
      
      klitt_bg = params_littersom%klitt_bg10 * &
        ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )
      
      kexu = params_littersom%kexu10 * &
        ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )

      !-------------------------------------------------------------------------
      ! SOIL TEMPERATURE AND MOISTURE MODIFIER
      ! Temperature: Lloyd & Taylor 1994
      ! Moisture: Foley, 1995; Fang and Moncrieff, 1999; Gerten et al., 2004;
      !           Wania et al., 2009; Frolking et al., 2010; Spahni et al., 2012
      !-------------------------------------------------------------------------
      ksoil_fs = params_littersom%ksoil_fs10 * &
        ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )

      ksoil_sl = params_littersom%ksoil_sl10 * &
        ftemp( tile(lu)%soil%phy%temp, "lloyd_and_taylor" ) * &
        fmoist( tile(lu)%soil%phy%wscal, "foley" )


      ! ! record for experimental output
      ! tile_fluxes(lu)%plant(1)%debug1 = ksoil_fs
      ! tile_fluxes(lu)%plant(1)%debug2 = klitt_af
      ! tile_fluxes(lu)%plant(1)%debug3 = tile(lu)%soil%phy%temp
      ! tile_fluxes(lu)%plant(1)%debug4 = tile(lu)%soil%phy%wscal  ! wscal is not stable!!!


      !////////////////////////////////////////////////////////////////
      ! LITTER DECAY
      ! Collect litter decomposition into LU-specific 
      ! pool 'dlitt'.
      ! All goes to daily updated litter decomposition pool
      !----------------------------------------------------------------

      ! ! xxx debug: n_litter + n_soil + no3 + nh4 + nfix_free = const.
      ! nbal1 = tile(lu)%soil%psoil_fs%n%n14 &
      !       + tile(lu)%soil%psoil_sl%n%n14 &
      !       + tile(lu)%soil%plitt_af%n%n14 &
      !       + tile(lu)%soil%plitt_as%n%n14 &
      !       + tile(lu)%soil%plitt_bg%n%n14 &
      !       + tile(lu)%soil%pno3%n14 &
      !       + tile(lu)%soil%pnh4%n14 &
      !       + tile_fluxes(lu)%soil%dnfix_free

      !-------------------------------------------------------------------------
      ! Initialisation of decomposing pool 
      ! (temporary, decomposition for each LU-class).
      !-------------------------------------------------------------------------
      call orginit( dlitt )

      ! ! xxx debug
      ! nbal1 = tile(lu)%soil%plitt_af%n%n14 + tile(lu)%soil%plitt_as%n%n14 + tile(lu)%soil%plitt_bg%n%n14 + dlitt%n%n14

      ! amount of litter decay
      dlitt_af = orgfrac( 1.0 - exp( -klitt_af ), tile(lu)%soil%plitt_af )
      dlitt_as = orgfrac( 1.0 - exp( -klitt_as ), tile(lu)%soil%plitt_as )
      dlitt_bg = orgfrac( 1.0 - exp( -klitt_bg ), tile(lu)%soil%plitt_bg )

      ! Update the litter _pools
      call orgmv( dlitt_af, tile(lu)%soil%plitt_af, dlitt )
      call orgmv( dlitt_as, tile(lu)%soil%plitt_as, dlitt )
      call orgmv( dlitt_bg, tile(lu)%soil%plitt_bg, dlitt )

      ! ! xxx debug - this is ok
      ! nbal2 = tile(lu)%soil%plitt_af%n%n14 + tile(lu)%soil%plitt_as%n%n14 + tile(lu)%soil%plitt_bg%n%n14 + dlitt%n%n14
      ! print*,'A: nbal: ', nbal2 - nbal1
      ! if (abs(nbal2 - nbal1) > eps) stop 'A: balance not satisfied for N'
  
      !////////////////////////////////////////////////////////////////
      ! EXUDATES DECAY
      ! Calculate the exudates respiration before litter respiration.
      ! Exudates are mostly short organic compounds (poly- and mono-
      ! saccharides, amino acids, organic acids, phenolic compounds and
      ! enzymes) and are quickly respired and released as CO2.
      ! Exudates decay goes to soil respiration 'drhet'.
      ! This is executed after litter mineralisation as N fixation by 
      ! free-living bacteria is driven by exudates availability.
      !----------------------------------------------------------------
      dexu = cfrac( 1.0 - exp(-kexu), tile(lu)%soil%pexud )
      call cmv( dexu, tile(lu)%soil%pexud, tile_fluxes(lu)%soil%drhet )

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
        ntoc_crit = params_littersom%ntoc_crit1 * ntoc( dlitt, default = 0.0 ) ** params_littersom%ntoc_crit2  ! = rCR
        eff = ntoc_crit * params_littersom%cton_soil

        if (eff > 1.0) eff = 1.0

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
        call ccp( cfrac( eff *        params_littersom%fastfrac , dlitt%c ), tile(lu)%soil%psoil_fs%c )
        call ccp( cfrac( eff * (1.0 - params_littersom%fastfrac), dlitt%c ), tile(lu)%soil%psoil_sl%c )

        ! move fraction '(1-eff)' of C to heterotrophic respiration
        call ccp( cfrac( (1.0 - eff), dlitt%c ), tile_fluxes(lu)%soil%drhet )

        ! get average litter -> soil flux for analytical soil C equilibration
        if ( myinterface%steering%average_soil ) then
          mean_insoil_fs(lu) = mean_insoil_fs(lu) + eff *        params_littersom%fastfrac  * dlitt%c%c12
          mean_insoil_sl(lu) = mean_insoil_sl(lu) + eff * (1.0 - params_littersom%fastfrac) * dlitt%c%c12
        end if

        !----------------------------------------------------------------    
        ! N MINERALISATION
        !----------------------------------------------------------------    
        ! N requirement to maintain rS (SOM N:C ratio)
        nreq = dlitt%c%c12 * eff * params_littersom%ntoc_soil  ! 1/cton_soil = rS

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
        netmin_litt = dlitt%n%n14 - nreq

        nfix = 0.0

        ! net mineralisation from litter decomposition is typically negative (net immobilisation)
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

          if (avl >= req) then

            ! enough mineral N for immobilisation
            tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 - req
            req = 0.0

          else

            ! not enough NH4 for immobilisation
            tile(lu)%soil%pnh4%n14 = 0.0
            req = req - avl

            !----------------------------------------------------------------    
            ! Immobilisation: second deplete NO3 pool
            !----------------------------------------------------------------    
            avl = tile(lu)%soil%pno3%n14

            if (avl >= req) then

              ! enough mineral N for immobilisation
              tile(lu)%soil%pno3%n14 = tile(lu)%soil%pno3%n14 - req
              req = 0.0

            else

              ! not enough NO3 for immobilisation
              tile(lu)%soil%pno3%n14 = 0.0
              req = req - avl

              !----------------------------------------------------------------    
              ! N fixation by free-living bacteria in litter to satisfy remainder
              !----------------------------------------------------------------    
              nfix = req
              print*,'implied N fixation: ', nfix
              req = 0.0
              ! stop 'could not get enough N upon immobilisation'

            end if

          end if
          
        end if

        ! record N required for balance as free-living N fixation
        tile_fluxes(lu)%soil%dnfix_free = tile_fluxes(lu)%soil%dnfix_free + nfix

        ! ! xxx debug
        ! nbal1 = tile(lu)%soil%psoil_fs%n%n14 + tile(lu)%soil%psoil_sl%n%n14 + nreq

        ! Move N to soil pools
        call ncp( nfrac( params_littersom%fastfrac        , nitrogen( nreq ) ), tile(lu)%soil%psoil_fs%n )
        call ncp( nfrac( (1.0 - params_littersom%fastfrac), nitrogen( nreq ) ), tile(lu)%soil%psoil_sl%n )

        ! tile(lu)%soil%psoil_fs%n*n14 = tile(lu)%soil%psoil_fs%c%c12 * params_littersom%ntoc_soil
        ! tile(lu)%soil%psoil_sl%n*n14 = tile(lu)%soil%psoil_sl%c%c12 * params_littersom%ntoc_soil

        ! ! xxx debug - this is ok
        ! nbal2 = tile(lu)%soil%psoil_fs%n%n14 + tile(lu)%soil%psoil_sl%n%n14
        ! print*,'C: nbal: ', nbal2 - nbal1
        ! if (abs(nbal2 - nbal1) > eps) stop 'C: balance not satisfied for N'

        ! if ( abs( cton(tile(lu)%soil%psoil_fs) - params_littersom%cton_soil ) > 1e-5 ) then
        !   write(0,*) 'psoil_fs', cton( tile(lu)%soil%psoil_fs )
        !   stop 'B fs: C:N not ok'
        ! end if
        ! if ( abs( cton(tile(lu)%soil%psoil_sl) - params_littersom%cton_soil ) > 1e-5 ) then
        !   write(0,*) 'psoil_sl', cton( tile(lu)%soil%psoil_sl )
        !   stop 'B sl: C:N not ok'
        ! end if

      end if

      ! ! xxx debug: n_litter + n_soil + no3 + nh4 + nfix_free = const. - is ok
      ! nbal2 = tile(lu)%soil%psoil_fs%n%n14 &
      !       + tile(lu)%soil%psoil_sl%n%n14 &
      !       + tile(lu)%soil%plitt_af%n%n14 &
      !       + tile(lu)%soil%plitt_as%n%n14 &
      !       + tile(lu)%soil%plitt_bg%n%n14 &
      !       + tile(lu)%soil%pno3%n14 &
      !       + tile(lu)%soil%pnh4%n14 &
      !       + tile_fluxes(lu)%soil%dnfix_free
      ! print*,'nbal, dnfix_free: ', nbal2 - nbal1, tile_fluxes(lu)%soil%dnfix_free
      ! if (abs(nbal2 - nbal1) > eps) stop 'A: balance not satisfied for N'

      !////////////////////////////////////////////////////////////////
      ! SOIL DECAY
      !----------------------------------------------------------------
      ! Calculate daily/monthly soil decomposition to the atmosphere

      ! ! xxx debug: n_litter + n_soil + no3 + nh4 + nfix_free = const.
      ! nbal1 = tile(lu)%soil%psoil_fs%n%n14 &
      !       + tile(lu)%soil%psoil_sl%n%n14 &
      !       + tile(lu)%soil%pnh4%n14
      !       ! + tile(lu)%soil%pno3%n14 &
      !       ! + tile_fluxes(lu)%soil%dnfix_free

      ! decomposing amount is first added to "temporary pool" dsoil
      call orginit( dsoil )
      dsoil_fs = orgfrac( (1.0 - exp(-ksoil_fs)), tile(lu)%soil%psoil_fs )
      dsoil_sl = orgfrac( (1.0 - exp(-ksoil_sl)), tile(lu)%soil%psoil_sl )
      call orgmv( dsoil_fs, tile(lu)%soil%psoil_fs, dsoil )
      call orgmv( dsoil_sl, tile(lu)%soil%psoil_sl, dsoil )

      ! N is all mineralised to NH4
      tile(lu)%soil%pnh4 = nplus(tile(lu)%soil%pnh4, dsoil%n)
      tile_fluxes(lu)%soil%dnetmin = nplus(tile_fluxes(lu)%soil%dnetmin, dsoil%n)

      ! C is all respired (heterotropic respiration)
      tile_fluxes(lu)%soil%drhet = cplus( tile_fluxes(lu)%soil%drhet, dsoil%c )

      ! Spinup trick: use projected soil N mineralisation before soil equilibration
      if ( myinterface%steering%project_nmin ) then
        ! projected soil N mineralisation
        if (dlitt%c%c12 > 0.0) tile(lu)%soil%pnh4%n14 = tile(lu)%soil%pnh4%n14 + eff * dlitt%c%c12 / params_littersom%cton_soil
      end if

      ! get average litter -> soil flux for analytical soil C equilibration
      if ( myinterface%steering%average_soil ) then
        mean_ksoil_fs(lu) = mean_ksoil_fs(lu) + ksoil_fs
        mean_ksoil_sl(lu) = mean_ksoil_sl(lu) + ksoil_sl
      end if

      ! ! xxx debug: n_litter + n_soil + no3 + nh4 + nfix_free = const. - is ok
      ! nbal2 = tile(lu)%soil%psoil_fs%n%n14 &
      !       + tile(lu)%soil%psoil_sl%n%n14 &
      !       + tile(lu)%soil%pnh4%n14
      !       ! + tile(lu)%soil%pno3%n14 &
      !       ! + tile_fluxes(lu)%soil%dnfix_free

      ! if (.not. myinterface%steering%project_nmin) print*,'nbal : ', nbal2 - nbal1
      ! if (abs(nbal2 - nbal1) > eps .and. (.not. myinterface%steering%project_nmin)) stop 'Balance not satisfied for N'

      ! analytical soil C equilibration
      if ( myinterface%steering%do_soilequil .and. doy == ndayyear ) then
        tile(lu)%soil%psoil_fs%c%c12 = mean_insoil_fs(lu) / mean_ksoil_fs(lu)
        tile(lu)%soil%psoil_sl%c%c12 = mean_insoil_sl(lu) / mean_ksoil_sl(lu)
        tile(lu)%soil%psoil_fs%n%n14 = tile(lu)%soil%psoil_fs%c%c12 * ntoc_save_fs
        tile(lu)%soil%psoil_sl%n%n14 = tile(lu)%soil%psoil_sl%c%c12 * ntoc_save_sl
        mean_insoil_fs(lu) = 0.0
        mean_insoil_sl(lu) = 0.0
        mean_ksoil_fs(lu)  = 0.0
        mean_ksoil_sl(lu)  = 0.0
      end if

    enddo luloop

  end subroutine littersom


  subroutine getpar_modl_littersom()
    !////////////////////////////////////////////////////////////////
    ! Subroutine reads littersom module-specific parameters 
    ! from input file
    !----------------------------------------------------------------
    use md_interface_cnmodel, only: myinterface

    ! above-ground fast (foliage and roots) litter decay rate [1/d] 
    params_littersom%klitt_af10    = myinterface%params_calib%klitt_af10 / ndayyear

    ! above-ground slow (woody) litter decay rate [1/d] 
    params_littersom%klitt_as10    = myinterface%params_calib%klitt_as10 / ndayyear

    ! below-ground (root) litter decay rate [1/d] 
    params_littersom%klitt_bg10    = myinterface%params_calib%klitt_bg10 / ndayyear
    
    ! exudates decay rate [1/d]
    params_littersom%kexu10        = myinterface%params_calib%kexu10 / ndayyear

    ! fast soil pool decay rate [1/d]
    params_littersom%ksoil_fs10    = myinterface%params_calib%ksoil_fs10 / ndayyear

    ! slow soil pool decay rate [1/d]    
    params_littersom%ksoil_sl10    = myinterface%params_calib%ksoil_sl10 / ndayyear

    ! factor for "Manzoni Equation" (XPXXX) [1]
    params_littersom%ntoc_crit1    = myinterface%params_calib%ntoc_crit1

    ! exponent for "Manzoni Equation" (XPXXX) [1]
    params_littersom%ntoc_crit2    = myinterface%params_calib%ntoc_crit2

    ! params_littersom%cton_microb = myinterface%params_calib%cton_microb 10.0

    ! C                            :N ratio of SOM
    params_littersom%cton_soil     = myinterface%params_calib%cton_soil

    ! N                            :C ratio of SOM
    params_littersom%ntoc_soil     = 1.0 / params_littersom%cton_soil

    ! fraction of litter input to fast soil pool [1]
    params_littersom%fastfrac      = myinterface%params_calib%fastfrac

  end subroutine getpar_modl_littersom


end module md_littersom
