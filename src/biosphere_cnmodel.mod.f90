module md_biosphere_cnmodel

  use md_params_core
  use md_classdefs
  use md_sofunutils, only: calc_patm
  use md_tile, only: tile_type, tile_fluxes_type, initglobal_tile, initdaily_tile_fluxes, &
    getpar_modl_tile, diag_daily, diag_annual, init_annual
  use md_plant, only: getpar_modl_plant
  use md_waterbal, only: waterbal, solar, getpar_modl_waterbal
  use md_gpp_pmodel, only: getpar_modl_gpp, gpp
  use md_vegdynamics_pmodel, only: vegdynamics
  use md_soiltemp, only: soiltemp
  use md_npp, only: npp

  implicit none

  private
  public biosphere_annual

  !----------------------------------------------------------------
  ! Module-specific (private) variables
  !----------------------------------------------------------------
  ! derived types from L1 modules
  type(tile_type),        dimension(nlu) :: tile             ! has gridcell-dimension because values are stored between years
  type(tile_fluxes_type), dimension(nlu) :: tile_fluxes      ! has no gridcell-dimension values need not be recorded

contains

  function biosphere_annual() result( out_biosphere )
    !////////////////////////////////////////////////////////////////
    ! function BIOSPHERE_annual calculates net ecosystem exchange (nee)
    ! in response to environmental boundary conditions (atmospheric 
    ! CO2, temperature, Nitrogen deposition. This SR "replaces" 
    ! LPJ, also formulated as subroutine.
    ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
    ! contact: b.stocker@imperial.ac.uk
    !----------------------------------------------------------------
    use md_interface_pmodel, only: myinterface, outtype_biosphere
  
    ! return variable
    type(outtype_biosphere) :: out_biosphere

    ! local variables
    integer :: dm, moy, doy
    logical, save           :: init_daily            ! is true only on the first day of the simulation 
    logical, parameter      :: verbose = .false.     ! change by hand for debugging etc.

    !----------------------------------------------------------------
    ! INITIALISATIONS
    !----------------------------------------------------------------
    if (myinterface%steering%init) then

      ! set to true on first simulation year and first day
      init_daily = .true.

      !----------------------------------------------------------------
      ! GET MODEL PARAMETERS
      ! read model parameters that may be varied for optimisation
      !----------------------------------------------------------------
      ! if (verbose) print*, 'getpar_modl() ...'
      call getpar_modl_tile()
      call getpar_modl_plant()
      call getpar_modl_waterbal()
      call getpar_modl_gpp()
      ! if (verbose) print*, '... done'

      !----------------------------------------------------------------
      ! Initialise pool variables and/or read from restart file (not implemented)
      !----------------------------------------------------------------
      ! if (verbose) print*, 'initglobal_() ...'
      call initglobal_tile( tile(:) )
      ! if (verbose) print*, '... done'

    endif 

    !----------------------------------------------------------------
    ! Set annual sums to zero
    !----------------------------------------------------------------
    call init_annual( tile_fluxes(:) )

    !----------------------------------------------------------------
    ! LOOP THROUGH MONTHS
    !----------------------------------------------------------------
    doy = 0
    monthloop: do moy = 1,nmonth

      !----------------------------------------------------------------
      ! LOOP THROUGH DAYS
      !----------------------------------------------------------------
      dayloop: do dm=1,ndaymonth(moy)
        doy = doy + 1

        ! if (verbose) print*,'----------------------'
        ! if (verbose) print*,'YEAR, Doy ', myinterface%steering%year, doy
        ! if (verbose) print*,'----------------------'

        !----------------------------------------------------------------
        ! initialise updated variables (fluxes)
        !----------------------------------------------------------------
        ! if (verbose) print*,'calling initdaily_() ...'
        call initdaily_tile_fluxes( tile_fluxes(:) )
        ! if (verbose) print*,'... done.'

        !----------------------------------------------------------------
        ! Get radiation based on daily temperature, sunshine fraction, and 
        ! elevation.
        !----------------------------------------------------------------
        ! if (verbose) print*,'calling solar() ... '
        ! if (verbose) print*,'    with argument lat = ', myinterface%grid%lat
        ! if (verbose) print*,'    with argument elv = ', myinterface%grid%elv
        ! if (verbose) print*,'    with argument dfsun (ann. mean) = ', sum( myinterface%climate(:)%dfsun / ndayyear )
        ! if (verbose) print*,'    with argument dppfd (ann. mean) = ', sum( myinterface%climate(:)%dppfd / ndayyear )
        call solar( tile_fluxes(:), &
                    myinterface%grid, & 
                    myinterface%climate(doy),  &
                    doy &
                    )
        ! if (verbose) print*,'... done'

        !----------------------------------------------------------------
        ! get soil moisture, and runoff
        !----------------------------------------------------------------
        ! if (verbose) print*,'calling waterbal() ... '
        call waterbal(  tile(:), &
                        tile_fluxes(:), &
                        myinterface%grid, &
                        myinterface%climate(doy) &
                        )
        ! if (verbose) print*,'... done'

        !----------------------------------------------------------------
        ! calculate soil temperature
        !----------------------------------------------------------------
        if (verbose) print*, 'calling soiltemp() ... '
        call soiltemp( tile(:), &
                       myinterface%climate(:)%dtemp, &
                       moy, &
                       doy, &
                       init_daily &
                       )
        if (verbose) print*, '... done'

        !----------------------------------------------------------------
        ! update canopy and stand variables and simulate daily 
        ! establishment / sprouting
        !----------------------------------------------------------------
        ! if (verbose) print*, 'calling vegdynamics() ... '
        ! if (verbose) print*, '              with state variables:'
        ! if (verbose) print*, '              plabl = ', plabl(:,jpngr)
        !----------------------------------------------------------------
        call vegdynamics( tile(:), &
                          myinterface%vegcover(doy)%dfapar, &
                          myinterface%fpc_grid(:) &
                          )
        !----------------------------------------------------------------
        ! if (verbose) print*, '              ==> returned: '
        ! if (verbose) print*, '              plabl = ', plabl(:,jpngr)
        ! if (verbose) print*, '... done'

        !----------------------------------------------------------------
        ! calculate GPP
        !----------------------------------------------------------------
        ! if (verbose) print*,'calling gpp() ... '
        call gpp( tile(:), &
                  tile_fluxes(:), &
                  myinterface%pco2, &
                  myinterface%climate(doy), &
                  myinterface%vegcover(doy), &
                  myinterface%grid, &
                  myinterface%params_siml%soilmstress, &
                  myinterface%params_siml%tempstress, &
                  init_daily &
                  )
        !----------------------------------------------------------------
        ! if (verbose) print*, '              ==> returned: '
        ! if (verbose) print*, '              dgpp  = ', dgpp(:)
        ! if (verbose) print*, '              drd   = ', drd(:)
        ! if (verbose) print*, '... done'

        !----------------------------------------------------------------
        ! substract autotrophic respiration to get NPP, remainder is added 
        ! to labile pool (plabl)
        !----------------------------------------------------------------
        ! if (verbose) print*, 'calling npp() ... '
        ! if (verbose) print*, '              with state variables:'
        ! if (verbose) print*, '              pleaf = ', pleaf(:,jpngr)
        ! if (verbose) print*, '              proot = ', proot(:,jpngr)
        ! if (verbose) print*, '              plabl = ', plabl(:,jpngr)
        ! if (baltest) orgtmp1 =  plabl(1,jpngr)
        !----------------------------------------------------------------
        call npp( tile(:), &
                  tile_fluxes(:), &
                  myinterface%climate(doy) &
                  )                  
        !----------------------------------------------------------------
        ! if (verbose) print*, '              ==> returned: '
        ! if (verbose) print*, '              dnpp  = ', dnpp(:)
        ! if (verbose) print*, '              dcex  = ', dcex(:)
        ! if (verbose) print*, '              plabl = ', plabl(:,jpngr)
        ! if (verbose) print*, '              dlabl = ', orgminus( plabl(1,jpngr), orgtmp1 )
        ! if (baltest) print*, '    --- balance: '
        ! if (baltest) cbal1 = dgpp(1) - dnpp(1)%c12 - drleaf(1) - drroot(1)
        ! if (baltest) cbal2 = dgpp(1) - ( plabl(1,jpngr)%c%c12 - orgtmp1%c%c12 ) - dcex(1) - drleaf(1) - drroot(1)
        ! if (verbose) print*, '        gpp - npp - ra_maint          = ', cbal1
        ! if (verbose) print*, '        gpp - dlabl - dcex - ra_maint = ', cbal2
        ! if (baltest .and. abs(cbal1)>eps) stop 'balance 1 not satisfied'
        ! if (baltest .and. abs(cbal2)>eps) stop 'balance 2 not satisfied'
        ! if (verbose) print*, '... done'

        ! !----------------------------------------------------------------
        ! ! calculate N acquisition as a function of C exudation
        ! !----------------------------------------------------------------
        ! if (verbose) print*, 'calling nuptake() ... '
        ! if (verbose) print*, '              with state variables:'
        ! if (verbose) print*, '              ninorg = ', pnh4(1,jpngr)%n14 + pno3(1,jpngr)%n14
        ! if (verbose) print*, '              nlabl  = ', plabl(1,jpngr)%n%n14
        ! if (baltest) ntmp1 = pnh4(1,jpngr)%n14 + pno3(1,jpngr)%n14
        ! if (baltest) ntmp2 = plabl(1,jpngr)%n%n14
        ! !----------------------------------------------------------------
        ! call nuptake( jpngr )
        ! !----------------------------------------------------------------
        ! if (verbose) print*, '              ==> returned: '
        ! if (verbose) print*, '              dnup   = ', dnup(:)
        ! if (verbose) print*, '              ninorg = ', pnh4(1,jpngr)%n14 + pno3(1,jpngr)%n14
        ! if (verbose) print*, '              nlabl  = ', plabl(1,jpngr)%n%n14
        ! if (baltest) print*, '    --- balance: '
        ! if (baltest) nbal1 = dnup(1)%n14 + ( pnh4(1,jpngr)%n14 + pno3(1,jpngr)%n14 - ntmp1 ) 
        ! if (baltest) nbal2 = ( plabl(1,jpngr)%n%n14 - ntmp2 ) + ( pnh4(1,jpngr)%n14 + pno3(1,jpngr)%n14 - ntmp1 )
        ! if (verbose) print*, '        nup - dninorg     = ', nbal1
        ! if (verbose) print*, '        dnlabl - dninorg  = ', nbal2
        ! if (baltest .and. abs(nbal1)>eps) stop 'balance 1 not satisfied'
        ! if (baltest .and. abs(nbal2)>eps) stop 'balance 2 not satisfied'
        ! if (verbose) print*, '... done'

        ! !----------------------------------------------------------------
        ! ! leaf, sapwood, and fine-root turnover
        ! !----------------------------------------------------------------
        ! if (verbose) print*, 'calling turnover() ... '
        ! if (verbose) print*, '              with state variables:'
        ! if (verbose) print*, '              pleaf = ', pleaf(:,jpngr)
        ! if (verbose) print*, '              proot = ', proot(:,jpngr)
        ! if (verbose) print*, '              plabl = ', plabl(:,jpngr)
        ! if (verbose) print*, '              plitt af = ', plitt_af(1,jpngr)
        ! if (verbose) print*, '              plitt as = ', plitt_as(1,jpngr)
        ! if (verbose) print*, '              plitt bg = ', plitt_bg(1,jpngr)
        ! if (verbose) print*, '              plitt tot = ', orgplus( plitt_af(1,jpngr), plitt_as(1,jpngr), plitt_bg(1,jpngr) )
        ! if (baltest) orgtmp1 = orgplus( pleaf(1,jpngr), proot(1,jpngr), plabl(1,jpngr) )
        ! if (baltest) orgtmp2 = orgplus( plitt_af(1,jpngr), plitt_as(1,jpngr), plitt_bg(1,jpngr) )
        ! !----------------------------------------------------------------
        ! call turnover( jpngr, day )
        ! !----------------------------------------------------------------
        ! if (verbose) print*, '              ==> returned: '
        ! if (verbose) print*, '              pleaf = ', pleaf(:,jpngr)
        ! if (verbose) print*, '              proot = ', proot(:,jpngr)
        ! if (verbose) print*, '              plabl = ', plabl(:,jpngr)
        ! if (verbose) print*, '              plitt af = ', plitt_af(1,jpngr)
        ! if (verbose) print*, '              plitt as = ', plitt_as(1,jpngr)
        ! if (verbose) print*, '              plitt bg = ', plitt_bg(1,jpngr)
        ! if (verbose) print*, '              plitt = ', orgplus( plitt_af(1,jpngr), plitt_as(1,jpngr), plitt_bg(1,jpngr) )
        ! if (baltest) print*, '   --- balance: '
        ! if (baltest) orgbal1 = orgminus( orgminus(   orgplus( plitt_af(1,jpngr), plitt_as(1,jpngr), plitt_bg(1,jpngr) ),   orgtmp2   ), orgminus(   orgtmp1,   orgplus( pleaf(1,jpngr), proot(1,jpngr), plabl(1,jpngr) )   ) )
        ! if (verbose) print*, '       dlitt - dplant                = ', orgbal1
        ! if (baltest .and. abs(orgbal1%c%c12)>eps) stop 'balance not satisfied for C'
        ! if (baltest .and. abs(orgbal1%n%n14)>eps) stop 'balance not satisfied for N'
        ! if (verbose) print*, '... done'

        ! !----------------------------------------------------------------
        ! ! grass / crop harvest
        ! !----------------------------------------------------------------
        ! if (verbose) print*, 'calling grharvest() ... '
        ! if (verbose) print*, '              with state variables:'
        ! if (verbose) print*, '              pleaf = ', pleaf(:,jpngr)
        ! if (verbose) print*, '              proot = ', proot(:,jpngr)
        ! if (verbose) print*, '              plabl = ', plabl(:,jpngr)
        ! if (verbose) print*, '              mharv = ', mharv(:,jpngr)
        ! if (baltest) orgtmp1 =  orgplus( pleaf(1,jpngr), proot(1,jpngr), plabl(1,jpngr) )
        ! if (baltest) orgtmp2 =  mharv(1,jpngr)
        ! !----------------------------------------------------------------
        ! call grharvest( jpngr, day )
        ! !----------------------------------------------------------------
        ! if (verbose) print*, '              ==> returned: '
        ! if (verbose) print*, '              pleaf = ', pleaf(:,jpngr)
        ! if (verbose) print*, '              proot = ', proot(:,jpngr)
        ! if (verbose) print*, '              plabl = ', plabl(:,jpngr)
        ! if (verbose) print*, '              mharv = ', mharv(:,jpngr)
        ! if (baltest) print*, '    --- balance: '
        ! if (baltest) orgbal1 = orgminus( orgminus( orgtmp1, orgplus( pleaf(1,jpngr), proot(1,jpngr), plabl(1,jpngr) ) ), orgminus( mharv(1,jpngr), orgtmp2 ) )
        ! if (verbose) print*, '        dharv - dplant  = ', orgbal1
        ! if (baltest .and. abs(orgbal1%c%c12)>eps) stop 'balance not satisfied for C'
        ! if (baltest .and. abs(orgbal1%n%n14)>eps) stop 'balance not satisfied for N'
        ! if (verbose) print*, '... done'

        ! !----------------------------------------------------------------
        ! ! litter and soil decomposition and N mineralisation
        ! !----------------------------------------------------------------
        ! if (verbose) print*, 'calling littersom() ... '
        ! if (verbose) print*, '              with state variables:'
        ! if (verbose) print*, '              plitt tot=  ', orgplus( plitt_af(1,jpngr), plitt_as(1,jpngr), plitt_bg(1,jpngr) )
        ! if (verbose) print*, '              psoil tot = ', orgplus( psoil_fs(1,jpngr), psoil_sl(1,jpngr) )
        ! if (verbose) print*, '              pexud     = ', pexud(1,jpngr)
        ! if (verbose) print*, '              pninorg=    ', pnh4(1,jpngr)%n14 + pno3(1,jpngr)%n14
        ! if (verbose) print*, '              drhet     = ', drhet(1)
        ! ! if (verbose) print*, '              dnetmin   = ', outdnetmin(1,day,jpngr)
        ! if (baltest) orgtmp1 = orgplus( plitt_af(1,jpngr), plitt_as(1,jpngr), plitt_bg(1,jpngr), psoil_fs(1,jpngr), psoil_sl(1,jpngr) )
        ! if (baltest) orgtmp2 = orgpool( drhet(1), nplus( pnh4(1,jpngr), pno3(1,jpngr) ) )
        ! ! if (baltest) ntmp1 = outdnetmin(1,day,jpngr)
        ! !----------------------------------------------------------------
        ! call littersom( jpngr, day, interface%climate(jpngr)%dtemp(day) )
        ! !----------------------------------------------------------------
        ! if (verbose) print*, '              ==> returned: '
        ! if (verbose) print*, '              plitt  = ', orgplus( plitt_af(1,jpngr), plitt_as(1,jpngr), plitt_bg(1,jpngr) )
        ! if (verbose) print*, '              psoil  = ', orgplus( psoil_fs(1,jpngr), psoil_sl(1,jpngr) )
        ! if (verbose) print*, '              pninorg= ', pnh4(1,jpngr)%n14 + pno3(1,jpngr)%n14
        ! if (verbose) print*, '              drhet  = ', drhet(1)
        ! ! if (verbose) print*, '              dnetmin= ', outdnetmin(1,day,jpngr)
        ! if (baltest) print*, '   --- balance: '
        ! if (baltest) orgtmp3 = orgplus( plitt_af(1,jpngr), plitt_as(1,jpngr), plitt_bg(1,jpngr), psoil_fs(1,jpngr), psoil_sl(1,jpngr) )
        ! if (baltest) orgtmp4 = orgpool( drhet(1), nplus( pnh4(1,jpngr), pno3(1,jpngr) ) )
        ! if (baltest) orgbal1 = orgminus( orgplus( orgtmp3, orgtmp4 ), orgplus( orgtmp1, orgtmp2 ) )
        ! ! if (baltest) nbal1 = (orgtmp1%n%n14 + ntmp1) - (orgtmp3%n%n14 + outdnetmin(1,day,jpngr))
        ! if (verbose) print*, '       d( litt + soil ) - d(drhet,ninorg) = ', orgbal1
        ! if (verbose) print*, '       d( litt + soil ) - netmin          = ', nbal1
        ! if (baltest .and. abs(orgbal1%c%c12)>eps) stop 'balance not satisfied for C'
        ! if (baltest .and. abs(orgbal1%n%n14)>eps) stop 'balance not satisfied for N'
        ! if (baltest .and. abs(nbal1)>eps)         stop 'balance not satisfied for N, test 1'
        ! if (verbose) print*, '... done'

        ! !----------------------------------------------------------------
        ! ! inorganic soil N dynamics (mass balance test only possible inside module)
        ! !----------------------------------------------------------------
        ! if (verbose) print*, 'calling ntransform() ... '
        ! !----------------------------------------------------------------
        ! call ntransform( dm, moy, jpngr, interface%ninput_field(jpngr)%dnhx(day), interface%ninput_field(jpngr)%dnoy(day), sum(interface%climate(jpngr)%dprec(:)) )
        ! !----------------------------------------------------------------
        ! if (verbose) print*, '... done'

        ! !----------------------------------------------------------------
        ! ! allocation of labile pools to biomass
        ! !----------------------------------------------------------------
        ! if (verbose) print*, 'calling allocation() ... '
        ! if (verbose) print*, '              with state variables:'
        ! if (verbose) print*, '              pleaf = ', pleaf(:,jpngr)
        ! if (verbose) print*, '              proot = ', proot(:,jpngr)
        ! if (verbose) print*, '              plabl = ', plabl(:,jpngr)
        ! if (verbose) print*, '              drgrow= ', drgrow(:)
        ! if (verbose) print*, '              dnup  = ', dnup(1)%n14
        ! if (baltest) orgtmp1 = orgminus( orgplus( pleaf(1,jpngr), proot(1,jpngr), plabl(1,jpngr), orgpool( carbon(drgrow(1)), nitrogen(0.0) ) ), orgpool(carbon(0.0),dnup(1)) )
        ! !----------------------------------------------------------------
        ! call allocation_daily( jpngr, day, dm, moy, interface%climate(jpngr)%dtemp(:) )
        ! !----------------------------------------------------------------
        ! if (verbose) print*, '              ==> returned: '
        ! if (verbose) print*, '              pleaf = ', pleaf(:,jpngr)
        ! if (verbose) print*, '              proot = ', proot(:,jpngr)
        ! if (verbose) print*, '              plabl = ', plabl(:,jpngr)
        ! if (verbose) print*, '              drgrow= ', drgrow(:)
        ! if (verbose) print*, '              dnup  = ', dnup(1)%n14
        ! if (verbose) print*, '   --- balance: '
        ! if (baltest) orgtmp2 = orgminus( orgplus( pleaf(1,jpngr), proot(1,jpngr), plabl(1,jpngr), orgpool( carbon(drgrow(1)), nitrogen(0.0) ) ), orgpool(carbon(0.0),dnup(1)) )
        ! if (baltest) orgbal1 = orgminus( orgtmp2, orgtmp1 )
        ! if (baltest) print*, '       d( pleaf + proot + plabl + Nfix ) =', orgbal1
        ! if (baltest .and. abs(orgbal1%c%c12)>eps) stop 'balance not satisfied for C'
        ! if (baltest .and. abs(orgbal1%n%n14)>eps) stop 'balance not satisfied for N'
        ! if (verbose) print*, '... done'


        !----------------------------------------------------------------
        ! daily diagnostics (e.g., sum over plant within canopy)
        !----------------------------------------------------------------
        call diag_daily(tile(:), tile_fluxes(:))

        !----------------------------------------------------------------
        ! populate function return variable
        !----------------------------------------------------------------
        ! if (nlu>1) stop 'think about nlu > 1'
        out_biosphere%fapar(doy)   = tile(1)%canopy%fapar
        out_biosphere%gpp(doy)     = tile_fluxes(1)%canopy%dgpp
        out_biosphere%transp(doy)  = tile_fluxes(1)%canopy%daet
        out_biosphere%latenth(doy) = tile_fluxes(1)%canopy%daet_e
        out_biosphere%pet(doy)     = tile_fluxes(1)%canopy%dpet
        out_biosphere%vcmax(doy)   = tile_fluxes(1)%canopy%vcmax
        out_biosphere%jmax(doy)    = tile_fluxes(1)%canopy%jmax
        out_biosphere%vcmax25(doy) = tile_fluxes(1)%canopy%vcmax25
        out_biosphere%jmax25(doy)  = tile_fluxes(1)%canopy%jmax25
        out_biosphere%gs_accl(doy) = tile_fluxes(1)%canopy%gs_accl
        out_biosphere%wscal(doy)   = tile(1)%soil%phy%wscal
        out_biosphere%chi(doy)     = tile_fluxes(1)%canopy%chi
        out_biosphere%iwue(doy)    = tile_fluxes(1)%canopy%iwue

        out_biosphere%tsoil(doy)   = tile(1)%soil%phy%temp
        out_biosphere%cleaf(doy)   = 0.0
        out_biosphere%nleaf(doy)   = 0.0
        out_biosphere%croot(doy)   = 0.0
        out_biosphere%nroot(doy)   = 0.0
        out_biosphere%clabl(doy)   = 0.0
        out_biosphere%nlabl(doy)   = 0.0
        out_biosphere%lai(doy)     = 0.0
        out_biosphere%ninorg(doy)  = 0.0
        out_biosphere%pno3(doy)    = 0.0
        out_biosphere%pnh4(doy)    = 0.0
        out_biosphere%en2o(doy)    = 0.0
        out_biosphere%enleach(doy) = 0.0

        out_biosphere%csoil(doy)   = 0.0
        out_biosphere%nsoil(doy)   = 0.0
        out_biosphere%clitt(doy)   = 0.0
        out_biosphere%nlitt(doy)   = 0.0
        out_biosphere%nfix(doy)    = 0.0
        out_biosphere%nup(doy)     = 0.0
        out_biosphere%cex(doy)     = 0.0

        out_biosphere%tmp(doy)     = 0.0

        init_daily = .false.

      end do dayloop

    end do monthloop

    !----------------------------------------------------------------
    ! annual diagnostics
    !----------------------------------------------------------------
    call diag_annual( tile(:), tile_fluxes(:) )
    

    ! if (verbose) print*,'Done with biosphere for this year. Guete Rutsch!'

  end function biosphere_annual

end module md_biosphere_cnmodel
