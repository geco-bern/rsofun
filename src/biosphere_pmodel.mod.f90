module md_biosphere_pmodel
  !////////////////////////////////////////////////////////////////
  ! Module containing loop through time steps within a year and 
  ! calls to SRs for individual processes.
  ! Does not contain any input/output; this is done in SR sofun.
  !----------------------------------------------------------------
  use md_params_core
  use md_classdefs
  use md_waterbal, only: waterbal, solar, getpar_modl_waterbal
  use md_gpp_pmodel, only: getpar_modl_gpp, gpp
  use md_vegdynamics_pmodel, only: vegdynamics
  use md_tile_pmodel, only: tile_type, tile_fluxes_type, initglobal_tile, initdaily_tile_fluxes, &
    getpar_modl_tile, diag_daily, diag_annual, init_annual
  use md_plant_pmodel, only: getpar_modl_plant
  use md_sofunutils, only: calc_patm
  use md_soiltemp, only: soiltemp

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
    ! Calculates one year of photosynthesis C and water fluxes. 
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
    doy=0
    monthloop: do moy=1,nmonth

      !----------------------------------------------------------------
      ! LOOP THROUGH DAYS
      !----------------------------------------------------------------
      dayloop: do dm=1,ndaymonth(moy)
        doy=doy+1

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
                    doy, &
                    myinterface%params_siml%in_netrad &
                    )
        ! if (verbose) print*,'... done'

        !----------------------------------------------------------------
        ! update canopy and tile variables and simulate daily 
        ! establishment / sprouting
        !----------------------------------------------------------------
        ! if (verbose) print*,'calling vegdynamics() ... '
        call vegdynamics( tile(:), &
                          myinterface%vegcover(doy)%dfapar, &
                          myinterface%fpc_grid(:) &
                          )
        ! if (verbose) print*,'... done'

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
                  init_daily, &
                  myinterface%params_siml%in_ppfd &
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
        ! if (verbose) print*, 'calling soiltemp() ... '
        call soiltemp(&
                      tile(:)%soil, &
                      myinterface%climate(:)%dtemp, &
                      moy, & 
                      doy & 
                      )
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
        out_biosphere%rd(doy)      = tile_fluxes(1)%canopy%drd
        out_biosphere%tsoil(doy)   = tile(1)%soil%phy%temp    
        out_biosphere%netrad(doy)  = (tile_fluxes(1)%canopy%drn + tile_fluxes(1)%canopy%drnn) &
                                     / myinterface%params_siml%secs_per_tstep                 ! output in W m-2 
        out_biosphere%wcont(doy)   = tile(1)%soil%phy%wcont
        out_biosphere%snow(doy)    = tile(1)%soil%phy%snow

        init_daily = .false.

      end do dayloop

    end do monthloop

    !----------------------------------------------------------------
    ! annual diagnostics
    !----------------------------------------------------------------
    call diag_annual( tile(:), tile_fluxes(:) )
    

    ! if (verbose) print*,'Done with biosphere for this year. Guete Rutsch!'

  end function biosphere_annual

end module md_biosphere_pmodel
