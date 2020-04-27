module md_biosphere_pmodel

  use md_params_core_pmodel
  use md_classdefs
  use md_waterbal, only: waterbal, solar, getpar_modl_waterbal
  use md_gpp_pmodel, only: getpar_modl_gpp, gpp
  use md_vegdynamics_pmodel, only: vegdynamics
  use md_tile_pmodel, only: tile_type, tile_fluxes_type, initglobal_tile, initdaily_tile_fluxes, getpar_modl_canopy
  use md_plant_pmodel, only: getpar_modl_plant
  ! use md_soiltemp, only: soiltemp
  use md_sofunutils, only: calc_patm


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
    use md_sofunutils, only: daily2monthly
  
    ! return variable
    type(outtype_biosphere) :: out_biosphere

    ! local variables
    integer :: dm, moy, jpngr, doy
    logical, save           :: init_daily = .true.   ! is true only on the first day of the simulation 
    logical, parameter      :: verbose = .false.     ! change by hand for debugging etc.

    !----------------------------------------------------------------
    ! INITIALISATIONS
    !----------------------------------------------------------------
    if (myinterface%steering%init) then

      !----------------------------------------------------------------
      ! GET MODEL PARAMETERS
      ! read model parameters that may be varied for optimisation
      !----------------------------------------------------------------
      if (verbose) print*, 'getpar_modl() ...'
      call getpar_modl_canopy()
      call getpar_modl_plant()
      call getpar_modl_waterbal()
      call getpar_modl_gpp()
      if (verbose) print*, '... done'

      !----------------------------------------------------------------
      ! Initialise pool variables and/or read from restart file (not implemented)
      !----------------------------------------------------------------
      if (verbose) print*, 'initglobal_() ...'
      call initglobal_tile( tile(:) )
      if (verbose) print*, '... done'

    endif 

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

        if (verbose) print*,'----------------------'
        if (verbose) print*,'YEAR, Doy ', myinterface%steering%year, doy
        if (verbose) print*,'----------------------'

        !----------------------------------------------------------------
        ! initialise daily updated variables 
        !----------------------------------------------------------------
        if (verbose) print*,'calling initdaily_() ...'
        call initdaily_tile_fluxes( tile_fluxes(:) )
        if (verbose) print*,'... done.'

        !----------------------------------------------------------------
        ! Get radiation based on daily temperature, sunshine fraction, and 
        ! elevation.
        !----------------------------------------------------------------
        if (verbose) print*,'calling solar() ... '
        if (verbose) print*,'    with argument lat = ', myinterface%grid%lat
        if (verbose) print*,'    with argument elv = ', myinterface%grid%elv
        if (verbose) print*,'    with argument dfsun (ann. mean) = ', sum( myinterface%climate(:)%dfsun / ndayyear )
        if (verbose) print*,'    with argument dppfd (ann. mean) = ', sum( myinterface%climate(:)%dppfd / ndayyear )
        call solar( tile_fluxes(:), &
                    myinterface%grid, & 
                    myinterface%climate(doy),  &
                    doy &
                    )
        if (verbose) print*,'... done'

        !----------------------------------------------------------------
        ! update canopy and tile variables and simulate daily 
        ! establishment / sprouting
        !----------------------------------------------------------------
        if (verbose) print*,'calling vegdynamics() ... '
        call vegdynamics( tile(:), &
                          myinterface%vegcover(doy)%dfapar, &
                          myinterface%fpc_grid(:) &
                          )
        if (verbose) print*,'... done'

        !----------------------------------------------------------------
        ! calculate GPP
        !----------------------------------------------------------------
        if (verbose) print*,'calling gpp() ... '
        call gpp( tile(:), &
                  tile_fluxes(:), &
                  myinterface%pco2, &
                  myinterface%climate(doy), &
                  myinterface%vegcover(doy), &
                  myinterface%params_siml%soilmstress, &
                  myinterface%params_siml%tempstress, &
                  init_daily &
                  )
        if (verbose) print*,'... done'

        !----------------------------------------------------------------
        ! get soil moisture, and runoff
        !----------------------------------------------------------------
        if (verbose) print*,'calling waterbal() ... '
        call waterbal(  tile(:), &
                        tile_fluxes(:), &
                        myinterface%grid, &
                        myinterface%climate(doy), &
                        doy &
                        )
        if (verbose) print*,'... done'

        ! !----------------------------------------------------------------
        ! ! calculate soil temperature
        ! !----------------------------------------------------------------
        ! if (verbose) print*, 'calling soiltemp() ... '
        ! call soiltemp(&
        !               tile(:)%soil, &
        !               myinterface%climate%dtemp(:), &
        !               size(myinterface%grid), &
        !               myinterface%steering%init, &
        !               jpngr, & 
        !               moy, & 
        !               doy & 
        !               )
        ! if (verbose) print*, '... done'

        !----------------------------------------------------------------
        ! populate function return variable
        !----------------------------------------------------------------
        !if (npft>1) stop 'think about npft > 1'
        out_biosphere%fapar(doy)   = tile(1)%canopy%fapar
        out_biosphere%gpp(doy)     = tile_fluxes(1)%canopy%dgpp
        out_biosphere%transp(doy)  = tile_fluxes(1)%canopy%daet
        out_biosphere%latenth(doy) = tile_fluxes(1)%canopy%daet_e

        init_daily = .false.

      end do dayloop

    end do monthloop

    if (verbose) print*,'Done with biosphere for this year. Guete Rutsch!'

  end function biosphere_annual

end module md_biosphere_pmodel
