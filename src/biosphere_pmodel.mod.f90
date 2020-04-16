module md_biosphere_pmodel

  use md_params_core_pmodel
  use md_classdefs
  use md_plant_pmodel, only: plant_type, plant_fluxes_type, initdaily_plant, initglobal_plant, getpar_modl_plant
  use md_params_soil_pmodel, only: paramtype_soil
  use md_waterbal, only: solartype, waterbal, get_solar, getpar_modl_waterbal, init_rlm_waterbal, get_rlm_waterbal, getrlm_daily_waterbal
  use md_gpp_pmodel, only: outtype_pmodel, getpar_modl_gpp, gpp
  use md_vegdynamics_pmodel, only: vegdynamics
  use md_tile_pmodel, only: tile_type, tile_fluxes_type, initglobal_tile, initdaily_tile
  use md_soiltemp, only: soiltemp
  use md_sofunutils, only: calc_patm


  implicit none

  private
  public biosphere_annual

  !----------------------------------------------------------------
  ! Module-specific (private) variables
  !----------------------------------------------------------------
  ! derived types from L1 modules
  type( tile_type ),         dimension(nlu)  :: tile
  type( tile_fluxes_type ),  dimension(nlu)  :: tile_fluxes
  type( plant_type ),        dimension(npft) :: plant
  type( plant_fluxes_type ), dimension(npft) :: plant_fluxes

  ! derived types from L2 modules
  type( solartype )                              :: solar
  type( outtype_pmodel ), dimension(npft,nmonth) :: out_pmodel ! P-model output variables for each month and PFT determined beforehand (per unit fAPAR and PPFD only)

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
      call getpar_modl_plant()
      call getpar_modl_waterbal()
      call getpar_modl_gpp()
      if (verbose) print*, '... done'

      !----------------------------------------------------------------
      ! Initialise pool variables and/or read from restart file (not implemented)
      !----------------------------------------------------------------
      if (verbose) print*, 'initglobal_() ...'
      call initglobal_plant( plant(:) )
      call initglobal_tile(  tile(:) )
      if (verbose) print*, '... done'

    endif 

    ! additional initialisation for rolling annual mean calculations (also needed in calibration mode)
    call init_rlm_waterbal()

    !----------------------------------------------------------------
    ! Get radiation based on daily temperature, sunshine fraction, and 
    ! elevation.
    ! This is not compatible with a daily biosphere-climate coupling. I.e., 
    ! there is a daily loop within 'get_solar'!
    !----------------------------------------------------------------
    if (verbose) print*,'calling get_solar() ... '
    if (verbose) print*,'    with argument lat = ', myinterface%grid%lat
    if (verbose) print*,'    with argument elv = ', myinterface%grid%elv
    if (verbose) print*,'    with argument dfsun (ann. mean) = ', sum( myinterface%climate%dfsun(:) / ndayyear )
    if (verbose) print*,'    with argument dppfd (ann. mean) = ', sum( myinterface%climate%dppfd(:) / ndayyear )
    solar = get_solar( &
                      myinterface%grid%lat, & 
                      myinterface%grid%elv, & 
                      myinterface%climate%dfsun(:), & 
                      myinterface%climate%dppfd(:)  &
                      )
    if (verbose) print*,'... done'

    ! !----------------------------------------------------------------
    ! ! calculate constant atmospheric pressure as a function of elevation
    ! !----------------------------------------------------------------
    ! myinterface%climate%dpatm(:) = calc_patm(myinterface%grid%elv)

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
        call initdaily_plant( plant_fluxes(:) )
        call initdaily_tile( tile_fluxes(:) )
        if (verbose) print*,'... done.'


        !----------------------------------------------------------------
        ! update canopy and tile variables and simulate daily 
        ! establishment / sprouting
        !----------------------------------------------------------------
        if (verbose) print*,'calling vegdynamics() ... '
        call vegdynamics( tile(:), &
                          plant(:), &
                          solar, &
                          out_pmodel(:,:), &
                          myinterface%vegcover%dfapar(doy), &
                          myinterface%fpc_grid(:) &
                          )
        if (verbose) print*,'... done'


        !----------------------------------------------------------------
        ! calculate GPP
        !----------------------------------------------------------------
        if (verbose) print*,'calling gpp() ... '
        call gpp( &
                  myinterface%pco2, & 
                  myinterface%climate%dtemp(doy), & 
                  myinterface%climate%dvpd(doy), & 
                  myinterface%climate%dpatm(doy), &
                  myinterface%climate%dppfd(doy), &
                  myinterface%vegcover%dfapar(doy), &
                  plant(:)%fpc_grid, &
                  solar%dayl(doy), &
                  solar%meanmppfd(moy), &
                  tile(:)%soil%phy%wscal, &
                  tile(:)%soil%phy%rlmalpha, &
                  myinterface%params_siml%soilmstress, &
                  myinterface%params_siml%tempstress, &
                  plant_fluxes(:)%dgpp, &
                  plant_fluxes(:)%drd, &
                  plant_fluxes(:)%dtransp, &
                  init_daily &
                  )
        if (verbose) print*,'... done'

        !----------------------------------------------------------------
        ! get soil moisture, and runoff
        !----------------------------------------------------------------
        if (verbose) print*,'calling waterbal() ... '
        call waterbal( &
                      tile(:)%soil, &
                      tile_fluxes(:), &
                      plant_fluxes(:), &
                      doy, &
                      jpngr, & 
                      myinterface%grid%lat,             & 
                      myinterface%grid%elv,             & 
                      myinterface%climate%dprec(doy),   & 
                      myinterface%climate%dsnow(doy),   & 
                      myinterface%climate%dtemp(doy),   & 
                      myinterface%climate%dfsun(doy),   &
                      myinterface%climate%dnetrad(doy), &
                      myinterface%climate%dvpd(doy),    &
                      myinterface%vegcover%dfapar(doy)  &
                      )
        if (verbose) print*,'... done'

        ! !----------------------------------------------------------------
        ! ! calculate soil temperature
        ! !----------------------------------------------------------------
        ! if (verbose) print*, 'calling soiltemp() ... '
        ! call soiltemp(&
        !               tile(:,jpngr)%soil, &
        !               myinterface%climate%dtemp(:), &
        !               size(myinterface%grid), &
        !               myinterface%steering%init, &
        !               jpngr, & 
        !               moy, & 
        !               doy & 
        !               )
        ! if (verbose) print*, '... done'


        call getrlm_daily_waterbal( doy )

        !----------------------------------------------------------------
        ! populate function return variable
        !----------------------------------------------------------------
        !if (npft>1) stop 'think about npft > 1'
        out_biosphere%fapar(doy)   = plant(1)%fapar_ind
        out_biosphere%gpp(doy)     = plant_fluxes(1)%dgpp
        out_biosphere%transp(doy)  = plant_fluxes(1)%dtransp
        out_biosphere%latenth(doy) = plant_fluxes(1)%dlatenth

        init_daily = .false.

      end do dayloop

    end do monthloop

    !----------------------------------------------------------------
    ! Get rolling multi-year averages (needs to store entire arrays)
    !----------------------------------------------------------------
    call get_rlm_waterbal( tile(:)%soil%phy, myinterface%steering%init )


    if (myinterface%steering%finalize) then
      !----------------------------------------------------------------
      ! Finazlize run: deallocating memory
      !----------------------------------------------------------------
      ! nothin ...
    end if

    if (verbose) print*,'Done with biosphere for this year. Guete Rutsch!'

  end function biosphere_annual

end module md_biosphere_pmodel
