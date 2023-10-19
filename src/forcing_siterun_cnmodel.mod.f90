module md_forcing_cnmodel
  !////////////////////////////////////////////////////////////////
  ! Module containing treatment of forcing for CN-model, linking
  ! what's obtained from R through SR pmodel_f and what's needed by biosphere SR.
  !----------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32, in=>int32
  use md_params_core, only: ndayyear, nlu, dummy
  use md_grid, only:  gridtype
  use md_sofunutils, only: calc_patm

  implicit none

  private
  public landuse_type, climate_type, getlanduse, &
    getclimate, getco2, getfapar, get_fpc_grid, vegcover_type

  type climate_type
    real(kind=sp) :: dtemp  ! daily mean air temperature, deg C
    real(kind=sp) :: dtmin  ! daily minimum air temperature, deg C
    real(kind=sp) :: dtmax  ! daily maximum air temperature, deg C
    real(kind=sp) :: dprec  ! mm d-1
    real(kind=sp) :: dsnow  ! mm d-1 water equivalents
    real(kind=sp) :: dfsun  ! unitless
    real(kind=sp) :: dvpd   ! Pa
    real(kind=sp) :: dppfd  ! mol m-2 d-1
    real(kind=sp) :: dnetrad! W m-2
    real(kind=sp) :: dpatm  ! Pa
  end type climate_type

  type vegcover_type
    real :: dfapar ! fraction of absorbed photosynthetically active radiation
  end type vegcover_type

  type landuse_type
    real(kind=sp) :: dfharv  ! fraction of biomass harvested per day (m-2 d-1)
    real(kind=sp) :: dnoy    ! NO3 inputs per day, fertilisation (gN m-2 d-1)
    real(kind=sp) :: dnhx    ! NH4 inputs per day, fertilisation (gN m-2 d-1)
    real(kind=sp) :: cseed   ! seed C input per day (sowing) (gC m-2 d-1)
    real(kind=sp) :: nseed   ! seed N input per day (sowing) (gN m-2 d-1)
  end type landuse_type

contains

  function getclimate( nt, forcing, climateyear_idx, in_ppfd, in_netrad, elv ) result ( out_climate )
    !////////////////////////////////////////////////////////////////
    ! This function invokes file format specific "sub-functions/routines"
    ! to read from NetCDF. This nesting is necessary because this 
    ! cannot be done file-specific in SR sofun, but can be done here
    ! as this module is compilation-specific (only for global simulations)
    !----------------------------------------------------------------
    ! arguments
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp),  dimension(nt,16), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    integer, intent(in) :: climateyear_idx
    logical, intent(in) :: in_ppfd
    logical, intent(in) :: in_netrad
    real, intent(in) :: elv

    ! local variables
    integer :: idx_start, idx_end
    integer, dimension(2) :: shape_forcing

    ! function return variable
    type( climate_type ), dimension(ndayyear) :: out_climate

    idx_start = (climateyear_idx - 1) * ndayyear + 1
    idx_end   = idx_start + ndayyear - 1
    
    ! Test if forcing dimensions are correct
    shape_forcing = shape(forcing)
    if (idx_end > shape_forcing(1)) then
      ! stop 'forcing array size does not have enough rows.'
    end if

    ! warning: column indices in forcing array are hard coded
    out_climate(:)%dtemp   = real(forcing(idx_start:idx_end, 1))
    out_climate(:)%dprec   = real(forcing(idx_start:idx_end, 2))
    out_climate(:)%dvpd    = real(forcing(idx_start:idx_end, 3))

    if (in_ppfd) then
      out_climate(:)%dppfd = real(forcing(idx_start:idx_end, 4))
    else
      out_climate(:)%dppfd = dummy
    end if
    if (in_netrad) then
      out_climate(:)%dnetrad = real(forcing(idx_start:idx_end, 5))
    else
      out_climate(:)%dnetrad = dummy
    end if
    if ( in_netrad .and. in_ppfd ) then
      out_climate(:)%dfsun = dummy
    else
      out_climate(:)%dfsun = real(forcing(idx_start:idx_end, 6))
    end if
    out_climate(:)%dsnow   = real(forcing(idx_start:idx_end, 7))
    out_climate(:)%dpatm   = real(forcing(idx_start:idx_end, 10))
    out_climate(:)%dtmin   = real(forcing(idx_start:idx_end, 11))
    out_climate(:)%dtmax   = real(forcing(idx_start:idx_end, 12))

  end function getclimate
  

  function getco2( nt, forcing, forcingyear, firstyeartrend ) result( pco2 )
    !////////////////////////////////////////////////////////////////
    !  Function reads this year's atmospheric CO2 from input
    !----------------------------------------------------------------
    ! arguments
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp),  dimension(nt,16), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    integer, intent(in) :: forcingyear
    integer, intent(in) :: firstyeartrend

    ! function return variable
    real :: pco2

    ! local variables 
    integer :: readyear_idx
    integer :: idx_start, idx_end

    readyear_idx = forcingyear - firstyeartrend + 1

    idx_start = (readyear_idx - 1) * ndayyear + 1
    idx_end   = idx_start + ndayyear - 1

    pco2 = sum(real(forcing(idx_start:idx_end, 8)))/ndayyear

  end function getco2


  function getfapar( nt, forcing, forcingyear_idx ) result( out_vegcover )
    !////////////////////////////////////////////////////////////////
    ! Function reads this year's atmospheric CO2 from input
    !----------------------------------------------------------------
    ! arguments
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp), dimension(nt,16), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    integer, intent(in) :: forcingyear_idx

    ! function return variable
    type( vegcover_type ), dimension(ndayyear) :: out_vegcover

    ! local variables 
    integer :: idx_start, idx_end

    idx_start = (forcingyear_idx - 1) * ndayyear + 1
    idx_end   = idx_start + ndayyear - 1

    out_vegcover(:)%dfapar = real(forcing(idx_start:idx_end, 9))

  end function getfapar


  function get_fpc_grid( params_siml ) result( fpc_grid_field )
    !////////////////////////////////////////////////////////////////
    ! Function returns the fractional land cover by vegetation types 
    ! based on the 10 IGBP types in the input file (MODIS Landcover)
    ! 1: ENF: type2 = "evergreen needleleaf forest" ;
    ! 2: EBF: type3 = "evergreen broadleaf forest" ;
    ! 3: DNF: type4 = "deciduous needleleaf forest" ;
    ! 4: DBF: type5 = "deciduous broadleaf forest" ;
    ! 5: MF:  type6 = "mixed forest" ;
    ! 6: SHR: type7+type8 = "closed shrublands" + "open shrublands";
    ! 7: SAV: type9+type10 = "savannas" plus "woody savannas"
    ! 8: GRA: type11 = "grasslands" ;
    ! 9: WET: type12 = "permanent wetlands" ;
    ! 10:CRO: type13 + type15 = "croplands" + "cropland (natural vegetation mosaic)";
    !----------------------------------------------------------------
    use md_params_siml_cnmodel, only: paramstype_siml
    use md_params_core, only: npft

    ! arguments
    type( paramstype_siml ), intent(in) :: params_siml

    ! function return variable
    real, dimension(npft) :: fpc_grid_field

    ! local variables
    integer :: pft

    ! get binary information of PFT presence from simulation parameters
    fpc_grid_field(:) = 0.0

    ! Code below must follow the same structure as in 'plant_pmodel.mod.f90'
    pft = 0
    if ( params_siml%ltre ) then
      ! xxx dirty: call all non-grass vegetation types 'TrE', see indeces above
      pft = pft + 1
      fpc_grid_field(pft) = 1.0
    end if 

    if ( params_siml%lgr3 ) then
      ! xxx dirty: call all grass vegetation types 'Gr3'
      pft = pft + 1
      fpc_grid_field(pft) = 1.0
    end if

    if ( params_siml%lgr4 ) then
      ! xxx dirty: call all grass vegetation types 'Gr3'
      pft = pft + 1
      fpc_grid_field(pft) = 1.0
    end if

    ! if (pft==0) stop 'get_fpc_grid: no PFT activated accoring to simulation parameter file.'
    ! if (pft/=npft) stop 'GET_FPC_GRID: Adjust npft manually in params_core.mod.f90'
    
  end function get_fpc_grid


  function getlanduse( nt, forcing, forcingyear, firstyeartrend ) result( out_landuse )
    !////////////////////////////////////////////////////////////////
    ! Function reads this year's annual landuse state and harvesting regime (day of above-ground harvest)
    ! Grass harvest forcing file is read for specific year, if none is available,
    ! use earliest forcing file available. 
    !----------------------------------------------------------------
    ! arguments
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp),  dimension(nt,16), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    integer, intent(in) :: forcingyear
    integer, intent(in) :: firstyeartrend

    ! function return variable
    type( landuse_type ), dimension(ndayyear) :: out_landuse

    ! local variables 
    integer :: readyear_idx
    integer :: idx_start, idx_end

    readyear_idx = forcingyear - firstyeartrend + 1
    idx_start = (readyear_idx - 1) * ndayyear + 1
    idx_end   = idx_start + ndayyear - 1

    out_landuse(:)%dfharv = real(forcing(idx_start:idx_end, 13))
    out_landuse(:)%dnoy   = real(forcing(idx_start:idx_end, 14))
    out_landuse(:)%dnhx   = real(forcing(idx_start:idx_end, 15))
    out_landuse(:)%cseed  = real(forcing(idx_start:idx_end, 16))
    out_landuse(:)%nseed  = real(forcing(idx_start:idx_end, 17))

  end function getlanduse


end module md_forcing_cnmodel