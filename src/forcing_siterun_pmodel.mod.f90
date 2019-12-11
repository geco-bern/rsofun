module md_forcing
  !////////////////////////////////////////////////////////////////
  ! Module contains forcing variables (climate, co2, ...), and
  ! subroutines used to read forcing input files for a specific year
  ! ('forcingyear'), specifically for site scale simulations.
  ! This module is only used on the level of 'sofun', but not
  ! within 'biosphere', as these variables are passed on to 'biosphere'
  ! as arguments.
  ! Copyright (C) 2015, see LICENSE, Benjamin David Stocker
  ! contact: b.stocker@imperial.ac.uk
  !----------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, dp=>real64, sp=>real32
  use md_params_core, only: ndayyear, nlu, dummy
  use md_grid, only: domaininfo_type, gridtype

  implicit none

  private
  public ninput_type, landuse_type, climate_type, &
    getclimate, getco2, getfapar, get_fpc_grid, vegcover_type

  type climate_type
    real(kind=sp), dimension(ndayyear) :: dtemp  ! deg C
    real(kind=sp), dimension(ndayyear) :: dprec  ! mm d-1
    real(kind=sp), dimension(ndayyear) :: dsnow  ! mm d-1 water equivalents
    real(kind=sp), dimension(ndayyear) :: dfsun  ! unitless
    real(kind=sp), dimension(ndayyear) :: dvpd   ! Pa
    real(kind=sp), dimension(ndayyear) :: dppfd  ! mol m-2 d-1
    real(kind=sp), dimension(ndayyear) :: dnetrad! W m-2
  end type climate_type

  type vegcover_type
    real, dimension(ndayyear) :: dfapar ! fraction of absorbed photosynthetically active radiation
  end type vegcover_type

  type landuse_type
    real(kind=sp), dimension(nlu)  :: lu_area
    logical, dimension(ndayyear)   :: do_grharvest
  end type landuse_type

  type ninput_type
    real(kind=sp), dimension(ndayyear) :: dnoy
    real(kind=sp), dimension(ndayyear) :: dnhx
    real(kind=sp), dimension(ndayyear) :: dtot
  end type ninput_type

contains

  function getclimate( nt, forcing, domaininfo, grid, init, climateyear_idx, in_ppfd, in_netrad ) result ( out_climate )
  ! function getclimate( nt, forcing, climateyear_idx, in_ppfd, in_netrad ) result ( out_climate )
    !////////////////////////////////////////////////////////////////
    ! This function invokes file format specific "sub-functions/routines"
    ! to read from NetCDF. This nesting is necessary because this 
    ! cannot be done file-specific in SR sofun, but can be done here
    ! as this module is compilation-specific (only for global simulations)
    !----------------------------------------------------------------
    ! arguments
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp),  dimension(nt,10), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    type( domaininfo_type ), intent(in) :: domaininfo
    type( gridtype ), dimension(1), intent(inout) :: grid
    logical, intent(in) :: init
    integer, intent(in) :: climateyear_idx
    logical, intent(in) :: in_ppfd
    logical, intent(in) :: in_netrad

    ! local variables
    integer :: idx_start, idx_end
    integer, dimension(2) :: shape_forcing

    ! function return variable
    type( climate_type ), dimension(1) :: out_climate

    idx_start = (climateyear_idx - 1) * ndayyear + 1
    idx_end   = idx_start + ndayyear - 1
    
    ! Test if forcing dimensions are correct
    shape_forcing = shape(forcing)
    if (idx_end>shape_forcing(1)) then
      stop 'forcing array size does not have enough rows.'
    end if

    ! warning: column indices in forcing array are hard coded
    out_climate(1)%dtemp(:)   = real(forcing(idx_start:idx_end, 1))
    out_climate(1)%dprec(:)   = real(forcing(idx_start:idx_end, 2))
    out_climate(1)%dvpd(:)    = real(forcing(idx_start:idx_end, 3))

    if (in_ppfd) then
      out_climate(1)%dppfd(:) = real(forcing(idx_start:idx_end, 4))
    else
      out_climate(1)%dppfd(:) = dummy
    end if
    if (in_netrad) then
      out_climate(1)%dnetrad(:) = real(forcing(idx_start:idx_end, 5))
    else
      out_climate(1)%dnetrad(:) = dummy
    end if
    if ( in_netrad .and. in_ppfd ) then
      out_climate(1)%dfsun(:) = dummy
    else
      out_climate(1)%dfsun(:) = real(forcing(idx_start:idx_end, 6))
    end if
    out_climate(1)%dsnow(:)   = real(forcing(idx_start:idx_end, 7))

  end function getclimate


  function getco2( nt, forcing, domaininfo, forcingyear, const_co2_year, firstyeartrend ) result( pco2 )
    !////////////////////////////////////////////////////////////////
    !  Function reads this year's atmospheric CO2 from input
    !----------------------------------------------------------------
    ! arguments
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp),  dimension(nt,10), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    type( domaininfo_type ), intent(in) :: domaininfo
    integer, intent(in) :: forcingyear
    integer, intent(in) :: const_co2_year
    integer, intent(in) :: firstyeartrend

    ! function return variable
    real :: pco2

    ! local variables 
    integer :: readyear_idx
    integer :: idx_start, idx_end

    if (const_co2_year/=int(dummy)) then
      readyear_idx = const_co2_year - firstyeartrend + 1
    else  
      readyear_idx = forcingyear - firstyeartrend + 1
    end if

    idx_start = (readyear_idx - 1) * ndayyear + 1
    idx_end   = idx_start + ndayyear - 1

    pco2 = sum(real(forcing(idx_start:idx_end, 8)))/ndayyear

  end function getco2


  function getfapar( nt, forcing,  domaininfo, grid, forcingyear_idx ) result( out_vegcover )
    !////////////////////////////////////////////////////////////////
    ! Function reads this year's atmospheric CO2 from input
    !----------------------------------------------------------------
    ! arguments
    integer,  intent(in) :: nt ! number of time steps
    real(kind=dp),  dimension(nt,10), intent(in)  :: forcing  ! array containing all temporally varying forcing data (rows: time steps; columns: 1=air temperature, 2=rainfall, 3=vpd, 4=ppfd, 5=net radiation, 6=sunshine fraction, 7=snowfall, 8=co2, 9=N-deposition) 
    type( domaininfo_type ), intent(in) :: domaininfo
    type( gridtype ), dimension(1), intent(in) :: grid
    integer, intent(in) :: forcingyear_idx

    ! function return variable
    type( vegcover_type ), dimension(1) :: out_vegcover

    ! local variables 
    integer :: idx_start, idx_end

    idx_start = (forcingyear_idx - 1) * ndayyear + 1
    idx_end   = idx_start + ndayyear - 1

    out_vegcover(1)%dfapar(:) = real(forcing(idx_start:idx_end, 10))

  end function getfapar


  function get_fpc_grid( domaininfo, params_siml ) result( fpc_grid_field )
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
    use md_params_siml, only: paramstype_siml
    use md_params_core, only: npft
    use md_grid, only: domaininfo_type

    ! arguments
    type( domaininfo_type ), intent(in) :: domaininfo
    type( paramstype_siml ), intent(in) :: params_siml

    ! function return variable
    real, dimension(npft,1) :: fpc_grid_field

    ! local variables
    integer :: pft

    ! get binary information of PFT presence from simulation parameters
    fpc_grid_field(:,1) = 0.0

    ! Code below must follow the same structure as in 'plant_pmodel.mod.f90'
    pft = 0
    if ( params_siml%ltre ) then
      ! xxx dirty: call all non-grass vegetation types 'TrE', see indeces above
      pft = pft + 1
      fpc_grid_field(pft,1) = 1.0
    end if 

    if ( params_siml%lgr3 ) then
      ! xxx dirty: call all grass vegetation types 'Gr3'
      pft = pft + 1
      fpc_grid_field(pft,1) = 1.0
    end if

    if ( params_siml%lgr4 ) then
      ! xxx dirty: call all grass vegetation types 'Gr3'
      pft = pft + 1
      fpc_grid_field(pft,1) = 1.0
    end if

    if (pft==0) stop 'get_fpc_grid: no PFT activated accoring to simulation parameter file.'

    if (pft/=npft) stop 'GET_FPC_GRID: Adjust npft manually in params_core.mod.f90'

  end function get_fpc_grid

end module md_forcing

