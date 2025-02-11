module md_params_siml_pmodel
  !////////////////////////////////////////////////////////////////
  ! Module for handling simulation parameters for P-model.
  !----------------------------------------------------------------
  use md_params_core, only: steering_parameters

  implicit none

  private
  public paramstype_siml_pmodel

  !----------------------------------------------------------------
  ! Derived type for simulation parameters
  !----------------------------------------------------------------
  type paramstype_siml_pmodel

    type(steering_parameters) :: steering
    integer :: outdt           ! output periodicity
    integer :: outnt           ! number of output time steps per year
    integer :: secs_per_tstep  ! number of seconds per time step (now daily => 60 * 60 * 24)

    character(len=256) :: runname
    character(len=256) :: sitename

    ! optionally prescribed variables (if false, then simulated internally)
    logical :: in_netrad    ! net radiation
    logical :: in_ppfd      ! photosynthetic photon flux density 

    ! activated PFTs
    logical :: ltre        ! evergreen tree
    logical :: ltne        ! evergreen tree, n-fixing
    logical :: ltrd        ! deciduous tree
    logical :: ltnd        ! deciduous tree, n-fixing
    logical :: lgr3        ! grass, c3 photosynthetic pathway
    logical :: lgn3        ! grass, c3 photosynthetic pathway, n-fixing
    logical :: lgr4        ! grass, c4 photosynthetic pathway

    ! integer :: npft        ! number of activated PFTs

  end type paramstype_siml_pmodel

end module md_params_siml_pmodel


