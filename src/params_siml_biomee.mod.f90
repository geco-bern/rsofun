module md_params_siml_biomee
  !////////////////////////////////////////////////////////////////
  ! Module for handling simulation parameters for BiomeE.
  !----------------------------------------------------------------
  use md_params_core, only: steering_parameters

  implicit none

  private
  public paramstype_siml

  !----------------------------------------------------------------
  ! Derived type for simulation parameters
  !----------------------------------------------------------------
  type paramstype_siml

    type(steering_parameters) :: steering

    ! integer :: model_run_years
    logical :: outputhourly
    logical :: outputdaily
    logical :: do_U_shaped_mortality
    logical :: update_annualLAImax
    logical :: do_closedN_run
    logical :: do_reset_veg
    integer :: dist_frequency
    character(len=30) :: method_photosynth
    character(len=30) :: method_mortality

  end type paramstype_siml

end module md_params_siml_biomee


