module md_params_soil_biomee
  !////////////////////////////////////////////////////////////////
  ! Module handling soil parameters for BiomeE
  !----------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, mydb=>real64, mysg=>real32
  use md_params_core, only: n_dim_soil_types

  implicit none

  private
  public paramtype_soil, getsoil

  type paramtype_soil
    real(kind=mysg), dimension(n_dim_soil_types) :: GMD
    real(kind=mysg), dimension(n_dim_soil_types) :: GSD
    real(kind=mysg), dimension(n_dim_soil_types) :: vwc_sat
    real(kind=mysg), dimension(n_dim_soil_types) :: chb
    real(kind=mysg), dimension(n_dim_soil_types) :: psi_sat_ref
    real(kind=mysg), dimension(n_dim_soil_types) :: k_sat_ref
    real(kind=mysg), dimension(n_dim_soil_types) :: alphaSoil
    real(kind=mysg), dimension(n_dim_soil_types) :: heat_capacity_dry
  end type paramtype_soil

contains

  function getsoil( params_soil_in ) result( params_soil_out )
    !////////////////////////////////////////////////////////////////
    ! Function to calculate soil parameters from texture info.
    !----------------------------------------------------------------
    ! arguments
    real(kind=mydb), dimension(n_dim_soil_types,8), intent(in) :: params_soil_in

    ! function return variable
    type(paramtype_soil) :: params_soil_out

    params_soil_out%GMD(:)               = real(params_soil_in(:,1))               
    params_soil_out%GSD(:)               = real(params_soil_in(:,2))               
    params_soil_out%vwc_sat(:)           = real(params_soil_in(:,3))           
    params_soil_out%chb(:)               = real(params_soil_in(:,4))               
    params_soil_out%psi_sat_ref(:)       = real(params_soil_in(:,5))       
    params_soil_out%k_sat_ref(:)         = real(params_soil_in(:,6))         
    params_soil_out%alphaSoil(:)         = real(params_soil_in(:,7))         
    params_soil_out%heat_capacity_dry(:) = real(params_soil_in(:,8)) 

  end function getsoil

end module md_params_soil_biomee

