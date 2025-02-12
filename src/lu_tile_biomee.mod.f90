module md_lu_tile_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing fractional land unit (LU) tiles for BiomeE
  !----------------------------------------------------------------
  use md_vegetation_tile_biomee, only: vegn_tile_type

  implicit none

  private
  public :: lu_tile

  type lu_tile
    type(vegn_tile_type) :: vegn
    real :: fraction = 0.0

  contains

    procedure non_empty
    procedure shut_down

  end type lu_tile

contains

  pure logical function non_empty(self)
    class(lu_tile), intent(in) :: self

    non_empty = self%fraction > 0.0

  end function non_empty

  subroutine shut_down(self)
    class(lu_tile), intent(inout) :: self

    self%fraction = 0.0
    call self%vegn%shut_down()

  end subroutine shut_down


end module md_lu_tile_biomee
