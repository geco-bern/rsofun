module md_product_pools
    !////////////////////////////////////////////////////////////////
    ! Module containing implementation of product pools
    !----------------------------------------------------------------
    use md_orgpool

    implicit none
    private
    public :: product_pools

    integer, parameter :: n_poduct_pools = 2 ! Number of product pools
    integer, public, parameter ::  e_fold_pp(n_poduct_pools) = (/2, 20/) ! e-folding time of product pools (in years)
    real, public, parameter :: DIRECT_LOSS                   = 0.25 ! Fraction directly lost to the athmosphere

    type product_pools

        type(orgpool), private, dimension(n_poduct_pools) :: product_pool = orgpool()
    
    contains

        procedure update
        procedure get_pool
        
    end type product_pools

  contains

    pure subroutine update(self, input)
      !////////////////////////////////////////////////////////////////
      ! Update product pools by both decaying the current pool values and adding the new exports from LUC.
      !----------------------------------------------------------------
      class(product_pools), intent(inout) :: self
      type(orgpool), intent(in) :: input

      ! Local variable
      real :: fraction

      fraction = (1.0 - DIRECT_LOSS) / n_poduct_pools

      self%product_pool(1) = self%product_pool(1) * exp(-1.0 / e_fold_pp(1)) + input * fraction
      self%product_pool(2) = self%product_pool(2) * exp(-1.0 / e_fold_pp(2)) + input * fraction

    end subroutine update

    pure function get_pool(self, i) result(pool)
        !////////////////////////////////////////////////////////////////
        ! Retrieve pool i
        !----------------------------------------------------------------
        class(product_pools), intent(in) :: self
        integer, intent(in) :: i
        type(orgpool) :: pool

        pool = self%product_pool(i)
    end function get_pool

end module md_product_pools