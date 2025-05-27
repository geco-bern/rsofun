module md_product_pools
    !////////////////////////////////////////////////////////////////
    ! Module containing implementation of product pools
    !----------------------------------------------------------------
    use md_orgpool

    implicit none
    private
    public :: product_pools

    integer, parameter :: n_product_pools = 2 ! Number of product pools
    real, public, parameter ::  e_fold_pp(n_product_pools) = (/2, 20/)          ! e-folding rate of product pools (in years)
    real, public, parameter ::  fraction_pp(n_product_pools) = (/0.375, 0.375/) ! fractions going to each product pool. The residual (1 - sum) is a direct loss going to the atmosphere

    type product_pools

        type(orgpool), private, dimension(n_product_pools) :: product_pool = orgpool()
    
    contains

        procedure update
        procedure get_pool
        
    end type product_pools

  contains

    pure subroutine update(self, products)
      !////////////////////////////////////////////////////////////////
      ! Update product pools by both decaying the current pool values and adding the new products from LUC.
      !----------------------------------------------------------------
      class(product_pools), intent(inout) :: self
      type(orgpool), intent(in) :: products

      ! Decay then add new products
      self%product_pool(1) = self%product_pool(1) * exp(-1.0 / e_fold_pp(1)) + products * fraction_pp(1)
      self%product_pool(2) = self%product_pool(2) * exp(-1.0 / e_fold_pp(2)) + products * fraction_pp(2)
      ! TODO: Fix below issues: Here we are missing to account for:
      !       a) the fraction going directly to the atmosphere, i.e. 1-(fraction_pp(1) + fraction_pp(2)) == 25% of LUC products
      !       b) the fraction that goes from the product pools to the atmosphere, i.e. pool(1) * [1 - exp(-1.0 / e_fold_pp(1))] and  pool(2) * [1 - exp(-1.0 / e_fold_pp(2))]

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