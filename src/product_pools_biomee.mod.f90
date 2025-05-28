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
        type(orgpool), private, dimension(n_product_pools + 1) :: product_loss = orgpool()
    
    contains

        procedure update
        procedure get_pool
        procedure get_loss_rates
        procedure get_direct_loss_rate

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

      ! Keep track of the decaying losses from product pools and direct losses due to LUC
      ! These are yearly loss rates
      self%product_loss(1) = self%product_pool(1) * (1.0 - exp(-1.0 / e_fold_pp(1)))
      self%product_loss(2) = self%product_pool(2) * (1.0 - exp(-1.0 / e_fold_pp(2)))
      self%product_loss(3) = products             * (1.0 - fraction_pp(1) - fraction_pp(1))
      ! The last loss is the direct loss to the atmosphere, i.e the residual of what is not attributed to the product pools

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

    pure function get_loss_rates(self, i) result(loss)
        !////////////////////////////////////////////////////////////////
        ! Retrieve losses from pool i
        !----------------------------------------------------------------
        class(product_pools), intent(in) :: self
        integer, intent(in) :: i
        type(orgpool) :: loss

        loss = self%product_loss(i)
    end function get_loss_rates

    pure function get_direct_loss_rate(self) result(loss)
      !////////////////////////////////////////////////////////////////
      ! Retrieve direct losses never added to any product pool
      !----------------------------------------------------------------
      class(product_pools), intent(in) :: self
      type(orgpool) :: loss

      loss = self%product_loss(n_product_pools + 1) ! For direct losses we need the last element 
    end function get_direct_loss_rate

end module md_product_pools