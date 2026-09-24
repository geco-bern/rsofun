module md_debug_print
  !////////////////////////////////////////////////////////////////
  ! Optional state-printing helpers for debugging BiomeE simulations.
  !----------------------------------------------------------------
  implicit none

  private
  public :: debug_print_state, debug_print_restart_state

contains

  subroutine debug_print_state(steering_state, aggregat)
    use md_aggregated_tile_biomee, only: aggregated_tile
    use md_params_core, only: outtype_steering
    use md_orgpool, only: orgpool

    implicit none

    type(outtype_steering), intent(in) :: steering_state
    class(aggregated_tile), intent(in) :: aggregat

    type(orgpool) :: prod_pool
    integer :: lu_idx, pool_idx

    print *, 'DEBUG BIOMEE STATE START year_loop=', steering_state%year
    call debug_print_steering_state(steering_state)

    do pool_idx = 1, 2
      prod_pool = aggregat%prod_pools%get_pool(pool_idx)
      print *, 'DEBUG BIOMEE STATE product_pool=', pool_idx, ' c12=', prod_pool%c12, &
        ' d13=', prod_pool%d13, ' n14=', prod_pool%n14
    end do
    print *, ' '

    do lu_idx = 1, aggregat%n_lu()
      call debug_print_lu_state('DEBUG BIOMEE STATE', lu_idx, aggregat%tiles(lu_idx))
    end do

    print *, 'DEBUG BIOMEE STATE END year_loop=', steering_state%year

  end subroutine debug_print_state

  subroutine debug_print_steering_state(steering_state)
    use md_params_core, only: outtype_steering

    implicit none

    type(outtype_steering), intent(in) :: steering_state

    print *, 'DEBUG BIOMEE STATE steering year=', steering_state%year, ' climateyear=', steering_state%climateyear, &
      ' climateyear_idx=', steering_state%climateyear_idx, ' forcingyear=', steering_state%forcingyear, &
      ' forcingyear_idx=', steering_state%forcingyear_idx, ' outyear=', steering_state%outyear
    print *, 'DEBUG BIOMEE STATE steering spinup=', steering_state%spinup, ' init=', steering_state%init, &
      ' finalize=', steering_state%finalize, ' daily_reporting=', steering_state%daily_reporting, &
      ' daily_report_idx=', steering_state%daily_report_idx, ' cohort_reporting=', steering_state%cohort_reporting, &
      ' cohort_report_idx=', steering_state%cohort_report_idx
    print *, ' '

  end subroutine debug_print_steering_state

  subroutine debug_print_lu_state(label, lu_idx, lu)
    use md_lu_tile_biomee, only: lu_tile
    use md_cohort_linked_list, only: cohort_stack_item

    implicit none

    character(len=*), intent(in) :: label
    integer, intent(in) :: lu_idx
    class(lu_tile), intent(in) :: lu

    type(cohort_stack_item), pointer :: it

    call debug_print_lu_header('DEBUG BIOMEE STATE LU')
    call debug_print_label_cell(trim(label)//' LU')
    call debug_print_int_cell(lu_idx)
    call debug_print_real_cell(lu%fraction)
    call debug_print_logical_cell(lu%non_empty())
    call debug_print_int_cell(lu%vegn%n_cohorts())
    call debug_print_real_cell(lu%vegn%age)
    call debug_print_real_cell(lu%vegn%density)
    call debug_print_real_cell(lu%vegn%pleaf%c12)
    call debug_print_real_cell(lu%vegn%pleaf%n14)
    call debug_print_real_cell(lu%vegn%proot%c12)
    call debug_print_real_cell(lu%vegn%proot%n14)
    call debug_print_real_cell(lu%vegn%psapw%c12)
    call debug_print_real_cell(lu%vegn%psapw%n14)
    call debug_print_real_cell(lu%vegn%pwood%c12)
    call debug_print_real_cell(lu%vegn%pwood%n14)
    call debug_print_real_cell(lu%vegn%pseed%c12)
    call debug_print_real_cell(lu%vegn%pseed%n14)
    call debug_print_real_cell(lu%vegn%plabl%c12)
    call debug_print_real_cell(lu%vegn%plabl%n14)
    call debug_print_real_cell(lu%vegn%psoil_fs%c12)
    call debug_print_real_cell(lu%vegn%psoil_fs%n14)
    call debug_print_real_cell(lu%vegn%psoil_sl%c12)
    call debug_print_real_cell(lu%vegn%psoil_sl%n14)
    call debug_print_real_cell(lu%vegn%pmicr%c12)
    call debug_print_real_cell(lu%vegn%pmicr%d13)
    call debug_print_real_cell(lu%vegn%pmicr%n14)
    call debug_print_real_cell(lu%vegn%inorg%c12)
    call debug_print_real_cell(lu%vegn%inorg%n14)
    call debug_print_real_cell(lu%vegn%wcl(1))
    call debug_print_real_cell(lu%vegn%wcl(2))
    call debug_print_real_cell(lu%vegn%wcl(3))
    call debug_print_real_cell(lu%vegn%tk_pheno)
    call debug_print_real_cell(lu%vegn%initialN0)
    call debug_print_real_cell(lu%vegn%annualN)
    call debug_print_real_cell(lu%vegn%totC)
    call debug_print_real_cell(lu%vegn%totN)
    call debug_print_real_cell(lu%vegn%totSeed%c12)
    call debug_print_real_cell(lu%vegn%totSeed%n14)
    call debug_print_real_cell(lu%vegn%totNewC%c12)
    call debug_print_real_cell(lu%vegn%totNewC%n14)
    call debug_print_real_cell(lu%vegn%tk_daily)
    call debug_print_real_cell(lu%vegn%tc_soil)
    write(*, *)
    print *, ' '

    call debug_print_cohort_header('DEBUG BIOMEE STATE COHORT')
    it => lu%vegn%cohorts()
    do while (associated(it))
      call debug_print_cohort_state(label, lu_idx, it)
      it => it%next()
    end do
    print *, ' '

    print *, trim(label), ' lu=', lu_idx, ' dtemp_pvy_allocated=', allocated(lu%vegn%dtemp_pvy), &
      ' wscal_pvy_allocated=', allocated(lu%vegn%wscal_pvy)
    if (allocated(lu%vegn%dtemp_pvy)) then
      print *, trim(label), ' lu=', lu_idx, ' dtemp_pvy=', lu%vegn%dtemp_pvy
    end if
    if (allocated(lu%vegn%wscal_pvy)) then
      print *, trim(label), ' lu=', lu_idx, ' wscal_pvy=', lu%vegn%wscal_pvy
    end if
    print *, trim(label), ' lu=', lu_idx, ' wscal_alldays=', lu%vegn%wscal_alldays

  end subroutine debug_print_lu_state

  subroutine debug_print_cohort_state(label, lu_idx, it)
    use md_cohort_linked_list, only: cohort_stack_item

    implicit none

    character(len=*), intent(in) :: label
    integer, intent(in) :: lu_idx
    type(cohort_stack_item), pointer, intent(in) :: it

    call debug_print_label_cell(trim(label)//' COHORT')
    call debug_print_int_cell(lu_idx)
    call debug_print_int_cell(it%uid())
    call debug_print_int_cell(it%cohort%species)
    call debug_print_real_cell(it%cohort%density)
    call debug_print_real_cell(it%cohort%age)
    call debug_print_int_cell(it%cohort%layer)
    call debug_print_int_cell(it%cohort%firstlayer)
    call debug_print_int_cell(it%cohort%status)
    call debug_print_real_cell(it%cohort%gdd)
    call debug_print_real_cell(it%cohort%leaf_age)
    call debug_print_real_cell(it%cohort%topyear)
    call debug_print_real_cell(it%cohort%bl_max)
    call debug_print_real_cell(it%cohort%br_max)
    call debug_print_real_cell(it%cohort%pleaf%c12)
    call debug_print_real_cell(it%cohort%pleaf%n14)
    call debug_print_real_cell(it%cohort%proot%c12)
    call debug_print_real_cell(it%cohort%proot%n14)
    call debug_print_real_cell(it%cohort%psapw%c12)
    call debug_print_real_cell(it%cohort%psapw%n14)
    call debug_print_real_cell(it%cohort%pwood%c12)
    call debug_print_real_cell(it%cohort%pwood%n14)
    call debug_print_real_cell(it%cohort%pseed%c12)
    call debug_print_real_cell(it%cohort%pseed%n14)
    call debug_print_real_cell(it%cohort%plabl%c12)
    call debug_print_real_cell(it%cohort%plabl%n14)
    write(*, *)

  end subroutine debug_print_cohort_state

  subroutine debug_print_lu_header(label)
    implicit none

    character(len=*), intent(in) :: label

    call debug_print_label_cell(trim(label))
    call debug_print_header_cell('lu')
    call debug_print_header_cell('fraction')
    call debug_print_header_cell('non_empty')
    call debug_print_header_cell('n_cohorts')
    call debug_print_header_cell('age')
    call debug_print_header_cell('density')
    call debug_print_header_cell('pleaf_c')
    call debug_print_header_cell('pleaf_n')
    call debug_print_header_cell('proot_c')
    call debug_print_header_cell('proot_n')
    call debug_print_header_cell('psapw_c')
    call debug_print_header_cell('psapw_n')
    call debug_print_header_cell('pwood_c')
    call debug_print_header_cell('pwood_n')
    call debug_print_header_cell('pseed_c')
    call debug_print_header_cell('pseed_n')
    call debug_print_header_cell('plabl_c')
    call debug_print_header_cell('plabl_n')
    call debug_print_header_cell('psoilfs_c')
    call debug_print_header_cell('psoilfs_n')
    call debug_print_header_cell('psoilsl_c')
    call debug_print_header_cell('psoilsl_n')
    call debug_print_header_cell('pmicr_c')
    call debug_print_header_cell('pmicr_d13')
    call debug_print_header_cell('pmicr_n')
    call debug_print_header_cell('inorg_c')
    call debug_print_header_cell('inorg_n')
    call debug_print_header_cell('wcl1')
    call debug_print_header_cell('wcl2')
    call debug_print_header_cell('wcl3')
    call debug_print_header_cell('tk_pheno')
    call debug_print_header_cell('initialN0')
    call debug_print_header_cell('annualN')
    call debug_print_header_cell('totC')
    call debug_print_header_cell('totN')
    call debug_print_header_cell('totSeed_c')
    call debug_print_header_cell('totSeed_n')
    call debug_print_header_cell('totNewC_c')
    call debug_print_header_cell('totNewC_n')
    call debug_print_header_cell('tk_daily')
    call debug_print_header_cell('tc_soil')
    write(*, *)

  end subroutine debug_print_lu_header

  subroutine debug_print_cohort_header(label)
    implicit none

    character(len=*), intent(in) :: label

    call debug_print_label_cell(trim(label))
    call debug_print_header_cell('lu')
    call debug_print_header_cell('cohort_uid')
    call debug_print_header_cell('species')
    call debug_print_header_cell('density')
    call debug_print_header_cell('age')
    call debug_print_header_cell('layer')
    call debug_print_header_cell('firstlayer')
    call debug_print_header_cell('status')
    call debug_print_header_cell('gdd')
    call debug_print_header_cell('leaf_age')
    call debug_print_header_cell('topyear')
    call debug_print_header_cell('bl_max')
    call debug_print_header_cell('br_max')
    call debug_print_header_cell('pleaf_c')
    call debug_print_header_cell('pleaf_n')
    call debug_print_header_cell('proot_c')
    call debug_print_header_cell('proot_n')
    call debug_print_header_cell('psapw_c')
    call debug_print_header_cell('psapw_n')
    call debug_print_header_cell('pwood_c')
    call debug_print_header_cell('pwood_n')
    call debug_print_header_cell('pseed_c')
    call debug_print_header_cell('pseed_n')
    call debug_print_header_cell('plabl_c')
    call debug_print_header_cell('plabl_n')
    write(*, *)

  end subroutine debug_print_cohort_header

  subroutine debug_print_label_cell(value)
    implicit none

    character(len=*), intent(in) :: value

    write(*, '(a24,1x)', advance='no') adjustl(value)

  end subroutine debug_print_label_cell

  subroutine debug_print_header_cell(value)
    implicit none

    character(len=*), intent(in) :: value

    write(*, '(a12,1x)', advance='no') adjustl(value)

  end subroutine debug_print_header_cell

  subroutine debug_print_real_cell(value)
    implicit none

    real, intent(in) :: value

    write(*, '(es12.4,1x)', advance='no') value

  end subroutine debug_print_real_cell

  subroutine debug_print_int_cell(value)
    implicit none

    integer, intent(in) :: value

    write(*, '(i12,1x)', advance='no') value

  end subroutine debug_print_int_cell

  subroutine debug_print_logical_cell(value)
    implicit none

    logical, intent(in) :: value

    write(*, '(l12,1x)', advance='no') value

  end subroutine debug_print_logical_cell

  subroutine debug_print_restart_state(output_restart_cohorts, output_restart_soil)
    use, intrinsic :: iso_c_binding, only: c_double

    implicit none

    real(kind=c_double), dimension(:, :, :), intent(in) :: output_restart_cohorts
    real(kind=c_double), dimension(:, :), intent(in) :: output_restart_soil

    integer :: lu_idx

    print *, 'DEBUG BIOMEE RESTART START'
    print *, 'DEBUG BIOMEE RESTART output_restart_soil='
    print *, output_restart_soil

    do lu_idx = 1, size(output_restart_cohorts, 3)
      print *, 'DEBUG BIOMEE RESTART lu=', lu_idx, ' output_restart_cohorts='
      print *, output_restart_cohorts(:, :, lu_idx)
    end do

    print *, 'DEBUG BIOMEE RESTART END'

  end subroutine debug_print_restart_state

end module md_debug_print
