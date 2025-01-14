

program tests
    ! Inout variable
    type(vegn_tile_type), intent(inout) :: vegn
    type(cohort_item), pointer :: ptr => NULL()
    integer :: start_uid

    if (vegn%n_cohorts() /= 0) STOP 'Wrong cohort number (expected 0)'

    ptr => vegn%new_cohort()
    ptr%cohort%height = 10
    start_uid = ptr%uid - 1

    if (vegn%n_cohorts() /= 1) STOP 'Wrong cohort number (expected 1)'

    ptr => vegn%new_cohort()
    ptr%cohort%height = 5

    if (vegn%n_cohorts() /= 2) STOP 'Wrong cohort number (expected 2)'
    if (ptr%uid - start_uid /= 2) STOP 'Wrong uid (expected: 2)'
    if( vegn%next%cohort%height /= 5) STOP 'Wrong value (expected 5)'

    ptr => vegn%new_cohort()
    ptr%cohort%height = 6

    if (vegn%n_cohorts() /= 3) STOP 'Wrong cohort number (expected 3)'
    if (ptr%uid - start_uid /= 3) STOP 'Wrong uid (expected: 3)'
    if( vegn%next%cohort%height /= 6) STOP 'Wrong value (expected 6)'

    ptr => vegn%remove(2) ! second (midle)

    if (vegn%n_cohorts() /= 2) STOP 'Wrong cohort number (expected 2)'
    if( ptr%uid - start_uid /= 1) STOP 'Wrong uid (expected: 1)'
    if( vegn%next%cohort%height /= 6) STOP 'Wrong value (expected 6)'
    if( vegn%next%next%cohort%height /= 10) STOP 'Wrong value (expected 10)'
    if( vegn%next%next%cohort%height /= 10) STOP 'Wrong value (expected 10)'
    if( ptr%cohort%height /= 10) STOP 'Wrong value (expected 10)'

    if( vegn%next%uid - start_uid /= 3) STOP 'Wrong uid (expected: 3)'
    ptr => vegn%remove(3) !first
    if (vegn%n_cohorts() /= 1) STOP 'Wrong cohort number (expected 1)'
    if( vegn%next%cohort%height /= 10) STOP 'Wrong value (expected 10)'
    if( ptr%cohort%height /= 10) STOP 'Wrong value (expected 10)'

    ptr => vegn%remove(3) !not existing uid
    if (vegn%n_cohorts() /= 1) STOP 'Wrong cohort number (expected 1)'
    if (associated(ptr)) STOP 'Expected: FALSE'

    ptr => vegn%remove(1) !last one
    if (vegn%n_cohorts() /= 0) STOP 'Wrong cohort number (expected 0)'
    if (associated(ptr)) STOP 'Expected: FALSE'

    ptr => vegn%new_cohort()
    ptr%cohort%height = 10

    if (vegn%n_cohorts() /= 1) STOP 'Wrong cohort number (expected 1)'
    if (ptr%uid - start_uid /= 4) STOP 'Wrong uid (expected: 4)'

    ptr => vegn%new_cohort()
    ptr%cohort%height = 5

    if (vegn%n_cohorts() /= 2) STOP 'Wrong cohort number (expected 2)'
    if (ptr%uid - start_uid /= 5) STOP 'Wrong uid (expected: 5)'
    if( vegn%next%cohort%height /= 5) STOP 'Wrong value (expected 5)'

    ptr => vegn%remove(4) !last one
    if (vegn%n_cohorts() /= 1) STOP 'Wrong cohort number (expected 1)'
    if (associated(ptr)) STOP 'Expected: FALSE'

    ptr => vegn%remove(5) !last one
    if (vegn%n_cohorts() /= 0) STOP 'Wrong cohort number (expected 0)'
    if (associated(ptr)) STOP 'Expected: FALSE'

    call vegn%sort_cohorts_by_height()

    if (vegn%n_cohorts() /= 0) STOP 'Wrong cohort number (expected 0)'

    ptr => vegn%new_cohort()
    ptr%cohort%height = 10

    if (vegn%n_cohorts() /= 1) STOP 'Wrong cohort number (expected 1)'

    call vegn%sort_cohorts_by_height()

    if (vegn%n_cohorts() /= 1) STOP 'Wrong cohort number (expected 1)'

    ptr => vegn%new_cohort()
    ptr%cohort%height = 2
    ptr => vegn%new_cohort()
    ptr%cohort%height = 5

    if (vegn%n_cohorts() /= 3) STOP 'Wrong cohort number (expected 3)'

    call vegn%sort_cohorts_by_height()

    if (vegn%n_cohorts() /= 3) STOP 'Wrong cohort number (expected 3)'
    if( vegn%next%cohort%height /= 10) STOP 'Wrong value (expected 10)'
    if( vegn%next%next%cohort%height /= 5) STOP 'Wrong value (expected 5)'
    if( vegn%next%next%next%cohort%height /= 2) STOP 'Wrong value (expected 2)'

    STOP
end program tests