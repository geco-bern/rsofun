module md_cohort_linked_list
  !////////////////////////////////////////////////////////////////
  ! Module containing cohort linked list
  !----------------------------------------------------------------
  ! Do NOT import anything else!
  use md_cohort, only: cohort_type

  ! define data types and constants
  implicit none
  private
  !=============== Public types ===========================================================
  public :: cohort_item

  !=============== Public procedures ===========================================================
  public :: create_cohort, reset_uid, destroy_all, destroy_cohort, sort_cohorts, detach_cohort, insert_head, length

  integer, private :: CurrentCohortUid = 0

  type :: abstract_linked_list

    type(cohort_item), private, pointer :: next_ptr => null() ! Pointer to next cohort_item. Important to nullify here!

    contains

    procedure has_next
    procedure next

  end type abstract_linked_list

  type, extends(abstract_linked_list) :: cohort_item
    integer :: uid ! Unique id. To be filled with 'new_uid()'
    type(cohort_type) :: cohort
  end type cohort_item

contains

  !!!!========= ATTENTION ============!!!
  ! The functions below are notoriously difficult to implement properly.
  ! Be sure to know what you are doing before doing any change, and be sure to
  ! thoroughly test each function which you modify!

  function next(self) result(next_item)
    ! Returns true if this element is followed by another item
    class(abstract_linked_list), intent(in) :: self
    type(cohort_item), pointer :: next_item

    next_item => self%next_ptr
  end function next

  pure function has_next(self) result(res)
    ! Returns true if this element is followed by another item
    class(abstract_linked_list), intent(in) :: self
    logical :: res

    res = associated(self%next_ptr)
  end function has_next

  function length(linked_list) result(res)
    ! Returns the current number of cohorts
    integer :: res
    type(cohort_item), pointer, intent(in) :: linked_list

    ! Local variable
    type(cohort_item), pointer :: it ! iterator

    res = 0

    it => linked_list
    do while (associated(it))
      res = res + 1
      it => it%next()
    end do
  end function length

  function next_uid() result(res)
    ! Get the next unique ID
    ! Should be called when creating new elements
    integer :: res

    CurrentCohortUid = CurrentCohortUid + 1
    res = CurrentCohortUid
  end function next_uid

  function create_cohort() result(new_cohort)
    ! Create a new cohort
    type(cohort_item), pointer :: new_cohort

    new_cohort => null()
    allocate(new_cohort)
    new_cohort%uid = next_uid()

  end function create_cohort

  subroutine reset_uid()
    ! Reset the uinique ID counter
    ! Call this method once when starting a new simulation

    CurrentCohortUid = 0
  end subroutine reset_uid

  subroutine destroy_all(chain)
    ! Destroy the provided chain.
    ! Follow all the element of a chain, freeing the memory for each.
    ! After the subroutine has returned, the parameter takes the value null().
    type(cohort_item), pointer, intent(inout) :: chain

    ! Local variable
    type(cohort_item), pointer :: ptr

    do while (associated(chain))
      ptr => chain
      chain => ptr%next_ptr
      deallocate(ptr)
      ptr => null()
    end do
  end subroutine

  subroutine sort_cohorts(linked_list, increasing, func)
    ! Low-level implementation for sorting cohort
    ! The cohort order is defined by applying the function 'func' to the cohorts and comparing these values
    ! between themselves.
    ! 'increasing' defines if the values should be ranked by increasing order.

    use, intrinsic :: ieee_arithmetic

    interface
      function func_sort(item) result(res)
        import :: cohort_item
        type(cohort_item) :: item
        real :: res
      end function func_sort
    end interface

    type(cohort_item), pointer, intent(inout) :: linked_list
    logical, intent(in) :: increasing
    procedure(func_sort) :: func

    ! Local variable
    type(cohort_item), pointer :: selected_item
    type(cohort_item), pointer :: selected_prev ! Pointer to parent of node pointed by 'selected_item'
    type(cohort_item), pointer :: old_cohorts
    type(cohort_item), pointer :: it !iterator
    type(cohort_item), pointer :: prev ! Pointer to parent of node pointed by 'it'
    logical :: new_winner
    real :: selected_value, current_value ! cache variable
    old_cohorts => linked_list
    linked_list => null()

    ! Repeat until the old list is empty
    do while (associated(old_cohorts))
      it => old_cohorts
      ! We reset the pointers
      prev => null()
      selected_item => null()
      selected_prev => null()
      ! We pick the smallest element of the old list
      do while (associated(it))
        current_value = func(it)
        if (ieee_is_nan(current_value)) then
          ! If the cohort has NA we skip it
          it => destroy_cohort(it, old_cohorts)
          cycle
        end if
        ! The use of new_winner below is meant at implementing the following offending line:
        ! if ((.not. associated(selected_item)) .or. (increasing .neqv. (it%cohort%height() < selected_item%cohort%height()))) then
        ! The line above works fine with -O2, but fails in -O0 as the compiler then evaluate both terms, which creates a
        ! segfault in case selected_item is not associated.
        new_winner = .not. associated(selected_item)
        if (.not. new_winner) new_winner = (increasing .neqv. (current_value < selected_value))
        if (new_winner) then
          selected_item => it
          selected_prev => prev
          selected_value = current_value
        end if
        prev => it
        it => it%next_ptr
      end do

      ! We remove it from the old list
      if (associated(selected_prev)) then
        selected_prev%next_ptr => selected_item%next_ptr
      else
        old_cohorts => selected_item%next_ptr
      end if

      ! We insert it in the head
      selected_item%next_ptr => linked_list
      linked_list => selected_item

    end do
  end subroutine sort_cohorts

  function detach_cohort(item, linked_list) result(next_item)
    ! Remove given item and return next item in the list
    ! or NULL if it was not found (or no item follows in the list)
    ! Attention, the item is not deleted from memory. Use destroy_cohort for this.
    ! ATTENTION: The provided item's next element is set to NULL, even if it was not found.
    type(cohort_item), pointer, intent(in) :: item
    type(cohort_item), pointer, intent(inout) :: linked_list
    type(cohort_item), pointer :: next_item

    ! Local variable
    type(cohort_item), pointer :: it !iterator
    type(cohort_item), pointer :: prev_it

    next_item => null()

    if (associated(item)) then

      it => linked_list
      prev_it => null() ! Important, otherwise may otherwise still be associated from a previous call to this method!

      do while (associated(it))
        if (associated(it, item)) then
          next_item => it%next_ptr
          if (associated(prev_it)) then
            ! We plug the previous item on the next
            prev_it%next_ptr => next_item
          else
            ! Or we plug the head if it was the first element
            linked_list => next_item
          end if
          exit
        else
          prev_it => it
          it => it%next_ptr
        end if
      end do

      ! Commenting the code below may help detecting pointer issues
      !if (.not. associated(it)) then
      !  PRINT *, 'NOT FOUND'
      !  STOP *, item%uid
      !end if

      ! We set next pointer to NULL in the detached item.
      item%next_ptr => null()
    endif
  end function detach_cohort

  function destroy_cohort(item, linked_list) result(next_item)
    ! Destroy item and return next item in the list
    ! or NULL if no item was removed (or no item follows in the list)
    type(cohort_item), pointer :: next_item
    type(cohort_item), pointer, intent(inout) :: item
    type(cohort_item), pointer, intent(inout) :: linked_list

    next_item => detach_cohort(item, linked_list)
    if (associated(item)) then
      deallocate(item)
      item => null()
    end if
  end function destroy_cohort

  subroutine insert_head(new_item, linked_list)
    ! Prepend a new cohort to the list and return its pointer
    type(cohort_item), pointer, intent(in) :: new_item
    type(cohort_item), pointer, intent(inout) :: linked_list

    new_item%next_ptr => linked_list
    linked_list => new_item
  end subroutine insert_head

end module md_cohort_linked_list
