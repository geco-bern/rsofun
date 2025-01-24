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
  public :: cohort_item, cohort_pool

  !=============== Public procedures ===========================================================
  public :: create_cohort

  type :: cohort_pool_item

    type(cohort_item), private, pointer :: next_ptr => null() ! Pointer to next cohort_item. Important to nullify here!

    contains

    procedure has_next
    procedure next

  end type cohort_pool_item

  type, extends(cohort_pool_item) :: cohort_item
    integer, private :: uid_internal = 0 ! Unique id. It is automatically set when inserted in a linked_list if 0.
    type(cohort_type) :: cohort

    contains

    procedure uid

  end type cohort_item

  type :: cohort_pool
    integer, private :: current_uid = 0
    type(cohort_item), private, pointer :: head_internal => null() ! Pointer to head of linked list. Important to nullify here!

  contains

    procedure length
    procedure head
    procedure :: destroy_all
    procedure :: detach_cohort
    procedure :: destroy_cohort
    procedure :: insert_item
    procedure :: sort_cohorts
    procedure, private :: next_uid

  end type cohort_pool

contains

  !!!!========= ATTENTION ============!!!
  ! The functions below are notoriously difficult to implement properly.
  ! Be sure to know what you are doing before doing any change, and be sure to
  ! thoroughly test each function which you modify!

  function uid(self) result(res)
    ! Returns the uid
    class(cohort_item), intent(in) :: self
    integer :: res

    res = self%uid_internal
  end function uid

  function next(self) result(next_item)
    ! Returns the next item, or NULL if none.
    class(cohort_pool_item), intent(in) :: self
    type(cohort_item), pointer :: next_item

    next_item => self%next_ptr
  end function next

  pure function has_next(self) result(res)
    ! Returns true if this element is followed by another item
    class(cohort_pool_item), intent(in) :: self
    logical :: res

    res = associated(self%next_ptr)
  end function has_next

  function length(self) result(res)
    ! Returns the current number of items in this linked list
    integer :: res
    class(cohort_pool), intent(in) :: self

    ! Local variable
    type(cohort_item), pointer :: it ! iterator

    res = 0

    it => self%head_internal
    do while (associated(it))
      res = res + 1
      it => it%next()
    end do
  end function length

  function head(self) result(ptr)
    ! Destroy all items in this linked list.
    ! Follow all the element of a chain, freeing the memory for each.
    ! After the subroutine has returned, the parameter takes the value null().
    class(cohort_pool), intent(in) :: self

    ! Local variable
    type(cohort_item), pointer :: ptr

    ptr => self%head_internal

  end function

  function next_uid(self) result(res)
    ! Get the next unique ID
    ! Private
    class(cohort_pool), intent(inout) :: self
    integer :: res

    self%current_uid = self%current_uid + 1
    res = self%current_uid
  end function next_uid

  function create_cohort() result(new_cohort)
    ! Create a new cohort
    type(cohort_item), pointer :: new_cohort

    new_cohort => null()
    allocate(new_cohort)

  end function create_cohort

  subroutine destroy_all(self)
    ! Destroy all items in this linked list.
    ! Follow all the element of a chain, freeing the memory for each.
    ! After the subroutine has returned, the parameter takes the value null().
    class(cohort_pool), intent(inout) :: self

    ! Local variable
    type(cohort_item), pointer :: ptr

    do while (associated(self%head_internal))
      ptr => self%head_internal
      self%head_internal => ptr%next_ptr
      deallocate(ptr)
      ptr => null()
    end do
  end subroutine

  subroutine sort_cohorts(self, increasing, func)
    ! Sort items given a function 'func' mapping an item to a real value.
    ! 'increasing' defines if the values should be ranked by increasing order.

    use, intrinsic :: ieee_arithmetic

    interface
      function func_sort(item) result(res)
        import :: cohort_item
        type(cohort_item) :: item
        real :: res
      end function func_sort
    end interface

    class(cohort_pool), intent(inout) :: self
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

    ! We start by deleting malformed items
    ! Is that really necessary???
    it => self%head_internal
    do while (associated(it))
      if (ieee_is_nan(func(it))) then
        ! If the cohort has NA we skip it
        it => self%destroy_cohort(it)
      else
        it => it%next()
      end if
    end do

    old_cohorts => self%head_internal
    self%head_internal => null()

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
      selected_item%next_ptr => self%head_internal
      self%head_internal => selected_item

    end do
  end subroutine sort_cohorts

  function detach_cohort(self, item) result(next_item)
    ! Remove given item and return next item in the list
    ! or NULL if it was not found (or no item follows in the list)
    ! Attention, the item is not deleted from memory. Use destroy_cohort for this.
    ! ATTENTION: The provided item's next element is set to NULL, even if it was not found.
    class(cohort_pool), intent(inout) :: self
    type(cohort_item), pointer, intent(in) :: item
    type(cohort_item), pointer :: next_item

    ! Local variable
    type(cohort_item), pointer :: it !iterator
    type(cohort_item), pointer :: prev_it

    next_item => null()

    if (associated(item)) then

      it => self%head_internal
      prev_it => null() ! Important, otherwise may otherwise still be associated from a previous call to this method!

      do while (associated(it))
        if (associated(it, item)) then
          next_item => it%next_ptr
          if (associated(prev_it)) then
            ! We plug the previous item on the next
            prev_it%next_ptr => next_item
          else
            ! Or we plug the head if it was the first element
            self%head_internal => next_item
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

  function destroy_cohort(self, item) result(next_item)
    ! Destroy item and return next item in the list
    ! or NULL if no item was removed (or no item follows in the list)
    class(cohort_pool), intent(inout) :: self
    type(cohort_item), pointer :: next_item
    type(cohort_item), pointer, intent(inout) :: item

    next_item => self%detach_cohort(item)
    if (associated(item)) then
      deallocate(item)
      item => null()
    end if
  end function destroy_cohort

  subroutine insert_item(self, new_item)
    ! Prepend a new cohort to the list and return its pointer
    class(cohort_pool), intent(inout) :: self
    type(cohort_item), pointer, intent(in) :: new_item

    if (new_item%uid_internal == 0) new_item%uid_internal = self%next_uid()
    new_item%next_ptr => self%head_internal
    self%head_internal => new_item
  end subroutine insert_item

end module md_cohort_linked_list
