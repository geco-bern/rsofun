module md_cohort_linked_list
  !////////////////////////////////////////////////////////////////
  ! Module defining types implementing a cohort linked list
  ! 'cohort_stack' is the cohort list itself, containing instances of 'cohort_stack_item'.
  ! 'cohort_stack_item' is simply a wrapper around `cohort_type`, adding a unique ID (uid) and a helper method for cloning cohorts.
  !----------------------------------------------------------------

  ! Do NOT import anything else!
  use md_cohort, only: cohort_type

  ! define data types and constants
  implicit none
  private
  !=============== Public types ===========================================================
  public :: cohort_stack_item, cohort_stack

  !=============== Public procedures ===========================================================
  public :: create_item

  type, abstract :: linked_list_abstract_item
    ! Abstract type at the heart of the linked list.
    ! It contains a pointer to the next item and helper methods for iterating the linked list.
    type(cohort_stack_item), private, pointer :: next_ptr => null() ! Pointer to next cohort_stack_item. Important to nullify here!

    contains

    ! unused function: procedure has_next
    procedure next

  end type linked_list_abstract_item

  type, extends(linked_list_abstract_item) :: cohort_stack_item
    ! Wrapper around `cohort_type`, adding a unique ID (uid) and a helper method for cloning cohorts.
    integer, private :: uid_internal = 0 ! Unique id. If 0, it is automatically created upon insertion into a linked_list.
    type(cohort_type) :: cohort

    contains

    procedure uid
    procedure clone

  end type cohort_stack_item

  type :: cohort_stack
    ! The cohort list itself, containing instances of 'cohort_stack_item'.
    ! It defines a number of important methods for modifying the list (adding and removing items, sorting, ...)
    integer, private :: last_created_uid = 0
    type(cohort_stack_item), private, pointer :: head_internal => null() ! Pointer to head of linked list. Important to nullify here!

  contains

    procedure length
    procedure head
    procedure destroy_all
    procedure detach_item
    procedure destroy_item
    procedure insert_item
    procedure sort
    procedure, private :: create_uid

  end type cohort_stack

contains

  !----------------------------------------------------------------
  ! cohort_stack_item
  !----------------------------------------------------------------

  pure function uid(self) result(res)
    !////////////////////////////////////////////////////////////////
    ! Returns the uid
    !---------------------------------------------------------------
    class(cohort_stack_item), intent(in) :: self
    integer :: res

    res = self%uid_internal
  end function uid

  pure function clone(self, same_uid) result(ptr)
    !////////////////////////////////////////////////////////////////
    ! Clone this item.
    ! If same_uid is true, the uid is copied, if not a new uid is created
    ! Note: to create a new cohort from scratch, use create_cohort() instead.
    !---------------------------------------------------------------
    class(cohort_stack_item), intent(in) :: self
    logical, intent(in) :: same_uid

    ! Local variable
    type(cohort_stack_item), pointer :: ptr

    ptr => create_item()
    ptr%cohort = self%cohort
    if (same_uid) then
      ptr%uid_internal = self%uid_internal
    else
      ptr%uid_internal = 0 ! Will be created when inserted into a stack
    end if

  end function clone

  !----------------------------------------------------------------
  ! cohort_stack
  !----------------------------------------------------------------

  !!!!===================== ATTENTION ============================!!!!
  ! The functions below are notoriously difficult to implement properly.
  ! Be sure to know what you are doing before doing any change, and be sure to
  ! thoroughly test each function that you modify!
  !
  ! The advantage is that all the trickiness is now encapsulated into a low
  ! level type, rather than spread throughout the code.
  !---------------------------------------------------------------

  function length(self) result(res)
    !////////////////////////////////////////////////////////////////
    ! Returns the current number of items in this linked list
    !---------------------------------------------------------------
    integer :: res
    class(cohort_stack), intent(in) :: self

    ! Local variable
    type(cohort_stack_item), pointer :: it ! iterator

    res = 0

    it => self%head_internal
    do while (associated(it))
      res = res + 1
      it => it%next_ptr
    end do
  end function length

  function head(self) result(ptr)
    !////////////////////////////////////////////////////////////////
    ! Destroy all items in this linked list.
    ! Follow all the element of a chain, freeing the memory for each.
    ! After the subroutine has returned, the parameter takes the value null().
    !---------------------------------------------------------------
    class(cohort_stack), intent(in) :: self

    ! Local variable
    type(cohort_stack_item), pointer :: ptr

    ptr => self%head_internal

  end function

  pure function create_item() result(new_itm)
    ! Create a new cohort
    type(cohort_stack_item), pointer :: new_itm

    new_itm => null()
    allocate(new_itm)

  end function create_item

  pure subroutine destroy_all(self)
    !////////////////////////////////////////////////////////////////
    ! Destroy all items in this linked list.
    ! Follow all the element of a chain, freeing the memory for each.
    ! After the subroutine has returned, the parameter takes the value null().
    !---------------------------------------------------------------
    class(cohort_stack), intent(inout) :: self

    ! Local variable
    type(cohort_stack_item), pointer :: ptr

    do while (associated(self%head_internal))
      ptr => self%head_internal
      self%head_internal => ptr%next_ptr
      deallocate(ptr)
      ptr => null()
    end do
  end subroutine

  pure subroutine sort(self, increasing, func)
    !////////////////////////////////////////////////////////////////
    ! Sort items given a function 'func' mapping an item to a real value.
    ! 'increasing' tells if the values should be ranked by increasing order or not.
    !---------------------------------------------------------------

    interface
      pure function func_sort(item) result(res)
        import :: cohort_stack_item
        type(cohort_stack_item), intent(in) :: item
        real :: res
      end function func_sort
    end interface

    class(cohort_stack), intent(inout) :: self
    logical, intent(in) :: increasing
    procedure(func_sort) :: func

    ! Local variable
    type(cohort_stack_item), pointer :: selected_item
    type(cohort_stack_item), pointer :: selected_prev ! Pointer to parent of node pointed by 'selected_item'
    type(cohort_stack_item), pointer :: old_cohorts
    type(cohort_stack_item), pointer :: it !iterator
    type(cohort_stack_item), pointer :: prev ! Pointer to parent of node pointed by 'it'
    logical :: new_winner
    real :: selected_value, current_value ! cache variable


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
  end subroutine sort

  function detach_item(self, item) result(next_item)
    !////////////////////////////////////////////////////////////////
    ! Remove given item and return next item in the list
    ! or NULL if it was not found (or no item follows in the list)
    ! Attention, the item is not deleted from memory. Use destroy_item for this.
    ! ATTENTION: The provided item's next element is set to NULL, even if it was not found.
    !---------------------------------------------------------------
    class(cohort_stack), intent(inout) :: self
    type(cohort_stack_item), pointer, intent(in) :: item
    type(cohort_stack_item), pointer :: next_item

    ! Local variable
    type(cohort_stack_item), pointer :: it !iterator
    type(cohort_stack_item), pointer :: prev_it

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
  end function detach_item

  function destroy_item(self, item) result(next_item)
    !////////////////////////////////////////////////////////////////
    ! Destroy item and return next item in the list
    ! or NULL if no item was removed (or no item follows in the list)
    !---------------------------------------------------------------
    class(cohort_stack), intent(inout) :: self
    type(cohort_stack_item), pointer :: next_item
    type(cohort_stack_item), pointer, intent(inout) :: item

    next_item => self%detach_item(item)
    if (associated(item)) then
      deallocate(item)
      item => null()
    end if
  end function destroy_item

  subroutine insert_item(self, new_item)
    !////////////////////////////////////////////////////////////////
    ! Prepend a new item to the head of the list and return its pointer
    !---------------------------------------------------------------
    class(cohort_stack), intent(inout) :: self
    type(cohort_stack_item), pointer, intent(in) :: new_item

    ! If the uid is not set, we create and set a new one with create_uid()
    if (new_item%uid_internal == 0) new_item%uid_internal = self%create_uid()
    new_item%next_ptr => self%head_internal
    self%head_internal => new_item
  end subroutine insert_item

  function create_uid(self) result(res)
    !////////////////////////////////////////////////////////////////
    ! Get a new unique ID
    ! Private
    !---------------------------------------------------------------
    class(cohort_stack), intent(inout) :: self
    integer :: res

    self%last_created_uid = self%last_created_uid + 1
    res = self%last_created_uid
  end function create_uid

  !----------------------------------------------------------------
  ! linked_list_abstract_item
  !----------------------------------------------------------------

  function next(self) result(next_item)
    !////////////////////////////////////////////////////////////////
    ! Returns the next item, or NULL if none.
    !---------------------------------------------------------------
    class(linked_list_abstract_item), intent(in) :: self
    type(cohort_stack_item), pointer :: next_item

    next_item => self%next_ptr
  end function next

  ! unused function pure function has_next(self) result(res)
  ! unused function   !////////////////////////////////////////////////////////////////
  ! unused function   ! Returns true if this element is followed by another item
  ! unused function   !---------------------------------------------------------------
  ! unused function   class(linked_list_abstract_item), intent(in) :: self
  ! unused function   logical :: res
  ! unused function 
  ! unused function   res = associated(self%next_ptr)
  ! unused function end function has_next

end module md_cohort_linked_list
