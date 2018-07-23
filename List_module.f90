module List_module
    use Object_module
    implicit none


    type, public :: record
       class (object), pointer :: data => null()
       type ( record ), pointer :: previous => null()
       type ( record ), pointer :: next => null()

       contains
           procedure :: remove
!           procedure :: print => record_print
           final :: record_destructor

     end type record

    interface record
        module procedure create_record
    end interface

    type, public :: list
        type ( record ), pointer :: head => null()
        type ( record ), pointer :: tail => null()
    contains
        procedure :: list_add_no_next
        procedure :: list_add_with_next
        generic :: add => list_add_with_next, list_add_no_next
        procedure :: list_count
        procedure :: clear => list_clear
    end type list

    interface list
        module procedure create_list
    end interface

contains

    subroutine create_record(self)
        type (Record) :: self
        nullify(self%data, self%previous, self%next)
    end subroutine

     subroutine remove(self, m_list)
        class(Record) :: self
        class (list) :: m_list
        if (associated(self%previous)) then
            self%previous%next => self%next
        else
            m_list%head => self%next
        end if

        if (associated(self%next)) then
            self%next%previous => self%previous
        else
            m_list%tail => self%previous
        end if
        nullify(self%previous)
        nullify(self%next)
    end subroutine

    subroutine record_destructor(self)
         type(Record) :: self

         nullify(self%data, self%previous, self%next)
       end subroutine record_destructor

   subroutine create_list(self)
        type(list) :: self
        nullify(self%head, self%tail)
    end subroutine create_list

    subroutine list_add_with_next(self, data, next)

        class(list) :: self
        class(object), target :: data
        type(record), target :: next

        next%data => data

        if (.not. associated(self%head)) then
            self%head => next
        end if
        if (.not. associated(self%tail)) then
            self%tail => next
        else
            next%previous => self%tail
            self%tail%next => next
            self%tail => next
        end if
    end subroutine

    subroutine list_add_no_next(self, data)
        class(list) :: self
        class(object), target :: data
        type(record), pointer :: next

        allocate(next)
        call self%add(data, next)
    end subroutine list_add_no_next


    function list_count(self) result(m_count)
        class(list), intent(in) :: self
        type(record), pointer :: cur_record
        integer :: m_count

        m_count=0
        cur_record => self%head
        do while (associated(cur_record))
            cur_record => cur_record%next
            m_count = m_count+1
        end do
    end function list_count

    subroutine list_clear(self, with_allocated_records)
        class(list), intent(in) :: self
        type(record), pointer :: cur_record, next_record
        class(object), pointer :: cur_data
        logical, optional :: with_allocated_records
        print *, "starting clean ", self%list_count(), " items"
        cur_record => self%head
        do while (associated(cur_record))
            next_record => cur_record%next
            cur_data => cur_record%data
            if (present(with_allocated_records)) then
                if (with_allocated_records) then
                    deallocate(cur_record)
                end if
            end if
            if (associated(cur_data)) then
                !print *, "deallocating object"
                deallocate(cur_data)
                nullify(cur_data)
                !print *, "finished deallocating object"
            end if
            cur_record => next_record
        end do
        print *, "end clean"
    end subroutine list_clear

end module List_module
