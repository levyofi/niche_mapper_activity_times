module Observer_module
    use parameters
    use Object_module
    use List_module
    use Grid_module
    use Factory_module
    implicit none
    private

   public :: initialize
   public :: observer_finalize
   public :: get_instance
   public :: m_observer
   public :: validation

    type, public :: observer
        type (list) :: l_individuals
        class (factory), pointer :: m_mouse_factory
    contains
        procedure :: populate
        procedure :: step => observer_step

    end type observer

    type (observer), pointer :: m_observer => null()
    
    logical :: validation = .false.
 contains

    subroutine initialize()
       if (.not. associated(m_observer)) then
          allocate(m_observer)
       end if
    end subroutine initialize


    subroutine get_instance(instance)
        type (observer), pointer, intent(out) :: instance
        call initialize()
        instance = m_observer
    end subroutine get_instance

    subroutine observer_finalize()
       if (associated(m_observer)) then
          call m_observer%m_mouse_factory%print_statistics()
          call m_observer%m_mouse_factory%clear()
          deallocate(m_observer)
       end if
    end subroutine observer_finalize

    subroutine populate(self)
        class (observer) :: self
        class (object), pointer :: new_mouse
        character*10 :: shade
        call self%m_mouse_factory%create(new_mouse)
        call get_command_argument(4, shade) 
        if (shade=="rest") then
          call new_mouse%create_array( (/1., 0./))
          return 
        end if
        if (shade=="shade") then
          call new_mouse%create_array( (/1., 1./))
        else 
          call new_mouse%create_array( (/0., 1./))
        end if
    end subroutine populate
    
    subroutine observer_step(self)
        class (observer) :: self
        call m_observer%m_mouse_factory%step(self%l_individuals)
    end subroutine observer_step


end module Observer_module
