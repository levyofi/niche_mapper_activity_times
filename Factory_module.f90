module Factory_module
    use Object_module
    use List_module
    implicit none

    type, public, abstract :: Factory
    contains
        procedure (factory_create_abstract), nopass, deferred :: create
        procedure (factory_step_abstract), nopass, deferred :: step
        procedure (factory_clear_abstract), deferred :: clear
        procedure (factory_statistics_abstract), deferred :: print_statistics
    end type Factory

    abstract interface
        subroutine factory_create_abstract(m_object)
            import :: Factory
            import :: Object
            class(object), pointer :: m_object
        end subroutine factory_create_abstract

        subroutine factory_step_abstract(m_list)
            import :: Factory
            import
            class(list) :: m_list
        end subroutine factory_step_abstract

        subroutine factory_clear_abstract(self)
            import :: Factory
            import
            class(factory) :: self
        end subroutine factory_clear_abstract

        subroutine factory_statistics_abstract(self)
            import :: Factory
            import
            class(factory) :: self

        end subroutine factory_statistics_abstract
      end interface
end module Factory_module
