module Object_module

   type, public, abstract :: Object
        integer :: x, y
        integer(kind=4) :: id
    contains
        procedure (step_init_abstract), pass(self), deferred :: step_init
        procedure (create_array_abstract), pass(self), deferred :: create_array
        procedure (create_abstract), pass(self), deferred :: create
    end type Object

    contains
        subroutine step_init_abstract(self)
            class(Object) :: self
        end subroutine step_init_abstract

        subroutine create_abstract(self)
            class(Object) :: self
        end subroutine create_abstract
        
        subroutine create_array_abstract(self, values)
            class(Object) :: self
            real, dimension(:) :: values
        end subroutine create_array_abstract

end module Object_module
