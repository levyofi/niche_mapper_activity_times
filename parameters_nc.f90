module parameters
    implicit none

    enum , bind (c)
        enumerator ::       hour=1,&  
                            day, &
                            month, &
                            year, &
                            jday, &
                            QAIR, &
                            ALBEDO, &
                            EAH, &
                            EAIR, &
                            FVEG, &
                            SMOIS, &
                            RHOAIR, &
                            PSFC, &
                            TAH, &
                            SWDOWN, &
                            GLW, &
                            wind3cm, &
                            TV, &
                            Tsurface, &
                            Tsurface100, &
                            Tsoil_6cm, &
                            Tsoil100_6cm, &
                            Tsoil_30cm, &
                            Tsoil100_30cm, &
                            Ta, &
                            Ta100
    end enum
                                                         
    integer, parameter :: num_of_shade_levels = 2, num_of_layers_to_read = 1
    integer :: process_id, current_file
    
    public :: get_array_index
    

  contains
  
    integer function get_array_index(arr, target_value)
        integer :: arr(:)
        integer :: i, target_value, num_elements

        num_elements = size(arr)
        do i = 1, num_elements
            if (arr(i) .eq. target_value) then
                get_array_index = i
                return
            endif
        end do
        get_array_index = -1
    end function get_array_index

    
end module parameters
