program main
    use Object_module
    use List_module
    use Observer_module
    use endo_abstract_module
    use climate_module
    use Summary_module
    use mpi
    implicit none


    integer :: julianday_i
    integer :: i, var_i
    character*500 :: input_files(7000), outputfilename, input
    integer :: ifile, inumber_of_files_all, starting_year
    integer, dimension(:), allocatable :: inumber_of_files, rdispls
    integer ( kind = 4 ) id
    integer ( kind = 4 ) ierr
    integer ( kind = 4 ) p
    real ( kind = 8 ) wtime

    id=0
    p=1
    
    call MPI_Init ( ierr )

    call MPI_Comm_rank ( MPI_COMM_WORLD, id, ierr )

    call MPI_Comm_size ( MPI_COMM_WORLD, p, ierr )
    
    process_id=id
    
    print *, "process ", id, " out of ", p
    allocate(inumber_of_files(0:p-1), rdispls(0:p-1))
    
    !get starting year
    call get_command_argument(3, input)
    read (input, *) starting_year
    
    !get the list of input files to run for this specific mpi slave
    call get_input_files()
    
    !initiate the summary data object
    call summary_init(inumber_of_files(id))
    !if (id==0) then !prepare mpi master to collect data from slaves
      call summary_init_all(inumber_of_files_all)
    !end if
    print *, "process ", id, " will process ", inumber_of_files(id), " files of", inumber_of_files_all

    call init_random_seed()
    
    !run the model for each file 
    ifile = 1
    do while  (ifile<=inumber_of_files(id))
      print *, "id:", id, " running ",ifile," of ", inumber_of_files(id)
      current_file=ifile
      call summary_initialize_steps(ifile)

      !initiailize observer
      call initialize()
      !call init_clutch_factory()
      call init_endo_factory()

  !   initiailize climate
      call climate_initialize(input_files(ifile))
      call m_climate%go_to_date(1,1,starting_year) 

      !add a mouse to the observer
      i=1
      call m_observer%populate()

      !run the model on every time step
      do while (associated(current_climate))
          print *, id, i
          i = i + 1
          call m_observer%step()
          call m_climate%step()          
      end do

      !finalize the model (calculate statistics etc.)
      call observer_finalize()
      call climate_finalize()
      ifile = ifile + 1
    end do
    print *, "my id is ", id
    call summary_initialize_steps(ifile)
    close(20)
    
    !!!!gather data to master mpi
    rdispls(0)=0
    do i=1, p-1
      rdispls(i) = rdispls(i-1) + inumber_of_files(i-1)      
    end do

    print *, "my id is ", id
    !gather latitudes
    call mpi_gatherv(sum_lats,inumber_of_files(id),mpi_real,sum_lats_all,inumber_of_files,rdispls,mpi_real,0, mpi_comm_world,ierr)
    call check_mpi(ierr)    
    write(*,*) id, 'lats data gathered'
    
    !gather longitudes
    call mpi_gatherv(sum_lons,inumber_of_files(id),mpi_real,sum_lons_all,inumber_of_files,rdispls,mpi_real,0, mpi_comm_world,ierr)
    call check_mpi(ierr)
    write(*,*) id, 'lons data gathered'

    !gather model results
    do i=1, num_of_vars
      if (var_dims(i) == enum_dim_lat_lon) then !2d
        call mpi_gatherv(sums_files(i, 1,:),inumber_of_files(id),mpi_real,sums_files_all(i, 1,:),inumber_of_files,rdispls,mpi_real,0, mpi_comm_world,ierr)
      else !3d
        call mpi_gatherv(sums_files(i, :,:),inumber_of_files(id)*365,mpi_real,sums_files_all(i, :,:),inumber_of_files*365,rdispls*365,mpi_real,0, mpi_comm_world,ierr)
        call check_mpi(ierr)
      end if
    end do

    write(*,*) id, 'all data gathered'
    
    !write results to netcdf file
    if (id==0) then
      print *, "opening output file"
    !  call get_command_argument(3, outputfilename) 
    !  open(2, file = outputfilename,  ACCESS='STREAM', FORM='UNFORMATTED')
    !  write (2) inumber_of_files_all, sum_lats_all, sum_lons_all, sums_files
    !  close(2)
    !  print *, "closing output file"
      call prepare_netcdf_data(num_of_coordinates=inumber_of_files_all)
      call write_netcdf_file()
    end if    
    
    print *, "process ", id, " waiting "
    call MPI_BCAST(ifile,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call summary_clean()
    deallocate(inumber_of_files, rdispls)
    print *, "process ", id, " finishing "
    call MPI_Finalize ( ierr )

contains
    
    subroutine check_mpi(ierr)
      integer ierr, ierr_out, str_error_length
      character(len=500) :: str_error
      if (ierr /= MPI_SUCCESS) then
        call MPI_Error_string(ierr,str_error,str_error_length, ierr_out)
        print *, id, trim(str_error)
      end if
    end subroutine check_mpi
    
    !this function splits the input files across the different mpi tasks
    subroutine get_input_files()

        integer ifile, iEOF, i
        logical EOF
        character*500 input_file, inputfilelistname

        ifile = 1
        inumber_of_files = 0
        
        call get_command_argument(1, inputfilelistname)

        open(2,form='formatted',file=inputfilelistname)
        read(2,*,iostat=iEOF) input_file

        if (iEOF<0) then;
	       EOF=.true.;
        else;
	       EOF=.false.;
        end if

        do while (.not. EOF)
          do i=0, p-1
            if (mod(ifile,  p)==i) then
                inumber_of_files(i) = inumber_of_files(i) +1
            end if
          end do
          if (mod(ifile,  p)==id) then
            input_files(inumber_of_files(id)) = input_file
            print *, "process ", id, " will process ", trim(input_files(inumber_of_files(id)))                
          end if
          ifile = ifile + 1
          read(2,*,iostat=iEOF) input_file
	        if (iEOF<0) then;
	           EOF=.true.;
	        else;
	           EOF=.false.;
	        end if	        
        end do
        close(2)
        inumber_of_files_all = ifile-1
    end subroutine get_input_files

    SUBROUTINE init_random_seed()
            INTEGER :: i, n, clock
            INTEGER, DIMENSION(:), ALLOCATABLE :: seed

            CALL RANDOM_SEED(size = n)
            ALLOCATE(seed(n))

            CALL SYSTEM_CLOCK(COUNT=clock)

            seed = clock + 37 * (/ (i - 1, i = 1, n) /)
            CALL RANDOM_SEED(PUT = seed)

            DEALLOCATE(seed)
    END SUBROUTINE
end program main
