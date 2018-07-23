module Summary_module
  use parameters
  implicit none

  enum , bind (c)
        enumerator :: enum_mean_temperature = 1, &
                      enum_mean_winter_temperature, &
                      enum_mean_summer_temperature, &
                      enum_mean_winter_day_temperature, &
                      enum_mean_winter_night_temperature, &
                      enum_mean_summer_day_temperature, &
                      enum_mean_summer_night_temperature, &
                      enum_mean_winter_day_wind, &
                      enum_mean_winter_night_wind, &
                      enum_mean_summer_day_wind, &
                      enum_mean_summer_night_wind, &
                      enum_mean_winter_day_qair, &
                      enum_mean_winter_night_qair, &
                      enum_mean_summer_day_qair, &
                      enum_mean_summer_night_qair, &
                      enum_sd_temperature, &
                      enum_sd_winter_temperature, &
                      enum_sd_summer_temperature, &                      
                      enum_evp_diurnal_summer, &
                      enum_evp_nocturnal_summer, &
                      enum_met_diurnal_summer, &
                      enum_met_nocturnal_summer, &
                      enum_SMOIS_summer, &
                      enum_FVEG_summer, &
                      enum_evp_diurnal_winter, &
                      enum_evp_nocturnal_winter, &
                      enum_met_diurnal_winter, &
                      enum_met_nocturnal_winter, &
                      enum_SMOIS_winter, &
                      enum_FVEG_winter, &
                      enum_qair_summer, &
                      enum_qair_winter, &
                      enum_wind_summer, &
                      enum_wind_winter, &  
                      enum_activity_diurnal_summer, &
                      enum_activity_nocturnal_summer, &
                      enum_activity_diurnal_winter, &
                      enum_activity_nocturnal_winter, &  
                      enum_evp_diurnal, &
                      enum_evp_nocturnal, &
                      enum_evp_hourly, &
                      enum_met_diurnal, &
                      enum_met_nocturnal, &
                      enum_met_hourly, &
                      enum_FVEG, &
                      enum_SMOIS, &
                      enum_last
        
        enumerator :: enum_sum_last = 1
        
        enumerator :: enum_dim_lat_lon = 1, &
                      enum_dim_lat_lon_jul, &
                      enum_dim_lat_lon_month
                                                                  
  end enum

  integer, dimension(enum_last-1) :: var_dims = (/ &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon, &
                      enum_dim_lat_lon_jul, &
                      enum_dim_lat_lon_jul, &
                      enum_dim_lat_lon_month, &
                      enum_dim_lat_lon_jul, &
                      enum_dim_lat_lon_jul, &
                      enum_dim_lat_lon_month, &
                      enum_dim_lat_lon_jul, &
                      enum_dim_lat_lon_jul &
                      /)
                      

  real(KIND=4), dimension(:), allocatable :: sum_lats, sum_lons, sum_lats_all, sum_lons_all
  real(KIND=4), dimension(:,:), allocatable ::  sums_steps

  real(KIND=4), dimension(:,:,:), allocatable :: sums_files, sums_files_all
  
  real :: num_of_steps = 0.
  integer num_of_vars, num_of_sums_vars, num_of_positions, num_of_decades, num_of_oviposition_months
  integer :: sum_current_file = 0

  !for netcdf
  real(KIND=4), dimension(:,:), allocatable :: netcdf_lats, netcdf_lons
  integer num_lats,num_lons
  integer, parameter :: num_julian_days = 365, month_hour = 288
  integer, dimension(:), allocatable :: lats_index,lons_index
  real(KIND=4), dimension(:,:,:,:), allocatable :: netcdf_data

contains

  subroutine summary_init(number_of_files)
    integer number_of_files
    num_of_vars = enum_last-1
    num_of_sums_vars = enum_sum_last - 1
    
    
    if (num_of_vars>0) then
      allocate(sum_lats(number_of_files), sum_lons(number_of_files), sums_files(num_of_vars,num_julian_days,number_of_files))
    end if
    if (num_of_sums_vars>0) then
      allocate(sums_steps(num_julian_days, num_of_sums_vars))
      sums_steps = 0.
    end if
    
  end subroutine

  subroutine summary_init_all(number_of_files)
    integer number_of_files
    allocate(sums_files_all(num_of_vars,num_julian_days, number_of_files), sum_lats_all(number_of_files), sum_lons_all(number_of_files))
  end subroutine

  subroutine summary_clean()
    deallocate(sum_lats, sum_lons, sums_files)
    if (num_of_sums_vars>0) then
      deallocate(sums_steps)
    end if
  end subroutine

  subroutine summary_initialize_steps(ifile)
    integer ifile, i,p
   
    sum_current_file = ifile
  end subroutine summary_initialize_steps

  subroutine summary_step(tair, month)
    integer month
    real tair    

    num_of_steps = num_of_steps + 1
  end subroutine summary_step


  subroutine prepare_netcdf_data(num_of_coordinates)
    use netcdf
    
    integer i, j, p, d, o,x,y,ncount, num_of_coordinates
    integer ncid, varid
    integer, dimension(nf90_max_var_dims) :: dimIDs
    real, dimension(:), allocatable :: lats, lons

    !find number of lats and their place in the matrix
    allocate(lats(num_of_coordinates), lons(num_of_coordinates), lats_index(num_of_coordinates),lons_index(num_of_coordinates))
    
    ! get lat/lon matrixes
    call check(nf90_open("geo_em.d01.nc", nf90_nowrite, ncId))
    call check(nf90_inq_varid(ncId, "LANDMASK", varid))
    call check(nf90_inquire_variable(ncid, varid, dimids = dimIDs))
    call check(nf90_inquire_dimension(ncid, dimIDs(1), len = num_lons))
    call check(nf90_inquire_dimension(ncid, dimIDs(2), len = num_lats))
    print *, num_lons, num_lats
    
    allocate(netcdf_lats(num_lons, num_lats), netcdf_lons(num_lons, num_lats))
    
    call check(nf90_inq_varid(ncId, "XLONG_M", varid))
    call check(nf90_get_var(ncid, varid,netcdf_lons(:,:)))
    call check(nf90_inq_varid(ncId, "XLAT_M", varid))
    call check(nf90_get_var(ncid, varid,netcdf_lats(:,:)))
    call check(nf90_close(ncId))

    
    !create index array
    lats_index = 0
    lons_index = 0
    do i=1, num_of_coordinates
      do y=1, num_lats
        do x=1, num_lons
          if (abs(sum_lats_all(i)-netcdf_lats(x,y))<0.001 .and. abs(sum_lons_all(i)-netcdf_lons(x,y))<0.001) then            
            lats_index(i) = y
            lons_index(i) = x
          end if
        end do
      end do
      if (lats_index(i)==0) then
        print *, "could not find lat/lon for coordinate", i, sum_lats_all(i), sum_lons_all(i)
      end if
    end do

    !create matrix
    allocate(netcdf_data(num_of_vars, num_lons, num_lats, num_julian_days))
    netcdf_data=-100000.
    do j=1, num_of_vars
      do i=1, num_of_coordinates
        if (.not. lats_index(i)==0) then  
          if (var_dims(j)==enum_dim_lat_lon_jul) then
            do p=1, num_julian_days 
              netcdf_data(j, lons_index(i), lats_index(i), p) = sums_files_all(j,p,i)
            end do 
          else if (var_dims(j)==enum_dim_lat_lon_month) then
            do p=1, month_hour
              netcdf_data(j, lons_index(i), lats_index(i), p) = sums_files_all(j,p,i)
            end do 
          else 
            netcdf_data(j, lons_index(i), lats_index(i), 1) = sums_files_all(j,1,i)
          end if
        end if
       end do
    end do
   print *, "created matrix"
   deallocate(lats, lons)
end subroutine prepare_netcdf_data

Subroutine write_netcdf_file() 
     use netcdf
    implicit none

    character*500 outputfilename
    integer :: ncid, dimids3d(3)
    integer :: x_dimid, y_dimid, julianday_dimid, month_dimid
    integer :: lat_varid, lon_varid, julianday_varid, month_varid
    integer, dimension(:), allocatable :: data_varid
    integer, parameter :: nvars = enum_last-1

    ! Loop indexes, and error handling.
    integer :: i,j,k,t,ierr,m
    character(len=50), dimension(enum_last-1) :: names =      (/  &
                                                       "mean_air50", &
                                                       "mean_winter_air50", &
                                                       "mean_summer_air50", &
                                                       "mean_winter_day_air50", &
                                                       "mean_winter_night_air50", &
                                                       "mean_summer_day_air50", &
                                                       "mean_summer_night_air50", &
                                                       "mean_winter_day_wind", &
                                                       "mean_winter_night_wind", &
                                                       "mean_summer_day_wind", &
                                                       "mean_summer_night_wind", &
                                                       "mean_winter_day_qair", &
                                                       "mean_winter_night_qair", &
                                                       "mean_summer_day_qair", &
                                                       "mean_summer_night_qair", &
                                                       "sd_air50", &
                                                       "sd_winter_air50", &
                                                       "sd_summer_air50", &
                                                       "evp_diurnal_summer", &
                                                       "evp_nocturnal_summer", &
                                                       "met_diurnal_summer", &
                                                       "met_nocturnal_summer", &
                                                       "SMOIS_summer", &
                                                       "FVEG_summer", &
                                                       "evp_diurnal_winter", &
                                                       "evp_nocturnal_winter", &
                                                       "met_diurnal_winter", &
                                                       "met_nocturnal_winter", &
                                                       "SMOIS_winter", &
                                                       "FVEG_winter", &
                                                       "QAIR_summer", &
                                                       "QAIR_winter", &
                                                       "WIND_summer", &
                                                       "WIND_winter", &
                                                       "activity_diurnal_summer", &
                                                       "activity_nocturnal_summer", &
                                                       "activity_diurnal_winter", &
                                                       "activity_nocturnal_winter", &
                                                       "evp_diurnal", &
                                                       "evp_nocturnal", &
                                                       "evp_hourly", &
                                                       "met_diurnal", &
                                                       "met_nocturnal", &
                                                       "met_hourly", &
                                                       "FVEG", &
                                                       "SMOIS" &
                                                        /)
    character(len=50), dimension(nvars) :: units =      (/  &
                                                       "K", &
                                                       "K", &
                                                       "K", &
                                                       "K", &
                                                       "K", &
                                                       "K", &
                                                       "K", &
                                                       "m/sec", &
                                                       "m/sec", &
                                                       "m/sec", &
                                                       "m/sec", &
                                                       "kg/kg", &
                                                       "kg/kg", &
                                                       "kg/kg", &
                                                       "kg/kg", &
                                                       "K", &
                                                       "K", &
                                                       "K", &
                                                       "mlH2O/sec", &
                                                       "mlH2O/sec", &
                                                       "W/sec", &
                                                       "W/sec", &
                                                       "m3/m3", &
                                                       "dec %", &
                                                       "mlH2O/sec", &
                                                       "mlH2O/sec", &
                                                       "W/sec", &
                                                       "W/sec", &
                                                       "m3/m3", &
                                                       "dec %", &
                                                       "m3/m3", &
                                                       "m3/m3", &
                                                       "m/sec", &
                                                       "m/sec", &
                                                       "hours", &
                                                       "hours", &
                                                       "hours", &
                                                       "hours", &
                                                       "mlH2O/sec", &
                                                       "mlH2O/sec", &
                                                       "mlH2O/sec", &
                                                       "W/sec",&
                                                       "W/sec", &
                                                       "W/sec", &
                                                       "dec %", &
                                                       "m3/m3" &
                                                       /)

    character(len=200), dimension(nvars) :: long_names = (/ &
                                                       "average air temperature at mouse's height and 50% shade ", &
                                                       "average winter air temperature at mouse's height and 50% shade ", &
                                                       "average summer air temperature at mouse's height and 50% shade ", &
                                                       "average daytime winter air temperature at mouse's height and 50% shade ", &
                                                       "average nighttime winter air temperature at mouse's height and 50% shade ", &
                                                       "average daytime summer air temperature at mouse's height and 50% shade ", &
                                                       "average nighttime summer air temperature at mouse's height and 50% shade ", &
                                                       "average daytime winter wind speed at mouse's height", &
                                                       "average nighttime winter wind speed at mouse's height ", &
                                                       "average daytime summer wind speed at mouse's height", &
                                                       "average nighttime summer wind speed at mouse's height ", &
                                                       "average daytime winter specific humidity at 2m", &
                                                       "average nighttime winter specific humidity at 2m", &
                                                       "average daytime summer specific humidity at 2m", &
                                                       "average nighttime summer specific humidity at 2m", &
                                                       "standard deviation of air temperature at mouse's height and 50% shade ", &
                                                       "winter standard deviation of air temperature at mouse's height and 50% shade ", &
                                                       "summer standard deviation of air temperature at mouse's height and 50% shade ", &
                                                       "average evaporative water loss for a diurnal active mouse during summer", &
                                                       "average evaporative water loss for a nocturnal active mouse during summer", &
                                                       "average energy expenditure for a diurnal active mouse during summer", &
                                                       "average energy expenditure for a nocturnal active mouse during summer", &
                                                       "average soil moisture during summer 0-10 cm", &
                                                       "average green vegetation cover during summer", &
                                                       "average evaporative water loss for a diurnal active mouse during winter", &
                                                       "average evaporative water loss for a nocturnal active mouse during winter", &
                                                       "average energy expenditure for a diurnal active mouse during winter", &
                                                       "average energy expenditure for a nocturnal active mouse during winter", &
                                                       "average soil moisture during winter 0-10 cm", &
                                                       "average green vegetation cover during winter", &
                                                       "average specific humidity during summer", &
                                                       "average specific humidity during winter", &
                                                       "average wind speed at animal height during summer", &
                                                       "average wind speed at animal height during winter", &                                                       
                                                       "average number of activity hours for a diurnal active mouse during summer", &
                                                       "average number of activity hours for a nocturnal active mouse during summer", &                                                       
                                                       "average number of activity hours for a diurnal active mouse during winter", &
                                                       "average number of activity hours for a nocturnal active mouse during winter", &
                                                       "evaporative water loss for a diurnal active mouse", &
                                                       "evaporative water loss for a nocturnal active mouse", &
                                                       "hourly evaporative water loss for a month", &
                                                       "energy expenditure for a diurnal active mouse", &
                                                       "energy expenditure for a nocturnal active mouse", &
                                                       "hourly energy expenditure for a month", &
                                                       "Green vegetation cover", &
                                                       "soil moisture"     &
                                                       /)                                                  
    
    allocate(data_varid(num_of_vars))

    call get_command_argument(2, outputfilename)
    ! Create the netCDF file. The nf90_clobber parameter tells netCDF to
    ! overwrite this file, if it already exists.
    print *, "trying to write netcdf file"
    call check( nf90_create(outputfilename, NF90_CLOBBER,  ncid) )
    
    ! Define the dimensions. NetCDF will hand back an ID for each.
    call check( nf90_def_dim(ncid, "west_east", num_lons, x_dimid) )
    call check( nf90_def_dim(ncid, "south_north", num_lats, y_dimid) )
    call check( nf90_def_dim(ncid, "julian_day", num_julian_days, julianday_dimid) )
    call check( nf90_def_dim(ncid, "month", month_hour, month_dimid) )

    ! Define the dimension variables. They will hold the coordinates,times,atm levels and soil layers
    call check( nf90_def_var(ncid, "lat", NF90_DOUBLE,  (/ x_dimid, y_dimid /), lat_varid) )
    call check( nf90_def_var(ncid, "lon", NF90_DOUBLE, (/ x_dimid, y_dimid /), lon_varid) )
    call check( nf90_def_var(ncid, "julian_day",  NF90_INT, julianday_dimid, julianday_varid) )
    call check( nf90_def_var(ncid, "month_hour",  NF90_INT, month_dimid, month_varid) )

    ! Assign units attributes to coordinates,times,atm levels and soil layers vars data.
    call check( nf90_put_att(ncid, lat_varid, 'units', "degree_north" ) )
    call check( nf90_put_att(ncid, lon_varid, 'units', "degree_east") )
    call check( nf90_put_att(ncid, lat_varid, 'long_name', "LATITUDE, SOUTH IS NEGATIVE" ) )
    call check( nf90_put_att(ncid, lon_varid, 'long_name', "LONGITUDE, WEST IS NEGATIVE") )
    !call check(nf90_put_att(ncid, NF90_GLOBAL, "ADULT_WEIGHT", 45))

    ! The dimids array is used to pass the IDs of the dimensions of
    ! the variables. Note that in fortran arrays are stored in
    ! column-major format.
    dimids3d =  (/ x_dimid, y_dimid , julianday_dimid /)

    do i=1, num_of_vars
       if (var_dims(i)==enum_dim_lat_lon_jul) then
         call check( nf90_def_var(ncid, names(i), NF90_REAL, (/ x_dimid, y_dimid , julianday_dimid /), data_varid(i) ))
       else if (var_dims(i)==enum_dim_lat_lon_month) then
         call check( nf90_def_var(ncid, names(i), NF90_REAL, (/ x_dimid, y_dimid , month_dimid /), data_varid(i) ))
       else
         call check( nf90_def_var(ncid, names(i), NF90_REAL, (/ x_dimid, y_dimid /), data_varid(i) ))
       end if
    end do

    !Define the attributes for the variables
    do i=1, num_of_vars
      call check( nf90_put_att(ncid, data_varid(i), 'units', units(i)) )
    end do

    do i=1, num_of_vars
      call check( nf90_put_att(ncid, data_varid(i), 'long_name', long_names(i)) )
    end do
    
    do i=1, num_of_vars
      call check( nf90_put_att(ncid, data_varid(i), '_FillValue', -100000.))
    end do

    ! End define mode. This tells netCDF we are done defining metadata.
    call check( nf90_enddef(ncid) )

    ! Write the dimensions data
    call check( nf90_put_var(ncid, lat_varid, netcdf_lats ) )
    call check( nf90_put_var(ncid, lon_varid, netcdf_lons) )
    call check( nf90_put_var(ncid, julianday_varid, (/ (I, I = 1, num_julian_days) /) ))
    call check( nf90_put_var(ncid, month_varid, (/ (I, I = 1, month_hour) /) ))
    
    do i=1, num_of_vars
      print *, "writing", names(i), var_dims(i), units(i), long_names(i)
      if (var_dims(i)==enum_dim_lat_lon_jul) then
        !print *, netcdf_data(i,:,:,:)
        call check( nf90_put_var(ncid, data_varid(i), netcdf_data(i,:,:,1:num_julian_days)))
      else if (var_dims(i)==enum_dim_lat_lon_month) then
        call check( nf90_put_var(ncid, data_varid(i), netcdf_data(i,:,:,1:month_hour)))
      else
        call check( nf90_put_var(ncid, data_varid(i), netcdf_data(i,:,:,1)))
      end if
    end do

    call check( nf90_close(ncid) )
    print *, "finished creating the netcdf file"
    deallocate(lats_index,lons_index, netcdf_lats, netcdf_lons, netcdf_data, data_varid)

end Subroutine write_netcdf_file

subroutine check(status, str)
    use netcdf
    integer, intent ( in) :: status
    character(len=100), optional :: str
    if(status /= nf90_noerr) then
     if (present(str)) then
      print *, adjustr(nf90_strerror(status)), adjustr(str)
     else
      print *, adjustr(nf90_strerror(status)), "no str"
     end if
     stop 2
    end if
end subroutine check

end module Summary_module
