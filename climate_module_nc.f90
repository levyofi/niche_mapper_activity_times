module climate_module
    use parameters
    use Summary_module
    implicit none

    public :: climate_initialize
    public :: climate_finalize
    public :: m_climate
    public :: current_climate

    type, public :: climate
        real(KIND=4) :: lat, lon
        integer(KIND=4) :: num_of_rows, num_of_layers
        real(KIND=4), dimension(:,:), allocatable :: climate_data
        integer(KIND=4), dimension(:), allocatable :: layers
        real(KIND=4), dimension(:,:), allocatable :: avgSWDOWN, avgGLW, avgALBEDO, mint, maxt 
        integer :: time_step = 1
        character :: location_id*20
        real :: julian_day = 1
        real :: pctdif=0.1
        integer :: min_year
        logical :: is_validation = .false.
        real :: humidity = -1000. !no constant relative humidity (I added this for the validation file)
    contains
        procedure :: load_data => climate_load_data
        procedure :: go_to_date => climate_go_to_date
        procedure :: step => climate_step
        procedure :: get_wetair
    end type climate

    type (climate), pointer :: m_climate => null()
    real(KIND=4), pointer :: current_climate(:)
contains

    subroutine climate_initialize(inputfilename)
        character :: inputfilename*500
        if (.not. associated(m_climate)) then
          allocate(m_climate)
          call m_climate%load_data(inputfilename)
       end if
    end subroutine
    
    subroutine climate_finalize()
       if (associated(m_climate)) then
          ! ... clean components of singelton
          deallocate(m_climate%climate_data)
          deallocate(m_climate)
       end if
    end subroutine climate_finalize

    subroutine climate_load_data(self, inputfilename)
        use netcdf 
        
        class(climate), target :: self
        
        integer :: space_i, mypos
        integer(kind=4), dimension(:), allocatable :: hours,days,months,years
        character(len=20) :: input_param_str
        logical :: soil
        character (len=200) :: inputfilename, SWDOWN_nc_file, GLW_nc_file, TAH_nc_file, Tsurface_nc_file, Tair_nc_file,Tsoil_nc_file, &
                                PSFC_nc_file, RHOAIR_nc_file, EAIR_nc_file, EAH_nc_file, TV_nc_file, ALBEDO_nc_file, WIND_nc_file, &
                                FVEG_nc_file, SMOIS_nc_file, QAIR_nc_file
        character :: input_is_mean*10
        integer :: ishade, i, ilayer, imonth, varid, &
                  SWDOWN_ncid, GLW_ncid, TAH_ncid, Tsurface_ncid, Tair_ncid,Tsoil_ncid, ALBEDO_ncid, WIND_ncid, &
                  RHOAIR_ncid, EAIR_ncid, EAH_ncid,PSFC_ncid, TV_ncid, num_of_shade_levels, num_of_layers, &
                  FVEG_ncid, SMOIS_ncid, QAIR_ncid
        real :: tair, current_wind, current_qair, mean_ta, mean_ta_summer, mean_ta_winter, delta, sd_ta_summer, sd_ta_winter, isummer, iwinter_day, isummer_day, iwinter_night, isummer_night, iwinter, &
                mean_fveg_summer, mean_fveg_winter, mean_smois_summer, mean_smois_winter, mean_ta_night_winter, mean_ta_day_winter, &
                mean_ta_night_summer, mean_ta_day_summer, mean_qair_summer, mean_qair_winter, mean_wind_summer, mean_wind_winter, &
                mean_wind_night_winter, mean_wind_day_winter, mean_wind_night_summer, mean_wind_day_summer, &
                mean_qair_night_winter, mean_qair_day_winter, mean_qair_night_summer, mean_qair_day_summer                
        integer, dimension(nf90_max_var_dims) :: dimIDs

        !get input file data
        write(*,*) inputfilename

        space_i=index(inputfilename, '_')-1
        self%location_id=inputfilename(1:space_i)
        soil = .false.
        print *, "location_id=", self%location_id

        !get file names of netcdf files
        EAH_nc_file = "netcdf_files/EAH_"//trim(inputfilename)
        EAIR_nc_file = "netcdf_files/EAIR_"//trim(inputfilename)
        RHOAIR_nc_file = "netcdf_files/RHOAIR_"//trim(inputfilename)
        PSFC_nc_file = "netcdf_files/PSFC_"//trim(inputfilename)
        SWDOWN_nc_file = "netcdf_files/SWDOWN_"//trim(inputfilename)
        GLW_nc_file = "netcdf_files/GLW_"//trim(inputfilename)
        TAH_nc_file = "netcdf_files/TAH_"//trim(inputfilename)
        Tsurface_nc_file = "netcdf_files/Tsurface_"//trim(inputfilename)
        Tair_nc_file = "netcdf_files/Tair_"//trim(inputfilename)
        Tsoil_nc_file = "netcdf_files/Tsoil_"//trim(inputfilename)
        TV_nc_file = "netcdf_files/TV_"//trim(inputfilename)
        ALBEDO_nc_file = "netcdf_files/ALBEDO_"//trim(inputfilename)
        WIND_nc_file = "netcdf_files/WIND10_"//trim(inputfilename)
        FVEG_nc_file = "netcdf_files/FVEG_"//trim(inputfilename)
        SMOIS_nc_file = "netcdf_files/SMOIS_"//trim(inputfilename)
        QAIR_nc_file = "netcdf_files/QAIR_"//trim(inputfilename)
        
        print *, EAH_nc_file
        call check(nf90_open(EAH_nc_file, nf90_nowrite, EAH_ncid), EAH_nc_file)
        print *, EAIR_nc_file        
        call check(nf90_open(EAIR_nc_file, nf90_nowrite, EAIR_ncid), EAIR_nc_file )
        print *, RHOAIR_nc_file        
        call check(nf90_open(RHOAIR_nc_file, nf90_nowrite, RHOAIR_ncid), RHOAIR_nc_file)
        print *, PSFC_nc_file        
        call check(nf90_open(PSFC_nc_file, nf90_nowrite, PSFC_ncid), PSFC_nc_file)
        print *, SWDOWN_nc_file        
        call check(nf90_open(SWDOWN_nc_file, nf90_nowrite, SWDOWN_ncid), SWDOWN_nc_file)
        print *, GLW_nc_file        
        call check(nf90_open(GLW_nc_file, nf90_nowrite, GLW_ncid), GLW_nc_file)
        print *, TAH_nc_file        
        call check(nf90_open(TAH_nc_file, nf90_nowrite, TAH_ncid), TAH_nc_file)
        print *, Tsurface_nc_file        
        call check(nf90_open(Tsurface_nc_file, nf90_nowrite, Tsurface_ncid), Tsurface_nc_file)
        print *, Tair_nc_file        
        call check(nf90_open(Tair_nc_file, nf90_nowrite, Tair_ncid), Tair_nc_file)
        print *, Tsoil_nc_file        
        call check(nf90_open(Tsoil_nc_file, nf90_nowrite, Tsoil_ncid), Tsoil_nc_file)
        print *, TV_nc_file        
        call check(nf90_open(TV_nc_file, nf90_nowrite, TV_ncid), TV_nc_file)
        print *, ALBEDO_nc_file        
        call check(nf90_open(ALBEDO_nc_file, nf90_nowrite, ALBEDO_ncid), ALBEDO_nc_file)
        print *, WIND_nc_file        
        call check(nf90_open(WIND_nc_file, nf90_nowrite, WIND_ncid), WIND_nc_file)
        print *, FVEG_nc_file        
        call check(nf90_open(FVEG_nc_file, nf90_nowrite, FVEG_ncid), FVEG_nc_file)
        print *, SMOIS_nc_file        
        call check(nf90_open(SMOIS_nc_file, nf90_nowrite, SMOIS_ncid), SMOIS_nc_file)
        print *, QAIR_nc_file        
        call check(nf90_open(QAIR_nc_file, nf90_nowrite, QAIR_ncid), QAIR_nc_file)
        
        !get lat and lon
        call check(nf90_inq_varid(SWDOWN_ncid, "lon", varid),  SWDOWN_nc_file)
        call check(nf90_get_var(SWDOWN_ncid, varid, self%lon),  SWDOWN_nc_file)
        call check(nf90_inq_varid(SWDOWN_ncid, "lat", varid),  SWDOWN_nc_file)
        call check(nf90_get_var(SWDOWN_ncid, varid, self%lat),  SWDOWN_nc_file)
        
        !get the number of time points
        call check(nf90_inq_varid(SWDOWN_ncid, "time", varid),  SWDOWN_nc_file)
        call check(nf90_inquire_variable(SWDOWN_ncid, varid, dimids = dimIDs),  SWDOWN_nc_file)
        call check(nf90_inquire_dimension(SWDOWN_ncid, dimIDs(1), len = self%num_of_rows),  SWDOWN_nc_file)
        
        allocate(self%climate_data(self%num_of_rows,Ta100), hours(self%num_of_rows),days(self%num_of_rows),months(self%num_of_rows),years(self%num_of_rows))

        !get time variables        
        call check(nf90_get_var(SWDOWN_ncid, varid, hours))
        do i=1, self%num_of_rows
              years(i) = hours(i)/1000000
              months(i) = mod(hours(i),1000000)/10000
              days(i) = mod(hours(i),10000)/100
              hours(i) = mod(hours(i),100)
        end do
        self%climate_data(:,hour) = hours
        self%climate_data(:,day) = days
        self%climate_data(:,month) = months
        self%climate_data(:,year) = years
        self%min_year = years(1)                      
        
        !get julian days
        call check(nf90_inq_varid(SWDOWN_ncid, "jday", varid),  SWDOWN_nc_file)
        call check(nf90_get_var(SWDOWN_ncid, varid, self%climate_data(:,jday)),  SWDOWN_nc_file)
        
        !get the rest of the variables
        call check(nf90_inq_varid(ALBEDO_ncid, "ALBEDO", varid))
        call check(nf90_get_var(ALBEDO_ncid, varid, self%climate_data(:,ALBEDO)),  ALBEDO_nc_file)
        !print *, "reading SWDOWN"
        call check(nf90_inq_varid(SWDOWN_ncid, "SWDOWN", varid))
        call check(nf90_get_var(SWDOWN_ncid, varid, self%climate_data(:,SWDOWN)),  SWDOWN_nc_file)
        !print *, "reading GLW"
        call check(nf90_inq_varid(GLW_ncid, "GLW", varid))
        call check(nf90_get_var(GLW_ncid, varid, self%climate_data(:,GLW)),  GLW_nc_file)
        !print *, "reading TAH"
        call check(nf90_inq_varid(TAH_ncid, "TAH", varid))
        call check(nf90_get_var(TAH_ncid, varid, self%climate_data(:,TAH)),  TAH_nc_file)
        call check(nf90_inq_varid(EAH_ncid, "EAH", varid))
        call check(nf90_get_var(EAH_ncid, varid, self%climate_data(:,EAH)),  EAH_nc_file)
        call check(nf90_inq_varid(EAIR_ncid, "EAIR", varid))
        call check(nf90_get_var(EAIR_ncid, varid, self%climate_data(:,EAIR)), EAIR_nc_file)
        call check(nf90_inq_varid(RHOAIR_ncid, "RHOAIR", varid))
        call check(nf90_get_var(RHOAIR_ncid, varid, self%climate_data(:,RHOAIR)), RHOAIR_nc_file)
        call check(nf90_inq_varid(PSFC_ncid, "PSFC", varid))
        call check(nf90_get_var(PSFC_ncid, varid, self%climate_data(:,PSFC)), PSFC_nc_file)
        call check(nf90_inq_varid(TV_ncid, "TV", varid))
        call check(nf90_get_var(TV_ncid, varid, self%climate_data(:,TV)), TV_nc_file)
        call check(nf90_inq_varid(Tsurface_ncid, "Tsurface", varid)) 
        call check(nf90_get_var(Tsurface_ncid, varid, start = (/1,1/), count = (/self%num_of_rows, 1/), values = self%climate_data(:,Tsurface)), Tsurface_nc_file)
        call check(nf90_get_var(Tsurface_ncid, varid, start = (/1,5/), count = (/self%num_of_rows, 1/), values = self%climate_data(:,Tsurface100)), Tsurface_nc_file)
        call check(nf90_inq_varid(Tsoil_ncid, "Tsoil", varid))  
        call check(nf90_get_var(Tsoil_ncid, varid, self%climate_data(:,Tsoil_6cm), start= (/1,1,2 /), count = (/self%num_of_rows, 1, 1/)), Tsoil_nc_file)
        call check(nf90_get_var(Tsoil_ncid, varid, self%climate_data(:,Tsoil100_6cm), start= (/1,5,2 /), count = (/self%num_of_rows, 1, 1/)), Tsoil_nc_file)
        call check(nf90_get_var(Tsoil_ncid, varid, self%climate_data(:,Tsoil_30cm), start= (/1,1,10 /), count = (/self%num_of_rows, 1, 1/)), Tsoil_nc_file)
        call check(nf90_get_var(Tsoil_ncid, varid, self%climate_data(:,Tsoil100_30cm), start= (/1,5,10 /), count = (/self%num_of_rows, 1, 1/)), Tsoil_nc_file)
        call check(nf90_inq_varid(Tair_ncid, "Tair", varid))        
        call check(nf90_get_var(Tair_ncid, varid, start = (/1,1,1 /), count = (/self%num_of_rows, 1, 1/), values = self%climate_data(:,Ta)), Tair_nc_file)
        call check(nf90_get_var(Tair_ncid, varid, start = (/1,5,1 /), count = (/self%num_of_rows, 1, 1/), values = self%climate_data(:,Ta100)), Tair_nc_file) 
        call check(nf90_inq_varid(WIND_ncid, "WIND10", varid))        
        call check(nf90_get_var(WIND_ncid, varid, start = (/1,1,1 /), count = (/self%num_of_rows, 1, 1/), values = self%climate_data(:,wind3cm)), WIND_nc_file)       
        call check(nf90_inq_varid(FVEG_ncid, "FVEG", varid))
        call check(nf90_get_var(FVEG_ncid, varid, self%climate_data(:,FVEG)), FVEG_nc_file)
        call check(nf90_inq_varid(SMOIS_ncid, "SMOIS", varid))
        call check(nf90_get_var(SMOIS_ncid, varid, self%climate_data(:,SMOIS)), SMOIS_nc_file)
        call check(nf90_inq_varid(QAIR_ncid, "QAIR", varid))
        call check(nf90_get_var(QAIR_ncid, varid, self%climate_data(:,QAIR)), QAIR_nc_file)
        
        !close netcdf files
        call check(nf90_close(EAH_ncid))
        call check(nf90_close(EAIR_ncid))
        call check(nf90_close(RHOAIR_ncid))
        call check(nf90_close(PSFC_ncid))
        call check(nf90_close(SWDOWN_ncid))
        call check(nf90_close(GLW_ncid))
        call check(nf90_close(TAH_ncid))
        call check(nf90_close(Tsurface_ncid))
        call check(nf90_close(Tsoil_ncid))
        call check(nf90_close(Tair_ncid))
        call check(nf90_close(TV_ncid))
        call check(nf90_close(ALBEDO_ncid))
        call check(nf90_close(WIND_ncid))
        call check(nf90_close(FVEG_ncid))
        call check(nf90_close(SMOIS_ncid))
        call check(nf90_close(QAIR_ncid))                                
        
        write(*,*) "finished reading", inputfilename

        current_climate => self%climate_data(self%time_step,:)
        
                
        self%julian_day = 1
        mean_ta = sum(self%climate_data(:,Ta))/real(self%num_of_rows)
        
        !calculate mean and sd for winter and summer
        !use the approach suggested in http://mathcentral.uregina.ca/QQ/database/QQ.09.02/carlos1.html to avoid looping twice through the data 
        ! also see "The Art of Computer Programming, Volume 2: Seminumerical Algorithms", section 4.2.2.
        ! and B.P. Welford, Technometrics, 4,(1962), 419-420.
        ! code adapted from http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance (function online_variance(data))
        isummer = 0
        iwinter = 0
        iwinter_day = 0
        isummer_day = 0
        iwinter_night = 0
        isummer_night = 0
        mean_ta_summer = 0
        mean_ta_winter = 0
        sd_ta_summer = 0
        sd_ta_winter = 0
        mean_fveg_summer = 0
        mean_fveg_winter = 0
        mean_smois_summer = 0
        mean_smois_winter = 0
        mean_ta_night_winter = 0
        mean_ta_day_winter = 0
        mean_ta_night_summer = 0
        mean_ta_day_summer = 0
        mean_wind_night_winter = 0
        mean_wind_day_winter = 0
        mean_wind_night_summer = 0
        mean_wind_day_summer = 0
        mean_qair_night_winter = 0
        mean_qair_day_winter = 0
        mean_qair_night_summer = 0
        mean_qair_day_summer = 0
        mean_qair_summer = 0
        mean_qair_winter = 0
        mean_wind_summer = 0
        mean_wind_winter = 0
        
        call get_command_argument(4, input_param_str)
        if (input_param_str=="rest") then
          soil = .true.
        end if
        do i=1, self%num_of_rows
          imonth = self%climate_data(i,month)
          if (soil) then
            tair = self%climate_data(i,Tsoil100_30cm)
          else
            tair = self%climate_data(i,Ta)
          end if
          current_wind = self%climate_data(i,wind3cm)
          current_qair = self%climate_data(i,QAIR)
          if (imonth==12 .or. imonth==1 .or. imonth==2) then          
            iwinter = iwinter + 1.
            delta = tair - mean_ta_winter               
            mean_ta_winter = mean_ta_winter + delta / iwinter
            sd_ta_winter = sd_ta_winter + delta * (tair - mean_ta_winter)            
            delta = self%climate_data(i,SMOIS) - mean_smois_winter
            mean_smois_winter = mean_smois_winter + delta/iwinter
            delta = self%climate_data(i,FVEG) - mean_fveg_winter
            mean_fveg_winter = mean_fveg_winter + delta/iwinter
            delta = self%climate_data(i,QAIR) - mean_qair_winter
            mean_qair_winter = mean_qair_winter + delta/iwinter 
            delta = self%climate_data(i,wind3cm) - mean_wind_winter
            mean_wind_winter = mean_wind_winter + delta/iwinter  
            if ( self%climate_data(i,SWDOWN)>0.5 ) then 
              iwinter_day = iwinter_day + 1
              delta = tair - mean_ta_day_winter
              mean_ta_day_winter = mean_ta_day_winter + delta/iwinter_day
              delta = current_wind - mean_wind_day_winter
              mean_wind_day_winter = mean_wind_day_winter + delta/iwinter_day
              delta = current_qair - mean_qair_day_winter
              mean_qair_day_winter = mean_qair_day_winter + delta/iwinter_day
            else
              iwinter_night = iwinter_night + 1
              delta = tair - mean_ta_night_winter
              mean_ta_night_winter = mean_ta_night_winter + delta/iwinter_night
              delta = current_wind - mean_wind_night_winter
              mean_wind_night_winter = mean_wind_night_winter + delta/iwinter_night
              delta = current_qair - mean_qair_night_winter
              mean_qair_night_winter = mean_qair_night_winter + delta/iwinter_night
            end if
          end if
          
          if (imonth>=6 .and. imonth<=8) then
            isummer = isummer + 1.
            delta = tair - mean_ta_summer               
            mean_ta_summer = mean_ta_summer + delta / isummer
            sd_ta_summer = sd_ta_summer + delta * (tair - mean_ta_summer)
            delta = self%climate_data(i,SMOIS) - mean_smois_summer
            mean_smois_summer = mean_smois_summer + delta/isummer
            delta = self%climate_data(i,FVEG) - mean_fveg_summer
            mean_fveg_summer = mean_fveg_summer + delta/isummer            
            delta = self%climate_data(i,QAIR) - mean_qair_summer
            mean_qair_summer = mean_qair_summer + delta/isummer 
            delta = self%climate_data(i,wind3cm) - mean_wind_summer
            mean_wind_summer = mean_wind_summer + delta/isummer  
            if ( self%climate_data(i,SWDOWN)>0.5 ) then 
              isummer_day = isummer_day + 1
              delta = tair - mean_ta_day_summer
              mean_ta_day_summer = mean_ta_day_summer + delta/isummer_day
              delta = current_wind - mean_wind_day_summer
              mean_wind_day_summer = mean_wind_day_summer + delta/isummer_day
              delta = current_qair - mean_qair_day_summer
              mean_qair_day_summer = mean_qair_day_summer + delta/isummer_day
            else
              isummer_night = isummer_night + 1
              delta = tair - mean_ta_night_summer
              mean_ta_night_summer = mean_ta_night_summer + delta/isummer_night
              delta = current_wind - mean_wind_night_summer
              mean_wind_night_summer = mean_wind_night_summer + delta/isummer_night
              delta = current_qair - mean_qair_night_summer
              mean_qair_night_summer = mean_qair_night_summer + delta/isummer_night
            end if
          end if
        end do
        
        sd_ta_winter = sqrt(sd_ta_winter/real((iwinter-1)))
        sd_ta_summer = sqrt(sd_ta_summer/real((isummer-1)))
        
        sums_files(enum_mean_temperature,1, sum_current_file) = mean_ta
        sums_files(enum_sd_temperature, 1, sum_current_file) = SQRT (SUM((self%climate_data(:,Ta)-mean_ta)**2.) / self%num_of_rows)  
        sums_files(enum_mean_winter_temperature,  1,sum_current_file) = mean_ta_winter 
        sums_files(enum_mean_summer_temperature,  1,sum_current_file) = mean_ta_summer 
        sums_files(enum_mean_winter_day_temperature,  1,sum_current_file) = mean_ta_day_winter
        sums_files(enum_mean_summer_day_temperature,  1,sum_current_file) = mean_ta_day_summer
        sums_files(enum_mean_winter_night_temperature,  1,sum_current_file) = mean_ta_night_winter
        sums_files(enum_mean_summer_night_temperature,  1,sum_current_file) = mean_ta_night_summer
        
        sums_files(enum_mean_winter_day_wind,  1,sum_current_file) = mean_wind_day_winter
        sums_files(enum_mean_summer_day_wind,  1,sum_current_file) = mean_wind_day_summer
        sums_files(enum_mean_winter_night_wind,  1,sum_current_file) = mean_wind_night_winter
        sums_files(enum_mean_summer_night_wind,  1,sum_current_file) = mean_wind_night_summer
        
        sums_files(enum_mean_winter_day_qair,  1,sum_current_file) = mean_qair_day_winter
        sums_files(enum_mean_summer_day_qair,  1,sum_current_file) = mean_qair_day_summer
        sums_files(enum_mean_winter_night_qair,  1,sum_current_file) = mean_qair_night_winter
        sums_files(enum_mean_summer_night_qair,  1,sum_current_file) = mean_qair_night_summer
        
        sums_files(enum_sd_winter_temperature,  1,sum_current_file) = sd_ta_winter 
        sums_files(enum_sd_summer_temperature,  1,sum_current_file) = sd_ta_summer 
        sums_files(enum_SMOIS_summer,  1,sum_current_file) = mean_smois_summer
        sums_files(enum_SMOIS_winter, 1,sum_current_file) = mean_smois_winter
        sums_files(enum_FVEG_summer, 1,sum_current_file) = mean_fveg_summer
        sums_files(enum_FVEG_winter, 1,sum_current_file) = mean_fveg_winter
        sums_files(enum_qair_summer, 1,sum_current_file) = mean_qair_summer
        sums_files(enum_qair_winter, 1,sum_current_file) = mean_qair_winter
        sums_files(enum_wind_summer, 1,sum_current_file) = mean_wind_summer
        sums_files(enum_wind_winter, 1,sum_current_file) = mean_wind_winter
        
        
        !print *, sums_files(enum_mean_winter_temperature, 1  ,sum_current_file) , mean_ta_summer, mean_smois_summer, mean_smois_winter, mean_fveg_summer, mean_fveg_winter
    end subroutine

    subroutine climate_go_to_date(self, m_day, m_month, m_year)

        class(climate), target :: self
        integer :: m_day, m_month, m_year, current_time_step

        current_time_step = self%time_step
        print *, current_climate(day), current_climate(month), current_climate(year)
        do while ((current_climate(day)/=m_day) .or. (current_climate(month)/=m_month) .or. (current_climate(year)<m_year))
            call self%step()
        end do
        if (.not. associated(current_climate)) then
            print *, m_day, m_month, m_year, ":date not found"
            self%time_step = current_time_step
            current_climate => self%climate_data(self%time_step,:)
        end if
        self%min_year = current_climate(year)
    end subroutine climate_go_to_date

    subroutine climate_step(self)
        class(climate), target :: self

        self%time_step = self%time_step + 1
        if (self%time_step > self%num_of_rows) then
            nullify(current_climate)
        else
            current_climate => self%climate_data(self%time_step,:)
            self%julian_day = current_climate(jday)
            if (mod(int(current_climate(year)), 4)==0 .and. current_climate(month)>2) then
              self%julian_day = current_climate(jday)-1
            end if
        end if        
                        
    end subroutine climate_step

    REAL(kind=8) function get_wetair(self, TK, E) !derived from WETAIR.for
      class(climate), target :: self
      REAL(kind=8) :: E ! VAPOR PRESSURE (Pa), in the microclimate input (EAIR - bare ground, EAH - canopy)
      REAL(kind=8) :: TK ! temperature (K), in the microclimate input (Tair)
            
      get_wetair = E * 0.018016 / (0.998 * 8.31434 * TK)
    end function get_wetair
    
    
    !calculate saturation vapor pressure 
    REAL(kind=8) function vapprs(db)
      double precision :: loge
	    double precision :: estar 
	    REAL(kind=8) :: db
	    real :: t !db is in C, t is in K
      
      !temperature limits on db
	    if (db .gt. 100.) then
	      db = 100.
	    else
	      if (db .lt. -40.) then
	        db = -40.
        endif
      endif
      t=db+273.16   
      if (t .gt. 273.16) then
        loge=-7.90298*(373.16/t-1.)+5.02808* alog10(373.16/t)-1.3816e-07 *(10.**(11.344*(1.-t/373.16))-1.)+8.1328e-03*(10.**(-3.49149* (373.16/t-1.))-1.)+alog10(1013.246)
        estar=(10.**loge)*100  
      else   
        loge=-9.09718*(273.16/t-1.)-3.56654*alog10(273.16/t)+.876793*(1.-t/273.16)+alog10(6.1071)   
        estar=(10.**loge)*100. 
      end if   
      vapprs=estar   
    end function vapprs
      
    Subroutine dryair(db,bp,patmos,densty,visdyn,viskin,difvpr, thcond,htovpr,tcoeff,ggroup)                                          
    !***********************************************************************   
    !     copyright 2004  warren p. porter.  all rights reserved.
    !    
    ! subroutine dryair calculates several properties of dry air and related    
    ! characteristics shown as output variables below.  the program is based    
    ! on data from list, r. j. 1971. smithsonian meteorological tables.        
    ! smithsonian institution press. washington, dc.                            
    !                                                                           
    ! the user must supply values for the input variables (db, bp, and alt).    
    ! if alt is known (-1 000 < alt < 20 000) but not bp then set bp=0.         
    !                                                                           
    !*************************** input variables ***************************   
    !                                                                           
    ! db=dry bulb temperature (degree celsius)                                  
    ! bp=barometric pressure (pascal) [bp at one standard atmosphere is         
    !    101 325 pascals (100 pascals=1 millibar)].                             
    ! alt=altitude (metre) (1 metre=3.280 839 9 feet)                           
    !                                                                           
    !*************************** output variables **************************   
    !                                                                           
    ! patmos=standard atmospheric pressure (pascal)                             
    ! densty=density (kilogram per cubic metre)                                 
    ! visdyn=dynamic viscosity (kilogram per metre second)                      
    ! viskin=kinematic viscosity (square metre per second)                      
    ! difvpr=diffusivity of water vapor in air (square metre per second)       
    ! thcond=thermal conductivity (watt per metre kelvin)                       
    ! htovpr=latent heat of vaporization of water (joule per kilogram)          
    ! tcoeff=temperature coefficient of volume expansion (1 per kelvin)         
    ! ggroup=group of variables in grashof number (1 per cubic metre kelvin)    
    !                                                                           
    !***********************************************************************    
	  implicit none

	  real(kind=8) :: bp,c,db,densty,difvpr,ggroup,htovpr
	  real(kind=8) :: patmos,pstd,tcoeff,thcond,tnot,tstd,visdyn,viskin,visnot
                                                                            
    tstd=273.15                                                         
    pstd=101325.                                                          
    patmos=current_climate(PSFC)
    if(bp .le. 0.)bp=patmos                                              
    densty=current_climate(RHOAIR)                                          
    visnot=1.8325e-5                                                      
    tnot=296.16                                                           
    c=120.                                                                
    visdyn=(visnot*((tnot+c)/(db+tstd+c)))*(((db+tstd)/tnot)**1.5)        
    viskin=visdyn/densty                                                  
    difvpr=2.26e-5*(((db+tstd)/tstd)**1.81)*(1.e5/bp)                     
    thcond=.02425+(7.038e-5*db)                                          
    htovpr=2.5012e6-2.3787e3*db                                          
    tcoeff=1./(db+tstd)                                                   
    ggroup=0.0980616*tcoeff/(viskin*viskin)                                
  end subroutine dryair
  
end module climate_module
