module endo_abstract_module
    use List_module
    use Object_module
    use Observer_module
    use Factory_module
    use List_module
    use climate_module
    use Summary_module
    use parameters
    use parameters_endo

    implicit none
    public
    
    type, public, extends(object) :: endo_abstract
        type (record) :: l_observer

        !GEOMETRIC PROPERTIES
        real(kind=8) :: min_q = 0.008 ! [J sec-1 g-1 Mb]
        
        real(kind=8) :: MASS  = 0.0483 !max weight (kg)
        real(kind=8) :: RINSUL = 0.010 !fat insulation thick (m)
        real(kind=8) :: AK1 = 0.4, AK1MAX = 2.8
        integer :: NG = 3 !Geometric approx (1=cyl,2=spher,3=ellips, 4=lizard )
        integer :: ANIMAL = 0 !no. of  appendages 
        integer :: hours_of_model = 0
        real(kind=8) :: PTCOND = 0.1 ! % ventral area  contacting substr (decimal %)
        real(kind=8) :: ANDENS = 932.9    ! animal density (kg/m3)
        real(kind=8) :: tc = 36. !regulated Tb (C)
        real(kind=8) :: TSKREG = 3. !differenc between core tb and skin temperature
        logical :: USRALOM  = .false. !use supplied allometry
        real(kind=8) :: FASKY = 0.5 ! % area exposed to the sky 
        real(kind=8) :: FAGRD = 0.5 !% area exposed to the ground
        real(kind=8) :: TSMIN = 3.0 !minimal skin temperature (C)
        real(kind=8) :: TSKIN !skin temperature 
        real(kind=8) :: TLUNG !lung temperature
        real(kind=8) :: SKINW ! area (%) of evaporative water loss
        real(kind=8) :: convsk ! total area (%) available for evaporative water loss
        real(kind=8) :: ENB !energy balance (should be very close to zero when the guessing routine succeed) 
        real(kind=8) :: A, B, C, D !A, B, an C are the animal’s meter height, width and length, respectively. D is convection characteristic dimension (m)
        real(kind=8) :: AIRVOL !volume of air needed to enter the lungs (l/s)
        real(kind=8) :: fur_depth !the depth of fur (m)
        real(kind=8) :: ta !air temperature near the animal
        logical :: active !true if the animal is active
        real(kind=8) :: shade !the percentage of shade above the animal (dec%)
        real(kind=8) :: QSEVAP !evaporative heat loss (W)
        real(kind=8) :: QRESP  !resperatory heat loss (W)
        real(kind=8) :: areaskin !area of the skin (m)
        real(kind=8) :: QSOLAR, QDORSL, QVENTR !for solar radiation (W)
        real(kind=8) :: SKYRAD, GRDRAD !solar and IR radiation from the sky and ground (W)
        real(kind=8) :: water ! evaoprative water loss (kg/s)
        real(kind=8) :: HD, HC !mass transfer coefficient (m/s) and heat transfer coefficient (W/m2/C)
        real(kind=8) :: atot, CONVAR !see below
        !FUR properties (arrays of 1:LEGS, 2:HEAD&NECK, 3:TORSO)
        real(kind=8), dimension(3) :: DHAIRD = (/500., 500., 500./) !Hair diameter dorsal (um)  Hair length   Fur depth   Hair dens. fur        fur       fur  
        real(kind=8), dimension(3) :: DHAIRV = (/240., 240., 240./) !Hair diameter ventral (um)
        real(kind=8), dimension(3) :: LHAIRD = (/10., 10., 10./) !Hair length dorsal (mm)
        real(kind=8), dimension(3) :: LHAIRV = (/4., 4., 4./) !Hair length ventral (mm)
        real(kind=8), dimension(3) :: ZFURD = (/4., 4., 4./) !Fur depth dorsal (mm)
        real(kind=8), dimension(3) :: ZFURV = (/1., 1., 1./) !Fur depth ventral (mm)
        real(kind=8), dimension(3) :: RHOD = (/40., 40., 40./) !Hair dens. dorsal (1/cm2)
        real(kind=8), dimension(3) :: RHOV = (/90., 90., 90./) !Hair dens. ventral (1/cm2)
        real(kind=8), dimension(3) :: REFLD = (/0.4, 0.4, 0.4/) !reflectance of dorsal fur (dec%)
        real(kind=8), dimension(3) :: REFLV = (/0.35, 0.35, 0.35/) !reflectance of ventral fur (dec%)
        real(kind=8), dimension(3) :: ABSAND, ABSANV !absorbanse of fur
        real(kind=8), dimension(3) :: DHARA, LHARA, RHOARA, ZFURAR ! will hold values for AVERAGE(1), FRONT(2), BACK(3) or DORSAL(2), VENTRAL(3) OF THE BODY PART
        real(kind=8), dimension(3,3) :: DHAR, LHAR, RHOAR, ZZFUR, REFLFR ! the first index is for AVERAGE(1), FRONT(2), BACK(3) or DORSAL(2), VENTRAL(3) OF THE BODY PART, the second is for 1=LEG,2=HEAD&NECK,3=TORSO
        
        real(kind=8) :: met, evp
        real(kind=8), dimension(3) :: KEFARA ! the effective thermal conductivity of fibers and air for conduction (W/m/C) 
        real(kind=8), dimension(3) :: betara ! Infrared extinction coefficient of fur/feathers (1/m)
        real(kind=8), dimension(3) :: b1ara, E4B1 ! Optical thickness of the fur, and the exponential integral of the optical thickness
        real(kind=8) :: keff ! torso effective thermal conductivity of fibers and air for conduction (W/m/C) 
        real(kind=8) :: KRAD ! radiant conductivity (W/m/C)
        real(kind=8) :: kair ! thermal conductivity of air (W/m/C)
        real(kind=8) :: vol, TFA ! volume of animal (m3), and fur-air interface temperature 
        real(kind=8) :: emis = 1. !emissivity of the skin
        real(kind=8) :: skin_radius, fur_radius 
        real(kind=8) :: c1 !helper variable for fur calculations
        real(kind=8) :: O2GAS = 20.95, CO2GAS = 0.03, N2GAS = 79.02 !percentages of O2, CO2 and N2 in the air
        real(kind=8) :: GEVAP ! water loss from respiration (G/S) 
        real(kind=8) :: GWCUT ! water loss from skin (G/S)
        real(kind=8) :: TAVBSH, TLOWER ! temperature of canopy cover, and temperature of the ground (K)
        real(kind=8) :: tair !air temperature near animal (C)
        real(kind=8) :: e, esat ! vapor pressure near animal
        
        !summary variables
        real(kind=4), dimension(0:23) :: evp_cur_day, met_cur_day, activity_hours_cur_day !predictions for current day
        real(kind=4), dimension(288) ::  count_monthly_hourly, evp_monthly_hourly, met_monthly_hourly, activity_hours_monthly_hourly
        real(kind=4), dimension(365) :: count_of_days, evp_diurnal, evp_nocturnal, met_diurnal, met_nocturnal, &
                                        activity_hours_diurnal, activity_hours_nocturnal, mean_fveg, mean_smois


    contains
        procedure, pass(self) :: step_init => step_endo_init_abs 
        procedure, pass(self) :: create => create_endo_abs
        procedure, pass(self) :: create_array => create_array_endo_abs
        procedure :: calculate_met_evp 
        procedure :: thermoregulation
        procedure :: daily_init => daily_init_abs
        procedure :: set_fur_properties
        procedure :: IRPROP
        procedure :: solar
        procedure :: calculate_SEVAP
        procedure :: resp
        procedure :: conv
        procedure :: calculate_rad_through_fur
        procedure :: allom
        procedure :: calculate_tskin
        procedure :: zbrac
        procedure :: zbrent
        procedure :: fun
        procedure :: step_save
        procedure :: step_save_daily
        
    end type endo_abstract

    type, public, extends(factory):: endo_factory_abstract
        real(kind=8) :: ref_min_q = 0.008 ! reference minimal energy expenditure
        real(kind=8) :: min_q = 0.008 ! minimal energy expenditure
        real(kind=8) :: ref_MASS  = 0.0483 ! reference body mass (kg)
        real(kind=8) :: MASS = 0.0483 !actual weight (kg)
        real(kind=8), dimension(3) :: ref_RHOD = (/40., 40., 40./) !reference hair dens. dorsal (1/cm2)
        real(kind=8), dimension(3) :: ref_RHOV = (/90., 90., 90./) !reference hair dens. ventral (1/cm2)
        real(kind=8), dimension(3) :: ref_REFLD = (/0.4, 0.4, 0.4/) !reference reflectance of dorsal fur (dec%)
        real(kind=8), dimension(3) :: ref_REFLV = (/0.1, 0.1, 0.1/) !reference reflectance of ventral fur (dec%)
        !these values can change if running a sensitivity analysis
        real(kind=8), dimension(3) :: RHOD = (/40., 40., 40./) !actual hair dens. dorsal (1/cm2)
        real(kind=8), dimension(3) :: RHOV = (/90., 90., 90./) !actual hair dens. ventral (1/cm2)
        real(kind=8), dimension(3) :: REFLD = (/0.4, 0.4, 0.4/) !actual reflectance of dorsal fur (dec%)
        real(kind=8), dimension(3) :: REFLV = (/0.1, 0.1, 0.1/) !actual reflectance of ventral fur (dec%)
    contains
        procedure, nopass :: create => create_endo_factory_abs
        procedure, nopass :: step  => step_endo_factory_abs
        procedure, pass(self) :: clear => clear_data_endo_factory_abs
        procedure, pass(self) :: print_statistics => print_statistics_endo_factory_abs
        procedure, nopass :: init => init_endo_factory
        
    end type endo_factory_abstract

    type (endo_factory_abstract), target :: m_mouse_factory
contains

    subroutine init_endo_factory()  
      character(len=20) :: input_param_str, input_param_num
      real :: input_num
      
      !use input arguments for sensitivity analysis      
      call get_command_argument(5, input_param_str)
      call get_command_argument(6, input_param_num)
      read(input_param_num, *)  input_num
      
      if (input_param_str=="mass") then
        print *, "running with mass", input_num
        m_mouse_factory%mass = m_mouse_factory%ref_mass + m_mouse_factory%ref_mass*input_num/100.
      end if
      if (input_param_str=="min_q") then
        print *, "running with min_q", input_num
        m_mouse_factory%min_q = m_mouse_factory%ref_min_q + m_mouse_factory%ref_min_q*input_num/100.
      end if
      if (input_param_str=="fur_refl") then
        print *, "running with fur reflectivity", input_num
        m_mouse_factory%REFLD = m_mouse_factory%ref_REFLD + m_mouse_factory%ref_REFLD*input_num/100.
        m_mouse_factory%REFLV = m_mouse_factory%ref_REFLV + m_mouse_factory%ref_REFLV*input_num/100.
      end if
      
      call get_command_argument(4, input_param_str)
      if (input_param_str=="rest") then
        print *, "running with resting phase"
        m_mouse_factory%min_q = m_mouse_factory%min_q*2./3.
      end if
      
      m_observer%m_mouse_factory => m_mouse_factory
    end subroutine

    subroutine create_endo_factory_abs(m_object)
        class(object), pointer :: m_object         
        allocate(endo_abstract :: m_object)
    end subroutine

    subroutine create_endo_abs(self)
        class(endo_abstract) :: self
        character(len=30) :: use_thermoregulation
         
        self%MASS = m_mouse_factory%MASS
        self%RHOD = m_mouse_factory%RHOD
        self%RHOV = m_mouse_factory%RHOV
        self%REFLD = m_mouse_factory%REFLD
        self%REFLV = m_mouse_factory%REFLV
        self%min_q = m_mouse_factory%min_q
        
        call self%set_fur_properties()
        call self%allom()
        
        self%TSKIN = self%TC - 2.0
	      self%TLUNG = (self%TC + self%TSKIN)/2.
	      	      
        call self%step_init()
        
        call m_observer%l_individuals%add(self, self%l_observer)
        
        self%count_of_days = 0.
        self%evp_diurnal = 0.
        self%evp_nocturnal = 0.
        self%met_diurnal = 0.
        self%met_nocturnal = 0.
        self%mean_fveg = 0.
        self%mean_smois = 0.
    end subroutine

    subroutine create_array_endo_abs(self, values)
        class(endo_abstract) :: self
        real(kind=4), dimension(:) :: values
        
        self%shade = values(1)
        self%active = NINT(values(2))
        
        call create_endo_abs(self)
    end subroutine
        
    subroutine calculate_tskin(self, GN)
      class(endo_abstract) :: self
      real(kind=8), intent(in) :: GN
      real(kind=8) :: ASQ, BSQ, CSQ
      real(kind=8) :: RATIO1, RATIO2, RATIO3
      real(kind=8) :: AR1, AR2, R3, K23
      real(kind=8) :: T1
      
      ASQ = self%A**2
      BSQ = self%B**2
      CSQ = self%C**2 
	    RATIO1 = (ASQ*BSQ*CSQ)/(ASQ*BSQ+ASQ*CSQ+BSQ*CSQ) 
      RATIO2 =  GN/(2.*self%AK1) 
	    RATIO3 =  GN/(4.*self%AK1)

  	  !GETTING AN AVERAGE RADIUS FOR THE ELLIPSOID...
	    AR1 =(self%A+self%B)/2.
	    AR2 = AR1 + self%RINSUL
	    R3 = AR2 + self%fur_depth
	    K23 = self%KEFF + self%KRAD
	    IF(AR2.GT.AR1) THEN  
  	    !FAT LAYER UNDER THE SKIN 
  	    !T1 = TEMP. AT FLESH-FAT INTERFACE                    
        T1 = self%TC - RATIO2*RATIO1
    		self%TSKIN = T1 - (GN*self%VOL*(AR2-AR1))/(4.*PI*AR1*AR2*AK2) 
  	    !TFA = FUR/FEATHER-AIR INTERFACE TEMPERATURE
	      self%TFA = self%TSKIN - (GN*self%VOL*(R3-AR2))/(4.*PI*AR2*R3*K23)
	     ELSE
  		!NO FAT LAYER
	      self%TSKIN = self%TC - RATIO2*RATIO1
	    ENDIF
	    self%TLUNG = (self%TC + self%TSKIN)/2.
      !LIMITING LUNG TEMPERATURE EXTREMES               
      IF (self%TLUNG .GT. self%TC) THEN
        self%TLUNG = self%TC
       ENDIF 
      IF (self%TLUNG .LT. 0.0) THEN
        self%TLUNG = 0.1
      ENDIF

    end subroutine calculate_tskin

    subroutine step_endo_init_abs(self)
        class(endo_abstract) :: self

        self%hours_of_model = self%hours_of_model + 1
        
        if (current_climate(hour)==0) then          
          call daily_init_abs(self)
        end if        
         
        call self%calculate_met_evp()
        call self%step_save()
        
    end subroutine step_endo_init_abs
    
    subroutine step_save(self)
        class(endo_abstract) :: self
        integer :: ihour, ihour_month
        real :: delta, wevp
        ihour = current_climate(hour)
        print *, self%evp
        self%evp_cur_day(ihour) = self%evp
        self%met_cur_day(ihour) = self%met 
        
        if (self%SKINW > 0.51) then
          self%activity_hours_cur_day(ihour) = 0. 
        else
          self%activity_hours_cur_day(ihour) = 1.
        end if
                
        !summarize by hour and month
        ihour_month = 24*(current_climate(month)-1)+ihour + 1      
        !add one to the count variable
        self%count_monthly_hourly(ihour_month) = self%count_monthly_hourly(ihour_month) + 1
        !calculate new averages
        print *, self%evp
        delta = self%evp - self%evp_monthly_hourly(ihour_month)
        self%evp_monthly_hourly(ihour_month) = self%evp_monthly_hourly(ihour_month) + delta/self%count_monthly_hourly(ihour_month)
        
        delta = self%met - self%met_monthly_hourly(ihour_month)
        self%met_monthly_hourly(ihour_month) = self%met_monthly_hourly(ihour_month) + delta/self%count_monthly_hourly(ihour_month)
        
        delta = self%activity_hours_cur_day(ihour) - self%activity_hours_monthly_hourly(ihour_month)
        self%activity_hours_monthly_hourly(ihour_month) = self%activity_hours_monthly_hourly(ihour_month) + delta/self%count_monthly_hourly(ihour_month)
                
    end subroutine step_save    
         
    subroutine step_save_daily(self)
        class(endo_abstract) :: self
        integer :: ihour
        real :: met_mean_day, met_mean_night, met_mean_crepuscular, met_sum_day, met_sum_night, count_day, count_night, &
                evp_mean_day, evp_mean_night, evp_mean_crepuscular, evp_sum_day, evp_sum_night, &
                activity_hours_mean_day, activity_hours_mean_night, activity_hours_mean_crepuscular, activity_hours_sum_day, activity_hours_sum_night
        real :: delta
        
        met_sum_day = 0.
        evp_sum_day = 0.
        activity_hours_sum_day = 0.
        met_sum_night = 0.
        evp_sum_night = 0.
        activity_hours_sum_night = 0.
        count_day = 0.
        count_night=0.
        
        !calculate 24h means of day. night and crepuscular costs
        do ihour=0, 23
          if (m_climate%climate_data(m_climate%time_step - 23 + ihour, SWDOWN)>0.5) then !this hour is of daytime
            met_sum_day = met_sum_day + self%met_cur_day(ihour)
            evp_sum_day = evp_sum_day + self%evp_cur_day(ihour)
            activity_hours_sum_day = activity_hours_sum_day + self%activity_hours_cur_day(ihour)
            count_day = count_day + 1
          else
            met_sum_night = met_sum_night + self%met_cur_day(ihour)
            evp_sum_night = evp_sum_night + self%evp_cur_day(ihour)
            activity_hours_sum_night = activity_hours_sum_night + self%activity_hours_cur_day(ihour)
            count_night = count_night + 1
          end if
        end do
        
        met_mean_day = met_sum_day/count_day
        met_mean_night = met_sum_night/count_night
        evp_mean_day = evp_sum_day/count_day
        evp_mean_night = evp_sum_night/count_night
        activity_hours_mean_day = activity_hours_sum_day/count_day
        activity_hours_mean_night = activity_hours_sum_night/count_night
        
        !add one to the number of time that this julian_day was analysed 
        self%count_of_days(m_climate%julian_day) = self%count_of_days(m_climate%julian_day) + 1
        
        !calculate the new average for each variable1
        delta = evp_mean_day - self%evp_diurnal(m_climate%julian_day)
        self%evp_diurnal(m_climate%julian_day) = self%evp_diurnal(m_climate%julian_day) + delta/self%count_of_days(m_climate%julian_day)

        delta = met_mean_day - self%met_diurnal(m_climate%julian_day)
        self%met_diurnal(m_climate%julian_day) = self%met_diurnal(m_climate%julian_day) + delta/self%count_of_days(m_climate%julian_day)
        
        delta = activity_hours_sum_day - self%activity_hours_diurnal(m_climate%julian_day)
        self%activity_hours_diurnal(m_climate%julian_day) = self%activity_hours_diurnal(m_climate%julian_day) + delta/self%count_of_days(m_climate%julian_day)
        
        delta = evp_mean_night - self%evp_nocturnal(m_climate%julian_day)
        self%evp_nocturnal(m_climate%julian_day) = self%evp_nocturnal(m_climate%julian_day) + delta/self%count_of_days(m_climate%julian_day)
        
        delta = met_mean_night - self%met_nocturnal(m_climate%julian_day)
        self%met_nocturnal(m_climate%julian_day) = self%met_nocturnal(m_climate%julian_day) + delta/self%count_of_days(m_climate%julian_day)
        
        delta = activity_hours_sum_night - self%activity_hours_nocturnal(m_climate%julian_day)
        self%activity_hours_nocturnal(m_climate%julian_day) = self%activity_hours_nocturnal(m_climate%julian_day) + delta/self%count_of_days(m_climate%julian_day)

        delta = current_climate(FVEG) - self%mean_fveg(m_climate%julian_day)
        self%mean_fveg(m_climate%julian_day) = self%mean_fveg(m_climate%julian_day) + delta/self%count_of_days(m_climate%julian_day)
        
        delta = current_climate(SMOIS) - self%mean_smois(m_climate%julian_day)
        self%mean_smois(m_climate%julian_day) = self%mean_smois(m_climate%julian_day) + delta/self%count_of_days(m_climate%julian_day)

    end subroutine step_save_daily
    
    subroutine daily_init_abs(self)
        class(endo_abstract) :: self
        
        call self%step_save_daily()
    end subroutine daily_init_abs

    subroutine step_endo_factory_abs(m_list)
        class(list) :: m_list
        type(record), pointer :: cur_record, next_record
        class(object), pointer :: cur_mouse
        
        !go through the list and run this function
        cur_record => m_list%head
        do while (associated(cur_record))
            next_record => cur_record%next 
            cur_mouse => cur_record%data
            select type(cur_mouse)
            type is (endo_abstract)
                call cur_mouse%step_init()
            end select
            cur_record => next_record
        end do
    end subroutine step_endo_factory_abs

    subroutine calculate_met_evp(self)
       class(endo_abstract) :: self
        
       real(kind=8) :: QMETAB, QSEVAP, SKINW, GWCUT, AIRVOL, MOLCO2
       real(kind=8) :: SKYIR, VEGIR, SKYRAD, GRDRAD, TOL, DELTA, qgen
       real(kind=8) :: X, X1, X2, TESTSOLN, HEATEST
       integer :: numtry
       logical :: success, full_success 
       
       QMETAB = 0.0
	     QSEVAP = 0.0
	     self%SKINW = 0.5
	     self%ak1 = 0.4
	     GWCUT = 0.0
	     AIRVOL  = 0.0
	     MOLCO2  = 0.0
	     GWCUT = 0.0
	     
	     self%ta = current_climate(Ta100)*self%shade + current_climate(Ta)*(1.-self%shade) - 273.
	     if (.not. self%active) then !assume in a burrow
          self%ta = current_climate(Tsoil100_30cm) - 273.
          CALL self%IRPROP()
          self%QSOLAR = 0.
          SKYIR = 0.  
          VEGIR = 0.
          self%SKYRAD = SKYIR + VEGIR  
          self%GRDRAD = self%areaskin*self%FAGRD*sigma*(self%ta + 273.)**4. 
          self%TLOWER = (self%ta + 273.)
          self%TAVBSH = (self%ta + 273.)
	     else 
          self%esat = vapprs(self%ta)
          if (m_climate%humidity>-0.1) then !climate data has constant relative humidity
            self%e = self%esat*m_climate%humidity
          else 
            self%e = current_climate(EAH)*self%shade + current_climate(EAIR)*(1.-self%shade)
          end if
          CALL self%IRPROP()

          !get radiation fluxes that do not depend on Tskin
          CALL self%SOLAR()  
          self%QSOLAR = (self%QDORSL  + self%QVENTR) !add this to the solar subroutine
          SKYIR = self%areaskin*self%FASKY*current_climate(GLW)*(1.-self%shade) 
          VEGIR = self%shade*self%areaskin*self%FASKY*sigma*(current_climate(TV)**4.)
          self%SKYRAD = SKYIR + VEGIR + self%QDORSL 
          self%GRDRAD = (1.-self%shade)*self%FAGRD*sigma*(current_climate(tsurface))**4. + self%shade*self%fagrd*sigma*current_climate(tsurface100)**4. + self%QVENTR*(1.0-self%shade)
          self%TLOWER = current_climate(tsurface)*(1.-self%shade) + current_climate(tsurface100)*self%shade 
          self%TAVBSH = current_climate(TAH)
        end if
        IF(self%MASS.GT.20)THEN 
              TOL = 0.01*self%MASS
        ENDIF
        IF(self%MASS.LT.20)THEN
          TOL = 0.01
        ENDIF
        
        !GUESS FOR METABOLIC RATE USING A STANDARD REGRESSION
        DELTA = 5.
        X=(70.*self%MASS**0.75)*(4.185/(24.*3.6))
        QGEN = X
        
        IF (X .EQ. 0.0000) THEN
          X1 = 0.1
         ELSE
          X1 = X
        ENDIF
        X2 = X1 + DELTA
        
        success = .false.
        full_success = .false.
        NUMTRY = 0
        do while ((.not. full_success) .and. numtry<5000)
          NUMTRY = NUMTRY + 1
          !FIND BRACKETING VALUES FOR GUESSING *************************************************
          CALL self%ZBRAC(X1,X2,SUCCESS)
          !DO THE GUESSING AND GET A SOLUTION **************************************************
          X = self%ZBRENT(X1,X2,TOL)

          QGEN = X
          TESTSOLN = ABS(self%ENB)
          HEATEST = self%TC - self%TSKIN
          self%evp = (self%GEVAP + self%GWCUT)*1000./self%mass 
          IF ((QGEN.LT.self%MIN_Q*self%MASS*1000.) .or. (TESTSOLN.GT.TOL)) THEN
	          CALL self%thermoregulation()
          ELSE IF(self%TA.LT.3)THEN
              !CHECK FOR COLD STRESS
              IF(self%TSKIN.LT.self%TSMIN)THEN
                !INCREASE FLESH THERMAL CONDUCTIVITY UP TO A MAXIMUM TO WARM SKIN
                IF(self%AK1.LT. 2.799999)THEN
                  self%AK1 = self%AK1 + 0.1
                ENDIF	            
              ENDIF
              if (SUCCESS) then
                full_success = .TRUE.
              end if
          else
            full_success = .TRUE.
          ENDIF 
       END DO !OF WHILE LOOP 

      if (full_success) then
        self%met = qgen/self%mass
        self%evp = (self%GEVAP + self%GWCUT)*1000./self%mass	
        print *, self%evp
      end if
                      
    end subroutine calculate_met_evp
    
    subroutine irprop(self)
      class(endo_abstract) :: self 
      real(kind=8) :: RHOEFF, aair, THTEST, LUNIT, HAIRSP, RAIR, AHAIR, KX, KY, RHOCM2, W, RHAIR
      integer :: L
      
      self%KAIR =.02425+(7.038E-5*self%TA)
      !CALCULATE OVERALL, DORSAL AND VENTRAL KEFF,BETA,B1 FOR ANIMAL 
      !DO LOOP CALCULATES (1) OVERALL, (2) DORSAL, (3) VENTRAL PROPERTIES
      DO L=1,3
      !INDEX,L,IS THE AVERAGE(1),FRONT(2), BACK(3) OR DORSAL(2), VENTRAL(3) OF THE BODY PART
          RHOEFF=self%RHOARA(L)*(self%LHARA(L)/self%ZFURAR(L))   
          W=1.0          
          LUNIT=1./RHOEFF**0.5   
          AAIR=(W/2.)*(LUNIT-self%DHARA(L))    
          RAIR=LUNIT/(self%KAIR*AAIR)
          AHAIR=RHOEFF*((self%DHARA(L)/2.)**2.*PI)     
          KX=AHAIR*KHAIR+((1.-AHAIR)*self%KAIR)                  
          RAIR=2./((RHOEFF**0.5)*self%KAIR*(LUNIT-self%DHARA(L))*W)
          RHAIR=(self%DHARA(L)*self%KAIR+(LUNIT-self%DHARA(L))*KHAIR)/(W*self%DHARA(L)*KHAIR*self%KAIR)       
          KY=(2./RAIR)+(1./RHAIR)         
          !EQUATION 3-28 KOWALSKI P. 82
          self%KEFARA(L)=(KY+KX)/2.   
          !CHECK TO ENSURE KAIR<KEFF<KHAIR   
          IF (self%KEFARA(L) .GT. KHAIR) THEN 
          !  KEFF TOO HIGH 
            self%KEFARA(L)=KHAIR
          ELSE      
            IF (self%KEFARA(L) .LT. self%KAIR) THEN
              self%KEFARA(L)=self%KAIR 
            ENDIF
          ENDIF     
          !END OF CHECK FOR VALUE OF KEFF
          self%BETARA(L)=(0.67/PI)*RHOEFF*self%DHARA(L)   
          !OPTICAL THICKNESS = B1
          self%B1ARA(L)=self%BETARA(L)*self%ZFURAR(L)  
          self%E4B1(L)=E4(self%B1ARA(L)) !TODO write function E4
      end do
      self%keff = self%KEFARA(torso)
    end subroutine irprop
    
    subroutine calculate_SEVAP(self) 
      class(endo_abstract) :: self 
      real(kind=8) :: esat !saturated vapor pressure
      real(kind=8) :: TK !air near animal (K)
      real(kind=8) :: surfvd !vapor density near skin (kg/m3)
      real(kind=8) :: airvd !vapor density in air (kg/m3)
      real(kind=8) :: SKINRA ! area of wet skin (dec%)
      real(kind=8) :: EFFSUR ! area of wet skin (m2)
      real(kind=8) :: wcut, weyes, pcteye
      real(kind=8) :: HTOVPR !
      !real(kind=8) :: QSEVAP !LATENT HEAT OF VAPORIZATION (J sec-1?)
      !calculate skin surface saturation vapor density
      !assume that humidity near the skin is 100%
      
      tk = self%tskin + 273.
      surfvd = vapprs(self%tskin) * 0.018016 / (0.998 * 8.31434 * tk)
       
      !calculate vapor density in air
      tk = self%ta + 273.
      if (self%active) then
        airvd = m_climate%get_wetair(tk, self%e)
      else
        airvd = m_climate%get_wetair(tk, self%esat*0.99)
      end if
      
      !CONVERTING FOM % TO A RATIO
	    SKINRA = 0.01*self%SKINW
      !SURFACE AREA THAT IS WET
      EFFSUR = self%CONVSK*SKINRA
      
      !AMOUNT OF WATER EVAPORATED FROM THE SKIN
      WCUT = EFFSUR * self%HD *(SURFVD - AIRVD)
      WEYES = 0.0
      self%WATER = WEYES + WCUT
      
      !FROM DRYAIR: LATENT HEAT OF VAPORIZATION 
      HTOVPR = 2.5012E+06 - 2.3787E+03 * self%TA  
      self%QSEVAP = self%WATER * HTOVPR   
      SELF%GWCUT = wcut*1000.
    end subroutine calculate_SEVAP
    
    subroutine conv (self) 
    !this subroutine computes the convective heat transfer from the surface of objects
    !copyright 2005 warren p. porter. all rights reserved.
    
    !all units si  (m,kg,s,c,k,j,pa)   
    !calculating sum of free and forced convection
    !srfcarea = surface area (m2)
      class(endo_abstract) :: self
         
      real(kind=8) :: srfcarea, densty, bp, patmos, visdyn, viskin, difvpr, thcond, htovpr, tcoeff, ggroup
      real(kind=8) :: pr, sc, beta, deltat, gr, ra, vel, anu, test, xtra, sh
      real(kind=8) :: db, re, nuforced
      
      srfcarea = self%CONVAR
      densty = current_climate(RHOAIR)
      bp=current_climate(PSFC)
      
      db=self%ta !in C
	    if(db .lt. -75.)then
	      db = -75.
	    endif
           
      !computing fluid properties
      call dryair(db,bp,patmos,densty,visdyn,viskin,difvpr, thcond,htovpr,tcoeff,ggroup) 

      !computing prandtl number      
      pr=cp*visdyn/thcond           

      !computing schmidt number          
      sc=visdyn/(densty*difvpr)

      !computing free convection variables for the grashof number
      beta=1./(self%ta+273.15)   
      deltat=self%tskin-self%ta

      !stability check
	    if(deltat .eq. 0.0000000e+00)then
	      deltat = deltat + 0.00001
	    endif  
      gr=((densty**2.)*beta*g*(self%d**3.)*deltat)/(visdyn**2.) 

      !correcting if negative deltat 
      gr = abs(gr) 

      !rayleight number 
	    ra=gr*pr 

      !calculate wind velocity near animal 
      vel = max(0.1, current_climate(wind3cm)*(1.-self%shade))

      test = (gr**.25)*(pr**.333)
      if (test .gt. 200.) then
        xtra = ((test - 200.)/test)*100.
        !this criteria rarely exceeded even for large animals.  when it 
        !is, it is less that 10% usually.  it also decreases as convergence
        !on a solution happens.        
        if (xtra .gt. 150.) then
          write(*,*)'(gr**.25)*(pr**.33)',xtra,'% too large for correl.'
        endif     
       endif 
	    
      
      !computing reynolds number from air properties from dryair
	    re = densty*vel*self%d/visdyn

      !computing nusselt number 
      anu=0.37*re**0.6
	    nuforced = anu

      !calculating the heat transfer coefficient, hc  (nu=hc*d/kair)
      self%hc=(nuforced*thcond)/self%d
	
      !calculating the sherwood number from the colburn analogy
      !(bird, stewart & lightfoot, 1960. transport phenomena. wiley.
      !note:  this is the same as hd = (hc*(cp*rho))*(pr/sc)**(2/3), where cp is for dry air and rho is for moist air.       
      sh = nuforced * (sc/pr)**.333 
      !calculating the mass transfer coefficient from the sherwood number; forced only      
      !sh = hd*d/difvpr
      self%hd=sh*difvpr/self%d   

  end subroutine conv  
 
  subroutine allom(self) 
        
      !subroutine to compute radii, diameter, & area = f (mass) 
      !this subroutine computes silhouette areas of a dinosaur
      !using an ellipsoid approximation.  assuming the animal is twice 
      !as long as it is wide.

      !4 areas are computed here: 
      !atot = total area at the fur/feather interface with the air (use
      ! for ir, solar calculations).
      !convar = total area at the fur/feather/skin interface with the air
      ! that is not in contact with the substrate (use for convective
      ! heat loss calculations).
      !convsk = total skin area available for evaporative water loss:
      ! i.e. the area not occupied on the skin by hair/feather shafts
      ! (use for evaporation from the skin).
      !areaskin = total skin area including hair/feather shafts (use to
      ! compute convsk and use in sub. sevap for water loss using aeff,
      ! effective area of skin that is wet).

      !copyright 2001-2003  warren p. porter.  all rights reserved.
	    implicit none
	    class(endo_abstract) :: self
      
	    real(kind=8) :: r, e, r1, a2, r2, dhair, rho, hairar, av
     
     !computing volume (m3) from kg mass        
     !mass = andens*vol = andens*(4./3.)*pi*r**3
     !density of body = 0.9329*10**3 kg/m^3   
	    self%vol = self%mass/self%andens
	    self%b = (((3./8.)*self%vol)/pi)**0.333 !animal height 
	    r = (3.*self%mass/(4.*pi*self%andens))**0.333
    
     !convection characteristic dimension (m) john mitchell (1976)
      self%d = 2.*r        
               
     !ellipsoid      
     !vol = 4./3.* pi*a*b*!
     !assuming a prolate spheroid of a=2b, b=c,then a = b+c; 
     
      self%c = self%b !animal width
      self%a = 2.*self%b ! animal length
      !ECCENTRICITY
      E = ((self%a**2 - self%C**2.)**0.5 )/self%A 
      self%areaskin = 2.*pi*self%b**2. + 2.*pi*(self%a*self%b/e)*asin(e) 
      r1 = self%b
      r2 = r1 + self%fur_depth
      a2 = self%a + self%fur_depth 
      !area at the fur tips (for dense fur only)              
      self%atot  = 2.*pi*r2**2 + 2.*pi*(a2*r2/e)*asin(e) 

      ! calculating skin area for evaporation
    	! total area on the body occupied by hair is 
	    ! (area/hair)*(hairs/m2)*m2
	    dhair = self%dhara(overall)
	    rho = self%rhoara(overall)
	    hairar = (pi * (dhair/2.)**2.) * (rho * self%areaskin)
	    ! skin area is total area - hair area
	    self%convsk = self%areaskin - hairar  
	
      !computing convective area at fur/feather interface
	    av = self%atot*self%ptcond 
	    self%convar = self%atot - av 
	    
      self%skin_radius = R1 
      self%fur_radius = self%skin_radius + self%fur_depth 

    end subroutine allom
      
    subroutine solar(self)
    !	this subroutine computes solar incident on an animal or object
    !	input data now come in with the call argument, output data leave in common 'alom2'
    !	input data the dorsal and ventral absorptivities, silhouette area and total area respectively
    !	warren porter version 8/22/05
	    implicit none
      class(endo_abstract) :: self
	    
	    real(kind=8) :: qsdir, qssky, qsrsb
	
      !computing solar energy absorbed (w): 
      !direct beam  
      qsdir=self%absand(torso)*self%atot*self%FASKY*(1.00-m_climate%pctdif)*current_climate(swdown)*(1.-self%shade) !self$absand and seld%absanv are dorsal and ventral animal emissivity todo: add to the 

      !diffuse components (sky and substrate)  
      qssky=self%absand(torso)*self%atot*self%FASKY*m_climate%pctdif*current_climate(swdown)*(1.-self%shade)
      qsrsb=self%absanv(torso)*self%atot*self%FAGRD*(1.0-current_climate(albedo))*current_climate(swdown)*(1.-self%shade)   
      
      !total energy absorbed on top & bottom parts of animal 
      self%qdorsl = qsdir + qssky
      self%qventr = qsrsb 
      
  end subroutine solar
  
      real FUNCTION FUN(self, X)  
      !ENERGY BALANCE CALCULATION FOR A CYLINDER, SPHERE, OR ELLIPSOID ALLOMETRY FOR AN ENDOTHERM WITH FAT & FUR
      !COPYRIGHT 2004 W.P. PORTER, ALL RIGHTS RESERVED.
      class(endo_abstract) :: self
      
      real(kind=8) :: G, X, QGEN, GEN, QGENET, GN,  QRAD, TFUR_AIR, TKSKIN, TKAIR, TLOCUP, &
              TAVSKY, QRADSK, TLOWER, TKGRD, QRADGR, QCONDD, PSISKY, PSISKY_shade, PSIGRD, QCONDV, QCOND, KFUR, QFUR, &
              TAVBSH, QRADBU, KRADSKY_shade, C1SKY_shade
	    real(kind=8) :: C1SKY, C1GRD, KRADSKY, KRADGRD

      !GUESSING FOR HEAT GENERATION THAT BALANCES THE 
      !ENERGY BALANCE EQUATION.      
      !TOTAL HEAT GENERATION=(HEAT GEN./VOLUME) * VOLUME (KG*M3/KG)
      !TOTAL HEAT GENERATION: 
	    IF(X.LT.0.0)THEN
	      X = 0.001
	    ENDIF
      QGEN = X 
      GEN = QGEN 
      !HEAT GENERATION/UNIT VOLUME
      G=X/self%VOL 

	    !ESTIMATE OF NEW LUNG TEMPERATURE TO GET THINGS STARTED
	    IF(self%TLUNG.LE. 0.000)THEN
	      self%TLUNG = (self%TC + self%TSKIN)/2.
	    ENDIF
	      
      !DETERMINE RESPIRATORY WATER LOSS AS A FUNCTION OF OXYGEN
      !CONSUMPTION. NO NEGATIVE METABOLIC RATES ALLOWED 
      !CALL FOR RESPIRATORY WATER & ENERGY LOSS           
      IF (QGEN .GT. 0.000) THEN
        CALL self%RESP(QGEN) !TODO: add this function          
      ELSE
        !NEGATIVE METABOLIC RATE. NO PHYSIOLOGICAL MEANING - DEAD.         
        self%QRESP = 0.00000
        QGEN = 0.01
      ENDIF       
      !NET INTERNAL HEAT GENERATION        
	    !TORSO: RESPIRATORY HEAT LOSS
  	  QGENET = QGEN - self%QRESP
      !NET INTERNAL HEAT GENERATION/UNIT VOLUME. USE FOR ESTIMATING SKIN TEMP.      
      GN = QGENET/self%VOL

      call self%calculate_tskin(GN)
  
      !LIMITING SKIN TEMPERATURE EXTREMES               
      IF (self%TSKIN .LT. 0.0) THEN
        self%TSKIN = self%TC - 0.1
	      self%TLUNG = (self%TC + self%TSKIN)/2.
      ENDIF 
    
      !INITIALIZING THE HEAT TRANSFER COEFFICIENT, self%hc
      IF(self%hc < 0.0000001) then 
        self%hc=30.
        QRAD=.0002
        TFUR_AIR =self%TA+1.
        self%C1=-1.0E-05   
      end if

      TFUR_AIR  = self%TA ! initiate fur tip temperature 
  		CALL self%CONV() 

      !CONTINUE WITH FURRY ANIMAL ENERGY BALANCE CALCULATIONS          
      TKSKIN=self%TSKIN+273.15  
      TKAIR=self%TA+273.15 
      TFUR_AIR =self%TSKIN-self%C1*self%TSKIN 

      !DETERMINE CUTANEOUS WATER LOSS.
      IF (self%SKINW .GT. 0.) THEN
        CALL self%calculate_SEVAP()
      ENDIF
       
      !SKY PORTION OF RADIANT HEAT EXCHANGE - ADDING SOLAR FLUX AS AN EQUIVALENT BLACK BODY RADIATION
  		TLOCUP = ((self%SKYRAD/(self%AREASKIN*self%FASKY*self%emis*sigma))**0.25) - 273.15 
  		TAVSKY=TLOCUP+273.15
  		call self%calculate_rad_through_fur(QRADSK, TAVSKY, dorsal, tkskin, tkair, KRADSKY, C1SKY, PSISKY, (1.-self%shade)*self%FASKY, 0.)
  	  
  	  TAVBSH= self%TAVBSH
      call self%calculate_rad_through_fur(QRADBU, TAVBSH, dorsal, tkskin, tkair, KRADSKY_shade, C1SKY_shade, PSISKY_shade, self%shade*self%FASKY, 0.)
	        
      !GROUND PORTION OF RADIANT HEAT EXCHANGE - VENTRAL PART  OF ANIMAL.
      TKGRD = self%TLOWER 
      call self%calculate_rad_through_fur(QRADGR, TKGRD, ventral, tkskin, tkair, KRADGRD, C1GRD, PSIGRD, self%FAGRD, 0.)

     	self%KRAD = (KRADGRD + KRADSKY)/2.
      self%C1 = ( C1SKY + C1GRD) / 2.
	    !NET RADIANT EXCHANGE BETWEEN ANIMAL AND ENVIRONMENT (W)	   
      QRAD = QRADSK + QRADGR + QRADBU !+ QRADBU + QRADSH
        
      !DORSAL CONDUCTION
      QCONDD=PSISKY*(self%AREASKIN/2.)*self%KEFARA(dorsal)*(C1SKY*TKSKIN/self%ZFURAR(dorsal)) + PSISKY_shade*(self%AREASKIN/2.)*self%KEFARA(dorsal)*(C1SKY_shade*TKSKIN/self%ZFURAR(dorsal))
      !VENTRAL CONDUCTION      
      QCONDV=PSIGRD*(self%AREASKIN/2.)*self%KEFARA(ventral)*(C1GRD *TKSKIN/self%ZFURAR(ventral)) 
      !TOTAL CONDUCTION
      QCOND = QCONDD + QCONDV 
      
      !energy balance at the fur	  
      QFUR = QCOND + QRAD  + self%QSEVAP 

      !WHOLE ANIMAL ENERGY BALANCE AT THE SKIN 
      self%ENB = QFUR - QGEN + self%QRESP 
      FUN=self%ENB 
  END function fun   
  
  SUBROUTINE RESP(self, gen)        
      class(endo_abstract) :: self
      real(kind=8) :: gen !energy expenditure
      real(kind=8), parameter :: WB = 0., DP = 999., STDPRS = 101325., RPCTO2 = 0.2095, RPCTN2 = 0.7902, RPCTCO2 = 0.0003, RHSAT = 100., RELXIT = 100. 
      real(kind=8) :: PO2, PCTO2, PCTN2, PCTCO2, O2STP, REFPO2, VO2CON, O2MOLC, O2MOL1, N2MOL1, AIRATO, AIRML1, AIRVOL,HTOVPR
      real(kind=8) :: BARPRS, air_esat, exit_esat, WMOL1, WMOL2, O2MOL2, N2MOL2, MOLCO2, AIRML2, TAEXIT, EVPMOL, GEVAP, TESVAL, GPERHR, KGEVAP
      
      
      !ALLOWING USER TO MODIFY GAS VALUES FOR BURROW, ETC. CONDITIONS
      PCTO2 = self%O2GAS/100.
      PCTN2 = self%N2GAS/100.
      PCTCO2 = self%CO2GAS/100.	
      BARPRS = current_climate(PSFC)           
      PO2 = BARPRS*PCTO2
      REFPO2 = STDPRS*RPCTO2
      
      !OXYGEN CONSUMPTION FROM GEN IN FUNCTION FUN THE TOTAL HEAT PRODUCTION
      !NEEDED TO MAINTAIN CORE TEMPERATURE, GIVEN THE CURRENT ENVIRONMENT &
      !THE ANIMAL'S PROPERTIES.
       
      !OXYGEN CONSUMPTION BASED ON HEAT GENERATION ESTIMATE TO MAINTAIN 
      !BODY TEMPERATURE, CORRECTED FOR SUBSTRATE UTILIZED.
      !LITERS OF O2/S @ STP: (DATA FOR EQUIVALENCIES FROM KLEIBER, 1961)
      !here, I assume a protein rich diet
      O2STP = GEN*TIMACT*(1./4.185)*(1./1000.)*(1.0/4.5) 
    
      !CONVERTING STP -> VOL. OF O2 AT ANIMAL TCORE, ATM. PRESS. 
      !FARIBA ASSADI-PORTER CORRECTION-5 DEC. 2002
      !((V1*P1)/T1)=((V2*P2)/T2)
      !OLD:	VO2CON = (O2STP*101325./273.15)*((TLUNG+273.15)/BARPRS)
      !NOTE IN OLD VERSION USING ATMOSPHERIC PRESSURE IN LAST TERM INSTEAD OF PO2 PRESSURE
      !TO BE CONSISTENT, WE HAVE TO CORRECT THE VOLUME OF O2 AT ITS REAL PARTIAL PRESSURE
      !IN THE ATMOSPHERE, SO WE HAVE TO USE THE ATMOSPHERIC PRESSURE IN V2 CORRECTED
      !FOR THE PARTIAL PRESSURE OF O2 IN THE ATMOSPHERE.
      VO2CON = ((O2STP*STDPRS)/273.15)*((self%TLUNG+273.15)/BARPRS)
       
      !O2 MOLES CONSUMED/S
      !N = PV/RT (IDEAL GAS LAW: NUMBER OF MOLES FROM PRESS,VOL,TEMP)
      !R = (P*V)/(N*T)= (101325PA*22.414L)/(1 MOL*273.15K)
      !HERE WE HAVE TO USE THE PARTIAL PRESSURE OF O2 TO GET MOLES OF O2
      O2MOLC = BARPRS*VO2CON/(RGC*(self%TLUNG+273.15))

      !MOLES/S O2, N2, & DRY AIR AT 1: (ENTRANCE) (AIR FLOW = F(O2 CONSUMPTION)
      O2MOL1 = O2MOLC/(EXTREF/100.)
      N2MOL1 = O2MOL1*(PCTN2/PCTO2)
      !DEMAND FOR AIR = F(%O2 IN THE AIR AND ELEVATION)
      !NOTE THAT AS LONG AS ALL 3 PERCENTAGES ADD TO 100%, NO CHANGE IN AIR FLOW,
      !UNLESS YOU CORRECT FOR CHANGE IN %O2 IN THE AIR AND ELEVATION CHANGES 
      !RELATIVE TO SEA LEVEL.
      AIRATO = (PCTN2+PCTO2+PCTCO2)/PCTO2
      AIRML1 = O2MOL1*AIRATO*(RPCTO2/PCTO2)*(REFPO2/PO2) 
      !AIR VOLUME @ STP (LITERS/S)
      AIRVOL = (AIRML1*RGC*273.15/101325.)

      !COMPUTING THE VAPOR PRESSURE AT SATURATION FOR THE SUBSEQUENT
      !CALCULATION OF ACTUAL MOLES OF WATER BASED ON ACTUAL RELATIVE
      !HUMIDITY.
      !RHSAT = 100.
      air_esat = vapprs(self%ta)
      
      !MOLES WATER/S IN AT 1 (ENTRANCE) BASED ON RELATIVE HUMIDITY.
      !NOTE THAT HUMIDITY IS SET TO 99% IN MAIN IF ANIMAL IS IN BURROW 
      !FOR THE CURRENT HOUR.     
      !WMOL1 = AIRML1*(self%ESAT*(RELHUM/100.))/(BARPRS-ESAT*(RELHUM/100.))
      WMOL1 = AIRML1*self%E/(BARPRS-self%E)

      !MOLES/S OF DRY AIR AT 2: (EXIT)
      O2MOL2 = O2MOL1 - O2MOLC
      N2MOL2 = N2MOL1
      MOLCO2 = RQ*O2MOLC 

      !TOTAL MOLES OF AIR AT 2 (EXIT) WILL BE APPROXIMATELY THE SAME
      !AS AT 1, SINCE THE MOLES OF O2 REMOVED = APPROX. THE # MOLES OF CO2 
      !ADDED.  AVOGADRO'S # SPECIFIES THE # MOLECULES/MOLE.
      AIRML2 = (O2MOL2+MOLCO2)*((PCTN2+PCTO2)/PCTO2)*(RPCTO2/PCTO2)*(REFPO2/PO2) 

      !EVAP. WATER LOSS REFLECTS NORMAL MAMMAL PATTERN
      TAEXIT = 0.33*self%TA + 26.75
      !CALCULATING SATURATION VAPOR PRESSURE, ESAT, AT EXIT TEMPERATURE.       
	    exit_esat = vapprs(0.33*self%ta+ 26.75)
	    WMOL2 = AIRML2*(exit_esat/(BARPRS-exit_esat))

      !MOLES/S LOST BY BREATHING:
	    EVPMOL = WMOL2-WMOL1
      !GRAMS/S LOST BY BREATHING = MOLES LOST * GRAM MOLECULAR WEIGHT OF WATER:
      GEVAP = EVPMOL*18. 
      
      !PUTTING A CAP ON WATER LOSS FOR SMALL ANIMALS IN VERY COLD CONDITIONS
      !BY ASSUMING THEY WILL SEEK MORE MODERATE CONDITIONS IF THEY EXCEED
      !THIS CAP. THIS WILL IMPROVE STABILITY FOR SOLUTION METHOD.
      !BASED ON DATA FROM W.R. WELCH. 1980. EVAPORATIVE WATER LOSS FROM 
      !ENDOTHERMS IN THERMALLY AND HYGRICALLY COMPLEX ENVIRONMENTS: AN
      !EMPIRICAL APPROACH FOR INTERSPECIFIC COMPARISONS. 
      !J. COMP. PHYSIOL. 139: 135-143.  MAXIMUM VALUE RECORDED FOR PRAIRIE 
      !DOGS WAS 0.6 G/(KG-H) = 1.667 X 10**-4 G/(KG-S) 
      !HIGHEST RECORDED RATE WAS A RESTING DEER MOUSE AT 8 G/KG-H =
      ! 2.22E-03*
      ! (EDWARDS & HAINES.1978. J. COMP. PHYSIOL. 128: 177-184 IN WELCH, 1980)
      !FOR A 0.01 KG ANIMAL, THE MAX. RATE WOULD BE 1.67**10^-6 G/S
      TESVAL = 2.22E-03*TIMACT*self%mass 
      IF (GEVAP .GT. TESVAL) THEN
        GEVAP = TESVAL
       ELSE
      ENDIF      
	
      !KG/S LOST BY BREATHING 
      GPERHR = GEVAP * 3600.
      KGEVAP = GEVAP/1000.
      !LATENT HEAT OF VAPORIZATION FROM SUB. DRYAIR (J/KG)
	    HTOVPR = 2.5012E+06 - 2.3787E+03*self%TLUNG 
    	!HEAT LOSS DUE TO WARMING AIR THAT ENTERS THE LUNG


      !HEAT LOSS BY BREATHING (J/S)=(J/KG)*(KG/S)
	    self%QRESP = HTOVPR*KGEVAP
	    self%GEVAP = GEVAP
      self%AIRVOL = AIRVOL
  end SUBROUTINE RESP
  
	subroutine calculate_rad_through_fur(self, Q, tout, part_no, tkskin, tkair, KRAD, C1, PSI, fskin, f_touching_substrate)
  	class(endo_abstract) :: self  
    real(kind=8), intent(in) :: tout, tkskin, tkair, fskin
    integer, intent(in) :: part_no
    real(kind=8), intent(out) :: KRAD, Q, C1, PSI
    real(kind=8) :: hr, bir, d1, c1n
    real :: bicv, Taverage_fur, tout_K, f_touching_substrate
    
    Taverage_fur=((tout+tkskin)/2.) !average dorsal fur temperature (K)
    tout_K = tout  
    KRAD=(16.0*sigma*Taverage_fur**3.)/(3.*self%BETARA(part_no))
    HR=fskin*4.*self%EMIS*sigma*Taverage_fur**3.   
    BIR=HR*self%ZFURAR(part_no)/(self%KEFARA(part_no)+KRAD)   
    BICV=self%HC*self%ZFURAR(part_no)/(self%KEFARA(part_no)+KRAD)
    IF(BICV .LE. 0.0000)THEN
      WRITE(0,*)'BICV ',BICV, 'IS NEGATIVE.  ERROR'
      PAUSE
    ENDIF  

    D1=1.+BICV+2.*BIR*(0.5-(0.333-self%E4B1(part_no)/self%B1ARA(part_no)))   
    C1N=(BICV*(1.-TKAIR/TKSKIN)+BIR*(1.-tout_K/TKSKIN))    
    C1=C1N/D1
    PSI=0.854+(ALOG10(BICV)*0.149)
    Q =PSI*self%AREASKIN*(fskin - f_touching_substrate)*KRAD*((C1*TKSKIN)/self%ZFURAR(part_no))     
  end subroutine calculate_rad_through_fur
    
  subroutine clear_data_endo_factory_abs(self)
    class(endo_factory_abstract) :: self    
    type(record), pointer :: cur_record
    class(object), pointer :: cur_mouse

    call m_observer%l_individuals%clear()
    print *, "clear finished"    
  end subroutine clear_data_endo_factory_abs
  

  subroutine print_statistics_endo_factory_abs(self)
      class(endo_factory_abstract) :: self
      integer :: ihour_month, iday
      real :: iwinter, isummer, delta, mean_met_diurnal_summer, mean_met_nocturnal_summer,  mean_met_diurnal_winter, mean_met_nocturnal_winter, &
              mean_evp_diurnal_summer, mean_evp_nocturnal_summer,  mean_evp_diurnal_winter, mean_evp_nocturnal_winter, &
              mean_activity_diurnal_summer, mean_activity_nocturnal_summer,  mean_activity_diurnal_winter, mean_activity_nocturnal_winter
      
      type(record), pointer :: log_record
      class(object), pointer :: cur_mouse
      
      
      iwinter=0 
      isummer=0
      mean_met_diurnal_summer=0
      mean_met_nocturnal_summer=0
      mean_met_diurnal_winter=0
      mean_met_nocturnal_winter=0
      mean_evp_diurnal_summer=0
      mean_evp_nocturnal_summer=0
      mean_evp_diurnal_winter=0
      mean_evp_nocturnal_winter=0
      mean_activity_diurnal_winter = 0
      mean_activity_diurnal_summer = 0
      mean_activity_nocturnal_winter = 0
      mean_activity_nocturnal_summer = 0
              
      log_record => m_observer%l_individuals%tail
      cur_mouse => log_record%data
      select type(cur_mouse)
      type is (endo_abstract) 
        
        do iday=1, 365
          sums_files(enum_evp_diurnal, iday, sum_current_file) = cur_mouse%evp_diurnal(iday)
          sums_files(enum_evp_nocturnal,iday ,sum_current_file) = cur_mouse%evp_nocturnal(iday)
          sums_files(enum_met_diurnal,iday ,sum_current_file) = cur_mouse%met_diurnal(iday)
          sums_files(enum_met_nocturnal,iday ,sum_current_file) = cur_mouse%met_nocturnal(iday)
          sums_files(enum_FVEG,iday ,sum_current_file) = cur_mouse%mean_fveg(iday)
          sums_files(enum_SMOIS,iday ,sum_current_file) = cur_mouse%mean_SMOIS(iday)
          
          !calculate mean winter results
          if (iday>=305 .or. iday<=59) then          
            iwinter = iwinter + 1.
            
            delta = cur_mouse%met_diurnal(iday) - mean_met_diurnal_winter  
            mean_met_diurnal_winter = mean_met_diurnal_winter + delta / iwinter
            
            delta = cur_mouse%met_nocturnal(iday) - mean_met_nocturnal_winter  
            mean_met_nocturnal_winter = mean_met_nocturnal_winter + delta / iwinter
            
            delta = cur_mouse%evp_diurnal(iday) - mean_evp_diurnal_winter  
            mean_evp_diurnal_winter = mean_evp_diurnal_winter + delta / iwinter
            
            delta = cur_mouse%evp_nocturnal(iday) - mean_evp_nocturnal_winter  
            mean_evp_nocturnal_winter = mean_evp_nocturnal_winter + delta / iwinter
            
            delta = cur_mouse%activity_hours_diurnal(iday) - mean_activity_diurnal_winter  
            mean_activity_diurnal_winter = mean_activity_diurnal_winter + delta / iwinter
              
            delta = cur_mouse%activity_hours_nocturnal(iday) - mean_activity_nocturnal_winter  
            mean_activity_nocturnal_winter = mean_activity_nocturnal_winter + delta / iwinter
            
          end if
          
          if (iday>=152 .and. iday<=243) then
            isummer = isummer + 1.
            
            delta = cur_mouse%met_diurnal(iday) - mean_met_diurnal_summer  
            mean_met_diurnal_summer = mean_met_diurnal_summer + delta / isummer
            
            delta = cur_mouse%met_nocturnal(iday) - mean_met_nocturnal_summer  
            mean_met_nocturnal_summer = mean_met_nocturnal_summer + delta / isummer
            
            delta = cur_mouse%evp_diurnal(iday) - mean_evp_diurnal_summer  
            mean_evp_diurnal_summer = mean_evp_diurnal_summer + delta / isummer
            
            delta = cur_mouse%evp_nocturnal(iday) - mean_evp_nocturnal_summer  
            mean_evp_nocturnal_summer = mean_evp_nocturnal_summer + delta / isummer  
            
            delta = cur_mouse%activity_hours_diurnal(iday) - mean_activity_diurnal_summer  
            mean_activity_diurnal_summer = mean_activity_diurnal_summer + delta / isummer
            
            delta = cur_mouse%activity_hours_nocturnal(iday) - mean_activity_nocturnal_summer  
            mean_activity_nocturnal_summer = mean_activity_nocturnal_summer + delta / isummer 
            
          end if
        end do
        print *, cur_mouse%evp_diurnal
        sums_files(enum_evp_diurnal_summer,1 ,sum_current_file) = mean_evp_diurnal_summer
        sums_files(enum_evp_nocturnal_summer,1 ,sum_current_file) = mean_evp_nocturnal_summer
        sums_files(enum_evp_diurnal_winter,1 ,sum_current_file) = mean_evp_diurnal_winter
        sums_files(enum_evp_nocturnal_winter,1 ,sum_current_file) = mean_evp_nocturnal_winter
        sums_files(enum_met_diurnal_summer,1 ,sum_current_file) = mean_met_diurnal_summer
        sums_files(enum_met_nocturnal_summer,1 ,sum_current_file) = mean_met_nocturnal_summer
        sums_files(enum_met_diurnal_winter,1 ,sum_current_file) = mean_met_diurnal_winter
        sums_files(enum_met_nocturnal_winter,1 ,sum_current_file) = mean_met_nocturnal_winter
        
        sums_files(enum_activity_diurnal_summer,1 ,sum_current_file) = mean_activity_diurnal_summer
        sums_files(enum_activity_nocturnal_summer,1 ,sum_current_file) = mean_activity_nocturnal_summer
        sums_files(enum_activity_diurnal_winter,1 ,sum_current_file) = mean_activity_diurnal_winter
        sums_files(enum_activity_nocturnal_winter,1 ,sum_current_file) = mean_activity_nocturnal_winter
        
        do ihour_month =1, 288
          sums_files(enum_met_hourly,ihour_month ,sum_current_file) = cur_mouse%met_monthly_hourly(ihour_month)
          sums_files(enum_evp_hourly,ihour_month ,sum_current_file) = cur_mouse%evp_monthly_hourly(ihour_month)
        end do
      end select
      !save data in sum arrays - will be used to transfer to root process and write netcdf
      sum_lats(sum_current_file) = m_climate%lat
      sum_lons(sum_current_file) = m_climate%lon
  end subroutine print_statistics_endo_factory_abs
  
  subroutine set_fur_properties(self)
      class(endo_abstract) :: self
      
      !CONVERTING INPUT DATA TO SI UNITS
      self%DHAIRD = self%DHAIRD/1.0E+06 ! um -> m
      self%DHAIRV = self%DHAIRV/1.0E+06 ! um -> m
      self%LHAIRD = self%LHAIRD/1.0E+03 !mm -> m
      self%LHAIRV = self%LHAIRV/1.0E+03 !mm -> m
      self%ZFURD = self%ZFURD /1.0E+03 !mm -> m
      self%ZFURV = self%ZFURV / 1000. !mm -> m
      self%RHOD = self%RHOD*1.0E+04 !HAIRS/CM2 -> HAIRS/M2
      self%RHOV = self%RHOV*1.0E+04 !HAIRS/CM2 -> HAIRS/M2
      
      self%ABSAND = 1.00 - self%REFLD
      self%ABSANV = 1.00 - self%REFLV
      
      self%DHARA(dorsal) = self%DHAIRD(torso)
      self%DHARA(ventral) = self%DHAIRV(torso)
      self%DHARA(overall) = (self%DHAIRD(torso) + self%DHAIRV(torso))/2.
      
      self%LHARA(overall) = (self%LHAIRD(torso) + self%LHAIRV(torso))/2.
      self%LHARA(dorsal) = self%LHAIRD(torso)
      self%LHARA(ventral) = self%LHAIRV(torso)
      
      self%RHOARA(overall) = (self%RHOD(torso)  + self%RHOV(torso) )/2.
      self%RHOARA(dorsal) = self%RHOD(torso) 
      self%RHOARA(ventral) = self%RHOV(torso) 
      
      self%ZFURAR(overall) = (self%ZFURD(torso)+self%ZFURV(torso))/2.
      self%ZFURAR(dorsal) = self%ZFURD(torso)
      self%ZFURAR(ventral) = self%ZFURV(torso)
      
      self%fur_depth = self%ZFURAR(overall)
  end subroutine set_fur_properties
    
  subroutine thermoregulation(self)
  
    class(endo_abstract) :: self

    IF(self%AK1 .LT. self%AK1MAX)THEN
      self%AK1 = self%AK1*1.1
      IF(self%AK1MAX-self%AK1.LE.0.01)THEN
        self%AK1=self%AK1MAX
      ELSE
        self%AK1 = self%AK1
      ENDIF
    else
      self%SKINW = self%SKINW + 0.1
    ENDIF

  end subroutine thermoregulation
  
  REAL FUNCTION ZBRENT(self,X1,X2,TOL)
  
    class(endo_abstract) :: self
   !EPS IS MACHINE FLOATING POINT PRECISION
   !FLOATING POINT PRECISION FOR THE COMPAQ III PORTABLE IS 5.0E-20
    REAL(kind=8) :: A,B,C,D,E,DIAGNOS,EPS,FA,FB,FC,FUN
    REAL(kind=8) :: P,Q,R,S,TOL,TOL1,X1,X2,XM
    INTEGER ITER,ITMAX

    ITMAX=20
    EPS=3.E-8
    DIAGNOS = 0.
    A=X1
    B=X2
    FA=self%FUN(A)
    FB=self%FUN(B)
    IF((FB*FA.GT.0.) .AND. (DIAGNOS.GT.0.0)) THEN 
      WRITE (0,*)'ROOT MUST BE BRACKETED FOR ZBRENT.' 
     ELSE
    ENDIF  
    FC=FB
    DO ITER=1,ITMAX
      IF(FB*FC.GT.0.) THEN
        C=A
        FC=FA
        D=B-A
        E=D
      ENDIF
      IF(ABS(FC).LT.ABS(FB)) THEN
        A=B
        B=C
        C=A
        FA=FB
        FB=FC
        FC=FA
      ENDIF
      TOL1=2.*EPS*ABS(B)+0.5*TOL
      XM=.5*(C-B)
      IF(ABS(XM).LE.TOL1 .OR. FB.EQ.0.)THEN
        ZBRENT=B
	      return
      ENDIF
  	  !AN ADDITION, SINCE THIS SUBROUTINE SOMETIMES MISSES A SOLUTION
  	  !WARREN PORTER 2003
      IF(ABS(FB).LE.TOL)THEN
        IF(ITER.GT.1)THEN
          ZBRENT=B
	      !GO TO 20
        ENDIF
      ENDIF
      IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
        S=FB/FA
        IF(A.EQ.C) THEN
          P=2.*XM*S
          Q=1.-S
        ELSE
          Q=FA/FC
          R=FB/FC
          P=S*(2.*XM*Q*(Q-R)-(B-A)*(R-1.))
          Q=(Q-1.)*(R-1.)*(S-1.)
        ENDIF
        IF(P.GT.0.) Q=-Q
        P=ABS(P)
        IF(2.*P .LT. MIN(3.*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
          E=D
          D=P/Q
        ELSE
          D=XM
          E=D
        ENDIF
      ELSE
        D=XM
        E=D
      ENDIF
      A=B
      FA=FB
      IF(ABS(D) .GT. TOL1) THEN
        B=B+D
      ELSE
        B=B+SIGN(TOL1,XM)
      ENDIF
      FB=self%FUN(B)
    end do  
    IF (DIAGNOS .GT. 0.0) THEN
      WRITE(0,*) 'ZBRENT EXCEEDING MAXIMUM ITERATIONS.' 
    ENDIF  
    ZBRENT=B
    return
  END function ZBRENT
  
  
  SUBROUTINE ZBRAC(self, X1,X2,SUCCES)
  class(endo_abstract) :: self
  REAL(kind=8) :: F1,F2,X1,X2
  INTEGER J
      LOGICAL SUCCES
      
      IF(X1.EQ.X2)PAUSE 'You have to guess an initial range'
      F1=self%FUN(X1)
      F2=self%FUN(X2)
      SUCCES=.TRUE.
      DO J=1,20
        IF(F1*F2.LT.0.)RETURN
        IF(ABS(F1).LT.ABS(F2))THEN
          X1=X1+1.6*(X1-X2)
          F1=self%FUN(X1)
        ELSE
          X2=X2+1.6*(X2-X1)
          F2=self%FUN(X2)
        ENDIF
      end do
      SUCCES=.FALSE.
    END subroutine ZBRAC


end module endo_abstract_module
