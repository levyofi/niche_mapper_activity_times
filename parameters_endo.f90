module parameters_endo
    implicit none
    
    enum , bind (c) ! enum for fur parts
        enumerator ::       overall=1,&  
                            dorsal, &
                            ventral
        enumerator ::       leg = 1, & 
                            head_and_neck, &
                            torso 
    end enum
    
    !put global parameters here (not specific to the studied species)
    real, parameter :: RHOHR = 1.31E+06 !DENSITY OF A HAIR FIBER (G/M3) FROM LEIDER AND BUNCKE, 1954. AMA ARCH. DERMATOL.SYPHIL. 69:563-569 
    REAL, parameter :: KHAIR = 0.209 !CONDUCTIVITY OF HAIR FIBERS (W/M-C)  
    real, parameter :: FLTYPE = 0.0                
    !ANIMAL EMISSIVITY
    real, parameter :: EMISAN = .99
    !ANIMAL DENSITY (KG/M**3)
    real, parameter :: ANDENS = 932.9
    !TISSUE CONDUCTIVITIES FROM CHATO, J.C. 1969.  HEAT TRANSFER IN 
    !BIOENGINEERING. IN ADVANCED HEAT TRANSFER, B.T. CHAO, ED.,
    !U. OF ILL. PRESS, URBANA. 395-412.       
    !maximum CONDUCTIVITY OF FLESH (W/M-C)
	  real, parameter :: AK1MAX = 0.60
    !CONDUCTIVITY OF FAT (W/M-C)
    real, parameter :: AK2=0.230
    !percentage of diffuse solar radiation
    real, parameter :: PCTDIF = 0.1
    
    real, parameter :: RQ = 0.8 ! the respiratory quotient (0.8 CO2 evolved / 1 O2 consumed)   
    
    !percentages of O2, N2, and CO2 in the air          
    real, parameter :: RPCTO2 = 0.2095
  	real, parameter :: RPCTN2 = 0.7902
    real, parameter :: RPCTCO2 = 0.0003
    
    real, parameter :: EXTREF = 24. !O2 extraction efficiency (%)
  
    !UNIVERSAL GAS CONSTANT (PA - LITERS)/(MOL - K)
    real, parameter :: RGC = 8309.28
    real, parameter :: TIMACT = 1. !multiplication for activity ?
    
    real, parameter :: g=9.80665 !acceleration due to gravity 
    real, parameter :: cp=1.0057e+3 !specific heat of air
    real (kind=8) :: sigma=5.67*10.**(-8) ! stefan-boltzman constant (W · m -2 · K -4 )
    real :: PI = 3.141592 

    CONTAINS
    
    REAL(kind=8) FUNCTION E4(B1)   
    !FUNCTION E4(B1) CALCULATES EXPONENTIAL INTEGRAL E4(B1) ACCORDING  
    !TO POLYNOMIAL APPROXIMATIONS IN ABRAMOWITZ AND STEGUN (A&S) 
    !COPYRIGHT 2004  WARREN P. PORTER.  ALL RIGHTS RESERVED.

	    REAL(kind=8) :: AI1,AI2,AI3,AI4,AJ1,AJ2,AJ3,AJ4,AN9
	    REAL(kind=8) :: B1,D9,E1,E2B1,E3B1,E4B1
	    REAL(kind=8) :: F0,F1,F2,F3,F4,F5,F9,X
	         
      X=B1  
      IF (X>88.0) then
        X=88.0
        WRITE(*,*) 'FUNCTION E4 ARGUMENT EXCEEDS 88.0: RESET TO 88'
      end if
      IF (X<=1.0) then  
        !EQUATION 5.1.53 PAGE 231 A&S  
        F0=-0.57721   
        F1=0.99999
        F2=-0.24991   
        F3=0.05519
        F4=-0.00976   
        F5=0.00107
        IF(X.EQ.0.0000000000000)THEN
          X = 0.001
          WRITE(0,*)'X IN FUNCTION E4 = ',X
          WRITE(0,*)'X RESET TO 0.001'
        ENDIF
        E1=(F0+F1*X+F2*X**2+F3*X**3+F4*X**4+F5*X**5)-LOG(X)   

        !EQUATION 5.1.14 PAGE 229 A&S  
        E2B1=         EXP(-X)-X*E1
        E3B1=(1./2.)*(EXP(-X)-X*E2B1) 
        E4B1=(1./3.)*(EXP(-X)-X*E3B1) 
        E4=E4B1   

        !EQUATION 5.1.56 PAGE 231 A&S  
      else
        AI1=8.57332   
        AI2=18.05901  
        AI3=8.63476   
        AI4=0.26777   
        AJ1=9.57332   
        AJ2=25.63295  
        AJ3=21.09965  
        AJ4=3.95849   
        AN9=X**4+AI1*X**3+AI2*X**2+AI3*X+AI4  
        D9=X**4+AJ1*X**3+AJ2*X**2+AJ3*X+AJ4   
        F9=AN9/D9 

        !EQUATION 5.1.14 PAGE 229 A&S  
        E1=F9/(X*EXP(X))  
        E2B1=         EXP(-X)-X*E1
        E3B1=(1./2.)*(EXP(-X)-X*E2B1) 
        E4B1=(1./3.)*(EXP(-X)-X*E3B1) 
        E4=E4B1
      end if   
    END function e4
      
end module parameters_endo

