	 subroutine dynprf (No_Rows)

	  real V_wind(52),T_profile(52),q_profile(52)
	  real aa,bb,dd
	  integer No_Levels, Thick_Level, No_variables, No_Rows
	  integer P_height(52)
	  integer Wind_dir(52)
	  integer*1 I_Header
          
$INCLUDE:'modvars.h'

      data I_Header,No_Levels, Thick_Level, No_variables
     /		/1, 52, 50, 4/
          
      DO 28 L=1,51
        V_wind(L+1)=SQRT(u_fine(L)**2+v_fine(L)**2)
	IF (ABS(u_fine(L)) .LT. 0.001) GOTO 250
        AA=270.0-ATAN2(v_fine(L),u_fine(L))*57.2958
        BB = AA - 360.0
        IF(BB)300,300,400
  300    DD=AA
         GO TO 280
  250    DD = 180.0
        IF(v_fine(L).LE.0.0) DD=0.0
         GO TO 280
  400    DD = BB
  280    CONTINUE
	 Wind_dir(L+1)=ANINT(DD)
c      first wind speed and direction
c      now temperature and humidity
	q_profile(L+1) = q_fine(L) * 1000   !g/Kg
	t_profile(L+1) = t_fine(L) - 273.13 !C
        P_height(l+1) = 50*L
   28  CONTINUE

c now fill in first elements of array
c wind speed is uten and direction same as at 50 m
c wind speed at lowest layer not calculated at night

	 V_wind(1) = uten
	 Wind_dir(1) = Wind_dir(2)
	 q_profile(1) = qa*1000 ! g/Kg
	 t_profile(1) = (ta*(1000/ps1)**0.286) - 273.13 !C
         P_height(1) = 0   

	if (I_Header .eq. 1) then

	write (12, 4) No_Levels, Thick_Level, No_variables, No_Rows
 4	format(4(I4,1X))

	WRITE (12,*) 'Wind_Speed/ms-1 ','Wind_Direction '
	WRITE (12,*) 'Air_Temperature/C ','Specific_Humidity/gKg-1 '

	I_Header = 2

      endif

      write (12, 89) ptime
      write (12, 90) (V_wind(k), k=1,52)
      write (12, 91) (Wind_dir(k), k=1,52)
      write (12, 92) (T_profile(k), k=1,52)
      write (12, 93) (q_profile(k), k=1,52)


  89  Format(F5.2)
  90  Format(20F5.1)
  91  Format(20I4)
  92  Format(20F5.1)
  93  Format(20F5.1)

      return
      end
