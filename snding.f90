use simsphere_mod

      SUBROUTINE  SNDING (ZLS, Old_Ahum)

      real*4,allocatable :: derivs(:)

      REAL PS(50), EW(50),QS(50), DEP(50), DD0(50), FF0(50), ZLS(50),
     /           ZH(50),UCOMP(50), VCOMP(50), GMQ(50), Pot_Temp (50)
      ! Actual and decomposed values

      ! Put here -- Arrays to hold interpolated values [in COMMON at present]
      ! TD(50), QD(50), UD(50), VD(50)

      real Old_Ahum
        Integer error
      
!      INCLUDE 'constant.h'
!      INCLUDE 'factors.h'
!      INCLUDE 'modvars.h'

      Data Vert_Spacing /250/ ! 250 metre intervals
        deltaz = vert_spacing

      READ(9,*) NOBS_pTq, NOBS_wind, Station_Height, UGS, VGS

      DO 5 I = 1, NOBS_pTq
       READ(9, * ) PS(I),TS(I),DEP(I) ! Read P/T/q Obs
    5 CONTINUE

      DO 10 J=1, NOBS_wind
       READ (9, * ) DD0(J),FF0(J),ZH(J) ! Read Wind Obs
   10 CONTINUE

*/ P/T/q first

      HEIGHT = 0.

      DO 15 J = 1 , NOBS_pTq

       I = J - 1

       TDEW = TS(J) - DEP(J) + 273.15 ! Dew Point (K)
       EW(J) = 6.11 * EXP((2.49E6 / 461.51)*(1 / 273.15 - 1 / TDEW)) ! es (mb)
       QS(J) = 0.622 * EW(J) / PS(J) ! Specific Humidity (g/Kg)
       Pot_Temp(J) = (TS(J) + 273.15) * (1000. / PS(J)) ** 0.286 ! Theta (K)

       If (J .GT. 1) Then

         TBAR = (( TS(I) + TS(I+1) ) / 2 ) + 273.15 ! Average Temperature (K)
         THICK = 287 * (TBAR / 9.8) * ALOG(PS(I) / PS(I+1)) ! Thickness (m)
         HEIGHT = HEIGHT + THICK ! Height (above station) of pressure level
         ZLS(I+1) = HEIGHT
         GM(I) = ( Pot_Temp(I+1) - Pot_Temp(I) ) / THICK ! d(Theta)/dZ
         GMQ(I) = ( QS(I) - QS(I+1) ) / THICK ! d(q)/dZ

         precip_H2O = -0.622/GRAV*10.*((EW(J-1)*PS(J)-EW(J)*PS(J-1))*
     /                     ALOG(PS(J)/PS(J-1))/(PS(J)-PS(J-1))+EW(J)-EW(J-1))
         sum_precip_H2O = sum_precip_H2O + precip_H2O
            omega = sum_precip_H2O
       Endif

   15 CONTINUE

*/ Winds -- Note way winds are recorded by the weather service

      ZH(1) = 0.
      do 20 i = 2, NOBS_wind
       ZH(i) = ((ZH(i) - Station_Height) * 1.0E3) / Ft_To_Metres
   20 continue

*/ Calculate u and v velocity components

      DO 25 J = 1 , NOBS_wind
       ucomp(j)=(-FF0(j)*cos((90.-DD0(j))/radian))/Kts_To_Metres
       vcomp(j)=(-FF0(j)*sin((90.-DD0(j))/radian))/Kts_To_Metres
  25  CONTINUE

*/ Number of levels it's possible to interpolate (NTRP) to are

      Height_at_NTRP = MIN(HEIGHT, ZH(NOBS_wind))
      NTRP = AINT((Height_at_NTRP - 50.) / Vert_Spacing)

*/ Interpolate at 'h' height intervals using cubic splines.
*/ Numerical Recipes in FORTRAN -- pages 109, 110.

*/ 1.  Temperature

      allocate (derivs(NOBS_pTq), STAT = error)
      if (error .ne. 0) stop 'not enough memory'

*/ Calculate derivatives at the boundaries

      dydx =  (Pot_Temp(2) - Pot_Temp(1)) / (ZLS(2) - ZLS(1))
      dydxn = (Pot_Temp(NOBS_pTq) - Pot_Temp(NOBS_pTq-1)) /
     /            (ZLS(NOBS_pTq) - ZLS(NOBS_pTq-1))

*/ Calculate array of derivatives

      call spline (ZLS, Pot_Temp, NOBS_pTq, dydx, dydxn, derivs)

*/ Call splint to get actual value at 50m and subsequent N metre intervals

      do 30 i = 0, NTRP
       h = 50 + (Vert_Spacing * i)
         zi(i+1) = h
       call splint (ZLS, Pot_Temp, derivs, NOBS_pTq, h, TD(i+1))
   30 continue

*/ Free up space taken by the list of derivatives

      deallocate (derivs, STAT = error)

*/ 2. Moisture

      allocate (derivs(NOBS_pTq), STAT = error)
      if (error .ne. 0) stop 'not enough memory'

*/ Calculate derivatives at the boundaries

      dydx = (QS(2) - QS(1)) / (ZLS(2) - ZLS(1))
      dydxn = (QS(NOBS_pTq) - QS(NOBS_pTq-1)) /
     /              (ZLS(NOBS_pTq) - ZLS(NOBS_pTq-1))

*/ Calculate array of derivatives

      call spline (ZLS, QS, NOBS_pTq, dydx, dydxn, derivs)

*/ Call splint to get actual value at 50m and subsequent N metre intervals

      do 35 i = 0, NTRP
       h = 50 + (Vert_Spacing * i)
       call splint (ZLS, QS, derivs, NOBS_pTq, h, QD(i+1))
   35 continue

*/ Free up space taken by the list of derivatives

      deallocate (derivs, STAT = error)

*/ 3. Winds

*/ Calculate the u component

*/ Create an array of REALS (floats) to put the dy/dx values into

      allocate (derivs(NOBS_wind), STAT = error)
      if (error .ne. 0) stop 'not enough memory'

*/ Calculate derivatives at the boundaries

      dydx = (ucomp(2) - ucomp(1)) / (ZH(2) - ZH(1))
      dydxn = (ucomp(NOBS_wind) - ucomp(NOBS_wind-1)) /
     /              (ZH(NOBS_wind) - ZH(NOBS_wind-1))

*/ Calculate array of derivatives

      call spline (ZH, ucomp, NOBS_wind, dydx, dydxn, derivs)

*/ Call splint to get actual value at 50m and subsequent 250m intervals

      do 40 i = 0, NTRP
       h = 50 + (Vert_Spacing * i)
       call splint (ZH, ucomp, derivs, NOBS_wind, h, UD(i+1))
   40 continue

*/ Free up space taken by the list of derivatives

      deallocate (derivs, STAT = error)

*/ Do the same for v component

*/ Create an array of REALS (floats) to put the dy/dx values into

      allocate (derivs(NOBS_wind), STAT = error)
      if (error .ne. 0) stop 'not enough memory'

*/ Calculate derivatives at the boundaries

      dydx = (vcomp(2) - vcomp(1)) / (ZH(2) - ZH(1))
      dydxn = (vcomp(NOBS_wind) - vcomp(NOBS_wind-1)) /
     /              (ZH(NOBS_wind) - ZH(NOBS_wind-1))

*/ Calculate array of derivatives

      call spline (ZH, vcomp, NOBS_wind, dydx, dydxn, derivs)

*/ Call splint to get actual value at 50m and subsequent 250m intervals

      do 45 i = 0, NTRP
       h = 50 + (Vert_Spacing * i)
       call splint (ZH, vcomp, derivs, NOBS_wind, h, VD(i+1))
   45 continue

*/ Free up space taken by the list of derivatives

      deallocate (derivs, STAT = error)

*/ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

*/ Calc lapse rate of temp in 1st layer and EW at screen level for
*/ use in the calc of screen level sat'n spec humidity.

      ATEMP = 50 * (TS(2) - TS(1)) / ZLS(2) + TS(1) + 273.15
      TSCREN = TS(1) + 273.15 ! Screen Temperature
      EW = 6.11 * EXP (( 2.49E6 / 461.51 ) *(1 / 273.15 - 1 / TSCREN))

      OSHUM = 0.622 * EW(1) / PS(1)
      AHUM = QS(1)
      Old_Ahum = QS(1)
      PS1 = PS(1)

*/ Changes 2/10/92

      Pres_50 = PS(1)*EXP(-9.8*50/(287* (TS(1) + 273.15)))
      Pot_50 = ATEMP * (1000 / Pres_50)** 0.286
      APTEMP = Pot_50
      O_Pot_Tmp = Pot_50
      Tdif_50 = Pot_50 - Atemp
      Tdif_s = Tdif_50 - 0.5

c        tdeww = tdew-273.15
c        expt=7.5*tdeww/(237.3+tdeww)
c        eww = 6.11*10**expt
c        ewww = qs(j)*ps(j)/(0.378*qs(j)+0.622)

c        rhoa = ps(j)*100./(287.*tbar) ! Kell's Precip water calc
c        asum = asum + (qs(i) + qs(i+1))/2.*.001*rhoa*thick*100



c Input file wasn't being close explicitly, now it is. (It's opened 
c in subroutine START.

        CLOSE(UNIT=9)

      RETURN
      END
