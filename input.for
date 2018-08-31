      SUBROUTINE  INPUT

C ** Modified 8/14/90 >> Modified Hi (SKONST / S).

C **  Subroutine INPUT computes solar radiation as a function of the
C **  day, season, atmospheric attenuation, and albedo.

      DOUBLE PRECISION RADIAN , SDEC

      REAL SS(12)
      INTEGER MD(12)

$INCLUDE:'modvars.h'

C **  The definition of the variables in the following DATA statements
C **  can be found in the manual.

      DATA MD / 31 , 29 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30,
     #          31/
      DATA SS / 0.967 , 0.971 , 0.982 , 0.999 , 1.016 , 1.029 , 1.034,
     #          1.030 , 1.018 , 1.002 , 0.985 , 0.973 /
      DATA RADIAN,SDEC,SIGA / 572957.75913D-4 , 39784.988432D-5,
     #				    279.9348 /

      skonst = 1.94*4.1868e4/60

C **  Y ...Counter for determining celestial mechanics for long, lat and
C **  time of year.

      IF ( Y .NE. 1 ) GO TO 19
      Y = Y + 1

      IMO1 = IMO + 1
      IF (IMO1 .EQ. 13) IMO1 = 1

C **  Compute the solar distance factor.

      RM = FLOAT( IDAY - 1 ) / FLOAT( MD ( IMO ) )
      S = SS( IMO ) + RM * (SS( IMO1 ) - SS( IMO ) )

C **  Set up the number of the day in the year (DAD).

      K = IDAY
      JMO = IMO - 1
      IF ( JMO .LT. 1 ) GO TO 12
      DO 5 I = 1 , JMO
       K = K + MD(I)
    5 CONTINUE
   12 N = IYR / 4
      N = IYR - N * 4
      IF (N .NE. 0 .AND. K .GE. 60) K = K - 1
      DAD = K - 1

C **  Calculate the angular fraction of a year, convert to radians.

      DF = DAD * 360 / 365.242
      DE = DF / RADIAN

C **  Correction to declination caused by elliptical orbit.

      SIG = SIGA + DF + 1.914827 * SIN( DE ) - 0.079525 * COS( DE )
     #     + 0.019938 * SIN( DE * 2 ) - 0.00162 * COS( DE * 2 )
      SIG = SIG / RADIAN

C **  Declination of the sun.

      EFFDEC = ASIN( SDEC * SIN( SIG ) )

C **  True solar noon.

      EQT = 0.12357 * SIN( DE ) - 0.004289 * COS( DE )
     #      + 0.153809 * SIN( DE * 2 ) + 0.060783 * COS( DE * 2 )
      EQT = EQT + 12

   19 CONTINUE

C **  Calc time in Greenwich Mean Time.

      GMT = PTIME + TZ

*/  Calculate the solar hour angle in radians.

      HRANGL = 15 * ( GMT - EQT ) - XLONG
      HRANGL = HRANGL / RADIAN

*/  Now we can finally compute the solar elevation angle.

      SLB = XLAT / RADIAN
      SOLSIN = SIN(EFFDEC)*SIN(SLB)+COS(EFFDEC)*COS(SLB)*COS(HRANGL)
      SOLEL = ASIN( SOLSIN ) * RADIAN

      if (SLOPE .GT. 0) then
	call sslope (sinsolelslope,solelslope,effdec,slb,hrangl)
      else
	call albedo (solsin)
	sinsolelslope = solsin
      end if

*/  If the solar altitude is less that or equal to zero it's night so
*/  return, otherwise compute absolute optical air mass (PATH).

      IF ( SOLEL .LE. 0 ) THEN
       SWAVE = 0
       RETURN
      END IF

      RLPATH = ( SOLSIN + 0.15 * ( SOLEL + 3.885 ) ** (-1.253) ) ** (-1)
*/      if (rlpath .lt. 1.0) rlpath = 1.0 
      PATH = 0.001 * PS1 * RLPATH
      SOLEL = SOLEL / RADIAN

      CALL TRANSM (RLPATH,FTABS,FTSCAT,FBSCAT)

*/  Store values for use.

      TABS = FTABS
      TSCAT = FTSCAT
      BSCAT = FBSCAT

*/  Set the PATH length for diffuse radiation equal to 1.7.

      PATH = 1.7

      CALL TRANSM (PATH,FTABS,FTSCAT,FBSCAT)

*/  Again store values for use.

      TABSD  = FTABS
      TSCATD = FTSCAT
      BSCATD = FBSCAT

C **  SHEAT >>> Sunlight amount on horizontal plane outside atmosphere
C **  XSER  >>> Diffuse Shortwave Radiation at the ground.
C **  HI    >>> Direct SW Radiation reaching the ground.
C **  SWAVE >>> Diffuse + Direct = 'Global'

      SHEAT = SKONST * sinsolelslope / S
      XSER  = BSCATD * ALBDOE * ( 1 - TSCATD ) * TABSD * SIN( SOLEL )
      HI = ( SHEAT * TABS * TSCAT ) + ( SKONST / S* TABS * ( 1 - TSCAT )
     #     * ( 1 - BSCAT ) ) * SIN( SOLEL )
      SWAVE = ( HI * ( 1 - ALBDOE ) ) / ( 1 - XSER )

       IF(CLOUD_FLAG) SWAVE = SWAVE*(1-(0.7*(0.1*CLD_FRACT)))
       ! impose cloud fraction; reduce swave according to cloud amount
       ! Note: forcing real arithmetic



      RETURN
      END
