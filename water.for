      SUBROUTINE  WATER (TIME,BareEvapFlux)

C **  WATER is based on the technique of Deardroff (1978). It uses the
C **  evaporative flux value obtained in FLUX and updates two internal
C **  variables WGG and W2G, which represent the soil moisture content
C **  of the soil close to the surface and in the first 50 cm of soil,
C **  respectively.  The empirical constants can be found in the article.

C **  1988 modifications: Substrate layer assigned moisture availability
C **  which is called FSUB (W2G/WMAX).  Intermediate layer water
C **  equation for variable called (WIN).  Top layer is assumed to pertain
C **  to top 2 cm (instead of top 10 cm as for intermediate layer),
C **  20% of root evaporation is drawn from this layer.  Top layer draws
C **  on all surface evaporation.  Lowest (reservoir) layer on all
C **  evaporation.  Note that constant CONST2 changed from earlier version.
C **  Note if you wish to supress variation in substrate water content
C **  with time, let wmax equal to a very large value, e.g. 10.

      INCLUDE 'modvars.h'

C **  Constants for the water budget equation.

      DATA CONST1 , CONST2, D1P,DINT, D2P/1, 0.5, 0.1, 2*0.5/
     #	   OMG / 24 /

      IF ( TIME .EQ. 0 ) THEN
       WIN = ( WGG + W2G ) / 2
      END IF

      PER = OMG * 3600
      C11 = CONST1 / ( RHOW * D1P )
      C22 = CONST2 / PER
      C33 = 1 / ( D2P * RHOW )
      C44 = CONST1 / ( RHOW * DINT )

      EVAX = EVAP / LE

      IF ( FRVEG .GT. 0 .AND. RNET .GT. 0 ) THEN

       EVAS = (XLEG * FRVEG + ( 1 - FRVEG ) * BareEvapFlux) / LE
       EVAI = ( XLEF * FRVEG ) / LE

      ELSE

       EVAS = EVAX
       EVAI = 0

      END IF

      WW1 = ( C11 * EVAS + C22 * ( WGG - WIN ) ) * DELTA
      WW2 = ( C44 * EVAI - C22 * ( WGG + W2G - 2 * WIN ) ) * DELTA
      WW3 = EVAI * C33 * DELTA

      WGG = WGG - WW1
      WIN = WIN - WW2
      W2G = W2G - WW3

      IF ( WGG .LE. 0 ) WGG = 0.001
      IF ( WIN .LE. 0 ) WIN = 0.001
      IF ( W2G .LE. 0 ) W2G = 0.001

C **  Compute the updated version of moisture availability and substrate
C **  moisture availability.

      F = ( WGG / WMAX )
      FSUB = ( W2G / WMAX )

      RETURN
      END
