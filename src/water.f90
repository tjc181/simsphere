subroutine  WATER (TIME,BareEvapFlux)
  use simsphere_mod
  implicit none

  real, parameter :: CONST1 = 1
  real, parameter :: CONST2 = 0.5
  real, parameter :: D1P = 0.1
  real, parameter :: D_INT = 0.5
  real, parameter :: D2P = 0.5
  real, parameter :: OMG = 24
  real :: TIME, BareEvapFlux
  real :: WIN, PER, C11, C22, C33, C44, EVAX, EVAS, EVAI
  real :: WW1, WW2, WW3

! **  WATER is based on the technique of Deardroff (1978). It uses the
! **  evaporative flux value obtained in FLUX and updates two internal
! **  variables WGG and W2G, which represent the soil moisture content
! **  of the soil close to the surface and in the first 50 cm of soil,
! **  respectively.  The empirical constants can be found in the article.

! **  1988 modifications: Substrate layer assigned moisture availability
! **  which is called FSUB (W2G/WMAX).  Intermediate layer water
! **  equation for variable called (WIN).  Top layer is assumed to pertain
! **  to top 2 cm (instead of top 10 cm as for intermediate layer),
! **  20% of root evaporation is drawn from this layer.  Top layer draws
! **  on all surface evaporation.  Lowest (reservoir) layer on all
! **  evaporation.  Note that constant CONST2 changed from earlier version.
! **  Note if you wish to supress variation in substrate water content
! **  with time, let wmax equal to a very large value, e.g. 10.

!      INCLUDE 'modvars.h'

! **  Constants for the water budget equation.

!  DATA CONST1 , CONST2, D1P,D_INT, D2P/1, 0.5, 0.1, 2*0.5/ OMG / 24 /

  IF ( TIME == 0 ) THEN
    WIN = ( WGG + W2G ) / 2
  ELSE
    WIN = 0.001
  END IF

  PER = OMG * 3600
  C11 = CONST1 / ( RHOW * D1P )
  C22 = CONST2 / PER
  C33 = 1 / ( D2P * RHOW )
  C44 = CONST1 / ( RHOW * D_INT )

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

! **  Compute the updated version of moisture availability and substrate
! **  moisture availability.

  F = ( WGG / WMAX )
  FSUB = ( W2G / WMAX )

  return
end
