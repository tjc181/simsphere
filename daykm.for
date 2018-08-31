      SUBROUTINE  DAYKM

C **  This routine computes eddy diffusivities as a function of height,
C **  friction velocity and Monin Obukhov length for all the relevant
C **  levels in the mixing layer at each time step during the day; using
C **  the method of O'Brien.  Used in MOMDAY.

      REAL KTOP , KMA , KMAPRI , KMW(50)

      INCLUDE 'modvars.h'

      COMMON /MXDLYR/ THICK

      THICK = HGT - ZA

C **  If there is no mixing layer skip this routine to 520.

      IF ( THICK .EQ. 0 ) GO TO 520

C **  Define or calc eddy diff's (K's) at the top of the surface layer
C **  using the standard flux profile law.  Calculate the derivative
C **  of KMA and define K at the top of the mixing layer to be zero.

      KMA = KARMAN * USTAR * ZA * ( 1 - ( 15 * ZA / MOL ) )**0.25
      KM(1) = KMA
      KMW(1) = KMA
      KMAPRI = KMA * (1 / ZA - ( 15 / MOL ) / (1 - 15 * ZA / MOL))
      KTOP = 0

C **  Calc heights between the 250 metre intervals (ZI system) as passed
C **  from SPLINE, and call ZK system (Interval 50,175,425 etc).

      M = NTRP + 1
      DO 5 I = 2 , M
       ZK(I) = ZI(I) - 0.5 * DELTAZ
    5 CONTINUE
      ZK(1) = ZI(1)

C **  Calc the Eddy Diffusivities for momentum & water up to the top of
C **  the mixing layer using the O'Brien function.  If NRTP goes above
C **  HGT fill with zeros.

      DO 10 L = 2 , NTRP
       IF ( ZK(L) .LT. HGT ) THEN
	KM(L) = KTOP + ( ( ZK(L) - HGT)**2 / THICK**2 ) * ( KMA - KTOP +
     #	   ( ZK(L) - ZA ) * ( KMAPRI + 2 * ( KMA - KTOP ) / THICK ) )
	KMW(L) = KTOP + ( ( ZI(L) - HGT )**2 / THICK**2 ) * ( KMA - KTOP
     #	 + ( ZI(L) - ZA ) * ( KMAPRI + 2 * ( KMA - KTOP ) / THICK ) )
       ELSE
        KM(L)  = 0
        KMW(L) = 0
       END IF
   10 CONTINUE

C **  Smooth K's as a weighted average.

      DO 15 J = 2 , NTRP
       KM(J) = ( KMW(J) + KM(J) + KMW(J-1 ) ) / 3
   15 CONTINUE

C **  Label 520 ....... No mixing layer exists.

  520 CONTINUE

C **  Format statements

      RETURN
      END

