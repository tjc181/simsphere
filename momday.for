      SUBROUTINE  MOMDAY

C **  This daytime routine updates the daytime winds UD, VD and specific
C **  humidity QD using the eddy diff's obtained in DAYKM, assuming
C **  similarity between humidity and momentum transfer coefficients.
C **  Note that the time step is 120 sec ..this ensures computational
C **  stability during periods when KM values are large.

      REAL DUDT(50) , DVDT(50) , DU(50) , DV(50)
*/	MAG(50), DIRCT(50)
*/	CHARACTER *12 MENUCH

$INCLUDE:'modvars.h'

      COMMON /MXDLYR/ THICK

      DATA SMFH , UIF / 60 , 1 /

C **  SMF AND THICK is to deal with the eddy equations when the
C **  mixing layer is below 60m.

      SMF = THICK / SMFH
      IF ( SMF .GT. 1 ) SMF = 1

      AWIND = SQRT ( VD(1)**2 + UD(1)**2 )

      XXX = 0

C **  DELTZI is DELTAZ for the average of the two lowest layers
C **  for finite differencing of bottom boundary in diffusion calcs.

      DELTZI = ( DELTAZ + 50 ) / 2

C **  Check for the existance of a mixing layer.

666   IF ( THICK .EQ. 0 ) THEN

C **  Calc the partial derivatives of U and V with respect to time
C **  when there is no mixing layer.  Initialise specific humidities.

C **  If initialisation of actual wind profile and geostrophic winds do
C **  not 'match' properly the model will crash above the height of the
C **  mixed layer due to inertial occilation.  If necessary one may take
C **  out this feature above the height of the planetry boundary layer.

       DUDT50 = CF * ( VD(1) - VGD(1) ) - ( SMF * ( USTAR**2 *
     #		( UD(1) / AWIND ) / 25 ) )
       DVDT50 = -CF * ( UD(1) - UGD(1) ) - ( SMF * ( USTAR**2 *
     #		( VD(1) / AWIND ) / 25 ) )
       DV50 = DVDT50 * DELTA
       DU50 = DUDT50 * DELTA
       VD(1) = VD(1) + DV50
       UD(1) = UD(1) + DU50
       DQDT2(1) = 0

       DO 20 I = 2 , NTRP
        DUDT(I) = CF * ( VD(I) - VGD(I) )
        DVDT(I) = -CF * ( UD(I) - UGD(I) )
        DU(I) = DUDT(I) * DELTA
        DV(I) = DVDT(I) * DELTA
        UD(I) = UD(I) + DU(I)
        VD(I) = VD(I) + DV(I)
        DQDT2(I) = 0
   20  CONTINUE

       GO TO 999

      ELSE

C **  Set up contingences to check if near top of mixing layer.

       DO 30 I = 2 , NTRP

        DUDT(I) = CF * ( VD(I) - VGD(I) )
        DVDT(I) = -CF * ( UD(I) - UGD(I) )

c	IF ( ZI(I) .GT. HGT ) THEN
c	 TD(I) = TD(I) - (RAD - ADVGT ) * (DELTA/2)
c	ENDIF

	IF ( ZI(I) .GT. HGT ) GO TO 40

        IF ( ZI(I) .EQ. ZI(NTRP) ) THEN
         KM(I+1) = 0
         UD(I+1) = UD(I)
         VD(I+1) = VD(I)
        END IF

C **  Calc rate of change of Q,U & V with respect to time due to the
C **  diffusive properties within the layer.

        DQDT2(I) = ( ( KM(I+1) * ( QD(I+1) - QD(I) ) / DELTAZ ) -
     #             ( KM(I) * ( (QD(I) - QD(I-1) ) / DELTAZ ) ) ) /
     #             DELTAZ

        DUDT(I) = DUDT(I) + ( ( KM(I+1) * ( UD(I+1) - UD(I) ) / DELTAZ )
     #            - ( KM(I) * ( UD(I) - UD(I-1) ) / DELTAZ ) ) / DELTAZ

        DVDT(I) = DVDT(I) + ( ( KM(I+1) * ( VD(I+1) - VD(I) ) / DELTAZ )
     #            - ( KM(I) * ( VD(I) - VD(I-1) ) / DELTAZ ) ) / DELTAZ

   40   CONTINUE

        DU(I) = DUDT(I) * ( DELTA / 2 )
	DV(I) = DVDT(I) * ( DELTA / 2 )
        QD(I) = QD(I) + ( DQDT2(I) * DELTA / 2 )

   30 CONTINUE

C **  At lower boundary smooth out the derivatives using classical
C **  functional form.

       DUDT(1) = CF * ( VD(1) - VGD(1) ) + ( KM(2) * ( UD(2) - UD(1) )
     #		 / ( DELTAZ * DELTZI ) ) - ( SMF * ( UIF * USTAR**2 *
     #		 ( UD(1) / AWIND ) / DELTZI ) )
       DVDT(1) = -CF * ( UD(1) - UGD(1) ) + ( KM(2) * ( VD(2) - VD(1) )
     #		 / ( DELTAZ * DELTZI ) ) - ( SMF * ( UIF * USTAR**2 *
     #		 ( VD(1) / AWIND ) / DELTzi ) )
       DQDT2(1) = ( KM(2) * ( QD(2) - QD(1) ) / ( DELTAZ * DELTZI ) )
     #		  + ( EVAP * SMF * UIF ) / ( LE * DENS * DELTZI )

       DU(1) = DUDT(1) * ( DELTA / 2 )
       DV(1) = DVDT(1) * ( DELTA / 2)
       QD(1) = QD(1) + ( DQDT2(1) * DELTA / 2 )

C **  Compute the new values for U and V components.

       DO 50 J = 1 , NTRP
        UD(J) = UD(J) + DU(J)
        VD(J) = VD(J) + DV(J)
   50  CONTINUE

      END IF

C **  Windspeed at level one.

       AWIND = SQRT( UD(1)**2 + VD(1)**2 )

C **  Cycle through twice.

       XXX = XXX + 1
       OVER = AMOD( XXX , 2.0)
       IF ( OVER .NE. 0 ) GO TO 666

999   continue

*/ Now update the Fine Mesh Arrays

      call fine

C **  This code will be used with graphic interface .. leave for now.

C     IF (TMOD .NE. 0) THEN

C **  Calculate the magnitude and direction of the wind at each level.

C      DO 60 N=1,NTRP
C       MAG(N) = SQRT ( UD(N)**2 + VD(N)**2 )
C       DIRCT(N) = ATAN2 (UD(N),VD(N)) * (180./3.14159)+180.0
C  60  CONTINUE

C     END IF

      RETURN
      END

