      SUBROUTINE  AIR (ZLS, YCOUNT)

C **  Subroutine air computes the daytime height of the mixing layer and
C **  the potential temperature at height ZA.

      Real ZLS(50)

$INCLUDE:'modvars.h'

      IFIRST = 1.0
		

C **  Signal daytime situation, IFIRST = 1.

C **  Select the correct Pot. Temp lapse rate.

      DO 5 J = 2 , 9
       IF ( HGT .GT. ZLS(J - 1) ) GAM = GM(J)
    5 CONTINUE

C **  CHGT is calculated here once only, based initially on a parameter-
C **  isation of Tennekes, before passing onto the ELSE statement.  Now
C **  mitre the mixing layer loop to 24 sec to increase accuracy, calc
C **  pot temp, temp at ZA and the mixing layer height over 240 sec.

      IF ( CHGT .EQ. 0 ) THEN
       HET = HEAT / ( DENS * CP )
       CHGT=(0.35*SQRT(0.3))*(((GRAV/OTEMP)**(1./3.))*(HGT**(1./3.))
     #      *(HET**(1./3.)))
       CDELT = ( GAM * HGT * CHGT - HET ) / HGT
       CTHETA = ( HET / HGT ) - RAD + ADVGT
      ELSE
       DELTX = DELTA / 10
       DO 10 I = 1 , 10
        APTEMP = ( CTHETA * DELTX ) + APTEMP
        ATEMP = APTEMP - Tdif_50
        DELT = ( CDELT * DELTX ) + DELT
        IF ( DELT .LT. 0.01 ) DELT = 0.01
        HGT = ( CHGT * DELTX ) + HGT
        HET = HEAT / ( DENS * CP )
        DHET = -0.5*HET/((1.+(2.6*(HET**(2./3.))/((GRAV*HGT/OTEMP)
     #         **(1./3.)*DELT))))
        CHGT = - DHET / DELT
        CTHETA = ( ( HET - DHET ) / HGT ) - RAD + ADVGT
        CDELT = ((GAM*HGT*CHGT)-(HET)-(DELT*CHGT))/HGT
   10  CONTINUE
      END IF

** 12/3/91 Sounding Profile for course

      td(1) = aptemp
      zmix = ZA
      do 20 l = 2, ntrp
	zmix = zmix + DELTAZ
        if (zmix .lt. hgt) then
          td(l) = aptemp
        endif
 20   continue

** tdel is at the height just above the mixing layer
** ttop is at the height of the mixing layer

       tdel = aptemp + cdelt
       ttop = aptemp

C **  YCOUNT advanced to set the mode definitely for the daytime.

      YCOUNT = YCOUNT + 1.

      RETURN
      END

