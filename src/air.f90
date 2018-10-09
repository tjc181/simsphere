subroutine  AIR (ZLS, YCOUNT)
  use simsphere_mod, only: hgt, gam, gm, het, heat, otemp, advgt, dens, cp, &
                           aptemp, atemp, tdif_50, delt, dhet, td, ntrp,    &
                           rad, grav, delta, za, ifirst, deltaz, eq
  implicit none

! **  Subroutine air computes the daytime height of the mixing layer and
! **  the potential temperature at height ZA.

  real :: ZLS(50)
  real :: CHGT = 0.0
  real :: CTHETA = 1.0
  real :: CDELT, DELTX, zmix, tdel, ttop, YCOUNT
  integer :: I, J, L

!      INCLUDE 'modvars.h'

  IFIRST = 1.0
  
! **  Signal daytime situation, IFIRST = 1.

! **  Select the correct Pot. Temp lapse rate.

  do J = 2 , 9
    IF ( HGT > ZLS(J - 1) ) GAM = GM(J)
  end do

! **  CHGT is calculated here once only, based initially on a parameter-
! **  isation of Tennekes, before passing onto the ELSE statement.  Now
! **  mitre the mixing layer loop to 24 sec to increase accuracy, calc
! **  pot temp, temp at ZA and the mixing layer height over 240 sec.

  IF ( eq(CHGT,0.0) ) THEN
    HET = HEAT / ( DENS * CP )
    CHGT=(0.35*SQRT(0.3))*(((GRAV/OTEMP)**(1./3.))*(HGT**(1./3.)) *(HET**(1./3.)))
    CDELT = ( GAM * HGT * CHGT - HET ) / HGT
    CTHETA = ( HET / HGT ) - RAD + ADVGT
  ELSE
    DELTX = DELTA / 10
    do I = 1 , 10
      APTEMP = ( CTHETA * DELTX ) + APTEMP
      ATEMP = APTEMP - Tdif_50
      DELT = ( CDELT * DELTX ) + DELT
      IF ( DELT < 0.01 ) DELT = 0.01
      HGT = ( CHGT * DELTX ) + HGT
      HET = HEAT / ( DENS * CP )
      DHET = -0.5*HET/((1.+(2.6*(HET**(2./3.))/((GRAV*HGT/OTEMP)**(1./3.)*DELT))))

      CHGT = - DHET / DELT
      CTHETA = ( ( HET - DHET ) / HGT ) - RAD + ADVGT
      CDELT = ((GAM*HGT*CHGT)-(HET)-(DELT*CHGT))/HGT
    end do
  END IF

!** 12/3/91 Sounding Profile for course

  td(1) = aptemp
  zmix = ZA
  do l = 2, ntrp
    zmix = zmix + DELTAZ
    if (zmix < hgt) then
      td(l) = aptemp
    endif
  end do

! ** tdel is at the height just above the mixing layer
! ** ttop is at the height of the mixing layer

  tdel = aptemp + cdelt
  ttop = aptemp

! **  YCOUNT advanced to set the mode definitely for the daytime.

  YCOUNT = YCOUNT + 1.


  RETURN
END
