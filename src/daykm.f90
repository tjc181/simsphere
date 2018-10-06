subroutine  DAYKM
  use simsphere_mod
  implicit none

! **  This routine computes eddy diffusivities as a function of height,
! **  friction velocity and Monin Obukhov length for all the relevant
! **  levels in the mixing layer at each time step during the day; using
! **  the method of O'Brien.  Used in MOMDAY.

  real :: KTOP , KMA , KMAPRI , KMW(50)
  real :: THICK

  integer :: I, J, L, M

!      INCLUDE 'modvars.h'

  COMMON /MXDLYR/ THICK

  THICK = HGT - ZA

! **  If there is no mixing layer skip this routine to 520.
! This conditional should wrap the entire subroutine up to label 520
! allowing for negative comparison (THICK .NE. 0) with the false case
! branching to the same position as label 520 (eliminate the GOTO).

  IF ( THICK .EQ. 0 ) GO TO 520

! **  Define or calc eddy diff's (K's) at the top of the surface layer
! **  using the standard flux profile law.  Calculate the derivative
! **  of KMA and define K at the top of the mixing layer to be zero.

  KMA = KARMAN * USTAR * ZA * ( 1.0 - ( 15.0 * ZA / MOL )**0.25)
  KM(1) = KMA
  KMW(1) = KMA
  KMAPRI = KMA * (1.0 / ZA - ( 15.0 / MOL ) / (1.0 - 15.0 * ZA / MOL))
  KTOP = 0.0

! **  Calc heights between the 250 metre intervals (ZI system) as passed
! **  from SPLINE, and call ZK system (Interval 50,175,425 etc).

  M = NTRP + 1
  do I = 2 , M
    ZK(I) = ZI(I) - 0.5 * DELTAZ
  end do
  ZK(1) = ZI(1)

! **  Calc the Eddy Diffusivities for momentum & water up to the top of
! **  the mixing layer using the O'Brien function.  If NRTP goes above
! **  HGT fill with zeros.

  do L = 2 , NTRP
    IF ( ZK(L) .LT. HGT ) THEN
      KM(L) = KTOP + ( ( ZK(L) - HGT)**2 / THICK**2 ) * ( KMA - KTOP +  &
              ( ZK(L) - ZA ) * ( KMAPRI + 2 * ( KMA - KTOP ) / THICK ) )
      KMW(L) = KTOP + ( ( ZI(L) - HGT )**2 / THICK**2 ) * ( KMA - KTOP  &
               + ( ZI(L) - ZA ) * ( KMAPRI + 2 * ( KMA - KTOP ) / THICK ) )
    ELSE
      KM(L)  = 0
      KMW(L) = 0
    END IF
  end do

! **  Smooth K's as a weighted average.

  do J = 2 , NTRP
    KM(J) = ( KMW(J) + KM(J) + KMW(J-1 ) ) / 3
  end do

! **  Label 520 ....... No mixing layer exists.

  520 CONTINUE

! **  Format statements

  return
end
