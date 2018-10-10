subroutine GTEMP
  use simsphere_mod
  implicit none

! **  GTEMP determines the surface temperature in the absence of
! **  turbulence. It sets up an energy balance equation as a fourth-
! **  order polynomial equation in surface temperature and solves it
! **  by Newton's iteration algorithm.  Note that GTEMP assumes
! **  radiative balance.

  real :: A(5)
  real :: ENGBAL, A1, B, C, DELTAX, XDIF, X

  integer :: I, J, K

!      INCLUDE 'modvars.h'

  X = OTEMP

! **  Set up the polynomial.

  ENGBAL = (AEPSI * SIGMA * (T_fine(3) - Tdif_s - 1.5)**4               &
           + SWAVE - HEAT - EVAP)* Z(2) + LAMBDA*TT(2)
  A1 = Z(2) * SIGMA * EPSI
  A(1) = -ENGBAL
  A(2) = LAMBDA
  A(3) = 0
  A(4) = 0
  A(5) = A1

! **  Reduce the boundaries until the difference is < .0001.

  do J = 1 , 20
     B = A(5)
     C = A(5)
    do I = 1 , 3
      K = 5 - I
      B = A(K) + X * B
      C = B + X * C
    end do
    B = A(1) + X * B
    DELTAX = B / C
    X = X - DELTAX
    XDIF = X - OTEMP
    IF ( ABS(XDIF) < .0001 ) GO TO 31
  end do

31 X = ( OTEMP + X ) / 2

  OTEMP = X
  return
end
