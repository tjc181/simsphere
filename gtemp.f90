      SUBROUTINE  GTEMP
  use simsphere_mod

C **  GTEMP determines the surface temperature in the absence of
C **  turbulence. It sets up an energy balance equation as a fourth-
C **  order polynomial equation in surface temperature and solves it
C **  by Newton's iteration algorithm.  Note that GTEMP assumes
C **  radiative balance.

      REAL A(5)

!      INCLUDE 'modvars.h'

      X = OTEMP

C **  Set up the polynomial.

      ENGBAL = (AEPSI * SIGMA * (T_fine(3) - Tdif_s - 1.5)**4
     #	       + SWAVE - HEAT - EVAP)* Z(2) + LAMBDA*TT(2)
      A1 = Z(2) * SIGMA * EPSI
      A(1) = -ENGBAL
      A(2) = LAMBDA
      A(3) = 0
      A(4) = 0
      A(5) = A1

C **  Reduce the boundaries until the difference is .LT. .0001.

      DO 5 J = 1 , 20
       B = A(5)
       C = A(5)
       DO 10 I = 1 , 3
        K = 5 - I
        B = A(K) + X * B
        C = B + X * C
   10  CONTINUE
       B = A(1) + X * B
       DELTAX = B / C
       X = X - DELTAX
       XDIF = X - OTEMP
       IF ( ABS(XDIF) .LT. .0001 ) GO TO 31
    5 CONTINUE

   31 X = ( OTEMP + X ) / 2

      OTEMP = X

      RETURN
      END
