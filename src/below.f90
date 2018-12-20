subroutine  BELOW (TIME,BareRadioTemp,BareEvapFlux)
  use simsphere_mod
  implicit none


! **  Below is called every time step (N+1) to update the sub-surface
! **  temp's. The lowest level has a constant temp BTEMP, whereas the
! **  the surface temp has been found previously in FLUX. Use of the
! **  leap-frog method computes sub-surface temp's at the next time
! **  step from those at the current and the previous time step. BELOW
! **  also calls WATER to update the sub-surface soil moisture status.

      real :: TE(9)=0.0 , TTT(9)=0.0 , DTDT(8)=0.0
      real :: TIME, BareRadioTemp, BareEvapFlux
      real :: TERM1=0.0, TERM2=0.0, TERM3=0.0
      integer :: NLVL1=0, I=0, K=0, dummy=0

!      INCLUDE 'modvars.h'

! **  TT(2) is the temperature at the first level below the soil.

      NLVL1 = NLVLS + 1

! **  Use the fraction of vegetation to set the boundary conditions
! **  for the 1st level in the ground (surface).

      IF ( FRVEG == 0 ) THEN
         TT(1) = OTEMP
      ELSE IF (FRVEG > 0 .and. FRVEG < 1) THEN
         TT(1) = (TG**4*FRVEG + BareRadioTemp**4 * (1-Frveg))**0.25
      ELSE IF (FRVEG == 1) THEN
         TT(1) = TG
      ELSE
!   PRINT*, ' Error ... Fraction of vegetation outwith bounds '
!   STOP
   continue
      END IF

      IF (HEAT < 0.0 .or. RNET < 0) THEN
          TT(1) = OTEMP
      END IF

! **  K refers to level, K=1 being the surface.

      IF (TIME == 0) THEN

       DO 5 I = 1 , NLVL1
        TE(I) = TT(I)
    5  CONTINUE

      END IF

! **  TT is at time N, TE is at time N-1, TTT is at time N+1
! **  Here we represent the diffusion term ... see manual for
! **  derivation.

      DO 10 K = 2 , NLVLS

       TERM1 = KAPPA / ( XFUN(K)**2 * DEL**2 )
       TERM2 = ( TT(K+1) - 2 * TE(K) + TT(K-1) ) / DZETA**2
       TERM3 = ( TE(K+1) - TT(K-1) ) / 2 * DZETA

       DTDT(K) = TERM1 * ( TERM2 - TERM3 )

        IF (TIME == 0) THEN

! **  Initial computation of TT(K) and redefinition for the future.

         TTT(K) = TE(K) + DELTA * DTDT(K)
         TT(K) = TTT(K)

       ELSE

! **  Computation of TTT(K) via leapfrog rule, along with redefinition
! **  for the future.

        TTT(K) = TE(K) + 2 * DELTA * DTDT(K)
        TE(K) = TT(K)
        TT(K) = TTT(K)

       END IF

   10 CONTINUE

 if (ptime == 16) then
 dummy = 1
 endif

! **  Skip the substrate water component if WMAX > 1.

      IF (WMAX > 1) THEN

       W2G = 99.99
       WGG = 99.99

      ELSE

         CALL WATER (TIME,BareEvapFlux)
         
      END IF

      RETURN
      END
