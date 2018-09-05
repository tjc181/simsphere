      SUBROUTINE  BELOW (TIME,BareRadioTemp,BareEvapFlux)
  use simphere_mod


C **  Below is called every time step (N+1) to update the sub-surface
C **  temp's. The lowest level has a constant temp BTEMP, whereas the
C **  the surface temp has been found previously in FLUX. Use of the
C **  leap-frog method computes sub-surface temp's at the next time
C **  step from those at the current and the previous time step. BELOW
C **  also calls WATER to update the sub-surface soil moisture status.

      REAL TE(9) , TTT(9) , DTDT(8)

!      INCLUDE 'modvars.h'

C **  TT(2) is the temperature at the first level below the soil.

      NLVL1 = NLVLS + 1

C **  Use the fraction of vegetation to set the boundary conditions
C **  for the 1st level in the ground (surface).

      IF ( FRVEG .EQ. 0 ) THEN
         TT(1) = OTEMP
      ELSE IF ( FRVEG .GT. 0 .AND. FRVEG .LT. 1) THEN
         TT(1) = (TG**4*FRVEG + BareRadioTemp**4 * (1-Frveg))**0.25
      ELSE IF ( FRVEG .EQ. 1 ) THEN
         TT(1) = TG
      ELSE
c	  PRINT*, ' Error ... Fraction of vegetation outwith bounds '
c	  STOP
	  continue
      END IF

      IF (HEAT .LT. 0.0 .OR. RNET .LT. 0) THEN
          TT(1) = OTEMP
      END IF

C **  K refers to level, K=1 being the surface.

      IF ( TIME .EQ. 0 ) THEN

       DO 5 I = 1 , NLVL1
        TE(I) = TT(I)
    5  CONTINUE

      END IF

C **  TT is at time N, TE is at time N-1, TTT is at time N+1
C **  Here we represent the diffusion term ... see manual for
C **  derivation.

      DO 10 K = 2 , NLVLS

       TERM1 = KAPPA / ( XFUN(K)**2 * DEL**2 )
       TERM2 = ( TT(K+1) - 2 * TE(K) + TT(K-1) ) / DZETA**2
       TERM3 = ( TE(K+1) - TT(K-1) ) / 2 * DZETA

       DTDT(K) = TERM1 * ( TERM2 - TERM3 )

        IF ( TIME .EQ. 0 ) THEN

C **  Initial computation of TT(K) and redefinition for the future.

         TTT(K) = TE(K) + DELTA * DTDT(K)
         TT(K) = TTT(K)

       ELSE

C **  Computation of TTT(K) via leapfrog rule, along with redefinition
C **  for the future.

        TTT(K) = TE(K) + 2 * DELTA * DTDT(K)
        TE(K) = TT(K)
        TT(K) = TTT(K)

       END IF

   10 CONTINUE

	if(ptime .eq. 16) then
	dummy = 1
	endif

C **  Skip the substrate water component if WMAX > 1.

      IF ( WMAX .GT. 1 ) THEN

       W2G = 99.99
       WGG = 99.99

      ELSE

         CALL WATER (TIME,BareEvapFlux)
         
      END IF

      RETURN
      END
