       Subroutine Capac (Unscaled_RAF,H,B1_P,B2_P,vfl2)

       INCLUDE 'modvars.h'

       ZADD = ZG + ZP * FRZP
       FPM = 1 - FRHGT
       ZUH = ZP * (1-FRZP)

c **  below there are two solutions for psist. The first is the
c **  log, while the second is a regular integral based on volume
c **  to a power.  There are also two similar solutions for zst
c **  and capacitance.
C **  Note also that psist, psist and psix are a time step behind because
c **  psist is not allowed to exceed psix of the previous time
c **  step (psix is a function of psis).
c **  There are also to ways to set the volume of water in
c **  storage. If xcap is set equal to zero, then volist storage is a
c **  function of psig. If not, it is a function of xcap or
c **  relative water content

      IF ( JCAP .EQ. 1 ) THEN

	NCAP = RCCAP
	IDEL = 0
	JDEL = 0
	PSIX = PSIG - FRHGT * H
	IXCAP = VOLREL
	VOLREL = .01 * VOLREL
	CAPINI = VOLINI / RKOCAP
	JCAP = 2

       IF ( IXCAP .EQ. 0 ) THEN

	IF ( NCAP .EQ. 1 ) THEN

	     VOLIST = VOLINI * EXP ( CAPINI * (PSIG - FRHGT * H)
     #		     / VOLINI )

	     CAPACI = CAPINI * ( VOLIST ) / VOLINI

	ELSE IF ( NCAP .NE. 1 ) THEN

	     CAPRAT =  (RCCAP - 1)/ RCCAP
	     VOLIST = VOLINI * ( 1 + CAPINI * (PSIG - FRHGT * H)
     #		      * CAPRAT / VOLINI ) ** ( 1 / CAPRAT )

	     CAPACI = CAPINI * ( ( VOLIST ) / VOLINI ) **
     #		      ( 1 / RCCAP )

	END IF

C ** It is possible that the volist will be calculated as being
C ** less than zero.  This is impossible.  Therefore, we set:

	IF ( VOLIST .LT. 0 ) THEN

	   VOLIST = .00001*VOLINI
	   VOLRMV = .99999*VOLINI

	END IF

       ELSE

	VOLIST = VOLREL * VOLINI
	VOLISO = VOLIST

	IF ( NCAP .EQ. 1 ) THEN

	     CAPACI = CAPINI *	VOLREL

	ELSE IF ( NCAP .NE. 1 ) THEN

	     CAPRAT =  (RCCAP - 1)/ RCCAP
	     CAPACI = CAPINI * ( VOLREL ) ** ( 1 / RCCAP )

	END IF

       END IF

      END IF

      VOLREL = VOLIST / VOLINI

      IF ( NCAP .EQ. 1 ) THEN

	  PSIST = ( VOLINI / CAPINI ) * LOG ( VOLREL )
	  CAPACI = CAPINI * ( VOLIST ) / VOLINI

      ELSE

	  PSIST = (VOLINI/CAPINI) * ( VOLREL ** CAPRAT	- 1 )
     #		   * (RCCAP /( RCCAP - 1 ) )

C **  THIS IF STATEMENT IS IN PLACE OF VOLUME CONSTRAINTS

	  IF ( PSIST .GE. 0 ) THEN

	       PSIST = 0

	  END IF

c **  The following if statement causes psist to fall as volume
c **  approaches zero (the above equation reaches a limit).
c **  (ITRAP is defined below to prevent the loop from being
c **   exercised on the first time step.)
C **   The next statement prevents psist from becoming more
C	positive then psig

	  IF ( ITRAP .EQ. 1 .AND. DELTVST .EQ. 0 ) THEN

	       PSIST = PSIX

	  END IF

	     CAPACI = CAPINI * ( VOLREL ) ** ( 1 / RCCAP)

      END IF

	  IF ( NCAP .EQ. 1 ) THEN

	      ZST = ZSTINI / VOLREL

	  ELSE IF ( NCAP .NE. 1 ) THEN

	      ZST = ZSTINI * ( 1./VOLREL ) ** ( RZCAP )

	  END IF

c **  The following if statement prevents zst from becoming very
c**   large.

      IF ( ZST .GT. 1E3 ) THEN

	 ZST = 1E3

      END IF

c **  The following if statements prevent psist from exceeding
c **  psix during a time interval

      IF ( DELTVST .LT. 0 .AND. PSIST .LT. PSIX ) THEN

	  PSIST = PSIX

      ELSE IF ( DELTVST .GT. 0 .AND. PSIST .GT. PSIX ) THEN

	  PSIST = PSIX

      END IF


      RSTDIV =	Unscaled_RAF + ( RCUT * RSCRIT ) / ( RCUT + RSCRIT )

c **  we need to compare psig to psigc before proceeding
C **  we can determine this by solving for psig given various
C **  parameters.

C **   ADDIT  is an additional term that incorporates storage flux.

      ADDIT = 1 + ZUH / ZADD + ZUH / ZST

c**    define a critical water potential. As the flux
c **   from storage approaches zero, it approaches that of
c **   the soil.

      PSIWC =  SGMA * VFL * ZUH / RSTDIV
     #	       + PSICE + beta * vfl2 + FPM * H

      PSISUP = PSIX

      IF ( PSISUP .GT. PSIWC ) THEN

       AROOT = FS * FT * b1_p * (RCUT + Unscaled_RAF) * ( ZST * (-1)
     #		- ZADD )

       BROOT = FS * FT * (RCUT + Unscaled_RAF) * ( RMIN *
     #		 ( - ZST - ZADD ) + b1_p * ( ZST * ( PSIG - BETA *
     #		 vfl2 - H ) + ZADD * (PSIST - beta * vfl2 - FPM * H )
     #	       - ZADD * ZST * SGMA * VFL * ADDIT /
     #		 ( RCUT + Unscaled_RAF) ) )
     #	       + RCUT * Unscaled_RAF * ( - ZST - ZADD )


       CROOT = FS * FT * (RCUT + Unscaled_RAF) * ( RMIN * ( ZST
     #	       * (PSIG - beta * vfl2 - H ) + ZADD * ( PSIST
     #	       - beta * vfl2 - FPM * H )
     #	       - ZADD * ZST * SGMA * VFL * ADDIT / (RCUT + Unscaled_RAF)
     #		 ) )
     #	       + RCUT * Unscaled_RAF * ( ZST * (PSIG - beta * vfl2 - H )
     #	       + ZADD * ( PSIST - beta * vfl2 - FPM * H ) )
     #	       - RCUT * ZADD * ZST * SGMA * VFL * ADDIT

      ELSE

       AROOT = FS * FT * b2_p * ( RCUT + Unscaled_RAF ) *
     #		 ( ZST + ZADD )

       BROOT = FS * FT *  ( RCUT + Unscaled_RAF ) * ( ( RMIN
     #	       + b1_p * PSICE + b2_p * PSICE) * ( -ZST - ZADD )
     #	       - b2_p * ( ZST * (PSIG - beta * vfl2 - H )
     #	       + ZADD * ( PSIST - beta * vfl2 - FPM * H )
     #	       - ZADD * ZST * SGMA * VFL * ADDIT /
     #		 (RCUT + Unscaled_RAF) ) )
     #	       + RCUT * Unscaled_RAF * ( - ZST - ZADD )

       CROOT = FS * FT* (RCUT + Unscaled_RAF) * ( RMIN + b1_p * PSICE
     #	       + b2_p * PSICE ) * ( ZST * (PSIG - beta * vfl2 - H )
     #	       + ZADD * (PSIST - beta * vfl2 - FPM * H )
     #	       - ZADD * ZST * SGMA * VFL * ADDIT /(RCUT + Unscaled_RAF))
     #	       + RCUT * Unscaled_RAF * ( ZST * ( PSIG - beta * vfl2 - H)
     #	       + ZADD * (PSIST - beta * vfl2 - FPM * H ) )
     #	       - RCUT * ZADD * ZST * SGMA * VFL * ADDIT

      END IF

      ACTSQRT  = BROOT ** 2 - 4 * AROOT * CROOT
      PSIE = ( -BROOT - SQRT ( ACTSQRT ) ) / (2 * AROOT )

      CALL STOMRS


C * * STRESS INDICES

      PSIM = PSIE + beta * vfl2
      PES = XLAI/2 + 1
      RLELF = SGMA * VFL / (RS*RCUT/(RS + RCUT) + Unscaled_RAF)
      PSIX = RLELF  * ZUH + PSIM + FPM * H
      FST = -( PSIST - PSIX ) / ZST
      DELTVST = -DELTA * ( PSIST - PSIX ) / (ZST * LE
     #	   * RHOW )
      FLUXGD = ( PSIG - PSIX - FRHGT * H ) / ZADD
      VOLRMO = VOLRMV
      VOLIST = VOLIST + DELTVST
      VOLRMV = VOLINI - VOLIST

c **   the following if statements prevent the volume in
c **   storage from becoming negative.

      IF ( VOLRMV .GT. VOLINI .AND. DELTVST .LE. 0 ) THEN

	   IF ( IDEL .EQ. 0 ) THEN

	      DELTVST = (VOLINI - VOLRMO)
	      IDEL = 1

	   ELSE

	      DELTVST = 0.0
	      ITRAP = 1

	   END IF

	   VOLRMV = .99999*VOLINI
	   VOLIST = .00001*VOLINI

      END IF

      return
      end
