      SUBROUTINE  PSLCAL (SGMA, PES)

      REAL SGMA
      integer*1 init

$INCLUDE:'modvars.h'

      data init  /1/

c **  a program that finds leaf water potential analytically.

*/    17 June 1992 - b1_p and b2_p are defined differently from original
*/    Lynn and Carlson article. To allow RMIN to be a true scaling coeff
*/    for the Stomatal Resistance.

c **  Explanation of conversion factors
c **  gma = (p*cp)/(0.622*Le) = .66 mb c-1
c **  Leef = roe*le*v/(rl + raf)
c **  substitute for Le
c **  Leef = (roe*cp/gma) * v/(rl + raf)
c **  e = q*(P-e)/0.622  = q*p/0.622
c **  p is set equal to a ps1 (mbs) below
c **  note sensible flux = roe*cp*delta t /rs
c **  note conversion to latent flux is roe * cp * delta e/(rs * gma)
c **  roe =[kg/m***3]
c **  Cp = 1005 J/kg-k
C **  SET THV = W2G)

      THV = W2G

      CALL STOMFS

c **  following is Choudhury and Idso's adaptation for zg
c **  units is a conversion factor from s given in Choudhury to
c **  bar/(w/m**2).

      UNITS2 = 0.4E-10

c **  vegheight (h) is the height of the plants in meters
c **  zeff is the effective rooting depth taken as 1/2 h

      ZEFF = .5 * VEGHEIGHT
      CALL COND
      ZG = ( 0.0013 / ( ZEFF * RKW ) ) * UNITS2
      ZTOT = ZP + ZG

C **  Compute the total soil-root and root-xylem resistance.
C **  Frht is fraction of height below the intersection point
C **  of the storage area with the xylem
C **  FPM represents the height of the tree above the
C **  intersection point.


c **  Following is the adaptation of FW using the weight of the column
c **  of water after Federer (1982)
c **  This makes an adjustment for the height of the column.
c **  the density of water is 1000kg/m**3
c **  the acceleartion of gravity is 9.8m/s**2
c **  we need the average height of the plant or the weighted mean
c **  height which is the displacement or the geometric height.  The
c **  The displacement height is set at 0.67 times the height of the
c **  vegetation.
c **  units1 is a conversion factor from kg/s**2 to bars

      b1_p = B1*RMIN
      b2_p = B2*RMIN

      HBAR = VEGHEIGHT * 0.67
      UNITS1 = 1E-5
      H = RHOW * GRAV * HBAR * UNITS1
      Unscaled_RAF = RAF * XLAI / PES ! Per Leaf Area

      CALL PSGCAL

      CALL STOMC

      if (init .eq. 1) then
       vfl2 = vfl
       init = 2
      else
       Q_boundary = (qstf/rst + qaf/raf) / (1/raf + 1/rst)
       vfl2 = (qstf - Q_boundary) * ps1/0.622
      endif

      IF (STEADY .EQ. 'N') THEN ! capacitance solution

	call capac (Unscaled_RAF,H,B1_P,B2_P,vfl2)

      ELSE ! Steady State (non-capacitance = Yes) solution

	 RSTDIV =  Unscaled_RAF + (RCUT * RSCRIT) / (RCUT + RSCRIT)
	 PSIWC = SGMA * VFL * ZTOT / RSTDIV
     #		+  PSICE + BETA * vfl2 + H

	 PSISUP = PSIG

       IF ( PSISUP .GT. PSIWC ) THEN

	AROOT = FS * FT * b1_p * (RCUT + Unscaled_RAF) *  (-1)

	BROOT =   FS * FT * (RCUT + Unscaled_RAF) * ( -RMIN
     #		+ b1_p * ( PSIG - BETA * vfl2 - H
     #		- ZTOT * SGMA * VFL / ( RCUT + Unscaled_RAF) ) )
     #		- RCUT * Unscaled_RAF


	CROOT =   FS * FT * (RCUT + Unscaled_RAF) * RMIN * ( PSIG
     #		- BETA * vfl2 - H - ZTOT * SGMA * VFL
     #		/ ( RCUT + Unscaled_RAF ) )
     #		+ RCUT * Unscaled_RAF * ( PSIG - BETA * vfl2 - H )
     #          - RCUT * ZTOT * SGMA * VFL

       ELSE

	AROOT = FS * FT * b2_p * ( RCUT + Unscaled_RAF )

	BROOT =   FS * FT *  ( RCUT + Unscaled_RAF ) * ( -1 * ( RMIN
     #          + b1_p * PSICE + b2_p * PSICE)
     #		- b2_p * ( PSIG - BETA * vfl2 - H - ZTOT * SGMA
     #		  * VFL / (RCUT + Unscaled_RAF) ) )
     #		- RCUT * Unscaled_RAF

	CROOT =   FS * FT* (RCUT + Unscaled_RAF) * (RMIN + b1_p * PSICE
     #		+ b2_p * PSICE ) * ( PSIG - BETA * vfl2 - H
     #		- ZTOT * SGMA * VFL / ( RCUT + Unscaled_RAF ) )
     #		+ RCUT * Unscaled_RAF * ( PSIG - BETA * vfl2 - H )
     #          - RCUT * ZTOT * SGMA * VFL

       END IF

       ACTSQRT  = BROOT ** 2 - 4 * AROOT * CROOT
       PSIE = ( -BROOT - SQRT ( ACTSQRT ) ) / (2 * AROOT )

       PSIM = PSIE + BETA * vfl2
       CALL STOMRS

       END IF  ! End of STEADY Loop

C * * STRESS INDICES

          WPSI = PSIWC - PSISUP
          RLPSI = PSICE - PSIE


      RETURN
      END
