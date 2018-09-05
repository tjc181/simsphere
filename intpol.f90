      SUBROUTINE  INTPOL
  use simsphere_mod

c      CHARACTER*1 CHOICE

!      INCLUDE 'modvars.h'

      DATA ZZ,ZZDAY /50,250/

      VGD(1) = VGS
      UGD(1) = UGS
 
C **  Set the geostrophic wind component equal winds at 1050m and above.
 
       DO 15 J=5,NTRP
        VGD(J) = VD(J)
        UGD(J) = UD(J)
   15  CONTINUE
 
C **   Make the winds above NTRP levels equal that at the NTRP level.
C **   Trap so that the winds are not zeroed above NTRP levels.
 
       DO 20 I = NTRP,20
        VGD(I) = VGD(NTRP)
        UGD(I) = UGD(NTRP)
   20  CONTINUE
 
C **   Set up a weigthing factor to interpolate from 50 to 1050 m.
C **   Calc vertical derivatives for U & V.
 
       FIX = 4 * ZZDAY + 50
       VTDZ = (VD(5)-VGS)/FIX
       UTDZ = (UD(5)-UGS)/FIX
 
C **   Calc geostrophic wind components in intermediate levels.
 
       DO 25 I=1,4
        TIX = FLOAT(I-1)*ZZDAY+50
        UGD(I) = UGS + UTDZ * TIX
        VGD(I) = VGS + VTDZ * TIX
   25  CONTINUE
 
C **   Vertical derivative for the night-time.
 
       UTDZ = (UD(3)-UGS)/550
       VTDZ = (VD(3)-VGS)/550
 
C **   Follow procedure again for the night-time (50 - 500m).
 
       DO 30 I=1,20
        TIX = FLOAT(I)*ZZ
        UG(I) = UGS+UTDZ*TIX
        VG(I) = VGS+VTDZ*TIX
   30  CONTINUE

       do 40 i=21,46
        ug(i) = u_fine(i)
        vg(i) = v_fine(i)
   40  continue 

      RETURN
      END
