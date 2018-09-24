subroutine  INTPOL
  use simsphere_mod
  implicit none


  real, parameter :: ZZ = 50 
  real, parameter :: ZZDAY = 250
  real :: FIX, VTDZ, UTDZ, TIX

  integer :: I, J

!      CHARACTER*1 CHOICE

!      INCLUDE 'modvars.h'

!  DATA ZZ,ZZDAY /50,250/

  VGD(1) = VGS
  UGD(1) = UGS
 
! **  Set the geostrophic wind component equal winds at 1050m and above.
 
  do J=5,NTRP
    VGD(J) = VD(J)
    UGD(J) = UD(J)
  end do
 
! **   Make the winds above NTRP levels equal that at the NTRP level.
! **   Trap so that the winds are not zeroed above NTRP levels.
 
  do I = NTRP,20
    VGD(I) = VGD(NTRP)
    UGD(I) = UGD(NTRP)
  end do
 
! **   Set up a weigthing factor to interpolate from 50 to 1050 m.
! **   Calc vertical derivatives for U & V.
 
  FIX = 4 * ZZDAY + 50
  VTDZ = (VD(5)-VGS)/FIX
  UTDZ = (UD(5)-UGS)/FIX
 
! **   Calc geostrophic wind components in intermediate levels.
 
  do I=1,4
    TIX = FLOAT(I-1)*ZZDAY+50
    UGD(I) = UGS + UTDZ * TIX
    VGD(I) = VGS + VTDZ * TIX
  end do
 
! **   Vertical derivative for the night-time.
 
  UTDZ = (UD(3)-UGS)/550
  VTDZ = (VD(3)-VGS)/550
 
! **   Follow procedure again for the night-time (50 - 500m).
 
  do I=1,20
    TIX = FLOAT(I)*ZZ
    UG(I) = UGS+UTDZ*TIX
    VG(I) = VGS+VTDZ*TIX
  end do

  do i=21,46
    ug(i) = u_fine(i)
    vg(i) = v_fine(i)
  end do 

  return
end

