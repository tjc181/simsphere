subroutine  VEGFLX (EVAPV)
  use simsphere_mod, only: tg, xleg, f, chg, dens, le, qaf, taf, hg, &
                           xlef, ta, chf, tf, cha, rst, qa, qstf, cp, &
                           eq
  implicit none

  real :: EVAPV, rprime, qstg


!      INCLUDE 'modvars.h'

! TG is set in VEGRAD from NETRAD only when FRVEG /= 0  .  We calculate a value 
! for TG below, however it may not be the correct value for calculating QSTG.  
! In VEGFLX TG is set to equal otemp (again, only when FRVEG /= 0).  Will wrap 
! this calculation in a conditional to avoid divide by zero.  -tjc 2018-10-04
  
  if ( .not. eq(TG,0.0) ) then
    QSTG = 10**( 6.1989 - 2353. / TG)
  else
    QSTG = 0.0
  end if

  XLEG = F * CHG * DENS * LE * (QSTG - QAF)
!      XLEG = (xleg + xlegn) / 2 ! Smooth
  if ( XLEG < 0 ) XLEG = 0

  TG = TAF + (HG / CHG) / (DENS * CP) 
  EVAPV = XLEG + XLEF
  TAF = (CHA * TA + CHF * TF + TG * CHG) / (CHF + CHG + CHA)
  rprime = 1/(1/chf + RST)
  QAF = ( CHA * QA + rprime * QSTF + F * ChG * QSTG ) / ( CHA + rprime + F * CHG )


  return
end

! XLEG smoothed .. prevents instabilities at Low XLAI in full vegetation
! mode. 5th May 1992
