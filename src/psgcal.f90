subroutine  PSGCAL
  use simsphere_mod, only: thmax, thv, psis, cosbyb, psig
  implicit none

  real :: perwmax, perw2g, rlogpsig, rnpsig

!	REAL RLOGPSIL

!      INCLUDE 'modvars.h'

! **  Calculates the conductivity of the soil
! **  Cosby curves and coefficients (1984)

!  Field Capacity(75% of THMAX) used instead of THMAX.  Lower value
!  felt to fit local measurements better than the fit with tabulated
!  values.

! **  convert ground water contents to percents

  PERWMAX = THMAX * 100 * 0.75
  PERW2G = THV * 100
  RLOGPSIG = ALOG10 ( PSIS ) + COSBYB * ALOG10 ( PERWMAX )              &
             - COSBYB * ALOG10 ( PERW2G )

! * * psig is positive in this program

  RNPSIG = 10 ** ( RLOGPSIG )


! * * convert cm to bars

  PSIG = -RNPSIG / 1020

  return
end
