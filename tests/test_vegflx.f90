program test_vegflx
  use simsphere_mod
  implicit none

! Call subroutine VEGFLX to determine variables changed, develop tests
! Will need at least 2 tests: XLEG >= 0 and XLEG < 0

  real :: EVAPV, rprime, XLEGN

  EVAPV = 0.3
  TAF = 10.0
  CHF = 10.0
  F = 100.0
  CHG = 0.1
  QAF = 0.5
  HG = 1.0
  CHG = 100.5
  XLEGN = 1.0
  XLEF = 1.2
  CHA = 100.0
  TA = 0.4
  CHF = 10.0
  TF = 1.0
  RST = 1.0
  QA = 100.0
  QSTF = 10.0
  

  call VEGFLX(EVAPV)

  write(*,*) QAF

end program test_vegflx
