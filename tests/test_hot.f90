program test_hot
  use simsphere_mod
  implicit none

  real :: B, BareNetRadn, BareEvapFlux,BareHeatFlux

  BareNetRadn = 4.0
  BareEvapFlux = 2.0
  BareHeatFlux = 1.0

  LAMBDA = 1.0
  ATEMP = 2.0
  TT(2) = 1.0
  Z(2) = 1.0
  FRVEG = 1.0
  RNET = 1.0
  TF = 1.0
  TAF = 1.0
  CHF = 1.0
  CHG = 1.0
  HF = 1.0
  RNETG = 3.0
  XLEG = 1.0
  GBL_sum = 1.0

  
! 3 cases in HOT
!   I. FRVEG == 1 AND RNET > 0
!   II. 1 > FRVEG > 0 AND RNET > 0
!   III. All other

! case I
  call hot(B,BareNetRadn,BareEvapFlux,BareHeatFlux)

  write(*,*) 'case I: ',Heat

! case II
  FRVEG = 0.5
  call hot(B,BareNetRadn,BareEvapFlux,BareHeatFlux)
  write(*,*) 'case II: ',Heat

! case III
  FRVEG = 1.5
  RNET = 0.0
  call hot(B,BareNetRadn,BareEvapFlux,BareHeatFlux)
  write(*,*) 'case III: ',Heat



end program test_hot
